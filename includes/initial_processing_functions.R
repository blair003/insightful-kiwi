
# Define the function that processes the camtrapdp package
process_camtrapdp_package <- function() {
  #browser()
  tryCatch({
    # Step 1: Read the camtrapdp data package
    logger::log_info("Starting Step 1: Reading the camtrapdp package")
    package <- read_camtrapdp()
    
    # Check for failure to read data
    if (is.null(package)) {
      logger::log_error("Error reading the camtrapdp package with read_camtrapdp()")
      stop("Error reading the camtrapdp package with read_camtrapdp()")
    }
    logger::log_info("Success in Step 1: read_camtrapdp()")
    
    core_data <- list()
    core_data$created <- package$created
    core_data$id <- package$id
    core_data$name <- package$name
    
    # Step 2: Transform the observations data
    logger::log_info("Starting Step 2: Consolidating species observations")
    core_data$obs <- consol_spp_obs(package$data$observations, config$globals$spp_consol_defs)
    
    if (is.null(core_data$obs)) {
      logger::log_error("Error consolidating species with consol_spp_obs()")
      stop("Error consolidating species with consol_spp_obs()")
    }
    logger::log_info("Success in Step 2: consol_spp_obs()")
    
    # Step 3: Transform the taxonomic data
    logger::log_info("Starting Step 3: Consolidating taxonomic data")
    core_data$taxonomic <- consol_taxa(package$taxonomic, config$globals$spp_consol_defs)
    
    if (is.null(core_data$taxonomic)) {
      logger::log_error("Error consolidating species with consol_taxa()")
      stop("Error consolidating species with consol_taxa()")
    }
    logger::log_info("Success in Step 3: consol_taxa()")
    
    # Step 4: Create period groups (before enhancing data)
    logger::log_info("Starting Step 4: Creating period groups")
    if (config$globals$period_grouping == "calculated_seasons") {
      core_data$period_groups <- create_seasons_available_list(package$data$deployments, config$globals$hemisphere)
    } else {
      # No other methods exist yet
    }

    if (is.null(core_data$period_groups)) {
      logger::log_error("Error creating deployment groups with create_seasons_available_list()")
      stop("Error creating deployment groups with create_seasons_available_list()")
    }
    logger::log_info("Success in Step 4: create_seasons_available_list()")
    
    # Step 5: Enhance the data
    logger::log_info("Starting Step 5: Enhancing the observation and deployment data")
    enhanced_data <- enhance_data(core_data$obs, 
                                  package$data$deployments, 
                                  core_data$period_groups, 
                                  config$globals$period_grouping)
    
    core_data$obs <- enhanced_data$obs
    core_data$deps <- enhanced_data$deps
    
    if (is.null(core_data)) {
      logger::log_error("Error creating core_data from enhance_data()")
      stop("Error creating core_data from enhance_data()")
    }
    
    # Step 6: Handle media data
    logger::log_info("Starting Step 6: Processing media data")
    core_data$media <- package$data$media %>%
      select(mediaID, deploymentID, sequenceID, timestamp, filePath, fileName, fileMediatype, favourite)
    
    # Perform any additional transformations or enhancements as needed
    logger::log_info("Success in Step 6: Data processing complete")
    
    # Return only the selected core components
    return(core_data)
    
  }, error = function(e) {
    # Log the error
    logger::log_error(logger, paste("An error occurred while preparing the data:", e$message))
    stop(paste("An error occurred while preparing the data:", e$message))
  })
}





enhance_data <- function(obs, deps, period_groups, period_grouping) {
  
  logger::log_info("camtrapdp_functions.R, enhance_data() running...")
  
  capitalise_first_word <- function(s) {
    str_replace_all(s, pattern = "^\\b(.)", replacement = str_to_upper)
  }
  
  # Handle different period groupings
  if (period_grouping == "calculated_seasons") {
    
    # Filter out the "ALL" season from period_groups
    filtered_seasons <- period_groups[!names(period_groups) %in% "ALL"]
    season_levels <- names(filtered_seasons)  # Maintain the chronological order for seasons
    
    # Function to determine period (currently season) for a given date
    determine_period <- function(date) {
      for (season_name in season_levels) {
        season <- filtered_seasons[[season_name]]
        if (date >= season$start_date & date <= season$end_date) {
          return(season_name)
        }
      }
      return(NA)  # Return NA if no season matches
    }
    
    # Add the period field (currently season) to deployments and make it a factor
    deps <- deps %>%
      mutate(
        period = factor(sapply(start, determine_period), levels = season_levels)  # Make 'period' (formerly 'season') a factor with specified levels
      ) %>%
      select(-c(coordinateUncertainty, timestampIssues, session, array, featureType, habitat, tags, `_id`)) %>%
      arrange(start)
    
  } else {
    stop("Unsupported period_grouping. Currently only 'calculate_seasons' is supported.")
  }
  
  # Count blank and unknown detections before we remove all non-animal observationType's
  obs_summary <- obs %>%
    select(deploymentID, observationType, observationID) %>%
    dplyr::filter(deploymentID %in% deps$deploymentID) %>%
    group_by(deploymentID) %>%
    summarise(
      blank_detections_count = sum(observationType == "blank"),
      unknown_detections_count = sum(observationType == "unknown"),
      animal_detections_count = sum(observationType == "animal"),
      .groups = 'drop'
    )
  
  deps <- deps %>%
    left_join(obs_summary, by = "deploymentID") %>%
    mutate(
      camera_hours = as.numeric(difftime(end, start, units = "hours")),
      locality = unlist(config$meta$localities_list[str_sub(locationName, 1, 2)]),
      line = as.integer(str_sub(locationName, 4, 4)),
      blank_detections_count = replace_na(blank_detections_count, 0),
      unknown_detections_count = replace_na(unknown_detections_count, 0),
      animal_detections_count = replace_na(animal_detections_count, 0)
    )
  
  spp_classes <- lapply(config$globals$spp_classes, function(x) tolower(unlist(x)))
  
  determine_species_class <- function(scientificName_lower, spp_classes) {
    species_class <- vector("character", length = length(scientificName_lower))
    
    for (class in names(spp_classes)) {
      indices <- scientificName_lower %in% spp_classes[[class]]
      species_class[indices] <- class
    }
    
    species_class[species_class == ""] <- config$globals$spp_class_unclassified
    
    return(species_class)
  }
  
  create_species_rank <- function(scientificName_lower, spp_classes) {
    species_list <- unlist(spp_classes)
    rank <- match(scientificName_lower, species_list)
    rank[is.na(rank)] <- Inf
    return(rank)
  }
  
  detection_threshold <- config$globals$dup_detect_threshold
  
  # Merge observation data with deployment information, including the period field (currently season)
  obs_merged <- obs %>%
    dplyr::filter(deploymentID %in% deps$deploymentID) %>%
    dplyr::filter(observationType == "animal") %>%
    select(-c(mediaID, observationType, cameraSetup, taxonIDReference, individualID, speed, radius, angle, `_id`, `vernacularNames.nld`)) %>%
    mutate(
      `vernacularNames.eng` = capitalise_first_word(`vernacularNames.eng`),
      scientificName = capitalise_first_word(scientificName),
      scientificName_lower = tolower(scientificName)
    ) %>%
    left_join(deps %>% select(deploymentID, locationID, locality, line, locationName, longitude, latitude, period), by = "deploymentID") %>%  # Include period from deployments
    mutate(
      period = factor(period, levels = season_levels)  # Make 'period' a factor with specified levels
    ) %>%
    arrange(deploymentID, scientificName, count, sex, lifeStage, timestamp) %>%
    group_by(deploymentID, scientificName, count, sex, lifeStage) %>%
    mutate(
      species_class = determine_species_class(scientificName_lower, spp_classes),
      species_rank = create_species_rank(scientificName_lower, spp_classes),
      possible_duplicate = difftime(timestamp, lag(timestamp, default = first(timestamp)), units = "mins") <= detection_threshold & row_number() > 1
    ) %>%
    ungroup()
  
  return(list(
    deps = deps,
    obs = obs_merged
  ))
}




# Finds the camtrap data package and reads it in from csv into R objects
read_camtrapdp <- function() {

  # Helper function to find the project root so we can get to extdata/datapackage.json
  find_project_root <- function(current_dir, marker) {
    while (TRUE) {
      # Check if the marker exists in the current directory
      if (file.exists(file.path(current_dir, marker))) {
        return(current_dir)
      }
      # Move up to the parent directory
      parent_dir <- dirname(current_dir)
      if (parent_dir == current_dir) {
        stop("Project root not found")
      }
      current_dir <- parent_dir
    }
  }
  
  # Looks for insightful-kiwi.Rproj to figure out project root
  project_root <- find_project_root(getwd(), "insightful-kiwi.Rproj")
  
  # Construct file path based on project_root
  file_path <- file.path(project_root, "extdata", "datapackage.json")
  
  # Function to check and log problems in a dataset
  check_and_log_problems <- function(data, dataset_name) {
    problems <- readr::problems(data)
    
    if (nrow(problems) > 0) {
      logger::log_warn(paste0("PROBLEM in ", dataset_name, ":"))
      logger::log_warn(paste0(capture.output(print(problems)), collapse = "\n"))
    }
  }
  
  # Capture warnings and log them using withCallingHandlers
  withCallingHandlers({
    
    # Read the data package (this will produce warnings we want to capture)
    camtrapdp <- read_camtrap_dp(file = file_path, TRUE)
    
    # Check for and log problems in the datasets
    check_and_log_problems(camtrapdp$data$deployments, "deployments")
    check_and_log_problems(camtrapdp$data$observations, "observations")
    check_and_log_problems(camtrapdp$data$media, "media")
    
  }, warning = function(w) {
    # Log the warning message using logger::log_warn
    logger::log_warn(paste0("WARNING: ", conditionMessage(w)))
    # Continue with the normal flow of execution
    invokeRestart("muffleWarning")  # Optionally, this prevents the warning from being printed to the console again
  })
  
  
  return(camtrapdp)
}


# Consolidates species observations based on spp_consol_defs by overwriting the scientificName, 
# vernacularNames.eng and taxonID of the matching observations

consol_spp_obs <- function(obs, spp_consol_defs) {

  for(species in names(spp_consol_defs)) {
    def <- spp_consol_defs[[species]]
    
    # Identify indices where scientificName matches any of the old_scientificName entries
    idx_sci <- which(obs$scientificName %in% def$old_scientificName)
    
    # If there are matching indices, update scientificName, vernacularNames.eng, taxonID and taxonRank together
    if(length(idx_sci) > 0) {
      obs$scientificName[idx_sci] <- species
      obs$`vernacularNames.eng`[idx_sci] <- def$new_vernacularNames.eng
      obs$taxonID[idx_sci] <- def$new_taxonID
      
      obs$taxonRank[idx_sci] <- def$new_taxonRank
    }
  }

  return(obs)
}

# Consolidates taxonomic based on spp_consol_defs by removing taxonomic
# entries for removed species, and adding a new entry for the remaining species

consol_taxa <- function(taxonomic, spp_consol_defs) {

  # Remove entries with matching old_scientificName
  to_remove <- sapply(taxonomic, function(entry) {
    any(sapply(spp_consol_defs, function(def) {
      entry$scientificName %in% def$old_scientificName
    }))
  })
  taxonomic <- taxonomic[!to_remove]
  
  # Add new entries based on spp_consol_defs
  for(species in names(spp_consol_defs)) {
    def <- spp_consol_defs[[species]]
    new_entry <- list(
      scientificName = species,
      taxonID = def$new_taxonID,
      taxonRank = def$new_taxonRank,
      order = def$new_taxon_order,
      family = def$new_taxon_family,
      vernacularNames = list(eng = def$new_vernacularNames.eng)
    )
    taxonomic <- c(taxonomic, list(new_entry))
  }

  return(taxonomic)
}





# Determine the season based on the month
get_season <- function(month, hemisphere) {
  hemisphere <- tolower(hemisphere)
  
  if (!hemisphere %in% c("north", "south")) {
    stop("Hemisphere must be 'north' or 'south'")
  }
  
  # Define seasons for both hemispheres
  seasons <- list(
    north = c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Fall", "Fall", "Fall", "Winter"),
    south = c("Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer")
  )
  
  # Map months to seasons using the defined vectors
  seasons[[hemisphere]][month]
}


# Assume get_season is defined and config$globals$hemisphere is set correctly

create_seasons_available_list <- function(deps, hemisphere) {
  
  deps <- deps %>%
    mutate(
      # Extract the date part only for start and end dates
      start_date = as.Date(start),
      end_date = as.Date(end),
      start_month = month(start),
      
      # Determine season based on month and hemisphere
      season = get_season(start_month, hemisphere),
      
      year = year(start_date),
      
      # Create a unique identifier for each season within its year
      season_year = paste(season, year),
      start_date = as.POSIXct(paste(start_date, "00:00"), tz = config$globals$timezone),
      end_date = as.POSIXct(paste(end_date, "23:59:59"), tz = config$globals$timezone)
    ) %>%
    group_by(season_year, year) %>%
    summarise(
      # Find the earliest start date and the latest end date for each season_year group
      start_date = min(start_date),
      end_date = max(end_date),
      .groups = 'drop'
    ) %>%
    ungroup()
  
  # Adding 'ALL' entry after arranging, ensuring it comes last
  all_start <- min(deps$start_date)
  all_end <- max(deps$end_date)
  deps <- bind_rows(deps, tibble(
    season_year = "ALL",
    year = year(all_start),  # Assign infinity to ensure 'ALL' sorts last
    start_date = all_start,
    end_date = all_end
  )) 
  # Convert to list format, ensuring order is maintained
  seasons_list <- split(deps[c("start_date", "end_date")], deps$season_year)
  #browser()
  # Remove "ALL" temporarily for sorting
  all_entry <- seasons_list[["ALL"]]
  seasons_list[["ALL"]] <- NULL
  
  # Extract start dates for sorting
  start_dates <- sapply(seasons_list, function(x) x$start_date)
  
  # Order the list by start dates
  ordered_indices <- order(desc(start_dates))
  
  # Apply the ordering to the list
  ordered_seasons_list <- seasons_list[ordered_indices]
  
  # Append "ALL" back to the ordered list
  ordered_seasons_list[["ALL"]] <- all_entry
  
  return(ordered_seasons_list)
}



# Used in creation of spp_classes_list

get_species_name <- function(scientific_name, nametype) {
  scientific_name_lower <- tolower(scientific_name)
  species_entry <- core_data$taxonomic[sapply(core_data$taxonomic, function(x) tolower(x$scientificName) == scientific_name_lower)]
  
  if (length(species_entry) == 0) return(scientific_name)
  
  species_entry <- species_entry[[1]]
  if (nametype == "scientificName") return(species_entry$scientificName)
  
  vernacular_name <- species_entry$vernacularNames[[sub("vernacularNames\\.", "", nametype)]]
  if (is.null(vernacular_name) || is.na(vernacular_name)) return(species_entry$scientificName)
  
  vernacular_name <- capitalise_first_word(vernacular_name)
  
  return(vernacular_name)
}


capitalise_first_word <- function(s) {
  # Using str_replace_all to ensure the operation is vectorized
  str_replace_all(s, pattern = "^\\b(.)", replacement = str_to_upper)
}


# Main function to process species data
create_species_list <- function(obs) {
  
  filter_observed_species <- function(species_list, observed_species) {
    tolower(species_list) %in% observed_species
  }
  
  build_species_list <- function(species, nametype) {
    setNames(species, sapply(species, get_species_name, nametype = nametype))
  }
  
  observed_species <- unique(obs$scientificName_lower)
  
  spp_classes_list <- setNames(
    lapply(names(config$globals$spp_classes), function(category) {
      species_list <- config$globals$spp_classes[[category]]
      species_list <- species_list[filter_observed_species(species_list, observed_species)]
      build_species_list(species_list, config$globals$species_name_type)
    }),
    tools::toTitleCase(names(config$globals$spp_classes))
  )
  
  if (isTRUE(config$globals$spp_show_unclassified)) {
    defined_species <- unique(tolower(unlist(config$globals$spp_classes)))
    uncategorised_species <- setdiff(observed_species, defined_species)
    
    if (length(uncategorised_species) > 0) {
      spp_classes_list[[tools::toTitleCase(config$globals$spp_class_unclassified)]] <- 
        build_species_list(uncategorised_species, config$globals$species_name_type)
    }
  }
  
  return(spp_classes_list)
}

