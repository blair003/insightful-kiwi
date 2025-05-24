
# Define the function that processes the camtrapdp package
process_camtrapdp_package <- function() {
 # browser()
  tryCatch({

    # Step 1: Read the camtrapdp data package
    logger::log_info("Starting Step 1: Reading the camtrapdp package")
    package <- read_camtrapdp()
    #browser()
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
    # Filter to include only deployments with non-NA start and end dates
    core_data$deps <- package$data$deployments %>%
      filter(!is.na(start) & !is.na(end))
    
    # Step 2: Transform the observations data
    logger::log_info("Starting Step 2: Consolidating species observations")
    #browser()
    core_data$obs <- consol_spp_obs(
      package$data$observations, 
      config$globals$spp_consol_defs
    )
    
    if (is.null(core_data$obs)) {
      logger::log_error("Error consolidating species with consol_spp_obs()")
      stop("Error consolidating species with consol_spp_obs()")
    }
    logger::log_info("Success in Step 2: consol_spp_obs()")
    
    # Step 3: Transform the taxonomic data
    logger::log_info("Starting Step 3: Consolidating taxonomic data")
    
    core_data$taxonomic <- consol_taxa(
      package$taxonomic, 
      config$globals$spp_consol_defs
    )
    
    if (is.null(core_data$taxonomic)) {
      logger::log_error("Error consolidating species with consol_taxa()")
      stop("Error consolidating species with consol_taxa()")
    }
    logger::log_info("Success in Step 3: consol_taxa()")

    # Step 4: Handle media data
    logger::log_info("Starting Step 4: Processing media data")
    core_data$media <- package$data$media %>%
      dplyr::select(
        mediaID, deploymentID, sequenceID, timestamp, filePath, fileName, 
        fileMediatype, favourite
      )
    
    # Perform any additional transformations or enhancements as needed
    logger::log_info("Success in Step 4: Data processing complete")
    

    return(core_data)
    
  }, error = function(e) {
    # Log the error
    logger::log_error("An error occurred while preparing the data:", e$message)
    stop(paste("An error occurred while preparing the data:", e$message))
  })
}





create_period_groups <- function(deps, 
                                 period_grouping_type = NULL, 
                                 hemisphere = NULL) {
  tryCatch({
  #  browser()
    period_groups <- NULL
    
    if (period_grouping_type == "calculated_seasons") {
      logger::log_debug("Creating period groups for calculated_seasons")
      period_groups <- create_seasons_available_list(deps, hemisphere)
    } else {
      # Placeholder for other grouping types in the future
      logger::log_warn("Unhandled period grouping type:", period_grouping_type)
      stop(paste("No valid period grouping method found for the provided type:", 
                 period_grouping_type))
    }
    
    # Check if period_groups was successfully created
    if (is.null(period_groups)) {
      stop(paste("Error creating period groups with method:", period_grouping_type))
    }
    
    logger::log_info("Period groups created successfully with method:", 
                     period_grouping_type)
    return(period_groups)
    
  }, error = function(e) {
    logger::log_error("An error occurred while creating period groups:", e$message)
    stop(e)  # Propagate the original error message
  })
}


# Following two only used for enhance_core_data, so can make them helper functions

determine_species_class <- function(scientificName_lower, spp_classes) {
  # Create an empty vector to store the classification results
  species_class <- vector("character", length = length(scientificName_lower))
  
  # Iterate through each class in the definitions
  for (class in names(spp_classes)) {
    # Find which scientificName_lower entries match the current class
    indices <- scientificName_lower %in% spp_classes[[class]]
    # Assign the class to those entries
    species_class[indices] <- class
  }
  
  # Assign 'config$globals$spp_class_unclassified' to any names that did not match any class
  species_class[species_class == ""] <- config$globals$spp_class_unclassified
  
  return(species_class)
}

create_species_rank <- function(scientificName_lower, spp_classes) {
  species_list <- unlist(spp_classes)
  rank <- match(scientificName_lower, species_list)
  rank[is.na(rank)] <- Inf
  return(rank)
}


enhance_core_data <- function(obs, deps, period_groups) {
  logger::log_info("camtrapdp_functions.R, enhance_core_data() running...")
  
  capitalise_first_word <- function(s) {
    str_replace_all(s, pattern = "^\\b(.)", replacement = str_to_upper)
  }
  
  # Try-catch block for period grouping and filtering
  deps <- tryCatch({
    # Filter out the "ALL" entry, if it exists
    filtered_periods <- period_groups[!names(period_groups) %in% "ALL"]
    period_levels <- names(filtered_periods)
    
    # Determine period for a given date
    determine_period <- function(date) {
      for (period_name in period_levels) {
        period <- filtered_periods[[period_name]]
        if (date >= period$start_date & date <= period$end_date) {
          return(period_name)
        }
      }
      return(NA)
    }
    
    # Add period to deployments and clean up fields
    deps <- deps %>%
      dplyr::mutate(
        period = factor(
          sapply(start, determine_period), 
          levels = period_levels
        )
      ) %>%
      dplyr::select(
        -c(coordinateUncertainty, timestampIssues, session, array, featureType, 
           habitat, tags, `_id`)
      ) %>%
      dplyr::arrange(start)
    
    deps
  }, error = function(e) {
    logger::log_error(
      "Error processing period groups in enhance_core_data: ", e$message
    )
    stop(e)
  })
  
  #browser()
  # Summarizing observations and adding them to deployments
  obs_summary <- obs %>%
    dplyr::select(deploymentID, observationType, observationID) %>%
    dplyr::filter(deploymentID %in% deps$deploymentID) %>%
    dplyr::group_by(deploymentID) %>%
    dplyr::count(deploymentID, observationType, name = "count") %>%
    tidyr::pivot_wider(
      names_from = observationType,
      values_from = count,
      values_fill = 0,
      names_glue = "{observationType}_detections_count"
    )


  deps <- tryCatch({
    deps %>%
      dplyr::left_join(obs_summary, by = "deploymentID") %>%
      dplyr::mutate(
        # Calculate camera hours
        camera_hours = as.numeric(difftime(end, start, units = "hours")),
        
        # Extract locality as everything before the first space
        locality = unlist(
          config$meta$localities_list[
            str_extract(locationName, "^[^ ]+")
          ]
        ),
        
        # Extract line as the number between the space and underscore
        line = as.integer(
          str_extract(locationName, "(?<= )[0-9]+")
        )
      ) %>%
      # Replace NA values with 0 for any detection count columns that exist
      dplyr::mutate(across(ends_with("_detections_count"), ~ tidyr::replace_na(.x, 0)))
  }, error = function(e) {
    logger::log_error("Error merging observation summary with deployments: ", e$message)
    stop(e)
  })
  
  
  spp_classes <- lapply(
    config$globals$spp_classes, 
    function(x) tolower(unlist(x))
  )
  
  # Processing observations and joining with deployments
  obs_merged <- tryCatch({
    obs %>%
      dplyr::filter(
        deploymentID %in% deps$deploymentID,
        observationType %in% c("animal", "human")
      ) %>%
      
      # Remove fields we never need
      dplyr::select(
        -c(
          mediaID, cameraSetup, taxonIDReference, individualID, 
          speed, radius, angle, `_id`, `vernacularNames.nld`
        )
      ) %>%
      
      # Format species names and add lower-case scientific name
      dplyr::mutate(
        `vernacularNames.eng` = capitalise_first_word(`vernacularNames.eng`),
        scientificName = capitalise_first_word(scientificName),
        scientificName_lower = tolower(scientificName)
      ) %>%
      
      # Join with deployments and add derived fields
      dplyr::left_join(
        deps %>% dplyr::select(
          deploymentID, locationID, locality, line, locationName, longitude, 
          latitude, period
        ),
        by = "deploymentID"
      ) %>%
      dplyr::mutate(
        period = factor(period, levels = period_levels),
        species_class = determine_species_class(scientificName_lower, spp_classes),
        species_rank = create_species_rank(scientificName_lower, spp_classes)
      ) %>%
      
      # Arrange by relevant fields for possible duplicate checking
      dplyr::arrange(
        deploymentID, scientificName, count, sex, lifeStage, timestamp
      ) %>%
      
      # Group for duplicate checking
      dplyr::group_by(
        deploymentID, scientificName, count, sex, lifeStage
      ) %>%
      
      # Mark possible duplicates within groups
      dplyr::mutate(
        possible_duplicate = difftime(
          timestamp, 
          lag(timestamp, default = first(timestamp)), 
          units = "mins"
        ) <= config$globals$dup_detect_threshold & dplyr::row_number() > 1
      ) %>%
      dplyr::ungroup()
  }, error = function(e) {
    logger::log_error("Error processing and merging observations: ", e$message)
    stop(e)
  })
  
  
  
  return(list(
    deps = deps,
    obs = obs_merged
  ))
}




# Finds the camtrap data package and reads it in from csv into R objects
read_camtrapdp <- function() {
  # browser()
  # Construct file path
  file_path <- file.path(config$env$dirs$camtrap_package, "datapackage.json")
  
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
    # Prevents  warning from being printed to the console again
    invokeRestart("muffleWarning")  
  })
  
  
  return(camtrapdp)
}


# Consolidates species observations based on spp_consol_defs by overwriting the 
# scientificName, vernacularNames.eng and taxonID of the matching observations

consol_spp_obs <- function(obs, spp_consol_defs) {
  # Convert scientificName in obs to lowercase
  obs$scientificName <- tolower(obs$scientificName)
  
  for (species in names(spp_consol_defs)) {
    def <- spp_consol_defs[[species]]
    
    # Convert species and old scientific names to lowercase for comparison
    species_lower <- tolower(species)
    old_sci_names_lower <- tolower(def$old_scientificName)
    
    # Identify indices where scientificName matches any of the old_scientificName entries
    idx_sci <- which(obs$scientificName %in% old_sci_names_lower)
    
    # If there are matching indices, update fields
    if (length(idx_sci) > 0) {
      obs$scientificName[idx_sci] <- species_lower
      if (!is.null(def$new_vernacularNames.eng)) obs$`vernacularNames.eng`[idx_sci] <- def$new_vernacularNames.eng
      if (!is.null(def$new_vernacularNames.nld)) obs$`vernacularNames.nld`[idx_sci] <- def$new_vernacularNames.nld
      if (!is.null(def$new_taxonID)) obs$taxonID[idx_sci] <- def$new_taxonID
      if (!is.null(def$new_taxonRank)) obs$taxonRank[idx_sci] <- def$new_taxonRank
    }
  }
  
  return(obs)
}


# Consolidates taxonomic based on spp_consol_defs by removing taxonomic
# entries for removed species, and adding a new entry for the remaining species

consol_taxa <- function(taxonomic, spp_consol_defs) {
 # browser()
  for (species in names(spp_consol_defs)) {
    def <- spp_consol_defs[[species]]
    species_lower <- tolower(species)
    old_sci_names_lower <- tolower(def$old_scientificName)
    
    # Remove entries in taxonomic where scientificName matches any old_scientificName
    taxonomic <- taxonomic[!sapply(taxonomic, function(entry) {
      tolower(entry$scientificName) %in% old_sci_names_lower
    })]
    
    # Initialize new entry with scientific name
    new_entry <- list(scientificName = species)  # Preserve original case
    
    # Add vernacular names only if they exist
    vernacular_names <- list()
    if (!is.null(def$new_vernacularNames.eng)) vernacular_names$eng <- def$new_vernacularNames.eng
    if (!is.null(def$new_vernacularNames.nld)) vernacular_names$nld <- def$new_vernacularNames.nld
    if (length(vernacular_names) > 0) new_entry$vernacularNames <- vernacular_names
    
    # Conditionally add fields if they exist in the definition
    if (!is.null(def$new_taxonID)) new_entry$taxonID <- def$new_taxonID
    if (!is.null(def$new_taxonRank)) new_entry$taxonRank <- def$new_taxonRank
    if (!is.null(def$new_taxon_order)) new_entry$order <- def$new_taxon_order
    if (!is.null(def$new_taxon_family)) new_entry$family <- def$new_taxon_family
    
    # Append the new entry to taxonomic
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
    north = c("Winter", "Winter", "Spring", "Spring", "Spring", 
              "Summer", "Summer", "Summer", "Fall", "Fall", "Fall", "Winter"),
    
    south = c("Summer", "Summer", "Autumn", "Autumn", "Autumn", 
              "Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer")
  )
  
  # Map months to seasons using the defined vectors
  seasons[[hemisphere]][month]
}


# Assume get_season is defined and config$globals$hemisphere is set correctly

create_seasons_available_list <- function(deps, hemisphere) {
#  browser()
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
  
  # Find the species entry by matching scientificName
  species_entry <- core_data$taxonomic[
    sapply(core_data$taxonomic, function(x) tolower(x$scientificName) == scientific_name_lower)
  ]
  
  # Return the scientific name if no match is found
  if (length(species_entry) == 0) {
    return(scientific_name)
  }
  
  species_entry <- species_entry[[1]]
  
  # Return the scientific name if nametype matches
  if (nametype == "scientificName") {
    return(species_entry$scientificName)
  }
  
  # Extract the vernacular name based on nametype
  vernacular_key <- sub("vernacularNames\\.", "", nametype)
  vernacular_name <- species_entry$vernacularNames[[vernacular_key]]
  
  # Return scientific name if vernacular name is missing or NA
  if (is.null(vernacular_name) || is.na(vernacular_name)) {
    return(species_entry$scientificName)
  }
  
  # Capitalize the vernacular name and return it
  return(capitalise_first_word(vernacular_name))
}


capitalise_first_word <- function(s) {
  # Using str_replace_all to ensure the operation is vectorized
  str_replace_all(s, pattern = "^\\b(.)", replacement = str_to_upper)
}


# Main function to process species data

create_species_list <- function(obs) {
  # Helper function to filter observed species in case-insensitive manner
  filter_observed_species <- function(species_list, observed_species) {
    tolower(species_list) %in% observed_species
  }
  #browser()
  # Helper function to build species list with the desired name type
  build_species_list <- function(species, nametype) {
    setNames(species, sapply(species, get_species_name, nametype = nametype))
  }
  
  # Get observed species in lowercase
  observed_species <- unique(tolower(obs$scientificName))
  
  # Create spp_classes list based on config$globals$spp_classes without filtering
  spp_classes <- setNames(
    lapply(names(config$globals$spp_classes), function(category) {
      species_list <- config$globals$spp_classes[[category]]
      # Build list with all species in config$globals$spp_classes, unfiltered
      build_species_list(species_list, config$globals$species_name_type)
    }),
    tools::toTitleCase(names(config$globals$spp_classes))
  )
  
  # If spp_show_unclassified is TRUE, add unclassified observed species
  if (isTRUE(config$globals$spp_show_unclassified)) {
    # Identify defined species and unclassified observed species
    defined_species <- unique(tolower(unlist(config$globals$spp_classes)))
    uncategorised_species <- setdiff(observed_species, defined_species)
    
    # Add unclassified species if any
    if (length(uncategorised_species) > 0) {
      spp_classes[[tools::toTitleCase(config$globals$spp_class_unclassified)]] <- 
        build_species_list(uncategorised_species, config$globals$species_name_type)
    }
  }
  
  return(spp_classes)
}




