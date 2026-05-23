
# Defines how source camera timestamps are interpreted. Some exports label local
# camera clock times as UTC, so these helpers can force the timezone without
# converting the displayed clock time.
source_timestamp_timezone <- function() {
  if (exists("config", inherits = TRUE) &&
      !is.null(config$globals$source_timestamp_timezone) &&
      nzchar(config$globals$source_timestamp_timezone)) {
    return(config$globals$source_timestamp_timezone)
  }

  if (exists("config", inherits = TRUE) &&
      !is.null(config$globals$actual_timezone) &&
      nzchar(config$globals$actual_timezone)) {
    return(config$globals$actual_timezone)
  }

  "Pacific/Auckland"
}

should_force_source_timestamp_timezone <- function() {
  exists("config", inherits = TRUE) &&
    isTRUE(config$globals$source_timestamps_are_local)
}

parse_source_timestamp_as_local <- function(value, timezone = source_timestamp_timezone()) {
  if (inherits(value, "POSIXt")) {
    value_chr <- format(value, "%Y-%m-%d %H:%M:%S")
    value_chr[is.na(value)] <- NA_character_
    return(suppressWarnings(as.POSIXct(value_chr, tz = timezone)))
  }

  if (inherits(value, "Date")) {
    return(as.POSIXct(value, tz = timezone))
  }

  value_chr <- as.character(value)
  value_chr[!nzchar(value_chr)] <- NA_character_
  value_chr <- sub("(Z|[+-][0-9]{2}:?[0-9]{2})$", "", value_chr)
  value_chr <- sub("T", " ", value_chr, fixed = TRUE)

  suppressWarnings(as.POSIXct(value_chr, tz = timezone))
}

force_timestamp_columns_timezone <- function(data, columns, timezone = source_timestamp_timezone()) {
  if (!should_force_source_timestamp_timezone() || is.null(data)) {
    return(data)
  }

  for (column in intersect(columns, names(data))) {
    data[[column]] <- parse_source_timestamp_as_local(data[[column]], timezone)
  }

  data
}

normalise_camtrapdp_source_timezones <- function(package) {
  if (!should_force_source_timestamp_timezone() || is.null(package$data)) {
    return(package)
  }

  package$data$deployments <- force_timestamp_columns_timezone(
    package$data$deployments,
    c("deploymentStart", "deploymentEnd", "start", "end")
  )
  package$data$observations <- force_timestamp_columns_timezone(
    package$data$observations,
    c("eventStart", "eventEnd", "timestamp")
  )
  package$data$media <- force_timestamp_columns_timezone(
    package$data$media,
    "timestamp"
  )

  package
}

normalise_core_data_timezones <- function(core_data) {
  if (!should_force_source_timestamp_timezone() || is.null(core_data)) {
    return(core_data)
  }

  core_data$deps <- force_timestamp_columns_timezone(core_data$deps, c("start", "end"))
  core_data$obs <- force_timestamp_columns_timezone(core_data$obs, "timestamp")
  core_data$media <- force_timestamp_columns_timezone(core_data$media, "timestamp")
  core_data$environment_daily <- force_timestamp_columns_timezone(
    core_data$environment_daily,
    c("sunrise", "sunset", "civil_dawn", "civil_dusk", "matutinal_end", "diurnal_end")
  )

  if (!is.null(core_data$period_groups)) {
    core_data$period_groups <- lapply(core_data$period_groups, function(period_group) {
      force_timestamp_columns_timezone(period_group, c("start_date", "end_date"))
    })
  }

  core_data
}


# Define the function that processes the camtrapdp package
core_data_detection_observation_types <- function() {
  c("animal", "human")
}

filter_detection_obs <- function(obs) {
  if (is.null(obs) || !("observationType" %in% names(obs))) {
    return(obs)
  }

  obs %>% dplyr::filter(.data$observationType %in% core_data_detection_observation_types())
}

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
    package <- normalise_camtrapdp_source_timezones(package)
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
        dplyr::any_of(c(
          "mediaID", "deploymentID", "sequenceID", "timestamp", "filePath",
          "fileName", "fileMediatype", "filePublic", "isPublic", "favourite",
          "favorite"
        ))
      )

    if (!"favourite" %in% names(core_data$media) && "favorite" %in% names(core_data$media)) {
      core_data$media$favourite <- core_data$media$favorite
    }

    # Perform any additional transformations or enhancements as needed
    logger::log_info("Success in Step 4: Data processing complete")


    return(core_data)

  }, error = function(e) {
    # Log the error
    logger::log_error("An error occurred while preparing the data: %s", e$message)
    stop(paste("An error occurred while preparing the data:", e$message))
  })
}





create_period_groups <- function(deps,
                                 period_grouping_type = NULL,
                                 hemisphere = NULL,
                                 include_years = FALSE) {
  tryCatch({
  #  browser()
    period_groups <- NULL

    if (period_grouping_type == "calculated_seasons") {
      logger::log_debug("Creating period groups for calculated_seasons")
      period_groups <- create_seasons_available_list(
        deps,
        hemisphere,
        include_years = include_years
      )
    } else if (period_grouping_type == "year") {
      logger::log_debug("Creating period groups for year")
      period_groups <- create_years_available_list(deps)
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

    logger::log_info("Period groups created successfully with method %s:",
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


enhance_core_data <- function(obs, deps, period_groups, include_weather = TRUE) {
  logger::log_info("camtrapdp_functions.R, enhance_core_data() running...")

  capitalise_first_word <- function(s) {
    str_replace_all(s, pattern = "^\\b(.)", replacement = str_to_upper)
  }

  # Try-catch block for period grouping and filtering
  deps <- tryCatch({
    # Filter out the "ALL" entry, if it exists
    filtered_periods <- period_groups[!names(period_groups) %in% "ALL"]
    filtered_periods <- Filter(function(period) {
      is.null(period$assign_period) || isTRUE(period$assign_period)
    }, filtered_periods)
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
      dplyr::filter(deploymentID %in% deps$deploymentID) %>%

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
        possible_duplicate = .data$observationType %in% core_data_detection_observation_types() &
          difftime(
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


  daylight_enrichment <- tryCatch({
    if (exists("add_observation_daylight_classes", mode = "function", inherits = TRUE)) {
      add_observation_daylight_classes(obs_merged, deps)
    } else if (exists("add_observation_time_classes", mode = "function", inherits = TRUE)) {
      list(obs = add_observation_time_classes(obs_merged, NULL), environment_daily = NULL)
    } else {
      list(obs = obs_merged, environment_daily = NULL)
    }
  }, error = function(e) {
    logger::log_warn(
      "Unable to add observation daylight/diel classifications in enhance_core_data: %s",
      conditionMessage(e)
    )
    list(obs = obs_merged, environment_daily = NULL)
  })

  obs_merged <- daylight_enrichment$obs
  daily_data <- daylight_enrichment$environment_daily

  weather_enrichment <- tryCatch({
    if (isTRUE(include_weather) &&
        exists("enrich_observations_with_daily_weather", mode = "function", inherits = TRUE)) {
      enrich_observations_with_daily_weather(obs_merged, deps, daily_data)
    } else {
      list(obs = obs_merged, environment_daily = daily_data)
    }
  }, error = function(e) {
    logger::log_warn(
      "Unable to add observation weather fields in enhance_core_data: %s",
      conditionMessage(e)
    )
    list(obs = obs_merged, environment_daily = daily_data)
  })

  obs_merged <- weather_enrichment$obs
  daily_data <- weather_enrichment$environment_daily


  return(list(
    deps = deps,
    obs = obs_merged,
    environment_daily = daily_data
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
      logger::log_warn("PROBLEM in %s:", dataset_name)
      logger::log_warn("%s", paste0(capture.output(print(problems)), collapse = "\n"))
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
    logger::log_warn("WARNING: %s", conditionMessage(w))
    # Prevents  warning from being printed to the console again
    invokeRestart("muffleWarning")
  })


  return(camtrapdp)
}


# Consolidates species observations based on spp_consol_defs by overwriting the
# scientificName, vernacularNames.eng and taxonID of the matching observations

consol_spp_obs <- function(obs, spp_consol_defs) {
  scientific_names_lower <- tolower(obs$scientificName)

  for (species in names(spp_consol_defs)) {
    def <- spp_consol_defs[[species]]

    old_sci_names_lower <- tolower(def$old_scientificName)

    # Identify indices where scientificName matches any of the old_scientificName entries
    idx_sci <- which(scientific_names_lower %in% old_sci_names_lower)

    # If there are matching indices, update fields
    if (length(idx_sci) > 0) {
      obs$scientificName[idx_sci] <- species
      scientific_names_lower[idx_sci] <- tolower(species)
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

  # Collect all old scientific names to remove them in one pass
  all_old_sci_names_lower <- unlist(lapply(spp_consol_defs, function(def) tolower(def$old_scientificName)), use.names = FALSE)

  if (length(all_old_sci_names_lower) > 0) {
    # Extract all scientific names from taxonomic list
    taxonomic_sci_names <- tolower(vapply(taxonomic, function(x) x$scientificName, FUN.VALUE = character(1)))

    # Filter the list using vectorized %in%
    taxonomic <- taxonomic[!(taxonomic_sci_names %in% all_old_sci_names_lower)]
  }

  # Generate new entries for consolidated species
  new_entries <- lapply(names(spp_consol_defs), function(species) {
    def <- spp_consol_defs[[species]]

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

    return(new_entry)
  })

  # Append the new entries to taxonomic
  taxonomic <- c(taxonomic, new_entries)

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

create_years_available_list <- function(deps) {
  display_timezone <- if (!is.null(config$globals$actual_timezone) &&
                          nzchar(config$globals$actual_timezone)) {
    config$globals$actual_timezone
  } else {
    config$globals$timezone
  }

  deps_dates <- deps %>%
    dplyr::mutate(
      start_date = as.Date(start),
      end_date = as.Date(end)
    )

  all_start <- min(deps_dates$start_date, na.rm = TRUE)
  all_end <- max(deps_dates$end_date, na.rm = TRUE)
  available_years <- seq(lubridate::year(all_start), lubridate::year(all_end))

  years <- tibble::tibble(
    period_name = as.character(available_years),
    start_date = as.Date(paste0(available_years, "-01-01")),
    end_date = as.Date(paste0(available_years, "-12-31")),
    period_type = "year",
    assign_period = TRUE
  ) %>%
    dplyr::mutate(
      start_date = pmax(start_date, all_start),
      end_date = pmin(end_date, all_end)
    ) %>%
    dplyr::filter(start_date <= end_date) %>%
    dplyr::mutate(
      start_date = as.POSIXct(paste(start_date, "00:00"), tz = display_timezone),
      end_date = as.POSIXct(paste(end_date, "23:59:59"), tz = display_timezone)
    )

  all_entry <- tibble::tibble(
    period_name = "ALL",
    start_date = as.POSIXct(paste(all_start, "00:00"), tz = display_timezone),
    end_date = as.POSIXct(paste(all_end, "23:59:59"), tz = display_timezone),
    period_type = "all",
    assign_period = FALSE
  )

  period_rows_to_list(dplyr::bind_rows(years, all_entry))
}

period_rows_to_list <- function(period_rows) {
  all_entry <- period_rows[period_rows$period_name == "ALL", , drop = FALSE]
  period_rows <- period_rows[period_rows$period_name != "ALL", , drop = FALSE]
  period_rows <- period_rows %>%
    dplyr::arrange(dplyr::desc(start_date), period_type, period_name)

  period_list <- split(
    period_rows[c("start_date", "end_date", "period_type", "assign_period")],
    factor(period_rows$period_name, levels = period_rows$period_name)
  )

  if (nrow(all_entry) > 0) {
    period_list[["ALL"]] <- all_entry[c(
      "start_date", "end_date", "period_type", "assign_period"
    )]
  }

  period_list
}

create_seasons_available_list <- function(deps, hemisphere, include_years = FALSE) {
#  browser()
  source_deps <- deps
  display_timezone <- if (!is.null(config$globals$actual_timezone) &&
                          nzchar(config$globals$actual_timezone)) {
    config$globals$actual_timezone
  } else {
    config$globals$timezone
  }

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
      start_date = as.POSIXct(paste(start_date, "00:00"), tz = display_timezone),
      end_date = as.POSIXct(paste(end_date, "23:59:59"), tz = display_timezone),
      period_type = "calculated_season",
      assign_period = TRUE
    ) %>%
    group_by(season_year, year, period_type, assign_period) %>%
    summarise(
      # Find the earliest start date and the latest end date for each season_year group
      start_date = min(start_date),
      end_date = max(end_date),
      .groups = 'drop'
    ) %>%
    ungroup() %>%
    dplyr::rename(period_name = season_year)

  # Adding 'ALL' entry after arranging, ensuring it comes last
  all_start <- min(deps$start_date)
  all_end <- max(deps$end_date)
  deps <- bind_rows(deps, tibble(
    period_name = "ALL",
    year = year(all_start),  # Assign infinity to ensure 'ALL' sorts last
    start_date = all_start,
    end_date = all_end,
    period_type = "all",
    assign_period = FALSE
  ))

  if (isTRUE(include_years)) {
    year_periods <- create_years_available_list(source_deps)
    year_rows <- dplyr::bind_rows(lapply(names(year_periods), function(period_name) {
      period <- year_periods[[period_name]]
      data.frame(
        period_name = period_name,
        year = lubridate::year(period$start_date),
        start_date = period$start_date,
        end_date = period$end_date,
        period_type = period$period_type,
        assign_period = FALSE,
        stringsAsFactors = FALSE
      )
    }))
    year_rows <- year_rows[year_rows$period_name != "ALL", , drop = FALSE]
    deps <- dplyr::bind_rows(deps, year_rows)
  }

  period_rows_to_list(deps)
}



# Used in creation of spp_classes_list

get_species_name <- function(scientific_name, nametype, taxonomic = NULL) {
  scientific_name_lower <- tolower(scientific_name)

  if (is.null(taxonomic) &&
      exists("core_data", inherits = TRUE) &&
      !is.null(core_data$taxonomic)) {
    taxonomic <- core_data$taxonomic
  }

  if (is.null(taxonomic)) {
    return(scientific_name)
  }

  # Find the species entry by matching scientificName
  species_entry <- taxonomic[
    sapply(taxonomic, function(x) tolower(x$scientificName) == scientific_name_lower)
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

create_species_list <- function(obs, taxonomic = NULL) {
  # Helper function to filter observed species in case-insensitive manner
  filter_observed_species <- function(species_list, observed_species) {
    tolower(species_list) %in% observed_species
  }
  #browser()
  # Helper function to build species list with the desired name type
  build_species_list <- function(species, nametype) {
    setNames(species, sapply(species, get_species_name, nametype = nametype, taxonomic = taxonomic))
  }

  obs <- filter_detection_obs(obs)
  obs <- obs[!is.na(obs$scientificName) & nzchar(as.character(obs$scientificName)), , drop = FALSE]

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
