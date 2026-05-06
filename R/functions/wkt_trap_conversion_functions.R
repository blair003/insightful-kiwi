# Conversion helpers for Whakatane Kiwi Trust trap check exports.
#
# The converter intentionally writes a separate Camtrap DP-style package rather
# than altering the main monitoring package in extdata.

wkt_trap_default_species_map <- function() {
  data.frame(
    outcome_id = c(
      "1", "2", "4", "5", "6", "7", "9", "10", "11", "14", "15",
      "1009", "1010", "1011", "1025", "1026", "1027", "1028", "1029",
      "1030", "1031", "1032", "1033", "1034", "1035", "1036", "1037"
    ),
    scientificName = c(
      "Mustela erminea",
      "Mustela nivalis",
      "Rattus",
      "Felis catus",
      "Oryctolagus cuniculus",
      "Erinaceus europaeus",
      "Mustela erminea",
      "Mustela putorius furo",
      "Mus musculus",
      "Animalia",
      "Trichosurus vulpecula",
      "Mustelidae",
      "Gallirallus australis",
      "Aves",
      "Mustela erminea",
      "Mustela erminea",
      "Mustela erminea",
      "Mustela erminea",
      "Mustela nivalis",
      "Mustela nivalis",
      "Mustela putorius furo",
      "Mustela putorius furo",
      "Felis catus",
      "Felis catus",
      "Felis catus",
      "Felis catus",
      "Felis catus"
    ),
    vernacularName = c(
      "stoat",
      "weasel",
      "rats",
      "domestic cat",
      "European rabbit",
      "European hedgehog",
      "stoat",
      "ferret",
      "house mouse",
      "animal",
      "common brushtail possum",
      "mustelid",
      "weka",
      "bird",
      "stoat",
      "stoat",
      "stoat",
      "stoat",
      "weasel",
      "weasel",
      "ferret",
      "ferret",
      "domestic cat",
      "domestic cat",
      "domestic cat",
      "domestic cat",
      "domestic cat"
    ),
    taxonRank = c(
      "species", "species", "genus", "species", "species", "species",
      "species", "subspecies", "species", "kingdom", "species", "family",
      "species", "class", "species", "species", "species", "species",
      "species", "species", "subspecies", "subspecies", "species",
      "species", "species", "species", "species"
    ),
    taxonID = c(
      "https://www.checklistbank.org/dataset/COL2023/taxon/43B9B",
      "https://www.checklistbank.org/dataset/COL2023/taxon/43B9C",
      "https://www.checklistbank.org/dataset/COL2023/taxon/63QK6",
      "https://www.checklistbank.org/dataset/COL2023/taxon/3DXV3",
      NA_character_,
      NA_character_,
      "https://www.checklistbank.org/dataset/COL2023/taxon/43B9B",
      NA_character_,
      NA_character_,
      "https://www.checklistbank.org/dataset/COL2023/taxon/N",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      "https://www.checklistbank.org/dataset/COL2023/taxon/43B9B",
      "https://www.checklistbank.org/dataset/COL2023/taxon/43B9B",
      "https://www.checklistbank.org/dataset/COL2023/taxon/43B9B",
      "https://www.checklistbank.org/dataset/COL2023/taxon/43B9B",
      "https://www.checklistbank.org/dataset/COL2023/taxon/43B9C",
      "https://www.checklistbank.org/dataset/COL2023/taxon/43B9C",
      NA_character_,
      NA_character_,
      "https://www.checklistbank.org/dataset/COL2023/taxon/3DXV3",
      "https://www.checklistbank.org/dataset/COL2023/taxon/3DXV3",
      "https://www.checklistbank.org/dataset/COL2023/taxon/3DXV3",
      "https://www.checklistbank.org/dataset/COL2023/taxon/3DXV3",
      "https://www.checklistbank.org/dataset/COL2023/taxon/3DXV3"
    ),
    stringsAsFactors = FALSE
  )
}


#' Convert WKT trap check data to a Camtrap DP-style package.
#'
#' Each unique trap/check date becomes a deployment interval from the previous
#' check date to the current check date. First checks are omitted by default
#' because their start date is unknown; set first_deployment_days to seed them.
#'
#' @return A list containing deployments, observations, media, datapackage and a
#'   conversion summary. If output_dir is supplied, the package files are written.
convert_wkt_trap_data_to_camtrapdp <- function(raw_trap_data_path,
                                               trap_locations_path,
                                               reference_tables_path,
                                               output_dir = NULL,
                                               first_deployment_days = NULL,
                                               include_missing_coordinates = FALSE,
                                               package_id = NULL,
                                               package_name = "wkt-trap-checks",
                                               timezone = "UTC",
                                               period_groups = NULL,
                                               species_map = wkt_trap_default_species_map()) {
  read_csv <- function(path) {
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA", "NULL"),
      check.names = FALSE
    )
  }

  required_columns <- function(data, columns, label) {
    missing_columns <- setdiff(columns, names(data))
    if (length(missing_columns) > 0) {
      stop(
        sprintf(
          "%s is missing required columns: %s",
          label,
          paste(missing_columns, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  clean_id <- function(x, prefix) {
    x <- gsub("[^A-Za-z0-9_-]+", "-", as.character(x))
    x <- gsub("(^-+|-+$)", "", x)
    paste(prefix, x, sep = "-")
  }

  format_camtrap_time <- function(date_value) {
    formatted <- format(
      as.POSIXct(date_value, tz = timezone),
      "%Y-%m-%dT%H:%M:%S%z",
      tz = timezone
    )
    sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", formatted)
  }

  parse_check_date <- function(x) {
    parsed <- as.Date(x, format = "%d/%m/%Y")
    if (any(is.na(parsed) & !is.na(x))) {
      bad_values <- unique(x[is.na(parsed) & !is.na(x)])
      stop(
        sprintf(
          "Could not parse trap check dates as day/month/year: %s",
          paste(utils::head(bad_values, 10), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    parsed
  }

  build_tags <- function(values) {
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) == 0) {
      return(NA_character_)
    }
    paste(values, collapse = " | ")
  }

  json_escape <- function(x) {
    x <- gsub("\\\\", "\\\\\\\\", x)
    x <- gsub("\"", "\\\\\"", x)
    x <- gsub("\n", "\\\\n", x)
    x <- gsub("\r", "\\\\r", x)
    x <- gsub("\t", "\\\\t", x)
    x
  }

  to_json <- function(x, indent = 0) {
    pad <- paste(rep(" ", indent), collapse = "")
    child_pad <- paste(rep(" ", indent + 2), collapse = "")

    if (is.null(x) || length(x) == 0) {
      if (is.atomic(x) && length(x) == 0) {
        return("[]")
      }
      return("null")
    }

    if (is.list(x)) {
      item_names <- names(x)
      is_object <- !is.null(item_names) && all(nzchar(item_names))

      if (is_object) {
        entries <- vapply(seq_along(x), function(i) {
          paste0(
            child_pad,
            "\"", json_escape(item_names[i]), "\": ",
            to_json(x[[i]], indent + 2)
          )
        }, character(1))
        return(paste0("{\n", paste(entries, collapse = ",\n"), "\n", pad, "}"))
      }

      entries <- vapply(x, to_json, character(1), indent = indent + 2)
      return(paste0("[\n", child_pad, paste(entries, collapse = paste0(",\n", child_pad)), "\n", pad, "]"))
    }

    if (length(x) > 1) {
      entries <- vapply(as.list(x), to_json, character(1), indent = indent + 2)
      return(paste0("[", paste(entries, collapse = ", "), "]"))
    }

    if (is.na(x)) {
      return("null")
    }
    if (is.logical(x)) {
      return(ifelse(isTRUE(x), "true", "false"))
    }
    if (is.numeric(x) || is.integer(x)) {
      return(as.character(x))
    }

    paste0("\"", json_escape(as.character(x)), "\"")
  }

  raw_trap_data <- read_csv(raw_trap_data_path)
  trap_locations <- read_csv(trap_locations_path)
  reference_tables <- read_csv(reference_tables_path)
  source_trap_record_count <- nrow(raw_trap_data)

  required_columns(
    raw_trap_data,
    c("trapdata_id", "date", "trap_id", "outcome_id"),
    "raw_trap_data"
  )
  required_columns(
    trap_locations,
    c("trap_id", "code", "line_id", "trap_number", "latitude", "longitude"),
    "trap_locations"
  )
  required_columns(
    reference_tables,
    c("outcome_id", "description", "kill"),
    "reference_tables"
  )
  required_columns(
    species_map,
    c("outcome_id", "scientificName", "vernacularName", "taxonRank", "taxonID"),
    "species_map"
  )

  raw_trap_data$trap_id <- as.character(raw_trap_data$trap_id)
  raw_trap_data$outcome_id <- as.character(raw_trap_data$outcome_id)
  raw_trap_data$check_date <- parse_check_date(raw_trap_data$date)
  raw_trap_data$trapdata_id <- as.character(raw_trap_data$trapdata_id)

  trap_locations$trap_id <- as.character(trap_locations$trap_id)
  trap_locations$code <- trimws(trap_locations$code)
  trap_locations$latitude <- suppressWarnings(as.numeric(trap_locations$latitude))
  trap_locations$longitude <- suppressWarnings(as.numeric(trap_locations$longitude))

  reference_tables$outcome_id <- as.character(reference_tables$outcome_id)
  reference_tables$kill <- suppressWarnings(as.integer(reference_tables$kill))
  species_map$outcome_id <- as.character(species_map$outcome_id)

  trap_locations <- trap_locations[!duplicated(trap_locations$trap_id), ]

  raw_trap_data <- merge(
    raw_trap_data,
    trap_locations,
    by = "trap_id",
    all.x = TRUE,
    suffixes = c("", "_trap")
  )

  if ("latitude_trap" %in% names(raw_trap_data)) {
    raw_trap_data$latitude <- raw_trap_data$latitude_trap
  }
  if ("longitude_trap" %in% names(raw_trap_data)) {
    raw_trap_data$longitude <- raw_trap_data$longitude_trap
  }

  raw_trap_data <- merge(
    raw_trap_data,
    reference_tables[c("outcome_id", "description", "kill")],
    by = "outcome_id",
    all.x = TRUE
  )

  raw_trap_data <- merge(raw_trap_data, species_map, by = "outcome_id", all.x = TRUE)

  pre_coordinate_filter_count <- nrow(raw_trap_data)
  if (!include_missing_coordinates) {
    raw_trap_data <- raw_trap_data[
      !is.na(raw_trap_data$latitude) & !is.na(raw_trap_data$longitude),
    ]
  }
  dropped_missing_coordinate_records <- pre_coordinate_filter_count - nrow(raw_trap_data)

  if (nrow(raw_trap_data) == 0) {
    stop(
      paste(
        "No trap check records remain after joining trap metadata",
        "and applying the coordinate filter."
      ),
      call. = FALSE
    )
  }

  check_events <- unique(raw_trap_data[c("trap_id", "check_date")])
  check_events <- check_events[order(check_events$trap_id, check_events$check_date), ]
  check_events$previous_check_date <- as.Date(NA)

  split_indices <- split(seq_len(nrow(check_events)), check_events$trap_id)
  for (indices in split_indices) {
    dates <- check_events$check_date[indices]
    check_events$previous_check_date[indices] <- c(as.Date(NA), dates[-length(dates)])
  }

  if (!is.null(first_deployment_days)) {
    if (!is.numeric(first_deployment_days) || length(first_deployment_days) != 1 ||
        is.na(first_deployment_days) || first_deployment_days <= 0) {
      stop("first_deployment_days must be a single positive number or NULL.", call. = FALSE)
    }

    first_indices <- is.na(check_events$previous_check_date)
    check_events$previous_check_date[first_indices] <-
      check_events$check_date[first_indices] - as.integer(first_deployment_days)
  }

  dropped_first_checks <- sum(is.na(check_events$previous_check_date))
  check_events <- check_events[!is.na(check_events$previous_check_date), ]

  if (nrow(check_events) == 0) {
    stop(
      paste(
        "No deployment intervals could be created.",
        "Each deployment needs a previous check date; set first_deployment_days",
        "if you want to seed the first check for each trap."
      ),
      call. = FALSE
    )
  }
  check_events$deploymentID <- clean_id(
    paste(check_events$trap_id, format(check_events$check_date, "%Y%m%d"), sep = "-"),
    "wkt-trap-deployment"
  )

  raw_trap_data <- merge(
    raw_trap_data,
    check_events[c("trap_id", "check_date", "previous_check_date", "deploymentID")],
    by = c("trap_id", "check_date"),
    all.x = FALSE
  )

  deployment_source <- raw_trap_data[
    !duplicated(raw_trap_data$deploymentID),
    c(
      "deploymentID", "trap_id", "code", "line_id", "trap_number", "project_id",
      "trap_type_id", "latitude", "longitude", "check_date", "previous_check_date"
    )
  ]

  deployments <- data.frame(
    deploymentID = deployment_source$deploymentID,
    locationID = clean_id(deployment_source$trap_id, "wkt-trap-location"),
    locationName = deployment_source$code,
    latitude = deployment_source$latitude,
    longitude = deployment_source$longitude,
    coordinateUncertainty = NA,
    deploymentStart = format_camtrap_time(deployment_source$previous_check_date),
    deploymentEnd = format_camtrap_time(deployment_source$check_date),
    setupBy = NA,
    cameraID = deployment_source$code,
    cameraModel = "predator trap",
    cameraDelay = NA,
    cameraHeight = NA,
    cameraDepth = NA,
    cameraTilt = NA,
    cameraHeading = NA,
    detectionDistance = NA,
    timestampIssues = "dateOnly",
    baitUse = NA,
    featureType = "trap",
    habitat = NA,
    deploymentGroups = paste("line", deployment_source$line_id),
    deploymentTags = mapply(
      function(trap_id, trap_number, project_id, trap_type_id) {
        build_tags(c(
          paste0("source_trap_id:", trap_id),
          paste0("trap_number:", trap_number),
          paste0("project_id:", project_id),
          paste0("trap_type_id:", trap_type_id)
        ))
      },
      deployment_source$trap_id,
      deployment_source$trap_number,
      deployment_source$project_id,
      deployment_source$trap_type_id,
      USE.NAMES = FALSE
    ),
    deploymentComments = paste(
      "Converted from WKT trap check data.",
      "Deployment start is the previous check date and deployment end is the current check date."
    ),
    stringsAsFactors = FALSE
  )

  raw_trap_data$mapped_kill <- raw_trap_data$kill == 1 & !is.na(raw_trap_data$scientificName)
  raw_trap_data$lifeStage <- ifelse(
    grepl("juvenile", raw_trap_data$description, ignore.case = TRUE),
    "juvenile",
    NA_character_
  )
  raw_trap_data$sex <- ifelse(
    grepl("female", raw_trap_data$description, ignore.case = TRUE),
    "female",
    ifelse(grepl("male", raw_trap_data$description, ignore.case = TRUE), "male", NA_character_)
  )

  observations <- data.frame(
    observationID = clean_id(raw_trap_data$trapdata_id, "wkt-trap-observation"),
    deploymentID = raw_trap_data$deploymentID,
    mediaID = NA,
    eventID = clean_id(raw_trap_data$trapdata_id, "wkt-trap-event"),
    eventStart = format_camtrap_time(raw_trap_data$check_date),
    eventEnd = format_camtrap_time(raw_trap_data$check_date),
    prior_check_date = raw_trap_data$previous_check_date,
    check_interval = as.integer(raw_trap_data$check_date - raw_trap_data$previous_check_date),
    observationLevel = "event",
    observationType = ifelse(raw_trap_data$mapped_kill, "animal", "unknown"),
    cameraSetupType = NA,
    scientificName = ifelse(raw_trap_data$mapped_kill, raw_trap_data$scientificName, NA),
    count = ifelse(raw_trap_data$mapped_kill, 1, NA),
    lifeStage = raw_trap_data$lifeStage,
    sex = raw_trap_data$sex,
    behavior = ifelse(raw_trap_data$mapped_kill, "killed", NA),
    individualID = NA,
    individualPositionRadius = NA,
    individualPositionAngle = NA,
    individualSpeed = NA,
    bboxX = NA,
    bboxY = NA,
    bboxWidth = NA,
    bboxHeight = NA,
    classificationMethod = "human",
    classifiedBy = ifelse(
      is.na(raw_trap_data$volunteer_id),
      NA,
      paste0("volunteer_id:", raw_trap_data$volunteer_id)
    ),
    classificationTimestamp = NA,
    classificationProbability = NA,
    observationTags = mapply(
      function(outcome_id, description, kill) {
        build_tags(c(
          paste0("outcome_id:", outcome_id),
          paste0("outcome:", description),
          paste0("kill:", kill)
        ))
      },
      raw_trap_data$outcome_id,
      raw_trap_data$description,
      raw_trap_data$kill,
      USE.NAMES = FALSE
    ),
    observationComments = mapply(
      function(trapdata_id, baitstatus, bait_outcome_id) {
        build_tags(c(
          paste0("source_trapdata_id:", trapdata_id),
          paste0("baitstatus:", baitstatus),
          paste0("bait_outcome_id:", bait_outcome_id)
        ))
      },
      raw_trap_data$trapdata_id,
      raw_trap_data$baitstatus,
      raw_trap_data$bait_outcome_id,
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )

  media <- data.frame(
    mediaID = character(),
    deploymentID = character(),
    captureMethod = character(),
    timestamp = character(),
    filePath = character(),
    filePublic = logical(),
    fileName = character(),
    fileMediatype = character(),
    exifData = character(),
    favorite = logical(),
    mediaComments = character(),
    stringsAsFactors = FALSE
  )

  taxonomic_source <- species_map[
    species_map$scientificName %in% unique(stats::na.omit(observations$scientificName)),
  ]
  taxonomic_source <- taxonomic_source[!duplicated(taxonomic_source$scientificName), ]
  taxonomic <- lapply(seq_len(nrow(taxonomic_source)), function(i) {
    taxon <- list(
      scientificName = taxonomic_source$scientificName[i],
      taxonRank = taxonomic_source$taxonRank[i],
      vernacularNames = list(eng = taxonomic_source$vernacularName[i])
    )
    if (!is.na(taxonomic_source$taxonID[i]) && nzchar(taxonomic_source$taxonID[i])) {
      taxon$taxonID <- taxonomic_source$taxonID[i]
    }
    taxon
  })

  valid_coords <- deployments[!is.na(deployments$latitude) & !is.na(deployments$longitude), ]
  if (nrow(valid_coords) > 0) {
    min_lon <- min(valid_coords$longitude)
    max_lon <- max(valid_coords$longitude)
    min_lat <- min(valid_coords$latitude)
    max_lat <- max(valid_coords$latitude)
    spatial_coords <- list(list(
      c(min_lon, min_lat),
      c(min_lon, max_lat),
      c(max_lon, max_lat),
      c(max_lon, min_lat),
      c(min_lon, min_lat)
    ))
  } else {
    spatial_coords <- list()
  }

  if (is.null(package_id)) {
    package_id <- clean_id(format(Sys.time(), "%Y%m%d%H%M%S"), "wkt-trap-package")
  }

  datapackage <- list(
    profile = "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/camtrap-dp-profile.json",
    name = package_name,
    id = package_id,
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    contributors = list(
      list(
        title = "Whakatane Kiwi Trust",
        role = "rightsHolder",
        organization = "Whakatane Kiwi Trust"
      )
    ),
    sources = list(
      list(
        title = "Whakatane Kiwi Trust trap check export",
        path = basename(raw_trap_data_path)
      )
    ),
    project = list(
      id = "wkt-trap-checks",
      title = "Whakatane Kiwi Trust trap checks",
      samplingDesign = "opportunistic",
      captureMethod = list("activityDetection"),
      individualAnimals = FALSE,
      observationLevel = list("event")
    ),
    spatial = list(type = "Polygon", coordinates = spatial_coords),
    temporal = list(
      start = as.character(min(as.Date(deployment_source$previous_check_date))),
      end = as.character(max(as.Date(deployment_source$check_date)))
    ),
    taxonomic = taxonomic,
    resources = list(
      list(
        name = "deployments",
        path = "deployments.csv",
        profile = "tabular-data-resource",
        format = "csv",
        mediatype = "text/csv",
        encoding = "utf-8",
        schema = "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/deployments-table-schema.json"
      ),
      list(
        name = "media",
        path = "media.csv",
        profile = "tabular-data-resource",
        format = "csv",
        mediatype = "text/csv",
        encoding = "utf-8",
        schema = "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/media-table-schema.json"
      ),
      list(
        name = "observations",
        path = "observations.csv",
        profile = "tabular-data-resource",
        format = "csv",
        mediatype = "text/csv",
        encoding = "utf-8",
        schema = "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/observations-table-schema.json"
      )
    )
  )

  summary <- list(
    source_trap_records = source_trap_record_count,
    converted_trap_records = nrow(raw_trap_data),
    deployments = nrow(deployments),
    observations = nrow(observations),
    animal_observations = sum(observations$observationType == "animal"),
    dropped_first_checks = dropped_first_checks,
    dropped_missing_coordinate_records = dropped_missing_coordinate_records,
    traps_with_deployments = length(unique(deployment_source$trap_id)),
    unmapped_kill_outcome_ids = sort(unique(raw_trap_data$outcome_id[
      raw_trap_data$kill == 1 & is.na(raw_trap_data$scientificName)
    ]))
  )

  result <- list(
    created = datapackage$created,
    id = datapackage$id,
    name = datapackage$name,
    deps = deployments,
    obs = observations,
    deployments = deployments,
    observations = observations,
    media = media,
    taxonomic = taxonomic,
    datapackage = datapackage,
    summary = summary
  )

  if (!is.null(period_groups)) {
    result <- annotate_wkt_trap_periods(result, period_groups)
    deployments <- result$deployments
    observations <- result$observations
    media <- result$media
  }

  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    utils::write.csv(deployments, file.path(output_dir, "deployments.csv"), row.names = FALSE, na = "")
    utils::write.csv(media, file.path(output_dir, "media.csv"), row.names = FALSE, na = "")
    utils::write.csv(observations, file.path(output_dir, "observations.csv"), row.names = FALSE, na = "")

    datapackage_path <- file.path(output_dir, "datapackage.json")
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(
        datapackage,
        datapackage_path,
        pretty = TRUE,
        auto_unbox = TRUE,
        na = "null"
      )
    } else {
      writeLines(to_json(datapackage), datapackage_path, useBytes = TRUE)
    }
    result$output_dir <- normalizePath(output_dir, mustWork = FALSE)
  }

  class(result) <- c("wkt_trap_camtrapdp", class(result))
  result
}


annotate_wkt_trap_periods <- function(trap_data, period_groups) {
  if (is.null(trap_data)) {
    return(NULL)
  }

  trap_data$deployments$check_date <- as.Date(trap_data$deployments$deploymentEnd)

  trap_data$observations$check_date <- as.Date(trap_data$observations$eventStart)
  if (!"prior_check_date" %in% names(trap_data$observations)) {
    trap_data$observations$prior_check_date <- as.Date(trap_data$observations$eventStart)
  } else {
    trap_data$observations$prior_check_date <- as.Date(trap_data$observations$prior_check_date)
  }

  trap_data$deps <- trap_data$deployments
  trap_data$obs <- trap_data$observations
  trap_data$period_groups <- period_groups

  trap_data
}
