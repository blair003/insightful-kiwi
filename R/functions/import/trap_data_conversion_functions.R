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
      "https://www.checklistbank.org/dataset/COL2023/taxon/44QY4",
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
      "https://www.checklistbank.org/dataset/COL2023/taxon/44QY4",
      "https://www.checklistbank.org/dataset/COL2023/taxon/44QY4",
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
#' check date for the same trap_id to the current check date. First checks are
#' retained with missing interval dates unless first_deployment_days is supplied.
#'
#' @return A list containing deployments, observations, media, datapackage and a
#'   conversion summary. If output_dir is supplied, the package files are written.
convert_wkt_trap_data_to_camtrapdp <- function(raw_trap_data_path,
                                               trap_locations_path,
                                               reference_tables_path,
                                               output_dir = NULL,
                                               first_deployment_days = NULL,
                                               include_missing_coordinates = TRUE,
                                               log_file = NULL,
                                               package_id = NULL,
                                               package_name = "wkt-trap-checks",
                                               timezone = "UTC",
                                               period_groups = NULL,
                                               monitoring_deployments = NULL,
                                               kill_prior_check_override_days = NULL,
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
    raw_values <- trimws(as.character(x))
    raw_values[!nzchar(raw_values)] <- NA_character_
    parsed <- as.Date(rep(NA_character_, length(raw_values)))

    numeric_values <- suppressWarnings(as.numeric(raw_values))
    excel_serial_dates <- !is.na(numeric_values) & grepl("^\\d+(\\.\\d+)?$", raw_values)
    parsed[excel_serial_dates] <- as.Date(floor(numeric_values[excel_serial_dates]), origin = "1899-12-30")

    remaining <- is.na(parsed) & !is.na(raw_values)
    parsed[remaining] <- as.Date(raw_values[remaining], format = "%d/%m/%Y")

    remaining <- is.na(parsed) & !is.na(raw_values)
    parsed[remaining] <- as.Date(raw_values[remaining], format = "%Y-%m-%d")

    if (any(is.na(parsed) & !is.na(raw_values))) {
      bad_values <- unique(raw_values[is.na(parsed) & !is.na(raw_values)])
      stop(
        sprintf(
          "Could not parse trap check dates as Excel serial dates, day/month/year, or ISO dates: %s",
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

  normalise_behavior_value <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("&", " and ", x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("(^_+|_+$)", "", x)
    x <- ifelse(is.na(x) | !nzchar(x), NA_character_, x)
    x
  }

  trap_check_behavior <- function(mapped_kill, outcome_basecol, outcome_description) {
    behavior <- normalise_behavior_value(outcome_basecol)
    missing_behavior <- is.na(behavior) | !nzchar(behavior)
    behavior[missing_behavior] <- normalise_behavior_value(outcome_description[missing_behavior])
    behavior[is.na(behavior) | !nzchar(behavior)] <- "trap_checked"
    behavior[mapped_kill] <- "killed"
    behavior
  }

  clean_column_name <- function(x, prefix = NULL) {
    x <- gsub("[^A-Za-z0-9]+", "_", as.character(x))
    x <- gsub("(^_+|_+$)", "", x)
    x <- ifelse(nzchar(x), x, "unknown")
    if (!is.null(prefix)) {
      x <- paste(prefix, x, sep = "_")
    }
    x
  }

  coalesce_character <- function(primary, fallback) {
    primary <- as.character(primary)
    fallback <- as.character(fallback)
    use_fallback <- is.na(primary) | !nzchar(trimws(primary))
    primary[use_fallback] <- fallback[use_fallback]
    primary
  }

  assign_trap_localities <- function(trap_source, monitoring_deployments) {
    trap_ids <- unique(as.character(trap_source$trap_id))
    empty_assignment <- data.frame(
      trap_id = trap_ids,
      locality = NA_character_,
      locality_match_type = NA_character_,
      locality_distance_km = NA_real_,
      nearest_monitoring_locationName = NA_character_,
      stringsAsFactors = FALSE
    )

    required_monitoring_columns <- c("locality", "latitude", "longitude")
    if (is.null(monitoring_deployments) ||
        nrow(monitoring_deployments) == 0 ||
        !all(required_monitoring_columns %in% names(monitoring_deployments))) {
      return(empty_assignment)
    }

    haversine_km <- function(lon1, lat1, lon2, lat2) {
      radius_km <- 6371.0088
      to_rad <- pi / 180
      lat1_rad <- lat1 * to_rad
      lat2_rad <- lat2 * to_rad
      delta_lat <- (lat2 - lat1) * to_rad
      delta_lon <- (lon2 - lon1) * to_rad
      a <- sin(delta_lat / 2)^2 +
        cos(lat1_rad) * cos(lat2_rad) * sin(delta_lon / 2)^2
      2 * radius_km * atan2(sqrt(a), sqrt(1 - a))
    }

    point_in_polygon <- function(x, y, polygon) {
      n <- nrow(polygon)
      if (n < 3) {
        return(FALSE)
      }

      inside <- FALSE
      j <- n
      for (i in seq_len(n)) {
        yi <- polygon$latitude[i]
        yj <- polygon$latitude[j]
        xi <- polygon$longitude[i]
        xj <- polygon$longitude[j]
        intersects <- ((yi > y) != (yj > y)) &&
          (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
        if (intersects) {
          inside <- !inside
        }
        j <- i
      }
      inside
    }

    trap_locations <- trap_source[
      !duplicated(trap_source$trap_id),
      c("trap_id", "latitude", "longitude")
    ]
    trap_locations$trap_id <- as.character(trap_locations$trap_id)
    trap_locations$latitude <- suppressWarnings(as.numeric(trap_locations$latitude))
    trap_locations$longitude <- suppressWarnings(as.numeric(trap_locations$longitude))

    valid_traps <- is.finite(trap_locations$latitude) & is.finite(trap_locations$longitude)
    if (!any(valid_traps)) {
      return(empty_assignment)
    }

    if (!"locationName" %in% names(monitoring_deployments)) {
      monitoring_deployments$locationName <- NA_character_
    }

    monitoring_locations <- monitoring_deployments[
      !is.na(monitoring_deployments$locality) &
        nzchar(trimws(as.character(monitoring_deployments$locality))) &
        is.finite(suppressWarnings(as.numeric(monitoring_deployments$latitude))) &
        is.finite(suppressWarnings(as.numeric(monitoring_deployments$longitude))),
      c("locality", "locationName", "latitude", "longitude")
    ]

    if (nrow(monitoring_locations) == 0) {
      return(empty_assignment)
    }

    monitoring_locations$locality <- as.character(monitoring_locations$locality)
    monitoring_locations$locationName <- as.character(monitoring_locations$locationName)
    monitoring_locations$latitude <- suppressWarnings(as.numeric(monitoring_locations$latitude))
    monitoring_locations$longitude <- suppressWarnings(as.numeric(monitoring_locations$longitude))
    monitoring_locations <- monitoring_locations[!duplicated(monitoring_locations), ]

    locality_points <- split(monitoring_locations, monitoring_locations$locality)
    locality_hulls <- lapply(locality_points, function(points) {
      distinct_points <- unique(points[c("longitude", "latitude")])
      if (nrow(distinct_points) < 3) {
        return(NULL)
      }
      hull_indices <- tryCatch(grDevices::chull(distinct_points$longitude, distinct_points$latitude), error = function(e) integer())
      if (length(hull_indices) < 3) {
        return(NULL)
      }
      distinct_points[hull_indices, , drop = FALSE]
    })
    locality_hulls <- locality_hulls[!vapply(locality_hulls, is.null, logical(1))]

    assignment <- empty_assignment
    for (i in seq_len(nrow(assignment))) {
      trap_row <- trap_locations[trap_locations$trap_id == assignment$trap_id[i], , drop = FALSE]
      if (nrow(trap_row) == 0 ||
          !is.finite(trap_row$latitude[[1]]) ||
          !is.finite(trap_row$longitude[[1]])) {
        next
      }

      containing_localities <- names(locality_hulls)[vapply(locality_hulls, function(hull) {
        point_in_polygon(trap_row$longitude[[1]], trap_row$latitude[[1]], hull)
      }, logical(1))]

      if (length(containing_localities) > 0) {
        assignment$locality[i] <- containing_localities[[1]]
        assignment$locality_match_type[i] <- "within"
        assignment$locality_distance_km[i] <- 0
        next
      }

      nearest_distances <- vapply(names(locality_points), function(locality_name) {
        points <- locality_points[[locality_name]]
        min(
          haversine_km(
            trap_row$longitude[[1]],
            trap_row$latitude[[1]],
            points$longitude,
            points$latitude
          ),
          na.rm = TRUE
        )
      }, numeric(1))

      nearest_locality <- names(nearest_distances)[which.min(nearest_distances)]
      nearest_points <- locality_points[[nearest_locality]]
      nearest_point_distances <- haversine_km(
        trap_row$longitude[[1]],
        trap_row$latitude[[1]],
        nearest_points$longitude,
        nearest_points$latitude
      )
      nearest_point <- nearest_points[which.min(nearest_point_distances), , drop = FALSE]
      assignment$locality[i] <- nearest_locality
      assignment$locality_match_type[i] <- "nearest"
      assignment$locality_distance_km[i] <- round(nearest_distances[[nearest_locality]], 2)
      assignment$nearest_monitoring_locationName[i] <- nearest_point$locationName[[1]]
    }

    assignment
  }

  write_import_log <- function(summary, missing_coordinate_traps, first_interval_starts) {
    if (is.null(log_file) || is.na(log_file) || !nzchar(log_file)) {
      return(invisible(NULL))
    }

    tryCatch({
      log_dir <- dirname(log_file)
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
      }

      lines <- c(
        sprintf("WKT trap data import - %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "",
        "Import summary:",
        sprintf("- source_trap_records: %s", summary$source_trap_records),
        sprintf("- converted_trap_records: %s", summary$converted_trap_records),
        sprintf("- deployments: %s", summary$deployments),
        sprintf("- observations: %s", summary$observations),
        sprintf("- animal_observations: %s", summary$animal_observations),
        sprintf("- prior_check_overrides: %s", summary$prior_check_overrides),
        sprintf("- first_checks_without_prior: %s", summary$first_checks_without_prior),
        sprintf("- dropped_first_checks: %s", summary$dropped_first_checks),
        sprintf("- dropped_missing_coordinate_records: %s", summary$dropped_missing_coordinate_records),
        sprintf("- traps_with_deployments: %s", summary$traps_with_deployments),
        sprintf("- trap_locality_within: %s", summary$trap_locality_within),
        sprintf("- trap_locality_nearest: %s", summary$trap_locality_nearest),
        sprintf("- trap_locality_unassigned: %s", summary$trap_locality_unassigned),
        "",
        "Exceptions:"
      )

      if (length(missing_coordinate_traps) > 0) {
        lines <- c(
          lines,
          "- trap_codes without coordinates:",
          paste0("  - ", sort(unique(missing_coordinate_traps)))
        )
      } else {
        lines <- c(lines, "- trap_codes without coordinates: none")
      }

      unmapped <- summary$unmapped_kill_outcome_ids
      if (length(unmapped) > 0) {
        lines <- c(
          lines,
          "- unmapped kill outcome_ids:",
          paste0("  - ", unmapped)
        )
      } else {
        lines <- c(lines, "- unmapped kill outcome_ids: none")
      }

      lines <- c(lines, "", "First deploymentStart with interval_days by trap_code:")
      if (nrow(first_interval_starts) > 0) {
        lines <- c(
          lines,
          sprintf(
            "- %s: %s",
            first_interval_starts$trap_code,
            first_interval_starts$first_deploymentStart
          )
        )
      } else {
        lines <- c(lines, "- none")
      }

      cat(paste0(lines, collapse = "\n"), "\n\n", file = log_file, append = TRUE)
    }, error = function(e) {
      invisible(NULL)
    })

    invisible(NULL)
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

  if (!is.null(kill_prior_check_override_days)) {
    if (!is.numeric(kill_prior_check_override_days) ||
        length(kill_prior_check_override_days) != 1 ||
        is.na(kill_prior_check_override_days) ||
        kill_prior_check_override_days <= 0) {
      stop(
        "kill_prior_check_override_days must be a single positive number or NULL.",
        call. = FALSE
      )
    }
    kill_prior_check_override_days <- as.integer(kill_prior_check_override_days)
  }

  required_columns(
    raw_trap_data,
    c("trapdata_id", "date", "trap_id", "outcome_id"),
    "raw_trap_data"
  )
  if (!"latitude" %in% names(trap_locations) && "lat" %in% names(trap_locations)) {
    trap_locations$latitude <- trap_locations$lat
  }
  if (!"longitude" %in% names(trap_locations) && "long" %in% names(trap_locations)) {
    trap_locations$longitude <- trap_locations$long
  }

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
  if (!"basecol" %in% names(reference_tables)) {
    reference_tables$basecol <- NA_character_
  }
  species_map$outcome_id <- as.character(species_map$outcome_id)

  if (!"project_id" %in% names(trap_locations)) {
    trap_locations$project_id <- NA_character_
  }
  if (!"trap_type_id" %in% names(trap_locations)) {
    trap_locations$trap_type_id <- NA_character_
  }

  trap_locations$trap_type_id <- as.character(trap_locations$trap_type_id)

  if (all(c("trap_type_id", "trap_type_desc") %in% names(reference_tables))) {
    trap_type_reference <- reference_tables[
      !is.na(reference_tables$trap_type_id) & nzchar(as.character(reference_tables$trap_type_id)),
      c("trap_type_id", "trap_type_desc")
    ]
    trap_type_reference$trap_type_id <- as.character(trap_type_reference$trap_type_id)
    trap_type_reference <- trap_type_reference[!duplicated(trap_type_reference$trap_type_id), ]

    trap_locations <- merge(
      trap_locations,
      trap_type_reference,
      by = "trap_type_id",
      all.x = TRUE
    )
  } else {
    trap_locations$trap_type_desc <- NA_character_
  }

  if ("bait_outcome_id" %in% names(raw_trap_data)) {
    raw_trap_data$bait_outcome_id <- as.character(raw_trap_data$bait_outcome_id)
  } else {
    raw_trap_data$bait_outcome_id <- NA_character_
  }

  if (all(c("bait_outcome_id", "bait_outcome_desc") %in% names(reference_tables))) {
    bait_outcome_reference <- reference_tables[
      !is.na(reference_tables$bait_outcome_id) & nzchar(as.character(reference_tables$bait_outcome_id)),
      c("bait_outcome_id", "bait_outcome_desc")
    ]
    bait_outcome_reference$bait_outcome_id <- as.character(bait_outcome_reference$bait_outcome_id)
    bait_outcome_reference <- bait_outcome_reference[!duplicated(bait_outcome_reference$bait_outcome_id), ]

    raw_trap_data <- merge(
      raw_trap_data,
      bait_outcome_reference,
      by = "bait_outcome_id",
      all.x = TRUE
    )
  } else {
    raw_trap_data$bait_outcome_desc <- NA_character_
  }

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
  raw_trap_data$trap_code <- coalesce_character(raw_trap_data$code, raw_trap_data$trap_id)

  raw_trap_data <- merge(
    raw_trap_data,
    reference_tables[c("outcome_id", "description", "basecol", "kill")],
    by = "outcome_id",
    all.x = TRUE
  )

  raw_trap_data <- merge(raw_trap_data, species_map, by = "outcome_id", all.x = TRUE)

  pre_coordinate_filter_count <- nrow(raw_trap_data)
  missing_coordinate_traps <- unique(raw_trap_data$trap_code[
    is.na(raw_trap_data$latitude) | is.na(raw_trap_data$longitude)
  ])
  missing_coordinate_traps <- missing_coordinate_traps[
    !is.na(missing_coordinate_traps) & nzchar(missing_coordinate_traps)
  ]
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

  first_checks_without_prior <- sum(is.na(check_events$previous_check_date))
  dropped_first_checks <- 0
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
      "trap_type_id", "trap_type_desc", "latitude", "longitude", "check_date",
      "previous_check_date", "trap_code"
    )
  ]
  deployment_source$interval_days <- as.integer(
    deployment_source$check_date - deployment_source$previous_check_date
  )
  locality_assignment <- assign_trap_localities(deployment_source, monitoring_deployments)
  deployment_source <- merge(
    deployment_source,
    locality_assignment,
    by = "trap_id",
    all.x = TRUE,
    sort = FALSE
  )

  deployments <- data.frame(
    deploymentID = deployment_source$deploymentID,
    locationID = clean_id(deployment_source$trap_code, "wkt-trap-location"),
    locationName = deployment_source$trap_code,
    latitude = deployment_source$latitude,
    longitude = deployment_source$longitude,
    coordinateUncertainty = NA,
    deploymentStart = format_camtrap_time(deployment_source$previous_check_date),
    deploymentEnd = format_camtrap_time(deployment_source$check_date),
    interval_days = deployment_source$interval_days,
    setupBy = NA,
    cameraID = deployment_source$trap_code,
    cameraModel = ifelse(
      is.na(deployment_source$trap_type_desc) | !nzchar(trimws(deployment_source$trap_type_desc)),
      "predator trap",
      deployment_source$trap_type_desc
    ),
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
    locality = deployment_source$locality,
    locality_match_type = deployment_source$locality_match_type,
    locality_distance_km = deployment_source$locality_distance_km,
    nearest_monitoring_locationName = deployment_source$nearest_monitoring_locationName,
    deploymentGroups = paste("line", deployment_source$line_id),
    deploymentTags = mapply(
      function(trap_id, trap_number, project_id, trap_type_id, trap_type_desc) {
        build_tags(c(
          paste0("source_trap_id:", trap_id),
          paste0("trap_number:", trap_number),
          paste0("project_id:", project_id),
          paste0("trap_type_id:", trap_type_id),
          paste0("trap_type:", trap_type_desc)
        ))
      },
      deployment_source$trap_id,
      deployment_source$trap_number,
      deployment_source$project_id,
      deployment_source$trap_type_id,
      deployment_source$trap_type_desc,
      USE.NAMES = FALSE
    ),
    deploymentComments = paste(
      "Converted from WKT trap check data.",
      "Deployment start is the previous check date and deployment end is the current check date."
    ),
    stringsAsFactors = FALSE
  )

  raw_trap_data$mapped_kill <- raw_trap_data$kill == 1 & !is.na(raw_trap_data$scientificName)
  raw_trap_data$behavior <- trap_check_behavior(
    raw_trap_data$mapped_kill,
    raw_trap_data$basecol,
    raw_trap_data$description
  )
  raw_trap_data$original_check_interval <- as.integer(
    raw_trap_data$check_date - raw_trap_data$previous_check_date
  )
  raw_trap_data$effective_previous_check_date <- raw_trap_data$previous_check_date
  raw_trap_data$prior_check_override_applied <- FALSE
  raw_trap_data$prior_check_override_note <- NA_character_

  if (!is.null(kill_prior_check_override_days)) {
    override_prior_check <- raw_trap_data$behavior == "killed" &
      !is.na(raw_trap_data$original_check_interval) &
      raw_trap_data$original_check_interval > kill_prior_check_override_days

    raw_trap_data$effective_previous_check_date[override_prior_check] <-
      raw_trap_data$check_date[override_prior_check] - kill_prior_check_override_days
    raw_trap_data$prior_check_override_applied[override_prior_check] <- TRUE

    raw_trap_data$prior_check_override_note[override_prior_check] <- paste0(
      "prior_check_override:kill interval capped from ",
      as.character(raw_trap_data$previous_check_date[override_prior_check]),
      " to ",
      as.character(raw_trap_data$effective_previous_check_date[override_prior_check]),
      " (source_interval_days:",
      raw_trap_data$original_check_interval[override_prior_check],
      "; max_days:",
      kill_prior_check_override_days,
      ")"
    )
  }
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
    prior_check_date = raw_trap_data$effective_previous_check_date,
    check_interval = as.integer(raw_trap_data$check_date - raw_trap_data$effective_previous_check_date),
    interval_days = as.integer(raw_trap_data$check_date - raw_trap_data$effective_previous_check_date),
    source_prior_check_date = raw_trap_data$previous_check_date,
    source_interval_days = raw_trap_data$original_check_interval,
    prior_check_override_applied = raw_trap_data$prior_check_override_applied,
    observationLevel = "event",
    observationType = ifelse(raw_trap_data$mapped_kill, "animal", "blank"),
    cameraSetupType = NA,
    scientificName = ifelse(raw_trap_data$mapped_kill, raw_trap_data$scientificName, NA),
    count = ifelse(raw_trap_data$mapped_kill, 1, NA),
    lifeStage = raw_trap_data$lifeStage,
    sex = raw_trap_data$sex,
    behavior = raw_trap_data$behavior,
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
      function(trapdata_id, baitstatus, bait_outcome_id, bait_outcome_desc, prior_check_override_note) {
        build_tags(c(
          paste0("source_trapdata_id:", trapdata_id),
          paste0("baitstatus:", baitstatus),
          paste0("bait_outcome_id:", bait_outcome_id),
          paste0("bait_outcome:", bait_outcome_desc),
          prior_check_override_note
        ))
      },
      raw_trap_data$trapdata_id,
      raw_trap_data$baitstatus,
      raw_trap_data$bait_outcome_id,
      raw_trap_data$bait_outcome_desc,
      raw_trap_data$prior_check_override_note,
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

  trap_summary <- deployment_source[
    !duplicated(deployment_source$trap_id),
    c(
      "trap_id", "trap_code", "line_id", "trap_number", "trap_type_id",
      "trap_type_desc", "locality", "locality_match_type",
      "locality_distance_km", "nearest_monitoring_locationName",
      "latitude", "longitude"
    )
  ]
  trap_summary <- trap_summary[order(trap_summary$trap_id), ]
  trap_summary$earliest_deploymentStart <- vapply(trap_summary$trap_id, function(trap_id) {
    starts <- deployments$deploymentStart[
      deployment_source$trap_id[match(deployments$deploymentID, deployment_source$deploymentID)] == trap_id &
        !is.na(deployments$deploymentStart)
    ]
    if (length(starts) == 0) {
      return(NA_character_)
    }
    min(starts)
  }, character(1))
  trap_summary$latest_deploymentEnd <- vapply(trap_summary$trap_id, function(trap_id) {
    ends <- deployments$deploymentEnd[
      deployment_source$trap_id[match(deployments$deploymentID, deployment_source$deploymentID)] == trap_id &
        !is.na(deployments$deploymentEnd)
    ]
    if (length(ends) == 0) {
      return(NA_character_)
    }
    max(ends)
  }, character(1))
  trap_summary$times_checked <- as.integer(vapply(trap_summary$trap_id, function(trap_id) {
    sum(deployment_source$trap_id == trap_id)
  }, numeric(1)))
  trap_summary$average_interval_days <- vapply(trap_summary$trap_id, function(trap_id) {
    intervals <- deployment_source$interval_days[deployment_source$trap_id == trap_id]
    intervals <- intervals[!is.na(intervals)]
    if (length(intervals) == 0) {
      return(NA_real_)
    }
    round(mean(intervals), 1)
  }, numeric(1))
  trap_summary$total_kills <- as.integer(vapply(trap_summary$trap_id, function(trap_id) {
    sum(
      raw_trap_data$trap_id == trap_id &
        raw_trap_data$mapped_kill,
      na.rm = TRUE
    )
  }, numeric(1)))
  mustelid_names <- c(
    "Mustelidae",
    "Mustela erminea",
    "Mustela nivalis",
    "Mustela putorius furo"
  )
  trap_summary$total_kills_mustelids <- as.integer(vapply(trap_summary$trap_id, function(trap_id) {
    sum(
      raw_trap_data$trap_id == trap_id &
        raw_trap_data$mapped_kill &
        raw_trap_data$scientificName %in% mustelid_names,
      na.rm = TRUE
    )
  }, numeric(1)))

  kill_species <- sort(unique(stats::na.omit(raw_trap_data$scientificName[
    raw_trap_data$mapped_kill
  ])))
  for (scientific_name in kill_species) {
    column_name <- clean_column_name(scientific_name, "kills")
    trap_summary[[column_name]] <- as.integer(vapply(trap_summary$trap_id, function(trap_id) {
      sum(
        raw_trap_data$trap_id == trap_id &
          raw_trap_data$mapped_kill &
          raw_trap_data$scientificName == scientific_name,
        na.rm = TRUE
      )
    }, numeric(1)))
  }

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

  temporal_start_dates <- as.Date(deployment_source$previous_check_date)
  temporal_start_dates <- temporal_start_dates[!is.na(temporal_start_dates)]
  temporal_start <- if (length(temporal_start_dates) > 0) {
    as.character(min(temporal_start_dates))
  } else {
    NA_character_
  }
  oldest_trap_check_date <- min(as.Date(deployment_source$check_date), na.rm = TRUE)
  newest_trap_check_date <- max(as.Date(deployment_source$check_date), na.rm = TRUE)
  trap_data_period_days <- as.integer(newest_trap_check_date - oldest_trap_check_date)

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
      start = temporal_start,
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
      ),
      list(
        name = "trap_summary",
        path = "trap_summary.csv",
        profile = "tabular-data-resource",
        format = "csv",
        mediatype = "text/csv",
        encoding = "utf-8"
      )
    )
  )

  summary <- list(
    source_trap_records = source_trap_record_count,
    converted_trap_records = nrow(raw_trap_data),
    deployments = nrow(deployments),
    observations = nrow(observations),
    animal_observations = sum(observations$observationType == "animal", na.rm = TRUE),
    prior_check_overrides = sum(raw_trap_data$prior_check_override_applied, na.rm = TRUE),
    oldest_trap_check_date = as.character(oldest_trap_check_date),
    newest_trap_check_date = as.character(newest_trap_check_date),
    trap_data_period_days = trap_data_period_days,
    first_checks_without_prior = first_checks_without_prior,
    dropped_first_checks = dropped_first_checks,
    dropped_missing_coordinate_records = dropped_missing_coordinate_records,
    traps_with_deployments = length(unique(deployment_source$trap_id)),
    trap_locality_within = sum(!duplicated(deployment_source$trap_id) &
      deployment_source$locality_match_type == "within", na.rm = TRUE),
    trap_locality_nearest = sum(!duplicated(deployment_source$trap_id) &
      deployment_source$locality_match_type == "nearest", na.rm = TRUE),
    trap_locality_unassigned = sum(!duplicated(deployment_source$trap_id) &
      (is.na(deployment_source$locality) | !nzchar(deployment_source$locality)), na.rm = TRUE),
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
    media = media,
    trap_summary = trap_summary,
    taxonomic = taxonomic,
    datapackage = datapackage,
    summary = summary
  )

  interval_deployments <- deployments[!is.na(deployments$interval_days), ]
  if (nrow(interval_deployments) > 0) {
    first_interval_starts <- stats::aggregate(
      deploymentStart ~ locationName,
      data = interval_deployments,
      FUN = min
    )
    names(first_interval_starts) <- c("trap_code", "first_deploymentStart")
    first_interval_starts <- first_interval_starts[order(first_interval_starts$trap_code), ]
  } else {
    first_interval_starts <- data.frame(
      trap_code = character(),
      first_deploymentStart = character(),
      stringsAsFactors = FALSE
    )
  }

  write_import_log(summary, missing_coordinate_traps, first_interval_starts)

  if (!is.null(period_groups)) {
    result <- annotate_wkt_trap_periods(result, period_groups)
    deployments <- result$deps
    observations <- result$obs
    media <- result$media
    trap_summary <- result$trap_summary
  }

  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    utils::write.csv(deployments, file.path(output_dir, "deployments.csv"), row.names = FALSE, na = "")
    utils::write.csv(media, file.path(output_dir, "media.csv"), row.names = FALSE, na = "")
    utils::write.csv(observations, file.path(output_dir, "observations.csv"), row.names = FALSE, na = "")
    utils::write.csv(trap_summary, file.path(output_dir, "trap_summary.csv"), row.names = FALSE, na = "")

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

  assignable_periods <- period_groups[!names(period_groups) %in% "ALL"]
  assignable_periods <- Filter(function(period) {
    is.null(period$assign_period) || isTRUE(period$assign_period)
  }, assignable_periods)
  period_levels <- names(assignable_periods)

  period_interval <- function(period) {
    data.frame(
      start = as.Date(period$start_date),
      end = as.Date(period$end_date),
      stringsAsFactors = FALSE
    )
  }

  period_intervals <- if (length(assignable_periods) > 0) {
    do.call(rbind, lapply(assignable_periods, period_interval))
  } else {
    data.frame(start = as.Date(character()), end = as.Date(character()))
  }

  overlapping_trap_period <- function(start_date, end_date) {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    if (is.na(start_date) || is.na(end_date) || length(period_levels) == 0) {
      return(NA_character_)
    }

    matches <- period_levels[
      period_intervals$start <= end_date & period_intervals$end >= start_date
    ]
    if (length(matches) == 0) {
      return(NA_character_)
    }

    matches[[1]]
  }

  trap_data$deps$check_date <- as.Date(trap_data$deps$deploymentEnd)
  trap_data$deps$prior_check_date <- as.Date(trap_data$deps$deploymentStart)
  trap_data$deps$period <- factor(
    mapply(
      overlapping_trap_period,
      trap_data$deps$prior_check_date,
      trap_data$deps$check_date,
      USE.NAMES = FALSE
    ),
    levels = period_levels
  )

  trap_data$obs$check_date <- as.Date(trap_data$obs$eventStart)
  if (!"prior_check_date" %in% names(trap_data$obs)) {
    trap_data$obs$prior_check_date <- as.Date(trap_data$obs$eventStart)
  } else {
    trap_data$obs$prior_check_date <- as.Date(trap_data$obs$prior_check_date)
  }
  trap_data$obs$period <- factor(
    mapply(
      overlapping_trap_period,
      trap_data$obs$prior_check_date,
      trap_data$obs$check_date,
      USE.NAMES = FALSE
    ),
    levels = period_levels
  )

  if (!is.null(trap_data$trap_summary) &&
      "trap_id" %in% names(trap_data$trap_summary) &&
      "locationName" %in% names(trap_data$deps) &&
      "period" %in% names(trap_data$deps)) {
    trap_period_summary <- trap_data$deps %>%
      dplyr::filter(!is.na(.data$period)) %>%
      dplyr::group_by(trap_id = .data$locationName) %>%
      dplyr::summarise(
        period = paste(unique(as.character(.data$period)), collapse = "; "),
        .groups = "drop"
      )

    if (nrow(trap_period_summary) > 0) {
      trap_data$trap_summary <- trap_data$trap_summary %>%
        dplyr::left_join(trap_period_summary, by = "trap_id")
    }
  }

  trap_data
}
