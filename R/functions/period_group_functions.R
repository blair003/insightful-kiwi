period_names_without_all <- function(period_groups, assignable_only = TRUE) {
  period_names <- names(period_groups)
  period_names <- period_names[period_names != "ALL"]

  if (isTRUE(assignable_only)) {
    period_names <- period_names[vapply(period_names, function(period_name) {
      period_info <- period_groups[[period_name]]
      is.null(period_info$assign_period) || isTRUE(period_info$assign_period)
    }, logical(1))]
  }

  period_names
}

find_matching_prior_year_period <- function(period_name, period_groups) {
  if (length(period_name) != 1 || is.na(period_name) || !nzchar(as.character(period_name))) {
    return(NA_character_)
  }
  period_name <- as.character(period_name)

  period_names <- period_names_without_all(period_groups)
  if (grepl("^\\d{4}$", period_name)) {
    matching_period <- as.character(as.integer(period_name) - 1)
    if (matching_period %in% period_names) {
      return(matching_period)
    }

    return(NA_character_)
  }

  period_match <- regexec("^(.+)\\s+(\\d{4})$", period_name)
  period_parts <- regmatches(period_name, period_match)[[1]]

  if (length(period_parts) != 3) {
    return(NA_character_)
  }

  matching_period <- paste(trimws(period_parts[[2]]), as.integer(period_parts[[3]]) - 1)
  if (matching_period %in% period_names) {
    return(matching_period)
  }

  NA_character_
}

period_index_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_integer_)
  }

  suppressWarnings(as.integer(value[[1]]))
}

normalise_period_index <- function(period_groups,
                                   period_index = NULL,
                                   period_name = NULL,
                                   default = 1L,
                                   assignable_only = TRUE) {
  period_names <- period_names_without_all(period_groups, assignable_only = assignable_only)
  if (length(period_names) == 0) {
    return(NA_integer_)
  }

  if (length(period_name) == 1 && !is.na(period_name)) {
    matched_index <- match(as.character(period_name), period_names)
    if (!is.na(matched_index)) {
      return(as.integer(matched_index))
    }
  }

  resolved_index <- period_index_value(period_index)
  if (is.na(resolved_index)) {
    resolved_index <- period_index_value(default)
  }
  if (is.na(resolved_index)) {
    resolved_index <- 1L
  }

  as.integer(min(max(resolved_index, 1L), length(period_names)))
}

period_name_from_index <- function(period_groups,
                                   period_index = NULL,
                                   period_name = NULL,
                                   fallback = NULL,
                                   assignable_only = TRUE) {
  period_names <- period_names_without_all(period_groups, assignable_only = assignable_only)

  if (length(period_name) == 1 && !is.na(period_name)) {
    period_name <- as.character(period_name)
    if (period_name %in% period_names) {
      return(period_name)
    }
  }

  if (!is.null(period_index) && length(period_index) > 0) {
    period_index_name <- as.character(period_index[[1]])
    if (!is.na(period_index_name) && period_index_name %in% period_names) {
      return(period_index_name)
    }

    period_index <- normalise_period_index(
      period_groups,
      period_index = period_index,
      assignable_only = assignable_only
    )
    if (!is.na(period_index)) {
      return(period_names[[period_index]])
    }
  }

  if (length(fallback) == 1 && !is.na(fallback)) {
    fallback <- as.character(fallback)
    if (fallback %in% period_names || fallback %in% names(period_groups)) {
      return(fallback)
    }
  }

  if (length(period_names) > 0) {
    return(period_names[[1]])
  }
  if (length(names(period_groups)) > 0) {
    return(names(period_groups)[[1]])
  }

  NA_character_
}

period_names_from_index <- function(period_groups,
                                    period_index = NULL,
                                    period_name = NULL,
                                    assignable_only = TRUE) {
  period_names <- period_names_without_all(period_groups, assignable_only = assignable_only)
  if (length(period_names) == 0) {
    return(character(0))
  }

  period_index <- normalise_period_index(
    period_groups,
    period_index = period_index,
    period_name = period_name,
    assignable_only = assignable_only
  )
  if (is.na(period_index)) {
    return(character(0))
  }

  period_names[seq.int(period_index, length(period_names))]
}

get_period_index <- function(period_groups, period_name) {
  period_index <- normalise_period_index(period_groups, period_name = period_name)
  if (is.na(period_index)) {
    return(1L)
  }

  period_index
}

is_deployment_annotation_complete <- function(deps) {
  max_normal_unclassified_count <- 2L
  completed_count <- rowSums(
    data.frame(
      animal = deps$animal_detections_count,
      blank = deps$blank_detections_count,
      unknown = deps$unknown_detections_count
    ),
    na.rm = TRUE
  )
  unclassified_count <- dplyr::coalesce(deps$unclassified_detections_count, 0)

  completed_count > unclassified_count |
    unclassified_count <= max_normal_unclassified_count
}

summarise_period_annotation_completeness <- function(deps, period_groups) {
  period_names <- period_names_without_all(period_groups)

  lapply(period_names, function(period_name) {
    period_info <- period_groups[[period_name]]
    period_uses_canonical_assignment <- is.null(period_info$assign_period) ||
      isTRUE(period_info$assign_period)

    period_deps <- if ("period" %in% names(deps) && period_uses_canonical_assignment) {
      deps %>% dplyr::filter(as.character(period) == period_name)
    } else {
      deps %>%
        dplyr::filter(
          start <= as.Date(.env$period_info$end_date),
          end >= as.Date(.env$period_info$start_date)
        )
    }

    deployment_complete <- if (nrow(period_deps) > 0) {
      is_deployment_annotation_complete(period_deps)
    } else {
      logical(0)
    }

    data.frame(
      period = period_name,
      deployment_count = nrow(period_deps),
      incomplete_deployment_count = sum(!deployment_complete),
      is_complete = nrow(period_deps) > 0 && all(deployment_complete),
      stringsAsFactors = FALSE
    )
  }) %>%
    dplyr::bind_rows()
}

get_default_complete_period_selection <- function(deps, period_groups, config) {
  period_names <- period_names_without_all(period_groups)
  completion_summary <- summarise_period_annotation_completeness(deps, period_groups)
  complete_periods <- completion_summary$period[completion_summary$is_complete]

  fallback_primary_period <- if (length(complete_periods) > 0) {
    complete_periods[[1]]
  } else if (length(period_names) > 0) {
    period_names[[1]]
  } else if (length(names(period_groups)) > 0) {
    names(period_groups)[[1]]
  } else {
    NA_character_
  }

  default_primary_period <- config_global_value(config, "default_primary_period")
  default_comparative_period <- config_global_value(config, "default_comparative_period")

  primary_period <- period_name_from_index(
    period_groups,
    period_index = default_primary_period,
    fallback = fallback_primary_period
  )

  fallback_comparative_period <- find_matching_prior_year_period(primary_period, period_groups)
  if (is.na(fallback_comparative_period)) {
    primary_index <- match(primary_period, period_names)
    fallback_comparative_period <- if (!is.na(primary_index) && primary_index < length(period_names)) {
      period_names[[primary_index + 1]]
    } else {
      primary_period
    }
  }

  comparative_period <- period_name_from_index(
    period_groups,
    period_index = default_comparative_period,
    fallback = fallback_comparative_period
  )

  list(
    primary_period = primary_period,
    primary_period_index = get_period_index(period_groups, primary_period),
    comparative_period = comparative_period,
    comparative_period_index = get_period_index(period_groups, comparative_period),
    completion_summary = completion_summary
  )
}

period_group_value <- function(period_info, field, default = NULL) {
  if (is.null(period_info) || !(field %in% names(period_info))) {
    return(default)
  }

  value <- period_info[[field]]
  if (length(value) == 0) {
    return(default)
  }

  value[[1]]
}

period_group_observation_dates <- function(core_data, trap_data = NULL, timezone = "UTC") {
  monitoring_dates <- as.Date(character())
  if (!is.null(core_data$obs) && "timestamp" %in% names(core_data$obs)) {
    monitoring_dates <- as.Date(core_data$obs$timestamp, tz = timezone)
  }

  trap_dates <- as.Date(character())
  if (!is.null(trap_data) && !is.null(trap_data$obs)) {
    if ("check_date" %in% names(trap_data$obs)) {
      trap_dates <- as.Date(trap_data$obs$check_date)
    } else if ("eventStart" %in% names(trap_data$obs)) {
      trap_dates <- parse_trap_period_date(trap_data$obs$eventStart, timezone)
    }
  }

  dates <- c(monitoring_dates, trap_dates)
  dates[!is.na(dates)]
}

parse_trap_period_date <- function(value, timezone = "UTC") {
  if (inherits(value, "Date")) {
    return(value)
  }
  if (inherits(value, "POSIXt")) {
    return(as.Date(value, tz = timezone))
  }

  raw_value <- as.character(value)
  raw_value[!nzchar(raw_value)] <- NA_character_
  date_prefix <- ifelse(
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", raw_value),
    substr(raw_value, 1, 10),
    raw_value
  )
  parsed <- suppressWarnings(as.Date(date_prefix))
  needs_posix <- is.na(parsed) & !is.na(raw_value)

  if (any(needs_posix)) {
    posix_value <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", raw_value[needs_posix])
    parsed_posix <- suppressWarnings(as.POSIXct(
      posix_value,
      tz = timezone,
      tryFormats = c(
        "%Y-%m-%dT%H:%M:%OS%z",
        "%Y-%m-%dT%H:%M:%S%z",
        "%Y-%m-%d %H:%M:%OS",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d"
      )
    ))
    parsed[needs_posix] <- as.Date(parsed_posix, tz = timezone)
  }

  parsed
}

update_year_period_bounds_from_observations <- function(core_data, trap_data = NULL, config) {
  if (is.null(core_data$period_groups) || length(core_data$period_groups) == 0) {
    return(core_data)
  }

  timezone <- config_actual_timezone(
    config,
    default = config_global_value(config, "timezone", "UTC")
  )

  observation_dates <- period_group_observation_dates(core_data, trap_data, timezone)
  if (length(observation_dates) == 0) {
    return(core_data)
  }

  for (period_name in names(core_data$period_groups)) {
    period_info <- core_data$period_groups[[period_name]]
    if (!identical(as.character(period_group_value(period_info, "period_type")), "year")) {
      next
    }

    period_year <- suppressWarnings(as.integer(period_name))
    if (is.na(period_year)) {
      period_year <- lubridate::year(period_group_value(period_info, "start_date"))
    }
    if (is.na(period_year)) {
      next
    }

    year_dates <- observation_dates[lubridate::year(observation_dates) == period_year]
    if (length(year_dates) == 0) {
      next
    }

    core_data$period_groups[[period_name]]$start_date <- as.POSIXct(
      paste(min(year_dates), "00:00:00"),
      tz = timezone
    )
    core_data$period_groups[[period_name]]$end_date <- as.POSIXct(
      paste(max(year_dates), "23:59:59"),
      tz = timezone
    )
  }

  core_data
}
