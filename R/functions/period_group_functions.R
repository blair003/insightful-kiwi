is_period_group_entry <- function(value) {
  !is.null(value) && all(c("start_date", "end_date") %in% names(value))
}

period_groups_are_structured <- function(period_groups) {
  !is.null(period_groups) &&
    length(period_groups) > 0 &&
    !all(vapply(period_groups, is_period_group_entry, logical(1)))
}

period_selector_families <- function(config = NULL, include_all = TRUE) {
  families <- if (!is.null(config)) {
    config_global_value(config, "period_selector_families", c("calendar_season", "calendar_year", "all"))
  } else {
    c("calendar_season", "calendar_year", "all")
  }

  families <- as.character(unlist(families, use.names = FALSE))
  families <- families[!is.na(families) & nzchar(families)]
  if (isTRUE(include_all) && !("all" %in% families)) {
    families <- c(families, "all")
  }

  unique(families)
}

period_family_label <- function(period_family) {
  labels <- c(
    calendar_season = "Calendar seasons",
    calendar_year = "Calendar years",
    all = "Whole project",
    monitoring_period = "Monitoring periods"
  )

  label <- labels[[period_family]]
  if (is.null(label)) {
    label <- gsub("_", " ", period_family)
    label <- paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
  }

  label
}

flatten_period_groups <- function(period_groups, families = NULL, include_all = TRUE) {
  if (is.null(period_groups) || length(period_groups) == 0) {
    return(list())
  }

  if (!period_groups_are_structured(period_groups)) {
    return(period_groups)
  }

  if (is.null(families)) {
    families <- names(period_groups)
  } else {
    families <- as.character(unlist(families, use.names = FALSE))
    if (isTRUE(include_all) && "all" %in% names(period_groups) && !("all" %in% families)) {
      families <- c(families, "all")
    }
  }

  families <- families[families %in% names(period_groups)]
  flattened <- list()
  for (family in families) {
    family_groups <- period_groups[[family]]
    if (is.null(family_groups) || length(family_groups) == 0) {
      next
    }
    for (period_name in names(family_groups)) {
      period_info <- family_groups[[period_name]]
      if (!("period_family" %in% names(period_info))) {
        period_info$period_family <- family
      }
      flattened[[period_name]] <- period_info
    }
  }

  flattened
}

period_group_by_name <- function(period_groups, period_name, families = NULL) {
  flat_period_groups <- flatten_period_groups(period_groups, families = families)
  if (length(period_name) != 1 || is.na(period_name) || !(period_name %in% names(flat_period_groups))) {
    return(NULL)
  }

  flat_period_groups[[period_name]]
}

period_selection_choices <- function(period_groups,
                                     config = NULL,
                                     families = NULL,
                                     assignable_only = FALSE) {
  if (is.null(families)) {
    families <- period_selector_families(config)
  }

  if (!period_groups_are_structured(period_groups)) {
    choice_names <- period_names_without_all(period_groups, assignable_only = assignable_only)
    if ("ALL" %in% names(period_groups)) {
      choice_names <- c(choice_names, "ALL")
    }
    return(choice_names)
  }

  choices <- list()
  for (family in families) {
    family_groups <- period_groups[[family]]
    if (is.null(family_groups) || length(family_groups) == 0) {
      next
    }

    family_names <- if (identical(family, "all")) {
      names(family_groups)
    } else {
      period_names_without_all(
        family_groups,
        assignable_only = assignable_only
      )
    }
    if (length(family_names) > 0) {
      choices[[period_family_label(family)]] <- stats::setNames(family_names, family_names)
    }
  }

  choices
}

period_names_without_all <- function(period_groups, assignable_only = TRUE, families = NULL) {
  flat_period_groups <- flatten_period_groups(period_groups, families = families)
  period_names <- names(flat_period_groups)
  period_names <- period_names[period_names != "ALL"]

  if (isTRUE(assignable_only)) {
    period_names <- period_names[vapply(period_names, function(period_name) {
      period_info <- flat_period_groups[[period_name]]
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

  split_year_match <- regexec("^(.+)\\s+(\\d{4})-(\\d{4})$", period_name)
  split_year_parts <- regmatches(period_name, split_year_match)[[1]]
  if (length(split_year_parts) == 4) {
    matching_period <- sprintf(
      "%s %s-%s",
      trimws(split_year_parts[[2]]),
      as.integer(split_year_parts[[3]]) - 1L,
      as.integer(split_year_parts[[4]]) - 1L
    )
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
    if (fallback %in% period_names || fallback %in% names(flatten_period_groups(period_groups))) {
      return(fallback)
    }
  }

  if (length(period_names) > 0) {
    return(period_names[[1]])
  }
  flat_period_groups <- flatten_period_groups(period_groups)
  if (length(names(flat_period_groups)) > 0) {
    return(names(flat_period_groups)[[1]])
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
  flat_period_groups <- flatten_period_groups(period_groups)
  period_names <- period_names_without_all(period_groups)

  lapply(period_names, function(period_name) {
    period_info <- flat_period_groups[[period_name]]
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
  } else if (length(names(flatten_period_groups(period_groups))) > 0) {
    names(flatten_period_groups(period_groups))[[1]]
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

update_all_period_bounds_from_observations <- function(core_data, trap_data = NULL, config) {
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

  if (!is.null(core_data$period_groups$all$ALL)) {
    core_data$period_groups$all$ALL$start_date <- min(observation_dates)
    core_data$period_groups$all$ALL$end_date <- max(observation_dates)
  }

  core_data
}
