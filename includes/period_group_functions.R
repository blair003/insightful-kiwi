period_names_without_all <- function(period_groups) {
  period_names <- names(period_groups)
  period_names[period_names != "ALL"]
}

find_matching_prior_year_period <- function(period_name, period_groups) {
  period_names <- period_names_without_all(period_groups)
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

get_period_index <- function(period_groups, period_name) {
  period_index <- match(period_name, period_names_without_all(period_groups))

  if (is.na(period_index)) {
    return(1)
  }

  period_index
}

is_deployment_annotation_complete <- function(deps) {
  completed_count <- rowSums(
    data.frame(
      animal = deps$animal_detections_count,
      blank = deps$blank_detections_count,
      unknown = deps$unknown_detections_count
    ),
    na.rm = TRUE
  )
  unclassified_count <- dplyr::coalesce(deps$unclassified_detections_count, 0)

  completed_count > unclassified_count
}

summarise_period_annotation_completeness <- function(deps, period_groups) {
  period_names <- period_names_without_all(period_groups)

  lapply(period_names, function(period_name) {
    period_deps <- if ("period" %in% names(deps)) {
      deps %>% dplyr::filter(as.character(period) == period_name)
    } else {
      period <- period_groups[[period_name]]
      deps %>%
        dplyr::filter(
          start <= as.Date(period$end_date),
          end >= as.Date(period$start_date)
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

get_default_complete_period_selection <- function(deps, period_groups) {
  period_names <- period_names_without_all(period_groups)
  completion_summary <- summarise_period_annotation_completeness(deps, period_groups)
  complete_periods <- completion_summary$period[completion_summary$is_complete]

  primary_period <- if (length(complete_periods) > 0) {
    complete_periods[[1]]
  } else if (length(period_names) > 0) {
    period_names[[1]]
  } else {
    names(period_groups)[[1]]
  }

  comparative_period <- find_matching_prior_year_period(primary_period, period_groups)
  if (is.na(comparative_period)) {
    primary_index <- match(primary_period, period_names)
    comparative_period <- if (!is.na(primary_index) && primary_index < length(period_names)) {
      period_names[[primary_index + 1]]
    } else {
      primary_period
    }
  }

  list(
    primary_period = primary_period,
    primary_period_index = get_period_index(period_groups, primary_period),
    comparative_period = comparative_period,
    comparative_period_index = get_period_index(period_groups, comparative_period),
    completion_summary = completion_summary
  )
}
