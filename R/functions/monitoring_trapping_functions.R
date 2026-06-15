monitoring_trapping_lag_windows <- function() {
  c(
    "Same monitoring period" = "same",
    "Next 2 weeks" = "next_14",
    "Next 4 weeks" = "next_28",
    "Next 8 weeks" = "next_56",
    "Next 12 weeks" = "next_84"
  )
}

monitoring_trapping_window <- function(start_date, end_date, lag_window) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (identical(lag_window, "same")) {
    return(list(
      start_date = start_date,
      end_date = end_date,
      label = "same monitoring period",
      lag_days = 0L
    ))
  }

  lag_days <- suppressWarnings(as.integer(sub("^next_", "", lag_window)))
  if (is.na(lag_days) || lag_days <= 0) {
    lag_days <- 28L
  }

  list(
    start_date = end_date + 1L,
    end_date = end_date + lag_days,
    label = sprintf("next %s days", lag_days),
    lag_days = lag_days
  )
}

monitoring_trapping_windows_for_periods <- function(period_groups,
                                                    period_names,
                                                    start_date,
                                                    end_date,
                                                    lag_window) {
  period_names <- as.character(period_names)
  flat_period_groups <- flatten_period_groups(period_groups)
  period_names <- period_names[period_names %in% names(flat_period_groups)]

  if (length(period_names) == 0) {
    trap_window <- monitoring_trapping_window(start_date, end_date, lag_window)
    return(list(
      windows = dplyr::tibble(
        window_start = trap_window$start_date,
        window_end = trap_window$end_date
      ),
      trap_window = trap_window
    ))
  }

  windows <- dplyr::bind_rows(lapply(period_names, function(period_name) {
    period <- flat_period_groups[[period_name]]
    window <- monitoring_trapping_window(period$start_date, period$end_date, lag_window)
    dplyr::tibble(
      period_name = period_name,
      window_start = window$start_date,
      window_end = window$end_date,
      lag_days = window$lag_days,
      label = window$label
    )
  })) %>%
    dplyr::distinct(.data$window_start, .data$window_end, .keep_all = TRUE)

  lag_days <- unique(windows$lag_days)
  label <- unique(windows$label)
  if (length(period_names) > 1) {
    label <- if (identical(lag_window, "same")) {
      "same selected monitoring seasons"
    } else {
      sprintf("%s after each selected monitoring season", label[[1]])
    }
  } else {
    label <- label[[1]]
  }

  list(
    windows = windows,
    trap_window = list(
      start_date = min(windows$window_start, na.rm = TRUE),
      end_date = max(windows$window_end, na.rm = TRUE),
      label = label,
      lag_days = lag_days[[1]]
    )
  )
}

monitoring_trapping_deployment_window_overlaps <- function(trap_deps, windows) {
  if (nrow(trap_deps) == 0 || is.null(windows) || nrow(windows) == 0) {
    return(dplyr::tibble())
  }

  joined <- merge(
    trap_deps,
    windows,
    by = NULL,
    all = FALSE
  )

  joined %>%
    dplyr::mutate(
      overlap_start = pmax(.data$prior_check_date, .data$window_start, na.rm = TRUE),
      overlap_end = pmin(.data$check_date, .data$window_end, na.rm = TRUE),
      overlap_days = as.numeric(.data$overlap_end - .data$overlap_start)
    ) %>%
    dplyr::filter(
      !is.na(.data$overlap_days),
      .data$overlap_days > 0
    )
}

empty_monitoring_trapping_trap_summary <- function() {
  dplyr::tibble(
    monitoring_key = character(),
    monitoring_locality = character(),
    monitoring_line = integer(),
    trap_count = integer(),
    trap_checks = integer(),
    trap_days = numeric(),
    selected_species_capture_count = numeric(),
    any_species_capture_count = numeric(),
    first_check = as.Date(character()),
    last_check = as.Date(character()),
    captures_per_100_trap_days_selected_species = numeric(),
    captures_per_100_trap_days_any_species = numeric(),
    trap_checks_per_100_trap_days = numeric(),
    mean_days_between_checks = numeric()
  )
}

empty_monitoring_trapping_trap_details <- function() {
  dplyr::tibble(
    monitoring_key = character(),
    monitoring_locality = character(),
    monitoring_line = integer(),
    trap_locality = character(),
    trap_code = character(),
    trap_line = character(),
    locationID = character(),
    locality_match_type = character(),
    locality_distance_km = numeric(),
    trap_checks = integer(),
    trap_days = numeric(),
    first_check = as.Date(character()),
    last_check = as.Date(character()),
    selected_species_capture_count = numeric(),
    any_species_capture_count = numeric(),
    captures_per_100_trap_days_selected_species = numeric(),
    captures_per_100_trap_days_any_species = numeric(),
    trap_checks_per_100_trap_days = numeric(),
    mean_days_between_checks = numeric()
  )
}

monitoring_trapping_species <- function(rai_groups, rai_group) {
  if (!is.null(rai_groups) && rai_group %in% names(rai_groups)) {
    return(as.character(rai_groups[[rai_group]]))
  }

  as.character(rai_group)
}

monitoring_trapping_latest_trap_date <- function(trap_data) {
  if (is.null(trap_data) || is.null(trap_data$deps) || nrow(trap_data$deps) == 0) {
    return(as.Date(NA))
  }

  date_values <- if ("check_date" %in% names(trap_data$deps)) {
    as.Date(trap_data$deps$check_date)
  } else if ("deploymentEnd" %in% names(trap_data$deps)) {
    as.Date(trap_data$deps$deploymentEnd)
  } else {
    as.Date(character())
  }

  if (!any(!is.na(date_values))) {
    return(as.Date(NA))
  }

  max(date_values, na.rm = TRUE)
}

monitoring_trapping_default_supported_period <- function(core_data,
                                                        trap_data,
                                                        after_days = 21) {
  monitoring_period_groups <- core_data$monitoring_period_groups
  period_names <- period_names_without_all(monitoring_period_groups, assignable_only = FALSE)
  fallback_period <- function() {
    if (length(period_names) > 0) {
      return(period_names[[1]])
    }
    period_names_without_all(core_data$period_groups)[[1]]
  }
  latest_trap_date <- monitoring_trapping_latest_trap_date(trap_data)

  if (length(period_names) == 0 || is.na(latest_trap_date)) {
    return(fallback_period())
  }

  supported <- period_names[vapply(period_names, function(period_name) {
    period <- monitoring_period_groups[[period_name]]
    as.Date(period$end_date) + as.integer(after_days) <= latest_trap_date
  }, logical(1))]

  if (length(supported) > 0) {
    return(supported[[1]])
  }

  fallback <- period_names[vapply(period_names, function(period_name) {
    period <- monitoring_period_groups[[period_name]]
    as.Date(period$start_date) <= latest_trap_date
  }, logical(1))]

  if (length(fallback) > 0) {
    return(fallback[[1]])
  }

  fallback_period()
}

monitoring_trapping_outcomes_windows <- function(period_groups,
                                                    period_names,
                                                    start_date,
                                                    end_date,
                                                    window_days = 21) {
  period_names <- as.character(period_names)
  flat_period_groups <- flatten_period_groups(period_groups)
  period_names <- period_names[period_names %in% names(flat_period_groups)]
  window_days <- suppressWarnings(as.integer(window_days))
  if (is.na(window_days) || window_days < 1) {
    window_days <- 21L
  }

  if (length(period_names) == 0) {
    period_names <- "Selected monitoring period"
    flat_period_groups <- list("Selected monitoring period" = list(
      start_date = as.Date(start_date),
      end_date = as.Date(end_date)
    ))
  }

  dplyr::bind_rows(lapply(period_names, function(period_name) {
    period <- flat_period_groups[[period_name]]
    period_start <- as.Date(period$start_date)
    period_end <- as.Date(period$end_date)
    dplyr::tibble(
      period_name = period_name,
      phase_key = c("before", "during", "after"),
      phase = c("Before monitoring", "During monitoring", "After monitoring"),
      window_start = c(period_start - window_days, period_start, period_end + 1L),
      window_end = c(period_start - 1L, period_end, period_end + window_days)
    )
  })) %>%
    dplyr::filter(.data$window_start <= .data$window_end)
}

monitoring_trapping_group_capture_summary <- function(capture_rows, rai_groups) {
  empty <- dplyr::tibble(
    rai_group = character(),
    species_in_group = character(),
    selected_group_captures = numeric(),
    species_with_captures = integer(),
    trap_capture_status = character()
  )

  if (is.null(rai_groups) || length(rai_groups) == 0) {
    return(empty)
  }

  group_rows <- dplyr::bind_rows(lapply(names(rai_groups), function(group_name) {
    dplyr::tibble(
      rai_group = group_name,
      scientificName = as.character(rai_groups[[group_name]])
    )
  }))

  if (nrow(group_rows) == 0 || is.null(capture_rows) || nrow(capture_rows) == 0) {
    return(empty)
  }

  capture_totals <- capture_rows %>%
    dplyr::group_by(scientificName_lower = tolower(.data$scientificName)) %>%
    dplyr::summarise(captures = sum(.data$capture_count, na.rm = TRUE), .groups = "drop")

  group_rows %>%
    dplyr::mutate(scientificName_lower = tolower(.data$scientificName)) %>%
    dplyr::left_join(capture_totals, by = "scientificName_lower") %>%
    dplyr::mutate(captures = dplyr::coalesce(.data$captures, 0)) %>%
    dplyr::group_by(.data$rai_group) %>%
    dplyr::summarise(
      species_in_group = paste(sort(unique(.data$scientificName)), collapse = ", "),
      selected_group_captures = sum(.data$captures, na.rm = TRUE),
      species_with_captures = dplyr::n_distinct(.data$scientificName[.data$captures > 0]),
      trap_capture_status = "Has trap captures",
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$selected_group_captures > 0) %>%
    dplyr::arrange(dplyr::desc(.data$selected_group_captures), .data$rai_group)
}

monitoring_trapping_trapping_outcomes_summary <- function(core_data,
                                                             trap_data,
                                                             period_names,
                                                             start_date,
                                                             end_date,
                                                             rai_groups,
                                                             window_days = 21,
                                                             selected_localities = NULL,
                                                             max_locality_distance_km = 1) {
  windows <- monitoring_trapping_outcomes_windows(
    period_groups = core_data$period_groups,
    period_names = period_names,
    start_date = start_date,
    end_date = end_date,
    window_days = window_days
  )

  empty_species <- dplyr::tibble(
    scientificName = character(),
    phase = character(),
    capture_count = numeric(),
    capture_percent = numeric(),
    trap_check_records = integer(),
    trap_count = integer(),
    monitoring_line_count = integer()
  )
  empty_line_species <- dplyr::tibble(
    monitoring_locality = character(),
    monitoring_line = integer(),
    phase = character(),
    scientificName = character(),
    capture_count = numeric(),
    trap_check_records = integer(),
    trap_count = integer()
  )
  empty_trap_species <- dplyr::tibble(
    trap_code = character(),
    monitoring_locality = character(),
    monitoring_line = integer(),
    phase = character(),
    scientificName = character(),
    latitude = numeric(),
    longitude = numeric(),
    capture_count = numeric(),
    trap_check_records = integer()
  )

  if (is.null(trap_data) || is.null(trap_data$obs) || nrow(trap_data$obs) == 0 || nrow(windows) == 0) {
    return(list(
      species_summary = empty_species,
      line_species_summary = empty_line_species,
      trap_species_summary = empty_trap_species,
      group_summary = monitoring_trapping_group_capture_summary(empty_species, rai_groups),
      windows = windows,
      latest_trap_date = monitoring_trapping_latest_trap_date(trap_data)
    ))
  }

  deployment_lookup <- monitoring_trapping_filtered_deployments(
    trap_data = trap_data,
    core_data = core_data,
    start_date = min(windows$window_start, na.rm = TRUE),
    end_date = max(windows$window_end, na.rm = TRUE),
    selected_localities = selected_localities,
    max_locality_distance_km = max_locality_distance_km,
    windows = windows %>% dplyr::select("window_start", "window_end") %>% dplyr::distinct()
  )

  if (nrow(deployment_lookup) == 0) {
    return(list(
      species_summary = empty_species,
      line_species_summary = empty_line_species,
      trap_species_summary = empty_trap_species,
      group_summary = monitoring_trapping_group_capture_summary(empty_species, rai_groups),
      windows = windows,
      latest_trap_date = monitoring_trapping_latest_trap_date(trap_data)
    ))
  }

  deployment_species_lookup <- deployment_lookup %>%
    dplyr::select(
      "deploymentID",
      "monitoring_key",
      "monitoring_locality",
      "monitoring_line",
      "locationID",
      "trap_code",
      "latitude",
      "longitude"
    ) %>%
    dplyr::distinct()

  capture_rows <- trap_data$obs %>%
    dplyr::filter(.data$deploymentID %in% deployment_species_lookup$deploymentID) %>%
    dplyr::mutate(
      check_date = if ("check_date" %in% names(.)) as.Date(.data$check_date) else as.Date(.data$eventStart),
      source_capture_flag = dplyr::coalesce(
        .data$observationType == "animal" |
          extract_trap_tag_value(.data$observationTags, "capture") == "1",
        FALSE
      ),
      capture_count = dplyr::if_else(
        .data$source_capture_flag,
        dplyr::coalesce(suppressWarnings(as.numeric(.data$count)), 1),
        0
      ),
      scientificName = dplyr::if_else(
        !is.na(.data$scientificName) & nzchar(as.character(.data$scientificName)),
        as.character(.data$scientificName),
        "Unknown species"
      )
    ) %>%
    dplyr::filter(.data$source_capture_flag, .data$capture_count > 0, !is.na(.data$check_date)) %>%
    dplyr::left_join(deployment_species_lookup, by = "deploymentID")

  if (nrow(capture_rows) == 0) {
    return(list(
      species_summary = empty_species,
      line_species_summary = empty_line_species,
      trap_species_summary = empty_trap_species,
      group_summary = monitoring_trapping_group_capture_summary(capture_rows, rai_groups),
      windows = windows,
      latest_trap_date = monitoring_trapping_latest_trap_date(trap_data)
    ))
  }

  capture_rows <- merge(capture_rows, windows, by = NULL, all = FALSE) %>%
    dplyr::filter(
      .data$check_date >= .data$window_start,
      .data$check_date <= .data$window_end
    )

  if (nrow(capture_rows) == 0) {
    return(list(
      species_summary = empty_species,
      line_species_summary = empty_line_species,
      trap_species_summary = empty_trap_species,
      group_summary = monitoring_trapping_group_capture_summary(capture_rows, rai_groups),
      windows = windows,
      latest_trap_date = monitoring_trapping_latest_trap_date(trap_data)
    ))
  }

  total_captures <- sum(capture_rows$capture_count, na.rm = TRUE)
  species_summary <- capture_rows %>%
    dplyr::group_by(.data$phase, .data$scientificName) %>%
    dplyr::summarise(
      capture_count = sum(.data$capture_count, na.rm = TRUE),
      trap_check_records = dplyr::n_distinct(.data$deploymentID),
      trap_count = dplyr::n_distinct(.data$locationID),
      monitoring_line_count = dplyr::n_distinct(.data$monitoring_key),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      capture_percent = if (total_captures > 0) 100 * .data$capture_count / total_captures else NA_real_,
      phase = factor(.data$phase, levels = c("Before monitoring", "During monitoring", "After monitoring"))
    ) %>%
    dplyr::arrange(.data$phase, dplyr::desc(.data$capture_count), .data$scientificName)

  line_species_summary <- capture_rows %>%
    dplyr::group_by(.data$monitoring_locality, .data$monitoring_line, .data$phase, .data$scientificName) %>%
    dplyr::summarise(
      capture_count = sum(.data$capture_count, na.rm = TRUE),
      trap_check_records = dplyr::n_distinct(.data$deploymentID),
      trap_count = dplyr::n_distinct(.data$locationID),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$monitoring_locality, .data$monitoring_line, .data$phase, dplyr::desc(.data$capture_count))

  trap_species_summary <- capture_rows %>%
    dplyr::filter(is.finite(.data$latitude), is.finite(.data$longitude)) %>%
    dplyr::group_by(
      .data$trap_code,
      .data$monitoring_locality,
      .data$monitoring_line,
      .data$phase,
      .data$scientificName,
      .data$latitude,
      .data$longitude
    ) %>%
    dplyr::summarise(
      capture_count = sum(.data$capture_count, na.rm = TRUE),
      trap_check_records = dplyr::n_distinct(.data$deploymentID),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$monitoring_locality, .data$monitoring_line, .data$trap_code, .data$phase, dplyr::desc(.data$capture_count))

  list(
    species_summary = species_summary,
    line_species_summary = line_species_summary,
    trap_species_summary = trap_species_summary,
    group_summary = monitoring_trapping_group_capture_summary(capture_rows, rai_groups),
    windows = windows,
    latest_trap_date = monitoring_trapping_latest_trap_date(trap_data)
  )
}

monitoring_trapping_rank <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x))
  valid <- is.finite(x)
  if (!any(valid)) {
    return(out)
  }

  out[valid] <- dplyr::percent_rank(x[valid])
  out[valid & length(unique(x[valid])) == 1L] <- 0.5
  out
}

monitoring_trapping_classify <- function(monitoring_rank,
                                         trap_rank,
                                         camera_hours,
                                         trap_days,
                                         min_trap_days = 25,
                                         high_percentile = 0.6) {
  low_percentile <- 1 - high_percentile

  dplyr::case_when(
    is.na(camera_hours) | camera_hours <= 0 | is.na(trap_days) | trap_days < min_trap_days ~ "Insufficient data",
    monitoring_rank >= high_percentile & (is.na(trap_rank) | trap_rank <= low_percentile) ~ "Relatively high monitoring / relatively low trapping",
    monitoring_rank >= high_percentile & trap_rank >= high_percentile ~ "Relatively high monitoring / relatively high trapping",
    monitoring_rank <= low_percentile & trap_rank >= high_percentile ~ "Relatively low monitoring / relatively high trapping",
    monitoring_rank <= low_percentile & (is.na(trap_rank) | trap_rank <= low_percentile) ~ "Relatively low monitoring / relatively low trapping",
    TRUE ~ "Mixed signal"
  )
}

trap_capture_rate_per_100_days <- function(capture_count, trap_days, min_trap_days = 100) {
  dplyr::if_else(
    is.finite(trap_days) & trap_days >= min_trap_days,
    100 * capture_count / trap_days,
    NA_real_
  )
}

summarise_monitoring_rai_by_line <- function(core_data,
                                             period_names,
                                             start_date,
                                             end_date,
                                             taxa_groups,
                                             rai_norm_hours,
                                             use_net = TRUE) {
  monitoring_groups <- flatten_period_groups(core_data$monitoring_period_groups)
  selected_groups <- monitoring_groups[as.character(period_names)]
  selected_groups <- selected_groups[!vapply(selected_groups, is.null, logical(1))]
  period_intervals <- if (length(selected_groups) > 0) {
    dplyr::bind_rows(lapply(names(selected_groups), function(period_name) {
      data.frame(
        period_name = period_name,
        start_date = selected_groups[[period_name]]$start_date,
        end_date = selected_groups[[period_name]]$end_date,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    NULL
  }

  deps <- filter_deps_by_period_names(
    core_data$deps,
    period_names,
    start_date,
    end_date,
    period_intervals
  )

  obs <- filter_detection_obs(filter_obs_by_period_names(
    core_data$obs,
    period_names,
    start_date,
    end_date,
    period_intervals
  ))

  rai <- calculate_rai(
    obs = obs,
    deps = deps,
    taxa_groups = taxa_groups,
    rai_norm_hours = rai_norm_hours,
    use_net = use_net
  )

  line_locations <- deps %>%
    dplyr::group_by(.data$locality, .data$line) %>%
    dplyr::summarise(
      latitude = mean(.data$latitude, na.rm = TRUE),
      longitude = mean(.data$longitude, na.rm = TRUE),
      camera_count = dplyr::n_distinct(.data$locationName),
      .groups = "drop"
    )

  rai$line %>%
    dplyr::left_join(line_locations, by = c("locality", "line")) %>%
    dplyr::mutate(
      monitoring_key = paste(.data$locality, .data$line, sep = " | "),
      monitoring_label = paste(.data$locality, "Line", .data$line)
    )
}

monitoring_trapping_filtered_deployments <- function(trap_data,
                                                     core_data,
                                                     start_date,
                                                     end_date,
                                                     selected_localities = NULL,
                                                     max_locality_distance_km = 1,
                                                     windows = NULL) {
  if (is.null(trap_data) || is.null(trap_data$deps) || nrow(trap_data$deps) == 0) {
    return(dplyr::tibble())
  }

  max_locality_distance_km <- suppressWarnings(as.numeric(max_locality_distance_km))
  if (is.na(max_locality_distance_km) || max_locality_distance_km < 0) {
    max_locality_distance_km <- 1
  }

  if (is.null(windows)) {
    windows <- dplyr::tibble(
      window_start = as.Date(start_date),
      window_end = as.Date(end_date)
    )
  } else {
    windows <- windows %>%
      dplyr::transmute(
        window_start = as.Date(.data$window_start),
        window_end = as.Date(.data$window_end)
      ) %>%
      dplyr::filter(!is.na(.data$window_start), !is.na(.data$window_end)) %>%
      dplyr::distinct()
  }

  if (nrow(windows) == 0) {
    return(dplyr::tibble())
  }

  trap_deps <- trap_data$deps %>%
    dplyr::mutate(
      prior_check_date = if ("prior_check_date" %in% names(.)) as.Date(.data$prior_check_date) else as.Date(.data$deploymentStart),
      check_date = if ("check_date" %in% names(.)) as.Date(.data$check_date) else as.Date(.data$deploymentEnd),
      interval_days = dplyr::coalesce(
        suppressWarnings(as.numeric(if ("interval_days" %in% names(.)) .data$interval_days else NA_real_)),
        as.numeric(.data$check_date - .data$prior_check_date)
      ),
      trap_code = if ("locationName" %in% names(.)) trimws(.data$locationName) else NA_character_,
      trap_line = if ("deploymentGroups" %in% names(.)) .data$deploymentGroups else NA_character_,
      locality = if ("locality" %in% names(.)) .data$locality else NA_character_,
      locality_match_type = if ("locality_match_type" %in% names(.)) .data$locality_match_type else NA_character_,
      locality_distance_km = suppressWarnings(as.numeric(if ("locality_distance_km" %in% names(.)) .data$locality_distance_km else NA_real_)),
      nearest_monitoring_locationName = if ("nearest_monitoring_locationName" %in% names(.)) .data$nearest_monitoring_locationName else NA_character_
    ) %>%
    dplyr::filter(
      !is.na(.data$deploymentID),
      !is.na(.data$check_date),
      .data$prior_check_date <= max(windows$window_end, na.rm = TRUE),
      .data$check_date >= min(windows$window_start, na.rm = TRUE)
    )

  if (!is.null(selected_localities) && length(selected_localities) > 0) {
    trap_deps <- trap_deps %>%
      dplyr::filter(
        .data$locality %in% selected_localities,
        .data$locality_match_type == "within" |
          suppressWarnings(as.numeric(.data$locality_distance_km)) <= max_locality_distance_km
      )
  }

  if (nrow(trap_deps) == 0) {
    return(dplyr::tibble())
  }

  trap_deps <- monitoring_trapping_deployment_window_overlaps(trap_deps, windows)

  if (nrow(trap_deps) == 0) {
    return(dplyr::tibble())
  }

  monitoring_lookup <- core_data$deps %>%
    dplyr::select(
      nearest_monitoring_locationName = "locationName",
      monitoring_locality = "locality",
      monitoring_line = "line"
    ) %>%
    dplyr::distinct()

  trap_deps %>%
    dplyr::left_join(monitoring_lookup, by = "nearest_monitoring_locationName") %>%
    dplyr::mutate(
      monitoring_locality = dplyr::coalesce(.data$monitoring_locality, .data$locality),
      monitoring_line = dplyr::coalesce(.data$monitoring_line, suppressWarnings(as.integer(gsub("[^0-9]+", "", .data$trap_line)))),
      monitoring_key = paste(.data$monitoring_locality, .data$monitoring_line, sep = " | ")
    )
}

monitoring_trapping_trap_details <- function(trap_data,
                                             core_data,
                                             start_date,
                                             end_date,
                                             scientific_names,
                                             selected_localities = NULL,
                                             max_locality_distance_km = 1,
                                             windows = NULL) {
  deployment_lookup <- monitoring_trapping_filtered_deployments(
    trap_data = trap_data,
    core_data = core_data,
    start_date = start_date,
    end_date = end_date,
    selected_localities = selected_localities,
    max_locality_distance_km = max_locality_distance_km,
    windows = windows
  )

  if (nrow(deployment_lookup) == 0) {
    return(empty_monitoring_trapping_trap_details())
  }

  effort_summary <- deployment_lookup %>%
    dplyr::group_by(
      .data$monitoring_key,
      .data$monitoring_locality,
      .data$monitoring_line,
      trap_locality = .data$locality,
      .data$trap_code,
      .data$trap_line,
      .data$locationID,
      .data$locality_match_type,
      .data$locality_distance_km
    ) %>%
    dplyr::summarise(
      trap_checks = dplyr::n_distinct(.data$deploymentID),
      trap_days = sum(.data$overlap_days, na.rm = TRUE),
      first_check = suppressWarnings(min(.data$prior_check_date, na.rm = TRUE)),
      last_check = suppressWarnings(max(.data$check_date, na.rm = TRUE)),
      .groups = "drop"
    )

  if (is.null(trap_data$obs) || nrow(trap_data$obs) == 0) {
    capture_summary <- empty_monitoring_trapping_trap_details() %>%
      dplyr::select("locationID", "selected_species_capture_count", "any_species_capture_count")
  } else {
    selected_species <- tolower(as.character(scientific_names))
    capture_summary <- trap_data$obs %>%
      dplyr::filter(.data$deploymentID %in% deployment_lookup$deploymentID) %>%
      dplyr::mutate(
        source_capture_flag = dplyr::coalesce(
          .data$observationType == "animal" |
            extract_trap_tag_value(.data$observationTags, "capture") == "1",
          FALSE
        ),
        source_count = dplyr::if_else(
          .data$source_capture_flag,
          dplyr::coalesce(suppressWarnings(as.numeric(.data$count)), 1),
          0
        ),
        selected_species_capture_count_source = dplyr::if_else(
          .data$source_capture_flag &
            !is.na(.data$scientificName) &
            tolower(.data$scientificName) %in% selected_species,
          .data$source_count,
          0
        )
      ) %>%
      dplyr::left_join(
        deployment_lookup %>% dplyr::select("deploymentID", "locationID") %>% dplyr::distinct(),
        by = "deploymentID"
      ) %>%
      dplyr::group_by(.data$locationID) %>%
      dplyr::summarise(
        selected_species_capture_count = sum(.data$selected_species_capture_count_source, na.rm = TRUE),
        any_species_capture_count = sum(.data$source_count, na.rm = TRUE),
        .groups = "drop"
      )
  }

  effort_summary %>%
    dplyr::left_join(capture_summary, by = "locationID") %>%
    dplyr::mutate(
      selected_species_capture_count = dplyr::coalesce(.data$selected_species_capture_count, 0),
      any_species_capture_count = dplyr::coalesce(.data$any_species_capture_count, 0),
      captures_per_100_trap_days_selected_species = trap_capture_rate_per_100_days(.data$selected_species_capture_count, .data$trap_days),
      captures_per_100_trap_days_any_species = trap_capture_rate_per_100_days(.data$any_species_capture_count, .data$trap_days),
      trap_checks_per_100_trap_days = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$trap_checks / .data$trap_days,
        NA_real_
      ),
      mean_days_between_checks = dplyr::if_else(
        .data$trap_checks > 0,
        .data$trap_days / .data$trap_checks,
        NA_real_
      )
    )
}

summarise_trapping_by_monitoring_line <- function(trap_data,
                                                 core_data,
                                                 start_date,
                                                 end_date,
                                                 scientific_names,
                                                 selected_localities = NULL,
                                                 max_locality_distance_km = 1,
                                                 windows = NULL) {
  trap_details <- monitoring_trapping_trap_details(
    trap_data = trap_data,
    core_data = core_data,
    start_date = start_date,
    end_date = end_date,
    scientific_names = scientific_names,
    selected_localities = selected_localities,
    max_locality_distance_km = max_locality_distance_km,
    windows = windows
  )

  if (nrow(trap_details) == 0) {
    return(empty_monitoring_trapping_trap_summary())
  }

  trap_details %>%
    dplyr::group_by(.data$monitoring_key, .data$monitoring_locality, .data$monitoring_line) %>%
    dplyr::summarise(
      trap_count = dplyr::n_distinct(.data$locationID),
      trap_checks = sum(.data$trap_checks, na.rm = TRUE),
      trap_days = sum(.data$trap_days, na.rm = TRUE),
      selected_species_capture_count = sum(.data$selected_species_capture_count, na.rm = TRUE),
      any_species_capture_count = sum(.data$any_species_capture_count, na.rm = TRUE),
      first_check = suppressWarnings(min(.data$first_check, na.rm = TRUE)),
      last_check = suppressWarnings(max(.data$last_check, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      captures_per_100_trap_days_selected_species = trap_capture_rate_per_100_days(.data$selected_species_capture_count, .data$trap_days),
      captures_per_100_trap_days_any_species = trap_capture_rate_per_100_days(.data$any_species_capture_count, .data$trap_days),
      trap_checks_per_100_trap_days = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$trap_checks / .data$trap_days,
        NA_real_
      ),
      mean_days_between_checks = dplyr::if_else(
        .data$trap_checks > 0,
        .data$trap_days / .data$trap_checks,
        NA_real_
      )
    )
}

monitoring_trapping_mismatch_summary <- function(core_data,
                                                 trap_data,
                                                 period_names,
                                                 start_date,
                                                 end_date,
                                                 rai_groups,
                                                 rai_group,
                                                 lag_window,
                                                 rai_norm_hours,
                                                 use_net = TRUE,
                                                 selected_localities = NULL,
                                                 max_locality_distance_km = 1,
                                                 min_trap_days = 25,
                                                 high_percentile = 0.6) {
  taxa_groups <- rai_groups[rai_group]
  scientific_names <- monitoring_trapping_species(rai_groups, rai_group)
  trap_windows <- monitoring_trapping_windows_for_periods(
    period_groups = core_data$period_groups,
    period_names = period_names,
    start_date = start_date,
    end_date = end_date,
    lag_window = lag_window
  )
  trap_window <- trap_windows$trap_window

  monitoring <- summarise_monitoring_rai_by_line(
    core_data = core_data,
    period_names = period_names,
    start_date = start_date,
    end_date = end_date,
    taxa_groups = taxa_groups,
    rai_norm_hours = rai_norm_hours,
    use_net = use_net
  )

  if (!is.null(selected_localities) && length(selected_localities) > 0) {
    monitoring <- monitoring %>%
      dplyr::filter(.data$locality %in% selected_localities)
  }

  trap_details <- monitoring_trapping_trap_details(
    trap_data = trap_data,
    core_data = core_data,
    start_date = trap_window$start_date,
    end_date = trap_window$end_date,
    scientific_names = scientific_names,
    selected_localities = selected_localities,
    max_locality_distance_km = max_locality_distance_km,
    windows = trap_windows$windows
  )

  trapping <- summarise_trapping_by_monitoring_line(
    trap_data = trap_data,
    core_data = core_data,
    start_date = trap_window$start_date,
    end_date = trap_window$end_date,
    scientific_names = scientific_names,
    selected_localities = selected_localities,
    max_locality_distance_km = max_locality_distance_km,
    windows = trap_windows$windows
  )

  summary <- monitoring %>%
    dplyr::left_join(
      trapping,
      by = c(
        "monitoring_key",
        "locality" = "monitoring_locality",
        "line" = "monitoring_line"
      )
    ) %>%
    dplyr::mutate(
      trap_count = dplyr::coalesce(.data$trap_count, 0L),
      trap_checks = dplyr::coalesce(.data$trap_checks, 0L),
      trap_days = dplyr::coalesce(.data$trap_days, 0),
      selected_species_capture_count = dplyr::coalesce(.data$selected_species_capture_count, 0),
      any_species_capture_count = dplyr::coalesce(.data$any_species_capture_count, 0),
      captures_per_100_trap_days_selected_species = trap_capture_rate_per_100_days(.data$selected_species_capture_count, .data$trap_days),
      captures_per_100_trap_days_any_species = trap_capture_rate_per_100_days(.data$any_species_capture_count, .data$trap_days),
      trap_checks_per_100_trap_days = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$trap_checks / .data$trap_days,
        NA_real_
      ),
      mean_days_between_checks = dplyr::if_else(
        .data$trap_checks > 0,
        .data$trap_days / .data$trap_checks,
        NA_real_
      ),
      monitoring_rank = monitoring_trapping_rank(.data$selected_RAI),
      trap_rank = monitoring_trapping_rank(.data$captures_per_100_trap_days_selected_species),
      mismatch_category = monitoring_trapping_classify(
        .data$monitoring_rank,
        .data$trap_rank,
        .data$camera_hours,
        .data$trap_days,
        min_trap_days = min_trap_days,
        high_percentile = high_percentile
      ),
      mismatch_score = .data$monitoring_rank - dplyr::coalesce(.data$trap_rank, 0),
      monitoring_period_start = as.Date(start_date),
      monitoring_period_end = as.Date(end_date),
      trap_window_start = trap_window$start_date,
      trap_window_end = trap_window$end_date,
      trap_window_label = trap_window$label
    )

  list(
    summary = summary,
    trap_details = trap_details,
    trap_window = trap_window,
    scientific_names = scientific_names
  )
}

monitoring_trapping_lag_summary <- function(core_data,
                                            trap_data,
                                            period_groups,
                                            rai_groups,
                                            rai_group,
                                            rai_norm_hours,
                                            use_net = TRUE,
                                            selected_localities = NULL,
                                            max_locality_distance_km = 1) {
  lag_values <- monitoring_trapping_lag_windows()
  flat_period_groups <- flatten_period_groups(period_groups)
  assignable_periods <- period_names_without_all(period_groups)
  taxa_groups <- rai_groups[rai_group]
  scientific_names <- monitoring_trapping_species(rai_groups, rai_group)

  period_rows <- lapply(assignable_periods, function(period_name) {
    period <- flat_period_groups[[period_name]]
    monitoring <- summarise_monitoring_rai_by_line(
      core_data = core_data,
      period_names = period_name,
      start_date = period$start_date,
      end_date = period$end_date,
      taxa_groups = taxa_groups,
      rai_norm_hours = rai_norm_hours,
      use_net = use_net
    )

    if (!is.null(selected_localities) && length(selected_localities) > 0) {
      monitoring <- monitoring %>%
        dplyr::filter(.data$locality %in% selected_localities)
    }

    if (nrow(monitoring) == 0) {
      return(dplyr::tibble())
    }

    dplyr::bind_rows(lapply(names(lag_values), function(lag_label) {
      lag_key <- unname(lag_values[[lag_label]])
      trap_window <- monitoring_trapping_window(
        period$start_date,
        period$end_date,
        lag_key
      )

      trapping <- summarise_trapping_by_monitoring_line(
        trap_data = trap_data,
        core_data = core_data,
        start_date = trap_window$start_date,
        end_date = trap_window$end_date,
        scientific_names = scientific_names,
        selected_localities = selected_localities,
        max_locality_distance_km = max_locality_distance_km
      )

      monitoring %>%
        dplyr::left_join(
          trapping,
          by = c(
            "monitoring_key",
            "locality" = "monitoring_locality",
            "line" = "monitoring_line"
          )
        ) %>%
        dplyr::mutate(
          period_name = period_name,
          monitoring_period_start = as.Date(period$start_date),
          monitoring_period_end = as.Date(period$end_date),
          lag_label = lag_label,
          lag_key = lag_key,
          lag_days = trap_window$lag_days,
          trap_window_start = trap_window$start_date,
          trap_window_end = trap_window$end_date,
          trap_days = dplyr::coalesce(.data$trap_days, 0),
          selected_species_capture_count = dplyr::coalesce(.data$selected_species_capture_count, 0),
          trap_checks = dplyr::coalesce(.data$trap_checks, 0L),
          trap_checks_per_100_trap_days = dplyr::if_else(
            .data$trap_days > 0,
            100 * .data$trap_checks / .data$trap_days,
            NA_real_
          ),
          mean_days_between_checks = dplyr::if_else(
            .data$trap_checks > 0,
            .data$trap_days / .data$trap_checks,
            NA_real_
          ),
          captures_per_100_trap_days_selected_species = trap_capture_rate_per_100_days(.data$selected_species_capture_count, .data$trap_days)
        )
    }))
  })

  rows <- dplyr::bind_rows(period_rows)
  if (nrow(rows) == 0) {
    return(list(rows = rows, correlations = dplyr::tibble()))
  }

  correlations <- rows %>%
    dplyr::filter(
      is.finite(.data$selected_RAI),
      is.finite(.data$captures_per_100_trap_days_selected_species)
    ) %>%
    dplyr::group_by(.data$lag_label, .data$lag_key, .data$lag_days) %>%
    dplyr::summarise(
      n = dplyr::n(),
      correlation = if (dplyr::n() >= 3) {
        suppressWarnings(stats::cor(.data$selected_RAI, .data$captures_per_100_trap_days_selected_species, use = "complete.obs"))
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$lag_days)

  list(rows = rows, correlations = correlations)
}
