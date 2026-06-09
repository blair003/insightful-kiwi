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
  period_names <- period_names[period_names %in% names(period_groups)]

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
    period <- period_groups[[period_name]]
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
    selected_species_kill_count = numeric(),
    any_species_kill_count = numeric(),
    first_check = as.Date(character()),
    last_check = as.Date(character()),
    kills_per_100_trap_days_selected_species = numeric(),
    kills_per_100_trap_days_any_species = numeric(),
    trap_checks_per_100_trap_days = numeric(),
    mean_days_between_checks = numeric()
  )
}

empty_monitoring_trapping_trap_details <- function() {
  dplyr::tibble(
    monitoring_key = character(),
    monitoring_locality = character(),
    monitoring_line = integer(),
    trap_code = character(),
    trap_line = character(),
    locationID = character(),
    locality_match_type = character(),
    locality_distance_km = numeric(),
    trap_checks = integer(),
    trap_days = numeric(),
    first_check = as.Date(character()),
    last_check = as.Date(character()),
    selected_species_kill_count = numeric(),
    any_species_kill_count = numeric(),
    kills_per_100_trap_days_selected_species = numeric(),
    kills_per_100_trap_days_any_species = numeric(),
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
    monitoring_rank >= high_percentile & (is.na(trap_rank) | trap_rank <= low_percentile) ~ "High monitoring / low trapping",
    monitoring_rank >= high_percentile & trap_rank >= high_percentile ~ "High monitoring / high trapping",
    monitoring_rank <= low_percentile & trap_rank >= high_percentile ~ "Low monitoring / high trapping",
    monitoring_rank <= low_percentile & (is.na(trap_rank) | trap_rank <= low_percentile) ~ "Low monitoring / low trapping",
    TRUE ~ "Mixed signal"
  )
}

summarise_monitoring_rai_by_line <- function(core_data,
                                             period_names,
                                             start_date,
                                             end_date,
                                             taxa_groups,
                                             rai_norm_hours,
                                             use_net = TRUE) {
  deps <- filter_deps_by_period_names(
    core_data$deps,
    period_names,
    start_date,
    end_date
  )

  obs <- filter_detection_obs(filter_obs_by_period_names(
    core_data$obs,
    period_names,
    start_date,
    end_date
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
    kill_summary <- empty_monitoring_trapping_trap_details() %>%
      dplyr::select("locationID", "selected_species_kill_count", "any_species_kill_count")
  } else {
    selected_species <- tolower(as.character(scientific_names))
    kill_summary <- trap_data$obs %>%
      dplyr::filter(.data$deploymentID %in% deployment_lookup$deploymentID) %>%
      dplyr::mutate(
        source_kill_flag = dplyr::coalesce(
          .data$observationType == "animal" |
            extract_trap_tag_value(.data$observationTags, "kill") == "1",
          FALSE
        ),
        source_count = dplyr::if_else(
          .data$source_kill_flag,
          dplyr::coalesce(suppressWarnings(as.numeric(.data$count)), 1),
          0
        ),
        selected_species_kill_count_source = dplyr::if_else(
          .data$source_kill_flag &
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
        selected_species_kill_count = sum(.data$selected_species_kill_count_source, na.rm = TRUE),
        any_species_kill_count = sum(.data$source_count, na.rm = TRUE),
        .groups = "drop"
      )
  }

  effort_summary %>%
    dplyr::left_join(kill_summary, by = "locationID") %>%
    dplyr::mutate(
      selected_species_kill_count = dplyr::coalesce(.data$selected_species_kill_count, 0),
      any_species_kill_count = dplyr::coalesce(.data$any_species_kill_count, 0),
      kills_per_100_trap_days_selected_species = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$selected_species_kill_count / .data$trap_days,
        NA_real_
      ),
      kills_per_100_trap_days_any_species = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$any_species_kill_count / .data$trap_days,
        NA_real_
      ),
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
      selected_species_kill_count = sum(.data$selected_species_kill_count, na.rm = TRUE),
      any_species_kill_count = sum(.data$any_species_kill_count, na.rm = TRUE),
      first_check = suppressWarnings(min(.data$first_check, na.rm = TRUE)),
      last_check = suppressWarnings(max(.data$last_check, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      kills_per_100_trap_days_selected_species = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$selected_species_kill_count / .data$trap_days,
        NA_real_
      ),
      kills_per_100_trap_days_any_species = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$any_species_kill_count / .data$trap_days,
        NA_real_
      ),
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
      selected_species_kill_count = dplyr::coalesce(.data$selected_species_kill_count, 0),
      any_species_kill_count = dplyr::coalesce(.data$any_species_kill_count, 0),
      kills_per_100_trap_days_selected_species = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$selected_species_kill_count / .data$trap_days,
        NA_real_
      ),
      kills_per_100_trap_days_any_species = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$any_species_kill_count / .data$trap_days,
        NA_real_
      ),
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
      trap_rank = monitoring_trapping_rank(.data$kills_per_100_trap_days_selected_species),
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
  assignable_periods <- period_names_without_all(period_groups)
  taxa_groups <- rai_groups[rai_group]
  scientific_names <- monitoring_trapping_species(rai_groups, rai_group)

  period_rows <- lapply(assignable_periods, function(period_name) {
    period <- period_groups[[period_name]]
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
          selected_species_kill_count = dplyr::coalesce(.data$selected_species_kill_count, 0),
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
          kills_per_100_trap_days_selected_species = dplyr::if_else(
            .data$trap_days > 0,
            100 * .data$selected_species_kill_count / .data$trap_days,
            NA_real_
          )
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
      is.finite(.data$kills_per_100_trap_days_selected_species)
    ) %>%
    dplyr::group_by(.data$lag_label, .data$lag_key, .data$lag_days) %>%
    dplyr::summarise(
      n = dplyr::n(),
      correlation = if (dplyr::n() >= 3) {
        suppressWarnings(stats::cor(.data$selected_RAI, .data$kills_per_100_trap_days_selected_species, use = "complete.obs"))
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$lag_days)

  list(rows = rows, correlations = correlations)
}
