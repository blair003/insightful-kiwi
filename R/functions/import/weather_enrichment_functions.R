environment_daily_key <- function(environment_daily) {
  paste(as.character(environment_daily$locationID), as.character(environment_daily$date), sep = "|")
}

environment_daily_has_weather_data <- function(environment_daily) {
  if (is.null(environment_daily) || nrow(environment_daily) == 0) {
    return(logical(0))
  }

  weather_columns <- intersect(
    c("weathercode", "temperature_2m_max", "temperature_2m_min", "precipitation_sum"),
    names(environment_daily)
  )
  if (length(weather_columns) == 0) {
    return(rep(FALSE, nrow(environment_daily)))
  }

  Reduce(
    `|`,
    lapply(weather_columns, function(column) !is.na(environment_daily[[column]]))
  )
}

merge_environment_daily <- function(existing_environment_daily, new_environment_daily) {
  environment_daily <- dplyr::bind_rows(
    new_environment_daily,
    existing_environment_daily
  )

  if (is.null(environment_daily) || nrow(environment_daily) == 0) {
    return(NULL)
  }

  environment_daily <- environment_daily %>%
    dplyr::filter(!is.na(locationID), !is.na(date)) %>%
    dplyr::arrange(locationID, date) %>%
    dplyr::distinct(locationID, date, .keep_all = TRUE)

  add_daylight_boundaries_to_daily(environment_daily)
}

weather_coverage_status <- function(obs, environment_daily = NULL) {
  if (is.null(obs) || nrow(obs) == 0 ||
      !"timestamp" %in% names(obs) ||
      !"locationID" %in% names(obs)) {
    return(list(status = "not_applicable", required = 0L, available = 0L, missing = 0L))
  }

  required <- obs %>%
    dplyr::filter(!is.na(timestamp), !is.na(locationID)) %>%
    dplyr::mutate(date = as.Date(timestamp, tz = weather_timeline_timezone())) %>%
    dplyr::distinct(locationID, date) %>%
    dplyr::filter(!is.na(date))

  required_count <- nrow(required)
  if (required_count == 0) {
    return(list(status = "not_applicable", required = 0L, available = 0L, missing = 0L))
  }

  if (is.null(environment_daily) || nrow(environment_daily) == 0 ||
      !all(c("locationID", "date") %in% names(environment_daily))) {
    return(list(status = "missing", required = required_count, available = 0L, missing = required_count))
  }

  available <- required %>%
    dplyr::inner_join(
      environment_daily %>%
        dplyr::filter(environment_daily_has_weather_data(.)) %>%
        dplyr::distinct(locationID, date),
      by = c("locationID", "date")
    ) %>%
    nrow()

  missing <- required_count - available
  status <- if (missing == 0) {
    "complete"
  } else if (available > 0) {
    "partial"
  } else {
    "missing"
  }

  list(status = status, required = required_count, available = available, missing = missing)
}

fetch_weather_for_location <- function(location, start_date, end_date) {
  if (is.null(location) || nrow(location) == 0) {
    return(NULL)
  }

  lat <- location$latitude[[1]]
  lng <- location$longitude[[1]]
  if (is.na(lat) || is.na(lng)) {
    return(NULL)
  }

  weather_df <- environment_daily_to_df(fetch_weather_data(lat, lng, start_date, end_date))
  if (is.null(weather_df) || nrow(weather_df) == 0) {
    return(NULL)
  }

  weather_df$locationID <- location$locationID[[1]]
  if ("locationName" %in% names(location)) {
    weather_df$locationName <- location$locationName[[1]]
  }
  if ("locality" %in% names(location)) {
    weather_df$locality <- location$locality[[1]]
  }
  weather_df$latitude <- lat
  weather_df$longitude <- lng
  weather_df <- add_daylight_boundaries_to_daily(weather_df)

  weather_df[, c(
    intersect(c("locationID", "locationName", "locality", "latitude", "longitude"), names(weather_df)),
    setdiff(names(weather_df), c("locationID", "locationName", "locality", "latitude", "longitude"))
  )]
}

build_observation_environment_daily <- function(obs, deps, environment_daily = NULL) {
  required_obs_columns <- c("timestamp", "locationID")
  required_dep_columns <- c("locationID", "latitude", "longitude")
  if (is.null(obs) || nrow(obs) == 0 ||
      is.null(deps) || nrow(deps) == 0 ||
      !all(required_obs_columns %in% names(obs)) ||
      !all(required_dep_columns %in% names(deps))) {
    return(NULL)
  }

  required_dates <- obs %>%
    dplyr::filter(!is.na(timestamp), !is.na(locationID)) %>%
    dplyr::mutate(observation_date = as.Date(timestamp, tz = weather_timeline_timezone())) %>%
    dplyr::distinct(locationID, observation_date) %>%
    dplyr::filter(!is.na(observation_date))

  if (nrow(required_dates) == 0) {
    return(add_daylight_boundaries_to_daily(environment_daily))
  }

  existing_weather <- add_daylight_boundaries_to_daily(environment_daily)
  if (!is.null(existing_weather) && nrow(existing_weather) > 0 &&
      all(c("locationID", "date") %in% names(existing_weather))) {
    existing_keys <- environment_daily_key(existing_weather[environment_daily_has_weather_data(existing_weather), , drop = FALSE])
    required_dates <- required_dates %>%
      dplyr::filter(!paste(as.character(locationID), as.character(observation_date), sep = "|") %in% existing_keys)
  }

  if (nrow(required_dates) == 0) {
    return(existing_weather)
  }

  obs_dates <- required_dates %>%
    dplyr::group_by(locationID) %>%
    dplyr::summarise(
      start_date = min(observation_date, na.rm = TRUE),
      end_date = max(observation_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(start_date), !is.na(end_date))

  if (nrow(obs_dates) == 0) {
    return(existing_weather)
  }

  locations <- deps %>%
    dplyr::filter(locationID %in% obs_dates$locationID) %>%
    dplyr::distinct(locationID, .keep_all = TRUE) %>%
    dplyr::left_join(obs_dates, by = "locationID")

  locations <- locations %>%
    dplyr::mutate(
      weather_latitude = round(latitude, 2),
      weather_longitude = round(longitude, 2)
    ) %>%
    dplyr::filter(!is.na(weather_latitude), !is.na(weather_longitude))

  location_groups <- locations %>%
    dplyr::group_by(weather_latitude, weather_longitude) %>%
    dplyr::summarise(
      start_date = min(start_date, na.rm = TRUE),
      end_date = max(end_date, na.rm = TRUE),
      .groups = "drop"
    )

  weather_by_location <- lapply(seq_len(nrow(location_groups)), function(i) {
    if (open_meteo_rate_limit_hit() && open_meteo_should_defer_on_rate_limit()) {
      return(NULL)
    }

    group <- location_groups[i, , drop = FALSE]
    weather_df <- environment_daily_to_df(fetch_weather_data(
      group$weather_latitude[[1]],
      group$weather_longitude[[1]],
      group$start_date[[1]],
      group$end_date[[1]]
    ))
    if (is.null(weather_df) || nrow(weather_df) == 0) {
      return(NULL)
    }

    weather_df <- add_daylight_boundaries_to_daily(weather_df)
    group_locations <- locations %>%
      dplyr::filter(
        weather_latitude == group$weather_latitude[[1]],
        weather_longitude == group$weather_longitude[[1]]
      )

    dplyr::bind_rows(lapply(seq_len(nrow(group_locations)), function(j) {
      location <- group_locations[j, , drop = FALSE]
      location_weather <- weather_df %>%
        dplyr::filter(
          date >= location$start_date[[1]],
          date <= location$end_date[[1]]
        )

      if (nrow(location_weather) == 0) {
        return(NULL)
      }

      location_weather$locationID <- location$locationID[[1]]
      if ("locationName" %in% names(location)) {
        location_weather$locationName <- location$locationName[[1]]
      }
      if ("locality" %in% names(location)) {
        location_weather$locality <- location$locality[[1]]
      }
      location_weather$latitude <- location$latitude[[1]]
      location_weather$longitude <- location$longitude[[1]]

      location_weather[, c(
        intersect(c("locationID", "locationName", "locality", "latitude", "longitude"), names(location_weather)),
        setdiff(names(location_weather), c("locationID", "locationName", "locality", "latitude", "longitude"))
      )]
    }))
  })

  weather_by_location <- Filter(Negate(is.null), weather_by_location)
  if (length(weather_by_location) == 0) {
    return(existing_weather)
  }

  merge_environment_daily(existing_weather, dplyr::bind_rows(weather_by_location))
}

add_observation_weather_fields <- function(obs, environment_daily = NULL) {
  if (is.null(obs) || nrow(obs) == 0 || !"timestamp" %in% names(obs)) {
    return(obs)
  }

  timezone <- weather_timeline_timezone()
  obs$.observation_row_id <- seq_len(nrow(obs))
  obs <- obs %>%
    dplyr::select(-dplyr::any_of(c(
      "weathercode", "temperature_2m_max", "temperature_2m_min",
      "precipitation_sum", "weather_condition", "weather_icon"
    )))
  obs$observation_date <- as.Date(obs$timestamp, tz = timezone)

  if (!is.null(environment_daily) && nrow(environment_daily) > 0 &&
      all(c("locationID", "date") %in% names(environment_daily)) &&
      "locationID" %in% names(obs)) {
    weather_lookup <- environment_daily %>%
      dplyr::filter(environment_daily_has_weather_data(.)) %>%
      dplyr::select(
        dplyr::any_of(c(
          "locationID", "date", "weathercode", "temperature_2m_max",
          "temperature_2m_min", "precipitation_sum", "weather_condition",
          "weather_icon"
        ))
      ) %>%
      dplyr::rename(observation_date = date)

    obs <- obs %>%
      dplyr::left_join(weather_lookup, by = c("locationID", "observation_date"))
  }

  obs %>%
    dplyr::arrange(.observation_row_id) %>%
    dplyr::select(-.observation_row_id)
}

enrich_observations_with_daily_weather <- function(obs, deps, environment_daily = NULL) {
  environment_daily <- build_observation_environment_daily(obs, deps, environment_daily)
  coverage <- weather_coverage_status(obs, environment_daily)
  if (open_meteo_rate_limit_hit()) {
    coverage$status <- if (coverage$available > 0) "deferred_partial" else "deferred"
  }

  list(
    obs = add_observation_weather_fields(obs, environment_daily),
    environment_daily = environment_daily,
    weather_status = coverage
  )
}

enrich_core_data_weather <- function(core_data, cache_file = NULL, config) {
  weather_enrichment <- enrich_observations_with_daily_weather(
    core_data$obs,
    core_data$deps,
    core_data$environment_daily
  )
  core_data$obs <- weather_enrichment$obs
  core_data$environment_daily <- weather_enrichment$environment_daily
  if (is.null(core_data$app)) {
    core_data$app <- list()
  }
  if (is.null(core_data$app$status)) {
    core_data$app$status <- list()
  }
  core_data$app$status$weather_data <- weather_enrichment$weather_status
  core_data$app$weather_data <- NULL
  core_data$weather_status <- NULL
  core_data <- mark_core_data_app_updated(core_data, "core_data_weather_updated", config = config)
  core_data <- normalise_core_data_timezones(core_data)

  if (!is.null(cache_file)) {
    save_core_data_cache(core_data, cache_file)
  }

  core_data
}

core_data_weather_incomplete <- function(core_data) {
  weather_status <- core_data$app$status$weather_data
  if (is.null(weather_status) && !is.null(core_data$app$weather_data$status)) {
    weather_status <- core_data$app$weather_data$status
  }
  if (is.null(weather_status) && !is.null(core_data$weather_status)) {
    weather_status <- core_data$weather_status
  }

  if (is.null(weather_status) ||
      is.null(weather_status$status)) {
    return(TRUE)
  }

  !weather_status$status %in% c("complete", "not_applicable")
}
