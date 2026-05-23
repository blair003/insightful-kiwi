daylight_timezone <- function() {
  if (exists("source_timestamp_timezone", mode = "function", inherits = TRUE)) {
    return(source_timestamp_timezone())
  }

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

sunlight_times_for_date <- function(date, latitude, longitude) {
  if (is.na(date) || is.na(latitude) || is.na(longitude)) {
    return(NULL)
  }

  suncalc::getSunlightTimes(
    date = as.Date(date),
    lat = latitude,
    lon = longitude,
    tz = daylight_timezone(),
    keep = c("dawn", "sunrise", "sunset", "dusk")
  )
}

civil_twilight_time <- function(date, latitude, longitude, event = c("dawn", "dusk")) {
  event <- match.arg(event)
  sunlight_times <- sunlight_times_for_date(date, latitude, longitude)
  if (is.null(sunlight_times)) {
    return(as.POSIXct(NA, tz = daylight_timezone()))
  }

  if (identical(event, "dawn")) sunlight_times$dawn[[1]] else sunlight_times$dusk[[1]]
}

sunrise_sunset_time <- function(date, latitude, longitude, event = c("dawn", "dusk")) {
  event <- match.arg(event)
  sunlight_times <- sunlight_times_for_date(date, latitude, longitude)
  if (is.null(sunlight_times)) {
    return(as.POSIXct(NA, tz = daylight_timezone()))
  }

  if (identical(event, "dawn")) sunlight_times$sunrise[[1]] else sunlight_times$sunset[[1]]
}

add_daylight_boundaries_to_daily <- function(daily_data) {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(daily_data)
  }

  timezone <- daylight_timezone()

  if (!"sunrise" %in% names(daily_data)) {
    daily_data$sunrise <- as.POSIXct(NA, tz = timezone)
  }
  if (!"sunset" %in% names(daily_data)) {
    daily_data$sunset <- as.POSIXct(NA, tz = timezone)
  }
  if (!"civil_dawn" %in% names(daily_data)) {
    daily_data$civil_dawn <- as.POSIXct(NA, tz = timezone)
  }
  if (!"civil_dusk" %in% names(daily_data)) {
    daily_data$civil_dusk <- as.POSIXct(NA, tz = timezone)
  }
  if (!"matutinal_end" %in% names(daily_data)) {
    daily_data$matutinal_end <- as.POSIXct(NA, tz = timezone)
  }
  if (!"diurnal_end" %in% names(daily_data)) {
    daily_data$diurnal_end <- as.POSIXct(NA, tz = timezone)
  }

  if (all(c("latitude", "longitude", "date") %in% names(daily_data))) {
    sunlight_times <- suncalc::getSunlightTimes(
      data = data.frame(
        date = as.Date(daily_data$date),
        lat = daily_data$latitude,
        lon = daily_data$longitude
      ),
      tz = timezone,
      keep = c("dawn", "sunrise", "sunset", "dusk")
    )
    daily_data$sunrise <- sunlight_times$sunrise
    daily_data$sunset <- sunlight_times$sunset
    daily_data$civil_dawn <- sunlight_times$dawn
    daily_data$civil_dusk <- sunlight_times$dusk
  }

  daily_data$matutinal_end <- daily_data$sunrise
  daily_data$diurnal_end <- daily_data$sunset
  daily_data
}

build_observation_daylight_rows <- function(obs, deps) {
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
    dplyr::mutate(date = as.Date(timestamp, tz = daylight_timezone())) %>%
    dplyr::distinct(locationID, date) %>%
    dplyr::filter(!is.na(date))

  if (nrow(required_dates) == 0) {
    return(NULL)
  }

  daylight_rows <- required_dates %>%
    dplyr::left_join(
      deps %>%
        dplyr::select(dplyr::any_of(c("locationID", "locationName", "locality", "latitude", "longitude"))) %>%
        dplyr::distinct(locationID, .keep_all = TRUE),
      by = "locationID"
    ) %>%
    dplyr::filter(!is.na(latitude), !is.na(longitude))

  add_daylight_boundaries_to_daily(daylight_rows)
}

merge_environment_daylight_rows <- function(existing_daily = NULL, daylight_rows = NULL) {
  existing_daily <- add_daylight_boundaries_to_daily(existing_daily)
  if (is.null(daylight_rows) || nrow(daylight_rows) == 0) {
    return(existing_daily)
  }

  daily_data <- dplyr::bind_rows(daylight_rows, existing_daily)
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(NULL)
  }

  daily_data %>%
    dplyr::filter(!is.na(locationID), !is.na(date)) %>%
    dplyr::arrange(locationID, date) %>%
    dplyr::distinct(locationID, date, .keep_all = TRUE) %>%
    add_daylight_boundaries_to_daily()
}

classify_observation_time <- function(timestamp, sunrise, sunset, civil_dawn, civil_dusk) {
  timezone <- daylight_timezone()
  timestamp <- as.POSIXct(timestamp, tz = timezone)

  if (is.na(timestamp) || is.na(sunrise) || is.na(sunset) || sunset <= sunrise) {
    return(list(day_night_class = "Unknown", diel_class = "Unknown"))
  }

  day_night_class <- if (timestamp >= sunrise && timestamp < sunset) "Day" else "Night"
  if (is.na(civil_dawn) || civil_dawn > sunrise) {
    civil_dawn <- sunrise
  }
  if (is.na(civil_dusk) || civil_dusk < sunset) {
    civil_dusk <- sunset
  }

  diel_class <- if (timestamp < civil_dawn || timestamp >= civil_dusk) {
    "Nocturnal"
  } else if (timestamp < sunrise) {
    "Matutinal"
  } else if (timestamp < sunset) {
    "Diurnal"
  } else {
    "Vespertine"
  }

  list(day_night_class = day_night_class, diel_class = diel_class)
}

add_observation_time_classes <- function(obs, daily_data = NULL) {
  if (is.null(obs) || nrow(obs) == 0 || !"timestamp" %in% names(obs)) {
    return(obs)
  }

  timezone <- daylight_timezone()
  obs$.observation_row_id <- seq_len(nrow(obs))
  obs <- obs %>%
    dplyr::select(-dplyr::any_of(c(
      "observation_date", "sunrise", "sunset", "civil_dawn", "civil_dusk",
      "matutinal_end", "diurnal_end", "day_night_class", "diel_class"
    )))
  obs$observation_date <- as.Date(obs$timestamp, tz = timezone)

  if (!is.null(daily_data) && nrow(daily_data) > 0 &&
      all(c("locationID", "date") %in% names(daily_data)) &&
      "locationID" %in% names(obs)) {
    daylight_lookup <- daily_data %>%
      dplyr::select(
        dplyr::any_of(c(
          "locationID", "date", "sunrise", "sunset", "matutinal_end",
          "diurnal_end", "civil_dawn", "civil_dusk"
        ))
      ) %>%
      dplyr::rename(observation_date = date)

    obs <- obs %>%
      dplyr::left_join(daylight_lookup, by = c("locationID", "observation_date"))
  } else {
    obs$sunrise <- as.POSIXct(NA, tz = timezone)
    obs$sunset <- as.POSIXct(NA, tz = timezone)
    obs$civil_dawn <- as.POSIXct(NA, tz = timezone)
    obs$civil_dusk <- as.POSIXct(NA, tz = timezone)
    obs$matutinal_end <- as.POSIXct(NA, tz = timezone)
    obs$diurnal_end <- as.POSIXct(NA, tz = timezone)
  }

  for (time_column in c("sunrise", "sunset", "civil_dawn", "civil_dusk", "matutinal_end", "diurnal_end")) {
    if (!time_column %in% names(obs)) {
      obs[[time_column]] <- as.POSIXct(NA, tz = timezone)
    }
  }

  classifications <- lapply(seq_len(nrow(obs)), function(i) {
    classify_observation_time(
      obs$timestamp[[i]],
      obs$sunrise[[i]],
      obs$sunset[[i]],
      obs$civil_dawn[[i]],
      obs$civil_dusk[[i]]
    )
  })

  obs$day_night_class <- vapply(classifications, `[[`, character(1), "day_night_class")
  obs$diel_class <- vapply(classifications, `[[`, character(1), "diel_class")

  obs %>%
    dplyr::arrange(.observation_row_id) %>%
    dplyr::select(-.observation_row_id)
}

add_observation_daylight_classes <- function(obs, deps, existing_daily = NULL) {
  daylight_rows <- build_observation_daylight_rows(obs, deps)
  daily_lookup <- merge_environment_daylight_rows(existing_daily, daylight_rows)

  list(
    obs = add_observation_time_classes(obs, daily_lookup),
    environment_daily = daily_lookup
  )
}
