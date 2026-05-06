# R/functions/weather_functions.R

weather_cache <- new.env()
open_meteo_rate_limit <- new.env()
open_meteo_rate_limit$request_times <- numeric(0)
open_meteo_rate_limit$backoff_until <- as.POSIXct(NA)
open_meteo_rate_limit$rate_limited <- FALSE

open_meteo_should_defer_on_rate_limit <- function() {
  isTRUE(getOption("insightfulkiwi.defer_weather_on_rate_limit", TRUE))
}

open_meteo_reset_rate_limit_flag <- function() {
  open_meteo_rate_limit$rate_limited <- FALSE
}

open_meteo_mark_rate_limited <- function() {
  open_meteo_rate_limit$rate_limited <- TRUE
}

open_meteo_rate_limit_hit <- function() {
  isTRUE(open_meteo_rate_limit$rate_limited)
}

open_meteo_prune_request_times <- function(now = Sys.time()) {
  cutoff <- as.numeric(now) - 24 * 60 * 60
  open_meteo_rate_limit$request_times <- open_meteo_rate_limit$request_times[
    open_meteo_rate_limit$request_times > cutoff
  ]
}

open_meteo_request_count <- function(window_seconds, now = Sys.time()) {
  open_meteo_prune_request_times(now)
  cutoff <- as.numeric(now) - window_seconds
  sum(open_meteo_rate_limit$request_times > cutoff)
}

open_meteo_wait_until_available <- function(cache_key) {
  limits <- list(
    minute = list(seconds = 60, max = 600),
    hour = list(seconds = 60 * 60, max = 5000),
    day = list(seconds = 24 * 60 * 60, max = 10000)
  )

  now <- Sys.time()
  if (!is.na(open_meteo_rate_limit$backoff_until) &&
      now < open_meteo_rate_limit$backoff_until) {
    wait_seconds <- max(1, ceiling(as.numeric(difftime(
      open_meteo_rate_limit$backoff_until,
      now,
      units = "secs"
    ))))
    logger::log_warn(
      "weather_functions.R, Open-Meteo backoff active before %s; waiting %s seconds",
      cache_key,
      wait_seconds
    )
    if (open_meteo_should_defer_on_rate_limit()) {
      open_meteo_mark_rate_limited()
      return(FALSE)
    }
    Sys.sleep(wait_seconds)
  }

  now <- Sys.time()
  hour_count <- open_meteo_request_count(limits$hour$seconds, now)
  day_count <- open_meteo_request_count(limits$day$seconds, now)

  if (hour_count >= limits$hour$max) {
    logger::log_error(
      "weather_functions.R, Open-Meteo hourly limit reached (%s/%s); skipping request for %s",
      hour_count,
      limits$hour$max,
      cache_key
    )
    return(FALSE)
  }

  if (day_count >= limits$day$max) {
    logger::log_error(
      "weather_functions.R, Open-Meteo daily limit reached (%s/%s); skipping request for %s",
      day_count,
      limits$day$max,
      cache_key
    )
    return(FALSE)
  }

  minute_count <- open_meteo_request_count(limits$minute$seconds, now)
  if (minute_count >= limits$minute$max) {
    cutoff <- as.numeric(now) - limits$minute$seconds
    oldest <- min(open_meteo_rate_limit$request_times[open_meteo_rate_limit$request_times > cutoff])
    wait_seconds <- max(1, ceiling(oldest + limits$minute$seconds - as.numeric(now)))
    logger::log_warn(
      "weather_functions.R, Open-Meteo minute limit reached before %s; waiting %s seconds",
      cache_key,
      wait_seconds
    )
    if (open_meteo_should_defer_on_rate_limit()) {
      open_meteo_mark_rate_limited()
      return(FALSE)
    }
    Sys.sleep(wait_seconds)
  }

  now <- Sys.time()
  hour_count <- open_meteo_request_count(limits$hour$seconds, now)
  day_count <- open_meteo_request_count(limits$day$seconds, now)

  if (hour_count >= limits$hour$max) {
    logger::log_error(
      "weather_functions.R, Open-Meteo hourly limit reached (%s/%s); skipping request for %s",
      hour_count,
      limits$hour$max,
      cache_key
    )
    return(FALSE)
  }

  if (day_count >= limits$day$max) {
    logger::log_error(
      "weather_functions.R, Open-Meteo daily limit reached (%s/%s); skipping request for %s",
      day_count,
      limits$day$max,
      cache_key
    )
    return(FALSE)
  }

  TRUE
}

open_meteo_record_request <- function(now = Sys.time()) {
  open_meteo_prune_request_times(now)
  open_meteo_rate_limit$request_times <- c(
    open_meteo_rate_limit$request_times,
    as.numeric(now)
  )
}

open_meteo_backoff <- function(wait_seconds = 60, cache_key = NA_character_) {
  wait_seconds <- max(1, as.integer(wait_seconds))
  open_meteo_rate_limit$backoff_until <- Sys.time() + wait_seconds
  logger::log_warn(
    "weather_functions.R, Open-Meteo rate limit response for %s; backing off for %s seconds",
    cache_key,
    wait_seconds
  )
}

weather_actual_timezone <- function() {
  if (exists("config", inherits = TRUE) &&
      !is.null(config$globals$actual_timezone) &&
      nzchar(config$globals$actual_timezone)) {
    return(config$globals$actual_timezone)
  }

  "Pacific/Auckland"
}

weather_playback_timezone <- function() {
  if (exists("config", inherits = TRUE) &&
      !is.null(config$globals$actual_timezone) &&
      nzchar(config$globals$actual_timezone)) {
    return(config$globals$actual_timezone)
  }

  weather_actual_timezone()
}

weather_html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

get_weather_description <- function(code) {
  # WMO Weather interpretation codes (WW)
  # https://open-meteo.com/en/docs
  desc <- switch(as.character(code),
                 "0" = list(label = "Clear sky", icon = "sun"),
                 "1" = list(label = "Mainly clear", icon = "cloud-sun"),
                 "2" = list(label = "Partly cloudy", icon = "cloud-sun"),
                 "3" = list(label = "Overcast", icon = "cloud"),
                 "45" = list(label = "Fog", icon = "smog"),
                 "48" = list(label = "Depositing rime fog", icon = "smog"),
                 "51" = list(label = "Light drizzle", icon = "cloud-rain"),
                 "53" = list(label = "Moderate drizzle", icon = "cloud-rain"),
                 "55" = list(label = "Dense drizzle", icon = "cloud-rain"),
                 "56" = list(label = "Light freezing drizzle", icon = "icicles"),
                 "57" = list(label = "Dense freezing drizzle", icon = "icicles"),
                 "61" = list(label = "Slight rain", icon = "cloud-showers-heavy"),
                 "63" = list(label = "Moderate rain", icon = "cloud-showers-heavy"),
                 "65" = list(label = "Heavy rain", icon = "cloud-showers-heavy"),
                 "66" = list(label = "Light freezing rain", icon = "cloud-meatball"),
                 "67" = list(label = "Heavy freezing rain", icon = "cloud-meatball"),
                 "71" = list(label = "Slight snow fall", icon = "snowflake"),
                 "73" = list(label = "Moderate snow fall", icon = "snowflake"),
                 "75" = list(label = "Heavy snow fall", icon = "snowflake"),
                 "77" = list(label = "Snow grains", icon = "snowflake"),
                 "80" = list(label = "Slight rain showers", icon = "cloud-showers-heavy"),
                 "81" = list(label = "Moderate rain showers", icon = "cloud-showers-heavy"),
                 "82" = list(label = "Violent rain showers", icon = "cloud-showers-heavy"),
                 "85" = list(label = "Slight snow showers", icon = "snowflake"),
                 "86" = list(label = "Heavy snow showers", icon = "snowflake"),
                 "95" = list(label = "Thunderstorm", icon = "bolt"),
                 "96" = list(label = "Thunderstorm with slight hail", icon = "bolt"),
                 "99" = list(label = "Thunderstorm with heavy hail", icon = "bolt"),
                 list(label = "Unknown", icon = "question")
  )
  return(desc)
}

get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  if (length(uniqv) == 0) return(NA)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fetch_weather_data <- function(lat, lng, start_date, end_date) {
  start_str <- format(as.Date(start_date), "%Y-%m-%d")
  end_str <- format(as.Date(end_date), "%Y-%m-%d")

  timezone <- weather_actual_timezone()
  cache_key <- paste(round(lat, 2), round(lng, 2), start_str, end_str, timezone, sep = "_")
  if (exists(cache_key, envir = weather_cache)) {
    logger::log_info("weather_functions.R, fetch_weather_data: using cached data for %s", cache_key)
    return(get(cache_key, envir = weather_cache))
  }

  timezone_query <- gsub("/", "%2F", timezone, fixed = TRUE)
  url <- sprintf("https://archive-api.open-meteo.com/v1/archive?latitude=%f&longitude=%f&start_date=%s&end_date=%s&daily=weathercode,temperature_2m_max,temperature_2m_min,precipitation_sum,sunrise,sunset&timezone=%s", lat, lng, start_str, end_str, timezone_query)

  logger::log_info("weather_functions.R, fetch_weather_data: querying open-meteo for %s", cache_key)

  daily <- tryCatch({
    daily_result <- NULL

    for (attempt in seq_len(2)) {
      if (open_meteo_rate_limit_hit() && open_meteo_should_defer_on_rate_limit()) {
        break
      }

      if (!open_meteo_wait_until_available(cache_key)) {
        break
      }

      open_meteo_record_request()
      res <- httr::GET(url, httr::timeout(8))
      status_code <- httr::status_code(res)
      response_text <- httr::content(res, "text", encoding = "UTF-8")

      if (status_code == 200) {
        parsed <- jsonlite::fromJSON(response_text)
        daily_result <- parsed$daily
        break
      }

      logger::log_error(
        "weather_functions.R: Failed to fetch weather data for %s. Status code: %s. Response: %s",
        cache_key,
        status_code,
        substr(response_text, 1, 300)
      )

      if (status_code == 429 && attempt == 1) {
        retry_after <- httr::headers(res)[["retry-after"]]
        wait_seconds <- suppressWarnings(as.integer(retry_after))
        if (is.na(wait_seconds)) {
          wait_seconds <- 60L
        }
        open_meteo_backoff(wait_seconds, cache_key)
        if (open_meteo_should_defer_on_rate_limit()) {
          open_meteo_mark_rate_limited()
          break
        }
        next
      }

      break
    }

    daily_result
  }, error = function(e) {
    logger::log_error("weather_functions.R: Weather fetch failed for %s: %s", cache_key, conditionMessage(e))
    NULL
  })

  if (!is.null(daily) && length(daily$time) > 0) {
    assign(cache_key, daily, envir = weather_cache)
    return(daily)
  }

  return(NULL)
}

weather_daily_to_df <- function(daily_data) {
  if (is.null(daily_data) || is.null(daily_data$time) || length(daily_data$time) == 0) {
    return(NULL)
  }

  weather_descriptions <- lapply(daily_data$weathercode, get_weather_description)

  data.frame(
    date = as.Date(daily_data$time),
    weathercode = daily_data$weathercode,
    weather_condition = vapply(weather_descriptions, `[[`, character(1), "label"),
    weather_icon = vapply(weather_descriptions, `[[`, character(1), "icon"),
    temperature_2m_max = daily_data$temperature_2m_max,
    temperature_2m_min = daily_data$temperature_2m_min,
    precipitation_sum = daily_data$precipitation_sum,
    sunrise = as.POSIXct(daily_data$sunrise, format = "%Y-%m-%dT%H:%M", tz = weather_playback_timezone()),
    sunset = as.POSIXct(daily_data$sunset, format = "%Y-%m-%dT%H:%M", tz = weather_playback_timezone()),
    stringsAsFactors = FALSE
  )
}

add_diel_boundaries_to_weather_daily <- function(weather_df) {
  if (is.null(weather_df) || nrow(weather_df) == 0) {
    return(weather_df)
  }

  if (!all(c("sunrise", "sunset") %in% names(weather_df))) {
    weather_df$matutinal_end <- as.POSIXct(NA, tz = weather_playback_timezone())
    weather_df$diurnal_end <- as.POSIXct(NA, tz = weather_playback_timezone())
    return(weather_df)
  }

  daylight <- as.numeric(difftime(weather_df$sunset, weather_df$sunrise, units = "secs"))
  invalid_daylight <- is.na(daylight) | daylight <= 0
  daylight[invalid_daylight] <- NA_real_

  weather_df$matutinal_end <- weather_df$sunrise + daylight / 3
  weather_df$diurnal_end <- weather_df$sunrise + 2 * daylight / 3
  weather_df
}

weather_daily_key <- function(weather_daily) {
  paste(as.character(weather_daily$locationID), as.character(weather_daily$date), sep = "|")
}

merge_weather_daily <- function(existing_weather_daily, new_weather_daily) {
  weather_daily <- dplyr::bind_rows(
    new_weather_daily,
    existing_weather_daily
  )

  if (is.null(weather_daily) || nrow(weather_daily) == 0) {
    return(NULL)
  }

  weather_daily <- weather_daily %>%
    dplyr::filter(!is.na(locationID), !is.na(date)) %>%
    dplyr::arrange(locationID, date) %>%
    dplyr::distinct(locationID, date, .keep_all = TRUE)

  add_diel_boundaries_to_weather_daily(weather_daily)
}

weather_coverage_status <- function(obs, weather_daily = NULL) {
  if (is.null(obs) || nrow(obs) == 0 ||
      !"timestamp" %in% names(obs) ||
      !"locationID" %in% names(obs)) {
    return(list(status = "not_applicable", required = 0L, available = 0L, missing = 0L))
  }

  required <- obs %>%
    dplyr::filter(!is.na(timestamp), !is.na(locationID)) %>%
    dplyr::mutate(date = as.Date(timestamp, tz = weather_playback_timezone())) %>%
    dplyr::distinct(locationID, date) %>%
    dplyr::filter(!is.na(date))

  required_count <- nrow(required)
  if (required_count == 0) {
    return(list(status = "not_applicable", required = 0L, available = 0L, missing = 0L))
  }

  if (is.null(weather_daily) || nrow(weather_daily) == 0 ||
      !all(c("locationID", "date") %in% names(weather_daily))) {
    return(list(status = "missing", required = required_count, available = 0L, missing = required_count))
  }

  available <- required %>%
    dplyr::inner_join(
      weather_daily %>% dplyr::distinct(locationID, date),
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

  weather_df <- weather_daily_to_df(fetch_weather_data(lat, lng, start_date, end_date))
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
  weather_df <- add_diel_boundaries_to_weather_daily(weather_df)

  weather_df[, c(
    intersect(c("locationID", "locationName", "locality", "latitude", "longitude"), names(weather_df)),
    setdiff(names(weather_df), c("locationID", "locationName", "locality", "latitude", "longitude"))
  )]
}

build_observation_weather_daily <- function(obs, deps, weather_daily = NULL) {
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
    dplyr::mutate(observation_date = as.Date(timestamp, tz = weather_playback_timezone())) %>%
    dplyr::distinct(locationID, observation_date) %>%
    dplyr::filter(!is.na(observation_date))

  if (nrow(required_dates) == 0) {
    return(add_diel_boundaries_to_weather_daily(weather_daily))
  }

  existing_weather <- add_diel_boundaries_to_weather_daily(weather_daily)
  if (!is.null(existing_weather) && nrow(existing_weather) > 0 &&
      all(c("locationID", "date") %in% names(existing_weather))) {
    existing_keys <- weather_daily_key(existing_weather)
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
    weather_df <- weather_daily_to_df(fetch_weather_data(
      group$weather_latitude[[1]],
      group$weather_longitude[[1]],
      group$start_date[[1]],
      group$end_date[[1]]
    ))
    if (is.null(weather_df) || nrow(weather_df) == 0) {
      return(NULL)
    }

    weather_df <- add_diel_boundaries_to_weather_daily(weather_df)
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

  merge_weather_daily(existing_weather, dplyr::bind_rows(weather_by_location))
}

classify_observation_time <- function(timestamp, sunrise, sunset, matutinal_end, diurnal_end) {
  timezone <- weather_playback_timezone()
  timestamp <- as.POSIXct(timestamp, tz = timezone)

  if (is.na(timestamp) || is.na(sunrise) || is.na(sunset) || sunset <= sunrise) {
    return(list(day_night_class = "Unknown", diel_class = "Unknown"))
  }

  day_night_class <- if (timestamp >= sunrise && timestamp < sunset) "Day" else "Night"
  diel_class <- if (timestamp < sunrise || timestamp >= sunset) {
    "Nocturnal"
  } else if (!is.na(matutinal_end) && timestamp < matutinal_end) {
    "Matutinal"
  } else if (!is.na(diurnal_end) && timestamp < diurnal_end) {
    "Diurnal"
  } else {
    "Vespertine"
  }

  list(day_night_class = day_night_class, diel_class = diel_class)
}

add_observation_time_classes <- function(obs, weather_daily = NULL) {
  if (is.null(obs) || nrow(obs) == 0 || !"timestamp" %in% names(obs)) {
    return(obs)
  }

  timezone <- weather_playback_timezone()
  obs$.observation_row_id <- seq_len(nrow(obs))
  obs <- obs %>%
    dplyr::select(-dplyr::any_of(c(
      "observation_date", "sunrise", "sunset", "matutinal_end", "diurnal_end",
      "weathercode", "temperature_2m_max", "temperature_2m_min",
      "precipitation_sum", "day_night_class", "diel_class"
    )))
  obs$observation_date <- as.Date(obs$timestamp, tz = timezone)

  if (!is.null(weather_daily) && nrow(weather_daily) > 0 &&
      all(c("locationID", "date") %in% names(weather_daily)) &&
      "locationID" %in% names(obs)) {
    weather_lookup <- weather_daily %>%
      dplyr::select(
        dplyr::any_of(c(
          "locationID", "date", "sunrise", "sunset", "matutinal_end",
          "diurnal_end", "weathercode", "temperature_2m_max",
          "temperature_2m_min", "precipitation_sum", "weather_condition",
          "weather_icon"
        ))
      ) %>%
      dplyr::rename(observation_date = date)

    obs <- obs %>%
      dplyr::left_join(weather_lookup, by = c("locationID", "observation_date"))
  } else {
    obs$sunrise <- as.POSIXct(NA, tz = timezone)
    obs$sunset <- as.POSIXct(NA, tz = timezone)
    obs$matutinal_end <- as.POSIXct(NA, tz = timezone)
    obs$diurnal_end <- as.POSIXct(NA, tz = timezone)
  }

  for (time_column in c("sunrise", "sunset", "matutinal_end", "diurnal_end")) {
    if (!time_column %in% names(obs)) {
      obs[[time_column]] <- as.POSIXct(NA, tz = timezone)
    }
  }

  classifications <- lapply(seq_len(nrow(obs)), function(i) {
    classify_observation_time(
      obs$timestamp[[i]],
      obs$sunrise[[i]],
      obs$sunset[[i]],
      obs$matutinal_end[[i]],
      obs$diurnal_end[[i]]
    )
  })

  obs$day_night_class <- vapply(classifications, `[[`, character(1), "day_night_class")
  obs$diel_class <- vapply(classifications, `[[`, character(1), "diel_class")

  obs <- obs %>%
    dplyr::arrange(.observation_row_id) %>%
    dplyr::select(-.observation_row_id)

  obs
}

enrich_observations_with_daily_weather <- function(obs, deps, weather_daily = NULL) {
  weather_daily <- build_observation_weather_daily(obs, deps, weather_daily)
  coverage <- weather_coverage_status(obs, weather_daily)
  if (open_meteo_rate_limit_hit()) {
    coverage$status <- if (coverage$available > 0) "deferred_partial" else "deferred"
  }

  list(
    obs = add_observation_time_classes(obs, weather_daily),
    weather_daily = weather_daily,
    weather_status = coverage
  )
}

fetch_weather_for_deployments <- function(deployments, start_date, end_date) {
  if (is.null(deployments) || nrow(deployments) == 0) {
    return(NULL)
  }

  lat <- mean(deployments$latitude, na.rm = TRUE)
  lng <- mean(deployments$longitude, na.rm = TRUE)
  if (is.na(lat) || is.na(lng)) {
    return(NULL)
  }

  add_diel_boundaries_to_weather_daily(weather_daily_to_df(fetch_weather_data(lat, lng, start_date, end_date)))
}

weather_daily_from_core_data <- function(deployments, start_date, end_date) {
  if (!exists("core_data", inherits = TRUE) ||
      is.null(core_data$weather_daily) ||
      is.null(deployments) ||
      nrow(deployments) == 0 ||
      !"locationID" %in% names(deployments)) {
    return(NULL)
  }

  weather_daily <- core_data$weather_daily
  if (is.null(weather_daily) || nrow(weather_daily) == 0 ||
      !all(c("locationID", "date", "sunrise", "sunset") %in% names(weather_daily))) {
    return(NULL)
  }

  location_ids <- unique(deployments$locationID)
  start_date <- as.Date(start_date, tz = weather_playback_timezone())
  end_date <- as.Date(end_date, tz = weather_playback_timezone())

  filtered_weather <- weather_daily %>%
    dplyr::filter(
      locationID %in% location_ids,
      date >= start_date,
      date <= end_date
    )

  if (nrow(filtered_weather) == 0) {
    return(NULL)
  }

  summarised <- filtered_weather %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      weathercode = get_mode(weathercode),
      temperature_2m_max = mean(temperature_2m_max, na.rm = TRUE),
      temperature_2m_min = mean(temperature_2m_min, na.rm = TRUE),
      precipitation_sum = mean(precipitation_sum, na.rm = TRUE),
      sunrise = as.POSIXct(mean(as.numeric(sunrise), na.rm = TRUE), origin = "1970-01-01", tz = weather_playback_timezone()),
      sunset = as.POSIXct(mean(as.numeric(sunset), na.rm = TRUE), origin = "1970-01-01", tz = weather_playback_timezone()),
      .groups = "drop"
    )

  add_diel_boundaries_to_weather_daily(summarised)
}

weather_daily_as_api_daily <- function(weather_df) {
  if (is.null(weather_df) || nrow(weather_df) == 0) {
    return(NULL)
  }

  list(
    time = as.character(weather_df$date),
    weathercode = weather_df$weathercode,
    weather_condition = if ("weather_condition" %in% names(weather_df)) weather_df$weather_condition else NULL,
    weather_icon = if ("weather_icon" %in% names(weather_df)) weather_df$weather_icon else NULL,
    temperature_2m_max = weather_df$temperature_2m_max,
    temperature_2m_min = weather_df$temperature_2m_min,
    precipitation_sum = weather_df$precipitation_sum,
    sunrise = weather_df$sunrise,
    sunset = weather_df$sunset
  )
}

format_weather_clock_time <- function(value) {
  time_value <- if (inherits(value, "POSIXt")) {
    as.POSIXct(value, tz = weather_actual_timezone())
  } else {
    as.POSIXct(value, format="%Y-%m-%dT%H:%M", tz=weather_actual_timezone())
  }

  format(time_value, "%H:%M")
}

playback_weather_for_deployments <- function(deployments, start_date, end_date) {
  stored_weather <- weather_daily_from_core_data(deployments, start_date, end_date)
  if (!is.null(stored_weather) && nrow(stored_weather) > 0) {
    return(stored_weather)
  }

  fetch_weather_for_deployments(deployments, start_date, end_date)
}

weather_row_for_time <- function(weather_df, time_value) {
  if (is.null(weather_df) || nrow(weather_df) == 0 || is.null(time_value) || is.na(time_value)) {
    return(NULL)
  }

  target_date <- as.Date(time_value, tz = weather_playback_timezone())
  row <- weather_df[weather_df$date == target_date, , drop = FALSE]
  if (nrow(row) == 0) {
    return(NULL)
  }

  row[1, , drop = FALSE]
}

time_of_day_info <- function(weather_df, time_value, mode = "day") {
  if (!mode %in% c("hour", "day_night", "diel") ||
      is.null(time_value) || is.na(time_value)) {
    return(NULL)
  }

  timezone <- weather_playback_timezone()
  time_value <- as.POSIXct(time_value, tz = timezone)
  weather_row <- weather_row_for_time(weather_df, time_value)
  hour_value <- as.integer(format(time_value, "%H", tz = timezone))

  fallback <- if (!is.na(hour_value) && hour_value >= 6 && hour_value < 18) {
    list(label = "Day", icon = "sun")
  } else {
    list(label = "Night", icon = "moon")
  }

  if (is.null(weather_row) || nrow(weather_row) == 0 ||
      is.na(weather_row$sunrise[[1]]) || is.na(weather_row$sunset[[1]])) {
    return(fallback)
  }

  sunrise <- weather_row$sunrise[[1]]
  sunset <- weather_row$sunset[[1]]
  if (sunset <= sunrise) {
    return(fallback)
  }

  if (!identical(mode, "diel")) {
    if (time_value >= sunrise && time_value < sunset) {
      return(list(label = "Day", icon = "sun"))
    }
    return(list(label = "Night", icon = "moon"))
  }

  daylight <- as.numeric(difftime(sunset, sunrise, units = "secs"))
  matutinal_end <- sunrise + daylight / 3
  diurnal_end <- sunrise + 2 * daylight / 3

  if (time_value < sunrise || time_value >= sunset) {
    return(list(label = "Nocturnal", icon = "moon"))
  }
  if (time_value < matutinal_end) {
    return(list(label = "Matutinal", icon = c("moon", "arrow-right", "sun")))
  }
  if (time_value < diurnal_end) {
    return(list(label = "Diurnal", icon = "sun"))
  }
  list(label = "Vespertine", icon = c("sun", "arrow-right", "moon"))
}

render_time_of_day_icon_html <- function(icon) {
  if (is.null(icon) || length(icon) == 0) {
    return("")
  }

  icon_names <- vapply(icon, weather_html_escape, character(1))
  icons <- paste0(
    sprintf("<i class='fa-solid fa-%s fa fa-%s map-time-of-day-icon'></i>", icon_names, icon_names),
    collapse = ""
  )

  if (length(icon_names) == 1) {
    return(sprintf("%s", icons))
  }

  sprintf("<span class='map-time-of-day-icon-group'>%s</span>", icons)
}

render_weather_map_control <- function(weather_row, time_info = NULL) {
  if (is.null(weather_row) || nrow(weather_row) == 0 || is.na(weather_row$weathercode[[1]])) {
    return(NULL)
  }

  description <- get_weather_description(weather_row$weathercode[[1]])
  rain <- if (!is.na(weather_row$precipitation_sum[[1]])) {
    sprintf("%.1f mm", weather_row$precipitation_sum[[1]])
  } else {
    "Rain unknown"
  }

  icon_name <- weather_html_escape(description$icon)
  icon_html <- sprintf("<i class='fa-solid fa-%s fa fa-%s'></i>", icon_name, icon_name)
  time_icon_html <- ""
  time_label <- ""
  if (!is.null(time_info) && !is.null(time_info$icon) && !is.null(time_info$label)) {
    time_icon_html <- render_time_of_day_icon_html(time_info$icon)
    time_label <- paste0(weather_html_escape(time_info$label), " • ")
  }

  sprintf(
    "<div class='map-weather-badge' title='%s, %s'><div class='map-weather-icons'>%s%s</div><span>%s</span><small>%s%s</small></div>",
    weather_html_escape(description$label),
    weather_html_escape(format(weather_row$date[[1]], "%d %b %Y")),
    icon_html,
    time_icon_html,
    weather_html_escape(description$label),
    time_label,
    weather_html_escape(rain)
  )
}

fallback_day_night_boundaries <- function(start_time, end_time) {
  timezone <- weather_playback_timezone()
  start_day <- as.Date(start_time, tz = timezone) - 1
  end_day <- as.Date(end_time, tz = timezone) + 1
  days <- seq(start_day, end_day, by = "day")
  sort(as.POSIXct(c(paste(days, "06:00:00"), paste(days, "18:00:00")), tz = timezone))
}

normalise_playback_boundaries <- function(boundaries, start_time, end_time) {
  boundaries <- boundaries[!is.na(boundaries)]
  if (length(boundaries) == 0) {
    return(fallback_day_night_boundaries(start_time, end_time))
  }

  timezone <- weather_playback_timezone()
  boundaries <- sort(unique(as.POSIXct(boundaries, origin = "1970-01-01", tz = timezone)))
  boundaries[boundaries >= start_time & boundaries <= end_time]
}

day_night_boundaries <- function(weather_df, start_time, end_time) {
  if (is.null(start_time) || is.null(end_time) || is.na(start_time) || is.na(end_time)) {
    return(as.POSIXct(character(), tz = weather_playback_timezone()))
  }

  if (is.null(weather_df) || nrow(weather_df) == 0 ||
      !"sunrise" %in% names(weather_df) || !"sunset" %in% names(weather_df)) {
    return(fallback_day_night_boundaries(start_time, end_time))
  }

  normalise_playback_boundaries(c(weather_df$sunrise, weather_df$sunset), start_time, end_time)
}

diel_activity_boundaries <- function(weather_df, start_time, end_time) {
  if (is.null(start_time) || is.null(end_time) || is.na(start_time) || is.na(end_time)) {
    return(as.POSIXct(character(), tz = weather_playback_timezone()))
  }

  if (is.null(weather_df) || nrow(weather_df) == 0 ||
      !"sunrise" %in% names(weather_df) || !"sunset" %in% names(weather_df)) {
    return(fallback_day_night_boundaries(start_time, end_time))
  }

  boundaries <- unlist(lapply(seq_len(nrow(weather_df)), function(i) {
    sunrise <- weather_df$sunrise[[i]]
    sunset <- weather_df$sunset[[i]]
    if (is.na(sunrise) || is.na(sunset) || sunset <= sunrise) {
      return(NULL)
    }

    daylight <- as.numeric(difftime(sunset, sunrise, units = "secs"))
    c(
      sunrise,
      sunrise + daylight / 3,
      sunrise + 2 * daylight / 3,
      sunset
    )
  }))

  normalise_playback_boundaries(as.POSIXct(boundaries, origin = "1970-01-01", tz = weather_playback_timezone()), start_time, end_time)
}

playback_weather_boundaries <- function(mode, weather_df, start_time, end_time) {
  if (identical(mode, "diel")) {
    return(diel_activity_boundaries(weather_df, start_time, end_time))
  }

  day_night_boundaries(weather_df, start_time, end_time)
}

next_weather_boundary <- function(current_time, end_time, weather_df, mode = "day_night") {
  boundaries <- playback_weather_boundaries(mode, weather_df, current_time, end_time)
  next_boundary <- boundaries[boundaries > current_time]
  if (length(next_boundary) == 0) {
    return(end_time + 1)
  }

  min(next_boundary, na.rm = TRUE)
}

previous_weather_boundary <- function(current_time, start_time, weather_df, mode = "day_night") {
  boundaries <- playback_weather_boundaries(mode, weather_df, start_time, current_time)
  previous_boundary <- boundaries[boundaries < current_time]
  if (length(previous_boundary) == 0) {
    return(start_time)
  }

  max(previous_boundary, na.rm = TRUE)
}

next_day_night_boundary <- function(current_time, end_time, weather_df) {
  next_weather_boundary(current_time, end_time, weather_df, mode = "day_night")
}

previous_day_night_boundary <- function(current_time, start_time, weather_df) {
  previous_weather_boundary(current_time, start_time, weather_df, mode = "day_night")
}

summarise_weather <- function(daily_data) {
  if (is.null(daily_data)) {
    return(NULL)
  }

  total_rain <- sum(daily_data$precipitation_sum, na.rm = TRUE)

  # Count heavy rain days (>= 10mm)
  heavy_rain_days <- sum(daily_data$precipitation_sum >= 10.0, na.rm = TRUE)

  dominant_code <- get_mode(daily_data$weathercode)
  avg_max_temp <- mean(daily_data$temperature_2m_max, na.rm = TRUE)
  avg_min_temp <- mean(daily_data$temperature_2m_min, na.rm = TRUE)

  # Format sunrise and sunset times (average them by converting to seconds since midnight)
  get_avg_time <- function(times) {
    if (length(times) == 0 || all(is.na(times))) return(NA)
    times_posix <- if (inherits(times, "POSIXt")) {
      as.POSIXct(times, tz = weather_actual_timezone())
    } else {
      as.POSIXct(times, format="%Y-%m-%dT%H:%M", tz=weather_actual_timezone())
    }
    # extract hours and minutes as seconds from midnight
    secs <- as.numeric(format(times_posix, "%H")) * 3600 + as.numeric(format(times_posix, "%M")) * 60
    avg_secs <- mean(secs, na.rm = TRUE)
    h <- floor(avg_secs / 3600)
    m <- floor((avg_secs %% 3600) / 60)
    sprintf("%02d:%02d", h, m)
  }

  avg_sunrise <- get_avg_time(daily_data$sunrise)
  avg_sunset <- get_avg_time(daily_data$sunset)

  list(
    condition_code = dominant_code,
    condition = get_weather_description(dominant_code),
    total_rain = total_rain,
    heavy_rain_days = heavy_rain_days,
    avg_max_temp = avg_max_temp,
    avg_min_temp = avg_min_temp,
    avg_sunrise = avg_sunrise,
    avg_sunset = avg_sunset
  )
}

render_weather_info_link <- function(lat, lng, start_date, end_date, input_id = "dashboard_weather_details_clicked", locality = NULL) {
  token <- list(
    lat = lat,
    lng = lng,
    start_date = format(as.Date(start_date), "%Y-%m-%d"),
    end_date = format(as.Date(end_date), "%Y-%m-%d"),
    locality = if (is.null(locality)) "ALL" else paste(as.character(locality), collapse = ",")
  )

  tags$a(
    href = "#",
    class = "dashcard-info-link",
    title = "Show daily weather",
    onclick = sprintf(
      "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
      input_id,
      jsonlite::toJSON(token, auto_unbox = TRUE)
    ),
    icon("circle-info")
  )
}

render_weather_cards <- function(locality, start_date, end_date, info_input_id = "dashboard_weather_details_clicked") {
  # Compute average lat/lng for the deployments
  # core_data$deps is available globally in the environment
  if (!is.null(locality) && length(locality) > 0 && !all(locality == "ALL")) {
    deps <- core_data$deps %>% dplyr::filter(locality %in% !!locality)
  } else {
    deps <- core_data$deps
  }

  lat <- mean(deps$latitude, na.rm = TRUE)
  lng <- mean(deps$longitude, na.rm = TRUE)

  stored_weather <- weather_daily_from_core_data(deps, start_date, end_date)
  daily_data <- weather_daily_as_api_daily(stored_weather)
  if (is.null(daily_data)) {
    daily_data <- fetch_weather_data(lat, lng, start_date, end_date)
  }
  summary <- summarise_weather(daily_data)

  if (is.null(summary)) {
    return(div(class = "text-muted", "Weather data unavailable for this period."))
  }

  info_link_html <- render_weather_info_link(lat, lng, start_date, end_date, info_input_id, locality)

  rain_text <- if (summary$heavy_rain_days > 0) {
    sprintf("Rain: %.1f mm (%d heavy days)", summary$total_rain, summary$heavy_rain_days)
  } else {
    sprintf("Rain: %.1f mm", summary$total_rain)
  }

  layout_column_wrap(
    width = "180px",

    card(
      card_header(render_dashboard_card_header(summary$condition$icon, "Predominant Weather")),
      card_body(
        div(
          class = "dashcard-metric-state",
          div(class = "dashcard-card-action", info_link_html),
          div(summary$condition$label, class = "dashcard-output"),
          div(rain_text, class = "dashcard-period")
        )
      ),
      full_screen = FALSE
    ),

    card(
      card_header(render_dashboard_card_header("temperature-half", "Temperature")),
      card_body(
        div(
          class = "dashcard-metric-state",
          div(sprintf("%.1f°C", summary$avg_max_temp), class = "dashcard-output"),
          div(sprintf("Min: %.1f°C", summary$avg_min_temp), class = "dashcard-period")
        )
      ),
      full_screen = FALSE
    ),

    card(
      card_header(render_dashboard_card_header("sun", "Daylight")),
      card_body(
        div(
          class = "dashcard-metric-state",
          div(summary$avg_sunrise, class = "dashcard-output"),
          div(sprintf("Sunset: %s", summary$avg_sunset), class = "dashcard-period")
        )
      ),
      full_screen = FALSE
    )
  )
}

show_weather_modal <- function(lat, lng, start_date, end_date, locality = NULL) {
  deps <- NULL
  if (exists("core_data", inherits = TRUE) && !is.null(core_data$deps)) {
    if (!is.null(locality) && nzchar(locality) && locality != "ALL") {
      localities <- strsplit(locality, ",", fixed = TRUE)[[1]]
      deps <- core_data$deps %>% dplyr::filter(locality %in% localities)
    } else {
      deps <- core_data$deps
    }
  }

  stored_weather <- weather_daily_from_core_data(deps, start_date, end_date)
  daily_data <- weather_daily_as_api_daily(stored_weather)
  if (is.null(daily_data)) {
    daily_data <- fetch_weather_data(lat, lng, start_date, end_date)
  }
  if (is.null(daily_data)) {
    showNotification("Failed to fetch weather details.", type = "error")
    return()
  }

  df <- data.frame(
    Date = as.Date(daily_data$time),
    Condition = if (!is.null(daily_data$weather_condition)) {
      daily_data$weather_condition
    } else {
      sapply(daily_data$weathercode, function(c) get_weather_description(c)$label)
    },
    Max_Temp = sprintf("%.1f °C", daily_data$temperature_2m_max),
    Min_Temp = sprintf("%.1f °C", daily_data$temperature_2m_min),
    Rainfall = sprintf("%.1f mm", daily_data$precipitation_sum),
    Sunrise = sapply(daily_data$sunrise, format_weather_clock_time),
    Sunset = sapply(daily_data$sunset, format_weather_clock_time)
  )

  showModal(modalDialog(
    title = sprintf("Daily Weather: %s to %s", start_date, end_date),
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),
    tags$small("Weather data from open-meteo.com may lack accuracy. Use as a general guide only."),
    div(
      DT::renderDataTable({
        DT::datatable(df, options = list(pageLength = 10, dom = 'tip'), rownames = FALSE)
      })
    )
  ))
}
