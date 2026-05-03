# R/functions/weather_functions.R

weather_cache <- new.env()

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
    res <- httr::GET(url, httr::timeout(8))
    if (httr::status_code(res) == 200) {
      parsed <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
      parsed$daily
    } else {
      logger::log_error("weather_functions.R: Failed to fetch weather data. Status code: %s", httr::status_code(res))
      NULL
    }
  }, error = function(e) {
    logger::log_warn("weather_functions.R: Weather fetch failed for %s: %s", cache_key, conditionMessage(e))
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

  data.frame(
    date = as.Date(daily_data$time),
    weathercode = daily_data$weathercode,
    temperature_2m_max = daily_data$temperature_2m_max,
    temperature_2m_min = daily_data$temperature_2m_min,
    precipitation_sum = daily_data$precipitation_sum,
    sunrise = as.POSIXct(daily_data$sunrise, format = "%Y-%m-%dT%H:%M", tz = weather_playback_timezone()),
    sunset = as.POSIXct(daily_data$sunset, format = "%Y-%m-%dT%H:%M", tz = weather_playback_timezone()),
    stringsAsFactors = FALSE
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

  weather_daily_to_df(fetch_weather_data(lat, lng, start_date, end_date))
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
    times_posix <- as.POSIXct(times, format="%Y-%m-%dT%H:%M", tz=weather_actual_timezone())
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

render_weather_info_link <- function(lat, lng, start_date, end_date, input_id = "dashboard_weather_details_clicked") {
  token <- list(
    lat = lat,
    lng = lng,
    start_date = format(as.Date(start_date), "%Y-%m-%d"),
    end_date = format(as.Date(end_date), "%Y-%m-%d")
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

  daily_data <- fetch_weather_data(lat, lng, start_date, end_date)
  summary <- summarise_weather(daily_data)

  if (is.null(summary)) {
    return(div(class = "text-muted", "Weather data unavailable for this period."))
  }

  info_link_html <- render_weather_info_link(lat, lng, start_date, end_date, info_input_id)

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

show_weather_modal <- function(lat, lng, start_date, end_date) {
  daily_data <- fetch_weather_data(lat, lng, start_date, end_date)
  if (is.null(daily_data)) {
    showNotification("Failed to fetch weather details.", type = "error")
    return()
  }

  df <- data.frame(
    Date = as.Date(daily_data$time),
    Condition = sapply(daily_data$weathercode, function(c) get_weather_description(c)$label),
    Max_Temp = sprintf("%.1f °C", daily_data$temperature_2m_max),
    Min_Temp = sprintf("%.1f °C", daily_data$temperature_2m_min),
    Rainfall = sprintf("%.1f mm", daily_data$precipitation_sum),
    Sunrise = sapply(daily_data$sunrise, function(t) format(as.POSIXct(t, format="%Y-%m-%dT%H:%M", tz=weather_actual_timezone()), "%H:%M")),
    Sunset = sapply(daily_data$sunset, function(t) format(as.POSIXct(t, format="%Y-%m-%dT%H:%M", tz=weather_actual_timezone()), "%H:%M"))
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
