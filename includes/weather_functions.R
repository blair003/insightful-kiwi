# includes/weather_functions.R

weather_cache <- new.env()

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

  cache_key <- paste(round(lat, 2), round(lng, 2), start_str, end_str, sep = "_")
  if (exists(cache_key, envir = weather_cache)) {
    logger::log_info("weather_functions.R, fetch_weather_data: using cached data for %s", cache_key)
    return(get(cache_key, envir = weather_cache))
  }

  url <- sprintf("https://archive-api.open-meteo.com/v1/archive?latitude=%f&longitude=%f&start_date=%s&end_date=%s&daily=weathercode,temperature_2m_max,temperature_2m_min,precipitation_sum,sunrise,sunset&timezone=Pacific%%2FAuckland", lat, lng, start_str, end_str)

  logger::log_info("weather_functions.R, fetch_weather_data: querying open-meteo for %s", cache_key)

  res <- httr::GET(url)
  if (httr::status_code(res) == 200) {
    parsed <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    daily <- parsed$daily
    if (!is.null(daily) && length(daily$time) > 0) {
      assign(cache_key, daily, envir = weather_cache)
      return(daily)
    }
  } else {
    logger::log_error("weather_functions.R: Failed to fetch weather data. Status code: %s", httr::status_code(res))
  }

  return(NULL)
}

summarise_weather <- function(daily_data) {
  if (is.null(daily_data)) {
    return(NULL)
  }

  total_rain <- sum(daily_data$precipitation_sum, na.rm = TRUE)

  # Determine dominant condition based on rainfall contribution
  # If there is meaningful rain, pick the weather code that contributed the most rainfall
  if (total_rain > 5) {
    df <- data.frame(code = daily_data$weathercode, rain = daily_data$precipitation_sum)
    # Aggregate rain by weather code
    rain_by_code <- aggregate(rain ~ code, data = df, sum, na.rm = TRUE)
    # Get the code with the highest total rain
    dominant_code <- rain_by_code$code[which.max(rain_by_code$rain)]
  } else {
    dominant_code <- get_mode(daily_data$weathercode)
  }
  avg_max_temp <- mean(daily_data$temperature_2m_max, na.rm = TRUE)
  avg_min_temp <- mean(daily_data$temperature_2m_min, na.rm = TRUE)

  # Format sunrise and sunset times (average them by converting to seconds since midnight)
  get_avg_time <- function(times) {
    if (length(times) == 0 || all(is.na(times))) return(NA)
    times_posix <- as.POSIXct(times, format="%Y-%m-%dT%H:%M", tz="Pacific/Auckland")
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
    avg_max_temp = avg_max_temp,
    avg_min_temp = avg_min_temp,
    avg_sunrise = avg_sunrise,
    avg_sunset = avg_sunset
  )
}

render_weather_info_link <- function(lat, lng, start_date, end_date) {
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
      "Shiny.setInputValue('dashboard_weather_details_clicked', %s, {priority: 'event'}); return false;",
      jsonlite::toJSON(token, auto_unbox = TRUE)
    ),
    icon("circle-info")
  )
}

render_weather_cards <- function(locality, start_date, end_date) {
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

  info_link_html <- render_weather_info_link(lat, lng, start_date, end_date)

  layout_column_wrap(
    width = "180px",

    card(
      card_header(render_dashboard_card_header(summary$condition$icon, "Weather")),
      card_body(
        div(
          class = "dashcard-metric-state",
          div(class = "dashcard-card-action", info_link_html),
          div(summary$condition$label, class = "dashcard-output"),
          div(sprintf("Rain: %.1f mm", summary$total_rain), class = "dashcard-period")
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
    Sunrise = sapply(daily_data$sunrise, function(t) format(as.POSIXct(t, format="%Y-%m-%dT%H:%M", tz="Pacific/Auckland"), "%H:%M")),
    Sunset = sapply(daily_data$sunset, function(t) format(as.POSIXct(t, format="%Y-%m-%dT%H:%M", tz="Pacific/Auckland"), "%H:%M"))
  )

  showModal(modalDialog(
    title = sprintf("Daily Weather: %s to %s", start_date, end_date),
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),
    div(
      DT::renderDataTable({
        DT::datatable(df, options = list(pageLength = 10, dom = 'tip'), rownames = FALSE)
      })
    )
  ))
}
