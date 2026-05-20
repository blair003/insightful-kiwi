filter_deps <- function(deps, start_date, end_date) {
  deps %>%
    dplyr::filter(
      start <= as.Date(end_date),
      end >= as.Date(start_date)
    )
}


obs_filter_timezone <- function(obs) {
  timestamp_tz <- attr(obs$timestamp, "tzone")
  if (length(timestamp_tz) > 0 && nzchar(timestamp_tz[[1]])) {
    return(timestamp_tz[[1]])
  }

  "UTC"
}

obs_filter_boundary <- function(value, end_of_day = FALSE, timezone = "UTC") {
  if (inherits(value, "POSIXt")) {
    return(value)
  }

  boundary_time <- if (isTRUE(end_of_day)) "23:59:59" else "00:00:00"
  as.POSIXct(paste(as.Date(value), boundary_time), tz = timezone)
}

filter_obs <- function(obs, start_date, end_date) {
  timezone <- obs_filter_timezone(obs)
  start_datetime <- obs_filter_boundary(start_date, timezone = timezone)
  end_datetime <- obs_filter_boundary(end_date, end_of_day = TRUE, timezone = timezone)

  obs %>%
    dplyr::filter(
      timestamp >= start_datetime,
      timestamp <= end_datetime
    )
}

filter_deps_by_period_names <- function(deps, period_names, start_date, end_date) {
  period_names <- as.character(period_names)
  period_names <- period_names[!is.na(period_names) & nzchar(period_names)]

  if ("period" %in% names(deps) && length(period_names) > 0) {
    return(deps %>% dplyr::filter(as.character(period) %in% period_names))
  }

  filter_deps(deps, start_date, end_date)
}


filter_obs_by_period_names <- function(obs, period_names, start_date, end_date) {
  period_names <- as.character(period_names)
  period_names <- period_names[!is.na(period_names) & nzchar(period_names)]

  if ("period" %in% names(obs) && length(period_names) > 0) {
    return(obs %>% dplyr::filter(as.character(period) %in% period_names))
  }

  filter_obs(obs, start_date, end_date)
}
