filter_deps <- function(deps, start_date, end_date) {
  deps %>%
    dplyr::filter(
      start <= as.Date(end_date),
      end >= as.Date(start_date)
    )
}


filter_obs <- function(obs, start_date, end_date) {
  obs %>%
    dplyr::filter(
      timestamp >= as.Date(start_date),
      timestamp <= as.Date(end_date)
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
