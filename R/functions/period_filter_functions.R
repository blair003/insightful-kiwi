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
