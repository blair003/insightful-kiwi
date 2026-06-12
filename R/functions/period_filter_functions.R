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

filter_deps_by_intervals <- function(deps, period_intervals) {
  if (is.null(period_intervals) || nrow(period_intervals) == 0) {
    return(deps[0, , drop = FALSE])
  }

  keep <- rep(FALSE, nrow(deps))
  for (i in seq_len(nrow(period_intervals))) {
    keep <- keep |
      deps$start <= as.Date(period_intervals$end_date[[i]]) &
      deps$end >= as.Date(period_intervals$start_date[[i]])
  }

  deps[keep, , drop = FALSE]
}

filter_deps_by_start_intervals <- function(deps, period_intervals) {
  if (is.null(period_intervals) || nrow(period_intervals) == 0) {
    return(deps[0, , drop = FALSE])
  }

  keep <- rep(FALSE, nrow(deps))
  for (i in seq_len(nrow(period_intervals))) {
    keep <- keep |
      as.Date(deps$start) >= as.Date(period_intervals$start_date[[i]]) &
      as.Date(deps$start) <= as.Date(period_intervals$end_date[[i]])
  }

  deps[keep, , drop = FALSE]
}

filter_obs_by_intervals <- function(obs, period_intervals) {
  if (is.null(period_intervals) || nrow(period_intervals) == 0) {
    return(obs[0, , drop = FALSE])
  }

  timezone <- obs_filter_timezone(obs)
  keep <- rep(FALSE, nrow(obs))
  for (i in seq_len(nrow(period_intervals))) {
    start_datetime <- obs_filter_boundary(period_intervals$start_date[[i]], timezone = timezone)
    end_datetime <- obs_filter_boundary(period_intervals$end_date[[i]], end_of_day = TRUE, timezone = timezone)
    keep <- keep | (obs$timestamp >= start_datetime & obs$timestamp <= end_datetime)
  }

  obs[keep, , drop = FALSE]
}

period_intervals_for_names <- function(period_groups, period_names) {
  flat_period_groups <- flatten_period_groups(period_groups)
  period_names <- as.character(period_names)
  period_names <- period_names[period_names %in% names(flat_period_groups)]

  if (length(period_names) == 0) {
    return(data.frame(
      period_name = character(0),
      start_date = as.Date(character(0)),
      end_date = as.Date(character(0)),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(lapply(period_names, function(period_name) {
    period <- flat_period_groups[[period_name]]
    data.frame(
      period_name = period_name,
      start_date = as.Date(period$start_date),
      end_date = as.Date(period$end_date),
      stringsAsFactors = FALSE
    )
  }))
}

period_names_with_deployments <- function(deps, period_groups, period_names = NULL) {
  if (is.null(period_names)) {
    period_names <- period_names_without_all(period_groups)
  }

  period_intervals <- period_intervals_for_names(period_groups, period_names)
  if (nrow(period_intervals) == 0 || nrow(deps) == 0) {
    return(character(0))
  }

  period_names[vapply(seq_along(period_names), function(i) {
    interval <- period_intervals[period_intervals$period_name == period_names[[i]], , drop = FALSE]
    if (nrow(interval) == 0) {
      return(FALSE)
    }

    any(as.Date(deps$start) >= interval$start_date[[1]] & as.Date(deps$start) <= interval$end_date[[1]], na.rm = TRUE)
  }, logical(1))]
}

period_assignment_column <- function(data, period_names, period_intervals = NULL) {
  period_names <- as.character(period_names)
  period_names <- period_names[!is.na(period_names) & nzchar(period_names)]
  if (length(period_names) == 0 || nrow(data) == 0) {
    return(NA_character_)
  }

  interval_columns <- character(0)
  if (!is.null(period_intervals) && nrow(period_intervals) > 0) {
    if ("period_family" %in% names(period_intervals)) {
      interval_columns <- c(interval_columns, as.character(period_intervals$period_family))
    }
    if ("period_type" %in% names(period_intervals)) {
      interval_columns <- c(interval_columns, as.character(period_intervals$period_type))
    }
  }

  candidate_columns <- unique(c(interval_columns, "period"))
  candidate_columns <- candidate_columns[!is.na(candidate_columns) & nzchar(candidate_columns)]
  candidate_columns <- candidate_columns[candidate_columns %in% names(data)]

  for (column in candidate_columns) {
    values <- unique(as.character(data[[column]]))
    if (all(period_names %in% values)) {
      return(column)
    }
  }

  NA_character_
}

filter_deps_by_period_names <- function(deps, period_names, start_date, end_date, period_intervals = NULL) {
  period_names <- as.character(period_names)
  period_names <- period_names[!is.na(period_names) & nzchar(period_names)]

  assignment_column <- period_assignment_column(deps, period_names, period_intervals)
  if (!is.na(assignment_column)) {
    return(deps %>% dplyr::filter(as.character(.data[[assignment_column]]) %in% period_names))
  }

  if (!is.null(period_intervals)) {
    return(filter_deps_by_start_intervals(deps, period_intervals))
  }

  filter_deps(deps, start_date, end_date)
}


filter_obs_by_period_names <- function(obs, period_names, start_date, end_date, period_intervals = NULL) {
  period_names <- as.character(period_names)
  period_names <- period_names[!is.na(period_names) & nzchar(period_names)]

  assignment_column <- period_assignment_column(obs, period_names, period_intervals)
  if (!is.na(assignment_column)) {
    return(obs %>% dplyr::filter(as.character(.data[[assignment_column]]) %in% period_names))
  }

  if (!is.null(period_intervals)) {
    return(filter_obs_by_intervals(obs, period_intervals))
  }

  filter_obs(obs, start_date, end_date)
}
