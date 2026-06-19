# period.R — derive the temporal segmentation substrate (ik_data$app$period) that
# every period selection hangs off. Two derived tables, built at import and joined
# on demand (never widened onto the pristine packages):
#   1. $deployments      — one row per deployment with its assigned calendar season
#                          (exactly one, by MAX TEMPORAL OVERLAP) + effort hours.
#   2. $monitoring_season — empirical monitored envelope per (season × reserve ×
#                          method): the real start/end of monitoring, which varies by
#                          reserve and method (a logistics artefact, not a fixed window).
# Season boundaries are LOCAL wall-clock, so we assign on localised deployment times
# (ik_localize_times). Southern-hemisphere 3-month bins; summer is labelled split-year
# ("Summer 2023/24"). The monitoring-YEAR rollup is deliberately not built here — it's
# a project convention deferred to config (see docs/data-model/04-data-selection.md).

#' `[start, end)` Date bounds of a season's three-month bin for an anchor year. Summer
#' anchors to its December year, so its Jan/Feb fall under the previous year's summer.
#' @keywords internal
ik_season_bounds <- function(name, anchor) {
  start <- as.Date(switch(name,
    Summer = sprintf("%d-12-01", anchor),
    Autumn = sprintf("%d-03-01", anchor),
    Winter = sprintf("%d-06-01", anchor),
    Spring = sprintf("%d-09-01", anchor)
  ))
  c(start, seq(start, by = "3 months", length.out = 2)[2])
}

#' Display label: split-year for summer ("Summer 2023/24"), else "Season Year".
#' @keywords internal
ik_season_label <- function(name, anchor) {
  ifelse(name == "Summer",
         sprintf("Summer %d/%02d", anchor, (anchor + 1L) %% 100L),
         sprintf("%s %d", name, anchor))
}

#' All season intervals (as POSIXct in `tzone`) that could overlap a date span, plus a
#' season either side for safety. @keywords internal
ik_season_grid <- function(min_t, max_t, tzone) {
  yr <- function(t) as.integer(format(t, "%Y", tz = tzone))
  years <- (yr(min_t) - 1L):(yr(max_t) + 1L)
  rows <- list()
  for (y in years) for (nm in c("Summer", "Autumn", "Winter", "Spring")) {
    b <- ik_season_bounds(nm, y)
    rows[[length(rows) + 1L]] <- data.frame(
      season = nm, season_year = y, calendar_season = ik_season_label(nm, y),
      start = as.POSIXct(paste(b[1], "00:00:00"), tz = tzone),
      end   = as.POSIXct(paste(b[2], "00:00:00"), tz = tzone),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' Assign each deployment interval to exactly one season by maximum overlap.
#'
#' @param start,end POSIXct vectors (localised) of deployment start/end.
#' @return data.frame(season, season_year, calendar_season), one row per input.
#' @keywords internal
ik_assign_season <- function(start, end) {
  n <- length(start)
  out <- data.frame(season = rep(NA_character_, n), season_year = rep(NA_integer_, n),
                    calendar_season = rep(NA_character_, n), stringsAsFactors = FALSE)
  ok <- !is.na(start) & !is.na(end)
  if (!any(ok)) return(out)

  tzone <- attr(start, "tzone"); if (is.null(tzone) || !nzchar(tzone)) tzone <- "Pacific/Auckland"
  e <- end; e[ok & e < start] <- start[ok & e < start]          # guard inverted spans
  grid <- ik_season_grid(min(start[ok]), max(e[ok]), tzone)

  for (i in which(ok)) {
    ov <- pmax(0, as.numeric(pmin(e[i], grid$end) - pmax(start[i], grid$start), units = "secs"))
    k  <- if (max(ov) > 0) which.max(ov) else                  # max-overlap season, or
      utils::tail(which(grid$start <= start[i]), 1L)           # the one containing start
    if (length(k)) out[i, ] <- grid[k, c("season", "season_year", "calendar_season")]
  }
  out
}

#' Build ik_data$app$period (deployment seasons + empirical monitoring-season table).
#'
#' @param datasets  ik_data$datasets list (each `$package` + `$meta`).
#' @param geography Built geography (for the location -> reserve join).
#' @return list(deployments, monitoring_season).
build_period <- function(datasets, geography) {
  loc_reserve <- stats::setNames(geography$locations$reserve,
                                 geography$locations$location_id)

  per <- lapply(names(datasets), function(id) {
    ds  <- datasets[[id]]
    dep <- camtrapdp::deployments(ds$package)
    dep <- ik_localize_times(dep, ds$meta, c("deploymentStart", "deploymentEnd"))
    sa  <- ik_assign_season(dep$deploymentStart, dep$deploymentEnd)
    data.frame(
      deploymentID    = dep$deploymentID,
      dataset         = id,
      source_type     = ds$meta$source_type %||% NA_character_,
      locationID      = as.character(dep$locationID),
      reserve         = unname(loc_reserve[as.character(dep$locationID)]),
      deploymentStart = dep$deploymentStart,
      deploymentEnd   = dep$deploymentEnd,
      effort_hours    = as.numeric(difftime(dep$deploymentEnd, dep$deploymentStart, units = "hours")),
      season          = sa$season,
      season_year     = sa$season_year,
      calendar_season = sa$calendar_season,
      stringsAsFactors = FALSE
    )
  })
  deployments <- dplyr::bind_rows(per)
  list(deployments = deployments,
       monitoring_season = build_monitoring_season(deployments))
}

#' Empirical monitoring-season envelopes per (season × reserve × method).
#'
#' Each deployment's interval is CLIPPED to every season it overlaps, so a single
#' season's row reflects only the monitoring active *within* that season. This is the
#' difference between pulse monitoring (cameras — one short window inside one season,
#' clip is a no-op) and continuous control (traps — a long span contributing its
#' active portion to each season it touches, not its whole length to one). `start`/`end`
#' are the empirical bounds, `n_deployments` the count active in-season, `effort_hours`
#' the clipped (in-season) effort — the correct denominator for seasonal rates.
#' @keywords internal
build_monitoring_season <- function(dep) {
  empty <- data.frame(calendar_season = character(), season = character(),
                      season_year = integer(), reserve = character(),
                      source_type = character(), start = as.POSIXct(character()),
                      end = as.POSIXct(character()), n_deployments = integer(),
                      effort_hours = numeric(), stringsAsFactors = FALSE)
  d <- dep[!is.na(dep$deploymentStart) & !is.na(dep$deploymentEnd), , drop = FALSE]
  if (!nrow(d)) return(empty)

  tzone <- attr(d$deploymentStart, "tzone")
  if (is.null(tzone) || !nzchar(tzone)) tzone <- "Pacific/Auckland"
  start_s <- as.numeric(d$deploymentStart)
  end_s   <- pmax(as.numeric(d$deploymentEnd), start_s)        # guard inverted spans
  grid <- ik_season_grid(min(d$deploymentStart), max(d$deploymentEnd), tzone)
  nd <- nrow(d); ns <- nrow(grid)

  # clip every deployment (rows) against every season (cols); keep positive overlaps
  cs <- pmax(matrix(start_s, nd, ns), matrix(as.numeric(grid$start), nd, ns, byrow = TRUE))
  ce <- pmin(matrix(end_s,   nd, ns), matrix(as.numeric(grid$end),   nd, ns, byrow = TRUE))
  keep <- which(ce > cs)
  if (!length(keep)) return(empty)
  ri <- ((keep - 1L) %%  nd) + 1L                               # deployment index
  ci <- ((keep - 1L) %/% nd) + 1L                               # season index

  data.frame(
    calendar_season = grid$calendar_season[ci], season = grid$season[ci],
    season_year = grid$season_year[ci], reserve = d$reserve[ri],
    source_type = d$source_type[ri], cstart = cs[keep], cend = ce[keep],
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(.data$calendar_season, .data$season, .data$season_year,
                    .data$reserve, .data$source_type) |>
    dplyr::summarise(
      start         = as.POSIXct(min(.data$cstart), origin = "1970-01-01", tz = tzone),
      end           = as.POSIXct(max(.data$cend),   origin = "1970-01-01", tz = tzone),
      n_deployments = dplyr::n(),
      effort_hours  = sum(.data$cend - .data$cstart) / 3600,
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$season_year, .data$season, .data$reserve, .data$source_type)
}

#' The temporal segmentation substrate.
#' @param ik_data The ik_data container.
#' @return ik_data$app$period (list of `deployments` + `monitoring_season`).
ik_period <- function(ik_data) ik_data$app$period

#' Per-deployment season attributes (keyed by deploymentID).
#' @param ik_data The ik_data container.
#' @return ik_data$app$period$deployments.
ik_deployment_period <- function(ik_data) ik_data$app$period$deployments

#' Empirical monitoring-season envelopes per (season × reserve × method).
#' @param ik_data The ik_data container.
#' @return ik_data$app$period$monitoring_season.
ik_monitoring_season <- function(ik_data) ik_data$app$period$monitoring_season
