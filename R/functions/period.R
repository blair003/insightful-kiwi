# period.R — derive the temporal segmentation substrate (ik_data$app$period) that
# every period selection hangs off. Two derived tables, built at import and joined
# on demand (never widened onto the pristine packages):
#   1. $deployments      — one row per deployment with its assigned calendar season
#                          (exactly one, by MAX TEMPORAL OVERLAP) + effort hours.
#   2. $monitoring_season — empirical monitored envelope per (season × reserve ×
#                          source_type): the real start/end of monitoring, which varies
#                          by reserve and source_type (a logistics artefact, not fixed).
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

#' Assign each interval to exactly one season by maximum overlap (vectorised).
#'
#' Works for both deployment spans and observation events. An instant (or a span that
#' touches no season interior, e.g. a date-only event on a boundary) falls back to the
#' season *containing* its start. `end` may be `NA` (treated as an instant at `start`).
#'
#' @param start,end POSIXct vectors (localised).
#' @return data.frame(season, season_year, calendar_season), one row per input.
#' @keywords internal
ik_assign_season <- function(start, end) {
  n <- length(start)
  out <- data.frame(season = rep(NA_character_, n), season_year = rep(NA_integer_, n),
                    calendar_season = rep(NA_character_, n), stringsAsFactors = FALSE)
  ok <- !is.na(start)
  if (!any(ok)) return(out)

  tzone <- attr(start, "tzone"); if (is.null(tzone) || !nzchar(tzone)) tzone <- "Pacific/Auckland"
  s <- as.numeric(start)
  e <- as.numeric(end); e[is.na(e)] <- s[is.na(e)]; e <- pmax(e, s)   # NA/inverted -> instant
  idx <- which(ok)
  grid <- ik_season_grid(min(start[ok]),
                         as.POSIXct(max(e[idx]), origin = "1970-01-01", tz = tzone), tzone)
  grid <- grid[order(grid$start), ]                                   # contiguous partition
  gs <- as.numeric(grid$start); ge <- as.numeric(grid$end)
  ni <- length(idx); ns <- nrow(grid)

  cs <- pmax(matrix(s[idx], ni, ns), matrix(gs, ni, ns, byrow = TRUE))
  ce <- pmin(matrix(e[idx], ni, ns), matrix(ge, ni, ns, byrow = TRUE))
  ov <- ce - cs; ov[ov < 0] <- 0
  best <- max.col(ov, ties.method = "first")                         # season of max overlap
  zero <- rowSums(ov) == 0                                           # instants -> containing
  if (any(zero)) best[zero] <- pmin(pmax(findInterval(s[idx][zero], gs), 1L), ns)

  out$season[idx]          <- grid$season[best]
  out$season_year[idx]     <- grid$season_year[best]
  out$calendar_season[idx] <- grid$calendar_season[best]
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

  # Observation seasons. PULSE sources (camera) are ANCHORED ON THE DEPLOYMENT: every
  # detection inherits its deployment's (majority-overlap) season, so a ~3-week monitoring
  # block that grazes the next season keeps ALL its detections together under the season it
  # ran in — not split by the wall-clock date a few late captures happen to carry. This
  # matches the documented "period is anchored on the deployment" model and the Camera-review
  # page, and is robust to a misset camera clock. CONTINUOUS sources (traps) genuinely span
  # seasons, so each capture is attributed by its CHECK date (eventEnd, falling back to
  # eventStart) — a trap capture is only resolved to its check interval, see wkt_trapping.R.
  obs_per <- lapply(names(datasets), function(id) {
    ds    <- datasets[[id]]
    obs   <- camtrapdp::observations(ds$package)
    obs   <- ik_localize_times(obs, ds$meta, c("eventStart", "eventEnd"))
    pulse <- identical(ds$meta$source_type, "camera")
    if (pulse) {
      ds_dep <- deployments[deployments$dataset == id, , drop = FALSE]   # this dataset's deployment seasons
      mi  <- match(as.character(obs$deploymentID), ds_dep$deploymentID)
      snm <- ds_dep$season[mi]; syr <- ds_dep$season_year[mi]; cse <- ds_dep$calendar_season[mi]
      miss <- is.na(cse)                                                 # orphan deployment → own-date fallback
      if (any(miss)) {
        when <- if ("eventEnd" %in% names(obs)) obs$eventEnd else obs$eventStart
        nawh <- is.na(when); when[nawh] <- obs$eventStart[nawh]
        sa   <- ik_assign_season(when[miss], when[miss])
        snm[miss] <- sa$season; syr[miss] <- sa$season_year; cse[miss] <- sa$calendar_season
      }
      data.frame(observationID = obs$observationID, dataset = id,
                 season = snm, season_year = syr, calendar_season = cse, stringsAsFactors = FALSE)
    } else {
      when <- if ("eventEnd" %in% names(obs)) obs$eventEnd else obs$eventStart
      nawh <- is.na(when); when[nawh] <- obs$eventStart[nawh]
      sa   <- ik_assign_season(when, when)
      data.frame(observationID = obs$observationID, dataset = id,
                 season = sa$season, season_year = sa$season_year,
                 calendar_season = sa$calendar_season, stringsAsFactors = FALSE)
    }
  })

  list(deployments = deployments,
       observations = dplyr::bind_rows(obs_per),
       monitoring_season = build_monitoring_season(deployments))
}

# Start month of each austral season — the chronological key (summer's anchor year is
# its December year, so `season_year * 12 + start_month` sorts seasons by real time).
.IK_SEASON_START_MONTH <- c(Summer = 12L, Autumn = 3L, Winter = 6L, Spring = 9L)

#' Ordered unique seasons (oldest -> newest) from a period table's rows.
#' @keywords internal
ik_season_levels <- function(period_tbl) {
  u <- unique(period_tbl[!is.na(period_tbl$calendar_season),
                         c("calendar_season", "season", "season_year")])
  u$calendar_season[order(u$season_year * 12L + .IK_SEASON_START_MONTH[u$season])]
}

#' The season immediately before `season` in the available, ordered season list.
#' @param season A `calendar_season` label.
#' @param ik_data The ik_data container.
#' @return The prior `calendar_season`, or `NA` if none precedes.
ik_prior_season <- function(season, ik_data) {
  lv <- ik_season_levels(ik_deployment_period(ik_data))
  i  <- match(season, lv)
  if (is.na(i) || i <= 1L) NA_character_ else lv[i - 1L]
}

#' The same season one year earlier (e.g. "Autumn 2024" -> "Autumn 2023"), if present.
#' @param season A `calendar_season` label.
#' @param ik_data The ik_data container.
#' @return The matching `calendar_season` a year earlier, or `NA` if absent.
ik_same_season_last_year <- function(season, ik_data) {
  dp <- ik_deployment_period(ik_data)
  row <- dp[match(season, dp$calendar_season), ]
  if (nrow(row) == 0 || is.na(row$season)) return(NA_character_)
  hit <- dp$season == row$season & dp$season_year == row$season_year - 1L
  if (any(hit, na.rm = TRUE)) dp$calendar_season[which(hit)[1]] else NA_character_
}

# Austral monitoring year: the cycle that STARTS in summer (Summer Y/Y+1, then Autumn,
# Winter, Spring Y+1). Summer keeps its season_year as the cycle key; the other three
# belong to the cycle that began the previous summer. Display order is Summer -> Spring.
.IK_CYCLE_ORDER <- c(Summer = 1L, Autumn = 2L, Winter = 3L, Spring = 4L)
.ik_cycle_year <- function(season, season_year) {
  ifelse(season == "Summer", season_year, season_year - 1L)
}

#' Grouped choices for a single "Period" `selectInput`: "All data", then one optgroup
#' per **austral year** — the summer-first cycle, span-labelled (e.g. "2024/25"), newest
#' first — holding "Whole year" + its seasons in Summer -> Spring order. Values are encoded
#' so one control spans three granularities: `"all"`, `"year:<cycle>"`, `"season:<label>"`
#' — decode with `ik_expand_period()`.
#'
#' @param ik_data The ik_data container.
#' @return A named list suitable for `selectInput(choices = )`.
ik_period_choices <- function(ik_data) {
  info <- unique(ik_deployment_period(ik_data)[
    , c("calendar_season", "season", "season_year")])
  info <- info[!is.na(info$calendar_season), ]
  info$cycle <- .ik_cycle_year(info$season, info$season_year)
  cycles <- sort(unique(info$cycle), decreasing = TRUE)
  groups <- lapply(cycles, function(cy) {
    s <- info[info$cycle == cy, ]
    s <- s[order(.IK_CYCLE_ORDER[s$season]), ]                     # Summer..Spring
    c(stats::setNames(paste0("year:", cy), "Whole year"),
      stats::setNames(paste0("season:", s$calendar_season), s$calendar_season))
  })
  names(groups) <- sprintf("%d/%02d", cycles, (cycles + 1L) %% 100L)   # span label
  c(list("All data" = "all"), groups)
}

#' The default Period selection (an `ik_period_choices()` value) for a fresh control: the
#' latest season WITH CAMERA (monitoring) data — trapping runs later into the current season
#' (e.g. Winter 2026) where there's little/no camera yet, and the camera RAI is the headline.
#' Falls back to the latest season overall, then "all". Computed at UI-build time so the control
#' renders already pointing here — there's no empty → "all data" → season double-load.
#' @param ik_data The ik_data container.
#' @return An encoded period value: `"season:<label>"`, or `"all"` if no seasons exist.
ik_default_period <- function(ik_data) {
  dp  <- ik_deployment_period(ik_data)
  cam <- ik_season_levels(dp[!is.na(dp$source_type) & dp$source_type == "camera", , drop = FALSE])
  if (length(cam)) return(paste0("season:", cam[length(cam)]))
  seasons <- ik_season_levels(dp)
  if (length(seasons)) paste0("season:", seasons[length(seasons)]) else "all"
}

#' Expand a `ik_period_choices()` value into the `calendar_season`(s) it selects.
#'
#' @param value One encoded value (`"all"` / `"year:<Y>"` / `"season:<label>"`).
#' @param ik_data The ik_data container.
#' @return Character vector of `calendar_season` labels, or `NULL` ("all" = no filter).
ik_expand_period <- function(value, ik_data) {
  if (is.null(value) || !nzchar(value) || identical(value, "all")) return(NULL)
  if (startsWith(value, "season:")) return(sub("^season:", "", value))
  if (startsWith(value, "year:")) {
    cy  <- suppressWarnings(as.integer(sub("^year:", "", value)))
    dp  <- ik_deployment_period(ik_data)
    cyc <- .ik_cycle_year(dp$season, dp$season_year)
    return(unique(dp$calendar_season[!is.na(dp$calendar_season) & cyc == cy]))
  }
  NULL
}

#' Encoded value of the period BEFORE `period`, for comparison — `compare` = "prior" (the
#' previous period of the SAME TYPE) or "last_year" (one year back). Works for a single
#' season ("season:Autumn 2026") or a whole year ("year:2025"); `NA` when none precedes or
#' the period is "all". @keywords internal
ik_prev_period <- function(period, compare, ik_data) {
  if (is.null(period) || !nzchar(period) || identical(period, "all")) return(NA_character_)
  if (startsWith(period, "season:")) {
    s   <- sub("^season:", "", period)
    out <- switch(compare,
                  prior     = ik_prior_season(s, ik_data),
                  last_year = ik_same_season_last_year(s, ik_data),
                  NA_character_)
    return(if (is.na(out)) NA_character_ else paste0("season:", out))
  }
  if (startsWith(period, "year:")) {                              # prior year (both modes)
    cy  <- suppressWarnings(as.integer(sub("^year:", "", period))) - 1L
    cyc <- .ik_cycle_year(ik_deployment_period(ik_data)$season,
                          ik_deployment_period(ik_data)$season_year)
    return(if (cy %in% cyc) paste0("year:", cy) else NA_character_)
  }
  NA_character_
}

#' A comparison-period SPEC from a selection spec's `period` + `compare`, or NULL when
#' there's no comparison (compare = none, period = all, or nothing precedes). The returned
#' spec is the selection with its period shifted to the comparison period.
#'
#' @param ik_data The ik_data container.
#' @param spec    A selection spec carrying `period` (encoded value) + `compare`.
#' @return The comparison spec, or NULL.
ik_comparison_spec <- function(ik_data, spec) {
  cmp <- spec$compare
  if (is.null(cmp) || !length(cmp) || !nzchar(cmp) || identical(cmp, "none")) return(NULL)
  prev <- ik_prev_period(spec$period, cmp, ik_data)
  if (is.na(prev)) return(NULL)
  spec$period  <- prev
  spec$season  <- ik_expand_period(prev, ik_data)
  spec$compare <- NULL
  if (!length(spec$season)) return(NULL)
  spec
}

#' Empirical monitoring-season envelopes per (season × reserve × source_type).
#'
#' Two regimes, matching how the device monitors and how its observations are seasoned:
#'   - PULSE (camera): the WHOLE deployment belongs to its majority-overlap season — no
#'     boundary clipping. A ~3-week block that grazes the next season still counts wholly
#'     where it ran, so its effort (the RAI denominator) lines up with its detections,
#'     which are likewise anchored on the deployment (see `build_period`).
#'   - CONTINUOUS (trap, …): each deployment is CLIPPED to every season it overlaps, so a
#'     long run contributes only its in-season portion to each season it touches — the
#'     correct denominator for a control source that genuinely spans seasons.
#' `start`/`end` are the empirical bounds, `n_deployments` the count in-season, `effort_hours`
#' the (whole-pulse / in-season-clipped) effort.
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
  is_pulse <- !is.na(d$source_type) & d$source_type == "camera"
  parts <- list()

  # Pulse (camera): whole deployment under its assigned (majority) season. Compute the season
  # here from the deployment span (same rule as build_period) so this works whether the caller
  # passes the period table (build time) or raw deployments (ik_select, runtime).
  if (any(is_pulse)) {
    p  <- d[is_pulse, , drop = FALSE]
    sa <- ik_assign_season(p$deploymentStart, p$deploymentEnd)
    parts[[length(parts) + 1L]] <- data.frame(
      calendar_season = sa$calendar_season, season = sa$season, season_year = sa$season_year,
      reserve = p$reserve, source_type = p$source_type,
      cstart = as.numeric(p$deploymentStart),
      cend   = pmax(as.numeric(p$deploymentEnd), as.numeric(p$deploymentStart)),
      stringsAsFactors = FALSE)
  }

  # Continuous (traps, …): clip every deployment (rows) against every season (cols).
  if (any(!is_pulse)) {
    c_ <- d[!is_pulse, , drop = FALSE]
    start_s <- as.numeric(c_$deploymentStart); end_s <- pmax(as.numeric(c_$deploymentEnd), start_s)
    grid <- ik_season_grid(min(c_$deploymentStart), max(c_$deploymentEnd), tzone)
    nd <- nrow(c_); ns <- nrow(grid)
    cs <- pmax(matrix(start_s, nd, ns), matrix(as.numeric(grid$start), nd, ns, byrow = TRUE))
    ce <- pmin(matrix(end_s,   nd, ns), matrix(as.numeric(grid$end),   nd, ns, byrow = TRUE))
    keep <- which(ce > cs)
    if (length(keep)) {
      ri <- ((keep - 1L) %%  nd) + 1L; ci <- ((keep - 1L) %/% nd) + 1L
      parts[[length(parts) + 1L]] <- data.frame(
        calendar_season = grid$calendar_season[ci], season = grid$season[ci],
        season_year = grid$season_year[ci], reserve = c_$reserve[ri],
        source_type = c_$source_type[ri], cstart = cs[keep], cend = ce[keep],
        stringsAsFactors = FALSE)
    }
  }

  if (!length(parts)) return(empty)
  do.call(rbind, parts) |>
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

#' Per-deployment season attributes (keyed by deploymentID). Scoped to the session's active
#' datasets (global toggle) unless `dataset` is given explicitly (which overrides the toggle).
#' @param ik_data The ik_data container. @param dataset Optional explicit dataset id(s).
#' @return ik_data$app$period$deployments (filtered).
ik_deployment_period <- function(ik_data, dataset = NULL) {
  dp  <- ik_data$app$period$deployments
  ids <- if (is.null(dataset)) ik_active_datasets() else dataset
  if (!is.null(ids) && "dataset" %in% names(dp)) dp <- dp[dp$dataset %in% ids, , drop = FALSE]
  dp
}

#' Per-observation season attributes (keyed by observationID; by event date). Scoped like
#' `ik_deployment_period`. @param ik_data The container. @param dataset Optional explicit id(s).
#' @return ik_data$app$period$observations (filtered).
ik_observation_period <- function(ik_data, dataset = NULL) {
  op  <- ik_data$app$period$observations
  ids <- if (is.null(dataset)) ik_active_datasets() else dataset
  if (!is.null(ids) && "dataset" %in% names(op)) op <- op[op$dataset %in% ids, , drop = FALSE]
  op
}

#' Empirical monitoring-season envelopes per (season × reserve × source_type).
#' @param ik_data The ik_data container.
#' @return ik_data$app$period$monitoring_season.
ik_monitoring_season <- function(ik_data) ik_data$app$period$monitoring_season
