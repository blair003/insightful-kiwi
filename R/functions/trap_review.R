# trap_review.R — the trapping effort / check-frequency review. Per trap (= one locationID),
# over a period: how often was it checked, at what interval, for how much effort, with what
# result. The unit of replication is the TRAP and every row carries lat/long, so this table
# is map-ready (each trap a point with its check-frequency metrics) as well as feeding the
# Data → Quality "Trapping" review. A check = one trap observation/deployment (the converter
# makes one deployment per check interval), so the deployment duration IS the check interval.

# Servicing-health buckets (good / watch / neglected) by mean days between checks. The cutoffs
# are CANONICAL and statistical: percentiles of THIS project's own per-trap interval
# distribution, computed once at import (`ik_trap_health_thresholds`, stored on
# `meta$trapping$health`) and applied everywhere via `ik_trap_health()`. The constants below are
# only a fallback when those aren't present (e.g. a project with no trap data yet).
TRAP_GOOD_INTERVAL_DAYS    <- 28   # ≤ this → checked frequently (good)
TRAP_WATCH_INTERVAL_DAYS   <- 56   # ≤ this → watch; above → neglected

#' Per-trap mean check interval (days), named by locationID, for a set of trap deployments.
#' Defined as `(as_of − first_check) / n_checks` where `as_of` = the latest check anywhere in
#' `tr` and `first_check` = the trap's earliest check. This (a) DROPS the artificial pre-first
#' interval the converter fabricates for each trap's first record (we use the first REAL check,
#' not its made-up start), and (b) INCLUDES the open gap since the last check — so a trap that
#' simply stopped being checked no longer looks well-serviced. `as_of` comes from `tr`, so it's
#' relative to whatever period `tr` represents. @keywords internal
.trap_mean_intervals <- function(tr) {
  as_of <- max(tr$deploymentEnd, na.rm = TRUE)
  agg <- dplyr::summarise(dplyr::group_by(tr, .data$locationID),
    n = dplyr::n(), first = min(.data$deploymentEnd, na.rm = TRUE), .groups = "drop")
  mi <- as.numeric(difftime(as_of, agg$first, units = "days")) / agg$n
  stats::setNames(mi, as.character(agg$locationID))
}

#' Canonical trap servicing-health thresholds from the project's OWN data: the `good`/`watch`
#' percentiles of the per-trap mean check interval (`.trap_mean_intervals`), over ALL trap
#' checks. Computed at build and frozen on `meta$trapping$health` so the buckets recalibrate only
#' on re-import, not as the viewed period changes. @return list(good_max, watch_max, percentiles,
#' n_traps, median) or NULL when there are no trap checks.
ik_trap_health_thresholds <- function(ik_data, percentiles = c(good = 0.5, watch = 0.9)) {
  dp <- ik_deployment_period(ik_data)
  tr <- dp[!is.na(dp$source_type) & dp$source_type == "trap", , drop = FALSE]
  if (!nrow(tr)) return(NULL)
  mi <- .trap_mean_intervals(tr)
  mi <- mi[is.finite(mi) & mi > 0]
  if (!length(mi)) return(NULL)
  q <- stats::quantile(mi, probs = percentiles, names = FALSE, na.rm = TRUE)
  list(good_max = unname(q[1]), watch_max = unname(q[2]), percentiles = percentiles,
       n_traps = length(mi), median = stats::median(mi))
}

#' Classify mean check interval(s) into good / watch / neglected using the canonical project
#' thresholds (`meta$trapping$health`), falling back to the constants. The percentile cutoffs are
#' clamped by optional absolute GUARDRAILS (`floor`/`ceiling` in config): `floor` raises the good
#' cutoff (a trap checked at least this often is always good, even if the project median is
#' tighter), `ceiling` lowers the watch cutoff (a gap longer than this is always neglected, even
#' if the project is sluggish). Keeps the relative buckets meaningful in an unusually good or bad
#' project. @return ordered factor.
ik_trap_health <- function(mean_interval_days, ik_data) {
  h  <- ik_data$meta$trapping$health
  gm <- h$good_max  %||% TRAP_GOOD_INTERVAL_DAYS
  wm <- h$watch_max %||% TRAP_WATCH_INTERVAL_DAYS
  if (!is.null(h$floor))   gm <- max(gm, h$floor)              # never call a well-checked trap "watch"
  if (!is.null(h$ceiling)) wm <- min(wm, h$ceiling)            # never call a long-gap trap "good/watch"
  gm <- min(gm, wm)                                            # guard against floor ≥ ceiling misconfig
  factor(ifelse(mean_interval_days <= gm, "good",
         ifelse(mean_interval_days <= wm, "watch", "neglected")),
         levels = c("good", "watch", "neglected"))
}

#' The latest trap season with a near-full data envelope (≥ `min_days` of ~90), so a current
#' season with only a couple of weeks of data isn't the default. Falls back to the latest
#' season, then NULL. @keywords internal
.default_trap_season <- function(ik_data, min_days = 75) {
  dp <- ik_deployment_period(ik_data); dp <- dp[dp$source_type == "trap", , drop = FALSE]
  seasons <- ik_season_levels(dp)
  ms <- ik_data$app$period$monitoring_season; ms <- ms[ms$source_type == "trap", , drop = FALSE]
  span <- tapply(seq_len(nrow(ms)), ms$calendar_season, function(ix)
    as.numeric(difftime(max(ms$end[ix]), min(ms$start[ix]), units = "days")))
  full <- intersect(seasons, names(span)[span >= min_days])
  if (length(full)) full[length(full)] else if (length(seasons)) seasons[length(seasons)] else NULL
}

#' Keep the trap deployments (checks) that fall in `seasons`, attributing each check per the
#' project setting `meta$trapping$season_by`: "check_date" (default — the season the CHECK
#' DATE sits in, intuitive for check frequency, and consistent with the capture-rate metric
#' which already groups captures by check date) or "interval" (the assigned span season).
#' @keywords internal
.trap_in_period <- function(ik_data, tr, seasons) {
  if (is.null(seasons) || !nrow(tr)) return(tr)
  by  <- ik_data$meta$trapping$season_by %||% "check_date"
  key <- if (identical(by, "interval")) tr$calendar_season
         else ik_assign_season(tr$deploymentEnd, tr$deploymentEnd)$calendar_season
  tr[key %in% seasons, , drop = FALSE]
}

#' Per-trap check-frequency / effort for a period.
#'
#' @param ik_data The ik_data container.
#' @param seasons Optional vector of `calendar_season` to scope to (a period). NULL = all.
#' @return A data.frame, one row per trap checked in the period: location · name · line ·
#'   reserve · latitude · longitude · n_checks · trap_days · mean_interval_days ·
#'   first_check · last_check · captures · status. NULL when there are no trap checks.
ik_trap_review <- function(ik_data, seasons = NULL) {
  dp <- ik_deployment_period(ik_data)
  tr <- .trap_in_period(ik_data, dp[!is.na(dp$source_type) & dp$source_type == "trap", , drop = FALSE], seasons)
  if (!nrow(tr)) return(NULL)
  locs <- ik_data$app$geography$locations

  # captures = trap ANIMAL observations at each trap in the period
  obs <- ik_observations(ik_data, with_location = FALSE)
  obs <- obs[obs$deploymentID %in% tr$deploymentID &
               !is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  cap <- table(tr$locationID[match(obs$deploymentID, tr$deploymentID)])

  per <- dplyr::summarise(
    dplyr::group_by(tr, .data$locationID),
    n_checks    = dplyr::n(),                                  # checks in the period
    trap_days   = sum(.data$effort_hours, na.rm = TRUE) / 24,  # effort (sum of intervals)
    first_check = suppressWarnings(min(.data$deploymentEnd, na.rm = TRUE)),
    last_check  = suppressWarnings(max(.data$deploymentEnd, na.rm = TRUE)),
    .groups = "drop")
  # mean interval = (latest check anywhere − this trap's first check) / n_checks — drops the
  # fabricated pre-first interval AND counts the open gap since the last check (so a trap that
  # stopped being checked reads as neglected, not deceptively well-serviced). See helper.
  per$mean_interval_days <- unname(.trap_mean_intervals(tr)[as.character(per$locationID)])
  per$captures <- as.integer(cap[as.character(per$locationID)])
  per$captures[is.na(per$captures)] <- 0L

  gi <- match(per$locationID, locs$location_id)
  per$name      <- locs$name[gi]
  per$line      <- locs$line[gi]
  per$reserve   <- locs$reserve[gi]
  per$latitude  <- locs$latitude[gi]
  per$longitude <- locs$longitude[gi]
  names(per)[names(per) == "locationID"] <- "location"

  per$status <- as.character(ik_trap_health(per$mean_interval_days, ik_data))
  per
}

#' Roll the per-trap review up to per LINE (the natural review unit). @keywords internal
ik_trap_review_lines <- function(per, ik_data) {
  if (is.null(per) || !nrow(per)) return(NULL)
  out <- dplyr::summarise(
    dplyr::group_by(per, .data$reserve, .data$line),
    n_traps      = dplyr::n(),
    checks       = sum(.data$n_checks),
    trap_days    = sum(.data$trap_days),
    captures     = sum(.data$captures),
    mean_interval_days = mean(.data$mean_interval_days, na.rm = TRUE),  # mean of the line's trap means
    .groups = "drop")
  out$status <- as.character(ik_trap_health(out$mean_interval_days, ik_data))
  out[order(out$reserve, suppressWarnings(as.numeric(out$line)), out$line), , drop = FALSE]
}

#' Every individual check of one trap in a period (most recent first) — for the trap drill.
#'
#' @param ik_data  The ik_data container.
#' @param location The trap's locationID.
#' @param seasons  Optional vector of `calendar_season` (the period). NULL = all.
#' @return data.frame: check_date · interval_days (days since the prior check; NA for the trap's
#'   FIRST record in our data, whose interval is fabricated) · is_first · outcome (caught
#'   species, else the observationType) · bait · volunteer. NULL when none.
ik_trap_checks <- function(ik_data, location, seasons = NULL) {
  dp <- ik_deployment_period(ik_data)
  here <- dp[!is.na(dp$source_type) & dp$source_type == "trap" & dp$locationID == location, , drop = FALSE]
  first_ever <- if (nrow(here)) min(here$deploymentEnd, na.rm = TRUE) else NA   # first in OUR data
  tr <- .trap_in_period(ik_data, here, seasons)
  if (!nrow(tr)) return(NULL)
  tr <- tr[order(tr$deploymentEnd, decreasing = TRUE), , drop = FALSE]   # most recent first
  obs <- ik_observations(ik_data, with_location = FALSE)
  oi  <- match(tr$deploymentID, obs$deploymentID)                        # one obs per check
  lab <- ik_species_label(obs$scientificName[oi], ik_data, "vernacular")
  is_first <- !is.na(first_ever) & tr$deploymentEnd == first_ever        # fabricated prior interval
  data.frame(
    observationID = obs$observationID[oi],                               # for the record drill
    check_date    = tr$deploymentEnd,
    interval_days = ifelse(is_first, NA_real_, round(tr$effort_hours / 24)),
    is_first      = is_first,
    outcome       = ifelse(!is.na(lab) & nzchar(lab), lab,
                     ifelse(!is.na(obs$scientificName[oi]), obs$scientificName[oi],
                            tools::toTitleCase(as.character(obs$observationType[oi])))),
    bait          = vapply(obs$observationTags[oi], .ovw_tag, character(1), key = "bait"),
    volunteer     = vapply(obs$observationTags[oi], .ovw_tag, character(1), key = "volunteer"),
    stringsAsFactors = FALSE)
}
