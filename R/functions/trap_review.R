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
#' Defined as `(as_of − first_check) / n_checks` where `first_check` = the trap's earliest check.
#' This (a) DROPS the artificial pre-first interval the converter fabricates for each trap's first
#' record (we use the first REAL check, not its made-up start), and (b) INCLUDES the open gap up to
#' `as_of` — so a trap that stopped being checked no longer looks well-serviced. `as_of` defaults to
#' the latest check in `tr`, but a caller reviewing a PERIOD should pass the period END (clamped to
#' today) so the trailing gap is measured to the end of the window, not just to the last check that
#' happened to land in it. @keywords internal
.trap_mean_intervals <- function(tr, as_of = NULL) {
  if (is.null(as_of)) as_of <- max(tr$deploymentEnd, na.rm = TRUE)
  agg <- dplyr::summarise(dplyr::group_by(tr, .data$locationID),
    n = dplyr::n(), first = min(.data$deploymentEnd, na.rm = TRUE), .groups = "drop")
  mi <- pmax(as.numeric(difftime(as_of, agg$first, units = "days")) / agg$n, 0)
  stats::setNames(mi, as.character(agg$locationID))
}

#' Calendar [start, end) of a period (one or more `calendar_season` labels) as POSIXct. Used to
#' measure servicing staleness to the period END and to bracket "active during the period".
#' @keywords internal
.trap_period_bounds <- function(seasons, tzone = "Pacific/Auckland") {
  one <- function(lab) if (startsWith(lab, "Summer"))
      ik_season_bounds("Summer", as.integer(sub("^Summer ([0-9]{4}).*", "\\1", lab)))
    else { p <- strsplit(lab, " ", fixed = TRUE)[[1]]; ik_season_bounds(p[1], as.integer(p[2])) }
  bs <- lapply(seasons, one)
  s <- min(do.call(c, lapply(bs, `[`, 1))); e <- max(do.call(c, lapply(bs, `[`, 2)))
  c(as.POSIXct(paste(s, "00:00:00"), tz = tzone), as.POSIXct(paste(e, "00:00:00"), tz = tzone))
}

#' Canonical trap servicing-health thresholds from the project's OWN data: the `good`/`watch`
#' percentiles of the per-trap mean check interval (`.trap_mean_intervals`), over ALL trap
#' checks. Computed at build and frozen on `meta$trapping$health` so the buckets recalibrate only
#' on re-import, not as the viewed period changes. @return list(good_max, watch_max, percentiles,
#' n_traps, median) or NULL when there are no trap checks.
ik_trap_health_thresholds <- function(ik_data, percentiles = c(good = 0.5, watch = 0.9), dataset = NULL) {
  dp <- ik_deployment_period(ik_data, dataset = dataset)        # one dataset, or all when NULL
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
ik_trap_health <- function(mean_interval_days, ik_data, dataset = NULL) {
  h   <- ik_data$meta$trapping$health %||% list()
  hbd <- ik_data$meta$trapping$health_by_dataset
  gm0 <- h$good_max  %||% TRAP_GOOD_INTERVAL_DAYS
  wm0 <- h$watch_max %||% TRAP_WATCH_INTERVAL_DAYS
  # Per-dataset cutoffs when a dataset is given (each trap judged by its OWN check-rate spread),
  # falling back to the global figure. `dataset` may be a vector aligned to mean_interval_days.
  if (is.null(dataset) || is.null(hbd)) {
    gm <- gm0; wm <- wm0
  } else {
    pick <- function(ds, key, dflt) if (is.na(ds)) dflt else hbd[[ds]][[key]] %||% dflt
    gm <- vapply(dataset, pick, numeric(1), key = "good_max",  dflt = gm0)
    wm <- vapply(dataset, pick, numeric(1), key = "watch_max", dflt = wm0)
  }
  if (!is.null(h$floor))   gm <- pmax(gm, h$floor)             # never call a well-checked trap "watch"
  if (!is.null(h$ceiling)) wm <- pmin(wm, h$ceiling)           # never call a long-gap trap "good/watch"
  gm <- pmin(gm, wm)                                           # guard against floor ≥ ceiling misconfig
  factor(ifelse(mean_interval_days <= gm, "good",
         ifelse(mean_interval_days <= wm, "watch", "neglected")),
         levels = c("good", "watch", "neglected"))
}

#' Effective good/watch cutoffs for the DISPLAY legend: the per-dataset maxima AVERAGED over the
#' given datasets (default = the session's active datasets), keeping the project's shared
#' percentiles + floor/ceiling. A multi-dataset view shows a blended cutoff while each trap stays
#' coloured by its OWN dataset (ik_trap_health). Falls back to the global thresholds. @keywords internal
ik_trap_health_cutoffs <- function(ik_data, datasets = NULL) {
  base <- ik_data$meta$trapping$health %||% list()
  hbd  <- ik_data$meta$trapping$health_by_dataset
  ids  <- datasets %||% ik_active_datasets() %||% names(hbd)
  hs   <- Filter(Negate(is.null), hbd[intersect(ids, names(hbd))])
  if (!length(hs)) return(base)
  gm <- mean(vapply(hs, function(h) h$good_max  %||% NA_real_, numeric(1)), na.rm = TRUE)
  wm <- mean(vapply(hs, function(h) h$watch_max %||% NA_real_, numeric(1)), na.rm = TRUE)
  utils::modifyList(base, list(good_max = gm, watch_max = wm))
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

#' Trap locations with NO check in the period but that EXISTED by the assessment date `t_end`
#' (first-ever check ≤ `t_end`; not-yet-installed traps excluded). One row each with `last_before` =
#' its most recent check up to `t_end` — NO peek past `t_end`, so a past period isn't reclassified by
#' later checks. The neglect/dormant/historic tier is applied uniformly by gap in `ik_trap_review`;
#' this just identifies the rows. @keywords internal
.trap_skipped <- function(dptrap, checked_ids, t_end) {
  agg <- dplyr::summarise(dplyr::group_by(dptrap, .data$locationID),
    dataset     = dplyr::first(.data$dataset),
    first_ever  = min(.data$deploymentEnd, na.rm = TRUE),
    last_before = suppressWarnings(max(.data$deploymentEnd[.data$deploymentEnd <= t_end], na.rm = TRUE)),
    .groups = "drop")
  agg[!(agg$locationID %in% checked_ids) & agg$first_ever <= t_end & is.finite(agg$last_before), , drop = FALSE]
}

#' Per-trap servicing review for a period — the central trap register.
#'
#' EVERYTHING is assessed PERIOD-RELATIVE, standing at the period END `t_end` (clamped to today for
#' an in-progress season; "all data" uses today), and WITHOUT peeking at checks after `t_end` — so a
#' past period is never reclassified by later data. The whole dataset is used only to calibrate the
#' good/watch cadence yardstick (`ik_trap_health`). Status, from `gap = t_end − last check ≤ t_end`:
#'   - checked in the period, ≥ `min_checks_for_cadence` checks → good / watch / neglected (cadence);
#'   - checked but fewer → "insufficient_data" (sparse — can't judge a cadence);
#'   - not checked in the period (but existed by `t_end`) → "neglected";
#'   - then a UNIFORM override: gap ≥ `dormant_after_days` → "dormant"; ≥ `historic_after_days` →
#'     "historic". So a decommissioned trap walks neglected → dormant → historic over later periods,
#'     while a period when it was active still reads it as active.
#' @param ik_data The ik_data container. @param seasons calendar_season(s), or NULL = all data.
#' @param obs Optional pre-fetched `ik_observations(ik_data, with_location = FALSE)` — pass it when
#'   calling per-period in a loop (e.g. the timeline) so the observation table is built once, not
#'   once per period. NULL = fetch here.
#' @return one row per trap: location · dataset · n_checks · trap_days · first_check · last_check ·
#'   mean_interval_days · captures · name · line · reserve · latitude · longitude · status. NULL when
#'   there are no trap checks at all.
ik_trap_review <- function(ik_data, seasons = NULL, obs = NULL) {
  dp <- ik_deployment_period(ik_data)
  dptrap <- dp[!is.na(dp$source_type) & dp$source_type == "trap", , drop = FALSE]
  tr <- .trap_in_period(ik_data, dptrap, seasons)
  if (!nrow(tr)) return(NULL)
  locs <- ik_data$app$geography$locations
  t_end <- if (is.null(seasons)) min(max(dptrap$deploymentEnd, na.rm = TRUE), Sys.time())
           else min(.trap_period_bounds(seasons)[2], Sys.time())
  min_checks <- ik_data$meta$trapping$min_checks_for_cadence %||% 2
  dormant_d  <- ik_data$meta$trapping$dormant_after_days  %||% 182
  historic_d <- ik_data$meta$trapping$historic_after_days %||% 365

  # captures = trap ANIMAL observations at each trap in the period
  if (is.null(obs)) obs <- ik_observations(ik_data, with_location = FALSE)
  obs <- obs[obs$deploymentID %in% tr$deploymentID &
               !is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  cap <- table(tr$locationID[match(obs$deploymentID, tr$deploymentID)])

  per <- dplyr::summarise(
    dplyr::group_by(tr, .data$locationID),
    dataset     = dplyr::first(.data$dataset),                 # locationID is namespaced per dataset
    n_checks    = dplyr::n(),                                  # checks in the period
    trap_days   = sum(.data$effort_hours, na.rm = TRUE) / 24,  # effort (sum of intervals)
    first_check = suppressWarnings(min(.data$deploymentEnd, na.rm = TRUE)),
    last_check  = suppressWarnings(max(.data$deploymentEnd, na.rm = TRUE)),
    .groups = "drop")
  per$mean_interval_days <- unname(.trap_mean_intervals(tr, t_end)[as.character(per$locationID)])
  per$captures <- as.integer(cap[as.character(per$locationID)])
  per$captures[is.na(per$captures)] <- 0L

  gi <- match(per$locationID, locs$location_id)
  per$name      <- locs$name[gi]
  per$line      <- locs$line[gi]
  per$reserve   <- locs$reserve[gi]
  per$latitude  <- locs$latitude[gi]
  per$longitude <- locs$longitude[gi]
  names(per)[names(per) == "locationID"] <- "location"
  # cadence health, or "insufficient_data" when too few checks in the period to judge a cadence
  per$status <- ifelse(per$n_checks >= min_checks,
                       as.character(ik_trap_health(per$mean_interval_days, ik_data, dataset = per$dataset)),
                       "insufficient_data")

  # existed-but-unchecked-this-period traps → start as neglected (tiered to dormant/historic below)
  if (!is.null(seasons)) {
    sk <- .trap_skipped(dptrap, unique(tr$locationID), t_end)
    if (nrow(sk)) {
      gj <- match(sk$locationID, locs$location_id)
      add <- data.frame(
        location = sk$locationID, dataset = sk$dataset, n_checks = 0L, trap_days = 0,
        first_check = .POSIXct(rep(NA_real_, nrow(sk)), tz = "Pacific/Auckland"),
        last_check  = sk$last_before,
        mean_interval_days = as.numeric(difftime(t_end, sk$last_before, units = "days")),
        captures = 0L, name = locs$name[gj], line = locs$line[gj], reserve = locs$reserve[gj],
        latitude = locs$latitude[gj], longitude = locs$longitude[gj],
        status = "neglected", stringsAsFactors = FALSE)
      per <- rbind(per[, names(add)], add)
    }
  }

  # UNIFORM dormancy/historic override by gap since the last check ≤ t_end (period-relative, no peek):
  # a long-untouched trap is dormant/historic whatever it was otherwise, in this period's frame.
  gap <- as.numeric(difftime(t_end, per$last_check, units = "days"))
  per$status <- ifelse(!is.na(gap) & gap >= historic_d, "historic",
                ifelse(!is.na(gap) & gap >= dormant_d, "dormant", per$status))
  per
}

#' The DORMANT/historic subset of `ik_trap_review` — for the map's faint "not in service" layer.
#' NULL for "all data" or when none. @keywords internal
ik_trap_inactive <- function(ik_data, seasons) {
  if (is.null(seasons)) return(NULL)
  per <- ik_trap_review(ik_data, seasons)
  if (is.null(per)) return(NULL)
  d <- per[per$status %in% c("dormant", "historic"), , drop = FALSE]
  if (nrow(d)) d else NULL
}

#' Roll the per-trap review up to per LINE (the natural review unit). @keywords internal
ik_trap_review_lines <- function(per, ik_data) {
  if (is.null(per) || !nrow(per)) return(NULL)
  out <- dplyr::summarise(
    dplyr::group_by(per, .data$reserve, .data$line),
    dataset      = dplyr::first(.data$dataset),                # a line sits within one dataset
    n_traps      = dplyr::n(),
    checks       = sum(.data$n_checks),
    trap_days    = sum(.data$trap_days),
    captures     = sum(.data$captures),
    # cadence = mean over the line's ACTIVE traps (dormant/historic are out of service, not the line's
    # current fault); neglected/skipped traps' large gaps stay in, so a line with unserviced traps reads worse.
    mean_interval_days = mean(.data$mean_interval_days[!.data$status %in% c("dormant", "historic")], na.rm = TRUE),
    .groups = "drop")
  out$mean_interval_days[is.nan(out$mean_interval_days)] <- NA_real_
  out$status <- as.character(ik_trap_health(out$mean_interval_days, ik_data, dataset = out$dataset))
  out$status[is.na(out$mean_interval_days)] <- "dormant"      # a line with only dormant/historic traps
  out[order(out$reserve, suppressWarnings(as.numeric(out$line)), out$line), , drop = FALSE]
}

#' Trap servicing TIMELINE — the review's headline metrics per period across all of time, for the
#' history chart. `by = "season"` (each calendar season) or `"year"` (the austral year that starts in
#' summer). Each period is a full `ik_trap_review()` (so per-trap dedup + the dormancy/sparse logic
#' all apply), optionally scoped to `reserve`. NOT period-scoped otherwise — it spans every season.
#' @return tidy data.frame: period · order · facet · series · value, where facet is "Servicing (% of
#'   judged traps)" (series Good/Watch/Neglected), "Median check interval (days)" or "Captures". The
#'   in-progress trailing period is omitted (its traps aren't re-checked yet, so it reads as a false
#'   servicing collapse); its label is on `attr(out, "incomplete_period")`. NULL when there are no
#'   trap checks. @keywords internal
ik_trap_review_series <- function(ik_data, by = "season", reserve = NULL) {
  dp <- ik_deployment_period(ik_data); dp <- dp[!is.na(dp$source_type) & dp$source_type == "trap", , drop = FALSE]
  if (!nrow(dp)) return(NULL)
  seasons <- ik_season_levels(dp)
  if (!length(seasons)) return(NULL)
  norm <- ik_data$meta$trapping$rate$norm_trap_days %||% 100        # trap-nights to normalise catch rate to
  if (identical(by, "year")) {                                  # group seasons into austral years
    info <- unique(dp[, c("calendar_season", "season", "season_year")])
    info <- info[!is.na(info$calendar_season), , drop = FALSE]
    info$cycle <- .ik_cycle_year(info$season, info$season_year)
    cyc <- sort(unique(info$cycle))
    periods <- lapply(cyc, function(c0) info$calendar_season[info$cycle == c0])
    plabs <- sprintf("%d/%02d", cyc, (cyc + 1L) %% 100L); porder <- seq_along(cyc)
  } else {
    periods <- as.list(seasons); plabs <- seasons; porder <- seq_along(seasons)
  }
  obs_all <- ik_observations(ik_data, with_location = FALSE)     # built ONCE; reused for every period
  now <- Sys.time(); incomplete <- NULL                         # label of the dropped in-progress period
  rows <- lapply(seq_along(periods), function(i) {
    # Omit the still-open trailing period: with few/no re-checks yet it reads as a servicing collapse
    # (Neglected spikes), inverting the trend. The live table above shows the current period in detail.
    if (.trap_period_bounds(periods[[i]])[2] > now) { incomplete <<- plabs[i]; return(NULL) }
    per <- ik_trap_review(ik_data, periods[[i]], obs = obs_all)
    if (is.null(per)) return(NULL)
    if (!is.null(reserve)) per <- per[per$reserve %in% reserve, , drop = FALSE]
    jgd <- per$status %in% c("good", "watch", "neglected")      # judged-for-cadence traps = the denominator
    ng  <- sum(jgd); if (!ng) return(NULL)                      # (insufficient_data/dormant/historic excluded)
    st  <- table(factor(per$status[jgd], c("good", "watch", "neglected")))
    cad <- per$mean_interval_days[jgd & per$n_checks > 0]       # real intervals only — skipped traps' "gap" isn't one
    med <- if (length(cad)) stats::median(cad, na.rm = TRUE) else NA_real_
    base <- data.frame(period = plabs[i], order = porder[i], stringsAsFactors = FALSE)
    td    <- sum(per$trap_days, na.rm = TRUE)                   # period effort (trap-nights)
    crate <- if (td > 0) norm * sum(per$captures, na.rm = TRUE) / td else NA_real_
    rbind(
      cbind(base, facet = "Servicing (% of judged traps)", series = c("Good", "Watch", "Neglected"),
            value = 100 * c(st[["good"]], st[["watch"]], st[["neglected"]]) / ng),   # three lines sum to 100
      cbind(base, facet = "Median check interval (days)", series = "Interval", value = med),
      # catch RATE, not raw count: raw captures co-move with effort and season, which invites a false
      # "checking more catches more" read; the rate (per `norm` trap-nights) is comparable across periods.
      cbind(base, facet = sprintf("Catch rate (per %g trap-nights)", norm), series = "Catch rate", value = crate))
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) return(NULL)
  attr(out, "incomplete_period") <- incomplete
  out
}

#' Outcome label for a trap check: the caught species (vernacular label, else its scientificName);
#' for a non-capture, the recorded `status:` tag (Still set / Sprung / Bait gone), title-cased; and
#' failing that, the trap-aware observationType ("blank" → "Empty"). Vectorised. @keywords internal
.ik_trap_outcome <- function(species_label, scientific_name, status, observation_type) {
  st  <- as.character(status)
  has <- !is.na(st) & nzchar(st) & st != "NA" & tolower(st) != "caught"
  out <- ifelse(has, tools::toTitleCase(st), ik_obs_type_label(observation_type, TRUE))
  out <- ifelse(!is.na(species_label) & nzchar(species_label), species_label,
         ifelse(!is.na(scientific_name), scientific_name, out))
  out
}

#' Every individual check of one trap in a period (most recent first) — for the trap drill.
#'
#' @param ik_data  The ik_data container.
#' @param location The trap's locationID.
#' @param seasons  Optional vector of `calendar_season` (the period). NULL = all.
#' @return data.frame: check_date · interval_days (days since the prior check; NA for the trap's
#'   FIRST record in our data, whose interval is fabricated) · is_first · outcome (caught
#'   species, else the observationType) · bait · rebaited (raw trap.NZ re-bait flag) ·
#'   volunteer. NULL when none.
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
  status <- vapply(obs$observationTags[oi], .ovw_tag, character(1), key = "status")
  is_first <- !is.na(first_ever) & tr$deploymentEnd == first_ever        # fabricated prior interval
  data.frame(
    observationID = obs$observationID[oi],                               # for the record drill
    check_date    = tr$deploymentEnd,
    interval_days = ifelse(is_first, NA_real_, round(tr$effort_hours / 24)),
    is_first      = is_first,
    outcome       = .ik_trap_outcome(lab, obs$scientificName[oi], status, obs$observationType[oi]),
    bait          = vapply(obs$observationTags[oi], .ovw_tag, character(1), key = "bait"),
    rebaited      = vapply(obs$observationTags[oi], .ovw_tag, character(1), key = "bait_change"),  # raw flag
    volunteer     = vapply(obs$observationTags[oi], .ovw_tag, character(1), key = "volunteer"),
    stringsAsFactors = FALSE)
}
