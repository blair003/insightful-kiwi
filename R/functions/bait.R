# bait.R — bait effectiveness: trap capture rate (captures / trap-days × norm_trap_days, default 100) BY bait recipe. A
# catch is found at a check, but the bait that caught it is the bait that was in the trap during
# the interval = the PRIOR check's `baitstatus` (the check's own baitstatus is the rebait set
# that day). So we lag the bait by one check per trap. baitstatus is a messy comma list —
# normalised at ANALYSIS time only (the raw conversion is left untouched).
#
# The unit is the bait RECIPE the trap actually had, i.e. the whole SET — "Ping Pong + Salted
# Rabbit" is its own entry, NOT credited to Ping Pong and Salted Rabbit separately (that would
# make a single ingredient look like the catcher when the pair did the work).

#' Normalise a raw `baitstatus` comma-list to a clean set of bait names. Splits on comma,
#' trims, drops empties / "No", drops " & " COMBO labels (e.g. "Rabbit & Ping Pong" — redundant
#' with the individual items, which the data also lists), and de-duplicates. @keywords internal
.normalize_bait <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) return(character(0))
  items <- trimws(strsplit(x, ",")[[1]])
  items <- items[nzchar(items) & !(items %in% c("No"))]
  items <- items[!grepl(" & ", items, fixed = TRUE)]   # drop combo labels
  unique(items)
}

#' The bait RECIPE label for an interval = the normalised set, sorted, joined with " + " (so
#' order in the raw list doesn't matter). "" when no real bait. @keywords internal
.bait_combo <- function(x) {
  b <- .normalize_bait(x)
  if (!length(b)) return("")
  paste(sort(b), collapse = " + ")
}

#' Per-interval table for the period: each trap check with its PRIOR bait, effort, capture
#' flag, and the check's observation id/species/trap name. The shared substrate for both the
#' rate and the captures drill. @keywords internal
.bait_intervals <- function(ik_data, seasons = NULL, species = NULL, reserve = NULL) {
  dp <- ik_deployment_period(ik_data)
  tr <- dp[!is.na(dp$source_type) & dp$source_type == "trap", , drop = FALSE]
  if (!nrow(tr)) return(NULL)
  obs <- ik_observations(ik_data, with_location = FALSE)
  oi  <- match(tr$deploymentID, obs$deploymentID)               # one obs per check
  tr$observationID  <- obs$observationID[oi]
  tr$scientificName <- obs$scientificName[oi]
  tr$baitstatus     <- vapply(obs$observationTags[oi], .ovw_tag, character(1), key = "bait")
  cap <- !is.na(obs$observationType[oi]) & obs$observationType[oi] == "animal"
  if (!is.null(species)) cap <- cap & obs$scientificName[oi] %in% species
  tr$is_capture <- cap
  tr <- tr |>                                                    # prior check's bait = bait during interval
    dplyr::arrange(.data$locationID, .data$deploymentEnd) |>
    dplyr::group_by(.data$locationID) |>
    dplyr::mutate(prior_bait = dplyr::lag(.data$baitstatus)) |>
    dplyr::ungroup()
  if (!is.null(seasons)) {                                       # by check-date season
    cs <- ik_assign_season(tr$deploymentEnd, tr$deploymentEnd)$calendar_season
    tr <- tr[cs %in% seasons, , drop = FALSE]
  }
  tr <- tr[!is.na(tr$prior_bait), , drop = FALSE]                # drop first-ever check (no prior bait)
  if (!nrow(tr)) return(NULL)
  tr$combo <- vapply(tr$prior_bait, .bait_combo, character(1))   # the recipe (full set) per interval
  tr <- tr[nzchar(tr$combo), , drop = FALSE]                     # drop "no bait" intervals
  if (!nrow(tr)) return(NULL)
  locs <- ik_data$app$geography$locations
  tr$name <- locs$name[match(tr$locationID, locs$location_id)]
  if (!is.null(reserve) && length(reserve)) {            # scope to a reserve (a trap belongs to one,
    tr_res <- locs$reserve[match(tr$locationID, locs$location_id)]   # so whole histories are kept/dropped)
    tr <- tr[!is.na(tr_res) & tr_res %in% reserve, , drop = FALSE]
    if (!nrow(tr)) return(NULL)
  }
  # each interval's trap servicing-health FOR THIS PERIOD: classify the trap's mean check
  # interval over the period (the same metric as the Trapping review) against the canonical cutoffs.
  trap_mi <- .trap_mean_intervals(tr)
  tr$health <- as.character(ik_trap_health(trap_mi[as.character(tr$locationID)], ik_data))
  tr
}

#' Capture rate per bait for a period.
#'
#' @param ik_data       The ik_data container.
#' @param seasons       Optional `calendar_season` vector (a period); NULL = all. Filtered by
#'   the CHECK DATE (when the catch was found), consistent with the capture-rate metric.
#' @param species       Optional scientificName(s) to count as captures; NULL = any animal.
#' @param norm          Trap-nights the rate is normalised to (captures / trap-days × `norm`).
#'   Pass the project's `norm_trap_days` so the bait rate is on the same scale as every other
#'   capture rate in the app. Default 100.
#' @param min_trap_days Drop baits with less than this much effort (too noisy). Default `norm`
#'   (one normalisation unit of effort — the same floor the other rate views use).
#' @param min_captures  Drop baits with fewer than this many captures (one lucky catch on a
#'   rare species otherwise tops the rate chart — a 1-catch / ~100-trap-day bait reads as
#'   rate ≈ 1.0). Default 3.
#' @param group "recipe" (the whole bait SET is the unit — clean but fragments) or "ingredient"
#'   (credit EVERY ingredient for the catches in intervals where it appeared). Ingredient mode
#'   is associational/confounded — if two ingredients ride together their numbers blur — so it
#'   answers "which ingredients show up in baits that catch", not "which ingredient catches".
#' @param health Optional vector of servicing buckets ("good"/"watch"/"neglected") to INCLUDE;
#'   NULL = all. Lets you ask "does this bait do better in well-checked traps?" — the check
#'   frequency confounds bait rate (neglected traps drag their bait down).
#' @param by_health When TRUE, return per (bait, health) rows for the qualifying baits (a
#'   FACET, for the hypothesis test) instead of one row per bait.
#' @return data.frame: bait · n_intervals · trap_days · captures · rate, ordered by rate desc.
#'   With `by_health`: bait · health · trap_days · captures · rate · overall_rate. NULL = no data.
ik_bait_effectiveness <- function(ik_data, seasons = NULL, species = NULL,
                                  norm = 100, min_trap_days = norm, min_captures = 3,
                                  group = c("recipe", "ingredient"),
                                  health = NULL, by_health = FALSE, intervals = NULL, reserve = NULL) {
  group <- match.arg(group)
  # `.bait_intervals` scans ALL trap checks (~7s) — the chart and the captures drill share the
  # same (seasons × species × reserve) table, so the caller can pass it in once (a session reactive)
  # to avoid recomputing it on every bar click. NULL → compute here.
  tr <- intervals %||% .bait_intervals(ik_data, seasons, species, reserve)
  if (is.null(tr)) return(NULL)
  if (!is.null(health)) tr <- tr[tr$health %in% health, , drop = FALSE]
  if (!nrow(tr)) return(NULL)
  if (group == "recipe") {
    ex <- data.frame(bait = tr$combo, trap_days = tr$effort_hours / 24,  # one recipe per interval
                     capture = as.integer(tr$is_capture), health = tr$health, stringsAsFactors = FALSE)
  } else {
    ing <- lapply(tr$prior_bait, .normalize_bait)                        # explode to ingredients
    n   <- lengths(ing)
    ex  <- data.frame(bait = unlist(ing), trap_days = rep(tr$effort_hours / 24, n),
                      capture = rep(as.integer(tr$is_capture), n),
                      health = rep(tr$health, n), stringsAsFactors = FALSE)
  }
  if (!nrow(ex)) return(NULL)
  agg <- dplyr::summarise(
    dplyr::group_by(ex, .data$bait),
    n_intervals = dplyr::n(), trap_days = sum(.data$trap_days),
    captures = sum(.data$capture), .groups = "drop")
  agg$rate <- agg$captures / agg$trap_days * norm
  agg <- agg[agg$trap_days >= min_trap_days & agg$captures >= min_captures, , drop = FALSE]
  agg <- agg[order(-agg$rate), , drop = FALSE]
  if (!by_health || !nrow(agg)) return(agg)

  # facet: per (bait, health) for the qualifying baits — keep cells with a half-floor of effort
  # (so a real 0-capture cell still shows as rate 0, the informative case), ordered by overall.
  ex2 <- ex[ex$bait %in% agg$bait, , drop = FALSE]
  h <- dplyr::summarise(
    dplyr::group_by(ex2, .data$bait, .data$health),
    trap_days = sum(.data$trap_days), captures = sum(.data$capture), .groups = "drop")
  h <- h[h$trap_days >= min_trap_days / 2, , drop = FALSE]
  h$rate <- h$captures / h$trap_days * norm
  h$overall_rate <- agg$rate[match(h$bait, agg$bait)]
  h[order(-h$overall_rate, h$bait), , drop = FALSE]
}

#' The individual captures behind one bait (for the drill). `bait` is a label from
#' `ik_bait_effectiveness` — a recipe combo ("Ping Pong + Salted Rabbit") in recipe mode, or a
#' single ingredient in ingredient mode (matched if present in the interval's recipe). `health`
#' filters to the same servicing buckets as the chart.
#' @return data.frame: observationID · check_date · trap · locationID · species, newest first.
ik_bait_captures <- function(ik_data, bait, seasons = NULL, species = NULL,
                             group = c("recipe", "ingredient"), health = NULL, intervals = NULL,
                             reserve = NULL) {
  group <- match.arg(group)
  tr <- intervals %||% .bait_intervals(ik_data, seasons, species, reserve)   # reuse the chart's table
  if (is.null(tr)) return(NULL)
  if (!is.null(health)) tr <- tr[tr$health %in% health, , drop = FALSE]
  if (!nrow(tr)) return(NULL)
  has <- if (group == "recipe") tr$combo == bait
         else vapply(tr$prior_bait, function(x) bait %in% .normalize_bait(x), logical(1))
  t <- tr[tr$is_capture & has, , drop = FALSE]
  if (!nrow(t)) return(NULL)
  t <- t[order(t$deploymentEnd, decreasing = TRUE), , drop = FALSE]
  locs <- ik_data$app$geography$locations
  data.frame(observationID = t$observationID, check_date = t$deploymentEnd, trap = t$name,
             locationID = t$locationID,
             reserve = locs$reserve[match(t$locationID, locs$location_id)],
             species = ik_species_label(t$scientificName, ik_data, "vernacular"),
             stringsAsFactors = FALSE)
}
