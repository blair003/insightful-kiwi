# monitoring.R — the camera-monitoring deployment review: per (location, season), is the
# deployment present and trustworthy? Feeds the Monitoring page's coverage/health grid. Each
# cell gets a SEVERITY (drives the colour — a gradient, not a category) and a specific ISSUE
# string (shown on click):
#   none     — no deployment that season (grey)
#   ok       — fine (green)
#   mild     — a watch: few triggers, or elevated blanks (yellow)
#   moderate — a concern: mostly blanks (orange)
#   serious  — a problem: almost no triggers / likely dead camera (red)
# Severity = the worse of the trigger problem and the blank problem. The blank FRACTION is
# only judged with enough triggers (MIN_EVENTS_FOR_BLANK) — a camera that fired 11× with 2
# blanks is a low-trigger watch, not an "18% blank" one. Thresholds are tunable; cameras only.

MONITORING_DEAD_EVENTS_PER_DAY  <- 0.2   # ≈ no triggers → serious (likely dead camera)
MONITORING_LOW_EVENTS_PER_DAY   <- 1.0   # below this avg trigger rate → mild (few triggers)
MONITORING_MIN_EVENTS_FOR_BLANK <- 30    # need ≥ this many triggers before judging the blank %
MONITORING_HIGH_BLANK_FRAC      <- 0.9   # blanks / triggers above this → moderate (mostly blanks)
MONITORING_ELEVATED_BLANK_FRAC  <- 0.15  # above this (but below high) → mild (elevated blanks)

#' Per (location, season) camera deployment review.
#'
#' @param ik_data The ik_data container.
#' @return A long data.frame: location · name · reserve · line · season · season_order ·
#'   n_deployments · effort_hours · n_events · n_animal · n_blank · blank_frac ·
#'   events_per_day · last_trigger · severity · issue. One row per camera location × season (the full
#'   grid, so gaps are explicit). NULL when there are no camera deployments.
ik_monitoring_review <- function(ik_data) {
  dp  <- ik_deployment_period(ik_data)
  cam <- dp[!is.na(dp$source_type) & dp$source_type == "camera", , drop = FALSE]
  if (!nrow(cam)) return(NULL)
  locs <- ik_data$app$geography$locations

  # camera observations tagged with their deployment's season + location
  obs <- ik_observations(ik_data, with_location = FALSE)
  obs <- obs[obs$deploymentID %in% cam$deploymentID, , drop = FALSE]
  mi  <- match(obs$deploymentID, cam$deploymentID)
  obs$season   <- cam$calendar_season[mi]
  obs$location <- cam$locationID[mi]

  obs_agg <- dplyr::summarise(
    dplyr::group_by(obs, .data$location, .data$season),
    n_events     = dplyr::n(),
    n_animal     = sum(.data$observationType == "animal", na.rm = TRUE),
    n_blank      = sum(.data$observationType == "blank", na.rm = TRUE),
    last_trigger = suppressWarnings(max(.data$eventEnd, na.rm = TRUE)),
    .groups = "drop")

  # deployment-record context for the detail modal (setupBy/comments aren't in the period
  # table; join them from the full deployments). Per cell there's usually one camera
  # deployment, but aggregate defensively for the rare multi-deployment season.
  depf <- ik_deployments(ik_data)
  cam$setupBy   <- depf$setupBy[match(cam$deploymentID, depf$deploymentID)]
  cam$dcomments <- depf$deploymentComments[match(cam$deploymentID, depf$deploymentID)]
  blank_na <- function(x) if (length(x)) paste(x, collapse = " | ") else NA_character_
  dep_agg <- dplyr::summarise(
    dplyr::group_by(cam, .data$locationID, .data$calendar_season),
    n_deployments = dplyr::n(),
    effort_hours  = sum(.data$effort_hours, na.rm = TRUE),
    dep_start     = suppressWarnings(min(.data$deploymentStart, na.rm = TRUE)),
    dep_end       = suppressWarnings(max(.data$deploymentEnd, na.rm = TRUE)),
    deployment_id = paste(unique(.data$deploymentID), collapse = ", "),
    setup_by      = blank_na(unique(stats::na.omit(.data$setupBy))),
    comments      = blank_na(unique(stats::na.omit(.data$dcomments))),
    .groups = "drop")
  names(dep_agg)[1:2] <- c("location", "season")

  # full grid: every camera location × every season → gaps are explicit rows
  seasons <- ik_season_levels(cam)
  grid <- expand.grid(location = unique(cam$locationID), season = seasons,
                      stringsAsFactors = FALSE)
  grid <- dplyr::left_join(grid, dep_agg, by = c("location", "season"))
  grid <- dplyr::left_join(grid, obs_agg, by = c("location", "season"))
  for (col in c("n_deployments", "effort_hours", "n_events", "n_animal", "n_blank")) {
    grid[[col]][is.na(grid[[col]])] <- 0
  }

  gi <- match(grid$location, locs$location_id)
  grid$name    <- locs$name[gi]
  grid$reserve <- locs$reserve[gi]
  grid$line    <- locs$line[gi]
  grid$season_order <- match(grid$season, seasons)

  grid$events_per_day <- ifelse(grid$effort_hours > 0, grid$n_events / (grid$effort_hours / 24), NA_real_)
  grid$blank_frac     <- ifelse(grid$n_events > 0, grid$n_blank / grid$n_events, NA_real_)
  epd    <- grid$events_per_day
  bf     <- grid$blank_frac
  enough <- grid$n_events >= MONITORING_MIN_EVENTS_FOR_BLANK & !is.na(bf)  # blank % meaningful here
  trig_sev  <- ifelse(is.na(epd), 0L,
               ifelse(epd < MONITORING_DEAD_EVENTS_PER_DAY, 3L,
               ifelse(epd < MONITORING_LOW_EVENTS_PER_DAY, 1L, 0L)))
  blank_sev <- ifelse(enough & bf > MONITORING_HIGH_BLANK_FRAC, 2L,
               ifelse(enough & bf > MONITORING_ELEVATED_BLANK_FRAC, 1L, 0L))
  sev <- pmax(trig_sev, blank_sev)                                  # 0 ok · 1 mild · 2 mod · 3 serious
  grid$severity <- ifelse(grid$n_deployments == 0, "none",
                          c("ok", "mild", "moderate", "serious")[sev + 1L])

  # specific issue text (for the click detail); the colour only carries severity
  trig_issue  <- ifelse(trig_sev == 3L, "almost no triggers (likely dead camera)",
                 ifelse(trig_sev == 1L, "few triggers", NA_character_))
  blank_issue <- ifelse(blank_sev == 2L, sprintf("mostly blanks (%.0f%%)", 100 * bf),
                 ifelse(blank_sev == 1L, sprintf("elevated blanks (%.0f%%)", 100 * bf), NA_character_))
  grid$issue <- mapply(function(nd, ti, bi) {
    if (nd == 0) return("No deployment this season")
    parts <- c(ti, bi); parts <- parts[!is.na(parts)]
    if (!length(parts)) "OK" else paste(parts, collapse = " · ")
  }, grid$n_deployments, trig_issue, blank_issue, USE.NAMES = FALSE)
  grid
}
