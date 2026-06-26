# neighbourhood.R — the "neighbourhood seasonal panel": for a chosen protected hotspot (a camera
# LINE), track over seasons the predator & protected presence on cameras within a radius AND the
# predators caught in nearby traps. The question: is local trapping suppressing predators where the
# protected species is present? Built on app$proximity (the spatial neighbourhood) + season-anchored
# deployments.
#
# Deliberate measures, given the data:
#   • Camera presence = a POOLED per-camera-hour detection RATE over the neighbourhood's cameras
#     (NOT the per-LINE RAI, which doesn't fit a partial-line neighbourhood), net of possible
#     duplicates. Exploratory, like the map's per-camera rate.
#   • Trap removal = a per-season COUNT of predator captures in the neighbourhood's traps. Trap
#     captures are WINDOW-resolution (a kill is known only to its ~monthly check interval), so the
#     trap side is a seasonal count — never an event time. Season is the floor both sides share.
#   Both sides are seasoned by their DEPLOYMENT's season (camera = pulse window; trap = check date),
#   consistent with the app-wide season anchoring.

.nbhd_locations <- function(ik_data) {
  loc <- ik_active_locations(ik_data)   # scope to active datasets → neighbourhood/coverage hide unticked
  st  <- ik_dataset_source_types(ik_data$datasets)
  loc$source_type <- unname(st[loc$dataset])
  loc
}

#' Camera LINES available as neighbourhood anchors (reserve · line), for the picker. @keywords internal
ik_neighbourhood_lines <- function(ik_data) {
  loc <- .nbhd_locations(ik_data)
  cl  <- unique(loc[!is.na(loc$source_type) & loc$source_type == "camera" & !is.na(loc$line),
                    c("reserve", "line")])
  cl[order(cl$reserve, suppressWarnings(as.numeric(cl$line)), cl$line), , drop = FALSE]
}

#' Resolve an ANCHOR to its neighbourhood. `level` ∈ "site" (key = a camera location_id),
#' "line" (key = "reserve|line") or "reserve" (key = reserve name). Site/line take the cameras +
#' traps within `radius_m` of the anchor cameras; reserve takes ALL cameras + traps in the reserve
#' (radius not applied). Returns the anchor cameras, the neighbourhood cam/trap location_ids, a
#' location → distance-from-anchor map (0 for anchor cameras), plus `level`/`label`. Shared by the
#' series and the records drill so both use the SAME neighbourhood. NULL when the anchor has no
#' cameras. @keywords internal
.nbhd_resolve <- function(ik_data, level, key, radius_m) {
  loc <- .nbhd_locations(ik_data)
  is_cam <- !is.na(loc$source_type) & loc$source_type == "camera"
  if (identical(level, "site")) {
    anchor <- loc$location_id[is_cam & loc$location_id == key]
    label  <- loc$name[match(key, loc$location_id)]
  } else if (identical(level, "reserve")) {
    anchor <- loc$location_id[is_cam & !is.na(loc$reserve) & loc$reserve == key]
    label  <- key
  } else {                                                       # line: key = "reserve|line"
    rl <- strsplit(key, "|", fixed = TRUE)[[1]]
    anchor <- loc$location_id[is_cam & !is.na(loc$reserve) & loc$reserve == rl[1] &
                                !is.na(loc$line) & loc$line == rl[2]]
    label  <- sprintf("Line %s", rl[2])
  }
  if (!length(anchor)) return(NULL)
  if (identical(level, "reserve")) {                             # whole reserve — radius not applied
    inres <- !is.na(loc$reserve) & loc$reserve == key
    return(list(anchor = anchor, level = level, label = label,
                cam_locs  = loc$location_id[inres & is_cam],
                trap_locs = loc$location_id[inres & !is.na(loc$source_type) & loc$source_type == "trap"],
                dist_map  = numeric(0)))
  }
  # The camera story is the anchor's OWN cameras (the line itself), NOT diluted by neighbouring lines'
  # cameras. The radius governs only which TRAPS count as nearby — distance-only, so a trap just over
  # the reserve boundary still counts (predators don't respect boundaries; trap protection is about
  # proximity, not which reserve the trap belongs to).
  nb <- ik_within_distance(ik_data, anchor, radius_m = radius_m)
  list(anchor = anchor, level = level, label = label,
       cam_locs  = anchor,
       trap_locs = unique(nb$to_id[nb$to_type == "trap"]),
       dist_map  = if (nrow(nb)) tapply(nb$distance_m, nb$to_id, min) else numeric(0))
}

#' The calendar_season(s) a clicked period maps to ("season" → itself; "year" → the cycle's seasons).
#' @keywords internal
.nbhd_period_seasons <- function(ik_data, period, by) {
  if (!identical(by, "year")) return(period)
  dp   <- ik_deployment_period(ik_data)
  info <- unique(dp[!is.na(dp$calendar_season), c("calendar_season", "season", "season_year")])
  info$clab <- .ik_cycle_label(info$season, info$season_year)
  info$calendar_season[info$clab == period]
}

#' Neighbourhood seasonal series for an anchor (site / line / reserve).
#'
#' @param ik_data The container. @param level "site"/"line"/"reserve". @param key the anchor
#'   (camera location_id / "reserve|line" / reserve name; see .nbhd_resolve).
#' @param radius_m Neighbourhood radius for site/line (≤ the built app$proximity max; ignored for reserve).
#' @param predator_sci,protected_sci Resolved scientificName vectors (may be empty for one role).
#' @param by "season" or "year" (austral cycle).
#' @return tidy data.frame period · order · facet · series · value, with attrs `n_cam` / `n_trap` /
#'   `n_anchor` / `cam_facet`; NULL when the anchor or both roles are empty. Facets:
#'   "Camera activity (per N camera-hrs)" (series Protected/Predator) and
#'   "Predators caught (nearby traps)" (series Caught).
ik_neighbourhood_series <- function(ik_data, level, key, radius_m = 500,
                                    predator_sci = character(0), protected_sci = character(0),
                                    by = "season") {
  if (!length(predator_sci) && !length(protected_sci)) return(NULL)
  nbr <- .nbhd_resolve(ik_data, level, key, radius_m); if (is.null(nbr)) return(NULL)
  cam_locs <- nbr$cam_locs; trap_locs <- nbr$trap_locs

  dp   <- ik_deployment_period(ik_data)
  anim <- ik_observations(ik_data, with_location = FALSE)
  anim <- anim[!is.na(anim$observationType) & anim$observationType == "animal", , drop = FALSE]
  rel  <- ik_relations(ik_data)

  # camera: pooled net individuals / camera-hours over neighbourhood cameras, per (deployment) season
  cdep    <- dp[dp$source_type == "camera" & dp$locationID %in% cam_locs, , drop = FALSE]
  cam_eff <- tapply(cdep$effort_hours, cdep$calendar_season, sum, na.rm = TRUE)
  cam_obs <- anim[anim$deploymentID %in% cdep$deploymentID, , drop = FALSE]
  cam_obs$season <- cdep$calendar_season[match(cam_obs$deploymentID, cdep$deploymentID)]
  dup <- rel$possible_duplicate[match(cam_obs$observationID, rel$observationID)]
  cam_obs <- cam_obs[is.na(dup) | !dup, , drop = FALSE]                    # net view
  ind_by <- function(sci) {
    o <- cam_obs[!is.na(cam_obs$scientificName) & cam_obs$scientificName %in% sci, , drop = FALSE]
    if (!nrow(o)) return(stats::setNames(numeric(0), character(0)))
    tapply(o$count, o$season, sum, na.rm = TRUE)
  }
  prot_ind <- ind_by(protected_sci); pred_ind <- ind_by(predator_sci)

  # trap: predator captures in neighbourhood traps, per (check) season
  tdep     <- dp[dp$source_type == "trap" & dp$locationID %in% trap_locs, , drop = FALSE]
  trap_obs <- anim[anim$deploymentID %in% tdep$deploymentID &
                     !is.na(anim$scientificName) & anim$scientificName %in% predator_sci, , drop = FALSE]
  trap_obs$season <- tdep$calendar_season[match(trap_obs$deploymentID, tdep$deploymentID)]
  catch_by <- if (nrow(trap_obs)) tapply(trap_obs$count, trap_obs$season, sum, na.rm = TRUE)
              else stats::setNames(numeric(0), character(0))

  seasons <- ik_season_levels(dp)
  if (!length(seasons)) return(NULL)
  comp <- data.frame(season = seasons,
                     cam_hrs = as.numeric(cam_eff[seasons]), prot_ind = as.numeric(prot_ind[seasons]),
                     pred_ind = as.numeric(pred_ind[seasons]), catches = as.numeric(catch_by[seasons]),
                     stringsAsFactors = FALSE)
  for (c0 in c("cam_hrs", "prot_ind", "pred_ind", "catches")) comp[[c0]][is.na(comp[[c0]])] <- 0

  if (identical(by, "year")) {                                   # group seasons into austral years
    info <- unique(dp[!is.na(dp$calendar_season), c("calendar_season", "season", "season_year")])
    info$cycle <- .ik_cycle_year(info$season, info$season_year)
    comp$cycle <- info$cycle[match(comp$season, info$calendar_season)]
    cyc <- sort(unique(comp$cycle[!is.na(comp$cycle)]))
    agg <- do.call(rbind, lapply(cyc, function(c0) {
      s <- comp[!is.na(comp$cycle) & comp$cycle == c0, , drop = FALSE]
      data.frame(period = .ik_cycle_year_label(c0), order = c0,
                 cam_hrs = sum(s$cam_hrs), prot_ind = sum(s$prot_ind),
                 pred_ind = sum(s$pred_ind), catches = sum(s$catches), stringsAsFactors = FALSE)
    }))
  } else {
    agg <- data.frame(period = comp$season, order = seq_along(comp$season),
                      cam_hrs = comp$cam_hrs, prot_ind = comp$prot_ind,
                      pred_ind = comp$pred_ind, catches = comp$catches, stringsAsFactors = FALSE)
  }

  # Neighbourhood anchors at LINE or RESERVE level (pooling several cameras), so normalise to the
  # per-LINE figure (norm_hours, ≈ 4 cameras' worth) — same scale as RAI everywhere else — not the
  # per-camera camera_hours. (The Site anchor, which would have warranted the per-camera scale, was
  # removed.)
  per_line  <- (ik_data$meta$camera$rai %||% list())$norm_hours %||% 2000
  rate      <- function(ind, hrs) ifelse(hrs > 0, ind / hrs * per_line, NA_real_)
  cam_facet <- sprintf("Camera activity (per %s camera-hrs)", format(per_line, big.mark = ","))
  rows <- rbind(
    data.frame(period = agg$period, order = agg$order, facet = cam_facet,
               series = "Protected", value = rate(agg$prot_ind, agg$cam_hrs), stringsAsFactors = FALSE),
    data.frame(period = agg$period, order = agg$order, facet = cam_facet,
               series = "Predator",  value = rate(agg$pred_ind, agg$cam_hrs), stringsAsFactors = FALSE),
    data.frame(period = agg$period, order = agg$order, facet = "Predators caught (nearby traps)",
               series = "Caught",    value = agg$catches, stringsAsFactors = FALSE))
  attr(rows, "n_cam") <- length(cam_locs); attr(rows, "n_trap") <- length(trap_locs)
  attr(rows, "n_anchor") <- length(nbr$anchor); attr(rows, "cam_facet") <- cam_facet
  attr(rows, "level") <- level; attr(rows, "label") <- nbr$label
  rows
}

#' The individual records behind ONE point on the panel — the detections (camera) or captures (trap)
#' of `sci` in `seasons` at the neighbourhood's cameras/traps. For the click-to-drill modal.
#' @param source "camera" (net detections, by deployment season) or "trap" (captures, by check season).
#' @return data.frame observationID · when · scientificName · count · location_id · name · reserve ·
#'   line · distance_m (from the anchor), newest activity first; NULL when none.
ik_neighbourhood_records <- function(ik_data, level, key, radius_m, sci, seasons, source = "camera") {
  if (!length(sci) || !length(seasons)) return(NULL)
  nbr <- .nbhd_resolve(ik_data, level, key, radius_m); if (is.null(nbr)) return(NULL)
  dp   <- ik_deployment_period(ik_data)
  dep  <- if (identical(source, "trap")) dp[dp$source_type == "trap" & dp$locationID %in% nbr$trap_locs, , drop = FALSE]
          else                           dp[dp$source_type == "camera" & dp$locationID %in% nbr$cam_locs, , drop = FALSE]
  anim <- ik_observations(ik_data, with_location = FALSE)
  o <- anim[!is.na(anim$observationType) & anim$observationType == "animal" &
              anim$deploymentID %in% dep$deploymentID &
              !is.na(anim$scientificName) & anim$scientificName %in% sci, , drop = FALSE]
  if (!nrow(o)) return(NULL)
  o$season <- dep$calendar_season[match(o$deploymentID, dep$deploymentID)]
  o <- o[o$season %in% seasons, , drop = FALSE]
  if (identical(source, "camera") && nrow(o)) {                 # net: drop possible duplicates
    rel <- ik_relations(ik_data); dup <- rel$possible_duplicate[match(o$observationID, rel$observationID)]
    o <- o[is.na(dup) | !dup, , drop = FALSE]
  }
  if (!nrow(o)) return(NULL)
  locs <- ik_data$app$geography$locations
  o$location_id <- dep$locationID[match(o$deploymentID, dep$deploymentID)]
  gi <- match(o$location_id, locs$location_id)
  when <- if (identical(source, "trap")) o$eventEnd else o$eventStart
  dist <- ifelse(o$location_id %in% nbr$anchor, 0, as.numeric(nbr$dist_map[o$location_id]))
  data.frame(observationID = o$observationID, when = when, scientificName = o$scientificName,
             count = o$count, location_id = o$location_id, name = locs$name[gi],
             reserve = locs$reserve[gi], line = locs$line[gi], distance_m = dist,
             stringsAsFactors = FALSE)[order(when, decreasing = TRUE), , drop = FALSE]
}

#' All-time spatial snapshot of a neighbourhood, for the map under the Neighbourhood chart.
#'
#' The anchor's own cameras (with all-time protected & predator RAI) and the nearby traps (within
#' `radius_m`; all of the reserve's for a reserve) with all-time predator catches + servicing. Same
#' neighbourhood as the series/records (shared `.nbhd_resolve`), so the map shows exactly what the
#' chart summarises. Per-CAMERA rates here (map markers), unlike the per-line chart.
#'
#' @param ik_data The container. @param level "line"/"reserve". @param key the anchor.
#' @param radius_m Trap radius (line only). @param predator_sci,protected_sci resolved sci vectors.
#' @return list(cams, traps, anchor, per_cam, per_tn) — cams have prot_rai/pred_rai, traps have
#'   catches/n_checks/status/distance_m; or NULL when the anchor is empty.
ik_neighbourhood_map <- function(ik_data, level, key, radius_m = 500,
                                 predator_sci = character(0), protected_sci = character(0)) {
  nbr <- .nbhd_resolve(ik_data, level, key, radius_m); if (is.null(nbr)) return(NULL)
  locs <- ik_data$app$geography$locations
  per_cam <- (ik_data$meta$camera$rai %||% list())$camera_hours   %||% 500
  per_tn  <- (ik_data$meta$trapping$rate %||% list())$norm_trap_days %||% 100
  sel <- list(season = NULL, reserve = NULL)                       # all-time, all reserves; we filter by id
  fin <- function(d) d[is.finite(d$latitude) & is.finite(d$longitude), , drop = FALSE]
  metric_for <- function(taxa, src, norm) if (!length(taxa)) NULL else
    tryCatch(ik_location_metric(ik_data, sel, list(P = taxa), src, norm = norm), error = function(e) NULL)

  cam <- fin(locs[locs$location_id %in% nbr$cam_locs, , drop = FALSE])
  pm <- metric_for(protected_sci, "camera", per_cam); rm <- metric_for(predator_sci, "camera", per_cam)
  grab <- function(m, col) { if (is.null(m)) return(rep(0, nrow(cam))); v <- m[[col]][match(cam$location_id, m$location_id)]; v[is.na(v)] <- 0; v }
  cam$prot_rai <- grab(pm, "metric"); cam$pred_rai <- grab(rm, "metric")

  trp <- fin(locs[locs$location_id %in% nbr$trap_locs, , drop = FALSE])
  tm  <- metric_for(predator_sci, "trap", per_tn)
  trp$catches <- if (is.null(tm)) rep(0, nrow(trp)) else { v <- tm$captures[match(trp$location_id, tm$location_id)]; v[is.na(v)] <- 0; v }
  sv <- tryCatch(ik_trap_review(ik_data, seasons = NULL), error = function(e) NULL)
  si <- if (is.null(sv)) NA_integer_ else match(trp$location_id, sv$location)
  trp$n_checks <- if (is.null(sv)) NA_integer_ else sv$n_checks[si]
  trp$mean_interval_days <- if (is.null(sv)) NA_real_ else sv$mean_interval_days[si]
  trp$status   <- if (is.null(sv)) NA_character_ else sv$status[si]
  trp$distance_m <- if (length(nbr$dist_map)) as.numeric(nbr$dist_map[trp$location_id]) else NA_real_

  list(cams = cam, traps = trp, anchor = nbr$anchor, per_cam = per_cam, per_tn = per_tn)
}

#' Coverage-gap ranking — for every camera LINE (a protected hotspot), the protected & predator
#' presence on its cameras and the trapping in its neighbourhood (within `radius_m`) for the period,
#' classified to flag hotspots with thin / ineffective / neglected nearby predator control. Built on
#' app$proximity (.nbhd_resolve) + per-period ik_location_metric + ik_trap_review. Ordered worst-gap
#' first.
#'
#' @return data.frame: reserve · line · prot_rate · prot_ind · pred_rate · pred_ind · n_traps
#'   (active within R this period) · traps_per_km2 (n_traps ÷ the line's neighbourhood area at R — a
#'   per-line density that moves with the radius) · catches · n_neglected · n_active · status, where
#'   status ∈ no_trapping / predators_uncaught / neglected / covered / no_protected. NULL when none.
ik_coverage_gaps <- function(ik_data, seasons, predator_sci, protected_sci, radius_m = 500,
                             reserve = NULL, cross_boundary = FALSE) {
  lines <- ik_neighbourhood_lines(ik_data); if (!nrow(lines)) return(NULL)
  if (!is.null(reserve)) lines <- lines[lines$reserve %in% reserve, , drop = FALSE]   # Reserve picked → only its lines
  if (!nrow(lines)) return(NULL)
  norm <- (ik_data$meta$camera$rai %||% list())$norm_hours %||% 2000   # per-LINE rate (pooled cameras), like the Neighbourhood — not per-camera
  locs <- ik_data$app$geography$locations
  loc_reserve <- stats::setNames(locs$reserve, locs$location_id)       # trap → reserve, for the optional reserve scope
  # Area within `radius_m` of a line's cameras (union of disks, NZTM), km² — the denominator for the
  # PER-LINE trap density, so it tracks the line AND the gap radius (not a flat reserve average).
  nbhd_area_km2 <- function(anchor_ids) {
    p <- locs[match(anchor_ids, locs$location_id), c("longitude", "latitude"), drop = FALSE]
    p <- p[is.finite(p$longitude) & is.finite(p$latitude), , drop = FALSE]
    if (!nrow(p)) return(NA_real_)
    buf <- sf::st_union(sf::st_buffer(sf::st_transform(
      sf::st_as_sf(p, coords = c("longitude", "latitude"), crs = 4326), 2193), radius_m))
    as.numeric(units::drop_units(sf::st_area(buf))) / 1e6
  }
  spec <- list(season = seasons)
  cam_prot  <- if (length(protected_sci)) ik_location_metric(ik_data, spec, list(P = protected_sci), "camera", norm = norm) else NULL
  cam_pred  <- if (length(predator_sci))  ik_location_metric(ik_data, spec, list(P = predator_sci),  "camera", norm = norm) else NULL
  trap_pred <- if (length(predator_sci))  ik_location_metric(ik_data, spec, list(P = predator_sci),  "trap") else NULL
  serv <- ik_trap_review(ik_data, seasons = seasons)            # per-trap status for the period (may be NULL)
  pool_rate <- function(m, ids) {
    if (is.null(m)) return(list(rate = NA_real_, ind = 0L))
    d <- m[m$location_id %in% ids, , drop = FALSE]
    eff <- sum(d$camera_hours, na.rm = TRUE); ind <- sum(d$individuals, na.rm = TRUE)
    list(rate = if (eff > 0) ind / eff * norm else NA_real_, ind = as.integer(ind))
  }
  rows <- lapply(seq_len(nrow(lines)), function(i) {
    rl  <- lines[i, ]
    nbr <- .nbhd_resolve(ik_data, "line", paste(rl$reserve, rl$line, sep = "|"), radius_m); if (is.null(nbr)) return(NULL)
    # When a Reserve is selected with cross_boundary OFF, only that reserve's traps "reach" the line —
    # the count, markers and rings all stay inside the selection. With cross_boundary ON (the default
    # UI state), a trap within the radius counts wherever it's tagged (a buffer zone / the next reserve),
    # since predators don't respect boundaries. No reserve picked → the neighbourhood spans freely.
    trap_locs <- nbr$trap_locs
    if (!is.null(reserve) && !cross_boundary) trap_locs <- trap_locs[loc_reserve[trap_locs] %in% reserve]
    pr <- pool_rate(cam_prot, nbr$cam_locs); pd <- pool_rate(cam_pred, nbr$cam_locs)
    tp <- if (is.null(trap_pred)) NULL else trap_pred[trap_pred$location_id %in% trap_locs, , drop = FALSE]
    sv <- if (is.null(serv))      NULL else serv[serv$location %in% trap_locs, , drop = FALSE]
    # "Traps reaching the line" = ACTIVE traps in the neighbourhood, counted from the SAME servicing
    # universe as Neglected (one row per trap) so Neglected ⊆ Traps. NB the old count came from
    # ik_location_metric (traps with EFFORT this period), which EXCLUDES neglected traps (unchecked
    # this period) — so Neglected could exceed it. Active = not dormant/historic (i.e. still running).
    n_field <- if (is.null(sv)) 0L else as.integer(sum(!sv$status %in% c("dormant", "historic"), na.rm = TRUE))
    area_km2 <- nbhd_area_km2(nbr$anchor)                       # this line's neighbourhood area at radius_m
    data.frame(reserve = rl$reserve, line = rl$line,
      prot_rate = pr$rate, prot_ind = pr$ind, pred_rate = pd$rate, pred_ind = pd$ind,
      n_traps   = n_field,
      traps_per_km2 = if (!is.na(area_km2) && area_km2 > 0) n_field / area_km2 else NA_real_,
      catches   = if (is.null(tp)) 0L else as.integer(sum(tp$captures, na.rm = TRUE)),
      n_neglected = if (is.null(sv)) 0L else as.integer(sum(sv$status == "neglected", na.rm = TRUE)),
      n_active    = if (is.null(sv)) 0L else as.integer(sum(sv$status %in% c("good", "watch", "neglected"), na.rm = TRUE)),
      stringsAsFactors = FALSE)
  })
  g <- do.call(rbind, rows); if (is.null(g)) return(NULL)
  hotspot   <- !is.na(g$prot_rate) & g$prot_ind > 0             # protected actually present on camera
  pred_here <- !is.na(g$pred_rate) & g$pred_ind > 0             # predators on camera nearby
  negl_frac <- ifelse(g$n_active > 0, g$n_neglected / g$n_active, 0)
  g$status <- ifelse(!hotspot, "no_protected",
              ifelse(g$n_traps == 0, "no_trapping",
              ifelse(pred_here & g$catches == 0, "predators_uncaught",
              ifelse(negl_frac >= 0.5, "neglected", "covered"))))
  sev <- c(no_trapping = 4L, predators_uncaught = 3L, neglected = 2L, covered = 1L, no_protected = 0L)
  g[order(-sev[g$status], -g$prot_ind), , drop = FALSE]         # worst gaps first, then biggest hotspot
}
