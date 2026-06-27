# cooccurrence.R — temporal co-occurrence of PROTECTED species and predators on camera. For each
# protected-species detection, the gap to the nearest detection of the chosen PREDATOR at the SAME
# camera: short gaps mean the two are sharing the same ground close in time (pressure). Camera-only
# — needs a clock time, so date-only trap records are excluded. Both sides are selectable (from
# species_groups roles): rats are so frequent they'd push almost every gap to ≈ 0 otherwise.

#' Per-protected-detection gap to the nearest predator detection — at the same camera (default) or
#' within a gap radius (proximity view, like the Coverage map).
#'
#' @param ik_data       The ik_data container.
#' @param predator_sci  scientificName(s) of the predator(s) to measure against.
#' @param protected_sci scientificName(s) of the protected species.
#' @param seasons,reserve Optional period / reserve scoping.
#' @param radius_m      0 = the nearest predator AT THE SAME CAMERA (strict). >0 = the nearest
#'   predator at ANY camera within `radius_m` (proximity, via `ik_within_distance`).
#' @param cross_boundary With a reserve + radius, also pair predators on cameras in a NEIGHBOURING
#'   reserve within `radius_m` (a boundary buffer); the protected anchors stay in the reserve. Moot at
#'   radius 0. Default FALSE (strict to the reserve).
#' @return data.frame, one row per protected detection paired with a predator. The PROTECTED side:
#'   `observationID`, `location_id`, `scientificName`, `when`. The matched nearest PREDATOR: `pred_id`,
#'   `pred_sci`, `pred_when`, `pred_loc` (its camera) + `pred_dist_m` (camera separation; 0 = same
#'   camera). And `gap_h` (absolute hours) + `signed_h` (+ = predator AFTER, − = before). NULL when no
#'   pairings. Group by `when`'s season for a trend, by `location_id` for a per-camera layer, take
#'   `gap_h` for the distribution, or use the two ids to drill into either record.
ik_predator_protected_gaps <- function(ik_data, predator_sci, protected_sci, seasons = NULL,
                                       reserve = NULL, radius_m = 0, cross_boundary = FALSE,
                                       max_gap_days = (ik_data$meta$cooccurrence %||% list())$max_pair_days) {
  if (!length(predator_sci) || !length(protected_sci)) return(NULL)
  obs <- ik_observations(ik_data, with_location = TRUE)
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal" &
               !is.na(obs$eventStart) & obs$scientificName %in% c(protected_sci, predator_sci), , drop = FALSE]
  if (!is.null(seasons)) {                                  # scope to a period (camera obs = deployment season)
    op   <- ik_observation_period(ik_data)
    osea <- op$calendar_season[match(obs$observationID, op$observationID)]
    obs  <- obs[!is.na(osea) & osea %in% seasons, , drop = FALSE]
  }
  if (!nrow(obs)) return(NULL)
  is_prot <- obs$scientificName %in% protected_sci
  prot <- obs[is_prot, , drop = FALSE]; pred <- obs[!is_prot, , drop = FALSE]
  # Reserve scope: the PROTECTED anchors always stay in the selected reserve. The PREDATOR side does
  # too UNLESS cross_boundary is on — then a protected detection can pair with a predator on a camera in
  # a neighbouring reserve within radius (a boundary buffer), since predators don't respect boundaries.
  if (!is.null(reserve) && length(reserve)) {
    locs   <- ik_data$app$geography$locations
    res_of <- function(df) locs$reserve[match(df$locationID, locs$location_id)]
    prot <- prot[res_of(prot) %in% reserve, , drop = FALSE]
    if (!cross_boundary) pred <- pred[res_of(pred) %in% reserve, , drop = FALSE]
  }
  if (!nrow(prot) || !nrow(pred)) return(NULL)
  # Candidate predators for a protected detection = those at its camera (radius 0 = same camera, the
  # strict view) plus, when radius_m > 0, any camera within radius_m (the proximity view — same engine
  # as the Coverage map). The nearest IN TIME among them is the pairing; its camera distance is kept.
  adj <- if (!is.null(radius_m) && radius_m > 0)
    ik_within_distance(ik_data, unique(prot$locationID), radius_m, of = "camera") else NULL
  out <- do.call(rbind, lapply(split(seq_len(nrow(prot)), prot$locationID), function(qi) {
    cam  <- prot$locationID[qi[1]]
    nbrs <- cam; dist <- stats::setNames(0, cam)
    if (!is.null(adj)) { a <- adj[adj$from_id == cam, , drop = FALSE]
      nbrs <- c(cam, a$to_id); dist <- stats::setNames(c(0, a$distance_m), nbrs) }
    cand <- pred[pred$locationID %in% nbrs, , drop = FALSE]        # predators at this neighbourhood
    if (!nrow(cand)) return(NULL)
    qt <- as.numeric(prot$eventStart[qi]); pt <- as.numeric(cand$eventStart)
    nn <- vapply(qt, function(t) which.min(abs(t - pt)), integer(1))   # nearest predator per protected
    signed_h <- (pt[nn] - qt) / 3600                                  # + predator after, − before
    data.frame(observationID = prot$observationID[qi], location_id = cam,
               scientificName = prot$scientificName[qi], when = prot$eventStart[qi],
               pred_id = cand$observationID[nn], pred_sci = cand$scientificName[nn],
               pred_when = cand$eventStart[nn], pred_loc = cand$locationID[nn],
               pred_dist_m = as.numeric(dist[cand$locationID[nn]]),
               gap_h = abs(signed_h), signed_h = signed_h, stringsAsFactors = FALSE)
  }))
  if (is.null(out) || !nrow(out)) return(NULL)
  # Pairing window: a protected detection whose nearest predator is further away in time than this isn't a
  # meaningful co-occurrence (it just inflates the median gap), so drop it. Project config (project.R
  # cooccurrence$max_pair_days); NULL = no cap.
  if (!is.null(max_gap_days) && is.finite(max_gap_days)) {
    out <- out[out$gap_h <= max_gap_days * 24, , drop = FALSE]
    if (!nrow(out)) return(NULL)
  }
  out
}
