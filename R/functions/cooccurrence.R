# cooccurrence.R — temporal co-occurrence of PROTECTED species and predators on camera. For each
# protected-species detection, the gap to the nearest detection of the chosen PREDATOR at the SAME
# camera: short gaps mean the two are sharing the same ground close in time (pressure). Camera-only
# — needs a clock time, so date-only trap records are excluded. Both sides are selectable (from
# species_groups roles): rats are so frequent they'd push almost every gap to ≈ 0 otherwise.

#' Per-protected-detection gap to the nearest predator detection at the same camera.
#'
#' @param ik_data       The ik_data container.
#' @param predator_sci  scientificName(s) of the predator(s) to measure against.
#' @param protected_sci scientificName(s) of the protected species.
#' @return data.frame, one row per protected detection that shares a camera with the predator. The
#'   PROTECTED side: `observationID`, `location_id`, `scientificName`, `when`. The matched nearest
#'   PREDATOR: `pred_id`, `pred_sci`, `pred_when`. And `gap_h` (absolute hours) + `signed_h` (signed:
#'   + = predator AFTER the protected detection, − = before). NULL when there are no pairings. Group
#'   by `when`'s season for a trend, by `location_id` for a per-camera layer, take `gap_h` for the
#'   distribution, or use the two ids to drill into either record.
ik_predator_protected_gaps <- function(ik_data, predator_sci, protected_sci, seasons = NULL,
                                       reserve = NULL) {
  if (!length(predator_sci) || !length(protected_sci)) return(NULL)
  obs <- ik_observations(ik_data, with_location = TRUE)
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal" &
               !is.na(obs$eventStart) & obs$scientificName %in% c(protected_sci, predator_sci), , drop = FALSE]
  if (!is.null(seasons)) {                                  # scope to a period (camera obs = deployment season)
    op   <- ik_observation_period(ik_data)
    osea <- op$calendar_season[match(obs$observationID, op$observationID)]
    obs  <- obs[!is.na(osea) & osea %in% seasons, , drop = FALSE]
  }
  if (!is.null(reserve) && length(reserve)) {              # scope to a reserve (via the location table)
    locs <- ik_data$app$geography$locations
    res  <- locs$reserve[match(obs$locationID, locs$location_id)]
    obs  <- obs[!is.na(res) & res %in% reserve, , drop = FALSE]
  }
  if (!nrow(obs)) return(NULL)
  is_prot <- obs$scientificName %in% protected_sci
  out <- do.call(rbind, lapply(split(seq_len(nrow(obs)), obs$locationID), function(ix) {
    qi <- ix[is_prot[ix]]; pj <- ix[!is_prot[ix]]                  # protected rows · predator rows
    if (!length(qi) || !length(pj)) return(NULL)
    qt <- as.numeric(obs$eventStart[qi]); pt <- as.numeric(obs$eventStart[pj])
    nn <- vapply(qt, function(t) which.min(abs(t - pt)), integer(1))   # nearest predator per protected
    signed_h <- (pt[nn] - qt) / 3600                                  # + predator after, − before
    data.frame(observationID = obs$observationID[qi], location_id = obs$locationID[qi],
               scientificName = obs$scientificName[qi], when = obs$eventStart[qi],
               pred_id = obs$observationID[pj][nn], pred_sci = obs$scientificName[pj][nn],
               pred_when = obs$eventStart[pj][nn],
               gap_h = abs(signed_h), signed_h = signed_h, stringsAsFactors = FALSE)
  }))
  if (is.null(out) || !nrow(out)) return(NULL)
  out
}
