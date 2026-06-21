# duplicate_review.R — camera possible-duplicate analysis for the Data → Quality "Duplicate window"
# tuner. A possible duplicate is a repeat of the SAME species at the SAME camera within the project
# window (project.R `duplicate_window`); the flag is simply gap-to-previous-same-species ≤ window
# (R/functions/observation_relations.R). So the per-detection GAP table below is all the tuner needs:
# the histogram, the at-any-window sensitivity and the burst inspector all derive from it. Camera-
# only — traps are date-only and carry no duplicate flag.

#' Per-detection same-species gaps for ALL camera animal detections — the basis for the duplicate-
#' window tuner (one row per camera detection).
#'
#' @param ik_data The ik_data container.
#' @param prefer  "vernacular" | "scientific" for the species labels.
#' @return data.frame: observationID · eventID · species · camera · reserve · line · when ·
#'   gap (minutes since the previous same-species detection at that camera; NA = first / new visit).
#'   NULL when there are no camera detections.
ik_duplicate_gaps <- function(ik_data, prefer = "vernacular") {
  obs <- ik_observations(ik_data, with_location = TRUE)
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  if (!nrow(obs)) return(NULL)
  rel <- ik_relations(ik_data); m <- match(obs$observationID, rel$observationID)
  gap  <- rel$minutes_since_prev_same_species[m]
  flag <- rel$possible_duplicate[m]
  keep <- !is.na(flag)                                       # minute-resolution (camera) obs only
  obs  <- obs[keep, , drop = FALSE]; gap <- gap[keep]
  if (!nrow(obs)) return(NULL)
  locs <- ik_data$app$geography$locations; gi <- match(obs$locationID, locs$location_id)
  lab  <- ik_species_label(obs$scientificName, ik_data, prefer); lab[is.na(lab)] <- "Unidentified"
  data.frame(
    observationID = obs$observationID,
    eventID  = if ("eventID" %in% names(obs)) obs$eventID else NA_character_,
    species  = lab, camera = locs$name[gi], reserve = locs$reserve[gi], line = locs$line[gi],
    when     = obs$eventStart, gap = gap, stringsAsFactors = FALSE)
}
