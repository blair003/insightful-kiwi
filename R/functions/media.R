# media.R — access helpers for the camtrap DP `media` resource (camera image bursts).
# Observations are at EVENT level, so the images behind an observation are the media rows
# sharing its `eventID` (Agouti calls a burst a "sequence"; Camtrap DP standardises it to
# eventID). Traps carry no media (header-only resource). Provenance + timezone handling
# mirror ik_observations.R; this is the only place media leaves the pristine package.

#' Media across one or all datasets, with provenance + localized `timestamp`.
#'
#' @param ik_data The ik_data container.
#' @param dataset Dataset id(s), or NULL for all.
#' @return Media tibble (mediaID·deploymentID·eventID·timestamp·filePath·filePublic·
#'   fileName·favorite·…) with `dataset`/`source_type`. Datasets with no media contribute
#'   nothing (traps).
ik_media <- function(ik_data, dataset = NULL) {
  ids <- ik_dataset_ids(ik_data, dataset)
  dplyr::bind_rows(lapply(ids, function(id) {
    ds <- ik_data$datasets[[id]]
    m  <- camtrapdp::media(ds$package)
    if (!nrow(m)) return(NULL)
    m  <- ik_localize_times(m, ds$meta, "timestamp")
    ik_tag_provenance(m, id, ds)
  }))
}

#' The image burst behind one event (observation), ordered by capture time.
#'
#' Scans each dataset's media directly (no full-table bind) and stops at the first match —
#' `eventID` is globally unique, so one event lives in one dataset. Cheap enough to call per
#' modal open.
#'
#' @param ik_data     The ik_data container.
#' @param event_id    The observation/event id (`eventID`).
#' @param public_only Keep only `filePublic` media (default TRUE) — private images (≈ human
#'   captures) can't be fetched from Agouti, so they're not viewable.
#' @return Media tibble for the event, ordered by `timestamp`, with provenance; or NULL when
#'   the event has no (matching) media.
ik_event_media <- function(ik_data, event_id, public_only = TRUE) {
  if (length(event_id) != 1 || is.na(event_id)) return(NULL)
  for (id in names(ik_data$datasets)) {
    ds <- ik_data$datasets[[id]]
    m  <- camtrapdp::media(ds$package)
    if (!nrow(m) || !"eventID" %in% names(m)) next
    hit <- m[!is.na(m$eventID) & m$eventID == event_id, , drop = FALSE]
    if (!nrow(hit)) next
    hit <- ik_localize_times(hit, ds$meta, "timestamp")
    hit <- ik_tag_provenance(hit, id, ds)
    if (public_only) hit <- hit[!is.na(hit$filePublic) & hit$filePublic, , drop = FALSE]
    return(hit[order(hit$timestamp), , drop = FALSE])
  }
  NULL
}
