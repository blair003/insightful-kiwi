# ik_observations.R — access helpers for observations/deployments across datasets.
# The only place observation/deployment selection, the deployment-location join,
# and the cross-dataset union live (per AGENTS). `dataset = NULL` unions ALL
# datasets, tagging each row with its `dataset` id and `source_type` (provenance,
# materialised on the returned frame — never on the pristine package).

#' Dataset ids to operate on — one (or vector), or all when NULL.
#' @keywords internal
ik_dataset_ids <- function(ik_data, dataset = NULL) {
  if (is.null(dataset)) {
    if (length(ik_data$datasets) == 0) stop("ik_data has no datasets.", call. = FALSE)
    return(names(ik_data$datasets))
  }
  missing <- setdiff(dataset, names(ik_data$datasets))
  if (length(missing)) {
    stop(sprintf("No dataset(s) in ik_data: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  dataset
}

#' Tag a frame with provenance (dataset id + source_type). @keywords internal
ik_tag_provenance <- function(df, id, ds) {
  df$dataset     <- id
  df$source_type <- ds$meta$source_type
  df
}

#' Reinterpret datetime columns in the dataset's declared timezone.
#'
#' Override for sources that record local wall-clock but mislabel the UTC offset
#' (see the `timezone` manifest field): force_tz keeps the clock time and applies
#' the correct DST-aware offset. No-op when the dataset declares no timezone.
#' @keywords internal
ik_localize_times <- function(df, tz, cols) {
  if (is.null(tz) || is.na(tz) || !nzchar(tz)) return(df)
  for (col in intersect(cols, names(df))) {
    if (inherits(df[[col]], "POSIXct")) df[[col]] <- lubridate::force_tz(df[[col]], tz)
  }
  df
}

#' Deployments across one or all datasets.
#'
#' @param ik_data The ik_data container.
#' @param dataset Dataset id(s), or NULL for all.
#' @return Deployments tibble with `dataset`/`source_type` provenance columns.
ik_deployments <- function(ik_data, dataset = NULL) {
  ids <- ik_dataset_ids(ik_data, dataset)
  dplyr::bind_rows(lapply(ids, function(id) {
    ds  <- ik_data$datasets[[id]]
    dep <- camtrapdp::deployments(ds$package)
    dep <- ik_localize_times(dep, ds$meta$timezone, c("deploymentStart", "deploymentEnd"))
    ik_tag_provenance(dep, id, ds)
  }))
}

#' Observations across one or all datasets, optionally enriched with location.
#'
#' @param ik_data       The ik_data container.
#' @param dataset       Dataset id(s), or NULL for all (unified view).
#' @param with_location Join `locationName`/`latitude`/`longitude` from the
#'   deployment (default TRUE). The join happens here, never on the fact table.
#' @return Observations tibble with provenance (+ location) columns.
ik_observations <- function(ik_data, dataset = NULL, with_location = TRUE) {
  ids <- ik_dataset_ids(ik_data, dataset)
  dplyr::bind_rows(lapply(ids, function(id) {
    ds  <- ik_data$datasets[[id]]
    obs <- camtrapdp::observations(ds$package)
    if (with_location) {
      loc <- camtrapdp::deployments(ds$package)[
        , c("deploymentID", "locationName", "latitude", "longitude")
      ]
      obs <- dplyr::left_join(obs, loc, by = "deploymentID")
    }
    obs <- ik_localize_times(obs, ds$meta$timezone, c("eventStart", "eventEnd"))
    ik_tag_provenance(obs, id, ds)
  }))
}
