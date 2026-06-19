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

#' Present datetime columns in the dataset's timezone (manifest-driven).
#'
#' Two manifest mechanisms (mutually exclusive per dataset):
#' - `force_timezone`: the source mislabels its offset → reinterpret the wall-clock
#'   in this zone (force_tz, DST-aware) — no shift. For read packages we can't fix.
#' - `timezone`: the data is correctly offset (converters write it so) → convert to
#'   this zone for display (with_tz).
#' No-op when the dataset declares neither.
#' @keywords internal
ik_localize_times <- function(df, meta, cols) {
  set <- function(x) !is.null(x) && !is.na(x) && nzchar(x)
  op <- if (set(meta$force_timezone)) {
    function(x) lubridate::force_tz(x, meta$force_timezone)
  } else if (set(meta$timezone)) {
    function(x) lubridate::with_tz(x, meta$timezone)
  } else {
    return(df)
  }
  for (col in intersect(cols, names(df))) {
    if (inherits(df[[col]], "POSIXct")) df[[col]] <- op(df[[col]])
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
    dep <- ik_localize_times(dep, ds$meta, c("deploymentStart", "deploymentEnd"))
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
    obs <- ik_localize_times(obs, ds$meta, c("eventStart", "eventEnd"))
    ik_tag_provenance(obs, id, ds)
  }))
}
