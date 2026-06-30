# observations.R — access helpers for observations/deployments across datasets.
# The only place observation/deployment selection, the deployment-location join,
# and the cross-dataset union live (per AGENTS). `dataset = NULL` unions ALL
# datasets, tagging each row with its `dataset` id and `source_type` (provenance,
# materialised on the returned frame — never on the pristine package).

#' The session's ACTIVE datasets — the global Settings show/hide toggle (held in the Shiny
#' session's `userData$active_datasets`). NULL when there's no session (headless / build) or no
#' toggle set → "all". Reading it inside a reactive establishes the dependency, so toggling
#' re-runs the views. @keywords internal
ik_active_datasets <- function() {
  d <- shiny::getDefaultReactiveDomain()
  if (is.null(d)) return(NULL)
  v <- d$userData$active_datasets
  if (is.null(v)) return(NULL)
  if (!is.function(v)) return(v)
  # Reactive read inside a render (so a toggle re-runs the view); a plain isolated read at
  # module-init time (no reactive context → calling the reactiveVal would otherwise error).
  tryCatch(v(), error = function(e)
    tryCatch(shiny::isolate(v()), error = function(e2) NULL))
}

#' Drop rows of a dataset-tagged frame to the session's active datasets (the global toggle);
#' no-op outside a session or when the frame has no `dataset` column. @keywords internal
ik_active_filter <- function(df) {
  active <- ik_active_datasets()
  if (is.null(active) || !"dataset" %in% names(df)) return(df)
  df[df$dataset %in% active, , drop = FALSE]
}

#' Source type ("camera"/"trap"/…) of each dataset, as a named vector keyed by dataset id. The one
#' home for the `vapply(datasets, \(d) d$meta$source_type)` idiom. @param datasets `ik_data$datasets`
#' (or the raw datasets list at build time). @keywords internal
ik_dataset_source_types <- function(datasets)
  vapply(datasets, function(d) d$meta$source_type %||% NA_character_, character(1))

#' Does the container have ANY dataset of `source_type`? Replaces the scattered
#' `any(vapply(ik_data$datasets, \(d) identical(d$meta$source_type, st)))` checks. @keywords internal
ik_has_source_type <- function(ik_data, source_type)
  any(ik_dataset_source_types(ik_data$datasets) == source_type, na.rm = TRUE)

#' Is a project FEATURE enabled? Reads `meta$features[[key]]` from project.R, defaulting to TRUE
#' (an omitted flag = on), so the config block is optional and existing instances are unchanged.
#' This is the project-level WANT gate; pair it with the data-capability gate (ik_has_*), e.g.
#' `.has_trappers && ik_feature_enabled(ik_data, "top_trappers")` — a feature shows only when the
#' data can feed it AND the project wants it. @keywords internal
ik_feature_enabled <- function(ik_data, key, default = TRUE)
  isTRUE((ik_data$meta$features %||% list())[[key]] %||% default)

#' The canonical locations table SCOPED to the active datasets — use this (not the raw
#' `ik_data$app$geography$locations`) anywhere a view ENUMERATES places (reserve/line/location
#' choices, boundary hulls, density, the project header), so a hidden dataset's reserves/lines/sites
#' don't leak in. Reactive-aware (a toggle re-runs the view); returns all locations outside a session
#' (build / headless). Computed metrics already scope via ik_resolve(), so this is for the place
#' UNIVERSE only. `source_type` ("camera"/"trap") additionally scopes to that device's datasets — used
#' by the device-locked maps so their Reserve/Line pickers don't offer the OTHER device's places.
#' @keywords internal
ik_active_locations <- function(ik_data, source_type = NULL) {
  locs <- ik_active_filter(ik_data$app$geography$locations)
  if (!is.null(source_type)) {                                  # keep only the device's datasets' places
    keep <- names(ik_data$datasets)[vapply(ik_data$datasets,
              function(d) isTRUE(d$meta$source_type %in% source_type), logical(1))]
    locs <- locs[locs$dataset %in% keep, , drop = FALSE]
  }
  locs
}

#' Dataset ids to operate on — an explicit one (or vector), else the session's ACTIVE set (the
#' global toggle), else all. An explicit `dataset` OVERRIDES the toggle (so a view can be
#' independent of it, e.g. Records). @keywords internal
ik_dataset_ids <- function(ik_data, dataset = NULL) {
  if (is.null(dataset)) {
    active <- ik_active_datasets()                       # global Settings toggle
    if (!is.null(active)) return(intersect(names(ik_data$datasets), active))
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

#' Present datetime columns in the dataset's timezone + trustworthy resolution (manifest-driven).
#'
#' Timezone — two mutually-exclusive mechanisms per dataset:
#' - `force_timezone`: the source mislabels its offset → reinterpret the wall-clock
#'   in this zone (force_tz, DST-aware) — no shift. For read packages we can't fix.
#' - `timezone`: the data is correctly offset (converters write it so) → convert to
#'   this zone for display (with_tz).
#'
#' Resolution — `temporal_resolution` (OPTIONAL: "day"/"hour"/"minute"/…): the FINEST
#' granularity the source timestamps can actually be trusted to, regardless of what they
#' appear to carry (a data-entry artefact — e.g. trap checks stamped with a spurious time
#' that is really only good to the day). Timestamps are floored to this unit (in local time,
#' AFTER the tz step), so a "day" source reads as 00:00:00 → the app already treats it as
#' date-only everywhere (displays, diel, etc.). No-op / full precision when unset.
#'
#' No-op when the dataset declares none of these.
#' @keywords internal
ik_localize_times <- function(df, meta, cols) {
  set <- function(x) !is.null(x) && !is.na(x) && nzchar(x)
  ops <- list()
  if (set(meta$force_timezone))   ops[[length(ops) + 1L]] <- function(x) lubridate::force_tz(x, meta$force_timezone)
  else if (set(meta$timezone))    ops[[length(ops) + 1L]] <- function(x) lubridate::with_tz(x, meta$timezone)
  if (set(meta$temporal_resolution) && !identical(meta$temporal_resolution, "second"))
    ops[[length(ops) + 1L]] <- function(x) lubridate::floor_date(x, unit = meta$temporal_resolution)
  if (!length(ops)) return(df)
  apply_ops <- function(x) Reduce(function(acc, f) f(acc), ops, x)
  for (col in intersect(cols, names(df))) {
    if (inherits(df[[col]], "POSIXct")) df[[col]] <- apply_ops(df[[col]])
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
#' @param with_location Join `locationID`/`locationName`/`latitude`/`longitude` from
#'   the deployment (default TRUE). The join happens here, never on the fact table.
#'   `locationID` is the canonical key into `app$geography$locations` (= `location_id`).
#' @return Observations tibble with provenance (+ location) columns.
ik_observations <- function(ik_data, dataset = NULL, with_location = TRUE) {
  ids <- ik_dataset_ids(ik_data, dataset)   # reads ik_active_datasets() → keeps the reactive dependency
  # Memo within a Shiny SESSION: the Overview resolves the selection ~7×/render, each rebuilding this
  # (extract → location join → localize → tag). The result is pure given (ids, with_location) and the
  # package data is immutable per process, so cache it on the session (key includes ids, so a Settings
  # toggle yields a fresh key). No session (build / headless) → always compute fresh.
  dom <- shiny::getDefaultReactiveDomain()
  key <- paste0(paste(sort(ids), collapse = ","), "|", with_location)
  if (!is.null(dom)) {
    cache <- dom$userData$.ik_obs_cache %||% new.env(parent = emptyenv())
    dom$userData$.ik_obs_cache <- cache
    hit <- cache[[key]]; if (!is.null(hit)) return(hit)
  }
  res <- dplyr::bind_rows(lapply(ids, function(id) {
    ds  <- ik_data$datasets[[id]]
    obs <- camtrapdp::observations(ds$package)
    if (with_location) {
      loc <- camtrapdp::deployments(ds$package)[
        , c("deploymentID", "locationID", "locationName", "latitude", "longitude")
      ]
      obs <- dplyr::left_join(obs, loc, by = "deploymentID")
    }
    obs <- ik_localize_times(obs, ds$meta, c("eventStart", "eventEnd"))
    ik_tag_provenance(obs, id, ds)
  }))
  if (!is.null(dom)) cache[[key]] <- res
  res
}

#' One observation by id, enriched with its deployment + geography context.
#'
#' Scans datasets directly (no full-table bind) and stops at the first match — observationID
#' is globally unique. For the observation viewer: returns the single row plus locationID/
#' Name/lat/lon (from the deployment) and reserve/line (from app$geography), provenance, and
#' localized times.
#'
#' @param ik_data        The ik_data container.
#' @param observation_id The observationID.
#' @return A one-row data.frame, or NULL when not found.
ik_observation <- function(ik_data, observation_id) {
  if (length(observation_id) != 1 || is.na(observation_id)) return(NULL)
  for (id in names(ik_data$datasets)) {
    ds  <- ik_data$datasets[[id]]
    o   <- camtrapdp::observations(ds$package)
    hit <- o[!is.na(o$observationID) & o$observationID == observation_id, , drop = FALSE]
    if (!nrow(hit)) next
    hit <- ik_localize_times(hit, ds$meta, c("eventStart", "eventEnd"))
    dep <- camtrapdp::deployments(ds$package)
    dr  <- dep[match(hit$deploymentID, dep$deploymentID), , drop = FALSE]
    hit$locationID   <- dr$locationID
    hit$locationName <- dr$locationName
    hit$latitude     <- dr$latitude
    hit$longitude    <- dr$longitude
    hit$cameraModel  <- dr$cameraModel    # trap type for trap deployments
    hit$cameraID            <- dr$cameraID            # hardware id (Provenance)
    hit$deploymentStart     <- dr$deploymentStart     # deployment window (Provenance)
    hit$deploymentEnd       <- dr$deploymentEnd
    hit$deploymentComments  <- dr$deploymentComments  # e.g. "Converted from trap.NZ" (Provenance)
    locs <- ik_data$app$geography$locations
    gr   <- locs[match(hit$locationID, locs$location_id), , drop = FALSE]
    hit$reserve <- gr$reserve
    hit$line    <- gr$line
    hit$within_monitored_area          <- gr$within_monitored_area           # data-quality context for
    hit$nearest_monitoring_location    <- gr$nearest_monitoring_location     # out-of-reserve traps
    hit$nearest_monitoring_distance_km <- gr$nearest_monitoring_distance_km
    return(ik_tag_provenance(hit, id, ds))
  }
  NULL
}
