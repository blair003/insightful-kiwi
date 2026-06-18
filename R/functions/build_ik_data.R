# build_ik_data.R — assemble the global ik_data container from config + manifest.
# ik_data = $datasets (imported packages) + $app (shared derived layer, built
# per-feature; empty for now) + $meta (provenance). Built once at startup.

#' Build the ik_data container.
#'
#' @param config Runtime config (see build_config()).
#' @return ik_data: a list with `$datasets`, `$app`, `$meta`.
build_ik_data <- function(config) {
  manifest <- load_manifest(config)
  datasets <- ik_import_datasets(
    manifest,
    packages_dir = config$env$dirs$packages,
    strict       = isTRUE(config$strict_datasets)
  )

  list(
    datasets = datasets,
    app  = list(),  # shared derived layer (geography, species, ...) — built per-feature
    meta = list(
      built_at   = Sys.time(),
      n_datasets = length(datasets)
    )
  )
}
