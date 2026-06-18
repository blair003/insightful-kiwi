# build_ik_data.R — assemble the global ik_data container from config + manifest.
# ik_data = $datasets (imported packages) + $app (shared derived layer, built
# per-feature; empty for now) + $meta (provenance). Built once at startup.

#' Build the ik_data container.
#'
#' @param config   Runtime config (see build_config()).
#' @param manifest The dataset manifest; defaults to loading it from config.
#' @return ik_data: a list with `$datasets`, `$app`, `$meta`.
build_ik_data <- function(config, manifest = load_manifest(config)) {
  datasets <- ik_import_datasets(manifest, config)

  list(
    datasets = datasets,
    app  = list(  # shared derived layer (geography, ...) — grown per-feature
      taxonomy = build_taxonomy(datasets)  # species lookup: scientific <-> vernacular
    ),
    meta = list(
      built_at   = Sys.time(),
      n_datasets = length(datasets)
    )
  )
}
