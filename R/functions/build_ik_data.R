# build_ik_data.R — assemble the global ik_data container from config + manifest.
# ik_data = $datasets (imported packages) + $app (shared derived layer, built
# per-feature; empty for now) + $meta (provenance). Built once at startup.

#' Build the ik_data container.
#'
#' @param config   Runtime config (see build_config()).
#' @param manifest The dataset manifest; defaults to loading it from config.
#' @param project  Project config (instance/config/project.R); defaults to loading it.
#' @return ik_data: a list with `$datasets`, `$app`, `$meta`.
build_ik_data <- function(config, manifest = load_manifest(config),
                          project = load_project_config(config)) {
  datasets      <- ik_import_datasets(manifest, config)
  taxonomy      <- build_taxonomy(datasets)                 # species lookup
  species_roles <- resolve_species_roles(taxonomy, project) # ecological roles (config)
  geography     <- build_geography(datasets)                # canonical places

  list(
    datasets = datasets,
    app  = list(  # shared derived layer — grown per-feature
      taxonomy      = taxonomy,
      geography     = geography,
      species_roles = species_roles,
      period        = build_period(datasets, geography),    # deployment seasons + envelope
      relations     = build_observation_relations(          # temporal metrics
        datasets, species_roles, project$duplicate_window %||% list())
    ),
    meta = list(
      built_at   = Sys.time(),
      n_datasets = length(datasets)
    )
  )
}
