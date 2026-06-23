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
  datasets       <- ik_import_datasets(manifest, config)
  taxonomy       <- build_taxonomy(datasets)                  # species lookup
  species_groups <- resolve_species_groups(taxonomy, project) # curated registry (config)
  geography      <- build_geography(datasets)                 # canonical places

  ik_data <- list(
    datasets = datasets,
    app  = list(  # shared derived layer — grown per-feature
      taxonomy       = taxonomy,
      geography      = geography,
      species_groups = species_groups,
      period         = build_period(datasets, geography),    # deployment seasons + envelope
      temporal       = build_temporal(datasets, geography),  # per-reserve sun windows (diel)
      relations      = build_observation_relations(          # temporal metrics
        datasets, species_groups, project$duplicate_window %||% list()),
      proximity      = build_proximity(                      # spatial neighbour adjacency (camera → near)
        geography$locations, datasets, max_radius_m = (project$proximity %||% list())$max_radius_m %||% 2000),
      coverage       = build_coverage(                       # per-reserve density (area, traps/km², spacing)
        geography$locations, datasets)
    ),
    meta = list(
      built_at     = Sys.time(),
      n_datasets   = length(datasets),
      organisation = project$organisation,   # instance identity (project.R), shown in the header
      camera     = project$camera   %||% list(rai = list(norm_hours = 2000, use_net = TRUE)),
      trapping   = project$trapping %||% list(rate = list(norm_trap_days = 100), season_by = "check_date"),
      overview   = project$overview %||% list(show_rai_matrix_by_reserve = FALSE, list_other_species = TRUE,
                                              default_compare = "none"),
      proximity  = project$proximity %||% list(max_radius_m = 2000),   # neighbourhood radius (app$proximity)
      media      = project$media    %||% list(keep_originals = TRUE)
    )
  )

  # Canonical trap servicing-health thresholds: percentiles of THIS project's own per-trap
  # check-interval distribution, baked in now so they're stable and recalibrate only on
  # re-import (not as the viewed period changes). Merged ONTO the config block so the absolute
  # guardrails (floor/ceiling) and percentiles survive alongside the resolved day cutoffs.
  # Needs the assembled container (ik_deployment_period), hence post-build.
  cfg_health <- ik_data$meta$trapping$health %||% list()
  pcts <- cfg_health$percentiles %||% c(good = 0.5, watch = 0.9)
  thr <- ik_trap_health_thresholds(ik_data, pcts)
  ik_data$meta$trapping$health <- utils::modifyList(cfg_health, thr %||% list())
  # Per-dataset thresholds: each dataset judged by its OWN check-rate spread, so datasets with
  # different regimes don't blend into one global cutoff. Each trap is coloured by its dataset's
  # baseline (ik_trap_health); the trap-review legend averages these over the selected datasets
  # (ik_trap_health_cutoffs). Camera datasets have no trap deployments → NULL entry.
  ik_data$meta$trapping$health_by_dataset <- stats::setNames(
    lapply(names(ik_data$datasets), function(id) ik_trap_health_thresholds(ik_data, pcts, dataset = id)),
    names(ik_data$datasets))
  # Static all-data quality snapshots — precomputed here so they're cached in the RDS, not recomputed
  # (~1.2-1.4s each) at every session's module init, which was blocking the startup / overview load.
  ik_data$app$duplicate_gaps    <- ik_duplicate_gaps(ik_data)     # camera possible-duplicate gaps
  ik_data$app$monitoring_review <- ik_monitoring_review(ik_data)  # camera deployment review
  ik_data
}
