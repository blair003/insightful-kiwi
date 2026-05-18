load_trap_data <- function(config, core_data, cache_file) {
  if (!isTRUE(config$globals$import_trap_data)) {
    logger::log_info("trap_data_import_functions.R, WKT trap data import disabled by config$globals$import_trap_data.")
    return(list(
      trap_data = NULL,
      core_data = core_data
    ))
  }

  logger::log_info("trap_data_import_functions.R, importing WKT trap data...")
  source("R/functions/import/trap_data_conversion_functions.R")

  trap_data_source_dir <- config$env$dirs$trap_data_source
  trap_data_files <- config$env$trap_data_files

  trap_data <- convert_wkt_trap_data_to_camtrapdp(
    raw_trap_data_path = file.path(trap_data_source_dir, trap_data_files$raw_trap_data),
    trap_locations_path = file.path(trap_data_source_dir, trap_data_files$trap_locations),
    reference_tables_path = file.path(trap_data_source_dir, trap_data_files$reference_tables),
    output_dir = config$env$dirs$trap_monitoring_data,
    first_deployment_days = config$globals$trap_data_first_deployment_days,
    kill_prior_check_override_days = config$globals$trap_data_kill_prior_check_override_days,
    include_missing_coordinates = TRUE,
    package_name = "wkt-trap-checks",
    timezone = config$globals$actual_timezone,
    period_groups = core_data$period_groups,
    monitoring_deployments = core_data$deps
  )

  logger::log_info(
    "trap_data_import_functions.R, imported WKT trap data: %s deployments, %s observations, %s animal observations",
    trap_data$summary$deployments,
    trap_data$summary$observations,
    trap_data$summary$animal_observations
  )

  core_data <- mark_core_data_app_updated(core_data, "trapping_data_updated", config = config)
  save_core_data_cache(core_data, cache_file)

  list(
    trap_data = trap_data,
    core_data = core_data
  )
}
