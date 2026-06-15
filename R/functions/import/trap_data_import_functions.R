trap_data_cache_file <- function(config, core_data, core_cache_file = NULL) {
  package_id <- if (!is.null(core_data$id) && nzchar(as.character(core_data$id))) {
    as.character(core_data$id)
  } else if (!is.null(core_cache_file) && nzchar(core_cache_file)) {
    sub("^core_data_", "", tools::file_path_sans_ext(basename(core_cache_file)))
  } else {
    "default"
  }

  package_id <- gsub("[^A-Za-z0-9_-]+", "-", package_id)
  file.path(config$env$dirs$cache, paste0("trap_data_", package_id, ".RDS"))
}

trap_data_source_paths <- function(config) {
  trap_data_source_dir <- config$env$dirs$trap_data_source
  trap_data_files <- config$env$trap_data_files

  c(
    raw_trap_data = file.path(trap_data_source_dir, trap_data_files$raw_trap_data),
    trap_locations = file.path(trap_data_source_dir, trap_data_files$trap_locations),
    reference_tables = file.path(trap_data_source_dir, trap_data_files$reference_tables)
  )
}

trap_data_file_signature <- function(paths) {
  file_info <- file.info(paths)
  data.frame(
    name = names(paths),
    path = normalizePath(paths, mustWork = FALSE),
    exists = file.exists(paths),
    size = suppressWarnings(as.numeric(file_info$size)),
    mtime = suppressWarnings(as.numeric(file_info$mtime)),
    md5 = ifelse(file.exists(paths), unname(tools::md5sum(paths)), NA_character_),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

trap_data_period_signature <- function(period_groups) {
  if (is.null(period_groups) || length(period_groups) == 0) {
    return(data.frame())
  }

  flat_period_groups <- flatten_period_groups(period_groups)
  period_names <- names(flat_period_groups)
  data.frame(
    name = period_names,
    family = vapply(flat_period_groups, function(period) as.character(period_group_value(period, "period_family", NA_character_)), character(1)),
    start_date = vapply(flat_period_groups, function(period) as.character(as.Date(period$start_date)), character(1)),
    end_date = vapply(flat_period_groups, function(period) as.character(as.Date(period$end_date)), character(1)),
    assign_period = vapply(flat_period_groups, function(period) {
      is.null(period$assign_period) || isTRUE(period$assign_period)
    }, logical(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

trap_data_monitoring_signature <- function(core_data) {
  deps <- core_data$deps
  if (is.null(deps) || nrow(deps) == 0) {
    return(list(rows = 0L))
  }

  list(
    rows = nrow(deps),
    deployment_ids = paste(utils::head(as.character(deps$deploymentID), 50), collapse = "|"),
    start_min = as.character(suppressWarnings(min(as.Date(deps$start), na.rm = TRUE))),
    end_max = as.character(suppressWarnings(max(as.Date(deps$end), na.rm = TRUE))),
    locality_count = if ("locality" %in% names(deps)) length(unique(deps$locality)) else NA_integer_,
    location_count = if ("locationName" %in% names(deps)) length(unique(deps$locationName)) else NA_integer_
  )
}

trap_data_cache_signature <- function(config, core_data, paths) {
  list(
    cache_version = 2L,
    source_files = trap_data_file_signature(paths),
    conversion = list(
      first_deployment_days = config$globals$trap_data_first_deployment_days,
      capture_prior_check_override_days = config$globals$trap_data_capture_prior_check_override_days,
      include_missing_coordinates = TRUE,
      timezone = config$globals$actual_timezone
    ),
    period_groups = trap_data_period_signature(core_data$period_groups),
    monitoring_deployments = trap_data_monitoring_signature(core_data)
  )
}

save_trap_data_cache <- function(trap_data, signature, cache_file) {
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  cache_payload <- list(
    signature = signature,
    built_at = Sys.time(),
    trap_data = trap_data
  )

  tmp_file <- tempfile(
    pattern = paste0(basename(cache_file), "."),
    tmpdir = cache_dir,
    fileext = ".tmp"
  )

  saveRDS(cache_payload, tmp_file)
  if (!file.rename(tmp_file, cache_file)) {
    unlink(tmp_file)
    stop(sprintf("Unable to replace trap data cache file: %s", cache_file))
  }

  invisible(cache_file)
}

load_trap_data <- function(config, core_data, cache_file) {
  if (!isTRUE(config$globals$import_trap_data)) {
    logger::log_info("trap_data_import_functions.R, WKT trap data import disabled by config$globals$import_trap_data.")
    return(list(
      trap_data = NULL,
      core_data = core_data
    ))
  }

  source("R/functions/import/trap_data_conversion_functions.R")

  source_paths <- trap_data_source_paths(config)
  missing_paths <- source_paths[!file.exists(source_paths)]
  if (length(missing_paths) > 0) {
    stop(sprintf(
      "Missing WKT trap data source file(s): %s",
      paste(normalizePath(missing_paths, mustWork = FALSE), collapse = ", ")
    ), call. = FALSE)
  }

  trap_cache_file <- trap_data_cache_file(config, core_data, cache_file)
  cache_signature <- trap_data_cache_signature(config, core_data, source_paths)

  if (file.exists(trap_cache_file)) {
    cached <- tryCatch(readRDS(trap_cache_file), error = function(e) NULL)
    if (!is.null(cached) &&
        !is.null(cached$trap_data) &&
        identical(cached$signature, cache_signature)) {
      logger::log_info(
        "trap_data_import_functions.R, trap data cache hit, loading converted trap data from %s",
        trap_cache_file
      )

      core_data <- ensure_core_data_app_metadata(core_data, config)
      if (is.na(core_data$app$trapping_data_updated[[1]]) && !is.null(cached$built_at)) {
        core_data <- mark_core_data_app_updated(
          core_data,
          "trapping_data_updated",
          updated = cached$built_at,
          config = config
        )
        save_core_data_cache(core_data, cache_file)
      }

      return(list(
        trap_data = cached$trap_data,
        core_data = core_data
      ))
    }

    logger::log_info(
      "trap_data_import_functions.R, trap data cache miss or stale cache at %s, importing WKT trap data...",
      trap_cache_file
    )
  } else {
    logger::log_info("trap_data_import_functions.R, trap data cache miss, importing WKT trap data...")
  }

  trap_data_source_dir <- config$env$dirs$trap_data_source
  trap_data_files <- config$env$trap_data_files

  trap_data <- convert_wkt_trap_data_to_camtrapdp(
    raw_trap_data_path = file.path(trap_data_source_dir, trap_data_files$raw_trap_data),
    trap_locations_path = file.path(trap_data_source_dir, trap_data_files$trap_locations),
    reference_tables_path = file.path(trap_data_source_dir, trap_data_files$reference_tables),
    output_dir = config$env$dirs$trap_monitoring_data,
    first_deployment_days = config$globals$trap_data_first_deployment_days,
    capture_prior_check_override_days = config$globals$trap_data_capture_prior_check_override_days,
    include_missing_coordinates = TRUE,
    log_file = file.path(
      config$env$dirs$logs,
      sprintf("wkt-trap-data-import-%s.log", format(Sys.Date(), "%Y-%m-%d"))
    ),
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

  save_trap_data_cache(trap_data, cache_signature, trap_cache_file)
  core_data <- mark_core_data_app_updated(core_data, "trapping_data_updated", config = config)
  save_core_data_cache(core_data, cache_file)

  list(
    trap_data = trap_data,
    core_data = core_data
  )
}
