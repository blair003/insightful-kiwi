get_core_data_cache_info <- function(config) {
  monitoring_data <- jsonlite::fromJSON(file.path(config$env$dirs$camtrap_package, "datapackage.json"))
  package_id <- monitoring_data$id
  cache_filename <- paste0("core_data_", package_id, ".RDS")

  list(
    package_id = package_id,
    cache_file = file.path(config$env$dirs$cache, cache_filename)
  )
}

save_core_data_cache <- function(core_data, cache_file) {
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  tmp_file <- tempfile(
    pattern = paste0(basename(cache_file), "."),
    tmpdir = cache_dir,
    fileext = ".tmp"
  )

  saveRDS(core_data, tmp_file)
  if (!file.rename(tmp_file, cache_file)) {
    unlink(tmp_file)
    stop(sprintf("Unable to replace core_data cache file: %s", cache_file))
  }

  invisible(cache_file)
}

core_data_needs_cache_upgrade <- function(core_data) {
  !("diel_class" %in% names(core_data$obs)) ||
    !("day_night_class" %in% names(core_data$obs)) ||
    !("civil_dawn" %in% names(core_data$environment_daily)) ||
    !("civil_dusk" %in% names(core_data$environment_daily)) ||
    !identical(core_data$app$daylight_classification, "suncalc_v1")
}

upgrade_cached_core_data <- function(core_data, cache_file) {
  core_data <- normalise_core_data_timezones(core_data)

  if (core_data_needs_cache_upgrade(core_data)) {
    daylight_enrichment <- add_observation_daylight_classes(
      core_data$obs,
      core_data$deps,
      core_data$environment_daily
    )
    core_data$obs <- daylight_enrichment$obs
    core_data$environment_daily <- daylight_enrichment$environment_daily
    if (is.null(core_data$app)) {
      core_data$app <- list()
    }
    core_data$app$daylight_classification <- "suncalc_v1"
    save_core_data_cache(core_data, cache_file)
  }

  core_data
}
