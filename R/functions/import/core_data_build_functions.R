build_core_data_from_source <- function(config) {
  core_data <- process_camtrapdp_package()

  if (!is.null(config$globals$custom_start_date)) {
    core_data$deps <- core_data$deps %>%
      dplyr::filter(start >= as.Date(config$globals$custom_start_date))
  }

  core_data$period_groups <- create_period_groups(
    core_data$deps,
    config$globals$period_grouping,
    config$globals$hemisphere,
    include_years = isTRUE(config$globals$period_grouping_include_years)
  )

  enhanced_data <- enhance_core_data(
    core_data$obs,
    core_data$deps,
    core_data$period_groups,
    include_weather = FALSE
  )

  core_data$obs <- enhanced_data$obs
  core_data$deps <- enhanced_data$deps
  core_data$weather_daily <- enhanced_data$weather_daily
  core_data$app <- list(
    core_data_updated = as_core_data_build_datetime(Sys.time(), config),
    core_data_weather_updated = empty_core_data_build_datetime(config),
    trapping_data_updated = empty_core_data_build_datetime(config),
    daylight_classification = "suncalc_v1",
    period_grouping_signature = core_data_period_grouping_signature(config),
    status = list(
      weather_data = list(status = "not_started", required = NA_integer_, available = 0L, missing = NA_integer_)
    )
  )
  core_data <- normalise_core_data_timezones(core_data)

  core_data$spp_classes <- create_species_list(core_data$obs, core_data$taxonomic)
  core_data
}

load_core_data <- function(config, force_rebuild = FALSE, refresh_weather = FALSE) {
  if (exists("open_meteo_reset_rate_limit_flag", mode = "function", inherits = TRUE)) {
    open_meteo_reset_rate_limit_flag()
  }

  cache_info <- get_core_data_cache_info(config)
  cache_file <- cache_info$cache_file
  package_id <- cache_info$package_id

  if (isTRUE(force_rebuild) && file.exists(cache_file)) {
    logger::log_info(
      "core_data_build_functions.R, deleting cached core_data for data package id %s from %s",
      package_id,
      cache_file
    )
    unlink(cache_file)
  }

  if (file.exists(cache_file)) {
    logger::log_info(
      "core_data_build_functions.R, cache hit for data package id %s, loading core_data from %s",
      package_id,
      config$env$dirs$cache
    )
    core_data <- readRDS(cache_file)
    core_data <- upgrade_cached_core_data(core_data, cache_file)
    core_data <- ensure_core_data_app_metadata(core_data, config)
    if (!identical(core_data$app$period_grouping_signature, core_data_period_grouping_signature(config))) {
      logger::log_info(
        "core_data_build_functions.R, period grouping config changed for data package id %s, rebuilding core_data",
        package_id
      )
      core_data <- build_core_data_from_source(config)
      save_core_data_cache(core_data, cache_file)
    }
  } else {
    logger::log_info(
      "core_data_build_functions.R, cache miss for data package id %s, processing data...",
      package_id
    )
    core_data <- build_core_data_from_source(config)
    save_core_data_cache(core_data, cache_file)
  }

  if (isTRUE(refresh_weather)) {
    core_data <- enrich_core_data_weather(core_data, cache_file, config)
  }

  list(
    core_data = core_data,
    cache_file = cache_file,
    package_id = package_id,
    weather_deferred = (
      exists("open_meteo_rate_limit_hit", mode = "function", inherits = TRUE) &&
        open_meteo_rate_limit_hit()
    ) || core_data_weather_incomplete(core_data)
  )
}
