config_core_data_app_timezone <- function(config) {
  config_actual_timezone(config, default = "UTC")
}

as_core_data_build_datetime <- function(value = Sys.time(), config) {
  as.POSIXct(
    as.numeric(value),
    origin = "1970-01-01",
    tz = config_core_data_app_timezone(config)
  )
}

empty_core_data_build_datetime <- function(config) {
  as.POSIXct(
    NA_real_,
    origin = "1970-01-01",
    tz = config_core_data_app_timezone(config)
  )
}

config_period_grouping_signature <- function(config) {
  globals <- require_config_globals(config)
  list(
    period_grouping = globals$period_grouping,
    hemisphere = globals$hemisphere,
    actual_timezone = globals$actual_timezone,
    source_timestamps_are_local = isTRUE(globals$source_timestamps_are_local),
    source_timestamp_timezone = globals$source_timestamp_timezone,
    period_calendar_timezone_version = 3L
  )
}

config_species_consolidation_signature <- function(config) {
  config_global_value(config, "spp_consol_defs")
}

core_data_source_observation_rules_version <- function() {
  "exclude_setup_unclassified_v1"
}

core_data_cache_invalidation_signature <- function(config) {
  list(
    source_observation_rules_version = core_data_source_observation_rules_version(),
    period_grouping = config_period_grouping_signature(config),
    species_consolidation = config_species_consolidation_signature(config)
  )
}

default_species_dashboard_diel_thresholds <- function() {
  list(
    insufficient_n = 30,
    normal_confidence_n = 60,
    dominant_share = 0.60,
    crepuscular_share = 0.45,
    crepuscular_component_share = 0.12,
    cathemeral_day_night_share = 0.25
  )
}

normalise_species_dashboard_diel_thresholds <- function(thresholds = NULL) {
  defaults <- default_species_dashboard_diel_thresholds()
  if (is.null(thresholds)) {
    return(defaults)
  }

  resolved <- defaults
  for (threshold_name in intersect(names(thresholds), names(defaults))) {
    threshold_value <- suppressWarnings(as.numeric(thresholds[[threshold_name]]))
    if (!is.na(threshold_value)) {
      resolved[[threshold_name]] <- threshold_value
    }
  }

  resolved
}

config_species_dashboard_diel_thresholds <- function(config) {
  normalise_species_dashboard_diel_thresholds(
    config_global_value(config, "species_dashboard_diel_thresholds")
  )
}

ensure_core_data_app_metadata <- function(core_data, config) {
  if (is.null(core_data$app)) {
    core_data$app <- list()
  }

  timestamp_fields <- c(
    "core_data_updated",
    "core_data_weather_updated",
    "trapping_data_updated"
  )

  for (field in timestamp_fields) {
    if (is.null(core_data$app[[field]])) {
      core_data$app[[field]] <- empty_core_data_build_datetime(config)
    }
  }

  core_data
}

mark_core_data_app_updated <- function(core_data, field, updated = Sys.time(), config) {
  core_data <- ensure_core_data_app_metadata(core_data, config)
  core_data$app[[field]] <- as_core_data_build_datetime(updated, config)
  core_data
}

format_core_data_build_datetime <- function(value, config) {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return("Not recorded")
  }

  timezone <- config_core_data_app_timezone(config)
  value <- as_core_data_build_datetime(value[[1]], config)
  if (is.na(value)) {
    return("Not recorded")
  }

  format(value, "%Y-%m-%d %H:%M:%S %Z", tz = timezone)
}
