# global.R
# At R console, run: install.packages("shiny") before running this code

# This ensures install_if_missing is defined
source("R/functions/global_functions.R")

# These are needed before their first use or before config/environment.R is sourced.
# Need shiny installed independently of this just to run this code
bootstrap_cran_packages <- c("logger")
install_if_missing(bootstrap_cran_packages, "cran")

# Essential libraries we just installed that are needed immediately
library(logger)

logger::log_formatter(logger::formatter_sprintf)


# Source project configurations and environment settings
source("config/config-wkt_main.R")
# source("config/config-wkt_ohiwa_forest.R")

# environment.R can now be sourced (it uses logger, which is now available).
source("config/environment.R")

# Set log threshold (logger is loaded, config$globals from config-wkt_main.R is available)
if (!is.null(config$globals$log_threshold)) {
  log_threshold(config$globals$log_threshold)
} else {
  logger::log_warn("config$globals$log_threshold is not defined. Using default logger threshold.")
}



################################################################
# Main package installation and loading for the application
################################################################

# Install all other required CRAN & GitHub packages
# install_if_missing will skip packages already installed in the bootstrap phase.
logger::log_info("Checking and installing main list of CRAN packages...")
install_if_missing(config$env$required_cran_packages, "cran")

logger::log_info("Checking and installing main list of GitHub packages...")
install_if_missing(config$env$required_github_packages, "github") # devtools is now available

# Load all application packages
logger::log_info("Loading all required packages...")
all_packages <- c(
  config$env$required_cran_packages,
  sapply(config$env$required_github_packages, \(pkg) sub(".*/", "", pkg))
)
lapply(all_packages, library, character.only = TRUE)
logger::log_info("All required packages loaded.")

# create_directories_if_missing is defined in global_functions.R
create_directories_if_missing(config$env$dirs)

source("R/functions/weather_functions.R")
source("R/functions/camtrapdp_functions.R")

# Load environment variables (dotenv is now available)
dotenv::load_dot_env("config/.env")


# Code to check if this data package has been processed before
monitoring_data <- fromJSON(file.path(config$env$dirs$camtrap_package, "datapackage.json"))

package_id <- monitoring_data$id

cache_filename <- paste0("core_data_", package_id, ".RDS")

# Construct the full file path
cache_file <- file.path(config$env$dirs$cache, cache_filename)

# If processed before, read session data and use it
if (file.exists(cache_file)) {
  logger::log_info(sprintf("global.R, cache hit for data package id %s, loading core_data from %s", 
            package_id, config$env$dirs$cache)
  )
  core_data <- readRDS(cache_file)
  core_data <- normalise_core_data_timezones(core_data)
  if (is.null(core_data$weather_daily) ||
      !("matutinal_end" %in% names(core_data$weather_daily)) ||
      !("diel_class" %in% names(core_data$obs)) ||
      !("day_night_class" %in% names(core_data$obs))) {
    weather_enrichment <- enrich_observations_with_daily_weather(
      core_data$obs,
      core_data$deps,
      core_data$weather_daily
    )
    core_data$obs <- weather_enrichment$obs
    core_data$weather_daily <- weather_enrichment$weather_daily
    saveRDS(core_data, cache_file)
  }
  
} else {
  # Cache miss, proceed with initial processing
  logger::log_info(
    sprintf("global.R, cache miss for data package id %s, processing data...", 
            package_id)
  )
  core_data <- process_camtrapdp_package()

  # Trim the dataset -- a hack for Ohiwa as we want to ignore intial deployments
  if (!is.null(config$globals$custom_start_date)) {
    core_data$deps <- core_data$deps %>%
      dplyr::filter(start >= as.Date(config$globals$custom_start_date))
  }


  # Ordering is important; each step builds on the last
  core_data$period_groups <- create_period_groups(
    core_data$deps, config$globals$period_grouping, config$globals$hemisphere
  )
  #browser()
  enhanced_data <- enhance_core_data(
    core_data$obs, core_data$deps, core_data$period_groups
  )
  
  core_data$obs <- enhanced_data$obs
  core_data$deps <- enhanced_data$deps
  core_data$weather_daily <- enhanced_data$weather_daily
  core_data <- normalise_core_data_timezones(core_data)
  
  core_data$spp_classes <- create_species_list(core_data$obs)
  
  # Save the cached data to file. Disable for debugging
  saveRDS(core_data, cache_file)
}

# These modules are required for UI and server. Dependent on core_data$period_groups
source("R/functions/period_group_functions.R")
core_data$period_defaults <- get_default_complete_period_selection(
  core_data$deps,
  core_data$period_groups
)

trap_data <- NULL

if (isTRUE(config$globals$import_trap_data)) {
  logger::log_info("global.R, importing WKT trap data...")
  source("R/functions/wkt_trap_conversion_functions.R")

  trap_data_source_dir <- config$env$dirs$trap_data_source
  trap_data_files <- config$env$trap_data_files

  trap_data <- convert_wkt_trap_data_to_camtrapdp(
    raw_trap_data_path = file.path(trap_data_source_dir, trap_data_files$raw_trap_data),
    trap_locations_path = file.path(trap_data_source_dir, trap_data_files$trap_locations),
    reference_tables_path = file.path(trap_data_source_dir, trap_data_files$reference_tables),
    output_dir = config$env$dirs$trap_monitoring_data,
    first_deployment_days = config$globals$trap_data_first_deployment_days,
    package_name = "wkt-trap-checks",
    timezone = config$globals$actual_timezone,
    period_groups = core_data$period_groups
  )

  logger::log_info(
    "global.R, imported WKT trap data: %s deployments, %s observations, %s animal observations",
    trap_data$summary$deployments,
    trap_data$summary$observations,
    trap_data$summary$animal_observations
  )
} else {
  logger::log_info("global.R, WKT trap data import disabled by config$globals$import_trap_data.")
}

source("R/modules/period_selection_module.R")
source("R/modules/plotting_module.R")
source("R/modules/mapping_module.R")
source("R/modules/species_dashboard_module.R")
source("R/modules/activity_patterns_module.R")
source("R/modules/dashboard_module.R")
source("R/functions/media_functions.R")
source("R/functions/spatial_functions.R")
source("R/functions/visualisation_functions.R")
source("R/functions/utility_functions.R")
source("R/ui/ui_components.R")

# For future call
plan(multisession)


# --- Background Caching of favourite and selected species images on Startup ---
logger::log_info("Attempting to launch background caching process...")

image_cache_log_run_id <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
image_cache_log_file <- image_cache_log_path(config, "image-cache")

future::future({
  configure_image_cache_logger(config, image_cache_log_file)

  logger::log_info("----- Background image caching started: %s -----", Sys.time())

  tryCatch({
    cache_selected_images(core_data$media, core_data$obs, config)
    logger::log_info("----- Background image caching finished: %s -----", Sys.time())
    TRUE
  }, error = function(e) {
    msg <- conditionMessage(e)
    logger::log_error("Background caching process failed: %s", msg)
    stop(msg)
  })
}, seed = TRUE) %...>% {
  logger::log_info("Background caching complete. Details written to %s", image_cache_log_file)
} %...!% (function(error) {
  logger::log_error("Background caching process failed: %s", conditionMessage(error))
})
# --- End Background Caching ---

enableBookmarking(store = "url")
