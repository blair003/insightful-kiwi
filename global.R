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
source("R/functions/import/camtrapdp_functions.R")
source("R/functions/daylight_functions.R")
source("R/functions/import/core_data_metadata_functions.R")
source("R/functions/import/core_data_cache_functions.R")
source("R/functions/import/weather_enrichment_functions.R")
source("R/functions/import/core_data_build_functions.R")
source("R/functions/import/trap_data_import_functions.R")

# Load environment variables (dotenv is now available)
dotenv::load_dot_env("config/.env")


core_data_weather_deferred <- local({
  core_data_result <- load_core_data(config)
  core_data <<- core_data_result$core_data
  cache_file <<- core_data_result$cache_file
  package_id <<- core_data_result$package_id
  isTRUE(core_data_result$weather_deferred)
})

# These modules are required for UI and server. Dependent on core_data$period_groups
source("R/functions/period_group_functions.R")

trap_data_result <- load_trap_data(config, core_data, cache_file)
trap_data <- trap_data_result$trap_data
core_data <- trap_data_result$core_data
rm(trap_data_result)

core_data <- update_year_period_bounds_from_observations(core_data, trap_data, config)
core_data$app$period_defaults <- get_default_complete_period_selection(
  core_data$deps,
  core_data$period_groups
)

source("R/modules/period_selection_module.R")
source("R/modules/plotting_module.R")
source("R/modules/mapping_module.R")
source("R/functions/monitoring_trapping_functions.R")
source("R/modules/monitoring_trapping_module.R")
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

if (isTRUE(core_data_weather_deferred)) {
  logger::log_warn(
    "global.R, weather enrichment is incomplete or deferred. 
    Loading app now and completing core_data weather cache in the background."
  )

  future::future({
    old_defer <- getOption("insightfulkiwi.defer_weather_on_rate_limit", TRUE)
    options(insightfulkiwi.defer_weather_on_rate_limit = FALSE)
    on.exit(options(insightfulkiwi.defer_weather_on_rate_limit = old_defer), add = TRUE)

    Sys.sleep(60)
    load_core_data(config, refresh_weather = TRUE)
  }, seed = TRUE) %...>% (function(rebuilt) {
    core_data <<- rebuilt$core_data
    cache_file <<- rebuilt$cache_file
    package_id <<- rebuilt$package_id

    logger::log_info(
      "global.R, background weather rebuild complete for data package id %s. 
      Cache saved to %s and global core_data updated.",
      rebuilt$package_id,
      rebuilt$cache_file
    )
  }) %...!% (function(error) {
    logger::log_error(
      "global.R, background weather rebuild failed: %s",
      conditionMessage(error)
    )
  })
}
rm(core_data_weather_deferred)


# --- Background Caching of favourite and selected species images on Startup ---
logger::log_info("Attempting to launch background image caching process...")

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
    logger::log_error("Background image caching process failed: %s", msg)
    stop(msg)
  })
}, seed = TRUE) %...>% {
  logger::log_info("Background image caching complete. Details written to %s", image_cache_log_file)
} %...!% (function(error) {
  logger::log_error("Background caching process failed: %s", conditionMessage(error))
})
# --- End Background Caching ---

enableBookmarking(store = "url")
