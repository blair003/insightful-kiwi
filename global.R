# global.R

################################################################
# Bootstrap helpers, logging, and configuration
################################################################

source("R/functions/global_functions.R")

# Only autoreload if code has changed
options(
  shiny.autoreload.pattern = ".*\\.(r|R|json)$",
  shiny.autoreload = FALSE
)

# logger is needed before instance/config/environment.R is sourced.
assert_packages_available("logger")
library(logger)
logger::log_formatter(logger::formatter_sprintf)

source("instance/config/base.R")
source("instance/config/project.R")
source("instance/config/environment.R")

if (!is.null(config$globals$log_threshold)) {
  logger::log_threshold(config$globals$log_threshold)
} else {
  logger::log_warn("config$globals$log_threshold is not defined. Using default logger threshold.")
}


################################################################
# Package validation and loading
################################################################

main_cran_packages <- unique(c(config$env$required_cran_packages, "promises"))
main_github_packages <- config$env$required_github_packages

github_package_names <- if (length(main_github_packages) > 0) {
  vapply(main_github_packages, function(pkg) sub(".*/", "", pkg), character(1))
} else {
  character(0)
}

logger::log_info("Checking required packages are available...")
all_packages <- unique(c(main_cran_packages, github_package_names))
assert_packages_available(all_packages)

logger::log_info("Loading all required packages...")
invisible(lapply(all_packages, library, character.only = TRUE))
logger::log_info("All required packages loaded.")

create_directories_if_missing(config$env$dirs)


################################################################
# Function sources
################################################################

source("R/functions/weather_functions.R")
source("R/functions/import/camtrapdp_functions.R")
source("R/functions/daylight_functions.R")
source("R/functions/import/core_data_metadata_functions.R")
source("R/functions/import/core_data_cache_functions.R")
source("R/functions/import/weather_enrichment_functions.R")
source("R/functions/import/core_data_build_functions.R")
source("R/functions/import/trap_data_import_functions.R")
source("R/functions/period_group_functions.R")


################################################################
# Local environment overrides
################################################################

if (file.exists("instance/config/.env")) {
  dotenv::load_dot_env("instance/config/.env")
} else {
  logger::log_info("No instance/config/.env file found; continuing with process environment variables.")
}

if (!is.null(config$globals$ga_tag) && !nzchar(config$globals$ga_tag) && nzchar(Sys.getenv("GA_TAG"))) {
  config$globals$ga_tag <- Sys.getenv("GA_TAG")
}

download_image_cache_override <- parse_env_flag(
  Sys.getenv("DOWNLOAD_IMAGE_CACHE_ON_STARTUP"),
  "DOWNLOAD_IMAGE_CACHE_ON_STARTUP"
)
if (!is.null(download_image_cache_override)) {
  config$globals$download_image_cache_on_startup <- download_image_cache_override
}


################################################################
# Data loading and runtime defaults
################################################################

core_data_result <- load_core_data(config)
core_data <- core_data_result$core_data
cache_file <- core_data_result$cache_file
package_id <- core_data_result$package_id
core_data_weather_deferred <- isTRUE(core_data_result$weather_deferred)
rm(core_data_result)

trap_data_result <- load_trap_data(config, core_data, cache_file)
trap_data <- trap_data_result$trap_data
core_data <- trap_data_result$core_data
rm(trap_data_result)

core_data <- prepare_runtime_core_data(core_data, trap_data, config)


################################################################
# UI, server, and app support sources
################################################################

source("R/modules/period_selection_module.R")
source("R/modules/plotting_module.R")
source("R/modules/spatial_map_module.R")
source("R/modules/analysis_output_module.R")
source("R/modules/spatial_analysis_module.R")
source("R/functions/monitoring_trapping_functions.R")
source("R/modules/trapping_analysis_module.R")
source("R/modules/species_overview_module.R")
source("R/modules/activity_patterns_module.R")
source("R/modules/overview_module.R")
source("R/functions/media_functions.R")
source("R/functions/spatial_functions.R")
source("R/functions/visualisation_functions.R")
source("R/functions/utility_functions.R")
source("R/ui/ui_components.R")

future::plan(future::multisession)


################################################################
# Background weather refresh
################################################################

if (isTRUE(core_data_weather_deferred)) {
  logger::log_warn(
    paste(
      "global.R, weather enrichment is incomplete or deferred.",
      "Loading app now and completing core_data weather cache in the background."
    )
  )

  future::future({
    old_defer <- getOption("insightfulkiwi.defer_weather_on_rate_limit", TRUE)
    options(insightfulkiwi.defer_weather_on_rate_limit = FALSE)
    on.exit(options(insightfulkiwi.defer_weather_on_rate_limit = old_defer), add = TRUE)

    Sys.sleep(60)
    load_core_data(config, refresh_weather = TRUE)
  }, seed = TRUE) %...>% (function(rebuilt) {
    core_data <<- prepare_runtime_core_data(rebuilt$core_data, trap_data, config)
    cache_file <<- rebuilt$cache_file
    package_id <<- rebuilt$package_id

    logger::log_info(
      paste(
        "global.R, background weather rebuild complete for data package id %s.",
        "Cache saved to %s and global core_data updated."
      ),
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


################################################################
# Background image cache
################################################################

if (isTRUE(config$globals$download_image_cache_on_startup)) {
  logger::log_info("Attempting to launch background image caching process...")

  image_cache_log_run_id <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  image_cache_log_file <- image_cache_log_path(config, "image-cache")

  future::future({
    configure_image_cache_logger(config, image_cache_log_file)

    logger::log_info("----- Background image caching started: %s -----", Sys.time())

    tryCatch({
      cache_selected_images(core_data$media, core_data$obs, config)
      rebuild_favourites_manifest_from_local_cache(core_data, config)
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
} else {
  logger::log_info(paste(
    "Background image caching skipped;",
    "set config$globals$download_image_cache_on_startup = TRUE to enable it."
  ))
  rebuild_favourites_manifest_from_local_cache(core_data, config)
}

shiny::enableBookmarking(store = "url")
