# global.R
# At R console, run: install.packages("shiny") before running this code

# This ensures install_if_missing is defined
source("includes/global_functions.R")

# These are needed before their first use or before config/environment.R is sourced.
# Need shiny installed independently of this just to run this code
bootstrap_cran_packages <- c("logger")
install_if_missing(bootstrap_cran_packages, "cran")

# Essential libraries we just installed that are needed immediately
library(logger)

# fs, jsonlite, devtools will be loaded later by the comprehensive lapply call

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

# Load environment variables (dotenv is now available)
dotenv::load_dot_env("config/.env")


# Code to check if this data package has been processed before
data_package <- fromJSON(file.path(config$env$dirs$camtrap_package, "datapackage.json"))

package_id <- data_package$id

cache_filename <- paste0("core_data_", package_id, ".RDS")

# Construct the full file path
cache_file <- file.path(config$env$dirs$cache, cache_filename)

# If processed before, read session data and use it
if (file.exists(cache_file)) {
  logger::log_info(sprintf("global.R, cache hit for data package id %s, loading core_data from %s", 
            package_id, config$env$dirs$cache)
  )
  core_data <- readRDS(cache_file)
  
} else {
  # Cache miss, proceed with initial processing
  logger::log_info(
    sprintf("global.R, cache miss for data package id %s, processing data...", 
            package_id)
  )
  source("includes/camtrapdp_functions.R")
  
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
  
  core_data$spp_classes <- create_species_list(core_data$obs)
  
  # Save the cached data to file. Disable for debugging
  saveRDS(core_data, cache_file)
}

# These modules are required for UI and server. Dependent on core_data$period_groups
source("modules/period_selection_module.R")
source("modules/plotting_module.R")
source("modules/mapping_module.R")
