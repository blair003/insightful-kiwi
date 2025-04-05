# global.R
library(dotenv)
dotenv::load_dot_env("config/.env")

# Edit the config file to configure settings
source("config/config-wkt_main.R")
#source("config/config-wkt_ohiwa_forest.R")

################################################################
######### You should not need to edit after this point ######### 
################################################################

source("includes/global_functions.R")
source("config/environment.R")
ensure_directories_exist(config$env$dirs)

# Install required packages
install_if_missing(config$env$required_cran_packages, "cran")
install_if_missing(config$env$required_github_packages, "github")

# Load all packages
all_packages <- c(
  config$env$required_cran_packages,
  sapply(config$env$required_github_packages, \(pkg) sub(".*/", "", pkg))
)
lapply(all_packages, library, character.only = TRUE)

log_threshold(config$globals$log_threshold)

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
  
  # Trim the
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

# Required for UI and server. Dependent on core_data$period_groups
source("modules/period_selection_module.R")
source("modules/plotting_module.R")
source("modules/mapping_module.R")