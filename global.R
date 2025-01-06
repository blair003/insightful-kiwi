# global.R
library(dotenv)
dotenv::load_dot_env("config/.env")

# Edit the config file to configure settings
source("config/config-wkt_main.R")
#source("config/config-wkt_ohiwa_forrest.R")

################################################################
######### You should not need to edit after this point ######### 
################################################################

source("includes/global_functions.R")
source("config/environment.R")

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
data_package <- fromJSON(file.path(config$env$camtrap_package_dir, "datapackage.json"))

package_id <- data_package$id

cache_filename <- paste0("core_data_", package_id, ".RDS")

# Construct the full file path
cache_file <- file.path(config$env$session_cache_dir, cache_filename)

# If processed before, read session data and use it
if (file.exists(cache_file)) {
  logger::log_info(
    sprintf("global.R, cache hit for data package id %s, loading core_data from %s", 
            package_id, config$env$session_cache_dir)
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
  
  # Ordering is important; each step builds on the last
  core_data$period_groups <- create_period_groups(
    core_data$deps, config$globals$period_grouping, config$globals$hemisphere
  )
  
  enhanced_data <- enhance_core_data(
    core_data$obs, core_data$deps, core_data$period_groups
  )
  
  core_data$obs <- enhanced_data$obs
  core_data$deps <- enhanced_data$deps
  
  core_data$spp_classes_list <- create_species_list(core_data$obs)
  
  saveRDS(core_data, cache_file)
}

# Required for UI and server
source("modules/period_selection_module.R") # Needs core_data$period_groups
source("modules/plotting_module.R")
source("modules/mapping_module.R")