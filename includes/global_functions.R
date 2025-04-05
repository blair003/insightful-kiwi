
# Function to check and install missing CRAN packages
install_if_missing_cran <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}

# Function to check and install missing GitHub packages
install_if_missing_github <- function(packages) {
  for (pkg in packages) {
    pkg_name <- basename(pkg)
    if (!pkg_name %in% installed.packages()[, "Package"]) {
      devtools::install_github(pkg)
    }
  }
}




ensure_directories_exist <- function(dirs) {
  if (length(dirs) == 0) {
    logger::log_warn("No directories provided to ensure_directories_exist().")
    return(invisible(NULL))
  }
  
  lapply(names(dirs), function(name) {
    dir_path <- dirs[[name]]
    if (!fs::dir_exists(dir_path)) {
      fs::dir_create(dir_path)
      logger::log_info(sprintf("Created directory '%s' at path: %s", name, dir_path))
    } else {
      logger::log_debug(sprintf("Directory '%s' already exists at path: %s", name, dir_path))
    }
  })
}