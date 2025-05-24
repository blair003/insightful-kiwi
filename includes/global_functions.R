# Function to check and install missing packages
install_if_missing <- function(packages, source = c("cran", "github")) {
  if (source == "cran") {
    missing_packages <- setdiff(packages, installed.packages()[, "Package"])
    if (length(missing_packages) > 0) {
      install.packages(missing_packages)
    }
  } else if (source == "github") {
    missing_packages <- setdiff(sapply(packages, basename), installed.packages()[, "Package"])
    if (length(missing_packages) > 0) {
      devtools::install_github(packages[match(missing_packages, sapply(packages, basename))])
    }
  } else {
    stop("Invalid source argument. Must be either 'cran' or 'github'.")
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