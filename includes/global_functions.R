
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
