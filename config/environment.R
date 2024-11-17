# Define environment list
config$env <- list(
  # List of required CRAN packages
  required_cran_packages = c(
    "logger", "shiny", "dplyr", "lubridate", "stringr", "tidyr", "bslib", "devtools",
    "bsicons", "DT", "kableExtra", "leaflet", "leaflet.extras", "ggplot2",
    "scales", "plotly", "httr", "fs", "magick", "promises", "future", "shinybusy",
    "htmlwidgets", "shinyjs", "sf", "geosphere", "webshot2", "mapview", "jsonlite",
    "dotenv"
  ),
  
  # List required GitHub packages
  required_github_packages = c(
    "inbo/camtraptor"
  ),
  
  camtrap_package_dir = "extdata",
  
  # Set cache directory
  session_cache_dir = "cache"
)




# Ensure cache directory exists
if (!fs::dir_exists(config$env$session_cache_dir)) { 
  fs::dir_create(config$env$session_cache_dir)
}