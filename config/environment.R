config$env <- list(
  
  # Directory paths, you can update these if you like.
  dirs = list(
    camtrap_package = normalizePath("extdata", mustWork = FALSE),
    cache = normalizePath("cache", mustWork = FALSE),
    temp = normalizePath("temp", mustWork = FALSE)
  ),
  
  
  # List of required CRAN packages
  required_cran_packages = c(
    "logger", "shiny", "dplyr", "lubridate", "stringr", "tidyr", "bslib", "devtools",
    "bsicons", "DT", "kableExtra", "leaflet", "leaflet.extras", "ggplot2",
    "scales", "plotly", "httr", "fs", "magick", "promises", "future", "shinybusy",
    "htmlwidgets", "shinyjs", "sf", "geosphere", "webshot2", "mapview", "jsonlite", 
    "dotenv", "chromote"
  ),
  
  # List of required GitHub packages
  required_github_packages = c(
    "inbo/camtraptor"
  )
  
)


# Add subdirectories dynamically based off root path
config$env$dirs$reports <- file.path(config$env$dirs$cache, "reports")
config$env$dirs$plots <- file.path(config$env$dirs$cache, "plots")
config$env$dirs$maps <- file.path(config$env$dirs$cache, "maps")
config$env$dirs$media <- file.path(config$env$dirs$cache, "media")

logger::log_debug("config$env$dirs structure:\n{paste(capture.output(str(config$env$dirs)), collapse = '\n')}")

