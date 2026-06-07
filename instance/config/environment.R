config$env <- list(
  
  # Directory paths, you can update these if you like.
  dirs = list(
    camtrap_package = normalizePath("instance/extdata", mustWork = FALSE),
    trap_data_source = normalizePath(file.path("instance/extdata", "trap-data"), mustWork = FALSE),
    cache = normalizePath("instance/cache", mustWork = FALSE),
    logs = normalizePath("instance/logs", mustWork = FALSE),
    public_media_cache = normalizePath(file.path("www", "media-cache"), mustWork = FALSE),
    project_images = normalizePath(file.path("www", "project-images"), mustWork = FALSE)
  ),

  # URL path prefixes served from Shiny's www root.
  web_paths = list(
    public_media_cache = "media-cache"
  ),
  
  # List of required CRAN packages

  required_cran_packages = c(
    "logger", "shiny", "dplyr", "lubridate", "stringr", "tidyr", "bslib", "remotes",
     "DT", "kableExtra", "leaflet",  "ggplot2",  "scales", "plotly", "httr", "fs",
     "magick", "future", "shinybusy", "htmlwidgets", "shinyjs", "sf",
     "geosphere", "suncalc", "webshot2",  "jsonlite", "dotenv", "chromote"
  ),
  
  # List of required GitHub packages
  required_github_packages = c(
    "inbo/camtraptor"
  )
  
)

# Removed as no longer using
  # "leaflet.extras",
  # "bsicons",
  # "mapview",
  # "promises", 

# Add subdirectories dynamically based off root path
config$env$dirs$trap_monitoring_data <- file.path(config$env$dirs$cache, "trap-data-camtrapdp")
config$env$dirs$reports <- file.path(config$env$dirs$cache, "reports")
config$env$dirs$plots <- file.path(config$env$dirs$cache, "plots")
config$env$dirs$maps <- file.path(config$env$dirs$cache, "maps")
config$env$dirs$media <- config$env$dirs$public_media_cache

config$env$trap_data_files <- list(
  raw_trap_data = "trap-data.csv",
  trap_locations = "traps.csv",
  reference_tables = "reference-tables.csv"
)

logger::log_debug("config$env$dirs structure:\n{paste(capture.output(str(config$env$dirs)), collapse = '\n')}")
