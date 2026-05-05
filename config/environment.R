config$env <- list(
  
  # Directory paths, you can update these if you like.
  dirs = list(
    camtrap_package = normalizePath("extdata", mustWork = FALSE),
    trap_data_source = normalizePath(file.path("extdata", "trap-data"), mustWork = FALSE),
    cache = normalizePath("cache", mustWork = FALSE),
    logs = normalizePath("logs", mustWork = FALSE)
  ),
  
  
  # List of required CRAN packages

  required_cran_packages = c(
    "logger", "shiny", "dplyr", "lubridate", "stringr", "tidyr", "bslib", "remotes",
     "DT", "kableExtra", "leaflet",  "ggplot2",  "scales", "plotly", "httr", "fs",
     "magick", "future", "shinybusy", "htmlwidgets", "shinyjs", "sf",
     "geosphere", "webshot2",  "jsonlite", "dotenv", "chromote"
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
config$env$dirs$trap_data_package <- file.path(config$env$dirs$cache, "trap-data-camtrapdp")
config$env$dirs$reports <- file.path(config$env$dirs$cache, "reports")
config$env$dirs$plots <- file.path(config$env$dirs$cache, "plots")
config$env$dirs$maps <- file.path(config$env$dirs$cache, "maps")
config$env$dirs$media <- file.path(config$env$dirs$cache, "media")

config$env$trap_data_files <- list(
  raw_trap_data = "raw-trap-data.csv",
  trap_locations = "raw-data-traps-including-coords.csv",
  reference_tables = "reference-tables.csv"
)

logger::log_debug("config$env$dirs structure:\n{paste(capture.output(str(config$env$dirs)), collapse = '\n')}")
