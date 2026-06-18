options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"),
  Ncpus = max(1, parallel::detectCores() - 1)
)

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# Removed
#   "shinyoverview",
#   "shinyWidgets",
#   "purrr",
#   "mapview",
#   "promises",
#   "gapminder",


pkgs <- c(
  "memoise",

  "logger",
  "shiny",
  "bslib",
  "lubridate",
  "dplyr",
  "tidyr",
  "DT",
  "stringr",
  "kableExtra",
  "leaflet",
  "leaflet.extras",
  "ggplot2",
  "scales",
  "plotly",
  "httr",
  "fs",
  "magick",
  "future",
  "shinybusy",
  "htmlwidgets",
  "shinyjs",
  "sf",
  "geosphere",
  "suncalc",
  "webshot2",
  "jsonlite",
  "dotenv",
  "chromote",
  "remotes",
  "inbo/camtraptor",
  "inbo/camtraptdp"
)

pak::pak(pkgs, ask = FALSE, upgrade = FALSE)
