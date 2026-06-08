options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"),
  Ncpus = max(1, parallel::detectCores() - 1)
)

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# Removed
#   "shinydashboard",
#   "leaflet.extras",
#   "shinyWidgets",
#   "purrr",
#   "mapview",
#   "promises",
#   "gapminder",


pkgs <- c(
  "languageserver",
  "httpgd",
  "styler",
  "lintr",
  "cyclocomp",

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
  "inbo/camtraptor"
)

pak::pak(pkgs, ask = FALSE, upgrade = FALSE)