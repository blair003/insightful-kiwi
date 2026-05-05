options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"),
  Ncpus = max(1, parallel::detectCores() - 1)
)

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pkgs <- c(
  "languageserver",
  "httpgd",
  "styler",
  "lintr",
  "gapminder",
  "logger",
  "shiny",
  "shinydashboard",
  "lubridate",
  "dplyr",
  "tidyr",
  "DT",
  "stringr",
  "kableExtra",
  "memoise",
  "leaflet",
  "leaflet.extras",
  "ggplot2",
  "scales",
  "plotly",
  "httr",
  "fs",
  "purrr",
  "magick",
  "promises",
  "future",
  "shinybusy",
  "shinyWidgets",
  "htmlwidgets",
  "shinyjs",
  "sf",
  "geosphere",
  "suncalc",
  "webshot2",
  "mapview",
  "jsonlite",
  "dotenv",
  "chromote",
  "remotes",
  "inbo/camtraptor"
)

pak::pak(pkgs, ask = FALSE, upgrade = FALSE)