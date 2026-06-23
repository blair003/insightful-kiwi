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


# NOTE: this list still carries packages from the OLD v0.1 app. The split below records which
# packages the CURRENT v1 app actually uses vs. legacy ones kept only for v0.1 — nothing is removed
# yet, so the legacy block can be pruned later once v0.1 is retired. (Determined by scanning v1 code:
# attached in global.R or referenced via pkg:: across R/.)
pkgs <- c(
  # ── Required by v1 (the current app) ───────────────────────────────────────────
  # Runtime — attached in global.R or called via pkg:: across R/ (the app needs these to run):
  "logger",
  "shiny",
  "bslib",
  "DT",
  "inbo/camtrapdp",   # v1 data reader (camtrap DP)
  "dplyr",
  "lubridate",
  "httr2",
  "sf",
  "suncalc",
  "ggplot2",
  "leaflet",
  "leaflet.extras",
  "scales",
  "magick",
  "jsonlite",
  # Dev / test tooling used by the v1 workflow (not the app runtime):
  "chromote",         # headless verification / profiling
  "watcher",          # performant autoreload file watcher (dev only)

  # ── Legacy / not referenced by v1 (kept for the v0.1 app; safe to prune later) ──
  "inbo/camtraptor",  # v0.1 read data via camtraptor; v1 uses camtrapdp
  "memoise",
  "tidyr",
  "stringr",
  "kableExtra",
  "plotly",
  "fs",
  "future",
  "shinybusy",
  "htmlwidgets",
  "shinyjs",
  "geosphere",
  "webshot2",
  "dotenv",
  "remotes"
)

pak::pak(pkgs, ask = FALSE, upgrade = FALSE)
