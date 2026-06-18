# global.R

################################################################
# Bootstrap helpers, logging, and configuration
################################################################

source("R/functions/bootstrap.R")

# Only autoreload if code has changed
options(
  shiny.autoreload.pattern = ".*\\.(r|R|json)$",
  shiny.autoreload = FALSE
)

# logger is needed before instance/config/environment.R is sourced.
attach_packages("logger")
logger::log_formatter(logger::formatter_sprintf)

# UI/server stack (ui.R + server.R use shiny + bslib).
attach_packages(c("shiny", "bslib"))

# UI builders — sourced after their packages are attached.
source("R/functions/settings_modal.R")

################################################################
# Data import — build the ik_data container once at startup
################################################################

attach_packages("camtrapdp")
source("R/functions/config.R")
source("R/functions/import/import_datasets.R")
source("R/functions/build_ik_data.R")

# Runtime config (paths/toggles) — the input that builds ik_data.
config <- build_config()

# Read the manifested Camtrap DP packages into the global data container.
ik_data <- build_ik_data(config)
