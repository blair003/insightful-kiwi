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
