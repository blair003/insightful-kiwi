# global.R

################################################################
# Bootstrap helpers, logging, and configuration
################################################################

source("R/functions/global_functions.R")

# Only autoreload if code has changed
options(
  shiny.autoreload.pattern = ".*\\.(r|R|json)$",
  shiny.autoreload = FALSE
)

# logger is needed before instance/config/environment.R is sourced.
assert_packages_available("logger")
library(logger)
logger::log_formatter(logger::formatter_sprintf)
