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

# UI/server stack (ui.R + server.R use shiny + bslib; DT for table views).
attach_packages(c("shiny", "bslib", "DT"))

# UI builders + modules — sourced after their packages are attached.
source("R/functions/settings_modal.R")
source("R/modules/records.R")

################################################################
# Data import — build the ik_data container once at startup
################################################################

attach_packages(c("camtrapdp", "dplyr", "tibble", "httr2", "lubridate"))
source("R/functions/config.R")
source("R/functions/ik_resolve_taxa.R")
source("R/functions/import/import_datasets.R")
source("R/functions/import/build_taxonomy.R")
source("R/functions/import/camtrapdp_template.R")
source("R/functions/import/converters/wkt_trapping.R")
source("R/functions/import/converters.R")
source("R/functions/build_ik_data.R")
source("R/functions/ik_data_cache.R")
source("R/functions/ik_taxonomy.R")
source("R/functions/ik_observations.R")

# Runtime config (paths/toggles) — the input that builds ik_data.
config <- build_config()

# Load ik_data from cache, re-importing only when the manifest or packages change.
ik_data <- load_or_build_ik_data(config)
