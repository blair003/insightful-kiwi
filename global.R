# global.R

################################################################
# Bootstrap helpers, logging, and configuration
################################################################

source("R/functions/bootstrap.R")

# Auto-reload the running app when source files change — but ONLY under the editor's dev
# runner (the Posit Shiny extension launches with `--devmode`). Production (Shiny Server) must
# never autoreload. Previously hard-FALSE, which silently defeated the editor's live reload.
options(
  shiny.autoreload         = any(grepl("devmode", commandArgs(FALSE), fixed = TRUE)),
  shiny.autoreload.pattern = ".*\\.(r|R|json)$"
)

# logger is needed before instance/config/environment.R is sourced.
attach_packages("logger")
logger::log_formatter(logger::formatter_sprintf)

# UI/server stack (ui.R + server.R use shiny + bslib; DT for table views).
attach_packages(c("shiny", "bslib", "DT"))

# UI builders + modules — sourced after their packages are attached.
source("R/functions/ui_helpers.R")
source("R/functions/species_icons.R")
source("R/functions/settings_modal.R")
source("R/modules/observation_viewer.R")
source("R/modules/monitoring.R")
source("R/modules/highlights.R")
source("R/modules/trapping.R")
source("R/modules/outcomes.R")
source("R/modules/cooccurrence.R")
source("R/modules/neighbourhood.R")
source("R/modules/coverage.R")
source("R/modules/reserve_report.R")
source("R/modules/bait.R")
source("R/modules/trapping_effectiveness.R")
source("R/modules/trap_hero.R")
source("R/modules/top_trappers.R")
source("R/modules/records.R")
source("R/modules/selection.R")
source("R/modules/overview.R")
source("R/modules/duplicates.R")
source("R/modules/maps.R")
source("R/modules/species.R")

################################################################
# Data import — build the ik_data container once at startup
################################################################

attach_packages(c("camtrapdp", "dplyr", "tibble", "httr2", "lubridate", "sf", "suncalc", "ggplot2", "leaflet"))
source("R/functions/registry.R")   # ik_registry — sourced before files that register into it
source("R/functions/config.R")
source("R/functions/resolve_taxa.R")
source("R/functions/geography.R")
source("R/functions/period.R")
source("R/functions/temporal.R")
source("R/functions/monitoring.R")
source("R/functions/trap_review.R")
source("R/functions/top_trappers.R")
source("R/functions/duplicate_review.R")
source("R/functions/cooccurrence.R")
source("R/functions/bait.R")
source("R/functions/outcomes.R")
source("R/functions/species.R")
source("R/functions/select.R")
source("R/functions/metrics.R")
source("R/functions/spatial.R")
source("R/functions/proximity.R")
source("R/functions/coverage.R")
source("R/functions/neighbourhood.R")
source("R/functions/import/import_datasets.R")
source("R/functions/import/build_taxonomy.R")
source("R/functions/import/consolidate_taxa.R")
source("R/functions/import/camtrapdp_template.R")
source("R/functions/import/converters/wkt_trapping.R")
source("R/functions/import/converters/trapnz.R")
source("R/functions/import/converters.R")
source("R/functions/observation_relations.R")
source("R/functions/build_ik_data.R")
source("R/functions/data_cache.R")
source("R/functions/taxonomy.R")
source("R/functions/observations.R")
source("R/functions/media.R")
source("R/functions/media_cache.R")
source("R/functions/favourites.R")
source("R/functions/media_validate.R")

# Runtime config (paths/toggles) — the input that builds ik_data. Prefixed `ik_` like the other
# app globals (ik_data, ik_registry) so the global namespace is consistent and `config` stays free.
ik_config <- build_config()

# Load ik_data from cache, re-importing only when the manifest, package data, or conversion
# code change. Set IK_REBUILD=1 to force a full rebuild — use it while editing BUILD code
# outside R/functions/import (geography, period, temporal, ...), which the fingerprint
# deliberately does NOT track (so the dev autoreload loop isn't a re-import every save).
ik_data <- load_or_build_ik_data(ik_config, force = nzchar(Sys.getenv("IK_REBUILD")))
