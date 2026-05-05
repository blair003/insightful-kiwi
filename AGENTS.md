# AGENTS.md

## Project

This is an R/Shiny app for Insightful Kiwi, which is primarily an app to analyse and visualise data from camera monitoring networks.

The app is running inside a VS Code dev container using Docker.

Do not reinstall R packages, rebuild the container, or modify Docker setup unless explicitly asked.


## Data model (important)

`extdata/` contains the primary camera monitoring source data in Camtrap DP format, which is the primary basis for the application.

`extdata/trap-data/` contains trapping data used for development and testing. This is also Camtrap DP format, but includes the source files which we run a custom conversion on as part of the application startup to transform to Camtrap DP format.

Camtrap DP key files:
- datapackage.json → schema and metadata
- deployments.csv → deployment-level data
- media.csv → media/image records
- observations.csv → observation records

When investigating data-related logic:
- start with `datapackage.json`
- then inspect specific CSVs as needed
- avoid loading full datasets unless necessary

## Image cache

`www/cache/` contains thousands of generated cache directories. Do not search or enumerate this folder.

Exception:
`www/cache/favourites/` directory structure is meaningful and may be inspected when relevant.

Structure of `www/cache/favourites/`:
- species favourites are grouped by scientific name
- period group favourites are grouped by `period_group_` directory name

When looking for representative images or favourite images, check:
`www/cache/favourites/`

## App structure

Main entry points:
- `global.R` → global setup, configuration, package loading, data loading
- `ui.R` → UI definition
- `server.R` → server logic
- `R/` → supporting functions and modules
- `config/` → environment and site-specific config

Prefer editing files in `R/` where possible rather than making large changes directly in `server.R`.

## Runtime usage

Do not run the full Shiny app unless explicitly asked.

The app is long-running and GUI-based. Running it provides limited value for debugging.

Assume the user will run the app and validate behaviour.

Prefer:
- inspecting functions
- running small, targeted R code
- reasoning about reactive logic