# AGENTS.md

## Project

This is an R/Shiny app for Insightful Kiwi, an app to allow community-based conversation groups to analyse and visualise data from camera monitoring networks.

The app is running inside a VS Code dev container using Docker.

Do not reinstall R packages, rebuild the container, or modify Docker setup unless explicitly asked.

## Data model

The performance of this application relies on using the `core_data` global variable as the main data source.

Don't write code or functions that ingest data from files in extdata/ directly. If you think you need to, `STOP` and ask first.
The solution will likely be to update `core_data` to include what we need, if it is missing. Ask before updating core_data.

For reference:
The source for `core_data$deps` is `extdata/deployments.csv`
The source for `core_data$obs` is `extdata/observations.csv`
The source for `core_data_media` is `extdata/media.csv`

## Image cache

`www/cache/` contains thousands of generated cache directories. There is no need to search or enumerate this folder.

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