# AGENTS.md

## Project

This is an R/Shiny app for Insightful Kiwi, an app to allow community-based consersation groups to analyse and visualise data from camera monitoring networks, and trapping-data.

The app is running inside a VS Code dev container using Docker.

Do not reinstall R packages, rebuild the container, or modify Docker setup unless explicitly asked.


## Repository structure

Main entry points:

- `global.R` → global setup, configuration, package loading, data loading
- `ui.R` → UI definition
- `server.R` → server logic
- `R/` → supporting functions and modules
- `www/` → static web assets and media cache
- `instance/` → instance-specific configuration, data, caches and logs
- `instance.example/` → example instance structure for new deployments

Prefer editing files in `R/` where possible rather than making large changes directly in `server.R`.

## Instance structure

The application stores instance-specific state under `instance/`.

instance/
├── config/
├── extdata/
├── cache/
└── logs/

Assume that anything under instance/ is deployment-specific rather than application code. However files in `instance/config/` are R code and do impact on how the application operates.

`instance.example/` contains example configuration and data structures intended for new deployments. Make sure `/instance.example/config/` is always updated when `instance/config/` is updated.

## Data model

The performance of this application relies on using the `core_data` and `trap_data` global variables as the main data sources. Both of these are calculated once and saved as .RDS files under `instance/cache/`. If you are not modifying the data ingestion pipeline, read the most recent cached versions instead of rebuilding them.

Don't write code or functions that ingest data from files in `instance/extdata/` directly. If you think you need to, `STOP` and ask first. 

The preferred solution will usually be to update `core_data` or `trap_data` rather than bypassing them.

For reference:
The source for `core_data$deps` is `extdata/deployments.csv`
The source for `core_data$obs` is `extdata/observations.csv`
The source for `core_data_media` is `extdata/media.csv`

## Runtime-generated data

The following locations may contain large volumes of generated files:

`instance/cache/`
`instance/app_cache/`
`instance/logs/`
`www/media-cache/`

Avoid searching or analysing these directories unless directly relevant to the task.

## Runtime usage

Do not run the full Shiny app unless explicitly asked.

The app is long-running and GUI-based. Running it provides limited value for debugging.

Assume the user will run the app and validate behaviour.