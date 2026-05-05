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

Do not recursively scan the entire dataset unless explicitly required.

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

## Cache usage

`cache/` contains runtime/generated data.

Some `.rds` files in `cache/` may be relevant for debugging and should not be ignored blindly.

Ignore these generated/heavy cache subfolders unless explicitly asked:

- `cache/exports/`
- `cache/maps/`
- `cache/media/`
- `cache/plots/`
- `cache/reports/`

## Search discipline

Avoid broad recursive searches.

Do not run these unless explicitly needed:

```bash
find .
ls -R
grep -R . .
```

When searching code, prefer targeted searches such as:
```bash
grep -R "search_term" R/ global.R server.R ui.R
```

Do not search:
- www/cache/
- logs/
- temp/
- large data folders unless directly relevant

## Debugging approach

When investigating an issue:

1. Reproduce the error first
2. Read the error carefully
3. Inspect only the directly relevant files
4. Make the smallest reasonable change

Do not reinstall packages, rebuild the container, or change Docker files unless explicitly asked.

## Environment

Assume these are already installed and working in the dev container:

- R 4.5.3
- Chrome for chromote / webshot2
- WeasyPrint
- system libraries for sf, GDAL, GEOS, PROJ
- R package dependencies from .devcontainer/setup-r.R

To verify the environment, use:

```bash
Rscript -e "library(shiny); library(sf); library(webshot2); library(chromote); library(magick)"
```

## Runtime

Run the app with:

```bash
Rscript -e "shiny::runApp('/srv/shiny-server/insightful.kiwi', host='0.0.0.0', port=3838)"
```

Open the app at:
`http://localhost:3838/`