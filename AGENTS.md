# AGENTS.md

## Project Context
**Insightful Kiwi:** R/Shiny app for community conservation groups to analyze camera monitoring and trapping data.
**Environment:** VS Code dev container (Docker).
**CRITICAL:** DO NOT reinstall R packages, rebuild the container, or modify the Docker setup unless explicitly requested.

## Repository Structure
- `global.R` → Global setup, config, package/data loading.
- `ui.R` / `server.R` → UI definition / Server logic.
- `R/` → Supporting functions and modules. **(Prefer editing here over `server.R`)**.
- `www/` → Static web assets and media cache.
- `instance/` → Deployment-specific config, data, caches, and logs. *(Note: `instance/config/` contains active R code).*

## Path Management

- **Use Dynamic Base Paths:** When referencing root directories in code, DO NOT hardcode paths (e.g., "instance/cache/"). Always use the dynamic base paths defined in config/environment.R via the config$env$dirs list (e.g., config$env$dirs$cache).

- **Subdirectories**: It is acceptable to hardcode subdirectories as long as they are appended to the dynamic base paths (e.g., file.path(config$env$dirs$cache, "reports")).

## Data Model Constraints
Performance relies heavily on `core_data` and `trap_data` global variables. Both are calculated once and cached as `.RDS` files in `instance/cache/`.

- **DO NOT** write code to ingest data directly from `instance/extdata/`. If you think this is necessary, **STOP** and ask the user first.
- **DO NOT add fallback or compatibility code for stale cached `core_data` / `trap_data` structures.**  If cached data is missing expected fields or uses old build logic, update the build logic and force a cache rebuild.

- **Preferred Solution:** Update the `core_data` or `trap_data` variables rather than bypassing them.

**Source Mapping:**
- `core_data$deps` ← `extdata/deployments.csv`
- `core_data$obs` ← `extdata/observations.csv`
- `core_data$media` ← `extdata/media.csv`

## Runtime Rules
- **DO NOT** run the Shiny app unless explicitly asked. It is a long-running GUI application; the user will run the app to validate behavior.
- **DO NOT** search, parse, or analyze the following directories unless directly relevant to the task, as they contain large volumes of generated files:
  - `instance/cache/`
  - `instance/app_cache/`
  - `instance/logs/`
  - `www/media-cache/`