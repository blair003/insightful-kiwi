# AGENTS.md

## Project
**Insightful Kiwi** — an **R / Shiny** app for community conservation groups to
analyse wildlife monitoring and control data (cameras, traps, tracking tunnels,
bait/poison stations) across one or many projects and areas, producing spatial,
temporal and species-level insight.

## Status: greenfield v1.0 rebuild
We are rebuilding the app from scratch as **v1.0**, feature by feature, prompted at
each stage. Start lean (near-empty `global.R` / `server.R` / `ui.R`) and add only
what is asked for.

- The previous app (**v0.1**) is preserved as `*-v0.1` copies for **reference only —
  never source, run, or edit them**: `R-v0.1/`, `global-v0.1.R`, `server-v0.1.R`,
  `ui-v0.1.R`, `instance-v0.1/`, `www/custom-v0.1.*`, `AGENTS-v0.1.md`,
  `README-v0.1.md`. Consult them to see *how something was done*, then reimplement
  cleanly against the new architecture. **No compat/bridge code for v0.1 structures.**

## Architecture — source of truth: `docs/data-model/`
**Read [docs/data-model/01-data-structure.md](docs/data-model/01-data-structure.md)
before touching data structures or import.** ([02-app-concept.md](docs/data-model/02-app-concept.md)
= purpose/data-flow/views; [03-camtrapdp-capabilities.md](docs/data-model/03-camtrapdp-capabilities.md)
= camtrapdp usage + spike.). A reference copy of the camtrapDP schema for [deployments](docs/camtrapdp/deployments-table-schema.json), [observations](docs/camtrapdp/observations-table-schema.json) and [media](docs/camtrapdp/media-table-schema.json) is in `docs/camtrapdp/`. 

Invariants:

- One global container **`ik_data`** = `$datasets` + `$app` + `$meta`.
- Each `$datasets$<id>` = `{ $package` (a **pristine** `camtrapdp` object) `, $meta` (per-dataset tags: `source_type`, `project`, … ) `}`. **Never add custom columns to a `camtrapdp` object** — only package-native transforms (`update_taxon`, `filter_*`, `merge`).
- All derived data is normalized into `$app` (shared) or `$meta` (per-dataset), **joined on demand** — never widened onto the fact tables. Placement rule: differs between datasets → `$meta`; shared reference → `$app`.
- **Event-centric:** observations carry `eventStart`/`eventEnd`; **there is no `timestamp` column** — derive a single instant in-app only where needed.
- **Geography (*where*)** and **project/organisation (*who*)** are independent axes. Geography is `app$geography` (canonical levels location/line/reserve/region/country/global); project is a dataset tag.


## Packages
**Any package may be used — but ASK FIRST.** Before `install.packages()`, adding to
`renv`/`DESCRIPTION`, or otherwise introducing a dependency, **stop and ask.** New
packages are welcome; silent installs are not. Core stack already chosen: `shiny`,
`camtrapdp` (Camtrap DP reader; successor to `camtraptor`), `dplyr`, `sf`, `suncalc`.

## Code organisation
- **All R code in `R/`.** Keep `global.R` / `server.R` / `ui.R` thin — logic lives in
  functions and Shiny modules under `R/`.
- **Modules** follow the Shiny pattern: `<name>_ui(id)` / `<name>_server(id, …)`.
- **CSS: every module owns its own stylesheet — do NOT dump into a shared
  `custom.css`.** Put a module's styles in `www/styles/<module>.css` and load them
  from that module's UI (`tags$link(rel="stylesheet", href="styles/<module>.css")`).
  A single small `www/styles/base.css` holds only app-wide theme/tokens.
- **Data access goes through `ik_`-prefixed helpers** (`ik_datasets()`,
  `ik_observations()`, `ik_deployments()`) — these wrappers are the *only* place
  dataset-selection + join logic lives. Filter first, then enrich the subset.

## Data & paths
- Data is loaded **once** into `ik_data` (cached as `.RDS`). App code consumes
  `ik_data`, **not** raw files. **DO NOT read directly from `instance/extdata/` in
  app code** — if you think you must, STOP and ask.
- **Never hardcode root paths.** Use `config$dirs` (e.g.
  `file.path(config$dirs$cache, "…")`). Subdirectories appended to a dynamic base
  are fine.
- **`config` is a separate global** (paths, toggles, timezone, API keys) — an *input*
  that builds `ik_data`, not part of it. **Project knowledge** (geography levels +
  location assignment) lives in `instance/config/project.R`.

## Runtime rules
- **DO NOT run the Shiny app unless explicitly asked** — it is a long-running GUI; the
  user runs it to validate behaviour.
- **DO NOT reinstall packages or rebuild the dev container** (VS Code / Docker)
  without asking.
- **DO NOT search/parse large generated dirs:** `instance/cache/`,
  `instance/app_cache/`, `instance/logs/`, `www/media-cache/`.
