# AGENTS.md

## Project
**Insightful Kiwi** — an **R / Shiny** app for community conservation groups to
**analyse** wildlife monitoring and control data (cameras, traps, tracking tunnels,
bait/poison stations) across one or many projects and areas, producing spatial,
temporal and species-level insight. It aims to be **world-class**, scaling from a
single group to **organisations that aggregate data across many conservation groups**
— though that single-group case is our main focus for now.

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
= camtrapdp usage + spike.). A reference copy of the camtrapDP schema for [deployments](docs/camtrapdp/deployments-table-schema.json), [observations](docs/camtrapdp/observations-table-schema.json) and [media](docs/camtrapdp/media-table-schema.json) is in `docs/camtrapdp/`. Field references for our derived `ik_data$app$*` tables (`taxonomy`, `geography`, `species_roles`, `period`, `relations`) are in [docs/app/](docs/app/) — **descriptive**, see [Documentation](#documentation). The data-**selection** model (deployment-first; period/geography/species/method axes) is [docs/data-model/04-data-selection.md](docs/data-model/04-data-selection.md). 

Invariants:

- One global container **`ik_data`** = `$datasets` + `$app` + `$meta`.
- Each `$datasets$<id>` = `{ $package` (a **pristine** `camtrapdp` object) `, $meta` (per-dataset tags: `source_type`, `project`, … ) `}`. **Never add custom columns to a `camtrapdp` object** — only package-native transforms (`update_taxon`, `filter_*`, `merge`).
- All derived data is normalized into `$app` (shared) or `$meta` (per-dataset), **joined on demand** — never widened onto the fact tables. Placement rule: differs between datasets → `$meta`; shared reference → `$app`.
- **Event-centric:** observations carry `eventStart`/`eventEnd`; **there is no `timestamp` column** — derive a single instant in-app only where needed.
- **Geography (*where*)** and **project/organisation (*who*)** are independent axes. Geography is `app$geography` (canonical levels location/line/reserve/region/country/global); project is a dataset tag.
- **Casing marks the layer:** camtrapdp package fields are camelCase (`locationID`, `eventStart`, `scientificName`); our derived `$app`/`$meta` columns are snake_case (`location_id`, `within_monitored_area`, `minutes_since_prev_same_species`). Keep new derived columns snake_case so the two never blur.


## Documentation
Two kinds, kept deliberately separate — don't collapse them:
- **This file (`AGENTS.md`) and `docs/data-model/` are prescriptive** — intent,
  conventions, invariants, limitations: *how we want things, and what not to do.*
  They are instruction. Keep them current when a **rule, convention, or
  architecture decision** changes.
- **`docs/app/` and `docs/camtrapdp/` are descriptive** — field references for the
  data tables: *how things are.* **Code is the source of truth**; treat these as a
  convenience data-dictionary that may lag. Don't cite them as authority over the
  code, and don't make this file depend on them for facts.
- **During the v1.0 rebuild, docs are not a hard per-change sync obligation.** Refresh
  a `docs/app/` field reference when that table's **shape or public contract**
  stabilises or changes — not on every internal tweak. (Tighten to "keep in sync"
  once the data model settles.)


## Packages & environment
**Any package may be used — but ASK FIRST.** Before introducing a dependency, **stop
and ask.** New packages are welcome; silent installs are not. Core stack already
chosen: `shiny`, `camtrapdp` (Camtrap DP reader; successor to `camtraptor`), `dplyr`,
`sf`, `suncalc`.

- **R packages are declared in [`.devcontainer/setup-r.R`](.devcontainer/setup-r.R)**
  (installed via `pak` by the Dockerfile). Once a package is agreed, add it to the
  `pkgs` list there — that file is **yours to maintain**. `pak` takes CRAN packages by
  name and **GitHub packages as `"user/repo"`** (e.g. `inbo/camtrapdp`; pin a ref with
  `"user/repo@<tag|sha>"`). Adding a package does *not* rebuild the container (see
  Runtime rules); the user does that.
- **The rest of `.devcontainer/` is infrastructure — do NOT edit without asking.**
  `Dockerfile`, `install-system-deps.sh` (system/`apt` libraries), and
  `devcontainer.json` are owned by the user: propose changes or wait to be asked to
  review them; don't edit them as a side effect of other work.

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
