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
= camtrapdp usage + spike.). A reference copy of the camtrapDP schema for [deployments](docs/reference/camtrapdp/deployments-table-schema.json), [observations](docs/reference/camtrapdp/observations-table-schema.json) and [media](docs/reference/camtrapdp/media-table-schema.json) is in `docs/reference/camtrapdp/`. Field references for our derived `ik_data$app$*` tables (`taxonomy`, `geography`, `species_groups`, `period`, `relations`) are in [docs/app/](docs/app/) — **descriptive**, see [Documentation](#documentation). The data-**selection** model (deployment-first; period/geography/species/method axes) is [docs/data-model/04-data-selection.md](docs/data-model/04-data-selection.md). 

Invariants:

- One global container **`ik_data`** = `$datasets` + `$app` + `$meta`.
- Each `$datasets$<id>` = `{ $package` (a **pristine** `camtrapdp` object) `, $meta` (per-dataset tags: `source_type`, `project`, … ) `}`. **Never add custom columns to a `camtrapdp` object** — only package-native transforms (`update_taxon`, `filter_*`, `merge`).
- All derived data is normalized into `$app` (shared) or `$meta` (per-dataset), **joined on demand** — never widened onto the fact tables. Placement rule: differs between datasets → `$meta`; shared reference → `$app`.
- **Event-centric:** observations carry `eventStart`/`eventEnd`; **there is no `timestamp` column** — derive a single instant in-app only where needed.
- **Geography (*where*)** and **project/organisation (*who*)** are independent axes. Geography is `app$geography` (canonical levels **location/line/reserve/global**; `region`/`country` are reserved for future cross-project aggregation, not yet in the vocabulary); project is a dataset tag.
- **Casing marks the layer:** camtrapdp package fields are camelCase (`locationID`, `eventStart`, `scientificName`); our derived `$app`/`$meta` columns are snake_case (`location_id`, `within_monitored_area`, `minutes_since_prev_same_species`). Keep new derived columns snake_case so the two never blur. (Native package columns *carried onto* a derived table keep their camelCase — e.g. `app$period$deployments` carries `deploymentStart`/`deploymentEnd`; only **newly-derived** columns must be snake_case.)


## Documentation
Two kinds, kept deliberately separate — don't collapse them:
- **This file (`AGENTS.md`) and `docs/data-model/` are prescriptive** — intent,
  conventions, invariants, limitations: *how we want things, and what not to do.*
  They are instruction. Keep them current when a **rule, convention, or
  architecture decision** changes.
- **`docs/app/` and `docs/reference/camtrapdp/` are descriptive** — field references for the
  data tables: *how things are.* **Code is the source of truth**; treat these as a
  convenience data-dictionary that may lag. Don't cite them as authority over the
  code, and don't make this file depend on them for facts.
- **`docs/reference/` holds external source material** — the camtrapDP table schemas
  (`docs/reference/camtrapdp/`) and the *DOC trail-camera guide* (the methodology basis for
  the **WKT Core Monitoring** dataset's RAI protocol: monitoring lines of 4 cameras × 21
  nights; RAI computed per line, then mean ± SE across the lines in a reserve). Authoritative
  for **that dataset's** methodology — **not** canonical for all Insightful Kiwi projects.
- **During the v1.0 rebuild, docs are not a hard per-change sync obligation.** Refresh
  a `docs/app/` field reference when that table's **shape or public contract**
  stabilises or changes — not on every internal tweak. (Tighten to "keep in sync"
  once the data model settles.)


## Packages & environment
**Any package may be used — but ASK FIRST.** Before introducing a dependency, **stop
and ask.** New packages are welcome; silent installs are not. Core stack already
chosen — the live attached set is in `global.R` (`attach_packages()`); the key
data/spatial stack is `camtrapdp` (Camtrap DP reader; successor to `camtraptor`),
`dplyr`, `sf`, `suncalc`. The mapping stack (`leaflet`, `leaflet.extras`, `geosphere`)
is already declared in `setup-r.R`, so the map module needs no new package ask.

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
- **Ad-hoc tooling for a task is yours to install freely.** System CLIs or utilities you
  need to *do* the work — render an SVG to inspect it, convert a file, probe the data —
  can be installed on the fly **without asking**; they're ephemeral dev aids (a container
  rebuild discards them), not app dependencies. The line is *persistence*: do **not** add
  anything to the `Dockerfile`, `install-system-deps.sh`, or `setup-r.R` to make a tool
  survive a rebuild without confirming first — those define the environment everyone
  shares.

## Code organisation
- **All R code in `R/`.** Keep `global.R` / `server.R` / `ui.R` thin — logic lives in
  functions and Shiny modules under `R/`.
- **Modules** follow the Shiny pattern: `<name>_ui(id)` / `<name>_server(id, …)`.
- **CSS: every module owns its own stylesheet — do NOT dump into a shared
  `custom.css`.** Put a module's styles in `www/styles/<module>.css` and load them
  from that module's UI (`tags$link(rel="stylesheet", href="styles/<module>.css")`).
  A single small `www/styles/base.css` holds only app-wide theme/tokens.
- **Data access goes through `ik_`-prefixed helpers** (`ik_dataset_ids()`,
  `ik_observations()`, `ik_deployments()`; resolvers `ik_select()` / `ik_resolve()`) —
  these wrappers are the *only* place dataset-selection + join logic lives. Filter
  first, then enrich the subset.
- **Control placement — sidebar vs in-panel, by ROLE (not by axis).** Cross-cutting
  *selection* (period, geography, dataset, comparison, net) lives in the global left
  **sidebar**: `selection_ui()` inside a `conditionalPanel("input.nav === '<value>'", …)`
  in `ui.R`, each view requesting its subset via `selection_ui(show = …)` and opting the
  sidebar open by listing its nav value in `SIDEBAR_NAVS` (`server.R`); views not listed
  manage controls locally and the sidebar auto-collapses. View-specific *mode / axis /
  display* controls (e.g. Maps device·measure·group, the Outcomes/Bait plot toggles, the
  Duplicate-window slider, review filters) render **in-panel**, above the view's content.
  Rule of thumb: **what you're looking _at_ (data in scope) → sidebar; how _this_ view
  shows it (mode, ranking, local filters) → in-panel.** **`period` ALWAYS lives in the
  sidebar** (overriding rule) — every period-scoped view, including the standalone Data →
  Quality reviews and Bait effectiveness, takes Period (and other scope axes it needs, e.g.
  Reserve) from its own `selection_ui`/`selection_server` instance, with a per-view default
  via `selection_ui(period_default=)`. The unified **`species`** picker (groups split-per-config
  + ungrouped; see [[species-picker]]) is **dual by role**, like `device`: a sidebar scope-filter
  in **Records**, but an **in-panel** control in **Maps** (there it's a GROUPING/mode choice — which
  taxon the surface is drawn for — sitting with Device/Measure, and the priority/timing sub-views
  swap it for their own predator/protected pickers). That per-view duality is **intentional, not an
  inconsistency to "fix".**

## Data & paths
- Data is loaded **once** into `ik_data` (cached as `.RDS`). App code consumes
  `ik_data`, **not** raw files. **DO NOT read directly from `instance/extdata/` in
  app code** — if you think you must, STOP and ask.
- **Never hardcode root paths.** Use `config$dirs` (e.g.
  `file.path(config$dirs$cache, "…")`). Subdirectories appended to a dynamic base
  are fine.
- **`config` is a separate global** (paths, toggles, timezone, API keys) — an *input*
  that builds `ik_data`, not part of it. **Project knowledge** splits by cadence:
  project-wide ecology (species groups, metric methodology, organisation identity)
  lives in `instance/config/project.R`; **per-dataset import + geography derivation
  (the `geography` block: `derive`, reserve maps, `reserve_match`) lives in the
  manifest `instance/config/datasets.R`.** The canonical level vocabulary itself is
  fixed in `R/functions/geography.R`, not configured.

## Runtime rules
- **DO NOT reinstall packages or rebuild the dev container** (VS Code / Docker)
  without asking.
- **DO NOT search/parse large generated dirs:** `instance/cache/`,
  `instance/app_cache/`, `instance/logs/`, `www/media-cache/`.
