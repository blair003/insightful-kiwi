# 02 — Current-State Inventory

> Supporting doc for the data-model modernisation. Captures the **current**
> monitoring + trapping pipelines, the **complete** set of fields derived during
> enhancement, the point-in-time `timestamp` assumptions, and the consumer map.
> This is the raw material the canonical structure ([01-data-structure.md](01-data-structure.md))
> is built from. Read-only audit — describes the code as it is today.

## 1. Pipelines

### Monitoring (camera)
Source: `instance/extdata/monitoring-data/` — `datapackage.json` (modern Camtrap DP
1.0) + `deployments.csv`, `observations.csv` (~11 MB), `media.csv` (~38 MB).

| Step | Where | Notes |
|---|---|---|
| Read package | `read_camtrapdp()` [camtrapdp_functions.R:530](../../R/functions/import/camtrapdp_functions.R#L530) | `camtraptor::read_camtrap_dp(file, TRUE)` — converts modern→legacy and **synthesizes `timestamp`** from `eventStart`. |
| Process | `process_camtrapdp_package()` [camtrapdp_functions.R:143](../../R/functions/import/camtrapdp_functions.R#L143) | timezone normalisation; species + taxonomy consolidation. |
| Consolidate species | `consol_spp_obs()` [camtrapdp_functions.R:571](../../R/functions/import/camtrapdp_functions.R#L571) | overwrites `scientificName`/vernacular/`taxonID` per `config$globals$spp_consol_defs`. |
| Consolidate taxa | `consol_taxa()` | taxonomy table consolidation. |
| Enhance | `enhance_core_data()` [camtrapdp_functions.R:297](../../R/functions/import/camtrapdp_functions.R#L297) | adds all derived columns (§2). |
| Build + cache | `build_core_data_from_source()` / `load_core_data()` [core_data_build_functions.R:1](../../R/functions/import/core_data_build_functions.R#L1) | caches to `instance/cache/core_data_<id>.RDS`. |
| Assemble global | [global.R:97-109](../../global.R#L97-L109) | `core_data` list. |

`core_data` = `{ id, name, created, deps, obs, media, taxonomic, period_groups,
monitoring_period_groups, environment_daily, app }`.

### Trapping
Source: `instance/extdata/trap-data/` — `trap-data.csv`, `traps.csv`,
`reference-tables.csv` (custom WKT format, **not** Camtrap DP).

| Step | Where | Notes |
|---|---|---|
| Load | `load_trap_data()` [trap_data_import_functions.R:117](../../R/functions/import/trap_data_import_functions.R#L117) | gated by `config$globals$import_trap_data`; caches RDS. |
| Convert | `convert_wkt_trap_data_to_camtrapdp()` [trap_data_conversion_functions.R:120](../../R/functions/import/trap_data_conversion_functions.R#L120) | writes Camtrap-DP-shaped files to `instance/cache/trap-data-camtrapdp/`. |
| Check dates | `parse_check_date()` [trap_data_conversion_functions.R:577](../../R/functions/import/trap_data_conversion_functions.R#L577); prior-check logic [:695-738](../../R/functions/import/trap_data_conversion_functions.R#L695-L738) | `check_date`, `previous_check_date`, `interval_days`. |
| Behavior | `trap_check_behavior()` [trap_data_conversion_functions.R:217-223](../../R/functions/import/trap_data_conversion_functions.R#L217-L223) | `captured`/`still_set`/`trap_triggered`/`bait_taken`/`trap_sprung`/`trap_rebaited`/`trap_checked`. |
| Locality match | [trap_data_conversion_functions.R:248-390](../../R/functions/import/trap_data_conversion_functions.R#L248-L390) | spatial match to monitoring locality (`within`/`nearest`). |
| Prior-check override | [trap_data_conversion_functions.R:534-544](../../R/functions/import/trap_data_conversion_functions.R#L534-L544), [:819](../../R/functions/import/trap_data_conversion_functions.R#L819) | caps capture interval via `capture_prior_check_override_days`. |
| Annotate periods | `annotate_wkt_trap_periods()` | adds `check_date`/`prior_check_date`/`period`. |

`trap_data` = `{ deps, obs, media (empty), trap_summary, taxonomic, datapackage,
summary, created, id, name }`.

## 2. Derived fields written during enhancement (the denormalization)

### `core_data$deps` — derived columns added by `enhance_core_data()`
| Field(s) | Source line | Function of |
|---|---|---|
| `calendar_season`, `calendar_year`, `monitoring_period`, `period` | [:335-347](../../R/functions/import/camtrapdp_functions.R#L335-L347) | `deploymentStart` date + period defs |
| `<observationType>_detections_count` (e.g. `animal_detections_count`) | [:365-375](../../R/functions/import/camtrapdp_functions.R#L365-L375) | obs counts per deployment |
| `camera_hours` | [:383](../../R/functions/import/camtrapdp_functions.R#L383) | `end - start` |
| `locality` | [:386-390](../../R/functions/import/camtrapdp_functions.R#L386-L390) | `locationName` + `config$meta$localities_list` |
| `line` | [:393-395](../../R/functions/import/camtrapdp_functions.R#L393-L395) | parsed from `locationName` |
| *(removed)* `coordinateUncertainty, timestampIssues, session, array, featureType, habitat, tags, _id` | [:349-352](../../R/functions/import/camtrapdp_functions.R#L349-L352) | dropped |

### `core_data$obs` — derived columns added by `enhance_core_data()`
| Field(s) | Source line | Function of |
|---|---|---|
| `scientificName_lower` | [:427](../../R/functions/import/camtrapdp_functions.R#L427) | `scientificName` (trivial) |
| `locationID, locality, line, locationName, longitude, latitude, monitoring_period` | [:431-437](../../R/functions/import/camtrapdp_functions.R#L431-L437) | **joined from `deps`** by `deploymentID` |
| `observation_date` | [:439](../../R/functions/import/camtrapdp_functions.R#L439) | `timestamp` (dropped again at [:453](../../R/functions/import/camtrapdp_functions.R#L453)) |
| `calendar_season, calendar_year, monitoring_period, period` | [:440-449](../../R/functions/import/camtrapdp_functions.R#L440-L449) | `observation_date` + period defs |
| `species_class, species_rank` | [:450-451](../../R/functions/import/camtrapdp_functions.R#L450-L451) | `scientificName_lower` + `spp_classes` |
| `possible_duplicate` | [:466-473](../../R/functions/import/camtrapdp_functions.R#L466-L473) | **sequencing** prior obs by group + `timestamp` window |
| `sunrise, sunset, civil_dawn, civil_dusk, matutinal_end, diurnal_end` | `add_observation_time_classes()` **join** [daylight_functions.R:207-208](../../R/functions/daylight_functions.R#L207-L208) | **already in `environment_daily`**, joined by `(locationID, observation_date)` |
| `day_night_class, diel_class` | [daylight_functions.R:234-235](../../R/functions/daylight_functions.R#L234-L235) | `timestamp` time-of-day vs sun thresholds |
| `weathercode, temperature_2m_max, temperature_2m_min, precipitation_sum, weather_condition, weather_icon` | `add_observation_weather_fields()` **join** [weather_enrichment_functions.R:261-273](../../R/functions/import/weather_enrichment_functions.R#L261-L273) | **already in `environment_daily`**, joined by `(locationID, observation_date)` |
| *(removed)* `mediaID, cameraSetup, taxonIDReference, individualID, speed, radius, angle, _id, vernacularNames.nld` | [:416-421](../../R/functions/import/camtrapdp_functions.R#L416-L421) | dropped |

> **Key finding:** daylight and weather columns on `obs` are *literally* `left_join`s
> from `environment_daily` keyed on `(locationID, observation_date)`. They duplicate
> data that already lives, de-duplicated, in `environment_daily`. The only obs-derived
> fields not reducible to a key-join are `day_night_class`/`diel_class` (time-of-day)
> and `possible_duplicate` (sequencing).

### `trap_data$deps` / `trap_data$obs` — derived columns
`deps`: `interval_days`, `check_date`, `prior_check_date`, `period`, `locality`,
`locality_match_type`, `locality_distance_km`, `nearest_monitoring_locationName`.
`obs`: `eventID`, `eventStart`/`eventEnd` (date-only, currently equal),
`prior_check_date`, `check_interval`, `interval_days`, `source_prior_check_date`,
`source_interval_days`, `prior_check_override_applied`, `behavior`, `check_date`,
`period`, plus capture-derived `scientificName`/`count`/`sex`/`lifeStage`.

### `environment_daily` (already normalized — the model to follow)
Keyed `(locationID, date)`; holds `sunrise, sunset, civil_dawn, civil_dusk,
matutinal_end, diurnal_end` + `weathercode, temperature_2m_max, temperature_2m_min,
precipitation_sum, weather_condition, weather_icon`. **This is exactly the
join-on-demand pattern the canonical structure generalises.**

## 3. Point-in-time `timestamp` assumption catalogue
Sites that assume a single instant — each must move to `eventStart`/`eventEnd` or an
explicit in-app instant:
- `as.Date(core_data$obs$timestamp, …)` for period bounds — [period_group_functions.R:422-423](../../R/functions/period_group_functions.R#L422-L423).
- `filter_obs()`/`filter_deps()` period filtering (`timestamp >= start & <= end`) — `overview_functions.R`, `server.R:340-344`.
- Daylight: requires `timestamp` column — [daylight_functions.R:182](../../R/functions/daylight_functions.R#L182), `as.Date(timestamp)` [:193](../../R/functions/daylight_functions.R#L193).
- Weather: `as.Date(obs$timestamp)` — [weather_enrichment_functions.R:256](../../R/functions/import/weather_enrichment_functions.R#L256).
- Duplicate detection window on `timestamp` — [camtrapdp_functions.R:466-473](../../R/functions/import/camtrapdp_functions.R#L466-L473).
- Activity-pattern hourly binning from `timestamp` — `activity_patterns_module.R:94-100`.
- Trapping uses `check_date`/`eventStart` as a point — [period_group_functions.R:427-431](../../R/functions/period_group_functions.R#L427-L431), `monitoring_trapping_functions.R:168-186`.

## 4. Consumer matrix (usage-ordered)
| File | core_data | trap_data | Primary use |
|---|---|---|---|
| `R/modules/spatial_map_module.R` | 2 | 51 | trap/marker mapping |
| `R/modules/spatial_analysis_module.R` | 41 | 17 | obs/deps filtering, density |
| `R/modules/species_overview_module.R` | 35 | 19 | species summaries, diel, period |
| `R/functions/monitoring_trapping_functions.R` | 24 | 31 | RAI vs trapping metrics, windows |
| `R/functions/overview_functions.R` | 30 | 0 | deployment/detection summaries |
| `server.R` | 31 | 29 | period filtering, browse tables |
| `R/modules/overview_module.R` | 21 | 0 | overview metrics, locality |
| `R/functions/period_group_functions.R` | 12 | 8 | period bounds (timestamp/check_date) |
| `R/functions/global_functions.R` | 15 | 2 | runtime prep, image cache |
| `R/functions/weather_functions.R` | 16 | 0 | weather by locality/period |
| `ui.R` | 31 | 0 | period/species/locality choices |
| `R/modules/trapping_analysis_module.R` | 10 | 9 | trapping analysis UI |
| `R/modules/activity_patterns_module.R` | 4 | 0 | activity timeline (timestamp) |
| `R/server/server_observation_handlers.R` | 2 | 13 | observation/trap lookup + edit |

Totals ≈ **479 `core_data` refs / 25 files**, **359 `trap_data` refs / 16 files**.
