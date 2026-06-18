# 01 — Canonical Data Structure (PRIMARY)

> The target data structure for the monitoring/trapping modernisation. This is the
> single primary deliverable of the design phase. Reader implementation, trap→Camtrap
> DP conversion, and consumer migration are **deferred** to a follow-up pass once
> this structure is signed off. Background + current state:
> [02-current-state-inventory.md](02-current-state-inventory.md). camtrapdp evidence:
> [03-camtrapdp-capabilities.md](03-camtrapdp-capabilities.md).

## Why this design

Two forces shape it:

1. **Event-centric time.** Observations are events spanning `eventStart`→`eventEnd`,
   not single instants. Camera detections are short intervals (often
   `eventStart == eventEnd`); trap events span `prior_check_date`→`check_date`
   (days–weeks). There is **no `timestamp` field**; a single instant is computed
   in-app only where needed.
2. **Memory.** Today enhancement *widens the observations fact table* with ~20
   derived columns (period/season/year, species class, six daylight datetimes, six
   weather columns, …). On 100k+ rows that denormalization is a primary memory
   driver. Six of those columns are *literally* `left_join`s from `environment_daily`
   ([02 §2](02-current-state-inventory.md)) — duplicated data we already store once.

**Principle: lean fact tables + normalized lookup/dimension tables, joined on
demand.** Keep the Camtrap DP tables native and pristine; hold every app-derived
attribute once, in `ik_data$app`, joined only after filtering.

## The structure

```
ik_data
├─ $datasets     named list of camtrapdp objects — one per imported package,
│                  each pristine Camtrap DP (deployments, media, observations,
│                  taxa, locations, contributors, metadata) with tag metadata:
│                  { id, source_type (camera|trap|bait_station|…), method, area }
│                  only package-native transforms applied
│     ├─ <id-1>   e.g. ohiwa_cameras    (source_type = camera)
│     ├─ <id-2>   e.g. wkt_traps_2024   (source_type = trap)
│     └─ <id-n>   e.g. poison_lines     (source_type = bait_station)
└─ $app          normalized derived layer — the ONLY home for app-derived data
   ├─ species              scientificName -> species_class, species_rank
   ├─ period_groups        period definitions (season/year/period computed on demand)
   ├─ monitoring_period_groups
   ├─ environment_daily    (canonical_locationID, date) -> sun times + weather [join]
   ├─ locations            canonical location registry: per-package (dataset,
   │                         locationID) -> canonical location (locality, line,
   │                         region, lat/long)
   ├─ possible_duplicates  sparse set of flagged (dataset, observationID)
   ├─ taxonomic            combined/merged taxonomy across all datasets
   └─ app                  metadata (build dates, spp_classes, diel thresholds, …)
```

### `$datasets` — a flat collection of tagged `camtrapdp` objects
Each imported package is **one** `camtrapdp` object, keyed by a meaningful `id` and
**tagged** (not name-prefixed) with `source_type` / `method` / `area` metadata. This
natively supports **multiple packages per type** and **new source types** with zero
structural change. The app reads/filters each through the package
([03](03-camtrapdp-capabilities.md)): `read_camtrapdp()`, `observations()`,
`deployments()`, `media()`, `filter_*`, `update_taxon()`, `merge_camtrapdp()`,
`write_camtrapdp()`/`check_camtrapdp()`.

**Each object stays pristine — no custom columns are ever added.** Only
package-native transforms touch it (species/taxon consolidation via `update_taxon()`,
filtering, merge). This keeps fact tables lean and makes the package the single
source of truth for Camtrap DP data. *(Object retention is gated by the Stage-1
spike — see [03 §3](03-camtrapdp-capabilities.md); fallback is to extract plain data
frames per dataset.)*

### Source types & extensibility
Camera monitoring, trapping, and bait/poison stations are the **same Camtrap DP
shape** — a *deployment* is a device at a location for a period; an *observation* is
an event with `eventStart`/`eventEnd`:

| Real thing | deployment | observation/event | `source_type` |
|---|---|---|---|
| Camera at a site | location + deploymentStart/End | detection | `camera` |
| Trap check interval | location + prior_check→check | capture / bait-taken / still-set | `trap` |
| Bait/poison station | location + deploy date→end | bait deployed / taken / refilled | `bait_station` |

So a new source like **poison data is not a new category** — it is another
`source_type` whose custom CSV is converted into a Camtrap DP package
(`write_camtrapdp()` → `check_camtrapdp()` → `read_camtrapdp()`), exactly like
trapping. `source_type` is an **open vocabulary**; the structure never hardcodes a
fixed set of buckets.

Interval/date-only events (trap, bait_station) write `eventStart`/`eventEnd` derived
from their check/deploy dates as local-midnight datetimes carrying
`timestampIssues = "dateOnly"` + an explicit precision marker, so `00:00:00` is never
read as a real time. Daylight/diel enrichment is N/A for these.

### The `app` derived layer
Each table is keyed so it joins cleanly onto a filtered observation/deployment
subset. `environment_daily` already exists in exactly this shape and is the model the
rest follow.

**`app$locations` is a canonical location registry — the hard cross-dataset
problem.** Different packages carry their own `locationID`s; the same physical place
may appear across a camera package, a trap package, and a poison package, and
`merge_camtrapdp()` will *concatenate* locations without reconciling identity. So
`app$locations` maps each `(dataset, locationID)` to a **canonical location**
(locality / line / region / coords), built by spatial reconciliation. Precedent
exists: the trap import already spatially matches traps to monitoring localities
([trap_data_conversion_functions.R:248-390](../../R/functions/import/trap_data_conversion_functions.R#L248-L390)).
This is a **named open problem** (see Open decisions), not solved here.

## Field classification — every current derived field has a home

Accounts for 100% of the derived columns enumerated in [02 §2](02-current-state-inventory.md).

| Current derived field(s) | Function of | New home | Access |
|---|---|---|---|
| `locality`, `line` | `locationID` | `app$locations` (canonical registry) | join on `(dataset, locationID)` |
| `locationName`, `latitude`, `longitude` | deployment | native object `deployments()`/`locations()` | accessor |
| `monitoring_period` | `deploymentStart` + defs | compute on demand | from `app$monitoring_period_groups` |
| `calendar_season`, `calendar_year`, `period` | `eventStart`/`deploymentStart` + defs | compute on demand | from `app$period_groups` |
| `species_class`, `species_rank` | `scientificName` | `app$species` | join on `scientificName` |
| `scientificName_lower`, `observation_date` | trivial | dropped | compute inline |
| `sunrise`, `sunset`, `civil_dawn`, `civil_dusk`, `matutinal_end`, `diurnal_end` | `(locationID, date)` | `app$environment_daily` | join on `(locationID, date)` |
| `weathercode`, `temperature_2m_max`, `temperature_2m_min`, `precipitation_sum`, `weather_condition`, `weather_icon` | `(locationID, date)` | `app$environment_daily` | join on `(locationID, date)` |
| `day_night_class`, `diel_class` | `eventStart` time vs sun thresholds | compute on demand | on filtered subset; N/A for traps |
| `possible_duplicate` | sequencing prior obs by location | `app$possible_duplicates` (sparse IDs) | membership test on `(dataset, observationID)` |
| `<observationType>_detections_count`, `camera_hours` | per-deployment aggregates | compute on demand | aggregate from observations |

**`possible_duplicate`** is the only genuinely per-observation derived datum. Stored
as a sparse set of flagged `observationID`s (typically a small fraction of rows),
recomputed at import from the native observations and reattached at read — versus a
TRUE/FALSE on every one of 100k+ rows today.

## Access patterns

The contract for every consumer: **select datasets → filter → enrich the subset.**

1. **Select** datasets by tag — `ik_datasets(ik_data, source_type = "camera")` (or
   `"trap"`, multiple, or all) — so reporting asks for "the camera data" without the
   structure hardcoding types.
2. **Filter** the selected datasets' native observations via `camtrapdp::filter_*`
   (or by `eventStart`/`eventEnd` interval overlap and `locationID`). Filtering by
   period/locality shrinks rows *before* any join.
3. **Enrich** the filtered subset by `left_join`ing only the `app` lookups the view
   needs: `app$locations` (canonical location → locality/line), `app$species`
   (class/rank), `app$environment_daily` (daylight/weather), period computed from
   `app$period_groups`.
4. **Compute** `day_night`/`diel` on the enriched subset from `eventStart` vs the
   joined sun times (skip for interval/date-only sources).
5. **Flag** duplicates via membership in `app$possible_duplicates`.

This is lower-memory *and* faster than today's enrich-everything-upfront: joins run
over the filtered subset, not the full fact table.

### Helper wrappers
Provide a small, named set of helpers so no consumer hand-rolls dataset selection or
joins, e.g.:
- `ik_datasets(ik_data, source_type = …, id = …, area = …)` → the matching
  `camtrapdp` objects.
- `ik_observations(ik_data, source_type = …, period = …, locality = …, enrich = c("location","species","environment"))`
  → filtered + enriched tibble, unioned across the selected datasets.
- `ik_deployments(ik_data, …)` → the deployment equivalent (camera-hours,
  trap-days, station-days computed on demand).

Cross-dataset union reconciles columns present in some datasets but not others
(candidate impl: `merge_camtrapdp()` if custom-data safe, else a column-reconciling
`bind_rows`), and resolves locations through `app$locations`. These wrappers are the
**only** place dataset-selection and join logic lives; the ~840 current
`core_data`/`trap_data` field reads migrate onto them.

## Memory rationale (to validate)
The fact table drops from ~native + ~20 derived columns to native only. Each derived
attribute is stored once in a small lookup (`environment_daily` rows ≈ locations ×
days; `species` ≈ species count; `possible_duplicates` ≈ flagged-only) instead of
repeated across every observation. [03 §3](03-camtrapdp-capabilities.md) records the
measured before/after footprint.

## Open decisions (resolve before the follow-up rebuild pass)
- **Canonical location reconciliation (hard):** how `app$locations` maps each
  `(dataset, locationID)` to a canonical location across packages — match by
  coordinates/proximity, name, or explicit mapping; conflict handling; how
  `merge_camtrapdp()` interacts. The cross-dataset identity problem.
- **Dataset identity & tagging:** id scheme (from `datapackage.json` `name`/`id` vs
  app-assigned), where `source_type`/`method`/`area` tags live (attributes vs a
  registry table), and the `source_type` open vocabulary.
- **Spike verdict:** retain `camtrapdp` objects vs extract data frames (memory,
  `filter_*` performance, whether anything needs custom columns).
- `locality`/`line`: cache in `app$locations` vs compute on demand from
  `locationName`.
- `possible_duplicates`: sparse ID set vs keyed tibble; exact recompute-on-import +
  reattach-at-read mechanism.
- Period/season/year: pure compute-on-demand vs a cached date→period lookup.
- `tibble` vs `data.table` (keyed joins) for `app` lookups, given memory is primary.
- Combined-taxonomy merge keys / conflict handling (lean: per-package `taxa()` +
  merged `app$taxonomic`).
- Union impl: `merge_camtrapdp()` vs hand-rolled `bind_rows` reconciler.

## Verification of this design
- **Completeness:** the table above homes 100% of [02 §2](02-current-state-inventory.md)'s
  derived columns — none unhomed. ✔ to confirm at review.
- **Memory sanity check:** estimate fact-table footprint wide (today) vs lean.
- **Access walkthrough:** for species-overview (period+locality), spatial-density,
  and trapping-mismatch, show the filter→join sequence resolves every needed field.
- **Spike sign-off:** `camtrapdp` object supports the access patterns + memory
  target, or document the data-frame fallback.
