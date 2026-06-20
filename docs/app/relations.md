# `ik_data$app$relations` — observation timing & co-occurrence

Field reference for the derived **observation-relations** table. (`docs/app/`
documents our derived `ik_data$app$*` tables, as `docs/camtrapdp/` documents the
upstream package tables.) Built by
[`build_observation_relations()`](../../R/functions/observation_relations.R) at
import and cached; read with `ik_relations(ik_data)`.

## What it is
One row **per observation**, holding temporal metrics that relate an observation to
*other* observations — without widening the pristine packages. Join it to
observations on demand by `observationID` (left join — observations absent here get
`NA`).

**Coverage.** Only **minute-resolution, species-bearing** observations are included:
- date-only sources are excluded by resolution (their deployment's Camtrap DP
  `timestampIssues = TRUE`) — currently the trap data; if trap timestamps ever gain
  minute precision they are included automatically;
- blank/unclassified observations (no `scientificName`) are excluded.

So today this is the camera data only.

**Time basis.** The ordering instant is the observation's `eventStart`; all metrics
are **durations in minutes**, which are timezone-invariant. "Same location" means
the same deployment `locationID`. All metrics are computed **within a dataset**
(no cross-dataset co-occurrence yet).

**Configuration** ([`instance/config/project.R`](../../instance/config/project.R)):
`species_groups` defines predator/protected roles; `duplicate_window` sets the
same-species duplicate threshold. Editing either re-derives this table.

## Fields

### Identity
| Field | Type | Description |
|---|---|---|
| `observationID` | chr | The observation (join key to `observations()`). |
| `dataset` | chr | Source dataset id (e.g. `wkt_camera_monitoring`). |

### Same-species timing & duplicates
| Field | Type | Description |
|---|---|---|
| `minutes_since_prev_same_species` | num (min) | Minutes from the **previous observation of the same `scientificName` at the same `locationID`** (ordered by `eventStart`) to this one. `NA` for the first such observation at the location. This is the neutral primitive — a metric, not a verdict. |
| `possible_duplicate` | lgl | `TRUE` when `minutes_since_prev_same_species` is within the species' duplicate window — `duplicate_window$by_species[[scientificName]]`, else `duplicate_window$default` (project.R). `FALSE` otherwise (including first-of-group). A coarse "same group lingering / drifting in and out" flag; refine the window per species in config. |

> **Consumers.** Surfaced as a hidden **Records** column (the data is never dropped, so
> "how many were duplicates?" stays answerable), and used by **net RAI**
> ([`R/functions/metrics.R`](../../R/functions/metrics.R)) when `project.R$rai$use_net` —
> the metric excludes flagged detections; the records keep them.

### Co-occurrence scope & method
**No time window — these span the WHOLE dataset.** For each observation the metric finds the
*nearest* predator/protected observation at the same location across the entire record, with
**no cap on how far apart in time**. A `*_before` value of, say, *1 yr 192 days* is not an
error: it means that camera simply had not recorded a predator/protected animal for that long
before this detection. So a small `*_after_min` (e.g. *43 min*) is a genuine co-occurrence,
while a huge `*_before_min` just marks a long gap. The values are computed **once at import
over all data and joined by `observationID`** — they are *independent of the selected period*,
so an observation viewed within "Autumn 2026" can legitimately point at a predator months
earlier or later. To make them *mean* co-occurrence, apply a window in analysis (e.g. predator
within 24 h). The observation viewer shows them as **readable durations** ("96 days 23 hr")
labelled *"Nearest predator before/after"* to set that expectation.

Method (per location): observations are sorted by `eventStart`; `findInterval()` locates the
nearest role observation strictly **before** (`left.open = TRUE`) and **at/after**
(`left.open = FALSE`, +1) each row, so a subject never matches itself or an exact tie. Minute
resolution; date-only (trap) deployments are excluded by resolution, not by source.

### Predator co-occurrence (same location)
The nearest **predator** observation before and after this observation, at the same
location — **any** predator species (the observation itself is excluded). This is a
neutral primitive: to restrict to a *different* species, filter in analysis on
`predator_before_species != scientificName` (just as `possible_duplicate` is derived
from a metric, not baked in). Both `*_min` values are **positive**; *before*/*after*
gives the direction.

| Field | Type | Description |
|---|---|---|
| `predator_before_min` | num (min) | Minutes since the nearest predator seen **before** this observation here. `NA` if none precedes. |
| `predator_before_species` | chr | That predator's `scientificName`. |
| `predator_before_group` | chr | That predator's role group (`mustelid`/`rat`/`cat`/`dog`). |
| `predator_after_min` | num (min) | Minutes until the nearest predator seen **after** this observation here. `NA` if none follows. |
| `predator_after_species` | chr | That predator's `scientificName`. |
| `predator_after_group` | chr | That predator's role group. |

### Protected co-occurrence (same location)
The nearest **protected** observation before and after, same location — any protected
species, self excluded. (No `*_group` — protected is a single group.) For WKT, kiwi is
the only protected species, so for a kiwi subject these point at the nearest *other*
kiwi sighting (≈ `minutes_since_prev_same_species`), and for a predator subject they
give the nearest kiwi before/after — the stalking signal.

| Field | Type | Description |
|---|---|---|
| `protected_before_min` | num (min) | Minutes since the nearest protected species seen **before** this observation here. `NA` if none. |
| `protected_before_species` | chr | That observation's `scientificName`. |
| `protected_after_min` | num (min) | Minutes until the nearest protected species seen **after** this observation here. `NA` if none. |
| `protected_after_species` | chr | That observation's `scientificName`. |

> **Stalking analysis:** filter to kiwi rows and read `predator_before_*` /
> `predator_after_*` to see how close in time a predator was at that camera; or
> filter to predator rows and read `protected_*`.

## Related: `ik_data$app$species_groups`
The role lookup these metrics use, resolved from `project.R$species_groups` against
`app$taxonomy` (`ik_species_groups(ik_data)`) — full reference in
[species_groups.md](species_groups.md):

| Field | Type | Description |
|---|---|---|
| `scientificName` | chr | The taxon. |
| `role` | chr | `predator` / `protected` / `other` (only the first two co-occur here). |
| `group` | chr | Group key (`mustelid`/`rat`/`cat`/`dog`/`kiwi`/…). |
| `priority` | int | Registry order (lower = higher interest; mustelid = 1). |

(Plus `label`/`class` for reporting — see [species_groups.md](species_groups.md).) Matching
is by `scientificName`, `family`, **or** `genus` (first word of the scientificName) — the
genus fallback catches taxa whose `family` didn't resolve (e.g. `Mustela putorius furo`).
