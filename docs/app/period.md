# `ik_data$app$period` — temporal segmentation substrate

Field reference for the derived **period** structure. (`docs/app/` documents our derived
`ik_data$app$*` tables, as `docs/camtrapdp/` documents the upstream package tables.) Built
by [`build_period()`](../../R/functions/period.R) at import and cached; read with
`ik_period(ik_data)`. The *model* this serves — deployment-first selection, the axes, the
season conventions — is in [`docs/data-model/04-data-selection.md`](../data-model/04-data-selection.md).

## What it is
The substrate every period selection hangs off, derived once at import (joined on demand,
never widened onto the pristine packages). `ik_period()` returns a **list** of two tables:

| Element | Accessor | What it is |
|---|---|---|
| `deployments` | `ik_deployment_period()` | one row **per deployment** with its assigned season + effort |
| `observations` | `ik_observation_period()` | one row **per observation**, seasoned by its **own** event date |
| `monitoring_season` | `ik_monitoring_season()` | one row **per `(season × reserve × source_type)`** — the empirical monitored envelope |

**Season basis.** Southern-hemisphere 3-month bins (Summer Dec–Feb, Autumn Mar–May,
Winter Jun–Aug, Spring Sep–Nov), computed on **localised** (`Pacific/Auckland`) times
because boundaries are local wall-clock. Summer is labelled **split-year** ("Summer
2023/24"). The **monitoring-year** rollup is deliberately *not* built here (deferred to
project config — see the model doc).

## `deployments` fields
Keyed by `deploymentID`; join to `ik_deployments()` / observations on demand.

| Field | Type | Description |
|---|---|---|
| `deploymentID` | chr | The deployment (join key). |
| `dataset` | chr | Source dataset id (e.g. `wkt_core_monitoring`). |
| `source_type` | chr | Method: `camera`, `trap`, … (provenance tag). |
| `locationID` | chr | The deployment's location (join key to `app$geography`). |
| `reserve` | chr | Canonical reserve, from `app$geography$locations`. |
| `deploymentStart` | POSIXct | Localised start. |
| `deploymentEnd` | POSIXct | Localised end. |
| `effort_hours` | num | Whole-deployment duration in hours (`end − start`). The *raw* per-deployment effort; for an in-season denominator use `monitoring_season$effort_hours` instead. |
| `season` | chr | `Summer` / `Autumn` / `Winter` / `Spring`. |
| `season_year` | int | Anchor year (summer anchors to its December year, so Jan/Feb fall under the previous year's summer). |
| `calendar_season` | chr | Display label: `"Summer 2023/24"` (split-year) or `"Autumn 2024"`. The single season assigned by **max temporal overlap**. |

> **One season per deployment** is the "majority of the period" rule — meaningful for
> *pulse* monitoring (cameras sit inside one season). For *continuous* sources (traps
> spanning several seasons) it's the **dominant** season only; the correct per-season view
> is `monitoring_season`, not this column.

## `observations` fields
Keyed by `observationID`. An observation is seasoned by **its own `eventEnd`** — the
recorded instant (camera detection end; trap **check date**), matching the Records
"When" column — **not** its deployment's season. So for a continuous trap whose
deployment straddles seasons, a catch checked in May is Autumn and one checked in July
is Winter. This is the period filter the selection resolver uses; it lines the numerator
(captures-in-season) up with the `monitoring_season` denominator (effort-in-season).

> **Resolution limitation — low-temporal-resolution sources.** This is general, not a
> trapping quirk; expect it from any source recorded by **periodic checks** rather than
> continuous sensing — predator **traps**, **tracking tunnels** / rodent monitoring, bait
> stations. Such a source only resolves an event to its **check interval**
> `[eventStart, eventEnd]`, never an instant: it could have occurred any time after the
> previous check, even in a *prior* season. We attribute it to the **check date**
> (`eventEnd`) — what was recorded — the standard convention.
>
> **The coarser the time bucket, the more this evens out.** At **season** or **year**
> granularity the interval uncertainty mostly averages away and the attribution is sound;
> at fine granularity (e.g. **daily**) it dominates. So low-resolution sources support
> seasonal/annual analysis but **not** accurate daily series — unlike cameras, which sense
> continuously (`eventStart ≈ eventEnd`) and *are* daily-accurate. For exactly how a given
> source sets these fields, see its converter (e.g.
> [`wkt_trapping.R`](../../R/functions/import/converters/wkt_trapping.R)).

| Field | Type | Description |
|---|---|---|
| `observationID` | chr | The observation (join key). |
| `dataset` | chr | Source dataset id. |
| `season` | chr | `Summer` / `Autumn` / `Winter` / `Spring` of the event. |
| `season_year` | int | Anchor year (summer anchors to its December year). |
| `calendar_season` | chr | Split-year label, e.g. `"Summer 2023/24"` / `"Autumn 2024"`. |

## `monitoring_season` fields
One row per `(calendar_season × reserve × source_type)`. Built by **clipping every
deployment to each season it overlaps** — so a camera pulse shows its true ~3-week window,
while continuous trapping shows the in-season portion of an ongoing run (bounded by the
season).

| Field | Type | Description |
|---|---|---|
| `calendar_season` | chr | Season label (e.g. `"Autumn 2024"`). |
| `season` | chr | Season name. |
| `season_year` | int | Anchor year. |
| `reserve` | chr | Canonical reserve. |
| `source_type` | chr | Method. |
| `start` | POSIXct | Earliest in-season monitoring instant for this cell (clipped to the season). |
| `end` | POSIXct | Latest in-season monitoring instant for this cell (clipped to the season). |
| `n_deployments` | int | Deployments **active during** this season at this reserve/source (a deployment spanning N seasons counts in each). |
| `effort_hours` | num | **In-season** (clipped) effort hours — the correct denominator for seasonal rates. |

The window genuinely **varies by reserve and source** — e.g. Autumn 2024 cameras: Ōhope
Apr 10–May 1 vs Kohi Point / Mokoroa May 3–24; Autumn 2024 traps: Mar 1–Jun 1 (continuous,
so the whole season). This is empirical, derived from the deployments — no configured
window to maintain.

## Notes
- **Casing.** Derived-layer columns are snake_case (`calendar_season`, `effort_hours`,
  `source_type`); the upstream camtrapdp fields stay camelCase (`deploymentID`,
  `deploymentStart`). See also [geography.md](geography.md).
- **Effort: raw vs in-season.** `deployments$effort_hours` is the full deployment length;
  `monitoring_season$effort_hours` is clipped to the season. Use the latter for any
  per-season rate (e.g. RAI normalised to 2000 hours).
