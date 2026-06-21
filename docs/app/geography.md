# `ik_data$app$geography` — canonical place hierarchy

Field reference for the derived **geography** structure. (`docs/app/` documents our
derived `ik_data$app$*` tables, as `docs/camtrapdp/` documents the upstream package
tables.) Built by [`build_geography()`](../../R/functions/geography.R) at import and
cached; read with `ik_geography(ik_data)`.

## What it is
The shared place model that puts every dataset's locations into one canonical
hierarchy so the app can group and filter spatially across sources. `ik_geography()`
returns a **list** with three elements:

| Element | What it is |
|---|---|
| `levels` | The canonical hierarchy, coarsening order: `c("location", "line", "reserve", "global")`. |
| `locations` | One row **per location** (all datasets unioned), with its place attributes — the table documented below. |
| `reserve_hulls` | Named list of `sf` polygons (WGS84), one convex hull per reserve, built from the canonical dataset's points. Used for maps and for the spatial reserve assignment. |

`line` and `reserve` are **independent** attributes of a location, not a strict tree —
a location has a line and (separately) a reserve. Each dataset names a registered
**deriver** (manifest `geography$derive`) that produces what it can from its own data
(camera → line + canonical reserve; trap → line only). A dataset whose reserve isn't
in its own data declares a `reserve_match` strategy that assigns the reserve by
**spatially** matching to another dataset's canonical reserves — the cross-dataset
reconciliation, done here in the app layer, never in a converter.

## `locations` fields
| Field | Type | Description |
|---|---|---|
| `location_id` | chr | The location identifier, taken straight from the package's `locationID` and globally unique already (cameras use Agouti hashes; converters namespace theirs, e.g. `wkt_trapping_SS24`). The join key to `deployments()`/`observations()`. |
| `dataset` | chr | Source dataset id (`wkt_core_monitoring`, `wkt_trapping`). |
| `name` | chr | The location's `locationName` (e.g. `OH 1_4`, `SS24`). |
| `latitude` | num | WGS84 latitude (`NA` if the source had no coordinates). |
| `longitude` | num | WGS84 longitude (`NA` if the source had no coordinates). |
| `line` | chr | Monitoring/trap line this location belongs to, from the deriver. A bare value (camera `"1"`, not `"OH 1"`; trap line names from `deploymentGroups`). `NA` if undetermined. |
| `reserve` | chr | Canonical reserve name (`Ohope`, `Kohi Point`, `Mokoroa`). Self-derived for the canonical dataset (camera, via the manifest `reserves` code map); **assigned spatially** for matched datasets (trap). |
| `within_monitored_area` | lgl | Whether the location falls inside a (buffered) reserve hull. `TRUE` by definition for a canonical dataset that supplies its own reserve; computed by containment for spatially-matched datasets. |
| `nearest_monitoring_location` | chr | For spatially-matched datasets, the `name` of the nearest **canonical** (camera) location. `NA` for the canonical dataset itself. |
| `nearest_monitoring_distance_km` | num | Distance in km to that nearest canonical location (point-to-point, not to a hull edge), rounded to 3 dp. `NA` for the canonical dataset. |

## How the two passes fill it
1. **Self-derive** (`build_geo_base`) — start from each dataset's `locations()`; run its
   configured deriver to set `line` and (if it has one) `reserve`. A canonical dataset
   that isn't spatially matched gets `within_monitored_area = !is.na(reserve)`.
2. **Spatial reserve assignment** (`assign_reserves_spatial`) — for any dataset whose
   manifest declares `reserve_match = list(strategy = "spatial_hull", canonical = …,
   buffer_m = …)`: transform both point sets to NZTM (EPSG:2193), build convex hulls
   per canonical reserve, and for each target location record the nearest canonical
   location/distance (always) and `reserve`/`within_monitored_area` by containment in
   the buffered hull (falling back to the nearest reserve when outside every hull). The
   hulls are returned (re-projected to WGS84) as `reserve_hulls`.

## Notes
- **Casing.** Computed-by-us columns are snake_case (`location_id`,
  `within_monitored_area`) to distinguish them from camtrapdp package fields
  (camelCase: `locationID`, `eventStart`). Contrast [taxonomy.md](taxonomy.md), whose
  columns are passed through from `taxa()`.
- **No separate source id.** `location_id` *is* the package `locationID`; converters
  namespace theirs at conversion time, so nothing extra is stored to keep them unique.
- **Coordinate-less locations** carry `NA` lat/long and are skipped by the spatial
  pass (they can't be hull-tested or distance-ranked).
