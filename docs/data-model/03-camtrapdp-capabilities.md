# 03 — camtrapdp Capabilities & Stage-1 Spike

> Supporting doc. What the INBO `camtrapdp` package gives us, how it maps to the
> canonical structure ([01-data-structure.md](01-data-structure.md)), the
> `camtraptor → camtrapdp` delta, and the **spike** that gates the
> "retain the object" decision. Spike *plan* is specified now; *results* are filled
> in during Stage 1 of the rebuild.

## 1. Why camtrapdp (not camtraptor)

| | `camtraptor` (current, 0.28.0) | `camtrapdp` (target, not yet installed) |
|---|---|---|
| Model | converts modern Camtrap DP → **legacy** | reads modern Camtrap DP **1.0 natively** |
| `timestamp` | **synthesizes** it from `eventStart` | none — keeps `eventStart`/`eventEnd` |
| Status | superseded | maintained successor |

Our source (`instance/extdata/monitoring-data/observations.csv`) is already modern
Camtrap DP 1.0 (`eventStart, eventEnd, observationLevel, observationType,
cameraSetupType, …`, **no `timestamp`**). `camtrapdp` reads it without the legacy
round-trip — removing the exact behaviour we want gone. `frictionless` (1.2.1,
installed) is the lower-level fallback.

## 2. Exported functions (confirmed) and how we use them

| Function | Use in our design |
|---|---|
| `read_camtrapdp()` | **the shared reader** for both monitoring and the generated trapping package |
| `observations()` / `deployments()` / `media()` | feed filtered/native tables to the access helpers |
| `locations()` / `taxa()` / `individuals()` / `contributors()` | locations + taxonomy; metadata |
| `filter_observations()` / `filter_media()` / `filter_deployments()` | referential-integrity filtering (filter-first step) |
| `update_taxon()` | **replace** `consol_spp_obs`/`consol_taxa` species/taxon consolidation — *if* it expresses `config$globals$spp_consol_defs` |
| `merge_camtrapdp()` | candidate for the on-demand union of monitoring + trapping |
| `write_camtrapdp()` + `check_camtrapdp()` | **emit and validate** the generated trapping package |
| `shift_time()` | timezone/offset handling if needed |
| `round_coordinates()` | location generalisation (not required) |
| `write_dwc()` | free future capability (Darwin Core export) — noted, not built |

Not provided (confirmed): an `events()` extractor or any `eventStart`/`eventEnd`/
`observationLevel` helper. The "event model" is simply the observations table with
its native interval columns — we compute precision/derivations ourselves.

## 3. Stage-1 spike (gates the "retain object" decision)

Run on branch `v2-data-model` after installing `camtrapdp`, against the real
monitoring package and a generated trapping package. **No production code changes.**

| # | Question | How to test | Result |
|---|---|---|---|
| S1 | Object class & shape; how to get plain tables | `class()`, `observations()`, `deployments()` | _TBD_ |
| S2 | **Memory:** object footprint vs extracted data frames | `lobstr::obj_size()` / `pryr` on object vs `as.data.frame` of each table | _TBD_ |
| S3 | `filter_*` correctness + performance on our row counts | filter by deployment/location/date; time it; check referential integrity | _TBD_ |
| S4 | Does `update_taxon()` express `spp_consol_defs`? | apply each consolidation def; diff vs current `consol_spp_obs` output | _TBD_ |
| S5 | Does `merge_camtrapdp()` preserve what the union needs? | merge monitoring + a generated trapping pkg; inspect observations | _TBD_ |
| S6 | Does `check_camtrapdp()` accept our generated trapping package? | `write_camtrapdp()` the converter output → `check_camtrapdp()` | _TBD_ |
| S7 | Can the object carry/round-trip anything custom, if ever needed? | attempt extra column on a table via accessor setter; re-read | _TBD_ |
| S8 | Cross-dataset **location reconciliation** — how `merge_camtrapdp()` treats overlapping locations | merge two packages sharing a physical site; inspect `locations()`/`locationID`s | _TBD_ |

**Decision rule:** if S1-S3 show the object is memory-comparable to data frames and
`filter_*` is performant, **retain the object** (the design's lead). If not,
**fall back** to extracting plain data frames per dataset (each dataset's
`observations`/`deployments`/…) and use `camtrapdp` only for read + `update_taxon`
+ `write/check` + `merge`. Either way the `app` normalized layer
([01](01-data-structure.md)) is unchanged. S8 informs the **canonical location
registry** (`app$locations`) — `merge_camtrapdp()` concatenates rather than
reconciles identity, so cross-dataset location matching stays our responsibility.

## 4. camtraptor → camtrapdp field delta (to characterise in Stage 1)

Confirm the native `camtrapdp` read still delivers every field the app relies on
today, and record the differences:
- `timestamp` — **intentionally gone**; consumers move to `eventStart` (see the
  point-in-time catalogue in [02 §3](02-current-state-inventory.md)).
- `observationLevel` (`media` vs `event`) — confirm which level the app's
  detections use and that `camtrapdp` exposes it the same way.
- `vernacularNames.*` — confirm column naming/shape vs legacy flattening.
- `taxa()` / taxonomy table shape vs the current `taxonomic` list.
- media linkage (`media()` ↔ observations) for the image cache
  (`cache_selected_images`, `global_functions.R`).
- any column the current pipeline renames/drops ([02 §2](02-current-state-inventory.md)).

Output: a checklist of fields/behaviours the new reader path must still deliver,
verified against a Stage-0 golden snapshot of current `core_data`.

## 5. Dependencies
- **Add:** `camtrapdp` (CRAN). Installs allowed for this refactor (confirmed with
  user); update `AGENTS.md`'s package note in the later rebuild pass.
- **Keep available during transition:** `frictionless` 1.2.1 (fallback reader).
- **Remove at end:** `camtraptor` 0.28.0 (legacy reader), once monitoring import is
  migrated.
