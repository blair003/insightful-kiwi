# `ik_data$app$species_groups` — curated "species we care about" registry

Field reference for the derived **species-groups** table. (`docs/app/` documents our
derived `ik_data$app$*` tables, as `docs/camtrapdp/` documents the upstream package
tables.) Built by [`resolve_species_groups()`](../../R/functions/species.R) at import and
cached; read with `ik_species_groups(ik_data)`.

## What it is
One row **per `scientificName`** for the species that matter to the project, resolving
`project.R`'s `species_groups` config against [`app$taxonomy`](taxonomy.md). It's the
**single source of truth** that says *not all species are equal* — unifying three uses
that v0.1 kept as separate, overlapping lists:
- **`role`** — ecological function (predator/protected/other); drives the co-occurrence
  metrics in [`app$relations`](relations.md).
- **`monitor`** / **`control`** — **per-method** prominence: monitoring and control care
  about different species, so a group is classed separately for cameras (`monitor`:
  target/interesting) and traps (`control`: target). The Overview camera panel shows the
  `monitor` groups as a fixed scorecard (zeros included); the trap panel shows the
  `control` target groups, then **all other species actually caught** (data-driven), so
  bycatch (birds, rabbits) appears and monitoring-only species (kiwi) don't sit at 0.
- **`group`** + **`label`** — the combined reporting/RAI unit (e.g. the three *Mustela*
  species → one "Mustelids").

Species matching no group are **unclassified** ("other") and simply absent from this table.

## Fields
| Field | Type | Description |
|---|---|---|
| `scientificName` | chr | The taxon (join key to taxonomy/observations). One row per matched taxon, at whatever rank taxonomy holds it (genus `Rattus`, subspecies `Mustela putorius furo`, …). |
| `group` | chr | The group key (`mustelid`, `rat`, `cat`, `kiwi`, `possum`, …). |
| `label` | chr | Display / reporting name for the group (`"Mustelids"`, `"Rats"`, `"Kiwi"`, …). |
| `role` | chr | `predator` / `protected` / `other`. Only `predator` & `protected` participate in `app$relations` co-occurrence. |
| `monitor` | chr | Camera-monitoring prominence — `target` / `interesting` / `NA` (open). |
| `control` | chr | Trapping/control prominence — `target` / `NA`. |
| `priority` | int | Group order from `project.R` (lower = higher interest; mustelid = 1). Drives display ordering. |

## How matching works (`resolve_species_groups`)
Each group declares matchers; a taxon joins it if **any** hit:
- `scientificName` — exact membership in the listed names;
- `family` — `taxonomy$family` in the listed families, **or the family taxon itself** (a
  family-level ID like `Mustelidae`, whose own `family` column is `NA` in GBIF, still folds
  into its group — e.g. a trapper's generic "Mustelid" catch joins Mustelids);
- `genus` — the **first word of `scientificName`** in the listed genera (robust to a
  missing `family` — e.g. the ferret `Mustela putorius furo`, whose `family` is `NA` in
  GBIF; see the `family` note in [taxonomy.md](taxonomy.md)).

Groups are evaluated in `project.R` order (= `priority`); the **first group to match wins**,
so each `scientificName` appears once. Editing `project.R` is in the cache fingerprint, so
changing the registry re-derives this table (and `app$relations`) on the next build.

## Used by
- [`app$relations`](relations.md) — predator/protected co-occurrence columns are computed
  against observations tagged by `role` here.
- **Overview** — the project summary groups detections by `group`/`label` and floats them
  by `class` then `priority`.
- **Future RAI** — `group`/`label` are the natural combined-RAI reporting unit.
