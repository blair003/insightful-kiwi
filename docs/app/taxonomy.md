# `ik_data$app$taxonomy` — shared species lookup

Field reference for the derived **taxonomy** table. (`docs/app/` documents our
derived `ik_data$app$*` tables, as `docs/camtrapdp/` documents the upstream package
tables.) Built by
[`build_taxonomy()`](../../R/functions/import/build_taxonomy.R) at import and cached;
read with `ik_taxonomy(ik_data)`.

## What it is
One row **per `scientificName`**, the shared reference that gives a species the same
meaning across datasets. It is `$app` (not widened onto the fact tables): observations
carry only `scientificName` and join here on demand. Its main job is the
scientific ↔ English-vernacular mapping behind
[`ik_species_label()`](../../R/functions/taxonomy.R), which honours the Settings
"Prefer scientific names" toggle and falls back to the other name when the preferred
one is missing.

**How it's built.** Every dataset's native `camtrapdp::taxa()` is unioned and deduped
by `scientificName`. The row carrying an English vernacular name sorts first, so the
informative row wins the dedupe. Rows can sit at **any rank** — the camera data
consolidates some taxa to genus (e.g. `Rattus`), so a "species" lookup legitimately
holds `kingdom`/`order`/`genus`/`subspecies` rows too. Taxonomy is resolved upstream
at import (GBIF; see the converters/resolvers), so by the time it reaches here the
`order`/`family`/vernacular fields are already filled where GBIF had them.

## Fields
| Field | Type | Description |
|---|---|---|
| `scientificName` | chr | The taxon name and the join key used everywhere else (observations, `species_roles`). May be at any rank (genus `Rattus`, subspecies `Mustela putorius furo`, …). |
| `taxonID` | chr | Source identifier for the taxon (e.g. a checklist URL), straight from the package's `taxa()`. |
| `taxonRank` | chr | Rank of the name: `kingdom`, `class`, `order`, `family`, `genus`, `species`, `subspecies`. |
| `order` | chr | Taxonomic order, where resolved (`NA` above order, e.g. a `kingdom` row). |
| `family` | chr | Taxonomic family, where resolved. **Can be `NA` even for a real animal** — GBIF gaps happen (e.g. `Mustela putorius furo`, the ferret). This is why `species_groups` matching also keys on **genus** (see [species_groups.md](species_groups.md)). |
| `vernacular_eng` | chr | English common name (`vernacularNames.eng`), or `NA`. Drives `ik_species_label()` when the name preference is "vernacular". |

## Notes
- **Casing.** This is one of our derived tables, but its columns mirror the upstream
  camtrapdp field names (`scientificName`, `taxonRank`, …) because they come straight
  from `taxa()` — only `vernacular_eng` is renamed. Compare the snake_case columns of
  [geography.md](geography.md), which are computed by us.
- **Genus fallback for roles.** Because `family` can be `NA`, registry matching parses
  the genus from the first word of `scientificName`. See
  [species_groups.md](species_groups.md).
- **Display, not analysis.** Use `ik_species_label()` for any user-facing name; don't
  read `vernacular_eng` directly in the UI, or you'll miss the preference toggle and
  the fallback.
