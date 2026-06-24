# datasets.R — the dataset manifest (USER-EDITED).
#
# The inventory of Camtrap DP packages to import. Edit this whenever a dataset is
# added or removed; the app reads it but never writes it. Per-dataset geography
# derivation (the `geography` block below) lives HERE, beside the dataset it belongs
# to; project-wide ecology (species groups, metric methodology) lives in project.R.
#
# Each entry registers one package and tags it. Tags are domain facts the files
# can't carry (source_type / project / method) and are how the app selects
# datasets later, e.g. ik_datasets(source_type = "camera"). The list NAME is the
# dataset id, used as ik_data$datasets$<id> — keep it stable and unique.
#
# On load the app reconciles this manifest against instance/extdata/packages/:
#   - listed + present  -> read, validate (check_camtrapdp), tag
#   - present, unlisted -> warn and skip (untagged data can't be placed)
#   - listed, missing   -> error (you asked to load something that isn't there)
#
# Fields:
#   dir          folder under instance/extdata/packages/. OPTIONAL — defaults to
#                the id. Set it only when the folder name differs from the id
#                (e.g. a hyphenated folder vs. an underscored R id, as below).
#   source_type  device/source — open vocabulary: camera | trap | tracking |
#                bait_station | ...  (REQUIRED)
#   project      who collected it (organisation / programme).
#   method       purpose axis, distinct from source_type: monitoring | control.
#   name           friendly label for the UI.
#   force_timezone OPTIONAL IANA zone. Use when a READ package's timestamps are local
#                  wall-clock but mislabel the UTC offset (e.g. exported as +00:00 but
#                  really NZ). The clock time is REINTERPRETED in this zone at read
#                  (force_tz, DST-aware) — NO shift. For sources we cannot fix.
#   timezone       OPTIONAL IANA zone = the data's true local zone. For RAW datasets
#                  the converter reads it to write correct timestamps; the app then
#                  displays in this zone. Use this (not force_timezone) when the
#                  timestamps are / will be correctly offset.
#   temporal_resolution OPTIONAL finest granularity the timestamps can be TRUSTED to,
#                  regardless of what they appear to carry: "day" | "hour" | "minute" |
#                  "second" (default = full precision). The app floors every timestamp to
#                  this unit (in local time), so e.g. a "day" source reads as date-only
#                  everywhere even if the source stamps a spurious time. Use for sources
#                  whose clock is a data-entry artefact (most trap-check records).
#   enabled        set FALSE to keep an entry but skip importing it (default TRUE).
#
#   consolidate_taxa  OPTIONAL list of taxon-roll-up rules (camtrapdp::update_taxon).
#                  Each rule: list(genus = "Rattus")  -> all Rattus species become the
#                  genus Rattus; or list(from = c(...), to = "X") for explicit maps.
#   geography      OPTIONAL per-dataset derivation of canonical levels. `derive`
#                  names a registered deriver (R/functions/geography.R) that yields
#                  what the dataset's own data supports (camera: bare line + reserve;
#                  trap: bare line only). `reserve_match` (optional) assigns the
#                  reserve by SPATIALLY matching to another dataset's canonical
#                  reserves: list(strategy="spatial_hull", canonical=<dataset id>,
#                  buffer_m=<metres>, max_km=<km>). Stores per location: reserve (assigned),
#                  within_monitored_area (inside the canonical dataset's camera
#                  footprint — NOT the real reserve boundary), and
#                  nearest_monitoring_location/distance_km. A trap farther than `max_km`
#                  from any monitored location is grouped as "Outside monitored areas"
#                  instead of force-assigned to the nearest reserve (default max_km = Inf).

datasets <- list(

  wkt_core_monitoring = list(
    dir         = "camera-monitoring/wkt-core-monitoring",   # folder is hyphenated; the id is not
    source_type = "camera",
    project     = "Whakatane Core Monitoring",
    method      = "monitoring",
    name           = "WKT Core Monitoring",
    force_timezone = "Pacific/Auckland",  # exported as +00:00 but the clock is NZ local

    consolidate_taxa = list(
    #  list(genus = "Rattus"),             # roll all rats up to the genus Rattus
      # "Reverse" consolidations — map a coarse taxon onto a specific one:
      list(from = "Felis", to = "Felis catus"),   # genus Felis sp. -> Domestic cat
      list(from = "Fannia",                       # Fannia sp. -> generic Insect
           to = list(scientificName = "Insecta", taxonID = NA_character_,
                     taxonRank = "class", "vernacularNames.eng" = "Insect"))
    ),
    geography = list(
      derive   = "coded_locationname",      # parse "NN A_B"
      reserves = c(OH = "Ohope", KP = "Kohi Point", MK = "Mokoroa")
    ),

    enabled = TRUE
  ),

  wkt_ohiwa_forest_monitoring = list(
    dir         = "camera-monitoring/wkt-ohiwa-forest-monitoring",
    source_type = "camera",
    project     = "WKT Ohiwa Forest",          # ← distinct from core (see step 2)
    method      = "monitoring",
    name        = "WKT Ohiwa Forest Monitoring",
    force_timezone = "Pacific/Auckland",        # match core if its timestamps are the same
    geography   = list(derive = "coded_locationname",
                       reserves = c(OF = "Ohiwa Forest")),   # ← use Ohiwa's real code
    enabled     = FALSE
  ),


  # Raw dataset: converted to a Camtrap DP package on import (see the `raw` block).
  wkt_trapping = list(
    source_type = "trap",
    project     = "Whakatane Kiwi Trust",
    method      = "control",
    name        = "WKT Trapping",
    timezone    = "Pacific/Auckland",   # naive dates; the converter writes them as NZ-local
    temporal_resolution = "day",        # trap checks are date-only; any time on a stamp is noise
    enabled     = TRUE,
    raw = list(
      dir       = "whakatane-kiwi-trust/trapping-john",   # under instance/extdata/raw/
      converter = "wkt_trapping"    # registered in R/functions/import/converters.R
    ),
    geography = list(
      derive = "wkt_trap_line",     # bare line name from deploymentGroups
      reserve_match = list(         # reserve not in trap data -> match to camera reserves
        strategy  = "spatial_hull",
        canonical = "wkt_core_monitoring",  # match to this dataset's reserves
        buffer_m  = 200,
        max_km    = 2     # traps > this from any camera → "Outside monitored areas" (TUNE to taste)
      )
    )
  ),

  # --- trap.NZ exports (converter "trapnz"; one robust converter for any trap.NZ records CSV) ---
  # Southern Lakes Sanctuary — NOT WKT data; here for testing the trapnz converter. Multi-project
  # file: each trap.NZ project becomes a reserve, its line the line.
  sls_trapping = list(
    source_type = "trap",
    project     = "Southern Lakes Sanctuary",
    method      = "control",
    name        = "Southern Lakes Trapping",
    timezone    = "Pacific/Auckland",
    temporal_resolution = "day",        # trap.NZ check records — trustworthy to the day
    enabled     = FALSE,
    raw = list(dir = "southern-lakes-sanctuary/sls-trapping", converter = "trapnz"),
    geography = list(derive = "trapnz")            # reserve = trap.NZ project · line = trap.NZ line
  ),

  # WKT Ohiwa Forest trapping (trap.NZ). Its trap.NZ project is "Ohiwa Forest", so it lands in the
  # same reserve as the Ohiwa camera dataset.
  wkt_ohiwa_trapping = list(
    source_type = "trap",
    project     = "WKT Ohiwa Forest",
    method      = "control",
    name        = "WKT Ohiwa Trapping",
    timezone    = "Pacific/Auckland",
    temporal_resolution = "day",        # trap.NZ check records — trustworthy to the day
    enabled     = FALSE,
    raw = list(dir = "whakatane-kiwi-trust/trapnz-ohiwa", converter = "trapnz"),
    geography = list(derive = "trapnz")
  )

)
