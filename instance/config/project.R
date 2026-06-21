# project.R — project-wide ecological knowledge (USER-EDITED).
#
# Unlike datasets.R (per-dataset import config), this is project-level: which
# species matter and how, applied across all datasets. Loaded at build time and
# materialised into ik_data$app. Changing it re-imports (it's in the cache key).

# Instance identity — the organisation running this deployment. Shown as the headline on the
# Overview; the datasets' own `name`s sit smaller beneath it.
organisation <- "Whakatane Kiwi Trust"

# Species groups — the curated "species we care about", so not all species are equal.
# One registry feeding: ecological ROLE (co-occurrence), per-method PROMINENCE, and the
# group itself (combined reporting / RAI unit). The LIST ORDER is priority (most
# important first). Each group declares:
#   label           display / reporting name (e.g. "Mustelids")
#   role            predator | protected | other  (drives co-occurrence analysis)
#   monitor         camera-monitoring class — target | interesting | (absent)
#   control         trapping/control class    — target | (absent)
#   sentiment       Overview card icon tint — bad | good | neutral. Defaults from role
#                   (predator→bad, protected→good, other→neutral); set explicitly to override —
#                   e.g. possums/hedgehogs/pigs/mice are role "other" but are pests → bad. Tints
#                   the CAMERA cards only (trapping targets are all pests, so all red = no signal).
# Monitoring and control care about DIFFERENT species, so prominence is per method: a
# camera view shows the monitor target+interesting groups (fixed); a trap view shows the
# control TARGET groups, then ALL OTHER species actually caught (data-driven — so bycatch
# like birds/rabbits shows up, while monitoring-only species like kiwi don't sit at 0).
# Matchers — a taxon joins the group if ANY hits (first group in order wins):
#   scientificName  exact name(s)
#   family          taxonomic family (from app$taxonomy)
#   genus           first word of the scientificName (robust to missing family, e.g.
#                   the ferret Mustela putorius furo has family = NA)
# Species matching no group are unclassified ("other").
species_groups <- list(
  mustelid = list(label = "Mustelids", role = "predator", monitor = "target", control = "target",
                  genus = "Mustela", family = "Mustelidae"),
  rat      = list(label = "Rats",      role = "predator", monitor = "target", control = "target", genus = "Rattus"),
  cat      = list(label = "Cats",      role = "predator", monitor = "target", control = "target", genus = "Felis"),
  hedgehog = list(label = "Hedgehogs", role = "other",    monitor = "interesting", control = "target", sentiment = "bad", scientificName = "Erinaceus europaeus"),
  possum   = list(label = "Possums",   role = "other",    monitor = "interesting", control = "target", sentiment = "bad", scientificName = "Trichosurus vulpecula"),
  dog      = list(label = "Dogs",      role = "predator", monitor = "interesting", family = "Canidae"),
  kiwi     = list(label = "Kiwi",      role = "protected", monitor = "interesting", genus = "Apteryx"),
  pig      = list(label = "Pigs",      role = "other", monitor = "interesting", sentiment = "bad", scientificName = "Sus scrofa"),
  mouse    = list(label = "Mice",      role = "other", monitor = "interesting", sentiment = "bad", scientificName = "Mus musculus"),
  weka     = list(label = "Weka",      role = "other", monitor = "interesting", scientificName = "Gallirallus australis")
)

# Same-species duplicate window — two observations of the same species at the same
# location within this many MINUTES are flagged a possible duplicate. `default`
# applies to all species; `by_species` overrides per scientificName. (Only applies
# to minute-resolution data; date-only sources are excluded.)
duplicate_window <- list(
  default    = 30,
  by_species = list()   # e.g. "Rattus" = 60
)

# Metric methodology — project-wide so reported metrics are reproducible and roll up
# consistently (NOT a per-user toggle).
#
# Camera RAI follows the DOC trail-camera protocol ("docs/reference/DOC trail camera guide
# v1.1.1...pdf") on which WKT Core Monitoring is based: monitoring LINES of 4 cameras, each set
# for 21 nights (≈ 504 camera-hours/camera → ≈ 2000 CH per line). The index is "detections per
# 2000 CH" computed PER LINE (the line is the unit of replication), then a reserve mean ± SE
# across its lines. So `norm_hours = 2000` is a per-LINE yardstick — it is NOT meaningful per
# camera; the map's per-camera detection rate uses `camera_hours` (one deployment) instead.
# This is the WKT CONVENTION — authoritative for the WKT camera dataset, NOT canonical for every
# project. When a project with a different design arrives, RAI methodology should become a
# per-dataset/per-project protocol {index, norm, replication unit}; see docs/data-model.
# Each METHOD has its own index: cameras → RAI; trapping → capture rate (per trap-night). A
# future method (e.g. tracking tunnels for rodents) would get its own list here.

# Camera monitoring.
camera <- list(
  rai = list(
    norm_hours   = 2000,   # per-LINE normalisation (≈ one line's 4×21-night effort) — DOC protocol
    camera_hours = 500,    # per-CAMERA scale for the map's per-camera detection rate: one camera's
                           # share of the 2000 CH line norm (2000/4) ≈ the guide's 504 CH (21×24).
                           # COSMETIC — the rate divides by each camera's ACTUAL hours, not this.
    use_net      = TRUE    # exclude possible_duplicate detections from the individuals count
  )
)

# Trapping.
trapping <- list(
  rate = list(
    norm_trap_days = 100   # captures per this many trap-days (= trap-nights)
  ),
  # Servicing-health buckets (good / watch / neglected) by mean days between checks. The cutoffs
  # are NOT hard-coded days — they're PERCENTILES of THIS project's own per-trap check-interval
  # distribution, computed at import and frozen on the cache (so they recalibrate only when the
  # data is re-imported, not as you change the period). good ≤ the `good` percentile, watch ≤ the
  # `watch` percentile, neglected above it. The resolved day values are shown in the UI.
  health = list(
    percentiles = c(good = 0.5, watch = 0.9),  # ≤ p50 good · ≤ p90 watch · > p90 neglected
    # Absolute GUARDRAILS so the relative percentiles stay meaningful in an unusually well- or
    # poorly-run project (percentiles alone always brand a "worst 10%", even if everyone is fine):
    floor   = 14,   # checked at least this often (days) ⇒ always GOOD, even if p50 is tighter
    ceiling = 60    # a gap longer than this (days) ⇒ always NEGLECTED, even if p90 is slacker
  ),
  # Which season a trap CHECK belongs to in the Trapping REVIEW:
  #   "check_date" — the season the check date falls in (intuitive; matches the capture-rate
  #                  metric, which already groups captures by their check date); or
  #   "interval"   — the season the check interval mostly sits in.
  season_by = "check_date"
)

# Overview display — how the per-species cards and the "other species" section behave.
overview <- list(
  # The Overview shows a per-species CARD for each target/interesting species (RAI headline for
  # camera monitoring; catch-count headline for trapping; click → reserve breakdown → records).
  # Optionally ALSO show the detailed per-reserve RAI/rate matrix table beneath the cards — set
  # TRUE for projects (few reserves) that want the at-a-glance by-reserve grid as well.
  show_rai_matrix_by_reserve = FALSE,

  # Species without a card (everything detected/caught that isn't a target/interesting/control
  # species) appear in an "other species" section beneath the cards. TRUE itemises each as its own
  # clickable drill-down entry; FALSE rolls them into a single total ("N detections across M
  # species"). Itemise for a handful of incidental species; total for projects recording dozens.
  # Applies to BOTH the camera and trapping sections.
  list_other_species = TRUE,

  # Default for the Overview "Compare to" control: "none" | "prior" | "last_year".
  default_compare = "prior"
)

# Image media cache (instance/www/media-cache). Each viewed camera burst caches a resized DISPLAY
# copy (1200px, what the app serves) and — by default — the full-res ORIGINAL (for the "view
# original" download). Originals are ~3.5× the size of the display copy, so a disk-constrained
# deployment can set keep_originals = FALSE for a display-only cache (much smaller; loses the
# full-res link and re-resizing). Re-caching with it off also prunes existing originals.
media <- list(
  keep_originals = TRUE
)
