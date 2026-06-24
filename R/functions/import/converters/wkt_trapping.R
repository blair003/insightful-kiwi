# wkt_trapping.R — converter for Whakatane Kiwi Trust predator-trap exports →
# a Camtrap DP package. Registered as "wkt_trapping" (see converters.R); the
# import pipeline runs it for any manifest entry whose raw$converter names it.
#
# Model (agreed): one DEPLOYMENT per check interval (a trap set between two
# consecutive checks; effort = deployment duration; first check gets a fixed
# lead-in). One OBSERVATION per check: animal (with species/sex/lifeStage) for
# capture outcomes, else blank. Taxonomy (taxonID/rank) comes from GBIF via
# ik_resolve_taxa(); vernacular is the curated one from outcomes.csv. Build NOT
# reused from v0.1 — only the reference semantics were carried over.
#
# TIME RESOLUTION LIMITATION: an observation's eventStart = the deployment start
# (previous check, or first-check minus the lead-in) and eventEnd = the CHECK DATE.
# A capture is therefore only resolved to the interval [eventStart, eventEnd] — it
# could have occurred any time after the previous check. Downstream we attribute a
# capture to the check date (eventEnd) — what was recorded, and the Records "When" —
# so a catch found early in a period may actually belong to the previous one. Both
# instants are date-only (deployment timestampIssues = TRUE), excluding traps from
# minute-resolution metrics (see observation_relations.R).

# Standardised file names expected in the raw dir.
WKT_TRAP_FILES <- c(
  traps = "traps.csv", checks = "trap-checks.csv", outcomes = "outcomes.csv",
  trap_types = "trap-types.csv", bait_outcomes = "bait-outcomes.csv"
)

# traps.csv is a denormalised SQL join with duplicate headers — name by position.
WKT_TRAP_COLUMNS <- c(
  "trap_id", "trap_code", "line_id", "trap_number", "project_id", "cat_trap",
  "inactive", "sponsor_id", "trap_type_id", "to_be_replaced", "to_be_maintained",
  "trap_name", "trapcode", "long", "lat", "ele", "project_id_b", "easting",
  "northing", "line_id_b", "line_code", "line_name", "project_id_c",
  "associated_cat_traps", "reserve_area"
)

#' Convert a WKT trap export into a Camtrap DP package.
#'
#' @param raw_dir   Directory holding the 5 standardised CSVs (WKT_TRAP_FILES).
#' @param out_dir   Where to write the Camtrap DP package.
#' @param meta      Manifest entry (used for package title/project).
#' @param dataset_id Dataset id; prepended to each trap's locationID to make it
#'   globally unique across datasets (trap codes are not unique on their own).
#' @param taxonomy_cache Optional .rds cache path for ik_resolve_taxa().
#' @param first_deployment_days Lead-in (days) for a trap's first check, which has
#'   no previous check to start the deployment interval.
#' @return Invisibly, the path to the written datapackage.json.
convert_wkt_trapping <- function(raw_dir, out_dir, meta = list(), dataset_id = "wkt_trapping",
                                 taxonomy_cache = NULL, first_deployment_days = 7) {
  na <- c("NA", "NULL", "")
  rd <- function(name, ...) utils::read.csv(
    file.path(raw_dir, WKT_TRAP_FILES[[name]]),
    stringsAsFactors = FALSE, strip.white = TRUE, na.strings = na, ...
  )

  traps <- utils::read.csv(file.path(raw_dir, WKT_TRAP_FILES[["traps"]]),
    header = FALSE, skip = 1, stringsAsFactors = FALSE, strip.white = TRUE,
    na.strings = na, col.names = WKT_TRAP_COLUMNS)
  checks        <- rd("checks", fileEncoding = "UTF-8-BOM")
  outcomes      <- rd("outcomes")
  trap_types    <- rd("trap_types")
  bait_outcomes <- rd("bait_outcomes")

  # --- conversion diagnostics: referenced ids absent from their lookup table, and rows with
  # no reference id at all — collected for the conversion log so upstream gaps are auditable.
  n_checks_read <- nrow(checks)
  unmapped <- list(
    trap_type_id    = setdiff(stats::na.omit(traps$trap_type_id),     trap_types$trap_type_id),
    outcome_id      = setdiff(stats::na.omit(checks$outcome_id),      outcomes$outcome_id),
    bait_outcome_id = setdiff(stats::na.omit(checks$bait_outcome_id), bait_outcomes$bait_outcome_id),
    trap_id         = setdiff(stats::na.omit(checks$trap_id),         traps$trap_id))
  no_ref <- list(
    traps_without_trap_type = sum(is.na(traps$trap_type_id)),
    checks_without_outcome  = sum(is.na(checks$outcome_id)),
    checks_without_bait     = sum(is.na(checks$bait_outcome_id)))

  checks$check_date <- as.Date(checks$date, format = "%d/%m/%Y")
  n_date_dropped <- sum(is.na(checks$check_date))
  if (n_date_dropped > 0) {
    logger::log_warn("wkt_trapping: %d check(s) with unparseable date — dropped.", n_date_dropped)
    checks <- checks[!is.na(checks$check_date), ]
  }

  # --- species lookup: outcome -> taxonomy (GBIF) + curated vernacular ----------
  spp <- unique(outcomes$scientificName[!is.na(outcomes$scientificName)])
  resolved <- ik_resolve_taxa(spp, cache_path = taxonomy_cache)
  taxa_lookup <- dplyr::left_join(
    dplyr::distinct(outcomes[!is.na(outcomes$scientificName),
                             c("scientificName", "vernacularName")]),
    resolved, by = "scientificName"
  )
  taxa_lookup$vernacular_final <- ifelse(
    !is.na(taxa_lookup$vernacularName) & nzchar(taxa_lookup$vernacularName),
    taxa_lookup$vernacularName, taxa_lookup$vernacular_eng
  )

  # --- join everything onto the checks -----------------------------------------
  d <- checks |>
    dplyr::left_join(outcomes, by = "outcome_id") |>
    dplyr::left_join(traps[, c("trap_id", "trap_code", "line_id",
                               "long", "lat", "trap_type_id")], by = "trap_id") |>
    dplyr::left_join(trap_types, by = "trap_type_id") |>
    dplyr::left_join(bait_outcomes, by = "bait_outcome_id")

  # Traps with no coordinates (e.g. mobile traps) are KEPT — their checks are valid data. They
  # carry NA lat/long (Camtrap DP tolerates this) and land in the "Unknown" reserve downstream,
  # so they appear in counts / capture-rate / the trapping review but fall out of spatial (map)
  # views. (Previously dropped — that lost real catch data.)
  no_coord <- is.na(d$lat) | is.na(d$long)
  n_coordless <- sum(no_coord)
  coordless_traps <- sort(unique(d$trap_code[no_coord]))
  if (n_coordless > 0)
    logger::log_info(
      "wkt_trapping: %d check(s) at %d trap(s) have no coordinates — kept (Unknown reserve): %s",
      n_coordless, length(coordless_traps), paste(coordless_traps, collapse = ", "))

  d <- d |>
    dplyr::arrange(.data$trap_id, .data$check_date) |>
    dplyr::group_by(.data$trap_id) |>
    dplyr::mutate(prev_date = dplyr::lag(.data$check_date)) |>
    dplyr::ungroup()

  first <- is.na(d$prev_date)
  d$deploy_start <- d$prev_date
  d$deploy_start[first] <- d$check_date[first] - first_deployment_days

  # Raw check dates are NAIVE (no time/offset); the manifest declares their zone.
  # Write each as local midnight in that zone so the package carries correct
  # instants (force_tz, DST-aware), not naive UTC.
  src_tz <- meta$timezone %||% "UTC"
  mid <- function(dates) {
    lubridate::force_tz(as.POSIXct(paste0(format(dates), " 00:00:00"), tz = "UTC"), src_tz)
  }
  dep_id <- paste0("wkt-trap-dep-", d$trapdata_id)
  is_animal <- !is.na(d$observationType) & d$observationType == "animal" & !is.na(d$scientificName)

  # --- deployments (one per check interval) ------------------------------------
  deployments <- data.frame(
    deploymentID = dep_id,
    locationID = paste0(dataset_id, "_", d$trap_code), locationName = d$trap_code,
    latitude = d$lat, longitude = d$long,
    coordinateUncertainty = NA_integer_,
    deploymentStart = mid(d$deploy_start), deploymentEnd = mid(d$check_date),
    setupBy = NA_character_, cameraID = d$trap_code,
    cameraModel = d$trap_type,                       # trap type; NA (→ blank) when unknown

    cameraDelay = NA_integer_, cameraHeight = NA_real_, cameraDepth = NA_real_,
    cameraTilt = NA_integer_, cameraHeading = NA_integer_, detectionDistance = NA_real_,
    # timestampIssues is boolean (dates only, no time); Camtrap DP has no
    # featureType for a trap device (it's a camera-feature enum), so trap-ness
    # rides in source_type ($meta) + a method tag, not here.
    timestampIssues = TRUE, baitUse = NA, featureType = NA_character_, habitat = NA_character_,
    # line_id is the canonical (GIS) trapline id; line_code/line_name are hand-typed.
    deploymentGroups = ifelse(is.na(d$line_id), NA_character_, paste0("line:", d$line_id)),
    deploymentTags = paste0("method:trapping | source_trap_id:", d$trap_id,
                            " | trap_type:", ifelse(is.na(d$trap_type), "", d$trap_type)),
    deploymentComments = "Converted from WKT trap checks; deployment = interval between checks.",
    stringsAsFactors = FALSE
  )

  # --- observations (one per check) --------------------------------------------
  tl <- taxa_lookup[match(d$scientificName, taxa_lookup$scientificName), ]
  observations <- data.frame(
    observationID = paste0("wkt-trap-obs-", d$trapdata_id),
    deploymentID = dep_id, mediaID = NA_character_,
    eventID = paste0("wkt-trap-evt-", d$trapdata_id),
    eventStart = mid(d$deploy_start), eventEnd = mid(d$check_date),
    observationLevel = "event",
    observationType = ifelse(is.na(d$observationType), "unclassified", d$observationType),
    cameraSetupType = NA_character_,
    scientificName = ifelse(is_animal, d$scientificName, NA_character_),
    count = ifelse(is_animal, 1L, NA_integer_),
    lifeStage = ifelse(is_animal, d$lifeStage, NA_character_),
    sex = ifelse(is_animal, d$sex, NA_character_),
    behavior = NA_character_, individualID = NA_character_,
    individualPositionRadius = NA_real_, individualPositionAngle = NA_real_,
    individualSpeed = NA_real_, bboxX = NA_real_, bboxY = NA_real_,
    bboxWidth = NA_real_, bboxHeight = NA_real_,
    classificationMethod = NA_character_, classifiedBy = NA_character_,
    classificationTimestamp = NA_character_, classificationProbability = NA_real_,
    # Carry the check's source fields that have no native Camtrap DP slot as key:value tags
    # (parsed by the observation viewer): the trap STATUS (camtrapDP observationType only knows
    # animal/blank, so "still set" vs "sprung" vs "bait gone" lives here — same `status:` convention
    # the trap.nz converter uses), the VOLUNTEER who did the check, the bait, and whether bait was
    # changed. volunteer_id is an id only — the raw export has no name table.
    observationTags = paste(
      paste0("status:",        ifelse(is_animal, "caught", d$description)),  # non-capture: still set/sprung/bait gone
      paste0("volunteer:",     d$volunteer_id %||% NA),
      paste0("bait_outcome:",  d$bait_outcome %||% NA),
      # "Special Recipe (In notes)" — the notes are a free-text per-trap history, too unstructured to
      # mine a recipe from, so shorten the label to just "Special Recipe" (less width on the Bait chart).
      paste0("bait:",          gsub("Special Recipe (In notes)", "Special Recipe", d$baitstatus %||% NA, fixed = TRUE)),
      paste0("bait_change:",   d$baitchange %||% NA),
      sep = " | "),
    observationComments = d$description,
    taxon.taxonID = ifelse(is_animal, tl$taxonID, NA_character_),
    taxon.taxonRank = ifelse(is_animal, tl$taxonRank, NA_character_),
    taxon.vernacularNames.eng = ifelse(is_animal, tl$vernacular_final, NA_character_),
    taxon.vernacularNames.nld = NA_character_,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  # --- media: traps carry no images, but frictionless can't WRITE a 0-row
  # resource. Write a single placeholder row, then truncate media.csv to its
  # header after writing (read tolerates a 0-row media table).
  media <- data.frame(
    mediaID = "wkt-trap-media-placeholder", deploymentID = deployments$deploymentID[1],
    captureMethod = NA_character_, timestamp = deployments$deploymentEnd[1],
    filePath = "placeholder.jpg", filePublic = FALSE, fileName = NA_character_,
    fileMediatype = "image/jpeg", exifData = NA_character_, favorite = NA,
    mediaComments = NA_character_, eventID = NA_character_,
    stringsAsFactors = FALSE
  )

  # --- assemble onto a clean ik template and write -----------------------------
  x <- ik_camtrapdp_template(
    title       = meta$name %||% "WKT trapping",
    id          = "wkt-trapping",
    description = "Whakatane Kiwi Trust predator trapping, converted from trap-check exports.",
    contributors = list(list(
      title = meta$project %||% "Whakatane Kiwi Trust", role = "rightsHolder"
    ))
  )
  deployments(x)  <- tibble::as_tibble(deployments)
  media(x)        <- tibble::as_tibble(media)
  observations(x) <- tibble::as_tibble(observations)

  # Taxonomy lives in the package metadata (write strips taxon.* from observations
  # and rebuilds them from here on read).
  x$taxonomic <- lapply(seq_len(nrow(taxa_lookup)), function(i) {
    r <- taxa_lookup[i, ]
    entry <- list(scientificName = r$scientificName)
    if (!is.na(r$taxonID))   entry$taxonID   <- r$taxonID
    if (!is.na(r$taxonRank)) entry$taxonRank <- r$taxonRank
    if (!is.na(r$vernacular_final) && nzchar(r$vernacular_final)) {
      entry$vernacularNames <- list(eng = r$vernacular_final)
    }
    entry
  })

  # spatial + temporal are auto-derived from the deployments by the deployments(x)<-
  # setter (update_spatial/update_temporal), so we don't set them here.

  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  camtrapdp::write_camtrapdp(x, out_dir)

  # Drop the placeholder media row -> header-only media.csv (0 media records).
  media_csv <- file.path(out_dir, "media.csv")
  writeLines(readLines(media_csv, n = 1L), media_csv)

  n_blank <- sum(!is_animal & observations$observationType == "blank")

  # --- conversion log: a plain-text audit of what came in, what was dropped, and which
  # referenced ids had no row in their lookup table (so data gaps are fixable upstream). ----
  ids <- function(x) if (length(x)) paste(sort(x), collapse = ", ") else "none"
  log_path <- file.path(out_dir, "conversion-log.txt")
  writeLines(c(
    "WKT trapping conversion log",
    paste("Generated:", format(Sys.time())),
    paste("Source:   ", raw_dir),
    "",
    "== Counts ==",
    sprintf("Traps (reference table):     %d", nrow(traps)),
    sprintf("Checks read:                 %d", n_checks_read),
    sprintf("Checks dropped (bad date):   %d", n_date_dropped),
    sprintf("Checks coordless (kept, Unknown): %d  (at %d trap(s))", n_coordless, length(coordless_traps)),
    sprintf("Deployments written:         %d", nrow(deployments)),
    sprintf("Observations written:        %d  (animal: %d, blank: %d)", nrow(observations), sum(is_animal), n_blank),
    "",
    "== Referenced IDs with no row in their lookup table ==",
    sprintf("trap_type_id  not in trap-types.csv:    %s", ids(unmapped$trap_type_id)),
    sprintf("outcome_id    not in outcomes.csv:      %s", ids(unmapped$outcome_id)),
    sprintf("bait_outcome_id not in bait-outcomes.csv: %s", ids(unmapped$bait_outcome_id)),
    sprintf("trap_id       not in traps.csv:         %s", ids(unmapped$trap_id)),
    "",
    "== Rows with no reference ID (NA) ==",
    sprintf("Traps with no trap_type_id (-> blank trap type): %d", no_ref$traps_without_trap_type),
    sprintf("Checks with no outcome_id:                       %d", no_ref$checks_without_outcome),
    sprintf("Checks with no bait_outcome_id:                  %d", no_ref$checks_without_bait),
    "",
    "== Coordless traps (no lat/long; kept, placed in 'Unknown' reserve) ==",
    ids(coordless_traps)
  ), log_path)

  logger::log_info("wkt_trapping: wrote package (%d deployments, %d observations: %d animal, %d blank); log -> %s",
                   nrow(deployments), nrow(observations), sum(is_animal), n_blank, log_path)
  invisible(file.path(out_dir, "datapackage.json"))
}
