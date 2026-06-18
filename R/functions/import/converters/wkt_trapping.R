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
#' @param taxonomy_cache Optional .rds cache path for ik_resolve_taxa().
#' @param first_deployment_days Lead-in (days) for a trap's first check, which has
#'   no previous check to start the deployment interval.
#' @return Invisibly, the path to the written datapackage.json.
convert_wkt_trapping <- function(raw_dir, out_dir, meta = list(),
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

  checks$check_date <- as.Date(checks$date, format = "%d/%m/%Y")
  if (any(is.na(checks$check_date))) {
    logger::log_warn("wkt_trapping: %d check(s) with unparseable date — dropped.",
                     sum(is.na(checks$check_date)))
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
    dplyr::left_join(traps[, c("trap_id", "trap_code", "line_id", "line_name",
                               "long", "lat", "trap_type_id")], by = "trap_id") |>
    dplyr::left_join(trap_types, by = "trap_type_id") |>
    dplyr::left_join(bait_outcomes, by = "bait_outcome_id")

  # Camtrap DP requires deployment latitude/longitude; checks at traps with no
  # coordinates can't form valid deployments (and can't be mapped) — drop them,
  # loudly, so the gap is auditable and fixable upstream.
  no_coord <- is.na(d$lat) | is.na(d$long)
  if (any(no_coord)) {
    dropped_traps <- sort(unique(d$trap_code[no_coord]))
    logger::log_warn(
      "wkt_trapping: dropped %d check(s) at %d trap(s) with no coordinates: %s",
      sum(no_coord), length(dropped_traps), paste(dropped_traps, collapse = ", ")
    )
    d <- d[!no_coord, , drop = FALSE]
  }

  d <- d |>
    dplyr::arrange(.data$trap_id, .data$check_date) |>
    dplyr::group_by(.data$trap_id) |>
    dplyr::mutate(prev_date = dplyr::lag(.data$check_date)) |>
    dplyr::ungroup()

  first <- is.na(d$prev_date)
  d$deploy_start <- d$prev_date
  d$deploy_start[first] <- d$check_date[first] - first_deployment_days

  iso <- function(x) ifelse(is.na(x), NA_character_, paste0(format(x, "%Y-%m-%d"), "T00:00:00Z"))
  dep_id <- paste0("wkt-trap-dep-", d$trapdata_id)
  is_animal <- !is.na(d$observationType) & d$observationType == "animal" & !is.na(d$scientificName)

  # --- deployments (one per check interval) ------------------------------------
  deployments <- data.frame(
    deploymentID = dep_id,
    locationID = d$trap_code, locationName = d$trap_code,
    latitude = d$lat, longitude = d$long,
    coordinateUncertainty = NA_integer_,
    deploymentStart = iso(d$deploy_start), deploymentEnd = iso(d$check_date),
    setupBy = NA_character_, cameraID = d$trap_code,
    cameraModel = ifelse(is.na(d$trap_type), "predator trap", d$trap_type),
    cameraDelay = NA_integer_, cameraHeight = NA_real_, cameraDepth = NA_real_,
    cameraTilt = NA_integer_, cameraHeading = NA_integer_, detectionDistance = NA_real_,
    # timestampIssues is boolean (dates only, no time); Camtrap DP has no
    # featureType for a trap device (it's a camera-feature enum), so trap-ness
    # rides in source_type ($meta) + a method tag, not here.
    timestampIssues = TRUE, baitUse = NA, featureType = NA_character_, habitat = NA_character_,
    deploymentGroups = ifelse(is.na(d$line_name), NA_character_, paste0("line:", d$line_name)),
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
    eventStart = iso(d$deploy_start), eventEnd = iso(d$check_date),
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
    observationTags = paste0("bait_outcome:", d$bait_outcome %||% NA, " | bait:", d$baitstatus %||% NA),
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

  logger::log_info("wkt_trapping: wrote package (%d deployments, %d observations: %d animal, %d blank).",
                   nrow(deployments), nrow(observations), sum(is_animal),
                   sum(!is_animal & observations$observationType == "blank"))
  invisible(file.path(out_dir, "datapackage.json"))
}
