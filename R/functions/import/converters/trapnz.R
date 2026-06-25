# trapnz.R — converter for trap.NZ "trap records" CSV exports → a Camtrap DP package.
# Registered as "trapnz" (see converters.R). trap.NZ (https://trap.nz) is a widely-used NZ
# community-trapping platform, so this aims to be reusable across any trap.NZ trap-records export.
#
# Model (same as the WKT trap converter): one DEPLOYMENT per check interval (a trap set between
# two consecutive checks; effort = the interval; the first check gets a fixed lead-in). One
# OBSERVATION per check — animal (species + count) for a catch, else blank. Taxonomy (taxonID/
# rank) comes from GBIF via ik_resolve_taxa(); the vernacular is our curated one.
#
# trap.NZ specifics: `strikes` is the number caught (the observation count); `species caught` is a
# common name ("None" = no catch) mapped to a scientificName below; lat/long are already WGS84
# (no easting/northing → NZTM conversion needed); `project` is the trapping group (→ reserve) and
# `line` the line within it. Times are a real DD/MM/YYYY HH:MM check timestamp, but a catch is
# still only resolved to the interval (the check time isn't the catch time), so timestampIssues
# stays TRUE — traps are excluded from minute-resolution metrics, as for WKT.

TRAPNZ_FILE <- "trapnz_trap_records.csv"   # the standard trap.NZ "trap records" export name

# trap.NZ common name → scientificName + curated vernacular. A FUNCTION (not a loose global data
# constant), used only by this converter. Anything not "None"/NA and not here (e.g. "Unspecified",
# "Other", a new label) becomes an animal catch with NO species (unidentified), and is listed in the
# conversion log so it can be added.
trapnz_species <- function() data.frame(stringsAsFactors = FALSE, check.names = FALSE,
  caught = c("Possum", "Rat", "Rat - Ship", "Rat - Norway", "Rat - Kiore", "Mouse", "Stoat",
             "Weasel", "Ferret", "Cat", "Hedgehog", "Rabbit", "Hare", "Goat", "Pig", "Bird",
             "Pūkeko", "Pukeko"),
  scientificName = c("Trichosurus vulpecula", "Rattus", "Rattus rattus", "Rattus norvegicus",
             "Rattus exulans", "Mus musculus", "Mustela erminea", "Mustela nivalis",
             "Mustela furo", "Felis catus", "Erinaceus europaeus", "Oryctolagus cuniculus",
             "Lepus europaeus", "Capra hircus", "Sus scrofa", "Aves", "Porphyrio melanotus",
             "Porphyrio melanotus"),
  vernacular = c("Common brushtail possum", "Rat", "Ship rat", "Norway rat", "Kiore",
             "House mouse", "Stoat", "Weasel", "Ferret", "Cat", "Hedgehog", "Rabbit", "Hare",
             "Goat", "Pig", "Bird", "Pūkeko", "Pūkeko"))

#' Convert a trap.NZ trap-records export into a Camtrap DP package.
#'
#' @param raw_dir   Directory holding `trapnz_trap_records.csv`.
#' @param out_dir   Where to write the Camtrap DP package.
#' @param meta      Manifest entry (title, timezone).
#' @param dataset_id Dataset id; prefixed onto each trap's locationID for global uniqueness.
#' @param taxonomy_cache Optional .rds cache path for ik_resolve_taxa().
#' @param first_deployment_days Lead-in (days) for a trap's first check (no previous check).
#' @return Invisibly, the path to the written datapackage.json.
convert_trapnz <- function(raw_dir, out_dir, meta = list(), dataset_id = "trapnz",
                           taxonomy_cache = NULL, first_deployment_days = 7) {
  na <- c("NA", "NULL", "")
  d  <- utils::read.csv(file.path(raw_dir, TRAPNZ_FILE), stringsAsFactors = FALSE,
                        strip.white = TRUE, na.strings = na, fileEncoding = "UTF-8-BOM",
                        check.names = FALSE)
  n_read <- nrow(d)

  # --- check timestamp → instant in the declared zone. trap.NZ exports vary in date format
  # (DD/MM/YYYY HH:MM and YYYY-MM-DD HH:MM both seen), so try several orders per row (naive,
  # then force_tz to the source zone — DST-aware, no shift).
  src_tz <- meta$timezone %||% "Pacific/Auckland"
  dt <- suppressWarnings(lubridate::parse_date_time(
    d$date, orders = c("dmY HM", "Ymd HM", "dmY HMS", "Ymd HMS", "dmY", "Ymd"),
    tz = "UTC", quiet = TRUE))
  d$check_dt <- lubridate::force_tz(dt, src_tz)
  n_date_dropped <- sum(is.na(d$check_dt))
  if (n_date_dropped > 0) {
    logger::log_warn("trapnz: %d row(s) with unparseable date — dropped.", n_date_dropped)
    d <- d[!is.na(d$check_dt), , drop = FALSE]
  }

  # --- catch / species --------------------------------------------------------------------
  caught   <- d[["species caught"]]
  is_none  <- is.na(caught) | caught == "None"
  sci      <- trapnz_species()$scientificName[match(caught, trapnz_species()$caught)]
  is_animal <- !is_none                                              # a catch (even if species unknown)
  unmapped_caught <- sort(unique(caught[is_animal & is.na(sci)]))    # e.g. "Unspecified", "Other"
  scientificName  <- ifelse(is_animal, sci, NA_character_)           # NA → unidentified animal
  strikes  <- suppressWarnings(as.integer(d$strikes))
  count    <- ifelse(is_animal, ifelse(is.na(strikes) | strikes < 1L, 1L, strikes), NA_integer_)
  obs_type <- ifelse(is_animal, "animal", "blank")

  # --- species lookup: scientificName -> taxonomy (GBIF) + curated vernacular --------------
  spp      <- unique(stats::na.omit(scientificName))
  resolved <- ik_resolve_taxa(spp, cache_path = taxonomy_cache)
  taxa_lookup <- dplyr::left_join(
    dplyr::distinct(trapnz_species()[, c("scientificName", "vernacular")]),
    resolved, by = "scientificName")
  taxa_lookup <- taxa_lookup[taxa_lookup$scientificName %in% spp, , drop = FALSE]
  taxa_lookup$vernacular_final <- ifelse(
    !is.na(taxa_lookup$vernacular) & nzchar(taxa_lookup$vernacular),
    taxa_lookup$vernacular, taxa_lookup$vernacular_eng)

  # --- deployments: one per check interval (prev check at the same trap) -------------------
  trap_nid <- d[["trap nid"]]
  d <- d |>
    dplyr::mutate(.trap = trap_nid) |>
    dplyr::arrange(.data$.trap, .data$check_dt) |>
    dplyr::group_by(.data$.trap) |>
    dplyr::mutate(prev_dt = dplyr::lag(.data$check_dt)) |>
    dplyr::ungroup()
  first <- is.na(d$prev_dt)
  d$start_dt <- d$prev_dt
  d$start_dt[first] <- d$check_dt[first] - first_deployment_days * 86400  # seconds

  # recompute the per-row vectors in the new (sorted) row order
  caught <- d[["species caught"]]; is_none <- is.na(caught) | caught == "None"
  sci <- trapnz_species()$scientificName[match(caught, trapnz_species()$caught)]
  is_animal <- !is_none; scientificName <- ifelse(is_animal, sci, NA_character_)
  strikes <- suppressWarnings(as.integer(d$strikes))
  count   <- ifelse(is_animal, ifelse(is.na(strikes) | strikes < 1L, 1L, strikes), NA_integer_)
  obs_type <- ifelse(is_animal, "animal", "blank")

  rid    <- d$nid                                            # trap.NZ record id (unique per check)
  dep_id <- paste0("trapnz-dep-", rid)
  loc_id <- paste0(dataset_id, "_", d[["trap nid"]])         # stable trap → location
  tl     <- taxa_lookup[match(scientificName, taxa_lookup$scientificName), ]

  pipe <- function(...) {                                    # NA-safe "k:v | k:v" tag string
    parts <- list(...); paste(unlist(parts), collapse = " | ")
  }
  tag_str <- function(keys, vals) paste(
    mapply(function(k, v) paste0(k, ":", ifelse(is.na(v) | !nzchar(as.character(v)), "", v)),
           keys, vals), collapse = " | ")

  deployments <- data.frame(
    deploymentID = dep_id,
    locationID = loc_id, locationName = d$trap,
    latitude = d$lat, longitude = d$long, coordinateUncertainty = NA_integer_,
    deploymentStart = d$start_dt, deploymentEnd = d$check_dt,
    setupBy = NA_character_, cameraID = d$code %||% d$trap,
    cameraModel = d[["trap type"]],                          # trap type (DOC 200, A12, …)
    cameraDelay = NA_integer_, cameraHeight = NA_real_, cameraDepth = NA_real_,
    cameraTilt = NA_integer_, cameraHeading = NA_integer_, detectionDistance = NA_real_,
    timestampIssues = TRUE, baitUse = NA, featureType = NA_character_, habitat = NA_character_,
    # project + line carried for the geography deriver ("trapnz"); rest as tags.
    deploymentGroups = paste0("project:", ifelse(is.na(d$project), "", d$project),
                              " | line:", ifelse(is.na(d$line), "", d$line)),
    deploymentTags = paste0("method:trapping | source_trap_id:", d[["trap nid"]],
                            " | trap_type:", ifelse(is.na(d[["trap type"]]), "", d[["trap type"]])),
    deploymentComments = "Converted from a trap.NZ trap-records export; deployment = interval between checks.",
    stringsAsFactors = FALSE
  )

  observations <- data.frame(
    observationID = paste0("trapnz-obs-", rid),
    deploymentID = dep_id, mediaID = NA_character_,
    eventID = paste0("trapnz-evt-", rid),
    eventStart = d$start_dt, eventEnd = d$check_dt,
    observationLevel = "event", observationType = obs_type, cameraSetupType = NA_character_,
    scientificName = scientificName, count = count,
    lifeStage = ifelse(is_animal, d$maturity, NA_character_),
    sex = ifelse(is_animal, tolower(d$gender), NA_character_),
    behavior = NA_character_, individualID = NA_character_,
    individualPositionRadius = NA_real_, individualPositionAngle = NA_real_,
    individualSpeed = NA_real_, bboxX = NA_real_, bboxY = NA_real_,
    bboxWidth = NA_real_, bboxHeight = NA_real_,
    classificationMethod = NA_character_, classifiedBy = NA_character_,
    classificationTimestamp = NA_character_, classificationProbability = NA_real_,
    # check fields with no native Camtrap DP slot → key:value tags (parsed by the viewer):
    # who checked, the bait set, whether rebaited, the trap status.
    observationTags = vapply(seq_len(nrow(d)), function(i) tag_str(
      c("volunteer", "bait", "bait_change", "status"),
      c(d[["recorded by"]][i], d[["bait type"]][i], d$rebaited[i], d$status[i])),
      character(1)),
    observationComments = d$notes,
    taxon.taxonID = ifelse(is_animal, tl$taxonID, NA_character_),
    taxon.taxonRank = ifelse(is_animal, tl$taxonRank, NA_character_),
    taxon.vernacularNames.eng = ifelse(is_animal, tl$vernacular_final, NA_character_),
    taxon.vernacularNames.nld = NA_character_,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  media <- data.frame(
    mediaID = "trapnz-media-placeholder", deploymentID = deployments$deploymentID[1],
    captureMethod = NA_character_, timestamp = deployments$deploymentEnd[1],
    filePath = "placeholder.jpg", filePublic = FALSE, fileName = NA_character_,
    fileMediatype = "image/jpeg", exifData = NA_character_, favorite = NA,
    mediaComments = NA_character_, eventID = NA_character_, stringsAsFactors = FALSE)

  # --- assemble + write -------------------------------------------------------------------
  x <- ik_camtrapdp_template(
    title = meta$name %||% "trap.NZ trapping", id = dataset_id,
    description = "Trapping records converted from a trap.NZ export.",
    contributors = list(list(title = meta$project %||% "trap.NZ", role = "rightsHolder")))
  deployments(x)  <- tibble::as_tibble(deployments)
  media(x)        <- tibble::as_tibble(media)
  observations(x) <- tibble::as_tibble(observations)
  x$taxonomic <- lapply(seq_len(nrow(taxa_lookup)), function(i) {
    r <- taxa_lookup[i, ]
    entry <- list(scientificName = r$scientificName)
    if (!is.na(r$taxonID))   entry$taxonID   <- r$taxonID
    if (!is.na(r$taxonRank)) entry$taxonRank <- r$taxonRank
    if (!is.na(r$vernacular_final) && nzchar(r$vernacular_final))
      entry$vernacularNames <- list(eng = r$vernacular_final)
    entry
  })

  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  camtrapdp::write_camtrapdp(x, out_dir)
  media_csv <- file.path(out_dir, "media.csv")
  writeLines(readLines(media_csv, n = 1L), media_csv)        # → header-only media.csv

  n_animal <- sum(is_animal); n_blank <- sum(!is_animal)
  ids <- function(v) if (length(v)) paste(sort(v), collapse = ", ") else "none"
  writeLines(c(
    "trap.NZ trapping conversion log",
    paste("Generated:", format(Sys.time())),
    paste("Source:   ", file.path(raw_dir, TRAPNZ_FILE)),
    "",
    "== Counts ==",
    sprintf("Rows read:            %d", n_read),
    sprintf("Rows dropped (date):  %d", n_date_dropped),
    sprintf("Traps (locations):    %d", length(unique(loc_id))),
    sprintf("Projects (→ reserve): %d", length(unique(stats::na.omit(d$project)))),
    sprintf("Deployments written:  %d", nrow(deployments)),
    sprintf("Observations written: %d  (animal: %d, blank: %d)", nrow(observations), n_animal, n_blank),
    "",
    "== Catches with no species mapping (→ unidentified animal; add to trapnz_species()) ==",
    ids(unmapped_caught)
  ), file.path(out_dir, "conversion-log.txt"))

  logger::log_info("trapnz: wrote package (%d deployments, %d obs: %d animal, %d blank).",
                   nrow(deployments), nrow(observations), n_animal, n_blank)
  invisible(file.path(out_dir, "datapackage.json"))
}
