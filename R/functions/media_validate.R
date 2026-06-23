# media_validate.R — maintenance diagnostics for the media cache + its source metadata.
#   ik_duplicate_deployment_windows() — camera deployments sharing an EXACT start+end window across
#                                       locations (the fingerprint of a re-uploaded SD card / a
#                                       duplicate deployment); also logged at import (build_period).
#   ik_media_duplicate_scan()         — byte-identical cached images shared across DIFFERENT
#                                       deployments (the on-disk consequence of the above).
#   ik_validate_media_cache()         — disk vs metadata integrity: orphan dirs/files, zero-byte.
# All read-only; safe to run anytime (the CLI wrapper is R/jobs/media_validate_job.R).

#' Camera deployments that share an EXACT (start, end) window across DIFFERENT locations — two
#' cameras can't be set and retrieved at the same second, so this flags a duplicate / misassigned
#' deployment (e.g. one SD card's images uploaded to two deployments). Camera-only: trap windows are
#' date-grain and legitimately shared. ONE ROW PER DEPLOYMENT in a flagged window (grouped by window,
#' sorted), each with its media-entry count — so you can see which deployment carries the images.
#' @param ik_data The container (gives the media counts; or pass `deployments`/`locations` directly,
#'   e.g. at build time, where `n_media` is NA).
#' @return data.frame: deploymentStart · deploymentEnd · location · deploymentID · dataset · n_media,
#'   grouped by window; empty when none. @keywords internal
ik_duplicate_deployment_windows <- function(ik_data = NULL, deployments = NULL, locations = NULL) {
  dp   <- deployments %||% ik_data$app$period$deployments
  locs <- locations   %||% ik_data$app$geography$locations
  empty <- data.frame(deploymentStart = as.POSIXct(character()), deploymentEnd = as.POSIXct(character()),
                      location = character(), deploymentID = character(), dataset = character(),
                      n_media = integer(), stringsAsFactors = FALSE)
  dp <- dp[!is.na(dp$deploymentStart) & !is.na(dp$deploymentEnd) & !is.na(dp$locationID) &
           !is.na(dp$source_type) & dp$source_type == "camera", , drop = FALSE]
  if (!nrow(dp)) return(empty)
  dp$location <- locs$name[match(dp$locationID, locs$location_id)]
  key    <- paste(as.numeric(dp$deploymentStart), as.numeric(dp$deploymentEnd))
  shared <- names(which(tapply(dp$locationID, key, function(x) length(unique(x))) > 1))  # window @ ≥2 locations
  dp <- dp[key %in% shared, , drop = FALSE]
  if (!nrow(dp)) return(empty)
  mcount <- if (is.null(ik_data)) NULL else {
    m <- tryCatch(ik_media(ik_data, dataset = names(ik_data$datasets)), error = function(e) NULL)
    if (is.null(m) || !nrow(m)) NULL else table(m$deploymentID)
  }
  n_media <- if (is.null(mcount)) NA_integer_ else { v <- as.integer(mcount[dp$deploymentID]); v[is.na(v)] <- 0L; v }
  out <- data.frame(deploymentStart = dp$deploymentStart, deploymentEnd = dp$deploymentEnd,
                    location = dp$location, deploymentID = dp$deploymentID, dataset = dp$dataset,
                    n_media = n_media, stringsAsFactors = FALSE)
  out[order(out$deploymentStart, out$deploymentEnd, out$location), , drop = FALSE]
}

#' Log a warning at import for any duplicate deployment windows (called from build_period).
#' @keywords internal
.warn_duplicate_deployment_windows <- function(deployments, locations) {
  d <- tryCatch(ik_duplicate_deployment_windows(deployments = deployments, locations = locations),
                error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(invisible(NULL))
  by <- split(d, paste(format(d$deploymentStart), format(d$deploymentEnd)))
  logger::log_warn("media: %d camera deployment(s) across %d shared window(s) — possible duplicate uploads:",
                   nrow(d), length(by))
  for (g in by)
    logger::log_warn("  %s .. %s : %s", format(g$deploymentStart[1]), format(g$deploymentEnd[1]),
                     paste(sprintf("%s [%s]", g$location, g$deploymentID), collapse = " | "))
  invisible(d)
}

#' Scan the media cache for byte-identical images shared across DIFFERENT deployments — the on-disk
#' consequence of a duplicate upload. Content-hashes the cached files (tools::md5sum), keeps hashes
#' that span more than one event, and resolves each to its deployment + location + species.
#' @param ik_data The container. @param kind "display" (the 1200px copies, always present — default,
#'   faster) or "original".
#' @return list(pairs, detail): `pairs` = one row per deployment pair (deployment_a/location_a,
#'   deployment_b/location_b, n_shared images, species), worst first; `detail` = one row per
#'   duplicated cached image (hash · eventID · mediaID · deploymentID · location · species). Empty
#'   frames when none. @keywords internal
ik_media_duplicate_scan <- function(ik_data, kind = c("display", "original")) {
  kind  <- match.arg(kind)
  root  <- ik_media_cache_dir()
  files <- list.files(root, pattern = "\\.jpg$", recursive = TRUE, full.names = TRUE)
  files <- if (kind == "display") files[grepl("_display\\.jpg$", files)]
           else                   files[!grepl("_display\\.jpg$", files)]
  empty_pairs  <- data.frame(deployment_a = character(), location_a = character(),
                             deployment_b = character(), location_b = character(),
                             n_shared = integer(), species = character(), stringsAsFactors = FALSE)
  empty_detail <- data.frame(hash = character(), eventID = character(), mediaID = character(),
                             deploymentID = character(), location = character(), species = character(),
                             stringsAsFactors = FALSE)
  if (!length(files)) return(list(pairs = empty_pairs, detail = empty_detail))

  df <- data.frame(hash = unname(tools::md5sum(files)), eventID = basename(dirname(files)),
                   mediaID = sub("(_display)?\\.jpg$", "", basename(files)), stringsAsFactors = FALSE)
  df <- df[!is.na(df$hash), , drop = FALSE]
  # keep content hashes that span MORE THAN ONE event (a within-event static burst isn't a dup)
  cross <- names(which(tapply(df$eventID, df$hash, function(x) length(unique(x))) > 1))
  df <- df[df$hash %in% cross, , drop = FALSE]
  if (!nrow(df)) return(list(pairs = empty_pairs, detail = empty_detail))

  # resolve event -> deployment (media table), deployment -> location, event -> species (observations)
  m   <- ik_media(ik_data, dataset = names(ik_data$datasets))
  dp  <- ik_data$app$period$deployments
  locs <- ik_data$app$geography$locations
  obs <- ik_observations(ik_data, with_location = FALSE)
  df$deploymentID <- m$deploymentID[match(df$eventID, m$eventID)]
  df$location     <- locs$name[match(dp$locationID[match(df$deploymentID, dp$deploymentID)], locs$location_id)]
  df$species      <- ik_species_label(obs$scientificName[match(df$eventID, obs$eventID)], ik_data, "vernacular")

  # deployment pairs: per shared-content hash, every distinct deployment pair (normalised a<b)
  pr <- do.call(rbind, lapply(split(df, df$hash), function(g) {
    deps <- sort(unique(g$deploymentID[!is.na(g$deploymentID)]))
    if (length(deps) < 2) return(NULL)
    cmb <- utils::combn(deps, 2)
    do.call(rbind, lapply(seq_len(ncol(cmb)), function(j) data.frame(
      deployment_a = cmb[1, j], location_a = g$location[match(cmb[1, j], g$deploymentID)],
      deployment_b = cmb[2, j], location_b = g$location[match(cmb[2, j], g$deploymentID)],
      species = g$species[1] %||% NA_character_, stringsAsFactors = FALSE)))
  }))
  if (is.null(pr)) return(list(pairs = empty_pairs, detail = df))
  pairs <- pr |>
    dplyr::group_by(.data$deployment_a, .data$location_a, .data$deployment_b, .data$location_b) |>
    dplyr::summarise(n_shared = dplyr::n(),
                     species = paste(sort(unique(stats::na.omit(.data$species))), collapse = ", "),
                     .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$n_shared)) |>
    as.data.frame()
  list(pairs = pairs, detail = df[order(df$hash), , drop = FALSE])
}

#' Validate the media cache on disk against the source metadata (disk vs the `media` resource).
#' Reports integrity problems — it does NOT flag un-cached events (the cache is lazy by design).
#' @param ik_data The container.
#' @return list: n_event_dirs · n_files · n_events_with_public_media · orphan_dirs (cached events no
#'   longer carrying public media) · orphan_files (a file whose mediaID isn't in that event's
#'   metadata) · zero_byte_files (corrupt/empty). @keywords internal
ik_validate_media_cache <- function(ik_data) {
  root  <- ik_media_cache_dir()
  dirs  <- list.dirs(root, recursive = FALSE, full.names = FALSE)            # eventIDs on disk
  files <- list.files(root, pattern = "\\.jpg$", recursive = TRUE, full.names = TRUE)
  m <- ik_media(ik_data, dataset = names(ik_data$datasets))
  m <- m[!is.na(m$filePublic) & m$filePublic, , drop = FALSE]
  meta_events <- unique(m$eventID)
  valid_mid   <- split(m$mediaID, m$eventID)                                 # eventID -> its mediaIDs

  ev  <- basename(dirname(files)); mid <- sub("(_display)?\\.jpg$", "", basename(files))
  orphan_file <- !mapply(function(e, i) i %in% (valid_mid[[e]] %||% character(0)), ev, mid)
  sizes <- file.info(files)$size

  list(
    n_event_dirs = length(dirs), n_files = length(files),
    n_events_with_public_media = length(meta_events),
    orphan_dirs     = setdiff(dirs, meta_events),       # cached event no longer in (public) metadata
    orphan_files    = files[orphan_file],               # file whose mediaID isn't in the event metadata
    zero_byte_files = files[is.na(sizes) | sizes == 0]  # corrupt / empty download
  )
}
