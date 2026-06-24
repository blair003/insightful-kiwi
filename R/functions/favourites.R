# favourites.R — the "Highlights" feed: curated favourite images (camera media flagged `favorite`
# in the dataset) for a gallery, plus pre-warming them into the local media cache. Builds on
# media.R (access) + media_cache.R (cache layout, download/resize worker, web paths). Favourites are
# a curation tag, NOT a sentiment — the set includes pests, so callers should label it "Highlights".

#' Cheap check: does the project have any public favourite media? (For conditionally showing the
#' Highlights nav.) Scans the media table only — no observation join. @keywords internal
ik_has_favourites <- function(ik_data) {
  m <- ik_media(ik_data)
  !is.null(m) && nrow(m) > 0 && any(!is.na(m$favorite) & m$favorite & !is.na(m$filePublic) & m$filePublic)
}

#' Manifest of favourite images across the project.
#'
#' One row per favourite media, enriched with the species + place from its event's observation, so a
#' gallery can caption each image. Public-only by default (private images can't be fetched from source).
#'
#' @param ik_data     The container.
#' @param public_only Keep only `filePublic` media (default TRUE).
#' @return tibble (mediaID·eventID·deploymentID·timestamp·filePath·fileName·scientificName·label·
#'   reserve·location·dataset), newest first; or NULL when there are none.
ik_favourite_media <- function(ik_data, public_only = TRUE) {
  m <- ik_media(ik_data)
  if (is.null(m) || !nrow(m)) return(NULL)
  m <- m[!is.na(m$favorite) & m$favorite, , drop = FALSE]
  if (public_only) m <- m[!is.na(m$filePublic) & m$filePublic, , drop = FALSE]
  if (!nrow(m)) return(NULL)

  # species + place from the event's observation; prefer an animal row over blank/unknown, one per event.
  o <- ik_observations(ik_data, with_location = TRUE)
  o <- o[!is.na(o$eventID), , drop = FALSE]
  o <- o[order(o$observationType != "animal"), , drop = FALSE]          # animal rows sort first
  ev <- o[!duplicated(o$eventID), , drop = FALSE]
  locs <- ik_data$app$geography$locations                              # reserve isn't on the obs join
  ev$reserve <- locs$reserve[match(ev$locationID, locs$location_id)]
  i  <- match(m$eventID, ev$eventID)

  out <- data.frame(
    mediaID = m$mediaID, eventID = m$eventID, deploymentID = m$deploymentID,
    timestamp = m$timestamp, filePath = m$filePath, fileName = m$fileName,
    scientificName = ev$scientificName[i],
    label   = ik_species_label(ev$scientificName[i], ik_data, "vernacular"),
    reserve = ev$reserve[i], location = ev$locationName[i], dataset = m$dataset,
    stringsAsFactors = FALSE)
  out[order(out$timestamp, decreasing = TRUE), , drop = FALSE]
}

#' Favourite manifest + cache state, ready for a gallery.
#'
#' Adds `cached` (is the resized display already on disk?) and `src` (the local web path when cached,
#' else the live source URL) to `ik_favourite_media()`. FAST — pure file-existence checks, no I/O to
#' source. Optionally filter to one species' favourites (for the species page).
#'
#' @param ik_data The container. @param scientific Optional scientificName(s) to filter to.
#' @param public_only Keep only public media (default TRUE).
#' @return the manifest with `cached`/`src`, or NULL when there are none.
ik_favourite_media_view <- function(ik_data, scientific = NULL, public_only = TRUE) {
  f <- ik_favourite_media(ik_data, public_only)
  if (is.null(f) || !nrow(f)) return(NULL)
  if (!is.null(scientific)) f <- f[f$scientificName %in% scientific, , drop = FALSE]
  if (!nrow(f)) return(NULL)
  disp <- vapply(seq_len(nrow(f)), function(i) .media_paths(f$eventID[i], f$mediaID[i])$disp, character(1))
  f$cached <- file.exists(disp)
  f$src    <- ifelse(f$cached, mapply(function(e, d) .media_web(e, basename(d)), f$eventID, disp), f$filePath)
  f
}

#' Pre-warm the media cache with every favourite image not yet cached — in ONE background process.
#'
#' Downloads + resizes the missing favourites so the Highlights gallery serves from disk. Groups by
#' event (the cache is event-keyed), then hands a batched payload to a detached Rscript so it never
#' blocks the session and survives its GC. No-op when everything is already cached.
#'
#' @param ik_data The container. @param width Display width (px).
#' @return Invisibly the number of favourite media queued (0 when none need caching).
ik_cache_favourites_async <- function(ik_data, width = IK_MEDIA_DISPLAY_WIDTH) {
  f <- ik_favourite_media(ik_data, public_only = TRUE)
  if (is.null(f) || !nrow(f)) return(invisible(0L))
  need <- !vapply(seq_len(nrow(f)), function(i) file.exists(.media_paths(f$eventID[i], f$mediaID[i])$disp), logical(1))
  f <- f[need, , drop = FALSE]
  if (!nrow(f)) return(invisible(0L))

  events <- lapply(split(f, f$eventID), function(g) list(
    event_dir = .media_event_dir_abs(g$eventID[1]),
    rows      = data.frame(mediaID = g$mediaID, filePath = g$filePath, stringsAsFactors = FALSE)))
  payload <- list(
    worker = normalizePath(file.path("R", "functions", "media_cache.R")),
    events = unname(events), width = width,
    keep_originals = isTRUE(ik_data$meta$media$keep_originals %||% TRUE))
  rds <- tempfile(fileext = ".rds"); saveRDS(payload, rds)
  job <- normalizePath(file.path("R", "jobs", "cache_favourites_job.R"))
  system2("Rscript", c(shQuote(job), shQuote(rds)), wait = FALSE, stdout = FALSE, stderr = FALSE)
  invisible(nrow(f))
}
