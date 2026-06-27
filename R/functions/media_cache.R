# media_cache.R — local cache of camera image bursts, keyed by eventID. Agouti is slow
# (1-2 Mbps), so we download each viewed sequence once and serve it from disk thereafter.
#
# Layout: instance/www/media-cache/<eventID>/<mediaID>.jpg          (original; retained unless
#                                                                    project.R media$keep_originals=FALSE)
#                                            /<mediaID>__display.jpg (1200px, aspect kept)
# instance/www/media-cache is symlinked as www/media-cache, so a file there serves at the
# web path "media-cache/<eventID>/<file>". Two split responsibilities:
#   ik_event_media_view()  — FAST, no I/O: cached path if present, else the Agouti URL. The
#                            viewer renders this immediately (first view streams from Agouti).
#   ik_cache_event()       — SLOW: download original + write the resized display. Run in the
#                            background so the next view (any user) is instant and Agouti is
#                            spared. Self-contained per media, robust to network failure.

IK_MEDIA_DISPLAY_WIDTH  <- 1200L
IK_MEDIA_WEB_BASE       <- "media-cache"
IK_MEDIA_DISPLAY_SUFFIX <- "_display.jpg"   # <mediaID>_display.jpg beside <mediaID>.jpg

#' Filesystem root of the media cache (under instance/www, symlinked into www). @keywords internal
ik_media_cache_dir <- function() file.path("instance", "www", "media-cache")

#' Ensure the media-cache directory AND its `www/` symlink exist, so cached images serve at the web
#' path `media-cache/...` regardless of how the app was deployed. The symlink is committed to git, but
#' a deploy that doesn't preserve symlinks (rsync/tar without `-l`, a Windows / `core.symlinks=false`
#' checkout) leaves it missing or as a plain file, and a fresh checkout has no `instance/www/media-cache`
#' target at all — so cached files get written but never served (the viewer silently falls back to the
#' slow Agouti URLs). Run once at startup (global.R). Idempotent; no-ops when already correct, never
#' clobbers a real directory, and logs a WARNING (rather than erroring) on any permission failure so a
#' read-only/locked-down host degrades to live-Agouti instead of crashing the app.
#' @return invisibly TRUE when the cache is serveable, FALSE otherwise. @keywords internal
ik_ensure_media_cache_link <- function() {
  target <- ik_media_cache_dir()                                # "instance/www/media-cache" (rel. to app root)
  link   <- file.path("www", "media-cache")
  rel    <- file.path("..", "instance", "www", "media-cache")   # the link's target, relative to www/

  # 1) The real cache directory under instance/ — create it if missing.
  if (!dir.exists(target) && !dir.create(target, recursive = TRUE, showWarnings = FALSE)) {
    logger::log_warn("media-cache: could not create '%s' (check permissions) — cached images will not be saved.", target)
    return(invisible(FALSE))
  }

  # 2) The www/ symlink that serves it. Sys.readlink: the target for a symlink, "" for a non-symlink,
  #    NA when the path can't be read — treat the latter two as "not a symlink".
  cur        <- suppressWarnings(Sys.readlink(link))
  is_symlink <- !is.na(cur) && nzchar(cur)
  if (is_symlink) {
    if (identical(normalizePath(link, mustWork = FALSE), normalizePath(target, mustWork = FALSE)))
      return(invisible(TRUE))                                   # already points at our target — done
    unlink(link)                                                # dangling / wrong target — replace it
  } else if (file.exists(link)) {
    logger::log_warn(paste0("media-cache: '%s' exists but is NOT a symlink to '%s' — cached images may not ",
                            "serve. Remove it and run: ln -s %s %s"), link, target, rel, link)
    return(invisible(FALSE))                                    # a real file/dir — never clobber (may hold data)
  }

  ok <- tryCatch(file.symlink(rel, link),
                 error = function(e) { logger::log_warn("media-cache symlink: %s", conditionMessage(e)); FALSE })
  if (isTRUE(ok)) logger::log_info("media-cache: created symlink '%s' -> '%s'.", link, rel)
  else logger::log_warn("media-cache: could not create symlink '%s' -> '%s' (check permissions) — cached images will not serve.", link, rel)
  invisible(isTRUE(ok))
}

#' Web path for a cached file (relative to the app root Shiny serves). @keywords internal
.media_web <- function(event_id, file) paste(IK_MEDIA_WEB_BASE, event_id, file, sep = "/")

#' Per-media cache filenames (original + display), absolute under the cache dir. @keywords internal
.media_paths <- function(event_id, media_id) {
  dir <- file.path(ik_media_cache_dir(), event_id)
  list(dir = dir,
       orig = file.path(dir, paste0(media_id, ".jpg")),
       disp = file.path(dir, paste0(media_id, IK_MEDIA_DISPLAY_SUFFIX)))
}

#' Download a URL to `dest` atomically (.part → rename). FALSE (not error) on failure.
#' @keywords internal
.media_download <- function(url, dest, timeout = 120L) {
  if (is.na(url) || !nzchar(url)) return(FALSE)
  tmp <- paste0(dest, ".part")
  tryCatch({
    curl::curl_download(url, tmp, mode = "wb",
                        handle = curl::new_handle(timeout = timeout, connecttimeout = 20L))
    if (file.exists(tmp) && file.info(tmp)$size > 0) { file.rename(tmp, dest); TRUE }
    else { unlink(tmp); FALSE }
  }, error = function(e) {
    unlink(tmp); logger::log_warn("media download failed (%s): %s", url, conditionMessage(e)); FALSE
  })
}

#' Write a width-capped JPEG of `src` to `dest` (aspect kept, never upscaled). @keywords internal
.media_resize <- function(src, dest, width = IK_MEDIA_DISPLAY_WIDTH) {
  tmp <- paste0(dest, ".part")
  tryCatch({
    img  <- magick::image_read(src)
    info <- magick::image_info(img)
    out  <- if (isTRUE(info$width[1] > width)) magick::image_resize(img, paste0(width, "x")) else img
    magick::image_write(out, tmp, format = "jpeg", quality = 85L)
    file.rename(tmp, dest); TRUE
  }, error = function(e) {
    unlink(tmp); logger::log_warn("media resize failed (%s): %s", src, conditionMessage(e)); FALSE
  })
}

#' View model for an event's burst — FAST, no downloads.
#'
#' @param ik_data  The ik_data container.
#' @param event_id The event/observation id.
#' @return data.frame (mediaID·timestamp·fileName·src·original·cached) or NULL when no public
#'   media. `src`/`original` are the cached web paths when present, else the live Agouti URL,
#'   so the viewer always has a usable source. `cached` flags whether the display is local.
ik_event_media_view <- function(ik_data, event_id) {
  m <- ik_event_media(ik_data, event_id, public_only = TRUE)
  if (is.null(m) || !nrow(m)) return(NULL)
  do.call(rbind, lapply(seq_len(nrow(m)), function(i) {
    p <- .media_paths(event_id, m$mediaID[i])
    has_disp <- file.exists(p$disp); has_orig <- file.exists(p$orig)
    data.frame(
      mediaID  = m$mediaID[i], timestamp = m$timestamp[i], fileName = m$fileName[i],
      src      = if (has_disp) .media_web(event_id, basename(p$disp)) else m$filePath[i],
      original = if (has_orig) .media_web(event_id, basename(p$orig)) else m$filePath[i],
      cached   = has_disp, stringsAsFactors = FALSE
    )
  }))
}

#' Cache a list of media (mediaID + filePath) into an ABSOLUTE event directory.
#'
#' The wd-independent worker shared by the synchronous and background paths — depends only on
#' curl + magick (+ logger), never on ik_data, so a bare Rscript can run it. Idempotent;
#' self-contained per media so one network failure doesn't sink the rest.
#'
#' @param media_df  data.frame with `mediaID` and `filePath` columns.
#' @param event_dir Absolute path of the event's cache directory.
#' @param width     Display width in px.
#' @return Invisibly, the count of media whose display is now present.
ik_cache_media_rows <- function(media_df, event_dir, width = IK_MEDIA_DISPLAY_WIDTH,
                                keep_originals = TRUE) {
  dir.create(event_dir, recursive = TRUE, showWarnings = FALSE)
  done <- 0L
  for (i in seq_len(nrow(media_df))) {
    orig <- file.path(event_dir, paste0(media_df$mediaID[i], ".jpg"))
    disp <- file.path(event_dir, paste0(media_df$mediaID[i], IK_MEDIA_DISPLAY_SUFFIX))
    ok_orig <- file.exists(orig) || .media_download(media_df$filePath[i], orig)
    ok_disp <- file.exists(disp) || (ok_orig && .media_resize(orig, disp, width))
    # display-only mode: once the resized copy exists, drop the (large) original to save disk.
    # Runs even for already-cached events, so re-caching with the flag off prunes old originals.
    if (!isTRUE(keep_originals) && file.exists(disp) && file.exists(orig)) unlink(orig)
    if (ok_disp) done <- done + 1L
  }
  invisible(done)
}

#' Absolute path of an event's cache directory (wd-independent). @keywords internal
.media_event_dir_abs <- function(event_id) file.path(getwd(), ik_media_cache_dir(), event_id)

#' Cache an event's burst SYNCHRONOUSLY (downloads + resizes; blocks ~10s/image on Agouti).
#' For the favourite pre-warm and tests; the viewer uses the async path. @return the cached
#' view (see ik_event_media_view) invisibly, or NULL when no public media.
ik_cache_event <- function(ik_data, event_id, width = IK_MEDIA_DISPLAY_WIDTH) {
  m <- ik_event_media(ik_data, event_id, public_only = TRUE)
  if (is.null(m) || !nrow(m)) return(invisible(NULL))
  ik_cache_media_rows(data.frame(mediaID = m$mediaID, filePath = m$filePath,
                                 stringsAsFactors = FALSE),
                      .media_event_dir_abs(event_id), width,
                      keep_originals = isTRUE(ik_data$meta$media$keep_originals %||% TRUE))
  invisible(ik_event_media_view(ik_data, event_id))
}

#' Cache an event's burst in the BACKGROUND (non-blocking), so the next view is instant.
#'
#' Resolves the media in-process (fast), then hands the uncached ones to a detached Rscript
#' job — an independent OS process, so it survives this session's GC and never blocks it. No-op
#' when nothing needs caching.
#'
#' @return Invisibly TRUE if a job was launched, FALSE/NULL otherwise.
ik_cache_event_async <- function(ik_data, event_id, width = IK_MEDIA_DISPLAY_WIDTH) {
  m <- ik_event_media(ik_data, event_id, public_only = TRUE)
  if (is.null(m) || !nrow(m)) return(invisible(FALSE))
  need <- !vapply(m$mediaID, function(id) file.exists(.media_paths(event_id, id)$disp), logical(1))
  if (!any(need)) return(invisible(FALSE))                       # already cached
  payload <- list(
    worker    = normalizePath(file.path("R", "functions", "media_cache.R")),
    rows      = data.frame(mediaID = m$mediaID[need], filePath = m$filePath[need],
                           stringsAsFactors = FALSE),
    event_dir = .media_event_dir_abs(event_id), width = width,
    keep_originals = isTRUE(ik_data$meta$media$keep_originals %||% TRUE))
  rds <- tempfile(fileext = ".rds"); saveRDS(payload, rds)
  job <- normalizePath(file.path("R", "jobs", "cache_media_job.R"))
  system2("Rscript", c(shQuote(job), shQuote(rds)), wait = FALSE, stdout = FALSE, stderr = FALSE)
  invisible(TRUE)
}
