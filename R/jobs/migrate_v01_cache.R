#!/usr/bin/env Rscript
# migrate_v01_cache.R — ONE-TIME migration of the v0.1 image cache to the v1.0 layout.
#
# v0.1 cached by mediaID: instance-v0.1/www/media-cache/<mediaID>/<fileName>.JPG (original)
#                                                                /<fileName>_resized.JPG
# v1.0 caches by eventID: instance/www/media-cache/<eventID>/<mediaID>.jpg        (original)
#                                                            /<mediaID>_display.jpg (1200px)
#
# For each old dir it resolves the eventID from the current wkt_camera_monitoring media
# table, copies the ORIGINAL, and regenerates the display with the live resize logic
# (.media_resize). Idempotent — skips entries whose display already exists, so it is safe to
# re-run / resume. Partial sequences are fine: an event dir may end up with only the media
# v0.1 had cached; the viewer fills the rest from Agouti on demand.
#
# Usage:  Rscript R/jobs/migrate_v01_cache.R [limit]   # limit = process first N dirs (test)

# Resizing thousands of images in one process exhausts ImageMagick's pixel cache; raise the
# limits (read at magick init) and gc() periodically below so unreferenced images are freed.
Sys.setenv(MAGICK_MEMORY_LIMIT = "2GiB", MAGICK_MAP_LIMIT = "4GiB",
           MAGICK_DISK_LIMIT = "16GiB", MAGICK_AREA_LIMIT = "2GP")
suppressMessages(source("global.R"))

old_root <- "instance-v0.1/www/media-cache"
new_root <- ik_media_cache_dir()
args     <- commandArgs(trailingOnly = TRUE)
limit    <- if (length(args)) suppressWarnings(as.integer(args[1])) else NA_integer_

m  <- ik_media(ik_data, "wkt_camera_monitoring")
ev <- stats::setNames(m$eventID, m$mediaID)          # mediaID -> eventID

dirs <- list.dirs(old_root, recursive = FALSE, full.names = FALSE)
if (!is.na(limit)) dirs <- utils::head(dirs, limit)
n <- length(dirs)
cat(sprintf("Converting %d v0.1 cache dir(s) -> %s\n", n, new_root))

cc <- list(converted = 0L, already = 0L, no_match = 0L, no_original = 0L, error = 0L)
t0 <- Sys.time()
for (i in seq_len(n)) {
  mid <- dirs[i]
  e   <- unname(ev[mid])                                  # `[` → NA if absent (`[[` would error)
  if (is.na(e)) { cc$no_match <- cc$no_match + 1L; next }

  new_dir  <- file.path(new_root, e)
  new_orig <- file.path(new_dir, paste0(mid, ".jpg"))
  new_disp <- file.path(new_dir, paste0(mid, IK_MEDIA_DISPLAY_SUFFIX))
  if (file.exists(new_disp)) { cc$already <- cc$already + 1L; next }

  files <- list.files(file.path(old_root, mid), full.names = TRUE)   # original = non-resized image
  orig  <- files[!grepl("(_resized\\.[A-Za-z]+|\\.part)$", files) & !dir.exists(files)]
  if (!length(orig)) { cc$no_original <- cc$no_original + 1L; next }

  ok <- tryCatch({
    dir.create(new_dir, recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(new_orig)) file.copy(orig[1], new_orig)
    isTRUE(.media_resize(new_orig, new_disp, IK_MEDIA_DISPLAY_WIDTH))
  }, error = function(err) FALSE)
  if (isTRUE(ok)) cc$converted <- cc$converted + 1L else cc$error <- cc$error + 1L

  if (i %% 10L == 0L) gc()                                # release magick pixel cache
  if (i %% 500L == 0L)
    cat(sprintf("  [%d/%d] conv=%d already=%d no_match=%d no_orig=%d err=%d (%.0fs)\n",
                i, n, cc$converted, cc$already, cc$no_match, cc$no_original, cc$error,
                as.numeric(Sys.time() - t0, units = "secs")))
}
cat(sprintf("DONE in %.0fs: conv=%d already=%d no_match=%d no_orig=%d err=%d\n",
            as.numeric(Sys.time() - t0, units = "secs"),
            cc$converted, cc$already, cc$no_match, cc$no_original, cc$error))
