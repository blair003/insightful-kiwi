#!/usr/bin/env Rscript
# media_cache_report.R — summarise the image media cache (instance/www/media-cache).
#
# The cache stores, per viewed event: the ORIGINAL frames (<mediaID>.jpg) AND a resized DISPLAY
# copy (<mediaID>_display.jpg, 1200px wide, q85). This reports the total footprint and the split,
# so we can judge whether the display-resize is worth its disk: keeping both costs the display
# bytes on top of the originals, but serving the small display saves bandwidth on every view.
#
# Run from the app root:
#   Rscript tools/media_cache_report.R            # report only
#   Rscript tools/media_cache_report.R --prune    # ALSO delete originals that have a display copy
#                                                 # (reclaim disk after going display-only; irreversible)

prune <- "--prune" %in% commandArgs(trailingOnly = TRUE)
dir <- file.path("instance", "www", "media-cache")
if (!dir.exists(dir)) { cat("No media cache at", dir, "\n"); quit(status = 0) }

files <- list.files(dir, recursive = TRUE, full.names = TRUE, pattern = "\\.jpg$")
if (!length(files)) { cat("Media cache is empty (", dir, ").\n", sep = ""); quit(status = 0) }

sz      <- file.info(files)$size
is_disp <- grepl("_display\\.jpg$", files)
events  <- length(list.dirs(dir, recursive = FALSE))
hsize   <- function(b) {
  u <- c("B", "KB", "MB", "GB", "TB"); i <- if (b <= 0) 1 else min(length(u), floor(log(b, 1024)) + 1)
  sprintf("%.1f %s", b / 1024^(i - 1), u[i])
}

orig <- sz[!is_disp]; disp <- sz[is_disp]
cat(sprintf("Media cache: %s\n\n", normalizePath(dir)))
cat(sprintf("Events cached:   %d\n", events))
cat(sprintf("Files:           %d  (%d originals · %d display)\n", length(files), length(orig), length(disp)))
cat(sprintf("TOTAL on disk:   %s\n\n", hsize(sum(sz))))
if (length(orig)) cat(sprintf("Originals:       %s   (mean %s/frame)\n", hsize(sum(orig)), hsize(mean(orig))))
if (length(disp)) cat(sprintf("Display copies:  %s   (mean %s/frame)\n", hsize(sum(disp)), hsize(mean(disp))))

if (length(orig) && length(disp)) {
  cat(sprintf("\nA display copy is ~%.0f%% the size of an original (mean).\n",
              100 * mean(disp) / mean(orig)))
  cat("\nWhat each policy would cost on disk (for the events cached so far):\n")
  cat(sprintf("  keep both (current):  %s\n", hsize(sum(sz))))
  cat(sprintf("  display only:         %s   (drop originals, -%s; lose full-res download)\n",
              hsize(sum(disp)), hsize(sum(orig))))
  cat(sprintf("  originals only:       %s   (no resize, -%s disk; +bandwidth, every view streams full-res)\n",
              hsize(sum(orig)), hsize(sum(disp))))
}

if (prune) {
  # delete each original that already has a display sibling — the display-only reclaim
  cand   <- sub("_display\\.jpg$", ".jpg", files[is_disp])
  to_del <- cand[file.exists(cand)]
  freed  <- if (length(to_del)) sum(file.info(to_del)$size) else 0
  unlink(to_del)
  cat(sprintf("\nPRUNED %d originals (each had a display copy) — freed %s.\n", length(to_del), hsize(freed)))
} else if (length(disp)) {
  cat("\n(Run with --prune to delete originals that already have a display copy.)\n")
}
