# cache_favourites_job.R — detached batch worker for ik_cache_favourites_async(). Caches favourite
# image bursts across MANY events in one process. Run as:
#   Rscript R/jobs/cache_favourites_job.R <payload.rds>
# The payload (written by ik_cache_favourites_async) holds a list of {event_dir, rows} per event,
# the resize width, and the absolute path to media_cache.R. Self-contained: sources only
# media_cache.R (curl/magick/logger via ::), needs no app/ik_data.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !file.exists(args[1])) quit(save = "no", status = 1L)

job <- readRDS(args[1])
source(job$worker)                                   # ik_cache_media_rows + helpers
keep <- if (is.null(job$keep_originals)) TRUE else isTRUE(job$keep_originals)
for (ev in job$events)
  try(ik_cache_media_rows(ev$rows, ev$event_dir, job$width, keep_originals = keep), silent = TRUE)
unlink(args[1])                                      # drop the payload
