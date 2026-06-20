# cache_media_job.R — detached background worker for ik_cache_event_async(). Run as:
#   Rscript R/jobs/cache_media_job.R <payload.rds>
# The payload (written by ik_cache_event_async) holds the media rows, the absolute event
# cache directory, the resize width, and the absolute path to media_cache.R. Self-contained:
# sources only media_cache.R (which uses curl/magick/logger via ::), needs no app/ik_data.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !file.exists(args[1])) quit(save = "no", status = 1L)

job <- readRDS(args[1])
source(job$worker)                                   # ik_cache_media_rows + helpers
try(ik_cache_media_rows(job$rows, job$event_dir, job$width), silent = TRUE)
unlink(args[1])                                      # drop the payload
