#!/usr/bin/env Rscript
# media_validate_job.R — run the media-cache diagnostics from the CLI, e.g.:
#   Rscript R/jobs/media_validate_job.R              # report to stdout
#   Rscript R/jobs/media_validate_job.R out.csv      # also write the duplicate-pair table to CSV
# Read-only. Run from the app root (it sources global.R). The content-hash scan reads every cached
# file, so it takes a minute or two on a large cache.

suppressWarnings(suppressMessages(source("global.R")))
args   <- commandArgs(trailingOnly = TRUE)
out_csv <- if (length(args)) args[[1]] else NA_character_

cat("=== 1. Duplicate deployment WINDOWS (camera; same start+end across locations) ===\n")
dw <- ik_duplicate_deployment_windows(ik_data)
if (nrow(dw)) print(dw, row.names = FALSE) else cat("  none\n")

cat("\n=== 2. Duplicate MEDIA (byte-identical images shared across deployments) ===\n")
dup <- ik_media_duplicate_scan(ik_data)
if (nrow(dup$pairs)) {
  print(dup$pairs, row.names = FALSE)
  cat(sprintf("\n  %d deployment pair(s), %d duplicated images total.\n",
              nrow(dup$pairs), nrow(dup$detail)))
  if (!is.na(out_csv)) { utils::write.csv(dup$detail, out_csv, row.names = FALSE)
    cat("  detail written to", out_csv, "\n") }
} else cat("  none\n")

cat("\n=== 3. Cache integrity (disk vs metadata) ===\n")
v <- ik_validate_media_cache(ik_data)
cat(sprintf("  event dirs: %d | files: %d | events with public media (metadata): %d\n",
            v$n_event_dirs, v$n_files, v$n_events_with_public_media))
cat(sprintf("  orphan dirs: %d | orphan files: %d | zero-byte files: %d\n",
            length(v$orphan_dirs), length(v$orphan_files), length(v$zero_byte_files)))
if (length(v$zero_byte_files)) { cat("  zero-byte (first 5):\n"); cat(paste0("    ", utils::head(v$zero_byte_files, 5)), sep = "\n"); cat("\n") }
