# data_cache.R — cache ik_data as .RDS and re-import only when inputs change.
# The cache is keyed on a fingerprint of the import inputs (the manifest + each
# package's files by size + mtime) plus a structure version. Any change to the
# manifest, the package files, or IK_CACHE_VERSION forces a re-import.

# Bump when a change to BUILD LOGIC or shape changes ik_data's CONTENT (new $meta tags,
# $app structure, a derivation tweak in geography/period/temporal/...). The fingerprint
# tracks the manifest + package DATA files + the CONVERSION code (R/functions/import) — but
# NOT the rest of the build code, so bump this (or run with IK_REBUILD=1) when you edit a
# builder outside import/ and want the cache rebuilt.
IK_CACHE_VERSION <- 38L   # v38: meta$features (per-feature on/off flags from project.R)

#' Fingerprint the import inputs (manifest + package files + conversion code).
#'
#' Cheap — file metadata only, no content hashing: two fingerprints compare equal
#' iff the manifest, every enabled package's files, and the conversion code
#' (R/functions/import) are unchanged. Conversion code is included so that editing a
#' converter/template re-imports (it otherwise serves a stale generated package).
#' Build code OUTSIDE import/ (geography, period, ...) is NOT tracked — bump
#' IK_CACHE_VERSION or run with IK_REBUILD=1 after editing those.
#'
#' @param config   Runtime config (see build_config()).
#' @param manifest The dataset manifest (see load_manifest()).
#' @return A list used as the cache key.
source_fingerprint <- function(config, manifest, project = list()) {
  files <- lapply(names(manifest), function(id) {
    entry <- manifest[[id]]
    if (isFALSE(entry$enabled)) return(NULL)
    dir <- if (!is.null(entry$raw)) {
      file.path(config$dirs$raw, entry$raw$dir %||% id)        # raw inputs
    } else {
      file.path(config$dirs$packages, entry$dir %||% id)       # received package
    }
    dir_fingerprint(dir)
  })
  names(files) <- names(manifest)
  list(manifest = manifest, project = project, files = files,
       import_code = dir_fingerprint("R/functions/import"))   # converter/template edits re-import
}

#' File-metadata fingerprint of a directory (size + mtime; no content hashing).
#' @keywords internal
dir_fingerprint <- function(dir) {
  paths <- sort(list.files(dir, recursive = TRUE, full.names = TRUE))
  info  <- file.info(paths)
  list(file = basename(paths), size = info$size, mtime = info$mtime)
}

#' Load ik_data from cache, re-importing only if the inputs changed.
#'
#' @param config Runtime config (see build_config()).
#' @param force  Rebuild and rewrite the cache regardless of freshness.
#' @return ik_data (see build_ik_data()).
load_or_build_ik_data <- function(config, force = FALSE) {
  manifest    <- load_manifest(config)
  project     <- load_project_config(config)
  fingerprint <- source_fingerprint(config, manifest, project)
  cache_path  <- file.path(config$dirs$cache, "ik_data.rds")

  if (!force && file.exists(cache_path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    fresh  <- !is.null(cached) &&
      identical(cached$version, IK_CACHE_VERSION) &&
      identical(cached$fingerprint, fingerprint)
    if (fresh) {
      logger::log_info("ik_data: cache hit (%s) — loaded without re-import.", cache_path)
      return(cached$ik_data)
    }
    logger::log_info("ik_data: cache stale — re-importing.")
  } else if (!force) {
    logger::log_info("ik_data: no cache — importing.")
  }

  ik_data <- build_ik_data(config, manifest, project)

  dir.create(config$dirs$cache, recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(version = IK_CACHE_VERSION, fingerprint = fingerprint, ik_data = ik_data),
    cache_path
  )
  logger::log_info("ik_data: cached to %s", cache_path)
  ik_data
}
