# import_datasets.R — read the manifested Camtrap DP packages into the $datasets
# layer of ik_data. Each package is read with camtrapdp::read_camtrapdp(), kept
# PRISTINE in $package, and tagged into $meta from the manifest. The manifest
# contract lives in instance/config/datasets.R.

#' Load the dataset manifest.
#'
#' @param config Runtime config (see build_config()).
#' @return The named `datasets` list defined in instance/config/datasets.R.
load_manifest <- function(config) {
  path <- file.path(config$dirs$config, "datasets.R")
  if (!file.exists(path)) {
    stop(sprintf("No dataset manifest at '%s'.", path), call. = FALSE)
  }
  source(path, local = TRUE)$value
}

#' Resolve a manifest entry to a ready datapackage.json path.
#'
#' Packaged entries point at `packages/<dir>`; raw entries are converted on import
#' (cached) into the generated-packages cache.
#'
#' @param id     Dataset id.
#' @param entry  Manifest entry.
#' @param config Runtime config (see build_config()).
#' @return Path to a datapackage.json ready for read_camtrapdp().
ik_materialize_package <- function(id, entry, config) {
  if (!is.null(entry$raw)) return(ik_convert_raw(id, entry, config))

  descriptor <- file.path(config$dirs$packages, entry$dir %||% id, "datapackage.json")
  if (!file.exists(descriptor)) {
    stop(sprintf("Dataset '%s': no datapackage.json at '%s' (manifested but missing on disk).",
                 id, dirname(descriptor)), call. = FALSE)
  }
  descriptor
}

#' Convert a raw dataset to a Camtrap DP package, cached by raw-input + code fingerprint.
#'
#' Re-runs the converter when the raw/reference files OR the conversion code change.
#' @keywords internal
ik_convert_raw <- function(id, entry, config) {
  raw_dir <- file.path(config$dirs$raw, entry$raw$dir %||% id)
  if (!dir.exists(raw_dir)) {
    stop(sprintf("Raw dataset '%s': directory '%s' not found.", id, raw_dir), call. = FALSE)
  }
  converter <- ik_registered("converter", entry$raw$converter)
  if (is.null(converter)) {
    stop(sprintf("Dataset '%s': no converter '%s' (see R/functions/import/converters.R).",
                 id, entry$raw$converter), call. = FALSE)
  }

  out_dir    <- file.path(config$dirs$cache, "generated-packages", id)
  descriptor <- file.path(out_dir, "datapackage.json")
  fp_file    <- file.path(out_dir, ".raw-fingerprint.rds")
  # Raw inputs AND conversion code: a converter/template edit must regenerate the package.
  # (This inner cache is consulted only on an outer rebuild; source_fingerprint() tracks
  # R/functions/import too, so the two layers invalidate together.)
  fp         <- list(raw = dir_fingerprint(raw_dir),
                     code = dir_fingerprint("R/functions/import"))

  if (file.exists(descriptor) && file.exists(fp_file) && identical(readRDS(fp_file), fp)) {
    logger::log_info("Raw dataset '%s': generated package up to date.", id)
    return(descriptor)
  }

  logger::log_info("Converting raw dataset '%s' via '%s' ...", id, entry$raw$converter)
  converter(raw_dir = raw_dir, out_dir = out_dir, meta = entry, dataset_id = id,
            taxonomy_cache = file.path(config$dirs$cache, "taxonomy.rds"))
  saveRDS(fp, fp_file)
  descriptor
}

#' Read and tag one manifested dataset (packaged or raw).
#'
#' @param id     Dataset id (the ik_data$datasets key).
#' @param entry  Manifest entry (tags + `dir`, or a `raw` block).
#' @param config Runtime config (see build_config()).
#' @return A list `{ package, meta }` for ik_data$datasets$<id>.
ik_read_dataset <- function(id, entry, config) {
  descriptor <- ik_materialize_package(id, entry, config)

  logger::log_info("Reading dataset '%s'.", id)
  package <- camtrapdp::read_camtrapdp(descriptor)
  tryCatch(
    camtrapdp::check_camtrapdp(package),
    error = function(e) {
      stop(sprintf("Dataset '%s' failed Camtrap DP validation: %s", id, conditionMessage(e)),
           call. = FALSE)
    }
  )

  # Package-native taxon consolidation (manifest rules), e.g. rats -> genus Rattus.
  package <- apply_taxon_consolidation(package, entry$consolidate_taxa)

  list(
    package = package,
    meta = list(
      id = id, source_type = entry$source_type, project = entry$project,
      method = entry$method, name = entry$name %||% id,
      timezone = entry$timezone, force_timezone = entry$force_timezone,
      temporal_resolution = entry$temporal_resolution,   # trustworthy timestamp granularity (optional)
      geography = entry$geography
    )
  )
}

#' Import all manifested datasets, reconciling packaged entries against disk.
#'
#' Reconciliation (packaged entries only): present-but-unlisted packages warn and
#' are skipped (error if `config$datasets$strict`); listed-but-missing error.
#' Raw entries are converted on import. `enabled = FALSE` entries are skipped.
#'
#' @param manifest The dataset manifest (see load_manifest()).
#' @param config   Runtime config (see build_config()).
#' @return Named list of `{ package, meta }`, one per imported dataset.
ik_import_datasets <- function(manifest, config) {
  packages_dir <- config$dirs$packages
  strict       <- isTRUE(config$datasets$strict)
  if (!dir.exists(packages_dir)) {
    stop(sprintf("Packages directory '%s' does not exist.", packages_dir), call. = FALSE)
  }

  # Reconcile only packaged (non-raw) entries against packages/. A package = any directory that
  # holds a datapackage.json (found at ANY depth), so a grouping folder like
  # packages/camera-monitoring/ that just CONTAINS packages isn't itself mistaken for one.
  packaged    <- Filter(function(e) is.null(e$raw), manifest)
  descriptors <- list.files(packages_dir, pattern = "^datapackage\\.json$", recursive = TRUE)
  on_disk     <- setdiff(unique(dirname(descriptors)), ".")   # relative package dirs (e.g. "camera-monitoring/x")
  listed_dirs <- vapply(packaged, function(e) e$dir %||% NA_character_, character(1))
  listed_dirs <- ifelse(is.na(listed_dirs), names(packaged), listed_dirs)

  for (folder in setdiff(on_disk, listed_dirs)) {
    msg <- sprintf(
      "Unregistered package '%s' in '%s' — not loaded. Add it to instance/config/datasets.R.",
      folder, packages_dir
    )
    if (strict) stop(msg, call. = FALSE) else logger::log_warn(msg)
  }

  datasets <- list()
  for (id in names(manifest)) {
    entry <- manifest[[id]]
    if (isFALSE(entry$enabled)) {
      logger::log_info("Dataset '%s' disabled in manifest — skipped.", id)
      next
    }
    datasets[[id]] <- ik_read_dataset(id, entry, config)
  }

  logger::log_info(
    "Imported %d dataset(s): %s",
    length(datasets), paste(names(datasets), collapse = ", ")
  )
  datasets
}
