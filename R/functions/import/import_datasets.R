# import_datasets.R — read the manifested Camtrap DP packages into the $datasets
# layer of ik_data. Each package is read with camtrapdp::read_camtrapdp(), kept
# PRISTINE in $package, and tagged into $meta from the manifest. The manifest
# contract lives in instance/config/datasets.R.

#' Load the dataset manifest.
#'
#' @param config Runtime config (see build_config()).
#' @return The named `datasets` list defined in instance/config/datasets.R.
load_manifest <- function(config) {
  path <- file.path(config$env$dirs$config, "datasets.R")
  if (!file.exists(path)) {
    stop(sprintf("No dataset manifest at '%s'.", path), call. = FALSE)
  }
  source(path, local = TRUE)$value
}

#' Read and tag one manifested dataset.
#'
#' Reads the package pristine and validates it; the manifest tags become $meta.
#'
#' @param id           Dataset id (manifest list name; the ik_data$datasets key).
#' @param entry        That dataset's manifest entry (tags + optional `dir`).
#' @param packages_dir Directory holding the package folders.
#' @return A list `{ package, meta }` for ik_data$datasets$<id>.
ik_read_dataset <- function(id, entry, packages_dir) {
  dir        <- entry$dir %||% id
  path       <- file.path(packages_dir, dir)
  descriptor <- file.path(path, "datapackage.json")

  if (!file.exists(descriptor)) {
    stop(sprintf(
      "Dataset '%s': no datapackage.json at '%s' (manifested but missing on disk).",
      id, path
    ), call. = FALSE)
  }

  logger::log_info("Importing dataset '%s' from '%s'", id, path)
  package <- camtrapdp::read_camtrapdp(descriptor)

  # Guard the pristine object; add dataset context to any validation failure.
  tryCatch(
    camtrapdp::check_camtrapdp(package),
    error = function(e) {
      stop(sprintf("Dataset '%s' failed Camtrap DP validation: %s", id, conditionMessage(e)),
           call. = FALSE)
    }
  )

  list(
    package = package,
    meta = list(
      id          = id,
      source_type = entry$source_type,
      project     = entry$project,
      method      = entry$method,
      name        = entry$name %||% id
    )
  )
}

#' Import all manifested datasets, reconciling against what's on disk.
#'
#' Reconciliation: present-but-unlisted folders warn and are skipped (error if
#' `strict`); listed-but-missing folders error (in ik_read_dataset); entries with
#' `enabled = FALSE` are skipped.
#'
#' @param manifest     The dataset manifest (see load_manifest()).
#' @param packages_dir Directory holding the package folders.
#' @param strict       Promote unregistered-package warnings to errors.
#' @return Named list of `{ package, meta }`, one per imported dataset.
ik_import_datasets <- function(manifest, packages_dir, strict = FALSE) {
  if (!dir.exists(packages_dir)) {
    stop(sprintf("Packages directory '%s' does not exist.", packages_dir), call. = FALSE)
  }

  on_disk     <- list.dirs(packages_dir, full.names = FALSE, recursive = FALSE)
  listed_dirs <- vapply(manifest, function(e) e$dir %||% NA_character_, character(1))
  listed_dirs <- ifelse(is.na(listed_dirs), names(manifest), listed_dirs)

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
    datasets[[id]] <- ik_read_dataset(id, entry, packages_dir)
  }

  logger::log_info(
    "Imported %d dataset(s): %s",
    length(datasets), paste(names(datasets), collapse = ", ")
  )
  datasets
}
