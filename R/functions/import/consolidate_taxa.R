# consolidate_taxa.R — apply per-dataset taxon consolidation rules (from the
# manifest `consolidate_taxa` field) using the package-native camtrapdp::update_taxon().
# Rolls fine taxa up to a coarser one, e.g. all Rattus species -> genus Rattus.
# Applied at import; the package stays a valid camtrapdp object.

#' Apply a dataset's taxon-consolidation rules to its package.
#'
#' @param package A camtrapdp object.
#' @param rules   List of rules from the manifest. Each rule supports:
#'   - `genus = "Rattus"` : consolidate every binomial under that genus to the genus.
#'   - `from = c(...), to = "X"` : consolidate the listed scientificNames to taxon X.
#' @return The package with consolidations applied.
apply_taxon_consolidation <- function(package, rules) {
  if (is.null(rules) || length(rules) == 0) return(package)
  for (rule in rules) {
    if (!is.null(rule$genus)) {
      package <- consolidate_to_genus(package, rule$genus)
    } else if (!is.null(rule$to) && !is.null(rule$from)) {
      package <- consolidate_to_taxon(package, rule$from, rule$to)
    }
  }
  package
}

#' Consolidate every binomial under a genus to the genus taxon. @keywords internal
consolidate_to_genus <- function(package, genus) {
  tx     <- camtrapdp::taxa(package)
  target <- tx[tx$scientificName == genus, , drop = FALSE]
  if (nrow(target) == 0) {
    logger::log_warn("consolidate: genus '%s' not present in taxa — skipped.", genus)
    return(package)
  }
  species <- tx$scientificName[startsWith(tx$scientificName, paste0(genus, " "))]
  package <- consolidate_to_taxon(package, species, as.list(target[1, ]))
  if (length(species)) {
    logger::log_info("consolidate: %d taxa -> '%s' (%s)",
                     length(species), genus, paste(species, collapse = ", "))
  }
  package
}

#' Consolidate `from` scientificName(s) to taxon `to`. @keywords internal
consolidate_to_taxon <- function(package, from, to) {
  if (is.character(to)) {
    tx     <- camtrapdp::taxa(package)
    target <- tx[tx$scientificName == to, , drop = FALSE]
    if (nrow(target) == 0) {
      logger::log_warn("consolidate: target taxon '%s' not present — skipped.", to)
      return(package)
    }
    to <- as.list(target[1, ])
  }
  for (sp in from) {
    package <- suppressMessages(camtrapdp::update_taxon(package, from = sp, to = to))
  }
  package
}
