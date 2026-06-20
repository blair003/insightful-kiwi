# taxonomy.R — access helpers for the shared species lookup. ik_species_label()
# maps scientific names to display labels, honouring the user's name preference
# (Settings: "Prefer scientific names") and falling back to the other name where
# the preferred one is missing. Taxonomy is joined on demand, never widened onto
# the fact tables.

#' The shared taxonomy lookup.
#'
#' @param ik_data The ik_data container.
#' @return The taxonomy tibble (see build_taxonomy()).
ik_taxonomy <- function(ik_data) {
  ik_data$app$taxonomy
}

#' Display label(s) for scientific name(s).
#'
#' Vectorised: pass a column of scientificName, get display labels back.
#'
#' @param scientific Character vector of scientific names.
#' @param ik_data    The ik_data container.
#' @param prefer     "vernacular" (default) or "scientific". The other name is
#'   used as a fallback wherever the preferred one is missing.
#' @return Character vector of labels, the same length as `scientific`.
ik_species_label <- function(scientific, ik_data, prefer = c("vernacular", "scientific")) {
  prefer     <- match.arg(prefer)
  tax        <- ik_taxonomy(ik_data)
  vernacular <- tax$vernacular_eng[match(scientific, tax$scientificName)]

  preferred <- if (prefer == "scientific") scientific else vernacular
  fallback  <- if (prefer == "scientific") vernacular else scientific

  missing <- is.na(preferred) | !nzchar(preferred)
  preferred[missing] <- fallback[missing]
  preferred
}
