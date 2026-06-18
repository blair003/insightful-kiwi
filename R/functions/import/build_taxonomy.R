# build_taxonomy.R — build the shared species lookup (ik_data$app$taxonomy) from
# every dataset's native taxa() table. Taxonomy is shared reference (a species
# means the same thing across datasets), so it lives in $app and is joined onto
# observations on demand — never widened onto the fact tables. Maps scientificName
# <-> English vernacular for display (see ik_species_label()).

#' Build the shared taxonomy lookup.
#'
#' Unions every dataset's `taxa()` and dedupes by scientificName, preferring the
#' row that carries an English vernacular name.
#'
#' @param datasets The ik_data$datasets list (each entry has `$package`).
#' @return A tibble: scientificName, taxonID, taxonRank, order, family,
#'   vernacular_eng.
build_taxonomy <- function(datasets) {
  # Pull a column as character, or NA where the package omits it.
  take <- function(df, name) {
    if (name %in% names(df)) as.character(df[[name]]) else rep(NA_character_, nrow(df))
  }

  per <- lapply(datasets, function(d) {
    tx <- camtrapdp::taxa(d$package)
    tibble::tibble(
      scientificName = take(tx, "scientificName"),
      taxonID        = take(tx, "taxonID"),
      taxonRank      = take(tx, "taxonRank"),
      order          = take(tx, "order"),
      family         = take(tx, "family"),
      vernacular_eng = take(tx, "vernacularNames.eng")
    )
  })

  combined <- dplyr::bind_rows(per)
  if (nrow(combined) == 0) return(combined)

  # Non-NA vernacular sorts first, so distinct() keeps the informative row.
  combined |>
    dplyr::arrange(.data$scientificName, is.na(.data$vernacular_eng)) |>
    dplyr::distinct(.data$scientificName, .keep_all = TRUE)
}
