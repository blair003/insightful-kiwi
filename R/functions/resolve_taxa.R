# resolve_taxa.R — resolve scientific names to taxonomy (taxonID, rank, higher
# classification, English vernacular) via the GBIF species API, instead of
# hard-coding it. Results are cached so only new names hit the network; offline or
# on error it falls back to names-only (NA taxonID) with a warning, never failing
# the import. App-level helper — any converter/builder with scientific names can use it.

GBIF_API <- "https://api.gbif.org/v1"

#' Resolve scientific names to taxonomy via GBIF.
#'
#' @param scientific_names Character vector of scientific names (duplicates allowed).
#' @param cache_path Optional .rds path to persist/reuse resolutions. When set,
#'   only names absent from the cache are queried.
#' @param online When FALSE, never hits the network (cache-only).
#' @return A tibble, one row per distinct input name: scientificName, taxonID,
#'   taxonRank, order, family, vernacular_eng, match_type.
ik_resolve_taxa <- function(scientific_names, cache_path = NULL, online = TRUE) {
  names_in <- unique(scientific_names[!is.na(scientific_names) & nzchar(scientific_names)])
  if (length(names_in) == 0) return(gbif_na_row(character(0)))

  cache    <- if (!is.null(cache_path) && file.exists(cache_path)) readRDS(cache_path) else NULL
  to_query <- setdiff(names_in, cache$scientificName)

  fetched <- list()
  if (length(to_query) && online) {
    for (nm in to_query) {
      fetched[[nm]] <- tryCatch(gbif_resolve_one(nm), error = function(e) {
        logger::log_warn("Taxon resolve failed for '%s': %s", nm, conditionMessage(e))
        gbif_na_row(nm)
      })
    }
  } else if (length(to_query)) {
    logger::log_warn("Taxonomy offline: %d name(s) left unresolved (%s).",
                     length(to_query), paste(to_query, collapse = ", "))
    fetched <- lapply(to_query, gbif_na_row)
  }

  resolved <- dplyr::bind_rows(cache, dplyr::bind_rows(fetched))

  if (!is.null(cache_path) && length(fetched)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(resolved, cache_path)
  }

  resolved[match(names_in, resolved$scientificName), , drop = FALSE]
}

#' Query GBIF for one name (match + English vernacular). @keywords internal
gbif_resolve_one <- function(name) {
  m <- httr2::request(paste0(GBIF_API, "/species/match")) |>
    httr2::req_url_query(name = name) |>
    httr2::req_timeout(15) |>
    httr2::req_user_agent("insightful.kiwi") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (is.null(m$usageKey) || identical(m$matchType, "NONE")) {
    logger::log_warn("GBIF: no match for '%s'.", name)
    return(gbif_na_row(name))
  }
  if (!identical(m$matchType, "EXACT")) {
    logger::log_warn("GBIF: %s match for '%s' -> '%s' (confidence %s) — check the name.",
                     m$matchType, name, m$canonicalName %||% name, m$confidence %||% NA)
  }

  tibble::tibble(
    scientificName = name,
    taxonID        = sprintf("https://www.gbif.org/species/%s", m$usageKey),
    taxonRank      = tolower(m$rank %||% NA_character_),
    order          = m$order  %||% NA_character_,
    family         = m$family %||% NA_character_,
    vernacular_eng = gbif_vernacular_eng(m$usageKey),
    match_type     = m$matchType %||% NA_character_
  )
}

#' First English vernacular name for a GBIF key, or NA. @keywords internal
gbif_vernacular_eng <- function(key) {
  tryCatch({
    res <- httr2::request(sprintf("%s/species/%s/vernacularNames", GBIF_API, key)) |>
      httr2::req_url_query(limit = 100) |>
      httr2::req_timeout(15) |>
      httr2::req_user_agent("insightful.kiwi") |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    eng <- Filter(function(x) isTRUE(x$language %in% c("eng", "en")), res$results)
    if (length(eng)) eng[[1]]$vernacularName else NA_character_
  }, error = function(e) NA_character_)
}

#' NA-filled resolution row(s) for given name(s). @keywords internal
gbif_na_row <- function(name) {
  tibble::tibble(
    scientificName = name, taxonID = NA_character_, taxonRank = NA_character_,
    order = NA_character_, family = NA_character_, vernacular_eng = NA_character_,
    match_type = NA_character_
  )
}
