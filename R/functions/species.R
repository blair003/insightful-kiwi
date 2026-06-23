# species.R — the curated "species we care about" registry (ik_data$app$species_groups).
# Resolves instance/config/project.R `species_groups` against the taxonomy into one
# per-scientificName lookup carrying GROUP (reporting unit), LABEL, ROLE (predator/
# protected/other — used by co-occurrence) and CLASS (target/interesting — used for
# prominence). One source of truth replacing v0.1's three overlapping lists.

#' Load instance/config/project.R into a list (species_groups, duplicate_window).
#'
#' @param config Runtime config (see build_config()).
#' @return list with `species_groups` and `duplicate_window` (empty if no file).
load_project_config <- function(config) {
  path <- file.path(config$dirs$config, "project.R")
  defaults <- list(species_groups = list(), duplicate_window = list(), organisation = NULL,
                   camera   = list(rai = list(norm_hours = 2000, use_net = TRUE)),
                   trapping = list(rate = list(norm_trap_days = 100),
                                   health = list(percentiles = c(good = 0.5, watch = 0.9),
                                                 floor = 14, ceiling = 60),
                                   season_by = "check_date"),
                   overview = list(show_rai_matrix_by_reserve = FALSE, list_other_species = TRUE,
                                   default_compare = "none"),
                   proximity = list(max_radius_m = 2000),
                   media    = list(keep_originals = TRUE))
  if (!file.exists(path)) return(defaults)
  e <- new.env()
  sys.source(path, envir = e)
  list(
    species_groups   = get0("species_groups",   envir = e, ifnotfound = defaults$species_groups),
    duplicate_window = get0("duplicate_window", envir = e, ifnotfound = defaults$duplicate_window),
    organisation     = get0("organisation", envir = e, ifnotfound = defaults$organisation),  # instance identity
    camera           = get0("camera",   envir = e, ifnotfound = defaults$camera),
    trapping         = get0("trapping", envir = e, ifnotfound = defaults$trapping),
    # merge onto defaults so a project that sets only SOME overview keys still gets the rest
    overview         = utils::modifyList(defaults$overview,
                                         get0("overview", envir = e, ifnotfound = list())),
    proximity        = utils::modifyList(defaults$proximity,
                                         get0("proximity", envir = e, ifnotfound = list())),
    media            = utils::modifyList(defaults$media,
                                         get0("media", envir = e, ifnotfound = list()))
  )
}

#' Resolve the species_groups config against the taxonomy into a scientificName lookup.
#'
#' Matchers (scientificName / family / genus) work as in the config docs; genus is the
#' first word of the scientificName (robust to a missing family). Groups are evaluated
#' in list order (= priority); the FIRST group to match a taxon wins.
#'
#' @param taxonomy ik_data$app$taxonomy.
#' @param project  Loaded project config (load_project_config()).
#' @return tibble: scientificName, group, label, role, monitor, control, sentiment, priority
#'   (one row per matched taxon). `monitor`/`control` are the per-method prominence classes
#'   (target / interesting / NA); `sentiment` (bad/good/neutral) tints the Overview card icon,
#'   defaulting from `role` (predator→bad, protected→good, other→neutral) unless set explicitly.
resolve_species_groups <- function(taxonomy, project) {
  groups <- project$species_groups
  empty  <- data.frame(scientificName = character(), group = character(),
                       label = character(), role = character(), monitor = character(),
                       control = character(), sentiment = character(), priority = integer(),
                       split = logical(), stringsAsFactors = FALSE)
  if (is.null(groups) || length(groups) == 0 || nrow(taxonomy) == 0) return(empty)

  genus_of <- function(sn) sub(" .*$", "", sn)
  parts <- list()
  for (gi in seq_along(groups)) {
    g   <- groups[[gi]]
    hit <- rep(FALSE, nrow(taxonomy))
    if (!is.null(g$scientificName)) hit <- hit | taxonomy$scientificName %in% g$scientificName
    if (!is.null(g$family))         hit <- hit | (!is.na(taxonomy$family) & taxonomy$family %in% g$family) |
                                                 taxonomy$scientificName %in% g$family   # the family taxon itself (e.g. a "Mustelidae" family-level ID)
    if (!is.null(g$genus))          hit <- hit | genus_of(taxonomy$scientificName) %in% g$genus
    if (any(hit)) parts[[length(parts) + 1]] <- data.frame(
      scientificName = taxonomy$scientificName[hit],
      group    = names(groups)[gi],
      label    = g$label %||% names(groups)[gi],
      role     = g$role    %||% "other",
      monitor  = g$monitor %||% NA_character_,
      control  = g$control %||% NA_character_,
      sentiment = g$sentiment %||% switch(g$role %||% "other", predator = "bad", protected = "good", "neutral"),
      priority = gi,
      split    = isTRUE(g$split),   # show sub-species individually in the Species picker (else group only)
      stringsAsFactors = FALSE
    )
  }
  res <- dplyr::bind_rows(parts)
  res[!duplicated(res$scientificName), ]   # first group in order wins
}

#' The resolved species-groups registry.
#' @param ik_data The ik_data container.
#' @return ik_data$app$species_groups (scientificName, group, label, role, class, priority).
ik_species_groups <- function(ik_data) ik_data$app$species_groups
