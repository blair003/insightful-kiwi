# observation_relations.R — derived temporal metrics that relate observations to
# each other (ik_data$app$relations), built at import and joined to observations on
# demand. Two metrics, both MINUTE-RESOLUTION (date-only deployments — those with
# Camtrap DP timestampIssues = TRUE — are excluded by resolution, not by source):
#   1. minutes_since_prev_same_species (+ possible_duplicate from the project window)
#   2. nearest predator / protected observation before & after, same location
# Species roles come from project config (instance/config/project.R).

#' Load instance/config/project.R into a list (species_roles, duplicate_window).
#'
#' @param config Runtime config (see build_config()).
#' @return list with `species_roles` and `duplicate_window` (empty if no file).
load_project_config <- function(config) {
  path <- file.path(config$dirs$config, "project.R")
  if (!file.exists(path)) return(list(species_roles = list(), duplicate_window = list()))
  e <- new.env()
  sys.source(path, envir = e)
  list(
    species_roles    = get0("species_roles",    envir = e, ifnotfound = list()),
    duplicate_window = get0("duplicate_window", envir = e, ifnotfound = list())
  )
}

#' Resolve species_roles config against the taxonomy into a scientificName lookup.
#'
#' @param taxonomy ik_data$app$taxonomy.
#' @param project  Loaded project config (load_project_config()).
#' @return tibble: scientificName, role, group, priority (one row per matched taxon).
resolve_species_roles <- function(taxonomy, project) {
  roles  <- project$species_roles
  empty  <- data.frame(scientificName = character(), role = character(),
                       group = character(), priority = integer(), stringsAsFactors = FALSE)
  if (is.null(roles) || length(roles) == 0 || nrow(taxonomy) == 0) return(empty)

  genus_of <- function(sn) sub(" .*$", "", sn)
  parts <- list()
  for (role in names(roles)) {
    groups <- roles[[role]]
    for (gi in seq_along(groups)) {
      m   <- groups[[gi]]
      hit <- rep(FALSE, nrow(taxonomy))
      if (!is.null(m$scientificName)) hit <- hit | taxonomy$scientificName %in% m$scientificName
      if (!is.null(m$family))         hit <- hit | (!is.na(taxonomy$family) & taxonomy$family %in% m$family)
      if (!is.null(m$genus))          hit <- hit | genus_of(taxonomy$scientificName) %in% m$genus
      if (any(hit)) parts[[length(parts) + 1]] <- data.frame(
        scientificName = taxonomy$scientificName[hit], role = role,
        group = names(groups)[gi], priority = gi, stringsAsFactors = FALSE
      )
    }
  }
  res <- dplyr::bind_rows(parts)
  res[!duplicated(res$scientificName), ]   # first match wins (roles/groups in priority order)
}

#' For one location's observations (sorted by time), the nearest role observation
#' of a DIFFERENT species before & after each row.
#'
#' Cross-species by design: same-species timing is `minutes_since_prev_same_species`,
#' so a counterpart of the subject's own species is excluded (this also excludes
#' self). Subjects are grouped by their species so each group searches the role set
#' minus that species; strict before/after via findInterval (so ties are excluded).
#' @keywords internal
nearest_role <- function(times, subject_species, role_mask, role_species, role_group,
                         prefix, with_group = TRUE) {
  n  <- length(times)
  before_min <- after_min <- rep(NA_real_, n)
  before_species <- before_group <- after_species <- after_group <- rep(NA_character_, n)

  if (any(role_mask)) {
    rt_all <- times[role_mask]; rs_all <- role_species[role_mask]; rg_all <- role_group[role_mask]
    for (s in unique(subject_species)) {
      si   <- which(subject_species == s)
      keep <- rs_all != s                 # counterparts of a DIFFERENT species
      if (!any(keep)) next
      rt <- rt_all[keep]; rs <- rs_all[keep]; rg <- rg_all[keep]
      t  <- times[si]
      k  <- findInterval(t, rt, left.open = TRUE)   # count of rt strictly < t
      m  <- findInterval(t, rt, left.open = FALSE)  # count of rt <= t
      bi <- pmax(k, 1L); ai <- pmin(m + 1L, length(rt))
      hb <- k >= 1L; ha <- (m + 1L) <= length(rt)
      before_min[si]     <- ifelse(hb, as.numeric(difftime(t, rt[bi], units = "mins")), NA_real_)
      before_species[si] <- ifelse(hb, rs[bi], NA_character_)
      before_group[si]   <- ifelse(hb, rg[bi], NA_character_)
      after_min[si]      <- ifelse(ha, as.numeric(difftime(rt[ai], t, units = "mins")), NA_real_)
      after_species[si]  <- ifelse(ha, rs[ai], NA_character_)
      after_group[si]    <- ifelse(ha, rg[ai], NA_character_)
    }
  }

  df <- if (with_group) {
    data.frame(before_min, before_species, before_group, after_min, after_species, after_group,
               stringsAsFactors = FALSE)
  } else {
    data.frame(before_min, before_species, after_min, after_species, stringsAsFactors = FALSE)
  }
  stats::setNames(df, paste0(prefix, "_", names(df)))
}

#' Build ik_data$app$relations from all datasets.
#'
#' @param datasets        ik_data$datasets list.
#' @param species_roles   Output of resolve_species_roles().
#' @param duplicate_window Project duplicate window (default + by_species).
#' @return tibble keyed by observationID with the timing metrics.
build_observation_relations <- function(datasets, species_roles, duplicate_window = list()) {
  default_w <- duplicate_window$default %||% Inf

  per <- lapply(names(datasets), function(id) {
    ds  <- datasets[[id]]
    obs <- camtrapdp::observations(ds$package)
    dep <- camtrapdp::deployments(ds$package)
    di  <- match(obs$deploymentID, dep$deploymentID)
    obs$locationID      <- dep$locationID[di]
    obs$timestampIssues <- dep$timestampIssues[di]

    # species-bearing + minute-resolution only (exclude date-only deployments)
    keep <- !is.na(obs$scientificName) &
      (is.na(obs$timestampIssues) | !obs$timestampIssues)
    obs <- obs[keep, , drop = FALSE]
    if (nrow(obs) == 0) return(NULL)

    obs$role  <- species_roles$role[match(obs$scientificName, species_roles$scientificName)]
    obs$group <- species_roles$group[match(obs$scientificName, species_roles$scientificName)]

    # --- Feature 1: minutes since previous same species at same location ---------
    obs <- obs |>
      dplyr::arrange(.data$locationID, .data$scientificName, .data$eventStart) |>
      dplyr::group_by(.data$locationID, .data$scientificName) |>
      dplyr::mutate(minutes_since_prev_same_species =
                      as.numeric(difftime(.data$eventStart, dplyr::lag(.data$eventStart), units = "mins"))) |>
      dplyr::ungroup()
    sp_thresh <- vapply(obs$scientificName, function(sp) {
      v <- duplicate_window$by_species[[sp]]; if (is.null(v)) default_w else v
    }, numeric(1))
    obs$possible_duplicate <- !is.na(obs$minutes_since_prev_same_species) &
      obs$minutes_since_prev_same_species <= sp_thresh

    # --- Feature 2: nearest predator / protected before & after (per location) ---
    f2 <- dplyr::bind_rows(lapply(split(seq_len(nrow(obs)), obs$locationID), function(ix) {
      o <- obs[ix, ][order(obs$eventStart[ix]), ]
      cbind(
        data.frame(observationID = o$observationID, stringsAsFactors = FALSE),
        nearest_role(o$eventStart, o$scientificName,
                     !is.na(o$role) & o$role == "predator", o$scientificName, o$group,
                     "predator", with_group = TRUE),
        nearest_role(o$eventStart, o$scientificName,
                     !is.na(o$role) & o$role == "protected", o$scientificName, o$group,
                     "protected", with_group = FALSE)
      )
    }))

    f1 <- obs[, c("observationID", "minutes_since_prev_same_species", "possible_duplicate")]
    f1$dataset <- id
    dplyr::left_join(f1, f2, by = "observationID")
  })

  dplyr::bind_rows(per)
}

#' The observation-relations table.
#' @param ik_data The ik_data container.
#' @return ik_data$app$relations.
ik_relations <- function(ik_data) ik_data$app$relations

#' The resolved species-roles lookup.
#' @param ik_data The ik_data container.
#' @return ik_data$app$species_roles.
ik_species_roles <- function(ik_data) ik_data$app$species_roles
