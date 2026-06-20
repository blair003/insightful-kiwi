# observation_relations.R — derived temporal metrics that relate observations to
# each other (ik_data$app$relations), built at import and joined to observations on
# demand. Two metrics, both MINUTE-RESOLUTION (date-only deployments — those with
# Camtrap DP timestampIssues = TRUE — are excluded by resolution, not by source):
#   1. minutes_since_prev_same_species (+ possible_duplicate from the project window)
#   2. nearest predator / protected observation before & after, same location
# Roles come from the species registry (app$species_groups; see R/functions/species.R).

#' For one location's observations (sorted by time), the nearest role observation
#' before & after each row.
#'
#' Records the nearest counterpart as a NEUTRAL primitive (any species; self excluded
#' via strict before/after, so a role subject never matches itself or a tie). Same-vs-
#' different species is left to analysis — compare `*_species` to the subject's — the
#' same primitive-not-verdict stance as `minutes_since_prev_same_species`.
#' @keywords internal
nearest_role <- function(times, role_mask, role_species, role_group,
                         prefix, with_group = TRUE) {
  n  <- length(times)
  before_min <- after_min <- rep(NA_real_, n)
  before_species <- before_group <- after_species <- after_group <- rep(NA_character_, n)

  if (any(role_mask)) {
    rt <- times[role_mask]; rs <- role_species[role_mask]; rg <- role_group[role_mask]
    k  <- findInterval(times, rt, left.open = TRUE)   # count of rt strictly < t
    m  <- findInterval(times, rt, left.open = FALSE)  # count of rt <= t
    bi <- pmax(k, 1L); ai <- pmin(m + 1L, length(rt))
    hb <- k >= 1L; ha <- (m + 1L) <= length(rt)
    before_min     <- ifelse(hb, as.numeric(difftime(times, rt[bi], units = "mins")), NA_real_)
    before_species <- ifelse(hb, rs[bi], NA_character_)
    before_group   <- ifelse(hb, rg[bi], NA_character_)
    after_min      <- ifelse(ha, as.numeric(difftime(rt[ai], times, units = "mins")), NA_real_)
    after_species  <- ifelse(ha, rs[ai], NA_character_)
    after_group    <- ifelse(ha, rg[ai], NA_character_)
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
#' @param species         Output of resolve_species_groups() (uses role + group).
#' @param duplicate_window Project duplicate window (default + by_species).
#' @return tibble keyed by observationID with the timing metrics.
build_observation_relations <- function(datasets, species, duplicate_window = list()) {
  default_w <- duplicate_window$default %||% Inf

  per <- lapply(names(datasets), function(id) {
    ds  <- datasets[[id]]
    obs <- camtrapdp::observations(ds$package)
    dep <- camtrapdp::deployments(ds$package)
    di  <- match(obs$deploymentID, dep$deploymentID)
    obs$locationID      <- dep$locationID[di]
    obs$timestampIssues <- dep$timestampIssues[di]
    date_only <- !is.na(obs$timestampIssues) & obs$timestampIssues

    # --- Minute-resolution metrics (cameras): species-bearing, non-date-only -----
    mr <- obs[!is.na(obs$scientificName) & !date_only, , drop = FALSE]
    cam_out <- NULL
    if (nrow(mr) > 0) {
      mr$role  <- species$role[match(mr$scientificName, species$scientificName)]
      mr$group <- species$group[match(mr$scientificName, species$scientificName)]

      # Feature 1: minutes since previous same species at same location
      mr <- mr |>
        dplyr::arrange(.data$locationID, .data$scientificName, .data$eventStart) |>
        dplyr::group_by(.data$locationID, .data$scientificName) |>
        dplyr::mutate(minutes_since_prev_same_species =
                        as.numeric(difftime(.data$eventStart, dplyr::lag(.data$eventStart), units = "mins"))) |>
        dplyr::ungroup()
      sp_thresh <- vapply(mr$scientificName, function(sp) {
        v <- duplicate_window$by_species[[sp]]; if (is.null(v)) default_w else v
      }, numeric(1))
      mr$possible_duplicate <- !is.na(mr$minutes_since_prev_same_species) &
        mr$minutes_since_prev_same_species <= sp_thresh

      # Feature 2: nearest predator / protected before & after (per location)
      f2 <- dplyr::bind_rows(lapply(split(seq_len(nrow(mr)), mr$locationID), function(ix) {
        o <- mr[ix, ][order(mr$eventStart[ix]), ]
        cbind(
          data.frame(observationID = o$observationID, stringsAsFactors = FALSE),
          # nearest ANY-animal detection here — "how long since activity at this location"
          nearest_role(o$eventStart, rep(TRUE, nrow(o)), o$scientificName, o$group,
                       "animal", with_group = FALSE),
          nearest_role(o$eventStart,
                       !is.na(o$role) & o$role == "predator", o$scientificName, o$group,
                       "predator", with_group = TRUE),
          nearest_role(o$eventStart,
                       !is.na(o$role) & o$role == "protected", o$scientificName, o$group,
                       "protected", with_group = FALSE)
        )
      }))
      f1 <- mr[, c("observationID", "minutes_since_prev_same_species", "possible_duplicate")]
      f1$dataset <- id
      cam_out <- dplyr::left_join(f1, f2, by = "observationID")
    }

    # --- Feature 3: prior CHECK at the same location (date-only / trap obs) -------
    # Every check (any outcome), not just captures: the previous trap check here. Date
    # resolution. From observations (not deployments) — fine, and handles first-check (NA).
    to <- obs[date_only, , drop = FALSE]
    trap_out <- NULL
    if (nrow(to) > 0) {
      # localize eventEnd — prior_check_date is an absolute instant we display, so it must be
      # in the dataset's zone (raw package times are UTC → would shift the DATE for NZ).
      to <- ik_localize_times(to, ds$meta, "eventEnd")
      # the bait set at THIS check (its own baitstatus); the PRIOR check's bait — the one that
      # was actually in the trap during the interval, hence what caught anything found here — is
      # that lagged by one check at the location (same as prior_check_date).
      to$baitstatus <- vapply(to$observationTags, .ovw_tag, character(1), key = "bait")
      to <- to |>
        dplyr::arrange(.data$locationID, .data$eventEnd) |>
        dplyr::group_by(.data$locationID) |>
        dplyr::mutate(prior_check_date = dplyr::lag(.data$eventEnd),
                      prior_baitstatus = dplyr::lag(.data$baitstatus)) |>
        dplyr::ungroup()
      trap_out <- data.frame(
        observationID          = to$observationID, dataset = id,
        prior_check_date       = to$prior_check_date,
        days_since_prior_check = as.numeric(difftime(to$eventEnd, to$prior_check_date, units = "days")),
        prior_baitstatus       = to$prior_baitstatus,
        stringsAsFactors = FALSE)
    }

    dplyr::bind_rows(cam_out, trap_out)
  })

  dplyr::bind_rows(per)
}

#' The observation-relations table.
#' @param ik_data The ik_data container.
#' @return ik_data$app$relations.
ik_relations <- function(ik_data) ik_data$app$relations
