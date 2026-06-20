# outcomes.R — the "are we winning" outcome time-series. The trust's causal chain on one
# seasonal timeline: trap predators → predator abundance (camera RAI) falls → kiwi abundance
# (camera RAI) rises. ik_outcome_series() walks every season and computes, at NETWORK level
# (mean over reserves ± SE, via the auditable metrics in metrics.R), three metric families:
#   trap_rate   — predator captures / 100 trap-nights  (the control effort/outcome)
#   camera_rai  — predator RAI                          (want DOWN)
#   camera_rai  — kiwi RAI                              (want UP)
# Long/tidy so the Outcomes view facets it. Network SE = spread across reserves (NA for <2).

#' Per-season outcome metrics (network ± SE) for the predators + kiwi story.
#'
#' @param ik_data The ik_data container.
#' @return A tidy data.frame: season · season_order · metric_type ("trap_rate"/"camera_rai") ·
#'   taxon · role ("predator"/"protected") · value · se · n_reserves. NULL when no seasons.
ik_outcome_series <- function(ik_data) {
  sg <- ik_species_groups(ik_data)
  pick <- function(roles, by) {                 # label -> scientificNames, in priority order
    s <- sg[!is.na(sg[[by]]) & sg$role %in% roles, , drop = FALSE]
    s <- s[order(s$priority), , drop = FALSE]
    split(s$scientificName, factor(s$label, levels = unique(s$label)))
  }
  cam_taxa  <- pick(c("predator", "protected"), "monitor")   # predators + kiwi on camera
  trap_taxa <- pick("predator", "control")                   # predators in traps
  role_of   <- stats::setNames(sg$role[match(names(cam_taxa), sg$label)], names(cam_taxa))

  seasons <- ik_season_levels(ik_deployment_period(ik_data))
  if (!length(seasons)) return(NULL)

  net <- function(summary, mtype, season, order, roles) {     # per-reserve summary → network row(s)
    if (is.null(summary) || !nrow(summary)) return(NULL)
    n <- ik_metric_combine(summary)
    if (!nrow(n)) return(NULL)
    data.frame(season = season, season_order = order, metric_type = mtype,
               taxon = n$taxon, role = roles[n$taxon], value = n$metric, se = n$se,
               n_reserves = n$n_lines, stringsAsFactors = FALSE)
  }

  rows <- lapply(seq_along(seasons), function(i) {
    sp  <- list(season = ik_expand_period(paste0("season:", seasons[i]), ik_data))
    rai  <- tryCatch(ik_rai(ik_data, sp, cam_taxa, level = "reserve")$summary, error = function(e) NULL)
    rate <- tryCatch(ik_trap_rate(ik_data, sp, trap_taxa, level = "reserve")$summary, error = function(e) NULL)
    dplyr::bind_rows(
      net(rai,  "camera_rai", seasons[i], i, role_of),
      net(rate, "trap_rate",  seasons[i], i, stats::setNames(rep("predator", length(trap_taxa)), names(trap_taxa))))
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) return(NULL)
  out
}
