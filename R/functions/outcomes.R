# outcomes.R — the "are we winning" outcome time-series. The trust's causal chain on one
# seasonal timeline: trap predators → predator activity (camera RAI) falls → kiwi activity
# (camera RAI) rises. ik_outcome_series() walks every season and computes, at NETWORK level
# (mean over reserves ± SE, via the auditable metrics in metrics.R), three metric families:
#   trap_rate   — predator captures / norm_trap_days trap-nights (default 100; the control effort/outcome)
#   camera_rai  — predator RAI                          (want DOWN)
#   camera_rai  — kiwi RAI                              (want UP)
# Long/tidy so the Outcomes view facets it. Network SE = spread across reserves (NA for <2).

#' Per-season outcome metrics (network ± SE) for the predators + kiwi story.
#'
#' @param ik_data The ik_data container.
#' @return A tidy data.frame: season · season_order · metric_type ("trap_rate"/"camera_rai") ·
#'   taxon · role ("predator"/"protected") · value · se · n_reserves. NULL when no seasons.
#' Superset of outcome taxa for the trend: predator+protected groups (camera/monitor) and predator
#' groups (trap/control), each itemised into sub-species where the group's `split` flag is set. Keys
#' are STABLE vernacular labels (group label, or species vernacular for a sub-species) so the picker
#' filter + the drill agree regardless of the scientific-name preference. @return list(cam, trap,
#' role) — each a label->scientificNames map (role is label->"predator"/"protected"). @keywords internal
ik_outcome_taxa <- function(ik_data) {
  sg <- ik_species_groups(ik_data)
  splits <- unique(sg$label[which(sg$split)])
  build <- function(roles, by) {
    s <- sg[!is.na(sg[[by]]) & sg$role %in% roles, , drop = FALSE]; s <- s[order(s$priority), , drop = FALSE]
    base <- split(s$scientificName, factor(s$label, levels = unique(s$label)))
    taxa <- list(); role <- character(0)
    for (lbl in names(base)) {
      taxa[[lbl]] <- base[[lbl]]; role[[lbl]] <- s$role[match(lbl, s$label)]
      if (lbl %in% splits) for (sn in base[[lbl]][grepl(" ", base[[lbl]], fixed = TRUE)]) {
        k <- ik_species_label(sn, ik_data, "vernacular"); taxa[[k]] <- sn; role[[k]] <- s$role[match(lbl, s$label)]
      }
    }
    list(taxa = taxa, role = role)
  }
  cam <- build(c("predator", "protected"), "monitor"); trap <- build("predator", "control")
  list(cam = cam$taxa, trap = trap$taxa, role = cam$role)
}

ik_outcome_series <- function(ik_data) {
  otx <- ik_outcome_taxa(ik_data)
  role_of <- otx$role
  seasons <- ik_season_levels(ik_deployment_period(ik_data))
  if (!length(seasons)) return(NULL)
  periods <- lapply(seq_along(seasons), function(i) list(label = seasons[i], order = i, seasons = seasons[i]))
  out <- .ik_metric_series(ik_data, periods, otx$cam, otx$trap)   # shared engine (see species.R)
  if (is.null(out) || !nrow(out)) return(NULL)
  # role: protected/predator from the camera taxa map; trap captures are always predators
  out$role <- ifelse(out$metric_type == "trap_rate", "predator", unname(role_of[out$taxon]))
  data.frame(season = out$period, season_order = out$order, metric_type = out$metric_type,
             taxon = out$taxon, role = out$role, value = out$value, se = out$se,
             n_reserves = out$n_reserves, stringsAsFactors = FALSE)
}
