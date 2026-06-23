# select.R — resolve a data SELECTION into the deployments + observations it covers.
# The single place the deployment-first selection pipeline lives (per AGENTS: dataset
# selection + joins go through ik_ helpers). The model is documented in
# docs/data-model/04-data-selection.md. Every axis is optional; an unset axis (NULL /
# empty) imposes no constraint.
#
# Pipeline:
#   1. filter DEPLOYMENTS by geography (reserve / line / location) and source_type;
#   2. take those deployments' OBSERVATIONS, then filter by PERIOD on each observation's
#      season (camera = its deployment's season; trap = its check-date season — set in
#      build_period) and by SPECIES;
#   3. report in-season effort for the kept deployments (denominator for rates).

#' Drop NA/empty entries; return NULL when nothing remains (so it's a no-op filter).
#' @keywords internal
.ik_nz <- function(x) {
  if (is.null(x)) return(NULL)
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (length(x)) x else NULL
}

#' Resolve a selection across the four axes into its data.
#'
#' @param ik_data  The ik_data container.
#' @param season   `calendar_season` label(s) (e.g. "Autumn 2024"), or NULL for all.
#' @param reserve,line,location Geography filters (values from `app$geography$locations`).
#' @param source_type Device/source value(s) (camera/trap/…), or NULL for all. (NB the
#'   manifest's separate `method` tag is monitoring/control — a different axis, not this.)
#' @param species  `scientificName` value(s), or NULL for all.
#' @return list(observations, deployments, effort_hours): the matching observations
#'   (with provenance + location, as `ik_observations()`), the deployments **assigned to
#'   the selected period** (majority-season rule; + geography/source), and the in-season
#'   clipped effort hours (across all overlapping deployments — splits cross-season traps).
ik_select <- function(ik_data, season = NULL, reserve = NULL, line = NULL,
                      location = NULL, source_type = NULL, species = NULL, dataset = NULL) {
  season <- .ik_nz(season); reserve <- .ik_nz(reserve); line <- .ik_nz(line)
  location <- .ik_nz(location); source_type <- .ik_nz(source_type); species <- .ik_nz(species)
  dataset <- .ik_nz(dataset)   # explicit datasets OVERRIDE the global toggle (NULL → toggle)

  # 1. deployments, enriched with geography, filtered by the deployment-level axes.
  dep  <- ik_deployments(ik_data, dataset)
  locs <- ik_data$app$geography$locations
  dep$reserve <- locs$reserve[match(dep$locationID, locs$location_id)]
  dep$line    <- locs$line[match(dep$locationID, locs$location_id)]

  keep <- rep(TRUE, nrow(dep))
  if (!is.null(reserve))  keep <- keep & dep$reserve     %in% reserve
  if (!is.null(line))     keep <- keep & dep$line        %in% line
  if (!is.null(location)) keep <- keep & dep$locationID  %in% location
  if (!is.null(source_type)) keep <- keep & dep$source_type %in% source_type
  dep <- dep[keep, , drop = FALSE]                          # geography + device scope

  # 2. observations of that scope, filtered by their season (from app$period$observations:
  #    camera detections carry their DEPLOYMENT's season, trap captures their check-date
  #    season) + species. So a pulse that grazes the next season keeps its late detections
  #    under the season it ran in, consistent with the deployment count + effort below.
  obs <- ik_observations(ik_data, dataset)
  obs <- obs[obs$deploymentID %in% dep$deploymentID, , drop = FALSE]
  if (!is.null(season)) {
    op   <- ik_observation_period(ik_data, dataset)
    osea <- op$calendar_season[match(obs$observationID, op$observationID)]
    obs  <- obs[!is.na(osea) & osea %in% season, , drop = FALSE]
  }
  if (!is.null(species)) {
    obs <- obs[!is.na(obs$scientificName) & obs$scientificName %in% species, , drop = FALSE]
  }

  # 3a. effort = in-season clipped hours across the WHOLE scope. Overlap-based clipping is
  #     required so a long cross-season trap interval splits its hours into each season.
  ef <- build_monitoring_season(dep)
  if (!is.null(season)) ef <- ef[ef$calendar_season %in% season, , drop = FALSE]

  # 3b. deployments returned/counted = those ASSIGNED to the selected season(s), BY SOURCE:
  #     - camera (pulse): the majority-overlap season, so a boundary-straddling pulse is counted
  #       ONCE, in the season holding most of it (matches how its detections are seasoned).
  #     - trap (one check per deployment): the CHECK's season, honouring meta$trapping$season_by
  #       ("check_date" default — by deploymentEnd; "interval" — the majority-overlap span). This
  #       keeps the trap count consistent with trap observations (check-date seasoned) and with the
  #       trap-review / servicing views, instead of the old majority-overlap-for-everything rule.
  if (!is.null(season)) {
    asg  <- ik_deployment_period(ik_data, dataset)
    asea <- asg$calendar_season
    is_trap <- !is.na(asg$source_type) & asg$source_type == "trap"
    if (any(is_trap) && !identical(ik_data$meta$trapping$season_by %||% "check_date", "interval"))
      asea[is_trap] <- ik_assign_season(asg$deploymentEnd[is_trap], asg$deploymentEnd[is_trap])$calendar_season
    dep <- dep[dep$deploymentID %in% asg$deploymentID[asea %in% season], , drop = FALSE]
  }

  list(observations = obs, deployments = dep, effort_hours = sum(ef$effort_hours))
}

#' Resolve a selection SPEC (named list of ik_select() axis args) into data, applying
#' optional per-call overrides. Lets a view re-resolve the shared sidebar spec — e.g. a
#' device-split Overview calls `ik_resolve(ik_data, spec, source_type = "camera")` then
#' `... "trap"` from one selection. Also the foundation for the master/derived swiper.
#'
#' @param ik_data The ik_data container.
#' @param spec    Named list with any of season/reserve/line/location/source_type/species.
#' @param ...     Overrides merged over `spec` (e.g. `source_type = "camera"`).
#' @return `ik_select()` output.
ik_resolve <- function(ik_data, spec, ...) {
  spec <- utils::modifyList(spec, list(...))
  spec <- spec[intersect(names(spec), names(formals(ik_select)))]   # drop period/compare metadata
  do.call(ik_select, c(list(ik_data), spec))
}
