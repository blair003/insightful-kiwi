# metrics.R — effort & outcome metrics, in ONE auditable place. Each function returns its
# INPUTS (the per-line table) beside the rolled-up summary, so the maths is inspectable and
# exportable. Camera RAI (individuals / camera-hours × norm) and trap capture rate
# (captures / trap-days × 100) both roll up line -> reserve as mean ± SE (sd/√n): the
# monitoring/trap LINE is the unit of replication. Zero-detection lines (that HAVE effort)
# contribute 0; zero-effort lines are excluded. Methodology (norm, net/raw) is project
# config, read at display time from ik_data$meta. Cameras only: net excludes
# possible_duplicate; traps are date-only so net == raw.

#' Build a taxa list (label -> scientificNames) from the registry, for one method/class.
#'
#' @param sg    ik_species_groups(ik_data).
#' @param by    Registry class column — "monitor" or "control".
#' @param class The class value to select (e.g. "target").
#' @return Named list: label -> character vector of scientificNames (in priority order).
ik_taxa_groups <- function(sg, by = "monitor", class = "target") {
  s <- sg[!is.na(sg[[by]]) & sg[[by]] == class, , drop = FALSE]
  s <- s[order(s$priority), , drop = FALSE]
  split(s$scientificName, factor(s$label, levels = unique(s$label)))
}

#' Animal observations of the resolved selection, tagged with reserve/line and (camera
#' only, when `net`) with possible-duplicates dropped. @keywords internal
.metrics_obs <- function(ik_data, r, locs, net = FALSE) {
  obs <- r$observations
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  if (net && nrow(obs)) {
    rel <- ik_relations(ik_data)
    dup <- rel$possible_duplicate[match(obs$observationID, rel$observationID)]
    obs <- obs[is.na(dup) | !dup, , drop = FALSE]                 # keep non-duplicates
  }
  obs$count[is.na(obs$count)] <- 1L
  alld <- ik_deployments(ik_data)
  obs$locationID <- alld$locationID[match(obs$deploymentID, alld$deploymentID)]
  obs$line       <- locs$line[match(obs$locationID, locs$location_id)]
  obs$reserve    <- locs$reserve[match(obs$locationID, locs$location_id)]
  obs
}

#' Effort per (reserve, line) for the resolved deployments — defines the line set (lines
#' WITH effort), `effort_hours` transformed by `unit_fn`. @keywords internal
.metrics_effort <- function(r, locs, unit_fn = identity) {
  dep <- r$deployments
  dep$effort_hours <- as.numeric(difftime(dep$deploymentEnd, dep$deploymentStart, units = "hours"))
  dep$line    <- locs$line[match(dep$locationID, locs$location_id)]
  dep$reserve <- locs$reserve[match(dep$locationID, locs$location_id)]
  dep |>
    dplyr::filter(!is.na(.data$line), !is.na(.data$reserve)) |>
    dplyr::group_by(.data$reserve, .data$line) |>
    dplyr::summarise(effort = unit_fn(sum(.data$effort_hours, na.rm = TRUE)), .groups = "drop")
}

#' Per-line value (individuals) per taxon joined to ALL effort-lines (0 where no
#' detections), then `metric = value / effort × norm`. @keywords internal
.metrics_lines <- function(effort, obs, taxa, norm) {
  dplyr::bind_rows(lapply(names(taxa), function(lab) {
    o <- obs[obs$scientificName %in% taxa[[lab]], , drop = FALSE]
    cnt <- if (nrow(o)) {
      dplyr::summarise(dplyr::group_by(o, .data$reserve, .data$line),
                       value = sum(.data$count), .groups = "drop")
    } else data.frame(reserve = character(), line = character(), value = numeric())
    out <- dplyr::left_join(effort, cnt, by = c("reserve", "line"))
    out$value[is.na(out$value)] <- 0
    out$taxon  <- lab
    out$metric <- out$value / out$effort * norm
    out
  }))
}

#' Roll the per-line metric up to `level` as mean ± SE (sd/√n; SE NA for n < 2).
#' @keywords internal
.metrics_rollup <- function(lines, level) {
  dplyr::summarise(
    dplyr::group_by(lines, dplyr::across(dplyr::all_of(c(level, "taxon")))),
    n_lines = dplyr::n(),
    se      = stats::sd(.data$metric) / sqrt(dplyr::n()),   # sd over lines; NA for n < 2
    metric  = mean(.data$metric),                          # redefine LAST (summarise is sequential)
    .groups = "drop")
}

#' Camera RAI per taxon, rolled line -> `level` as mean ± SE.
#'
#' @param ik_data The ik_data container.
#' @param spec    A selection spec (named list; see ik_select / ik_resolve).
#' @param taxa    Named list label -> scientificNames (e.g. `ik_taxa_groups(sg)`).
#' @param level   Roll-up level (default "reserve").
#' @return list(lines, summary): `lines` = reserve·line·taxon·value(individuals)·
#'   effort(camera-hours)·metric(RAI); `summary` = level·taxon·metric·se·n_lines (+ norm/net).
ik_rai <- function(ik_data, spec, taxa, level = "reserve") {
  cfg  <- ik_data$meta$camera$rai %||% list(norm_hours = 2000, use_net = TRUE)
  norm <- cfg$norm_hours %||% 2000
  net  <- isTRUE(cfg$use_net)
  locs <- ik_data$app$geography$locations

  r      <- ik_resolve(ik_data, spec, source_type = "camera")
  effort <- .metrics_effort(r, locs)                              # camera-hours / line (pulse = exact)
  obs    <- .metrics_obs(ik_data, r, locs, net = net)
  lines  <- dplyr::rename(.metrics_lines(effort, obs, taxa, norm),
                          individuals = "value", camera_hours = "effort")

  summary <- .metrics_rollup(lines, level)
  summary$norm_hours <- norm
  summary$use_net    <- net
  list(lines = lines, summary = summary)
}

#' Trap capture rate per taxon (captures / trap-days × norm), rolled line -> `level`.
#' NB trap-days use the assigned check-interval effort; a long interval straddling a season
#' boundary is attributed to its majority season (small boundary approximation).
#'
#' @inheritParams ik_rai
#' @return list(lines, summary): `lines` = reserve·line·taxon·value(captures)·
#'   effort(trap-days)·metric(rate); `summary` as ik_rai (+ norm_trap_days).
ik_trap_rate <- function(ik_data, spec, taxa, level = "reserve") {
  cfg  <- ik_data$meta$trapping$rate %||% list(norm_trap_days = 100)
  norm <- cfg$norm_trap_days %||% 100
  locs <- ik_data$app$geography$locations

  r      <- ik_resolve(ik_data, spec, source_type = "trap")
  effort <- .metrics_effort(r, locs, unit_fn = function(h) h / 24)   # trap-days
  obs    <- .metrics_obs(ik_data, r, locs, net = FALSE)              # date-only: net == raw
  lines  <- dplyr::rename(.metrics_lines(effort, obs, taxa, norm),
                          captures = "value", trap_days = "effort")

  summary <- .metrics_rollup(lines, level)
  summary$norm_trap_days <- norm
  list(lines = lines, summary = summary)
}

#' The animal observations BEHIND a metric cell — the auditable basis a RAI/rate figure is built
#' from, for the Overview drill down to the records. Re-resolves the same selection + counting
#' rules (camera = net per config; trap = raw) and filters to the clicked taxon / reserve / line.
#'
#' @param ik_data The ik_data container. @param spec The selection spec. @param taxa Named list
#'   label -> scientificNames (as passed to ik_rai/ik_trap_rate).
#' @param taxon The group label (a name of `taxa`). @param reserve,line Optional cell coords.
#' @param source_type "camera" (RAI) or "trap" (rate).
#' @return data.frame: observationID · when · scientificName · count · reserve · line ·
#'   locationName, newest first; NULL when none.
ik_metric_obs <- function(ik_data, spec, taxa, taxon, reserve = NULL, line = NULL,
                          source_type = c("camera", "trap")) {
  source_type <- match.arg(source_type)
  net  <- source_type == "camera" && isTRUE((ik_data$meta$camera$rai %||% list())$use_net)
  locs <- ik_data$app$geography$locations
  r    <- ik_resolve(ik_data, spec, source_type = source_type)
  obs  <- .metrics_obs(ik_data, r, locs, net = net)
  o    <- obs[obs$scientificName %in% taxa[[taxon]], , drop = FALSE]
  if (!is.null(reserve)) o <- o[!is.na(o$reserve) & o$reserve == reserve, , drop = FALSE]
  if (!is.null(line))    o <- o[!is.na(o$line)    & o$line    == line,    , drop = FALSE]
  if (!nrow(o)) return(NULL)
  when <- if (source_type == "camera") o$eventStart else o$eventEnd
  ord  <- order(when, decreasing = TRUE)
  data.frame(observationID = o$observationID[ord], when = when[ord],
             scientificName = o$scientificName[ord], count = o$count[ord],
             reserve = o$reserve[ord], line = o$line[ord],
             locationName = o$locationName[ord], stringsAsFactors = FALSE)
}

#' Roll a per-reserve `summary` up to ONE network row per taxon — mean ± SE across the
#' RESERVES (the replication unit at this level). For the Overview's combined column. This
#' is a project/region network, not "global" (don't blend RAI across regions/projects).
#'
#' @param summary A per-reserve summary (ik_rai/ik_trap_rate `$summary`).
#' @param label   Reserve label for the combined row (default "Combined" — it is the mean of
#'   whatever reserves are in `summary`, i.e. the *selected* ones, not necessarily all).
#' @return A summary with `reserve = label`, one row per taxon (`metric`, `se`, `n_lines` =
#'   number of reserves) — bind_rows()-compatible with the input.
ik_metric_combine <- function(summary, label = "Combined") {
  summary <- summary[summary$reserve != label, , drop = FALSE]   # idempotent
  out <- dplyr::summarise(
    dplyr::group_by(summary, .data$taxon),
    se      = stats::sd(.data$metric) / sqrt(dplyr::n()),         # over reserves
    n_lines = dplyr::n(),                                         # = n reserves here
    metric  = mean(.data$metric),
    .groups = "drop")
  out$reserve <- label
  out
}
