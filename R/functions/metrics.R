# metrics.R — effort & outcome metrics, in ONE auditable place. Each function returns its
# INPUTS (the per-group table) beside the rolled-up summary, so the maths is inspectable and
# exportable. Camera RAI (individuals / camera-hours × norm) and trap capture rate
# (captures / trap-days × norm_trap_days, default 100) both roll up line -> reserve as mean ± SE (sd/√n): the
# monitoring/trap LINE is the unit of replication. Zero-detection groups (that HAVE effort)
# contribute 0; zero-effort groups are excluded. Methodology (norm, net/raw) is project
# config, read at display time from ik_data$meta. Cameras only: net excludes
# possible_duplicate; traps are date-only so net == raw.
#
# The RAI/rate ATOM — value / effort × norm — lives in ONE grain-agnostic core
# (.metrics_effort + .metrics_grouped). ik_rai/ik_trap_rate group it by reserve·line and roll
# up with SE; ik_location_metric() groups the SAME atom by location for the map (one point per
# location, NO SE — a location is a single point, not a sample of replicate lines).

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

# ── Grouped species choices for selectInputs (group whole OR split into sub-species) ──────────────
# Anywhere a dropdown offers a taxa group, a multi-species group (Mustelids → Stoat/Weasel/Ferret;
# Rats → ship/Norway/kiore) should be pickable whole OR split. Values are encoded so one control
# spans both grains: "grp:<label>" (the whole group) and "sci:<scientificName>" (one species). This
# is the shared engine behind the bait "Captures of", the map predator/protected pickers, etc.

#' Grouped `selectInput` choices from a `taxa` map (label -> scientificNames, e.g. ik_taxa_groups()):
#' each group as "grp:<label>", plus an indented "sci:<name>" per species for any group resolving to
#' ≥ 2 species. @param all_label optional leading "everything" option label (NULL to omit).
#' @keywords internal
ik_species_choices <- function(taxa, ik_data, prefer = "vernacular", split = character(0),
                               all_label = NULL, all_value = "__all__") {
  ch <- if (!is.null(all_label)) stats::setNames(list(all_value), all_label) else list()
  for (lbl in names(taxa)) {
    ch[[lbl]] <- paste0("grp:", lbl)
    spp <- taxa[[lbl]][grepl(" ", taxa[[lbl]], fixed = TRUE)]        # resolved species (binomials)
    if (lbl %in% split)
      for (sn in spp) ch[[paste0("  ", ik_species_label(sn, ik_data, prefer))]] <- paste0("sci:", sn)
  }
  ch
}

#' Resolve `ik_species_choices()` value(s) ("grp:"/"sci:") to scientificNames, given the same `taxa`.
#' @keywords internal
ik_resolve_species_choice <- function(values, taxa) {
  if (!length(values)) return(character(0))                  # NULL/empty pick → no filter
  values <- as.character(values)
  g <- startsWith(values, "grp:"); s <- startsWith(values, "sci:")
  unique(c(unlist(taxa[sub("^grp:", "", values[g])], use.names = FALSE), sub("^sci:", "", values[s])))
}

#' One value -> a one-element named list `label -> scientificNames` (for ik_location_metric/ik_rai,
#' which take `list(Label = sci)`). A group keeps its label; a species uses its display name.
#' @keywords internal
ik_choice_taxa <- function(value, taxa, ik_data, prefer = "vernacular") {
  if (length(value) != 1 || is.na(value)) return(NULL)
  value <- as.character(value)
  if (startsWith(value, "grp:")) { lbl <- sub("^grp:", "", value); stats::setNames(list(unlist(taxa[[lbl]], use.names = FALSE)), lbl) }
  else if (startsWith(value, "sci:")) { sn <- sub("^sci:", "", value); stats::setNames(list(sn), ik_species_label(sn, ik_data, prefer)) }
  else NULL
}

#' Friendly display labels for selected `ik_species_choices()` values (group label or species name).
#' @keywords internal
ik_choice_labels <- function(values, ik_data, prefer = "vernacular") {
  if (!length(values)) return(character(0))
  values <- as.character(values)
  vapply(values, function(v) if (startsWith(v, "grp:")) sub("^grp:", "", v)
         else if (startsWith(v, "sci:")) ik_species_label(sub("^sci:", "", v), ik_data, prefer)
         else v, character(1), USE.NAMES = FALSE)
}

#' Group taxa map (label -> scientificNames) for ALL configured groups, priority order — the
#' canonical map for resolving grp: choices. @keywords internal
ik_group_taxa <- function(ik_data) {
  sg <- ik_species_groups(ik_data); ord <- order(sg$priority)
  split(sg$scientificName[ord], factor(sg$label[ord], levels = unique(sg$label[ord])))
}

#' The unified "Species" picker: every configured group (split into sub-species when its project
#' `split` flag is set), in priority order, THEN every species in the data belonging to no group,
#' listed individually. Values grp:/sci: (resolve via ik_resolve_species_choice + ik_group_taxa).
#' @keywords internal
ik_species_choices_full <- function(ik_data, prefer = "vernacular", all_label = NULL, all_value = "__all__") {
  sg     <- ik_species_groups(ik_data)
  grp    <- ik_group_taxa(ik_data)
  splits <- unique(sg$label[which(sg$split)])
  ch     <- ik_species_choices(grp, ik_data, prefer, split = splits, all_label = all_label, all_value = all_value)
  all_sci <- sort(unique(stats::na.omit(ik_observations(ik_data, with_location = FALSE)$scientificName)))
  ung <- setdiff(all_sci, unique(unlist(grp, use.names = FALSE)))
  if (length(ung)) { labs <- ik_species_label(ung, ik_data, prefer)
    for (i in order(labs)) ch[[labs[i]]] <- paste0("sci:", ung[i]) }
  ch
}

#' Animal observations of the resolved selection, tagged with location_id/reserve/line and
#' (camera only, when `net`) with possible-duplicates dropped. @keywords internal
.metrics_obs <- function(ik_data, r, locs, net = FALSE) {
  obs <- r$observations
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  if (net && nrow(obs)) {
    rel <- ik_relations(ik_data)
    dup <- rel$possible_duplicate[match(obs$observationID, rel$observationID)]
    obs <- obs[is.na(dup) | !dup, , drop = FALSE]                 # keep non-duplicates
  }
  obs$count[is.na(obs$count)] <- 1L
  obs$location_id <- obs$locationID                                # carried by ik_observations(with_location)
  obs$line        <- locs$line[match(obs$location_id, locs$location_id)]
  obs$reserve     <- locs$reserve[match(obs$location_id, locs$location_id)]
  obs
}

#' Effort per group for the resolved deployments — the groups WITH effort (a metric's
#' denominator), `effort_hours` transformed by `unit_fn`. `by` = the grouping/carry columns
#' (default reserve·line; the per-location metric adds location_id + coords); `key` = the
#' columns a deployment must have non-NA to be kept (defaults to `by`). @keywords internal
.metrics_effort <- function(r, locs, by = c("reserve", "line"), key = by, unit_fn = identity) {
  dep <- r$deployments
  dep$effort_hours <- as.numeric(difftime(dep$deploymentEnd, dep$deploymentStart, units = "hours"))
  m <- match(dep$locationID, locs$location_id)
  dep$location_id <- locs$location_id[m]
  dep$name        <- locs$name[m]
  dep$latitude    <- locs$latitude[m]
  dep$longitude   <- locs$longitude[m]
  dep$line        <- locs$line[m]
  dep$reserve     <- locs$reserve[m]
  dep <- dep[stats::complete.cases(dep[, key, drop = FALSE]), , drop = FALSE]   # drop NA-key deployments
  dep |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(effort = unit_fn(sum(.data$effort_hours, na.rm = TRUE)), .groups = "drop")
}

#' THE RAI/rate atom: per-taxon value joined to ALL effort-groups (0 where no detections), then
#' `metric = value / effort × norm`. `key` = the grouping columns (present on both `effort` and
#' `obs`); the SAME maths whether key is reserve·line (ik_rai/ik_trap_rate) or location_id (the
#' map). @keywords internal
.metrics_grouped <- function(effort, obs, taxa, norm, key = c("reserve", "line")) {
  dplyr::bind_rows(lapply(names(taxa), function(lab) {
    o <- obs[obs$scientificName %in% taxa[[lab]], , drop = FALSE]
    cnt <- if (nrow(o)) {
      dplyr::summarise(dplyr::group_by(o, dplyr::across(dplyr::all_of(key))),
                       value = sum(.data$count), .groups = "drop")
    } else dplyr::mutate(effort[0, key, drop = FALSE], value = numeric(0))
    out <- dplyr::left_join(effort, cnt, by = key)
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
  effort <- .metrics_effort(r, locs, by = c("reserve", "line"))   # camera-hours / line (pulse = exact)
  obs    <- .metrics_obs(ik_data, r, locs, net = net)
  lines  <- dplyr::rename(.metrics_grouped(effort, obs, taxa, norm, key = c("reserve", "line")),
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
  # Traps without a line still belong to their reserve — roll them up under one "(unlined)" pseudo-line
  # so their effort and catches count (the reserve×line grouping would otherwise DROP NA-line traps,
  # losing real captures, e.g. a goat caught at an unlined trap). Camera RAI is line-based by protocol,
  # so this is trap-only. NA reserve is already mapped to a pseudo-reserve at geography build.
  locs$line[is.na(locs$line)] <- "(unlined)"

  r      <- ik_resolve(ik_data, spec, source_type = "trap")
  effort <- .metrics_effort(r, locs, by = c("reserve", "line"), unit_fn = function(h) h / 24)  # trap-days
  obs    <- .metrics_obs(ik_data, r, locs, net = FALSE)              # date-only: net == raw
  lines  <- dplyr::rename(.metrics_grouped(effort, obs, taxa, norm, key = c("reserve", "line")),
                          captures = "value", trap_days = "effort")

  summary <- .metrics_rollup(lines, level)
  summary$norm_trap_days <- norm
  list(lines = lines, summary = summary)
}

#' Per-LOCATION detection rate per taxon — the camera-RAI / trap-rate atom evaluated at a single
#' point, for the map (markers + IDW surface). Same maths and config as ik_rai/ik_trap_rate
#' (value / effort × norm — the SAME atom as ik_rai, at point grain), grouped by LOCATION not line,
#' with NO SE: a location is one point, not a sample of replicate lines. A location with effort
#' but no detections of a taxon carries value 0 (a true zero on the map), so the denominator is
#' the deployed-location set, not just where the animal was seen.
#'
#' @param ik_data The ik_data container.
#' @param spec    A selection spec (named list; see ik_select / ik_resolve).
#' @param taxa    Named list label -> scientificNames (e.g. ik_taxa_groups(sg)).
#' @param source_type "camera" (detection rate) or "trap" (capture rate).
#' @param norm Normalisation constant (override). Default = the project config norm
#'   (camera = `norm_hours` 2000; trap = `norm_trap_days` 100). NB the 2000 CH norm is a
#'   per-LINE construct (DOC protocol: index per line, mean ± SE across lines — see ik_rai);
#'   for a PER-CAMERA detection rate the map passes a per-camera norm (one 21-night
#'   deployment ≈ 500 CH = one camera's share of the 2000 CH line norm), so a point reads as a per-camera rate, not a
#'   line-scaled "RAI". This function is point-grain only; the protocol RAI is ik_rai().
#' @return data.frame, one row per location × taxon: location_id · name · latitude · longitude ·
#'   reserve · line · taxon · (individuals|captures) · (camera_hours|trap_days) · metric · norm.
#'   NULL when the selection has no deployed locations. Rows may have NA coords (e.g. coordless
#'   traps) — the map filters those.
ik_location_metric <- function(ik_data, spec, taxa, source_type = c("camera", "trap"), norm = NULL) {
  source_type <- match.arg(source_type)
  locs <- ik_data$app$geography$locations
  by   <- c("location_id", "name", "latitude", "longitude", "reserve", "line")  # carry coords
  key  <- "location_id"                                                          # the point is the unit

  if (source_type == "camera") {
    cfg    <- ik_data$meta$camera$rai %||% list(norm_hours = 2000, use_net = TRUE)
    nrm    <- norm %||% cfg$norm_hours %||% 2000
    r      <- ik_resolve(ik_data, spec, source_type = "camera")
    effort <- .metrics_effort(r, locs, by = by, key = key)              # camera-hours / location
    obs    <- .metrics_obs(ik_data, r, locs, net = isTRUE(cfg$use_net))
    out    <- dplyr::rename(.metrics_grouped(effort, obs, taxa, nrm, key = key),
                            individuals = "value", camera_hours = "effort")
  } else {
    cfg    <- ik_data$meta$trapping$rate %||% list(norm_trap_days = 100)
    nrm    <- norm %||% cfg$norm_trap_days %||% 100
    r      <- ik_resolve(ik_data, spec, source_type = "trap")
    effort <- .metrics_effort(r, locs, by = by, key = key, unit_fn = function(h) h / 24)  # trap-days
    obs    <- .metrics_obs(ik_data, r, locs, net = FALSE)
    out    <- dplyr::rename(.metrics_grouped(effort, obs, taxa, nrm, key = key),
                            captures = "value", trap_days = "effort")
  }
  if (!nrow(out)) return(NULL)
  out$norm <- nrm
  out
}

#' The animal observations BEHIND a metric cell — the auditable basis a RAI/rate figure is built
#' from, for the Overview drill down to the records. Re-resolves the same selection + counting
#' rules (camera = net per config; trap = raw) and filters to the clicked taxon / reserve / line.
#'
#' @param ik_data The ik_data container. @param spec The selection spec. @param taxa Named list
#'   label -> scientificNames (as passed to ik_rai/ik_trap_rate).
#' @param taxon The group label (a name of `taxa`). @param reserve,line Optional cell coords.
#' @param source_type "camera" (RAI) or "trap" (rate).
#' @return data.frame: observationID · when · scientificName · count · location_id · reserve ·
#'   line · locationName, newest first; NULL when none. (`location_id` lets the map link a
#'   record to its marker.)
ik_metric_obs <- function(ik_data, spec, taxa, taxon, reserve = NULL, line = NULL,
                          source_type = c("camera", "trap")) {
  source_type <- match.arg(source_type)
  net  <- source_type == "camera" && isTRUE((ik_data$meta$camera$rai %||% list())$use_net)
  locs <- ik_data$app$geography$locations
  if (source_type == "trap") locs$line[is.na(locs$line)] <- "(unlined)"  # match ik_trap_rate's pseudo-line
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
             location_id = o$location_id[ord], reserve = o$reserve[ord], line = o$line[ord],
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
