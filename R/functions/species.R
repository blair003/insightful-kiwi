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
                                   default_compare = "none", default_period = "latest_complete"),
                   proximity = list(max_radius_m = 2000),
                   media    = list(keep_originals = TRUE),
                   features = list())   # per-feature on/off (omitted = on); see ik_feature_enabled()
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
                                         get0("media", envir = e, ifnotfound = list())),
    # feature flags: a flat name = TRUE/FALSE list; absent or TRUE = shown (subject to data capability)
    features         = utils::modifyList(defaults$features,
                                         get0("features", envir = e, ifnotfound = list()))
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

# ── Species dashboards ────────────────────────────────────────────────────────────────────────
# The per-species pages (Species menu). One "taxon spec" per species GROUP, plus one per resolvable
# member of a split=TRUE group (Mustelids → Stoat / Ferret / Least weasel). Ambiguous members
# (a vernacular with "/" or a higher-taxon term) stay in the GROUP page only.

#' kebab-case slug for nav values / ids. @keywords internal
.ik_slug <- function(x) gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(x)))

#' A member vernacular that can't be a clean single-species page (ambiguous ID or a higher taxon).
#' @keywords internal
.ik_vernacular_ambiguous <- function(v)
  is.na(v) || !nzchar(v) || grepl("/", v, fixed = TRUE) ||
  grepl("^(mustelid|mustelids|mustelidae|rodent|rodents|rat sp\\.?|unidentified)$", v, ignore.case = TRUE)

#' Taxon specs for the Species dashboards — one per group present in the data, plus per resolvable
#' split member. Each: `key` (nav value), `label`, `sci` (scientificNames it covers), `role`,
#' `group` (parent group label), `member` (TRUE for a split sub-species), `camera`/`trapped` (does it
#' appear in camera / trap data — gates device-specific tabs). Groups absent from the data are dropped.
#' @param ik_data The container. @return list of specs. @keywords internal
ik_species_taxa <- function(ik_data) {
  sg  <- ik_species_groups(ik_data)
  obs <- ik_observations(ik_data, with_location = FALSE)
  dp  <- ik_deployment_period(ik_data)
  src <- dp$source_type[match(obs$deploymentID, dp$deploymentID)]
  anim <- !is.na(obs$observationType) & obs$observationType == "animal" & !is.na(obs$scientificName)
  cam_sci <- unique(obs$scientificName[anim & !is.na(src) & src == "camera"])
  trp_sci <- unique(obs$scientificName[anim & !is.na(src) & src == "trap"])

  groups <- unique(sg$label[order(sg$priority)])
  specs  <- list()
  add <- function(key, label, sci, role, group, member) {
    if (!length(sci)) return(invisible())
    specs[[length(specs) + 1L]] <<- list(
      key = key, label = label, sci = sci, role = role, group = group, member = member,
      camera = any(sci %in% cam_sci), trapped = any(sci %in% trp_sci))
  }
  for (gl in groups) {
    rows <- sg[sg$label == gl, , drop = FALSE]
    gsci <- unique(rows$scientificName)
    split_grp <- any(rows$split, na.rm = TRUE)
    add(paste0("grp-", .ik_slug(gl)), if (split_grp) paste0(gl, " (all)") else gl, gsci, rows$role[1], gl, FALSE)
    if (split_grp) {                                          # resolvable members → their own page
      vern <- vapply(gsci, function(s) ik_species_label(s, ik_data, "vernacular") %||% NA_character_, character(1))
      keep <- !vapply(vern, .ik_vernacular_ambiguous, logical(1))
      if (any(keep)) for (vl in sort(unique(tools::toTitleCase(tolower(vern[keep]))))) {
        msci <- gsci[keep][tools::toTitleCase(tolower(vern[keep])) == vl]
        add(paste0("sp-", .ik_slug(vl)), vl, unique(msci), rows$role[1], gl, TRUE)
      }
    }
  }
  Filter(function(s) s$camera || s$trapped, specs)            # drop taxa with no data at all
}

#' Per-period network trend (camera RAI + trap catch-rate, mean ± SE over reserves) for one or more
#' taxa — the Species-dashboard landing chart. Mirrors ik_outcome_series but for arbitrary taxa and
#' supports a By-year roll-up. @param taxa Named list label→scientificNames. @param by "season"
#' (each season-year point) or "year" (austral cycle). @return long df: period (ordered factor) ·
#' order · metric_type (camera_rai / trap_rate intensity + camera_occ / trap_occ extent) · taxon ·
#' value · se; NULL when no seasons. @keywords internal
ik_species_trend <- function(ik_data, taxa, by = c("season", "year"), reserve = NULL) {
  by <- match.arg(by)
  dp <- ik_deployment_period(ik_data)
  levels <- ik_season_levels(dp)
  if (!length(levels) || !length(taxa)) return(NULL)
  if (by == "season") {
    periods <- lapply(seq_along(levels), function(i) list(label = levels[i], order = i, seasons = levels[i]))
  } else {
    info <- unique(dp[!is.na(dp$calendar_season), c("calendar_season", "season", "season_year")])
    info$ylab <- .ik_cycle_label(info$season, info$season_year)
    lvl_y <- info$ylab[match(levels, info$calendar_season)]
    ys <- unique(lvl_y[!is.na(lvl_y)])
    periods <- lapply(ys, function(y) { ss <- levels[which(lvl_y == y)]; list(label = y, order = min(match(ss, levels)), seasons = ss) })
    periods <- periods[order(vapply(periods, `[[`, numeric(1), "order"))]
    for (k in seq_along(periods)) periods[[k]]$order <- k
  }
  ms  <- .ik_metric_series(ik_data, periods, taxa, taxa, reserve)   # intensity: RAI + catch-rate
  if (!is.null(ms)) ms$n_reserves <- NULL                     # species trend doesn't surface it
  occ <- .ik_occupancy_series(ik_data, periods, taxa, reserve)      # extent: % of sites occupied
  out <- dplyr::bind_rows(ms, occ)
  if (is.null(out) || !nrow(out)) return(NULL)
  out$period <- factor(out$period, levels = unique(out$period[order(out$order)]))
  out
}

#' Per-period network metric series — the shared engine behind `ik_species_trend()` and
#' `ik_outcome_series()`. For each period (a `label` + `order` + its constituent calendar `seasons`)
#' compute camera RAI (for `cam_taxa`) and trap catch-rate (for `trap_taxa`), each combined across
#' reserves to a network mean ± SE. @param periods list of list(label, order, seasons).
#' @param cam_taxa,trap_taxa named taxa lists (label→sci); pass the same list for both to chart one
#' species both ways. @param reserve optional reserve filter. @return df: period · order · metric_type
#' (camera_rai/trap_rate) · taxon · value · se · n_reserves; NULL when empty. @keywords internal
.ik_metric_series <- function(ik_data, periods, cam_taxa, trap_taxa, reserve = NULL) {
  net <- function(s, mtype, p) {
    if (is.null(s) || !nrow(s)) return(NULL)
    n <- ik_metric_combine(s); if (!nrow(n)) return(NULL)
    data.frame(period = p$label, order = p$order, metric_type = mtype, taxon = n$taxon,
               value = n$metric, se = n$se, n_reserves = n$n_lines, stringsAsFactors = FALSE)
  }
  rows <- lapply(periods, function(p) {
    sp   <- list(season = unlist(lapply(p$seasons, function(s) ik_expand_period(paste0("season:", s), ik_data))),
                 reserve = reserve)
    rai  <- if (length(cam_taxa))  tryCatch(ik_rai(ik_data, sp, cam_taxa, level = "reserve")$summary, error = function(e) NULL) else NULL
    rate <- if (length(trap_taxa)) tryCatch(ik_trap_rate(ik_data, sp, trap_taxa, level = "reserve")$summary, error = function(e) NULL) else NULL
    dplyr::bind_rows(net(rai, "camera_rai", p), net(rate, "trap_rate", p))
  })
  out <- dplyr::bind_rows(rows); if (!nrow(out)) return(NULL)
  out
}

#' Per-period NAIVE OCCUPANCY series — the Species Trend's EXTENT complement to RAI/catch-rate: for
#' each period and taxon, the share of DEPLOYED sites holding >=1 record (camera detections / trap
#' catches), as a percentage with a binomial SE. Effort-sensitive (more monitoring -> higher naive
#' occupancy), so exploratory — read the shape over seasons (range contraction/expansion), not the
#' absolute level. Computed for BOTH devices per taxon; the Trend renderer drops the device a taxon
#' has no data on (same as the RAI/rate facets). `value` is a percent 0-100. @param periods list of
#' list(label, order, seasons). @param taxa named list label->sci. @return long df (period · order ·
#' metric_type camera_occ/trap_occ · taxon · value · se); NULL when empty. @keywords internal
.ik_occupancy_series <- function(ik_data, periods, taxa, reserve = NULL) {
  one <- function(p, lab, sci, src, mtype) {
    sp <- list(season = unlist(lapply(p$seasons, function(s) ik_expand_period(paste0("season:", s), ik_data))),
               reserve = reserve)
    m  <- tryCatch(ik_location_metric(ik_data, sp, stats::setNames(list(sci), lab), src), error = function(e) NULL)
    if (is.null(m) || !nrow(m)) return(NULL)
    cc  <- if ("individuals" %in% names(m)) "individuals" else "captures"   # >0 = occupied that period
    tot <- nrow(m); if (!tot) return(NULL); n <- sum(m[[cc]] > 0, na.rm = TRUE); pr <- n / tot
    data.frame(period = p$label, order = p$order, metric_type = mtype, taxon = lab,
               value = 100 * pr, se = 100 * sqrt(pr * (1 - pr) / tot), stringsAsFactors = FALSE)
  }
  rows <- lapply(periods, function(p)
    dplyr::bind_rows(unlist(lapply(seq_along(taxa), function(i) list(
      one(p, names(taxa)[i], taxa[[i]], "camera", "camera_occ"),
      one(p, names(taxa)[i], taxa[[i]], "trap",   "trap_occ"))), recursive = FALSE)))
  out <- dplyr::bind_rows(rows); if (!nrow(out)) return(NULL)
  out
}

#' CAMERA detections of `taxa` with their event time + diel period — for the species Behaviour tab
#' (time-of-day + diel). @param taxa Named list label→sci. @param selection A selection SPEC
#' (period/season + reserve etc.); honoured in full so the Behaviour tab tracks the sidebar.
#' @return df: when (POSIXct) · hour (0-23) · diel (factor IK_DIEL_PERIODS); NULL when none. @keywords internal
ik_species_camera_events <- function(ik_data, taxa, selection = list()) {
  o <- tryCatch(ik_metric_obs(ik_data, selection %||% list(), taxa, names(taxa)[1], source_type = "camera"),
                error = function(e) NULL)
  if (is.null(o) || !nrow(o)) return(NULL)
  data.frame(when = o$when, hour = as.integer(format(o$when, "%H")),
             diel = ik_diel_period(ik_data, o$when, o$reserve), stringsAsFactors = FALSE)
}

#' Effort-normalised diel activity for a camera species: detections per AVAILABLE camera-hour in each
#' diel period (Matutinal/Diurnal/Vespertine/Nocturnal). Available hours = total camera-hours in scope
#' × the mean fraction of the day each period spans (from the per-reserve sun table), so a long night
#' isn't mistaken for more activity. @param taxa Named list label→sci. @param selection A selection
#' SPEC (period/season + reserve); the detections AND the deployed-effort denominator both honour it.
#' @return df: period (ordered) · detections · avail_hours · rate; NULL when none. @keywords internal
ik_species_diel <- function(ik_data, taxa, selection = list()) {
  ev <- ik_species_camera_events(ik_data, taxa, selection)
  if (is.null(ev)) return(NULL)
  cnt <- as.integer(table(factor(ev$diel, levels = IK_DIEL_PERIODS)))
  r   <- tryCatch(ik_resolve(ik_data, selection %||% list(), source_type = "camera"), error = function(e) NULL)
  eff <- if (is.null(r) || !nrow(r$deployments)) 0 else
    sum(as.numeric(difftime(r$deployments$deploymentEnd, r$deployments$deploymentStart, units = "hours")), na.rm = TRUE)
  reserve <- (selection %||% list())$reserve
  sun <- ik_temporal(ik_data)$sun
  if (!is.null(reserve) && length(reserve)) sun <- sun[sun$reserve %in% reserve, , drop = FALSE]
  hf  <- function(a, b) as.numeric(difftime(b, a, units = "hours"))
  fr  <- c(Matutinal  = mean(hf(sun$civil_dawn, sun$sunrise), na.rm = TRUE),
           Diurnal    = mean(hf(sun$sunrise, sun$sunset),     na.rm = TRUE),
           Vespertine = mean(hf(sun$sunset, sun$civil_dusk),  na.rm = TRUE))
  fr["Nocturnal"] <- max(0, 24 - sum(fr, na.rm = TRUE))
  avail <- eff * (fr[IK_DIEL_PERIODS] / 24)
  data.frame(period = factor(IK_DIEL_PERIODS, levels = IK_DIEL_PERIODS), detections = cnt,
             avail_hours = as.numeric(avail),
             rate = ifelse(as.numeric(avail) > 0, cnt / as.numeric(avail), NA_real_), stringsAsFactors = FALSE)
}

#' Overall diel CLASS for a camera species, from its effort-normalised per-period rate SHARES.
#' Compares the four diel-period rates (each ÷ their total) against IK_DIEL_CLASS_RULES (project-
#' overridable via ik_data$meta$diel) to label the species Diurnal / Nocturnal / Crepuscular /
#' Cathemeral / Arrhythmic — or "Insufficient data" under the minimum-observation floor. Confidence
#' ("none"/"low"/"ok") comes from the net-detection count. @param taxa Named list label→sci.
#' @param selection A selection SPEC (period/season + reserve), honoured in full. @return list(class,
#' desc, confidence, n, shares [named integer % in IK_DIEL_PERIODS order], min_obs, low_obs, diel);
#' NULL when no camera detections at all. @keywords internal
ik_diel_class <- function(ik_data, taxa, selection = list()) {
  d <- ik_species_diel(ik_data, taxa, selection)
  if (is.null(d)) return(NULL)
  rules <- ik_data$meta$diel %||% IK_DIEL_CLASS_RULES
  n <- sum(d$detections, na.rm = TRUE)
  r <- d$rate; r[!is.finite(r)] <- 0; names(r) <- as.character(d$period)
  tot   <- sum(r)
  share <- if (tot > 0) r / tot else stats::setNames(rep(0, length(r)), names(r))
  day <- share[["Diurnal"]]; night <- share[["Nocturnal"]]
  mat <- share[["Matutinal"]]; ves <- share[["Vespertine"]]; crep <- mat + ves
  cls <- if (n < rules$min_obs)                                       "Insufficient data"
    else if (day   >= rules$dominant)                                "Diurnal"
    else if (night >= rules$dominant)                                "Nocturnal"
    else if (crep  >= rules$crepuscular && mat > 0 && ves > 0)        "Crepuscular"
    else if (day   >= rules$cathemeral && night >= rules$cathemeral)  "Cathemeral"
    else                                                             "Arrhythmic"
  desc <- switch(cls,
    Diurnal = "Day-active", Nocturnal = "Night-active", Crepuscular = "Dawn/dusk-active",
    Cathemeral = "Active day and night", Arrhythmic = "No clear diel rhythm",
    sprintf("Fewer than %d net detections", rules$min_obs))
  conf <- if (n < rules$min_obs) "none" else if (n < rules$low_obs) "low" else "ok"
  list(class = cls, desc = desc, confidence = conf, n = n,
       shares = round(100 * share[IK_DIEL_PERIODS]),
       min_obs = rules$min_obs, low_obs = rules$low_obs, diel = d)
}

#' The per-observation table behind the Diel Activity card — every camera detection of `taxa` in the
#' selection, with its diel class AND the four sun boundaries that classified it (each detection's
#' OWN date + reserve), so a classification can be checked by eye. Powers the card's drill-down.
#' @param taxa Named list label→sci. @param selection A selection SPEC. @return df: observationID ·
#' when · reserve · location · diel · civil_dawn · sunrise · sunset · civil_dusk (the four are POSIXct,
#' NA where the day/reserve isn't in the sun table); NULL when none. @keywords internal
ik_species_diel_records <- function(ik_data, taxa, selection = list()) {
  o <- tryCatch(ik_metric_obs(ik_data, selection %||% list(), taxa, names(taxa)[1], source_type = "camera"),
                error = function(e) NULL)
  if (is.null(o) || !nrow(o)) return(NULL)
  b <- .diel_bounds(ik_data, o$when, o$reserve)
  data.frame(observationID = o$observationID, when = o$when, reserve = o$reserve, location = o$locationName,
             diel = as.character(ik_diel_period(ik_data, o$when, o$reserve)),
             civil_dawn = b$dawn, sunrise = b$sunrise, sunset = b$sunset, civil_dusk = b$dusk,
             stringsAsFactors = FALSE)
}
