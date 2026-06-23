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
#' order · metric_type (camera_rai/trap_rate) · taxon · value · se; NULL when no seasons. @keywords internal
ik_species_trend <- function(ik_data, taxa, by = c("season", "year"), reserve = NULL) {
  by <- match.arg(by)
  dp <- ik_deployment_period(ik_data)
  levels <- ik_season_levels(dp)
  if (!length(levels) || !length(taxa)) return(NULL)
  if (by == "season") {
    periods <- lapply(seq_along(levels), function(i) list(label = levels[i], order = i, seasons = levels[i]))
  } else {
    info <- unique(dp[!is.na(dp$calendar_season), c("calendar_season", "season", "season_year")])
    info$ylab <- { cy <- .ik_cycle_year(info$season, info$season_year); sprintf("%d/%02d", cy, (cy + 1L) %% 100L) }
    lvl_y <- info$ylab[match(levels, info$calendar_season)]
    ys <- unique(lvl_y[!is.na(lvl_y)])
    periods <- lapply(ys, function(y) { ss <- levels[which(lvl_y == y)]; list(label = y, order = min(match(ss, levels)), seasons = ss) })
    periods <- periods[order(vapply(periods, `[[`, numeric(1), "order"))]
    for (k in seq_along(periods)) periods[[k]]$order <- k
  }
  net <- function(s, mtype, p) {
    if (is.null(s) || !nrow(s)) return(NULL)
    n <- ik_metric_combine(s); if (!nrow(n)) return(NULL)
    data.frame(period = p$label, order = p$order, metric_type = mtype,
               taxon = n$taxon, value = n$metric, se = n$se, stringsAsFactors = FALSE)
  }
  rows <- lapply(periods, function(p) {
    sp   <- list(season = unlist(lapply(p$seasons, function(s) ik_expand_period(paste0("season:", s), ik_data))),
                 reserve = reserve)
    rai  <- tryCatch(ik_rai(ik_data, sp, taxa, level = "reserve")$summary, error = function(e) NULL)
    rate <- tryCatch(ik_trap_rate(ik_data, sp, taxa, level = "reserve")$summary, error = function(e) NULL)
    dplyr::bind_rows(net(rai, "camera_rai", p), net(rate, "trap_rate", p))
  })
  out <- dplyr::bind_rows(rows); if (!nrow(out)) return(NULL)
  out$period <- factor(out$period, levels = unique(out$period[order(out$order)]))
  out
}
