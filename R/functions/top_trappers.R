# top_trappers.R — "Top trappers": a gamified per-season leaderboard of the volunteers who check the
# traps. Volunteer id, bait and catch status live in observationTags (deployments$setupBy is empty);
# each trap observation == one check, observationType "animal" == a catch. Scored by a weighted catch
# tally (stoat #1; weights tunable via project.R trapping$target_weights → meta), with single-winner
# TROPHIES and threshold BADGES so most active trappers earn something. One completed season at a time
# (a single season spreads the trophies far better than an all-time tally). By volunteer ID for now.

# Default catch weights by scientificName — bycatch (birds, Animalia) weight 0. Ordering = the WKT
# concern ranking: Stoat/Ferret > Weasel > Cat > Rats/Possums > Hedgehog > Mouse > Rabbit (ambiguous
# "Mustelidae" scores as a weasel — the lowest mustelid, conservative). Override per project in
# project.R as `trapping$target_weights`. See the wkt-species-priority memory + docs/dev/wkt-notes.md.
IK_DEFAULT_TRAP_WEIGHTS <- c(
  "Mustela erminea" = 6, "Mustela putorius furo" = 5, "Mustela nivalis" = 4, "Mustelidae" = 4,
  "Felis catus" = 3, "Rattus" = 2, "Trichosurus vulpecula" = 2, "Erinaceus europaeus" = 1.5,
  "Mus musculus" = 1, "Oryctolagus cuniculus" = 0.5)
.TT_MUSTELIDS <- c("Mustela erminea", "Mustela nivalis", "Mustela putorius furo", "Mustelidae")

#' Cheap check: does any trap data carry volunteer tags? (For conditionally showing the nav.)
#' @keywords internal
ik_has_trappers <- function(ik_data) {
  trap_ds <- names(ik_data$datasets)[vapply(ik_data$datasets,
    function(d) identical(d$meta$source_type %||% NA_character_, "trap"), logical(1))]
  for (id in trap_ds) {
    o <- ik_data$datasets[[id]]$package$data$observations
    if (!is.null(o$observationTags) && any(grepl("volunteer:", o$observationTags, fixed = TRUE))) return(TRUE)
  }
  FALSE
}

#' Extract a `key:value` field from a `|`-joined observationTags string. @keywords internal
.tt_tag <- function(tag, s) ifelse(grepl(paste0(tag, ":"), s),
                                   trimws(sub(paste0(".*\\b", tag, ":\\s*([^|]*).*"), "\\1", s)), NA_character_)

#' Every trap CHECK with its volunteer/bait/season/place/catch facts (the leaderboard's raw rows).
#' @keywords internal
.tt_checks <- function(ik_data, weights = NULL) {
  w <- weights %||% (ik_data$meta$trapping$target_weights %||% IK_DEFAULT_TRAP_WEIGHTS)
  trap_ds <- names(ik_data$datasets)[vapply(ik_data$datasets,
    function(d) identical(d$meta$source_type %||% NA_character_, "trap"), logical(1))]
  if (!length(trap_ds)) return(NULL)
  locs <- ik_data$app$geography$locations
  po   <- ik_data$app$period$observations
  out <- do.call(rbind, lapply(trap_ds, function(id) {
    pkg <- ik_data$datasets[[id]]$package; obs <- pkg$data$observations; dep <- pkg$data$deployments
    di  <- match(obs$deploymentID, dep$deploymentID)
    data.frame(
      observationID = obs$observationID,
      volunteer = .tt_tag("volunteer", obs$observationTags),
      bait      = .tt_tag("bait", obs$observationTags),
      scientificName = obs$scientificName,
      is_animal = !is.na(obs$observationType) & obs$observationType == "animal",
      locID     = dep$locationID[di],
      dep_days  = as.numeric(difftime(dep$deploymentEnd[di], dep$deploymentStart[di], units = "days")),
      stringsAsFactors = FALSE)
  }))
  out$reserve     <- locs$reserve[match(out$locID, locs$location_id)]
  out$season      <- po$season[match(out$observationID, po$observationID)]
  out$season_year <- po$season_year[match(out$observationID, po$observationID)]
  out$weight   <- unname(ifelse(out$scientificName %in% names(w), w[out$scientificName], 0))
  out$is_catch <- out$is_animal & out$weight > 0
  out$pts      <- ifelse(out$is_catch, out$weight, 0)
  out[!is.na(out$volunteer) & nzchar(out$volunteer), , drop = FALSE]
}

#' Completed trapping seasons that have checks, newest first (for the season picker).
#' @return ik_season_calendar rows (calendar_season·season·season_year·start·end·complete) or NULL.
ik_top_trapper_seasons <- function(ik_data) {
  sc <- tryCatch(ik_season_calendar(ik_data), error = function(e) NULL)
  if (is.null(sc) || !nrow(sc)) return(NULL)
  chk <- .tt_checks(ik_data); if (is.null(chk) || !nrow(chk)) return(NULL)
  have <- unique(paste(chk$season_year, chk$season))
  sc <- sc[sc$complete & paste(sc$season_year, sc$season) %in% have, , drop = FALSE]
  if (!nrow(sc)) return(NULL)
  sc[order(sc$start, decreasing = TRUE), , drop = FALSE]
}

#' The Top-trappers leaderboard for one completed season.
#'
#' @param ik_data The container. @param season_year,season The season (default: latest completed).
#' @param weights Optional named catch-weight vector (default meta / IK_DEFAULT_TRAP_WEIGHTS).
#' @return list(per, label, season, season_year, n_active, n_awarded, weights) where `per` is the
#'   ranked per-volunteer table with list-columns `trophies` / `badges`; or NULL when no data.
ik_top_trappers <- function(ik_data, season_year = NULL, season = NULL, weights = NULL) {
  w_used <- weights %||% (ik_data$meta$trapping$target_weights %||% IK_DEFAULT_TRAP_WEIGHTS)
  chk <- .tt_checks(ik_data, w_used); if (is.null(chk) || !nrow(chk)) return(NULL)
  if (is.null(season_year) || is.null(season)) {
    ss <- ik_top_trapper_seasons(ik_data); if (is.null(ss)) return(NULL)
    season_year <- ss$season_year[1]; season <- ss$season[1]
  }
  d <- chk[chk$season_year == season_year & chk$season == season, , drop = FALSE]
  if (!nrow(d)) return(NULL)

  agg <- d |> dplyr::group_by(volunteer) |> dplyr::summarise(
    checks = dplyr::n(), catches = sum(is_catch), score = sum(pts),
    trap_nights = round(sum(dep_days, na.rm = TRUE)),
    species   = dplyr::n_distinct(scientificName[is_catch]),
    traps     = dplyr::n_distinct(locID),
    baits     = dplyr::n_distinct(trimws(unlist(strsplit(bait[!is.na(bait)], ",")))),
    stoats    = sum(is_animal & scientificName == "Mustela erminea"),
    mustelids = sum(is_animal & scientificName %in% .TT_MUSTELIDS),
    rats      = sum(is_animal & scientificName == "Rattus"),
    .groups = "drop") |> as.data.frame()
  agg$rate10 <- ifelse(agg$checks > 0, round(10 * agg$catches / agg$checks, 1), 0)

  cw <- d[d$is_catch, , drop = FALSE]                                  # top species per volunteer
  top_sci <- tapply(cw$scientificName, cw$volunteer, function(x) names(which.max(table(x))))
  agg$top_species_sci <- unname(top_sci[agg$volunteer])
  agg$top_species <- ifelse(is.na(agg$top_species_sci), "—",
                            ik_species_label(agg$top_species_sci, ik_data, "vernacular"))
  bw <- d[!is.na(d$bait) & nzchar(d$bait), , drop = FALSE]             # favourite bait per volunteer
  top_bait <- tapply(bw$bait, bw$volunteer, function(x) {
    b <- trimws(unlist(strsplit(x, ","))); b <- b[nzchar(b)]
    if (length(b)) names(sort(table(b), decreasing = TRUE))[1] else NA_character_ })
  agg$top_bait <- unname(top_bait[agg$volunteer])

  per <- agg[order(-agg$score, -agg$catches, -agg$checks), , drop = FALSE]
  per$rank <- seq_len(nrow(per))

  # TROPHIES — one winner each (ties broken by more checks). Stoats are too rare for a trophy → a badge.
  won <- list()
  trophy <- function(col, name, min_checks = 0) {
    e <- per[per$checks >= min_checks & per[[col]] > 0, , drop = FALSE]
    if (!nrow(e)) return(invisible())
    e <- e[order(-e[[col]], -e$checks), ]; won[[name]] <<- e$volunteer[1]
  }
  trophy("score", "Top Trapper");      trophy("catches", "Most Catches")
  trophy("checks", "Hardest Worker");  trophy("species", "Most Variety")
  trophy("traps", "Most Ground Covered"); trophy("baits", "Bait Innovator")
  trophy("rats", "Rat Patrol");        trophy("rate10", "Sharpshooter", min_checks = 24)
  for (rsv in sort(unique(d$reserve[!is.na(d$reserve)]))) {           # reserve champions (geographic spread)
    sc <- tapply(d$pts[d$reserve == rsv & !is.na(d$reserve)], d$volunteer[d$reserve == rsv & !is.na(d$reserve)], sum)
    if (length(sc) && max(sc, na.rm = TRUE) > 0) won[[paste0("Champion · ", rsv)]] <- names(which.max(sc))
  }

  # BADGES — thresholds, so many earn them (the participation backbone).
  badges <- list(
    "Stoat Slayer"    = per$volunteer[per$stoats    >= 1],
    "Mustelid Hunter" = per$volunteer[per$mustelids >= 1],
    "All-rounder"     = per$volunteer[per$species   >= 3],
    "Quarter-century" = per$volunteer[per$catches   >= 25],
    "Half-century"    = per$volunteer[per$catches   >= 50],
    "Stalwart"        = per$volunteer[per$checks    >= 40])

  per$trophies <- lapply(per$volunteer, function(v) names(won)[vapply(won, identical, logical(1), v)])
  per$badges   <- lapply(per$volunteer, function(v) names(badges)[vapply(badges, function(vs) v %in% vs, logical(1))])
  awarded <- lengths(per$trophies) + lengths(per$badges) > 0

  label <- { sc <- ik_season_calendar(ik_data)
    l <- sc$calendar_season[sc$season == season & sc$season_year == season_year]
    if (length(l)) l[1] else paste(season, season_year) }
  list(per = per, label = label, season = season, season_year = season_year,
       n_active = nrow(per), n_awarded = sum(awarded), weights = w_used,
       trophy_winners = won, badge_groups = badges)
}
