# observation_viewer.R — the observation record viewer: a tabbed modal opened from a record
# (Records row click; reusable elsewhere). For camera observations WITH public media it leads
# with the image burst (the wow), then a comprehensive Details tab; traps and media-less
# observations show Details only. Generic over observation type — the Details sections adapt.
# Images render immediately from cache-or-Agouti and a background cache job warms the rest.

#' Format a scalar for the viewer (datetime → friendly local string; "—" for empty). @keywords internal
.ovw_val <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_character_)
  x <- x[1]
  if (inherits(x, "POSIXct")) {
    return(if (format(x, "%H:%M:%S") == "00:00:00") format(x, "%d %b %Y")
           else format(x, "%d %b %Y · %H:%M:%S"))
  }
  as.character(x)
}

#' A key/value row, dropped entirely when the value is empty/NA. @keywords internal
.ovw_row <- function(label, value) {
  v <- .ovw_val(value)
  if (is.na(v) || !nzchar(v)) return(NULL)
  tags$div(class = "ovw-row", tags$span(class = "ovw-k", label), tags$span(class = "ovw-v", v))
}

#' A titled section, dropped when it has no rows. @keywords internal
.ovw_section <- function(title, ...) {
  rows <- Filter(Negate(is.null), list(...))
  if (!length(rows)) return(NULL)
  tags$div(class = "ovw-section", tags$h6(title), rows)
}

#' The species display label, or NA when the observation has no species (unclassified /
#' blank). NA-aware (ik_species_label returns NA for NA input, which `%||%` won't catch).
#' @keywords internal
.ovw_species <- function(ik_data, ob, prefer) {
  s <- ik_species_label(ob$scientificName, ik_data, prefer)
  if (length(s) && !is.na(s) && nzchar(s)) return(s)
  if (length(ob$scientificName) && !is.na(ob$scientificName) && nzchar(ob$scientificName))
    return(ob$scientificName)
  NA_character_
}

#' A minutes value as a readable duration, coarsening with magnitude (the gaps here run from
#' minutes to years, so raw "139648 min" is unreadable). Two units max. @keywords internal
.ovw_duration <- function(mins) {
  if (length(mins) == 0 || is.na(mins)) return(NA_character_)
  days <- function(n) sprintf("%d day%s", n, if (n == 1) "" else "s")
  m <- round(mins)
  if (m < 60)        return(sprintf("%d min", m))
  h <- m %/% 60;  rm <- m %% 60
  if (h < 24)        return(if (rm) sprintf("%d hr %d min", h, rm) else sprintf("%d hr", h))
  d <- h %/% 24;  rh <- h %% 24
  if (d < 365)       return(if (rh) sprintf("%s %d hr", days(d), rh) else days(d))
  y <- d %/% 365; rd <- d %% 365
  if (rd) sprintf("%d yr %s", y, days(rd)) else sprintf("%d yr", y)
}

#' The subject heading: species label, else "Camera setup" (an unclassified image whose
#' `cameraSetupType` is "setup" — how Camtrap DP marks setup/calibration shots), else
#' "Unclassified". @keywords internal
.ovw_subject <- function(ik_data, ob, prefer) {
  sp <- .ovw_species(ik_data, ob, prefer)
  if (!is.na(sp)) return(sp)
  if (isTRUE(ob$cameraSetupType == "setup")) return("Camera setup")
  "Unclassified"
}

#' Classification confidence/validation, read by method. Human probabilities encode
#' VALIDATION (1 = validated, <1 = not separately validated), not confidence; machine
#' probabilities are real confidence. @keywords internal
.ovw_classprob <- function(method, prob) {
  if (length(prob) == 0 || is.na(prob)) return(NA_character_)
  if (isTRUE(method == "human")) return(if (prob >= 1) "Validated" else "Not separately validated")
  sprintf("%.0f%% confidence", 100 * prob)
}

#' Pull one `key:value` out of a "k:v | k:v" tag string ("NA"/empty → NA). @keywords internal
.ovw_tag <- function(tags, key) {
  if (length(tags) == 0 || is.na(tags)) return(NA_character_)
  parts <- trimws(strsplit(tags, "\\|")[[1]])
  hit   <- parts[startsWith(parts, paste0(key, ":"))]
  if (!length(hit)) return(NA_character_)
  v <- trimws(sub(paste0("^", key, ":"), "", hit[1]))
  if (!nzchar(v) || v == "NA") NA_character_ else v
}

#' "species · <duration>" for a co-occurrence cell, or NA when none. The species name
#' follows the Settings preference (relations store scientificName; map to the label, fall
#' back to scientific if unmapped). One cheap lookup per row. @keywords internal
.ovw_cooc <- function(ik_data, prefer, species, mins) {
  if (length(species) == 0 || is.na(species)) return(NA_character_)
  lbl <- ik_species_label(species, ik_data, prefer)
  if (length(lbl) == 0 || is.na(lbl) || !nzchar(lbl)) lbl <- species
  d <- .ovw_duration(mins)
  sprintf("%s%s", lbl, if (!is.na(d)) sprintf(" · %s", d) else "")
}

# Role display label + accent colour for a record's species (matches the Spatial explorer palette).
.OVW_ROLE_LABEL <- c(predator = "Predator", protected = "Protected", other = "Other")
.OVW_ROLE_COL   <- c(predator = "#d62728", protected = "#2e9e3f", other = "#8a8a8a")

#' The record's monitoring season (calendar_season, e.g. "Autumn 2026"), or NA. @keywords internal
.ovw_season <- function(ik_data, ob) {
  po <- ik_data$app$period$observations
  if (is.null(po) || !length(ob$observationID)) return(NA_character_)
  po$calendar_season[match(ob$observationID, po$observationID)]
}

#' The species' row in the project species-groups registry (group label, role, monitor/control intent,
#' sentiment) — the "about this species" context. NULL when the record has no resolvable species
#' (blank / unclassified). @keywords internal
.ovw_species_meta <- function(ik_data, ob) {
  sn <- ob$scientificName
  if (length(sn) == 0 || is.na(sn) || !nzchar(sn)) return(NULL)
  sg <- ik_species_groups(ik_data)
  if (is.null(sg) || !nrow(sg)) return(NULL)
  g <- sg[match(sn, sg$scientificName), , drop = FALSE]
  if (!nrow(g) || is.na(g$group)) return(NULL)
  g
}

#' The record species' role (predator / protected / other), or NA. @keywords internal
.ovw_role <- function(ik_data, ob) {
  m <- .ovw_species_meta(ik_data, ob)
  if (is.null(m)) return(NA_character_)
  m$role[1]
}

#' The de-duplication window applied to this species (project `duplicate_window`, per-species override
#' over the default) as a readable "N min", or NA. Minute-resolution only — meaningless for date-only
#' trap data, so callers pass it for camera records only. @keywords internal
.ovw_dupwindow <- function(ik_data, ob) {
  sn <- ob$scientificName
  if (length(sn) == 0 || is.na(sn)) return(NA_character_)
  dw <- ik_data$meta$duplicate_window %||% list(default = 5, by_species = list())  # canonical path (species Summary)
  w <- dw$by_species[[sn]]; if (is.null(w)) w <- dw$default
  if (is.null(w) || length(w) != 1 || is.na(w) || !is.finite(w)) return(NA_character_)
  sprintf("%g min", w)
}

#' The deployment window as "start – end" (dates), or a single date / NA. @keywords internal
.ovw_deploy_window <- function(ob) {
  fmt <- function(x) if (length(x) && !is.na(x)) format(x, "%d %b %Y") else NA_character_
  s <- fmt(ob$deploymentStart); e <- fmt(ob$deploymentEnd)
  if (is.na(s) && is.na(e)) return(NA_character_)
  if (is.na(e) || identical(s, e)) return(s)
  if (is.na(s)) return(e)
  paste(s, "–", e)
}

#' One labelled fact for the Summary grid, dropped when empty. @keywords internal
.ovw_fact <- function(label, value) {
  v <- .ovw_val(value)
  if (is.na(v) || !nzchar(v)) return(NULL)
  tags$div(class = "ovw-fact", tags$span(class = "ovw-fact-k", label), tags$span(class = "ovw-fact-v", v))
}

#' The "About <group>" section for the Summary tab — the record species' species-group meta. @keywords internal
.ovw_about <- function(ik_data, ob, meta, is_cam) {
  if (is.null(meta)) return(NULL)
  nz   <- function(x) if (length(x) && !is.na(x) && nzchar(x)) x else ""
  mon  <- switch(nz(meta$monitor), target = "Core target", interesting = "Of interest", NA_character_)
  ctrl <- if (isTRUE(meta$control == "target")) "Control target" else NA_character_
  # the project's coarse sentiment tag, shown verbatim (not dressed up as a specific "concern" —
  # a one-word tag can't carry a species' real ecological impact). Surfaced ONLY for "other"
  # species: for predators (always a pest) / protected (always a taonga) the Role already says it.
  sent <- if (identical(meta$role, "other"))
            switch(nz(meta$sentiment), bad = "Pest", good = "Beneficial", neutral = "Neutral", NA_character_)
          else NA_character_
  .ovw_section(sprintf("About %s", meta$label),
    .ovw_row("Role", unname(.OVW_ROLE_LABEL[meta$role]) %||% tools::toTitleCase(meta$role)),
    .ovw_row("Monitoring", mon),
    .ovw_row("Control", ctrl),
    .ovw_row("Sentiment", sent),
    .ovw_row("Duplicate window", if (is_cam) .ovw_dupwindow(ik_data, ob) else NA_character_))
}

#' Build the Summary tab: a headline (role badge + species + count), a compact key-facts grid, and the
#' "about this species" meta — the at-a-glance card that keeps the long Details tab for the deep dive.
#' @keywords internal
.ovw_summary <- function(ik_data, ob, prefer) {
  is_cam  <- identical(ob$source_type, "camera")
  subject <- .ovw_subject(ik_data, ob, prefer)
  role    <- .ovw_role(ik_data, ob)
  count   <- if (length(ob$count) && !is.na(ob$count) && ob$count > 1) ob$count else NA
  badge   <- if (!is.na(role)) tags$span(class = "ovw-role-badge",
    style = sprintf("background:%s", unname(.OVW_ROLE_COL[role]) %||% "#8a8a8a"),
    unname(.OVW_ROLE_LABEL[role]) %||% tools::toTitleCase(role)) else NULL
  head <- tags$div(class = "ovw-sum-head", badge,
    tags$div(class = "ovw-sum-sp", subject,
      if (!is.na(count)) tags$span(class = "ovw-sum-count", sprintf("×%s", count))))
  # the at-a-glance context that used to live on Details: a trap's outcome/status; a camera's
  # diel + duplicate context (the deeper before/after relations stay on Details).
  status <- if (!is_cam) { st <- .ovw_tag(ob$observationTags, "status")
              if (!is.na(st) && tolower(st) != "caught") tools::toTitleCase(st) else NA } else NA
  diel   <- if (is_cam) as.character(ik_diel_period(ik_data, ob$eventStart, ob$reserve)) else NA
  dn     <- if (is_cam) ik_day_night(ik_data, ob$eventStart, ob$reserve) else NA
  r      <- if (is_cam) { rel <- ik_relations(ik_data); rel[match(ob$observationID, rel$observationID), ] } else NULL
  posdup <- if (is_cam && length(r$possible_duplicate) && !is.na(r$possible_duplicate))
              (if (isTRUE(r$possible_duplicate)) "Yes" else "No") else NA
  facts <- Filter(Negate(is.null), list(
    .ovw_fact(if (is_cam) "Detected" else "Checked", if (is_cam) ob$eventStart else ob$eventEnd),
    .ovw_fact("Season",   .ovw_season(ik_data, ob)),
    .ovw_fact("Reserve",  ob$reserve),
    .ovw_fact("Line",     ob$line),
    .ovw_fact("Location", ob$locationName),
    .ovw_fact("Device",   tools::toTitleCase(ob$source_type %||% "")),
    if (!is_cam) .ovw_fact("Outcome", ik_obs_type_label(ob$observationType, TRUE)) else NULL,
    if (!is_cam) .ovw_fact("Status", status) else NULL,
    if (is_cam) .ovw_fact("Diel period", diel) else NULL,
    if (is_cam) .ovw_fact("Day / night", dn) else NULL,
    if (is_cam) .ovw_fact("Since prev (same sp.)", .ovw_duration(r$minutes_since_prev_same_species)) else NULL,
    if (is_cam) .ovw_fact("Possible duplicate", posdup) else NULL))
  tags$div(class = "ovw-summary",
    head,
    tags$div(class = "ovw-sum-grid", facts),
    .ovw_about(ik_data, ob, .ovw_species_meta(ik_data, ob), is_cam))
}

#' Build the Map tab: the record's coordinates (moved here from Details) above a small leaflet showing
#' the point within its reserve boundary. Returns the coords + an empty note when there's no location.
#' The map re-sizes when the tab is shown via the global maps.js resize-on-tab-show handler. @keywords internal
.ovw_map <- function(ik_data, ob) {
  lat <- ob$latitude; lng <- ob$longitude
  has_xy <- length(lat) && !is.na(lat) && length(lng) && !is.na(lng)
  coords <- if (has_xy) sprintf("%.5f, %.5f", lat, lng) else "Not recorded"
  header <- tags$div(class = "ovw-map-head",
    tags$div(class = "ovw-row",
      tags$span(class = "ovw-k", "Coordinates"), tags$span(class = "ovw-v", coords)),
    if (has_xy) tags$a(class = "ovw-orig", target = "_blank", rel = "noopener",
      href = sprintf("https://www.google.com/maps/search/?api=1&query=%.6f,%.6f", lat, lng),
      "Open in Google Maps ↗"))
  if (!has_xy) return(tags$div(class = "ovw-map", header,
    tags$p(class = "ovw-empty", "This record has no coordinates to map.")))
  role <- .ovw_role(ik_data, ob)
  col  <- unname(.OVW_ROLE_COL[role %||% "other"]); if (is.null(col) || is.na(col)) col <- "#1f78b4"
  hull <- tryCatch(ik_reserve_boundary(ik_data, ob$reserve), error = function(e) NULL)
  has_hull <- !is.null(hull) && nrow(hull) > 0
  m <- leaflet::leaflet(width = "100%", height = 380,
         options = leaflet::leafletOptions(attributionControl = FALSE))
  m <- leaflet::addProviderTiles(m, leaflet::providers$CartoDB.Positron)
  if (has_hull) {
    m <- ik_add_reserve_boundary(m, hull, color = "#6c757d", pane = NULL)
    # a PERMANENT (no-hover) reserve name, centred on the reserve, so the context reads at a glance
    bb  <- tryCatch(sf::st_bbox(hull), error = function(e) NULL)
    rnm <- if ("reserve" %in% names(hull)) hull$reserve[1] else ob$reserve
    if (!is.null(bb) && length(rnm) && !is.na(rnm) && nzchar(rnm))
      m <- leaflet::addLabelOnlyMarkers(m, lng = mean(c(bb[["xmin"]], bb[["xmax"]])),
        lat = mean(c(bb[["ymin"]], bb[["ymax"]])), label = as.character(rnm),
        labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "center",
          textOnly = TRUE, className = "ovw-reserve-label"))
  }
  m <- leaflet::addCircleMarkers(m, lng = lng, lat = lat, radius = 9, weight = 2,
    color = "#ffffff", opacity = 1, fillColor = col, fillOpacity = 1)
  # With a boundary, leave the view UNSET so maps.js (shown.bs.tab) fits to boundary + point — the
  # whole reserve in context. Without one, a lone point would zoom to max, so frame it zoomed out.
  if (!has_hull) m <- leaflet::setView(m, lng = lng, lat = lat, zoom = 13)
  tags$div(class = "ovw-map", header, tags$div(class = "ovw-map-canvas", m))
}

#' Build the Details tab content from an enriched observation row. Species, count, when & where and
#' the at-a-glance context now lead on the Summary tab — this tab keeps the deeper, less-scanned
#' detail: biology, the before/after neighbours, and the full trap log. @keywords internal
.ovw_details <- function(ik_data, ob, prefer) {
  is_cam  <- identical(ob$source_type, "camera")
  rel     <- ik_relations(ik_data)
  r       <- rel[match(ob$observationID, rel$observationID), ]

  tags$div(
    class = "ovw-details",
    .ovw_section("Identity",
      .ovw_row("Type", ik_obs_type_label(ob$observationType, !is_cam)),
      .ovw_row("Camera setup", if (isTRUE(ob$cameraSetupType == "setup")) "Yes" else NA),
      .ovw_row("Life stage", ob$lifeStage),
      .ovw_row("Sex", ob$sex),
      .ovw_row("Behaviour", ob$behavior),
      .ovw_row("Individual", ob$individualID)),
    if (is_cam) .ovw_section("Nearby activity",
      .ovw_row("Nearest animal before", .ovw_cooc(ik_data, prefer, r$animal_before_species, r$animal_before_min)),
      .ovw_row("Nearest animal after",  .ovw_cooc(ik_data, prefer, r$animal_after_species,  r$animal_after_min)),
      .ovw_row("Nearest predator before", .ovw_cooc(ik_data, prefer, r$predator_before_species,  r$predator_before_min)),
      .ovw_row("Nearest predator after",  .ovw_cooc(ik_data, prefer, r$predator_after_species,   r$predator_after_min)),
      .ovw_row("Nearest protected before", .ovw_cooc(ik_data, prefer, r$protected_before_species, r$protected_before_min)),
      .ovw_row("Nearest protected after",  .ovw_cooc(ik_data, prefer, r$protected_after_species,  r$protected_after_min))),
    if (!is_cam) .ovw_section("Trapping",
      .ovw_row("Volunteer", .ovw_tag(ob$observationTags, "volunteer")),
      .ovw_row("Trap type", ob$cameraModel),
      .ovw_row("Bait (this check)", .ovw_tag(ob$observationTags, "bait")),
      # the raw trap.NZ "rebaited" flag (volunteer's "did you re-bait?"), NOT a computed change —
      # so "Rebaited: Yes" with "Bait (prior check): None" just means the trap had no bait before.
      .ovw_row("Rebaited", .ovw_tag(ob$observationTags, "bait_change")),
      .ovw_row("Prior check", r$prior_check_date),
      # the bait actually in the trap during the just-ended interval — i.e. what caught anything
      # found here. Lagged one check at the location (see observation_relations.R).
      .ovw_row("Bait (prior check)", r$prior_baitstatus),
      .ovw_row("Interval since prior check",   # whole days — checks are date-only (DST drift otherwise)
               if (length(r$days_since_prior_check) && !is.na(r$days_since_prior_check))
                 .ovw_duration(round(r$days_since_prior_check) * 1440) else NA))
  )
}

#' The taxonomy-authority link row for Provenance: the record's `taxon.taxonID` is a full URL to the
#' resolved species (GBIF or Catalogue of Life), shown as "<Rank> · <source> ↗". Dropped when the
#' record carries no taxonID. Built directly (not via `.ovw_row`) so the link renders. @keywords internal
.ovw_taxon_row <- function(ob) {
  id <- ob$`taxon.taxonID`
  if (length(id) == 0 || is.na(id) || !nzchar(id)) return(NULL)
  rank <- ob$`taxon.taxonRank`
  rk   <- if (length(rank) && !is.na(rank) && nzchar(rank)) tools::toTitleCase(rank) else "Reference"
  src  <- if (grepl("gbif", id, ignore.case = TRUE)) "GBIF"
          else if (grepl("checklistbank|catalogueoflife|col2", id, ignore.case = TRUE)) "Catalogue of Life"
          else sub("^https?://(www\\.)?([^/]+).*$", "\\2", id)
  tags$div(class = "ovw-row",
    tags$span(class = "ovw-k", "Taxon"),
    tags$span(class = "ovw-v",
      tags$a(href = id, target = "_blank", rel = "noopener", sprintf("%s · %s ↗", rk, src))))
}

#' The Provenance tab: how this record came to be (classification lineage + taxonomy authority), the
#' deployment it belongs to, where it lives (dataset + ids + comments), and — for out-of-reserve
#' records — its data-quality context. Split off the Details tab to keep it short. @keywords internal
.ovw_provenance <- function(ik_data, ob, prefer) {
  is_cam <- identical(ob$source_type, "camera")
  tags$div(
    class = "ovw-details",
    .ovw_section("Classification",
      .ovw_row("Method", ob$classificationMethod),
      .ovw_row("Classified by", ob$classifiedBy),
      .ovw_row(if (isTRUE(ob$classificationMethod == "human")) "Validation" else "Confidence",
               .ovw_classprob(ob$classificationMethod, ob$classificationProbability)),
      .ovw_taxon_row(ob)),
    .ovw_section("Deployment",
      .ovw_row("Deployed", .ovw_deploy_window(ob)),               # the device's start–end window
      .ovw_row(if (is_cam) "Camera ID" else "Trap ID", ob$cameraID),
      .ovw_row("Deployment notes", ob$deploymentComments)),       # e.g. "Converted from trap.NZ"
    # Only for records OUTSIDE a monitored reserve — how far the (usually trap) point sits from the
    # surveyed network, so an out-of-area catch isn't mistaken for in-reserve monitoring.
    if (isFALSE(ob$within_monitored_area)) .ovw_section("Location quality",
      .ovw_row("Within monitored area", "No"),
      .ovw_row("Nearest monitoring site", ob$nearest_monitoring_location),
      .ovw_row("Distance to monitoring",
               if (length(ob$nearest_monitoring_distance_km) && !is.na(ob$nearest_monitoring_distance_km))
                 sprintf("%.2f km", ob$nearest_monitoring_distance_km) else NA)),
    .ovw_section("Record",
      .ovw_row("Dataset", ob$dataset),
      .ovw_row("Observation ID", ob$observationID),
      .ovw_row("Event ID", ob$eventID),
      .ovw_row("Deployment ID", ob$deploymentID),
      .ovw_row("Comments", ob$observationComments))
  )
}

#' Build the Photos tab: the image burst, each with its capture time + a link to the original.
#' Clicking a thumbnail opens the full-screen sequence viewer (ovw-lightbox.js) — `data-cap`
#' carries the timestamp it shows there. @keywords internal
.ovw_media <- function(mv) {
  tags$div(
    class = "ovw-media",
    lapply(seq_len(nrow(mv)), function(i) {
      tags$figure(
        class = "ovw-figure",
        tags$img(class = "ovw-img", src = mv$src[i], loading = "lazy", alt = "",
                 `data-cap` = .ovw_val(mv$timestamp[i]), title = "Click to view the full sequence"),
        tags$figcaption(
          class = "ovw-cap",
          tags$span(.ovw_val(mv$timestamp[i])),
          tags$a(class = "ovw-orig", href = mv$original[i], target = "_blank",
                 rel = "noopener", "View original ↗")
        )
      )
    })
  )
}

#' The observation viewer TITLE block (species subject + reserve · line · datetime). Reused by
#' the standalone modal (in the header) and the inline drill Record views (above the tabs).
#' @keywords internal
.ovw_title <- function(ik_data, ob, prefer) {
  is_cam <- identical(ob$source_type, "camera")
  tags$div(
    class = "ovw-title",
    tags$div(class = "ovw-title-sp", .ovw_subject(ik_data, ob, prefer)),
    tags$div(class = "ovw-title-sub", paste(stats::na.omit(c(
      ob$reserve,
      if (length(ob$line) && !is.na(ob$line)) paste("Line", ob$line) else NA_character_,
      .ovw_val(if (is_cam) ob$eventStart else ob$eventEnd))), collapse = " · "))
  )
}

#' The observation viewer BODY: a tabset — Photos (camera w/ public media only) · Summary · Map
#' (only when the record has coordinates) · Details · Provenance — reused by the standalone modal AND
#' inline (the drill Record tabs), so every record viewer looks the same. Photos lead for cameras
#' (the wow); Summary leads for traps / media-less records. Warms the media cache as a side effect.
#'
#' @param ik_data   The ik_data container.
#' @param ob        An enriched observation row (`ik_observation`).
#' @param prefer    "scientific" | "vernacular".
#' @param tabset_id Optional id for the tabset (give a unique/namespaced one when embedding
#'   inline so it doesn't collide with another viewer in the page).
#' @return A `tabsetPanel`.
#' @keywords internal
.ovw_tabs <- function(ik_data, ob, prefer, tabset_id = NULL) {
  is_cam  <- identical(ob$source_type, "camera")
  mv      <- if (is_cam) ik_event_media_view(ik_data, ob$eventID) else NULL
  has_med <- !is.null(mv) && nrow(mv) > 0
  has_xy  <- length(ob$latitude) && !is.na(ob$latitude) && length(ob$longitude) && !is.na(ob$longitude)
  tabs <- Filter(Negate(is.null), list(
    if (has_med) tabPanel("Photos", icon = icon("image"), .ovw_media(mv)),
    tabPanel("Summary",  icon = icon("clipboard"),        .ovw_summary(ik_data, ob, prefer)),
    if (has_xy) tabPanel("Map", icon = icon("location-dot"), .ovw_map(ik_data, ob)),
    tabPanel("Details",    icon = icon("circle-info"),    .ovw_details(ik_data, ob, prefer)),
    tabPanel("Provenance", icon = icon("clipboard-list"), .ovw_provenance(ik_data, ob, prefer))))
  body <- do.call(tabsetPanel, c(if (!is.null(tabset_id)) list(id = tabset_id), tabs))
  if (has_med) ik_cache_event_async(ik_data, ob$eventID)   # warm rest of burst, never blocks
  body
}

#' Open the observation viewer modal for one observation.
#'
#' @param ik_data          The ik_data container.
#' @param observation_id   The observationID to view.
#' @param prefer_scientific TRUE to label species scientifically (default FALSE).
#' @return Invisibly NULL (side effect: shows a modal + may launch a background cache job).
show_observation_modal <- function(ik_data, observation_id, prefer_scientific = FALSE) {
  ob <- ik_observation(ik_data, observation_id)
  if (is.null(ob)) {
    showModal(modalDialog("Observation not found.", easyClose = TRUE, footer = modalButton("Close")))
    return(invisible())
  }
  prefer <- if (isTRUE(prefer_scientific)) "scientific" else "vernacular"
  showModal(modalDialog(
    title = .ovw_title(ik_data, ob, prefer),
    .ovw_tabs(ik_data, ob, prefer, tabset_id = "ovw_tabs"),
    size = "l", easyClose = TRUE, footer = modalButton("Close")))
  invisible()
}
