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

#' Build the Details tab content from an enriched observation row. @keywords internal
.ovw_details <- function(ik_data, ob, prefer) {
  is_cam  <- identical(ob$source_type, "camera")
  species <- .ovw_species(ik_data, ob, prefer)
  rel     <- ik_relations(ik_data)
  r       <- rel[match(ob$observationID, rel$observationID), ]
  diel    <- if (is_cam) as.character(ik_diel_period(ik_data, ob$eventStart, ob$reserve)) else NA
  dn      <- if (is_cam) ik_day_night(ik_data, ob$eventStart, ob$reserve) else NA

  tags$div(
    class = "ovw-details",
    .ovw_section("Identity",
      .ovw_row("Species", species),
      .ovw_row("Count", ob$count),
      .ovw_row("Type", ob$observationType),
      .ovw_row("Camera setup", if (isTRUE(ob$cameraSetupType == "setup")) "Yes" else NA),
      .ovw_row("Life stage", ob$lifeStage),
      .ovw_row("Sex", ob$sex),
      .ovw_row("Behaviour", ob$behavior),
      .ovw_row("Individual", ob$individualID)),
    .ovw_section("When & where",
      .ovw_row(if (is_cam) "Detected" else "Checked", if (is_cam) ob$eventStart else ob$eventEnd),
      .ovw_row("Reserve", ob$reserve),
      .ovw_row("Line", ob$line),
      .ovw_row("Location", ob$locationName),
      .ovw_row("Device", tools::toTitleCase(ob$source_type %||% "")),
      .ovw_row("Coordinates",
               if (length(ob$latitude) && !is.na(ob$latitude))
                 sprintf("%.5f, %.5f", ob$latitude, ob$longitude) else NA),
      .ovw_row("Deployment", ob$deploymentID)),
    if (is_cam) .ovw_section("Detection context",
      .ovw_row("Diel period", diel),
      .ovw_row("Day / night", dn),
      .ovw_row("Since prev (same sp.)", .ovw_duration(r$minutes_since_prev_same_species)),
      .ovw_row("Possible duplicate",
               if (length(r$possible_duplicate) && !is.na(r$possible_duplicate))
                 (if (isTRUE(r$possible_duplicate)) "Yes" else "No") else NA),
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

#' The Provenance tab: how this record came to be (classification lineage) and where it lives
#' (dataset + ids + comments). Split off the Details tab to keep it short. @keywords internal
.ovw_provenance <- function(ik_data, ob, prefer) {
  tags$div(
    class = "ovw-details",
    .ovw_section("Classification",
      .ovw_row("Method", ob$classificationMethod),
      .ovw_row("Classified by", ob$classifiedBy),
      .ovw_row(if (isTRUE(ob$classificationMethod == "human")) "Validation" else "Confidence",
               .ovw_classprob(ob$classificationMethod, ob$classificationProbability))),
    .ovw_section("Record",
      .ovw_row("Dataset", ob$dataset),
      .ovw_row("Observation ID", ob$observationID),
      .ovw_row("Event ID", ob$eventID),
      .ovw_row("Comments", ob$observationComments))
  )
}

#' Build the Photos tab: the image burst, each with its capture time + a link to the original.
#' @keywords internal
.ovw_media <- function(mv) {
  tags$div(
    class = "ovw-media",
    lapply(seq_len(nrow(mv)), function(i) {
      tags$figure(
        class = "ovw-figure",
        tags$img(class = "ovw-img", src = mv$src[i], loading = "lazy", alt = ""),
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

#' The observation viewer BODY: a tabset — Photos (camera w/ public media only) · Details ·
#' Provenance — reused by the standalone modal AND inline (the drill Record tabs), so every
#' record viewer looks the same. Warms the media cache as a side effect (no-op if cached).
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
  tabs <- Filter(Negate(is.null), list(
    if (has_med) tabPanel("Photos", icon = icon("image"), .ovw_media(mv)),
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
