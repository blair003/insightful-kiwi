# overview.R — the Overview page. A project header for the current selection, then a
# section PER DEVICE: camera monitoring (detections · camera-hours) and trapping
# (catches · trap-nights) — because a camera detection and a trap kill are different
# things, use different effort units, and care about different species. Driven by a
# selection SPEC (period/geography), resolved per device with ik_resolve().
# First increment — RAI/metrics come later.

#' "How to read this" help body for the Overview MONITORING (camera) section — a tabbed walkthrough
#' of the RAI headline. `norm_hours` is the camera-hours RAI is normalised to, from project config,
#' woven in so the modal matches the actual metric. @keywords internal
overview_monitor_help_body <- function(norm_hours = 2000) {
  P <- function(...) tags$p(...)
  nh <- format(norm_hours, big.mark = ",")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "The ", tags$b("Monitoring"), " section summarises what your ", tags$b("cameras"),
        " recorded in the current selection: top boxes for ", tags$b("effort"), " (camera-hours), ",
        "deployments, animal detections and distinct species; then a ", tags$b("card per species"),
        " whose headline number is its ", tags$b("RAI"), " (relative activity index)."),
      P("Click a card → its per-reserve breakdown → a reserve → the actual detection records. The ",
        tags$b("Compare to"), " control adds a prior-period (or same-period-last-year) value and a ",
        tags$b("coloured arrow"), " — green where the change is desirable (predators down, protected up), ",
        "red where it isn't.")),
    tabPanel(
      "Relative activity (RAI)", icon = icon("chart-simple"),
      P(tags$br(), "A camera that runs twice as long will record about twice as many detections — so a ",
        tags$b("raw count"), " mostly measures how long the cameras were out, not how much animal activity ",
        "there was. RAI removes that by dividing by ", tags$b("effort"), " and scaling to a fixed window:"),
      P(tags$b(sprintf("RAI = detections per %s camera-hours.", nh))),
      P("So RAI is a ", tags$b("rate"), ", comparable across lines and periods regardless of how long each ",
        "camera ran. Higher = more activity of that species on camera. It's an ", tags$em("index of activity"),
        ", not a population count — good for trends and comparisons, not an absolute density."),
      P("The ", tags$b("card"), " shows the network RAI (mean over reserves) ± its standard error; a tiny SE ",
        "means the reserves agree, a large one that they differ.")),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Per camera-line"), " — detections of the species ÷ that line's actual camera-hours, ",
                "× ", nh, " (the ", tags$b("norm_hours"), " from project config, ≈ one line's full deployment)."),
        tags$li(tags$b("Net of duplicates"), " — a detection flagged as a likely repeat of the same animal ",
                "within the duplicate window is excluded, so one curious animal lingering doesn't inflate RAI."),
        tags$li(tags$b("Rolled up"), " — line RAIs average to a reserve, and reserves to the network, each as a ",
                tags$b("mean ± SE"), " (so a busy line doesn't dominate by its size)."),
        tags$li(tags$b("Effort"), " — for the standard 4-camera-line protocol the window is exact (a pulse of ",
                "deployments); otherwise it's summed camera-hours within the selection.")),
      P(tags$em("The norm is a scale only — RAI always divides by each camera's ACTUAL hours, so changing it ",
                "rescales every number equally and never changes which species or line is higher.")))
  )
}

#' "How to read this" help body for the Overview TRAPPING (control) section — the catch-rate
#' headline. `norm` = trap-nights the rate is normalised to (project config). @keywords internal
overview_trap_help_body <- function(norm = 100) {
  P <- function(...) tags$p(...)
  ntn <- paste0(format(norm, big.mark = ","), " trap-nights")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "The ", tags$b("Trapping"), " section summarises what your ", tags$b("traps"), " caught in ",
        "the current selection: top boxes for ", tags$b("effort"), " (trap-nights), checks, catches and distinct ",
        "species; then a ", tags$b("card per species caught"), " — headline is the ", tags$b("catch count"),
        ", with the ", tags$b("catch rate"), " (per ", ntn, ") beneath."),
      P("Trapping defaults to ", tags$b("show everything caught"), " — every species with a catch gets a card, ",
        "including incidental bycatch (hide one by setting ", tags$b("control = \"hide\""), " on its group in ",
        "project.R). Click a card → per-reserve breakdown → a reserve → the catch records.")),
    tabPanel(
      "Catch rate", icon = icon("chart-simple"),
      P(tags$br(), "The headline is the raw ", tags$b("count"), " — catches are sparse, so the honest first ",
        "number is simply how many were caught. But more checks over more traps for longer will catch more, so ",
        "to compare fairly the card also shows a ", tags$b("rate"), ":"),
      P(tags$b(sprintf("Catch rate = catches per %s.", ntn))),
      P("This is catch-per-unit-effort: it divides out how much trapping was done, so a small intensively-worked ",
        "area and a large lightly-worked one can be compared. With a comparison period set, the arrow is left ",
        tags$b("neutral"), " for trapping (every target is a pest, so there's no “good direction” to colour).")),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Catch"), " = an animal observation at a trap check of the carded species (a group like ",
                "“Mustelids” pools its members; an unconfigured species is counted on its own)."),
        tags$li(tags$b("Trap-night"), " = one trap, set, for one night — summed over the checks in the selection ",
                "(a check’s interval is its trap-nights of effort)."),
        tags$li(tags$b("Rate"), " = catches ÷ trap-nights × ", paste0(format(norm, big.mark = ","), ".")),
        tags$li(tags$b("Rolled up"), " — computed per trapline, averaged to a reserve and to the network as a ",
                tags$b("mean ± SE"), ", the same as the camera RAI.")),
      P(tags$em("A blank trap check (caught nothing) still counts as effort — that's the point of a rate: it ",
                "rewards catching per night set, not just total nights worked.")))
  )
}

#' BRIEF "key concepts" help for the SLIM main Overview blocks — a single panel (not the multi-tab
#' explainer, which stays on each device's own Overview page where there's room). Covers the one idea
#' a reader needs at a glance + points to the full page. @keywords internal
overview_monitor_help_brief <- function(norm_hours = 2000) {
  tagList(
    tags$p("What your ", tags$b("cameras"), " recorded in the current selection — animal ",
           tags$b("detections"), ", the number of distinct ", tags$b("species"), ", and a card per key species."),
    tags$p("Each card's headline is its ", tags$b("RAI"), " (relative activity index): detections per ",
           format(norm_hours, big.mark = ","), " camera-hours, so it's comparable across lines and periods ",
           "however long each camera ran."),
    tags$p(tags$em("Full effort, every species, by-reserve and drill-downs are on the Monitoring → Overview page.")))
}

#' BRIEF "key concepts" help for the slim main Overview trapping block. @keywords internal
overview_trap_help_brief <- function(norm = 100) {
  tagList(
    tags$p("What your ", tags$b("traps"), " caught in the current selection — ", tags$b("catches"),
           ", the number of distinct ", tags$b("species"), ", and a card per target species."),
    tags$p("Each card's headline is the ", tags$b("catch count"), ", with the ", tags$b("catch rate"),
           " (per ", format(norm, big.mark = ","), " trap-nights) beneath — catch-per-unit-effort, so areas ",
           "worked at different intensities compare fairly."),
    tags$p(tags$em("Full effort, every species caught, by-reserve and drill-downs are on the Trapping → Overview page.")))
}

#' The device (`source_type`) icon — ties the main page's section blocks to their navbar menus
#' (Monitoring = binoculars, Trapping = crosshairs). @keywords internal
.ov_device_icon <- function(kind)
  switch(kind, camera = "binoculars", trap = "location-crosshairs", "layer-group")

#' Overview nav panel UI.
#' @param id Module id.
#' @return A bslib nav_panel for page_navbar().
#' @param sections Which device sections this instance renders ("camera"/"trap"). The per-device
#'   Overview pages (Monitoring / Trapping menus) pass a single device; the main page passes both.
#' @param compact When TRUE, render the slim cross-device HEADLINE (one panel: Detections/Catches +
#'   Species boxes + target-species cards) instead of the full sections — the main Overview page. The
#'   full effort/by-reserve/other-species detail now lives on each device's own Overview page.
#' @param trends Optional content (the outcomes "are we winning?" body) — when given, the page becomes
#'   a tabset: Snapshot (the headline) + Trends (the seasonal graphs). The Trends tab is suspended
#'   until opened, so the heavy outcome-series compute never touches the landing render.
overview_ui <- function(id, sections = c("camera", "trap"), compact = FALSE,
                        label = "Overview", value = "overview", trends = NULL, network = FALSE) {
  ns <- NS(id)
  body <- div(
    class = "ik-overview",
    if (isTRUE(compact)) uiOutput(ns("compact"))
    else tagList(
      if ("camera" %in% sections) uiOutput(ns("camera")),
      if ("trap"   %in% sections) uiOutput(ns("trapping"))
    )
  )
  # Optional "Network density" tab — the structural "is the network even dense enough?" view: the
  # per-reserve density table beside a Cameras / Traps / Boundary map, all-data (period-independent).
  # Links out to the full Insights -> Coverage gap analysis.
  nd <- if (isTRUE(network)) tabPanel(
    "Network density", icon = icon("ruler-combined"),
    tags$p(class = "ik-ov-network-lead",
      "Is the network dense enough to work? Footprint, cameras & traps per km², and trap spacing per ",
      "reserve — structural coverage, independent of any period or season. ",
      tags$a(href = "#", style = "font-weight:600",
        onclick = "Shiny.setInputValue('ik_goto_nav','coverage',{priority:'event'});return false;",
        "Open the full Coverage analysis →")),
    bslib::layout_columns(
      class = "ik-maps-split", col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      div(class = "ik-maps-side", DT::DTOutput(ns("density"))),
      div(class = "ik-maps", leaflet::leafletOutput(ns("network_map"), height = "60vh"))))
  tabs <- list(tabPanel("Snapshot", icon = icon("gauge"), body))
  if (!is.null(trends)) tabs <- c(tabs, list(tabPanel("Trends", icon = icon("chart-line"), trends)))
  if (isTRUE(network))  tabs <- c(tabs, list(nd))
  inner <- if (length(tabs) > 1)
    do.call(tabsetPanel, c(list(id = ns("ov_tabs"), type = "tabs"), tabs))
  else body
  # The page title ("{org} — Monitoring/Trapping") leads; the read-only PERIOD banner sits as a subtitle
  # directly under it (tab-aware — on the Trends tab, a series the period doesn't constrain, it reads
  # "All data"). Datasets live on each device's section header, not here.
  nav_panel(
    label, value = value, icon = icon("gauge"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/overview.css")),
    if (isTRUE(network)) tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),
    if (isTRUE(network)) tags$script(src = .ik_asset("js/maps.js")),
    div(class = "ik-topbar", uiOutput(ns("header"))),
    div(class = "ik-page-period", uiOutput(ns("period_banner"))), inner
  )
}

#' Format a count with thousands separators; `—` for NA/empty. @keywords internal
.ov_num <- function(x) if (length(x) == 0 || is.na(x)) "—" else format(x, big.mark = ",")

#' Change direction at DISPLAY precision — compares values rounded to the same decimals the
#' cell shows, so two figures that render identically (e.g. "0.0" vs "0.0") read as flat, not
#' a spurious ▲/▼ off invisible floating-point difference. @keywords internal
.ov_dir <- function(cur, prv, digits) {
  c <- round(cur, digits); p <- round(prv, digits)
  if (c > p) "up" else if (c < p) "down" else "flat"
}

#' Desirability class for a change arrow — "good" (green) / "bad" (red) / "neutral" (uncoloured).
#' Neutral when we're not colouring (`colour = FALSE`, e.g. trapping), the change is flat, or the
#' species has NO desired direction (`want == "none"`): a species that's neither a pest nor protected
#' (e.g. weka) shouldn't read a fall as "good" or a rise as "bad". @keywords internal
.ov_change_cls <- function(dir, want, colour = TRUE) {
  if (!isTRUE(colour) || dir == "flat" || is.null(want) || identical(want, "none")) "neutral"
  else if (identical(dir, want)) "good" else "bad"
}

#' onclick that fires the species-panel drill: `input$<spp_drill> = {kind, label, sci[]}`.
#' `sci` is the scientificName(s) the item stands for (a group may be several). @keywords internal
.ov_spp_onclick <- function(spp_drill, kind, sci, label) {
  sprintf("Shiny.setInputValue('%s',{kind:'%s',label:%s,sci:%s},{priority:'event'})",
          spp_drill, kind, jsonlite::toJSON(label, auto_unbox = TRUE),
          jsonlite::toJSON(as.character(sci)))
}

#' One COLLAPSIBLE block (HTML <details>, collapsed by default) of label/count items — the "other
#' species" section, kept less prominent than the cards. The summary shows a generic paw + the
#' species count; expanding reveals the items. When `sci_sets` (a list parallel to `labels`, each
#' the item's scientificName vector) and `spp_drill` are given, every item with a known species set
#' is clickable → its records. @keywords internal
.ov_class_section <- function(title, labels, counts, sci_sets = NULL, kind = NULL, spp_drill = NULL) {
  if (!length(labels)) return(NULL)
  drillable <- !is.null(sci_sets) && !is.null(spp_drill)
  grid <- tags$div(class = "ik-spp-grid", lapply(seq_along(labels), function(i) {
    can <- drillable && counts[i] > 0 && length(sci_sets[[i]]) > 0
    cls <- c("ik-spp-item", if (can) "ik-spp-click")
    tags$div(
      class   = paste(cls, collapse = " "),
      title   = if (can) "Show these records",
      onclick = if (can) .ov_spp_onclick(spp_drill, kind, sci_sets[[i]], labels[i]),
      tags$span(class = "ik-spp-label", labels[i]),
      tags$span(class = "ik-spp-count", .ov_num(counts[i]))
    )
  }))
  tags$details(
    class = "ik-spp-class",
    tags$summary(class = "ik-spp-summary",
      ik_species_icon("other", "ik-spp-summary-icon"),
      sprintf("%s · %s species", title, .ov_num(length(labels)))),
    grid)
}

#' The "other species" section beneath a device's cards: every species detected/caught that ISN'T
#' already a card (group not in `card_groups`). `itemise = TRUE` lists each as its own clickable
#' drill-down entry (by registry label or vernacular, count desc); FALSE rolls them into a single
#' "N <noun> across M species" total. @keywords internal
.ov_other_species <- function(obs, sg, ik_data, prefer, card_groups, itemise, noun, title,
                              kind = NULL, spp_drill = NULL) {
  obs$group <- sg$group[match(obs$scientificName, sg$scientificName)]
  obs$label <- sg$label[match(obs$scientificName, sg$scientificName)]
  other <- obs[!(obs$group %in% card_groups), , drop = FALSE]
  if (!nrow(other)) return(NULL)
  if (!isTRUE(itemise)) {                                  # rolled-up total (+ unique-species count)
    n_spp <- length(unique(stats::na.omit(other$scientificName)))
    return(tags$div(class = "ik-spp-other", sprintf(
      "Other species: %s %s across %s species", .ov_num(nrow(other)), noun, .ov_num(n_spp))))
  }
  disp <- other$label; nl <- is.na(disp)                   # itemised: one drill-down entry per species
  disp[nl] <- ik_species_label(other$scientificName[nl], ik_data, prefer)
  disp[is.na(disp)] <- "Unidentified"
  oc      <- sort(table(disp), decreasing = TRUE)
  by_disp <- tapply(other$scientificName, disp, function(x) unique(x[!is.na(x)]))
  .ov_class_section(title, names(oc), as.integer(oc),
    sci_sets = lapply(names(oc), function(d) by_disp[[d]]), kind = kind, spp_drill = spp_drill)
}

#' Monitoring (camera) species panel: target/interesting are CARDS above; here the OTHER detected
#' species (incidental + bycatch). @keywords internal
.ov_panel_monitor <- function(obs, sg, ik_data, prefer, itemise, kind = NULL, spp_drill = NULL) {
  mon_groups <- sg$group[!duplicated(sg$group) & !is.na(sg$monitor) & sg$monitor %in% c("target", "interesting")]
  .ov_other_species(obs, sg, ik_data, prefer, mon_groups, itemise, "detections",
                    "All other detected", kind, spp_drill)
}

#' The trapping Overview card set: EVERY species caught gets a card, so trapping defaults to
#' "show all". Configured groups (by config priority, keeping FULL membership so e.g. "Mustelids"
#' counts stoat/weasel/ferret) come first, then any unconfigured caught species as its own card by
#' display name. A group flagged `control = "hide"` in project.R is the only opt-OUT — dropped from
#' the cards entirely (and not surfaced elsewhere). Static (all-time catches), so the card set is
#' stable across period/reserve filters — a card with no catch in the current selection shows "–".
#' @keywords internal
.ov_control_card_taxa <- function(ik_data) {
  sg  <- ik_species_groups(ik_data)
  dp  <- ik_deployment_period(ik_data)
  trap_deps <- dp$deploymentID[!is.na(dp$source_type) & dp$source_type == "trap"]
  if (!length(trap_deps)) return(list())
  obs <- ik_observations(ik_data, with_location = FALSE)
  caught <- unique(obs$scientificName[obs$deploymentID %in% trap_deps &
              !is.na(obs$observationType) & obs$observationType == "animal" &
              !is.na(obs$scientificName)])
  if (!length(caught)) return(list())
  hide_groups <- unique(sg$group[!is.na(sg$control) & sg$control == "hide"])
  caught_groups <- unique(sg$group[!is.na(sg$scientificName) & sg$scientificName %in% caught &
                                   !(sg$group %in% hide_groups)])
  gm <- sg[sg$group %in% caught_groups, , drop = FALSE]
  gm <- gm[order(gm$priority), , drop = FALSE]
  configured <- split(gm$scientificName, factor(gm$label, levels = unique(gm$label)))
  standalone_sci <- sort(setdiff(caught, sg$scientificName))   # caught but in no configured group
  standalone <- stats::setNames(as.list(standalone_sci),
    vapply(standalone_sci, function(s) ik_species_label(s, ik_data, "vernacular"), character(1)))
  c(configured, standalone)
}

#' Caption suffix for the active comparison mode. @keywords internal
.ov_compare_note <- function(compare) {
  switch(compare %||% "none",
         prior     = " · vs prior period",
         last_year = " · vs same period last year",
         "")
}

#' Fewest decimals in `[lo, hi]` that don't render any of `vals` (non-zero) as 0 — so a tiny
#' RAI/rate isn't shown as 0.00, while ordinary cells stay at the lean lower precision. A true
#' zero is fine at `lo`. @keywords internal
.ov_dp <- function(vals, lo, hi) {
  v <- vals[!is.na(vals)]
  d <- lo
  while (d < hi && any(v != 0 & round(v, d) == 0)) d <- d + 1L
  d
}

#' A metric table: target-group rows × reserve columns, cells "value ± SE". When `prev`
#' (a comparison summary) is given, each cell also shows the comparison value + a coloured
#' ▲/▼ — **green where the change is desirable, red where it isn't**: for a pest/predator
#' down is good, for a protected species (`desirable[[taxon]] == "up"`) up is good. When
#' `drill_id` is set, data cells are clickable (set that Shiny input → drill-down modal).
#' Precision is per-cell adaptive in `[min_digits, digits]` (driven by the metric/prior, not SE):
#' most cells stay at `min_digits`, only a near-zero value bumps up so it isn't shown as 0.
#' @keywords internal
.ov_metric_table <- function(summary, taxa_order, caption, prev = NULL, desirable = NULL,
                             drill_id = NULL, kind = "", colour = TRUE, digits = 1,
                             min_digits = digits) {
  reserves <- unique(summary$reserve)
  reserves <- c(sort(setdiff(reserves, "Combined")),                  # combined (network) last
                if ("Combined" %in% reserves) "Combined")

  body <- function(tx, rs) {
    row <- summary[summary$taxon == tx & summary$reserve == rs, , drop = FALSE]
    if (!nrow(row) || is.na(row$metric)) return(list(content = "–", drill = FALSE))
    pr  <- if (!is.null(prev)) prev[prev$taxon == tx & prev$reserve == rs, , drop = FALSE]
    dp  <- .ov_dp(c(row$metric, if (length(pr) && nrow(pr)) pr$metric), min_digits, digits)
    fmt <- paste0("%.", dp, "f")
    # keep "0.43 ± 0.15" intact (no wrap mid-value)
    main <- tags$span(class = "ik-metric-main",
      if (is.na(row$se)) sprintf(fmt, row$metric) else sprintf(paste0(fmt, " ± ", fmt), row$metric, row$se))
    if (is.null(prev)) return(list(content = main, drill = TRUE))
    if (!nrow(pr) || is.na(pr$metric))
      return(list(content = tagList(main, " ",
        tags$span(class = "ik-metric-cmp ik-metric-prev", "· –")), drill = TRUE))
    dir   <- .ov_dir(row$metric, pr$metric, dp)         # compare at the cell's precision
    want  <- desirable[[tx]] %||% "down"                # up=protected, down=pest, none=neutral
    cls   <- .ov_change_cls(dir, want, colour)
    arrow <- c(up = "▲", down = "▼", flat = "–")[[dir]]
    # arrow + prior stay together ("▲ 0.35" never splits); the cell can wrap only BETWEEN
    # "value ± se" and the comparison group, so cells wrap uniformly.
    content <- tagList(main, " ",
      tags$span(class = "ik-metric-cmp",
        tags$span(class = paste0("ik-arrow ik-arrow-", cls), arrow), " ",
        tags$span(class = "ik-metric-prev", sprintf(fmt, pr$metric))))
    list(content = content, drill = TRUE)
  }

  cell_td <- function(tx, rs) {
    b <- body(tx, rs)
    if (b$drill && !is.null(drill_id)) {
      tags$td(class = "ik-drill", b$content, title = "Show the breakdown",
              onclick = sprintf(
                "Shiny.setInputValue('%s',{taxon:%s,reserve:%s,kind:'%s'},{priority:'event'})",
                drill_id, .ik_jsq(tx), .ik_jsq(rs), kind))
    } else tags$td(b$content)
  }

  tags$div(
    class = "ik-metric",
    tags$div(class = "ik-metric-cap", caption),
    tags$table(
      class = "ik-metric-table",
      tags$thead(tags$tr(tags$th(""), lapply(reserves, function(r) tags$th(r)))),
      tags$tbody(lapply(taxa_order, function(tx) {
        tags$tr(tags$td(class = "ik-metric-label", tx),
                lapply(reserves, function(rs) cell_td(tx, rs)))
      }))
    )
  )
}

#' Total detections/captures per taxon, from a metric `$lines` table (col `individuals`/`captures`).
#' @keywords internal
.ov_counts <- function(lines) {
  if (is.null(lines) || !nrow(lines)) return(NULL)
  col <- intersect(c("individuals", "captures"), names(lines))[1]
  if (is.na(col)) return(NULL)
  tapply(lines[[col]], lines$taxon, sum, na.rm = TRUE)
}

#' Per-species metric CARDS — one card per target/interesting species, the headline view. Camera:
#' RAI ± SE is the HEADLINE, detection count secondary; trap: catch COUNT headline, capture rate
#' secondary (the device emphasis differs). A desirability-coloured ▲/▼ vs the comparison period
#' sits on the canonical metric (RAI / rate). Click a card → its per-reserve breakdown (the same
#' drill as a matrix cell, reserve = "Combined"). `compact = TRUE` renders a denser, smaller card
#' (used for the secondary "of interest" tier). @keywords internal
.ov_metric_cards <- function(summary, prev, counts, taxa_order, kind, desirable, drill_id,
                             headline = c("rai", "count"), rate_unit = "",
                             digits = 1, min_digits = digits, colour = TRUE, compact = FALSE,
                             sg = NULL, sort_by = "value", extra = NULL) {
  headline <- match.arg(headline)
  one <- function(tx) {
    s   <- summary[summary$taxon == tx & summary$reserve == "Combined", , drop = FALSE]
    # No "Combined" row when a SINGLE reserve is in scope (with_network only adds it for >1) — fall
    # back to that reserve's own row, so the card still shows its value and drills into it.
    if (!nrow(s)) s <- summary[summary$taxon == tx, , drop = FALSE]
    rsv <- if (nrow(s)) s$reserve[1] else "Combined"
    cnt <- as.integer(counts[tx] %||% 0L); if (length(cnt) == 0 || is.na(cnt)) cnt <- 0L
    has <- nrow(s) == 1 && !is.na(s$metric); metric_el <- tags$span(class = "ik-card-na", "–"); arrow <- NULL
    if (has) {
      pr <- if (!is.null(prev)) prev[prev$taxon == tx & prev$reserve == rsv, , drop = FALSE] else NULL
      pm <- if (!is.null(pr) && nrow(pr)) pr$metric else NA_real_
      dp <- .ov_dp(c(s$metric, pm), min_digits, digits); fmt <- paste0("%.", dp, "f")
      val <- if (is.na(s$se)) sprintf(fmt, s$metric) else sprintf(paste0(fmt, " ± ", fmt), s$metric, s$se)
      metric_el <- tagList(tags$span(class = "ik-card-metric", val),
                           if (nzchar(rate_unit)) tags$span(class = "ik-card-unit", rate_unit))
      if (!is.na(pm)) {
        dir <- .ov_dir(s$metric, pm, dp); want <- desirable[[tx]] %||% "down"
        cls <- .ov_change_cls(dir, want, colour)
        arrow <- tagList(" ", tags$span(class = paste0("ik-arrow ik-arrow-", cls),
                                        c(up = "▲", down = "▼", flat = "–")[[dir]]))
      }
    }
    if (headline == "rai") {
      big <- tagList(metric_el, arrow)
      small <- tags$span(class = "ik-card-cnt", sprintf("%s detections", .ov_num(cnt)))
    } else {
      big <- tagList(tags$span(class = "ik-card-metric", .ov_num(cnt)),
                     tags$span(class = "ik-card-unit", " catches"))
      small <- tagList(metric_el, arrow)
    }
    tags$div(
      class   = paste("ik-metric-card", if (has) "ik-spp-click"),
      title   = if (has) "Show the reserve breakdown",
      onclick = if (has && !is.null(drill_id)) sprintf(
        "Shiny.setInputValue('%s',{taxon:%s,reserve:%s,kind:'%s'},{priority:'event'})", drill_id, .ik_jsq(tx), .ik_jsq(rsv), kind),
      tags$div(class = "ik-card-name",
               if (!is.null(sg)) {
                 gi   <- match(tx, sg$label)
                 # sentiment tint on CAMERA cards only (trapping targets are all pests → all red, no
                 # signal; and it keeps the icon colour from competing with the comparison arrows there).
                 icls <- paste0("ik-card-icon",
                   if (identical(kind, "camera") && !is.na(sg$sentiment[gi])) paste0(" ik-icon-", sg$sentiment[gi]))
                 ik_species_icon(sg$group[gi], icls)
               }, tx),
      tags$div(class = "ik-card-big", big),
      tags$div(class = "ik-card-small", small))
  }
  # Order by the headline value (RAI for camera, catch count for trap), biggest first, so the row
  # reads by volume and wraps tidily; no-data species sort last (cheap — just an order() at render).
  # Project-configurable (overview$sort_cards_by): "value" sorts; anything else keeps config order.
  if (identical(sort_by, "value")) {
    hv <- vapply(taxa_order, function(tx) {
      if (headline == "count") { c0 <- as.integer(counts[tx] %||% 0L); return(if (is.na(c0)) 0 else as.numeric(c0)) }
      s <- summary[summary$taxon == tx & summary$reserve == "Combined", , drop = FALSE]
      if (nrow(s) && !is.na(s$metric)) s$metric else -Inf
    }, numeric(1))
    taxa_order <- taxa_order[order(-hv, seq_along(taxa_order))] # -Inf (no data) last; stable on ties
  }
  # `extra` is an optional trailing card (e.g. the slim main page's "see the full overview" link).
  tags$div(class = paste("ik-metric-cards", if (isTRUE(compact)) "ik-compact"), lapply(taxa_order, one), extra)
}

#' Trailing "see more" card for the slim main Overview blocks — a link card that navigates to the
#' device's own Overview page (where every species + the full breakdown live). Sets the global
#' `ik_goto_nav` input, handled in server.R via `nav_select`. @param nav target nav value.
#' @param dest device label ("Monitoring"/"Trapping") for the link text. @keywords internal
.ov_more_card <- function(nav, dest) tags$div(
  class = "ik-metric-card ik-card-more",
  title = sprintf("Open the full %s overview", tolower(dest)),
  onclick = sprintf("Shiny.setInputValue('ik_goto_nav', %s, {priority:'event'})", .ik_jsq(nav)),
  tags$div(class = "ik-card-more-lead", sprintf("This %s summary shows flagged species only.", tolower(dest))),
  tags$div(class = "ik-card-more-link", sprintf("%s Detail", dest), icon("arrow-right-long")))

#' Drop possible-duplicate observations — the project-defined NET view used across the analytical
#' surface (cards, value boxes, drill-downs). Camera-only in effect: trap obs carry no duplicate
#' flag, so they pass through. NULL `ik_data` is a safe no-op. @keywords internal
.ov_net_obs <- function(obs, ik_data) {
  if (is.null(obs) || !nrow(obs) || is.null(ik_data)) return(obs)
  rel <- ik_relations(ik_data)
  d <- rel$possible_duplicate[match(obs$observationID, rel$observationID)]
  if (is.null(d) || !is.logical(d)) return(obs)               # no duplicate flag (e.g. trap-only data) → no dedup
  obs[is.na(d) | !d, , drop = FALSE]                          # keep non-duplicates
}

#' Map a device (`source_type`) to its ACTIVITY label — the same word the navbar menu uses, so the
#' page heading reads "{org} — Monitoring/Trapping". Trapping is predator CONTROL (trapping today,
#' poison/etc. later); when a non-trap control device arrives, map it here too and the join below
#' collapses {Trapping, …} → "Control". Falls back to a title-cased device for unknown types so a new
#' dataset is never label-less. @keywords internal
.OV_DEVICE_LABEL <- c(camera = "Monitoring", trap = "Trapping")

#' Header activity label for a set of devices present on the page: each device's activity word,
#' de-duplicated and joined with " & " (so the main page across camera+trap reads "Monitoring &
#' Trapping"; a single-device page reads just "Monitoring" / "Trapping"). NULL for no devices.
#' @keywords internal
.ov_section_label <- function(source_types) {
  st <- unique(stats::na.omit(source_types)); if (!length(st)) return(NULL)
  labs <- unname(ifelse(st %in% names(.OV_DEVICE_LABEL), .OV_DEVICE_LABEL[st], tools::toTitleCase(st)))
  paste(unique(labs), collapse = " & ")
}

#' Display string for a selection's PERIOD span (the header date line). A contiguous selection
#' (one season / a year / rolling-12 / latest / all data) shows its window `start – end`; a
#' "seasonall" selection (All summers/…) is non-contiguous, so instead of a misleading single range
#' it shows the season count + the year span. @keywords internal
.ov_period_span <- function(ik_data, sel) {
  fmt <- function(x) format(x, "%d %b %Y")
  sc  <- ik_season_calendar(ik_data); if (is.null(sc) || !nrow(sc)) return("—")
  sub <- if (length(sel$season)) sc[sc$calendar_season %in% sel$season, , drop = FALSE] else sc
  if (!nrow(sub)) return("—")
  p <- sel$period
  if (!is.null(p) && startsWith(p, "seasonall:")) {           # non-contiguous → count + year span
    nm <- sub("^seasonall:", "", p); cy <- .ik_cycle_year(sub$season, sub$season_year)
    return(sprintf("All %ss · %d seasons (%s – %s)", tolower(nm), nrow(sub),
                   .ik_cycle_year_label(min(cy)), .ik_cycle_year_label(max(cy))))
  }
  sprintf("%s – %s", fmt(min(sub$start)), fmt(max(sub$end) - 1))   # end is exclusive → show last day
}

#' Per-device header sub-line: how many distinct DAYS this device has records on in the selection,
#' plus its own reserve / line / location counts. The monitoring-window DATE is deliberately NOT shown
#' here — once a selection spans more than a season a single range misleads, and the actual window now
#' lives in the page-header period banner. NULL when nothing resolved. (`period` is unused — retained
#' for call-site compatibility.) @keywords internal
.ov_section_meta <- function(res, ik_data, period = NULL) {
  dep <- res$deployments; if (is.null(dep) || !nrow(dep)) return(NULL)
  obs  <- res$observations
  when <- if (!is.null(obs) && nrow(obs)) { w <- obs$eventEnd; na <- is.na(w); w[na] <- obs$eventStart[na]; w
          } else dep$deploymentEnd
  tz    <- attr(when, "tzone"); if (is.null(tz) || !nzchar(tz)) tz <- "Pacific/Auckland"
  ndays <- length(unique(as.Date(when[is.finite(as.numeric(when))], tz = tz)))   # distinct days with records
  locs  <- ik_data$app$geography$locations
  gl    <- locs[locs$location_id %in% unique(dep$locationID), , drop = FALSE]
  lines <- length(unique(paste(gl$reserve, gl$line)[!is.na(gl$line)]))   # distinct reserve×line (real lines)
  dot <- function() tags$span(class = "ik-ov-dot", "·")
  tags$div(class = "ik-device-meta",
    sprintf("%d day%s with records", ndays, if (ndays == 1) "" else "s"), dot(),
    sprintf("%d reserves · %d lines · %d locations",
            length(unique(gl$reserve[!is.na(gl$reserve)])), lines, length(unique(gl$location_id))))
}

#' One device card: metric value-boxes + an outcome-metric table + a species panel. Each value
#' box is clickable when `box_drill` (a Shiny input id) is given — clicking sets it to
#' `{kind, metric}` (metric = effort/deployments/records/species), handled into a drill modal.
#' Counts are NET (possible duplicates excluded) when `ik_data` is supplied. @keywords internal
.ov_device_section <- function(title, res, effort_label, effort_value,
                               deploy_label, record_label, sg, panel_fn, metric = NULL,
                               kind = "", box_drill = NULL, spp_drill = NULL, subs = NULL,
                               ik_data = NULL, help = NULL, period = NULL) {
  obs <- res$observations
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  obs <- .ov_net_obs(obs, ik_data)                            # net by default (trap no-op)
  spp <- length(unique(stats::na.omit(obs$scientificName)))
  box <- function(metric_key, ...) {                       # value box, clickable when wired
    args <- list(...)                                      # optional secondary sub-line under the value
    if (!is.null(subs[[metric_key]])) args <- c(args, list(tags$div(class = "ik-box-sub", subs[[metric_key]])))
    vb <- do.call(value_box, args)
    if (is.null(box_drill)) return(vb)
    div(class = "ov-box-click", title = "Show the underlying records",
        onclick = sprintf("Shiny.setInputValue('%s',{kind:'%s',metric:'%s'},{priority:'event'})",
                          box_drill, kind, metric_key),
        vb)
  }
  card(
    card_header(tags$span(class = "ik-device-title", icon(.ov_device_icon(kind)), " ", title), help,
                .ov_section_meta(res, ik_data, period),
                class = paste0("ik-device-head ik-device-", kind)),
    card_body(
      layout_column_wrap(
        width = 1/4,
        box("effort",      effort_label, effort_value,                   showcase = icon("clock")),
        box("deployments", deploy_label, .ov_num(nrow(res$deployments)),
            showcase = icon(if (identical(kind, "trap")) "clipboard-check" else "camera")),
        box("records",     record_label, .ov_num(nrow(obs)),             showcase = icon("paw")),
        box("species",     "Species",    .ov_num(spp),                   showcase = icon("dna"))
      ),
      metric,
      panel_fn(obs, sg, kind, spp_drill)
    )
  )
}

#' One slim device block for the COMPACT main Overview: a small device title, two clickable value
#' boxes (records + species) and the target-species cards — the cross-device headline. The full
#' effort/deployments boxes, the by-reserve matrix and the "other species" panel live on the device's
#' OWN Overview page now. Boxes/cards reuse the same `box_drill`/`drill` modals as the full section.
#' @keywords internal
.ov_compact_block <- function(title, kind, record_label, record_value, species_value, cards,
                              box_drill = NULL, help = NULL, count_label = NULL, count_value = NULL) {
  box <- function(metric_key, label, value, icon_name, drill = TRUE, tip = "Show the underlying records") {
    vb <- value_box(label, value, showcase = icon(icon_name))
    if (is.null(box_drill) || !drill) return(vb)
    div(class = "ov-box-click", title = tip,
        onclick = sprintf("Shiny.setInputValue('%s',{kind:'%s',metric:'%s'},{priority:'event'})",
                          box_drill, kind, metric_key),
        vb)
  }
  tags$div(
    class = paste0("ik-ov-compact-block ik-device-", kind),
    tags$div(class = "ik-ov-compact-head",
             tags$span(class = "ik-device-title ik-ov-compact-title",
                       icon(.ov_device_icon(kind)), " ", title),
             help),
    # A leading device-COUNT box (active cameras / traps in the period) adds a sense of network size and,
    # with three boxes rather than two, stops them stretching super-wide on a laptop. It drills to the SAME
    # modal as the device's own Overview card: camera Deployments ("deployments") / trap Traps ("effort").
    layout_column_wrap(
      width = if (is.null(count_value)) 1/2 else 1/3,
      if (!is.null(count_value)) box(
        if (identical(kind, "trap")) "effort" else "deployments",
        count_label, count_value, .ov_device_icon(kind),
        tip = if (identical(kind, "trap")) "Show the traps" else "Show the deployments"),
      box("records", record_label, record_value, "paw"),
      box("species", "Species",    species_value, "dna")),
    cards
  )
}

#' Empty-state for a device section (no data loaded / nothing in the selection) — keeps the styled
#' header so the page still reads as two sections. @keywords internal
.ov_empty_section <- function(title, kind, msg) {
  card(
    card_header(title, class = paste0("ik-device-head ik-device-", kind)),
    card_body(tags$p(class = "ik-spp-other", msg)))
}

#' Camera possible-duplicate tally for a resolved selection: raw animal records, net (non-duplicate)
#' count, duplicate count and %. A possible duplicate is a repeat of the same species at the same
#' camera within the project window; excluded from net RAI. Drives the Detections sub-line + RAI
#' caption. @keywords internal
.ov_cam_dup <- function(res, ik_data) {
  obs <- res$observations
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  if (!nrow(obs)) return(list(raw = 0L, net = 0L, dup = 0L, pct = 0))
  rel <- ik_relations(ik_data)
  d   <- rel$possible_duplicate[match(obs$observationID, rel$observationID)]; d[is.na(d)] <- FALSE
  list(raw = nrow(obs), net = sum(!d), dup = sum(d), pct = round(100 * sum(d) / nrow(obs)))
}

#' The four camera value-box sub-lines for a resolved camera selection: Camera-hrs → cameras · lines
#' (lines = the RAI replication unit); Deployments → median deployment length (protocol ≈ 21 nights);
#' Detections → net count + possible duplicates (so the headline raw count reconciles to the net
#' figure the cards/RAI use); Species → most-detected. NULL when no deployments. @keywords internal
.ov_camera_subs <- function(res, ik_data, prefer) {
  deps <- res$deployments
  if (is.null(deps) || !nrow(deps)) return(NULL)
  det  <- res$observations
  det  <- det[!is.na(det$observationType) & det$observationType == "animal", , drop = FALSE]
  n_cam  <- length(unique(deps$locationID))
  # line numbers repeat across reserves (1..4 in each), so count distinct reserve×line pairs — the
  # RAI replication unit (matches the metric's summed n_lines), not the bare line value.
  n_line <- nrow(unique(deps[!is.na(deps$line), c("reserve", "line"), drop = FALSE]))
  med_n  <- stats::median(as.numeric(difftime(deps$deploymentEnd, deps$deploymentStart, units = "hours")) / 24, na.rm = TRUE)
  dd  <- .ov_cam_dup(res, ik_data)                          # the box headline is NET; sub shows raw + dup
  net <- .ov_net_obs(det, ik_data)                          # most-detected from the net records
  top <- if (nrow(net)) {
    lab <- ik_species_label(net$scientificName, ik_data, prefer); lab[is.na(lab)] <- "Unidentified"
    names(sort(table(lab), decreasing = TRUE))[1]
  } else NULL
  list(
    effort      = sprintf("%s camera%s · %s line%s", .ov_num(n_cam), if (n_cam == 1) "" else "s",
                          .ov_num(n_line), if (n_line == 1) "" else "s"),
    deployments = if (is.finite(med_n)) sprintf("median %s nights", .ov_num(round(med_n))) else "—",
    records     = sprintf("%s raw · %s duplicates", .ov_num(dd$raw), .ov_num(dd$dup)),
    species     = if (!is.null(top)) sprintf("most: %s", top) else "—")
}

#' The four trapping value-box sub-lines for a resolved trap selection: Trap-nights → distinct traps;
#' Checks → servicing-health split (good/watch/neglected, canonical thresholds); Catches → traps that
#' caught something; Species → the most-caught species. NULL when the selection has no checks.
#' @keywords internal
.ov_trap_subs <- function(res, ik_data, prefer) {
  deps <- res$deployments
  if (is.null(deps) || !nrow(deps)) return(NULL)
  det  <- res$observations
  det  <- det[!is.na(det$observationType) & det$observationType == "animal", , drop = FALSE]
  n_traps <- length(unique(deps$locationID))
  hh   <- table(factor(ik_trap_health(.trap_mean_intervals(deps), ik_data),
                       levels = c("good", "watch", "neglected")))
  succ <- unique(deps$locationID[match(det$deploymentID, deps$deploymentID)])
  n_succ <- length(succ[!is.na(succ)])
  top  <- if (nrow(det)) {
    lab <- ik_species_label(det$scientificName, ik_data, prefer); lab[is.na(lab)] <- "Unidentified"
    names(sort(table(lab), decreasing = TRUE))[1]
  } else NULL
  list(
    effort      = sprintf("%s trap%s", .ov_num(n_traps), if (n_traps == 1) "" else "s"),
    deployments = sprintf("%d good · %d watch · %d neglected", hh[["good"]], hh[["watch"]], hh[["neglected"]]),
    records     = sprintf("%s trap%s caught", .ov_num(n_succ), if (n_succ == 1) "" else "s"),
    species     = if (!is.null(top)) sprintf("most: %s", top) else NULL)
}

#' Overview server.
#'
#' @param id                Module id.
#' @param ik_data           The ik_data container.
#' @param prefer_scientific A reactive returning TRUE to show scientific names (used for
#'   non-registry caught species in the trap panel).
#' @param selection         A reactive returning the selection SPEC (period/geography).
#' @param sections          Which device(s) this instance covers ("camera"/"trap") — drives the header
#'   activity label (Monitoring/Trapping) on the device pages.
#' @param landing           TRUE for the main cross-device landing page — its header is just the
#'   organisation name (no "— Monitoring & Trapping" suffix, which reads too technical for a landing).
overview_server <- function(id, ik_data, prefer_scientific, selection, sections = c("camera", "trap"),
                            landing = FALSE, network = FALSE, color_mode = reactive("light")) {
  moduleServer(id, function(input, output, session) {
    is_dark <- reactive(identical(color_mode(), "dark"))
    sg       <- ik_species_groups(ik_data)
    locs     <- ik_data$app$geography$locations
    projects <- unique(unlist(lapply(ik_data$datasets, function(d) d$meta$project)))
    has_camera <- ik_has_source_type(ik_data, "camera"); has_trap <- ik_has_source_type(ik_data, "trap")
    mon_t <- ik_taxa_groups(sg, "monitor", "target")        # lead cards (protocol targets)
    mon_i <- ik_taxa_groups(sg, "monitor", "interesting")   # secondary "of interest" tier
    mon_targets <- c(mon_t, mon_i)                          # camera metric/drill set = target + interesting
    ctl_targets <- .ov_control_card_taxa(ik_data)   # trap cards: EVERY caught species (minus control="hide")
    ctl_t       <- ik_taxa_groups(sg, "control", "target")  # compact headline shows only control TARGETS
    # Desirable change direction from the card SENTIMENT (not role): "good" (protected) wants UP,
    # "bad" (pest/predator) wants DOWN, "neutral" wants NEITHER — a species that's neither a pest nor
    # protected (e.g. weka, role "other" → neutral) gets an uncoloured arrow, not a false green/red.
    desire  <- function(taxa) {
      s <- sg$sentiment[match(names(taxa), sg$label)]
      stats::setNames(ifelse(is.na(s) | s == "neutral", "none", ifelse(s == "good", "up", "down")), names(taxa))
    }
    mon_dir   <- desire(mon_targets)
    ctl_dir   <- desire(ctl_targets)
    ctl_t_dir <- desire(ctl_t)

    cam     <- reactive(ik_resolve(ik_data, selection(), source_type = "camera"))
    trp     <- reactive(ik_resolve(ik_data, selection(), source_type = "trap"))
    # Append the combined row (mean ± SE over reserves) when >1 reserve — the right-most
    # "Combined" column. Only meaningful with several reserves to average.
    with_network <- function(summary) {
      if (length(unique(summary$reserve)) > 1) dplyr::bind_rows(summary, ik_metric_combine(summary))
      else summary
    }
    # Compute a metric for the selection + (when a comparison is set) the comparison period,
    # keeping the comparison's per-line table too so the drill modal can show prior values.
    # Cached: the result is numeric + deterministic in (selection axes, comparison, active datasets),
    # so it's keyed on those and shared across sessions in-process — the FIRST visitor to the default
    # landing view warms it and every later visitor gets it free (until restart / re-import). `key`
    # distinguishes the metrics (bindCache shares one cache, so each cached reactive needs a unique key).
    metric_react <- function(fn, taxa, key) reactive({
      m  <- fn(ik_data, selection(), taxa)
      m$summary <- with_network(m$summary)
      cs <- ik_comparison_spec(ik_data, selection())
      if (!is.null(cs)) {
        p <- fn(ik_data, cs, taxa)
        m$prev       <- with_network(p$summary)
        m$prev_lines <- p$lines
      } else { m$prev <- NULL; m$prev_lines <- NULL }
      m
    }) |> bindCache(key, selection()$period, selection()$compare, selection()$season,
                    selection()$reserve, selection()$line, selection()$location, ik_active_datasets())
    rai_r  <- metric_react(ik_rai, mon_targets, "rai")          # full set (target + interesting) — device pages
    rate_r <- metric_react(ik_trap_rate, ctl_targets, "rate")   # full set (every caught species) — device pages
    # The slim main page shows only TARGET cards, so it computes only those — not the interesting tier
    # / every-caught-species the full device sections need. Halves the landing's metric work.
    rai_compact  <- metric_react(ik_rai, mon_t, "rai_compact")
    rate_compact <- metric_react(ik_trap_rate, ctl_t, "rate_compact")

    output$header <- renderUI({
      # The instance ORGANISATION (project.R) leads, with the page's ACTIVITY appended so one header
      # serves all three Overviews: "{org} — Monitoring" (camera page) / "— Trapping" (trap page) /
      # "— Monitoring & Trapping" (the main page across both). Datasets now sit on each device's
      # section header (not here); the period banner sits inside the view it filters (not here).
      org     <- ik_data$meta$organisation
      active  <- ik_active_datasets()                           # reactive → re-runs when the dataset toggle is saved
      st      <- ik_dataset_source_types(if (is.null(active)) ik_data$datasets
                                         else ik_data$datasets[intersect(names(ik_data$datasets), active)])
      present <- sort(unique(stats::na.omit(st[st %in% sections])))
      seclab  <- .ov_section_label(present)
      title   <- if (isTRUE(landing)) {
        org %||% "Overview"                                    # landing: just the organisation
      } else if (!is.null(org)) {
        if (!is.null(seclab)) paste0(org, " — ", seclab) else org   # device page: "{org} — Monitoring"
      } else seclab %||% "Overview"
      tags$div(class = "ik-ov-header", tags$h3(title))
    })

    # Tab-aware period banner above the title: the Trends tab spans every season (period doesn't apply)
    # so it reads "All data"; Snapshot and the single-view device pages honour the selected window.
    output$period_banner <- renderUI(
      .ik_period_banner(ik_data, selection(), all_data = input$ov_tabs %in% c("Trends", "Network density")))

    # ---- Network density tab (structural coverage; all-data) — only on the cross-device landing ----
    if (isTRUE(network)) {
      nd_hulls <- reactive(ik_selection_hulls(ik_active_locations(ik_data), "reserve"))   # footprints (map + hover)

      # Per-reserve density table (moved here from Insights -> Coverage). A cross-reserve comparison, so it
      # shows EVERY reserve with a device — including the coordless pseudo-reserves (Unknown / Outside), which
      # get device counts but no area metrics (build_coverage drops them: no coherent convex hull). Structural,
      # independent of both Period and the sidebar Reserve (matching the whole-network map beside it). A "Total"
      # row sums counts/area; densities there are area-weighted (total devices ÷ total footprint) and spacing is
      # trap-weighted.
      output$density <- DT::renderDT({
        cam <- ik_active_locations(ik_data, "camera"); trp <- ik_active_locations(ik_data, "trap")
        validate(need(nrow(cam) || nrow(trp), "No located devices."))
        cov <- ik_coverage(ik_data)                                # area/density/spacing — coherent reserves only
        present  <- unique(c(cam$reserve, trp$reserve))            # every reserve with a device
        rsv <- .ik_nz(selection()$reserve); if (!is.null(rsv)) present <- intersect(present, rsv)  # honour the sidebar Reserve
        validate(need(length(present), "No reserves in this selection."))
        reserves <- c(intersect(cov$reserve, present), sort(setdiff(present, cov$reserve)))  # coherent (area desc) first
        idx  <- match(reserves, cov$reserve)
        area <- cov$area_km2[idx]; cpk <- cov$cameras_per_km2[idx]; tpk <- cov$traps_per_km2[idx]
        spc  <- cov$mean_trap_spacing_m[idx]
        ncam <- as.integer(table(factor(cam$reserve, levels = reserves)))
        ntrp <- as.integer(table(factor(trp$reserve, levels = reserves)))
        coh  <- !is.na(area); ta <- sum(area[coh])                 # footprints we can size (for the weighted means)
        w    <- !is.na(spc) & ntrp > 0                             # reserves with a spacing + traps (spacing weight)
        t_cpk <- if (ta > 0) sum(ncam[coh]) / ta else NA_real_     # area-weighted density = total devices ÷ total area
        t_tpk <- if (ta > 0) sum(ntrp[coh]) / ta else NA_real_
        t_spc <- if (any(w)) stats::weighted.mean(spc[w], ntrp[w]) else NA_real_   # trap-weighted spacing
        if (length(reserves) > 1) {                                # aggregate row only when comparing >1 reserve
          reserves <- c(reserves, "Total")
          area <- c(area, ta); ncam <- c(ncam, sum(ncam)); ntrp <- c(ntrp, sum(ntrp))
          cpk  <- c(cpk, t_cpk); tpk <- c(tpk, t_tpk); spc <- c(spc, t_spc)
        }
        fa <- function(a) ifelse(is.na(a) | a <= 0, "—", format(round(a * 100), big.mark = ","))
        fd <- function(d) ifelse(is.na(d), "—", sprintf("%.1f", d))
        fs <- function(s) ifelse(is.na(s), "—", format(round(s), big.mark = ","))
        df <- data.frame(Reserve = reserves, `Area (ha)` = fa(area), Cameras = ncam, Traps = ntrp,
          `Cameras/km²` = fd(cpk), `Traps/km²` = fd(tpk), `Trap spacing (m)` = fs(spc),
          check.names = FALSE, stringsAsFactors = FALSE)
        DT::datatable(df, rownames = FALSE, selection = "none", class = "stripe hover row-border ik-nd-table",
          options = list(dom = "t", ordering = FALSE, paging = FALSE,
            rowCallback = DT::JS("function(r,d){if(d[0]==='Total'){r.style.fontWeight='700';r.style.borderTop='2px solid var(--bs-border-color,#ccc)';}}"),
            createdRow = DT::JS(sprintf(                            # hover a reserve row → outline it on the map
              "function(row,data){var R=data[0];if(R==='Total')return;row.addEventListener('mouseenter',function(){Shiny.setInputValue('%s',R,{priority:'event'});});row.addEventListener('mouseleave',function(){Shiny.setInputValue('%s','',{priority:'event'});});}",
              session$ns("density_hover"), session$ns("density_hover")))))
      })

      # Simplified network map — Cameras / Traps / Boundary only (no surface/predators), all-data. Every
      # layer is static, so it's drawn once in the base render; only the tiles re-theme via a proxy below.
      output$network_map <- leaflet::renderLeaflet({
        fin   <- function(d) if (is.null(d) || !nrow(d)) NULL else d[is.finite(d$longitude) & is.finite(d$latitude), , drop = FALSE]
        rsv  <- .ik_nz(selection()$reserve)                        # honour the sidebar Reserve
        keep <- function(d) { if (is.null(d) || is.null(rsv)) return(d); x <- d[d$reserve %in% rsv, , drop = FALSE]; if (nrow(x)) x else NULL }
        cams  <- keep(fin(ik_active_locations(ik_data, "camera")))
        traps <- keep(fin(ik_active_locations(ik_data, "trap")))
        ext   <- rbind(cams[, c("longitude", "latitude")], traps[, c("longitude", "latitude")])
        validate(need(!is.null(ext) && nrow(ext), "No mapped devices in this selection."))
        h <- nd_hulls(); if (!is.null(h) && !is.null(rsv)) h <- h[h$reserve %in% rsv, , drop = FALSE]
        canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
        m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
        m <- leaflet::addProviderTiles(m, canvas, group = "Map")
        m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
        if (!is.null(h) && nrow(h)) m <- leaflet::addPolygons(m, data = h, group = "Boundary", label = ~reserve,
          fill = FALSE, color = "#6c757d", weight = 1.5, opacity = 0.85, dashArray = "5,5")
        if (!is.null(traps)) m <- leaflet::addCircleMarkers(m, data = traps, lng = ~longitude, lat = ~latitude,
          group = "Traps", radius = 4, stroke = FALSE, fillColor = "#8a8a8a", fillOpacity = 0.85,
          label = ~name, popup = ~sprintf("<b>%s</b><br>Trap &middot; %s", name, reserve))
        if (!is.null(cams)) m <- leaflet::addCircleMarkers(m, data = cams, lng = ~longitude, lat = ~latitude,
          group = "Cameras", radius = 4, stroke = FALSE, fillColor = "#2c7fb8", fillOpacity = 0.85,
          label = ~name, popup = ~sprintf("<b>%s</b><br>Camera &middot; %s", name, reserve))
        ov <- c(if (!is.null(cams)) "Cameras", if (!is.null(traps)) "Traps", if (!is.null(h) && nrow(h)) "Boundary")
        m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"), overlayGroups = ov,
          options = leaflet::layersControlOptions(collapsed = FALSE))
        leaflet::fitBounds(m, min(ext$longitude), min(ext$latitude), max(ext$longitude), max(ext$latitude),
          options = list(padding = c(30, 30)))
      })
      # No suspendWhenHidden override: this map lives on a hidden tab of the LANDING page, so let it render
      # lazily on first open (at full size) rather than load ~1k markers into a 0-size box at every startup.
      # Every layer is in the base render, so nothing races a proxy; maps.js re-fits on later tab shows.

      observeEvent(color_mode(), {                                      # re-theme tiles without a full re-render
        p <- leaflet::leafletProxy("network_map", session); leaflet::clearGroup(p, "Map")
        leaflet::addProviderTiles(p, if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron, group = "Map")
      }, ignoreInit = TRUE)

      # Hover a density-table row → outline that reserve's footprint on the map (cleared on mouse-out).
      nd_hover <- shiny::debounce(reactive(input$density_hover), 100)
      observeEvent(nd_hover(), {
        p <- leaflet::leafletProxy("network_map", session); leaflet::clearGroup(p, "nd_highlight")
        r <- nd_hover(); if (is.null(r) || !nzchar(r)) return()
        h <- nd_hulls(); if (is.null(h) || !nrow(h)) return()
        hr <- h[h$reserve == r, , drop = FALSE]; if (!nrow(hr)) return()
        leaflet::addPolygons(p, data = hr, group = "nd_highlight", fill = TRUE, fillColor = "#cf6819",
          fillOpacity = 0.15, color = "#cf6819", weight = 3, opacity = 0.95,
          options = leaflet::pathOptions(interactive = FALSE))
      }, ignoreNULL = FALSE)
    }

    output$camera <- renderUI({
      if (is.null(cam()$deployments) || !nrow(cam()$deployments))
        return(.ov_empty_section("Camera monitoring", "camera",
          if (has_camera) "No camera monitoring data for the current selection." else "No camera monitoring data loaded."))
      rc  <- ik_data$meta$camera$rai
      m   <- rai_r()
      prefer  <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      itemise <- isTRUE(ik_data$meta$overview$list_other_species)
      dd  <- .ov_cam_dup(cam(), ik_data)                     # possible-duplicate share, for the caption
      cap <- sprintf("RAI · per %s camera-hrs%s%s", format(rc$norm_hours, big.mark = ","),
                     if (isTRUE(rc$use_net)) sprintf(" · net (%s%% excluded)", dd$pct) else "",
                     if (!is.null(m$prev)) .ov_compare_note(selection()$compare) else "")
      cnts   <- .ov_counts(m$lines)
      mk     <- function(taxa, compact) .ov_metric_cards(m$summary, m$prev, cnts, names(taxa),
                  "camera", mon_dir, session$ns("drill"), headline = "rai", compact = compact, sg = sg,
                  sort_by = ik_data$meta$overview$sort_cards_by %||% "value")
      cards  <- tagList(
        if (length(mon_t)) mk(mon_t, FALSE),                              # protocol targets lead
        if (length(mon_i)) mk(mon_i, TRUE))                               # of-interest species follow: smaller, set off by the gap
      matrix <- if (isTRUE(ik_data$meta$overview$show_rai_matrix_by_reserve))
        .ov_metric_table(m$summary, names(mon_targets), "By reserve", prev = m$prev,
                         desirable = mon_dir, drill_id = session$ns("drill"), kind = "camera")
      .ov_device_section("Camera monitoring", cam(),
                         "Camera-hrs", .ov_num(round(cam()$effort_hours)),
                         "Deployments", "Detections", sg,
                         function(o, s, k, sd) .ov_panel_monitor(o, s, ik_data, prefer, itemise, k, sd),
                         metric = tagList(tags$div(class = "ik-metric-cap", cap), cards, matrix),
                         kind = "camera", box_drill = session$ns("box_drill"),
                         spp_drill = session$ns("spp_drill"), subs = .ov_camera_subs(cam(), ik_data, prefer),
                         ik_data = ik_data, period = selection()$period,
                         help = .ik_info(session$ns("mon_help"), "Camera monitoring — how to read this",
                                         overview_monitor_help_body(ik_data$meta$camera$rai$norm_hours %||% 2000)))
    })

    output$trapping <- renderUI({
      if (is.null(trp()$deployments) || !nrow(trp()$deployments))
        return(.ov_empty_section("Trapping (control)", "trap",
          if (has_trap) "No trapping data for the current selection." else "No trapping data loaded."))
      prefer  <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      itemise <- isTRUE(ik_data$meta$overview$list_other_species)
      m       <- rate_r()
      cap    <- sprintf("Captures · per %s trap-nights%s", ik_data$meta$trapping$rate$norm_trap_days,
                        if (!is.null(m$prev)) .ov_compare_note(selection()$compare) else "")
      cards  <- .ov_metric_cards(m$summary, m$prev, .ov_counts(m$lines), names(ctl_targets),
                                 "trap", ctl_dir, session$ns("drill"), headline = "count",
                                 rate_unit = sprintf(" /%s TN", ik_data$meta$trapping$rate$norm_trap_days %||% 100),
                                 colour = FALSE, digits = 3, min_digits = 2, sg = sg,
                                 sort_by = ik_data$meta$overview$sort_cards_by %||% "value")
      matrix <- if (isTRUE(ik_data$meta$overview$show_rai_matrix_by_reserve))
        .ov_metric_table(m$summary, names(ctl_targets), "By reserve", prev = m$prev,
                         desirable = ctl_dir, drill_id = session$ns("drill"), kind = "trap",
                         colour = FALSE, digits = 3, min_digits = 2)
      section <- .ov_device_section("Trapping (control)", trp(),
                         "Trap-nights", .ov_num(round(trp()$effort_hours / 24)),
                         "Checks", "Catches", sg,
                         function(o, s, k, sd) NULL,   # trapping cards now cover EVERY caught species → no "other" section

                         metric = tagList(tags$div(class = "ik-metric-cap", cap), cards, matrix),
                         kind = "trap", box_drill = session$ns("box_drill"),
                         spp_drill = session$ns("spp_drill"), subs = .ov_trap_subs(trp(), ik_data, prefer),
                         ik_data = ik_data, period = selection()$period,
                         help = .ik_info(session$ns("trap_help"), "Trapping — how to read this",
                                         overview_trap_help_body(ik_data$meta$trapping$rate$norm_trap_days %||% 100)))
      # Network-health strip on top — a row of red→green gauges (v1: Network active, Check frequency). Modular:
      # add/redo metrics in ik_trapping_health(); the strip UI is generic (R/functions/health.R).
      tagList(
        ik_health_strip(ik_trapping_health(ik_data, selection(), trp()), "Network health",
          help = .ik_info(session$ns("health_help"), "Network health — how to read this", health_help_body(ik_data))),
        section)
    })

    # The COMPACT main Overview: one panel, both devices, just the headline — Detections/Catches +
    # Species boxes and the TARGET-species cards (camera: monitor targets, RAI; trap: control targets,
    # catch count). The full effort/by-reserve/other-species detail + drills are on each device's own
    # Overview page; the boxes/cards here reuse the very same box_drill/drill modals. Net counts.
    output$compact <- renderUI({
      sort_by <- ik_data$meta$overview$sort_cards_by %||% "value"   # cards label off sg, so no name-pref needed here
      n_spp   <- function(o) length(unique(stats::na.omit(o$scientificName)))
      animals <- function(res, net) { o <- res$observations
        o <- o[!is.na(o$observationType) & o$observationType == "animal", , drop = FALSE]
        if (net) .ov_net_obs(o, ik_data) else o }
      blocks <- list()
      if (has_camera && !is.null(cam()$deployments) && nrow(cam()$deployments)) {
        o <- animals(cam(), TRUE); m <- rai_compact()          # targets only — the slim landing
        cards <- .ov_metric_cards(m$summary, m$prev, .ov_counts(m$lines),
          names(mon_t), "camera", mon_dir, session$ns("drill"), headline = "rai", sg = sg, sort_by = sort_by,
          extra = .ov_more_card("monitoring-overview", "Monitoring"))   # → full overview (every species)
        blocks[["camera"]] <- .ov_compact_block("Camera monitoring", "camera",
          "Detections", .ov_num(nrow(o)), .ov_num(n_spp(o)), cards, session$ns("box_drill"),
          count_label = "Active cameras", count_value = .ov_num(length(unique(cam()$deployments$locationID))),
          help = .ik_info(session$ns("mon_help_brief"), "Camera monitoring",
                          overview_monitor_help_brief(ik_data$meta$camera$rai$norm_hours %||% 2000)))
      }
      if (has_trap && !is.null(trp()$deployments) && nrow(trp()$deployments)) {
        o <- animals(trp(), FALSE); m <- rate_compact()        # control targets only — the slim landing
        unit  <- sprintf(" /%s TN", ik_data$meta$trapping$rate$norm_trap_days %||% 100)
        cards <- .ov_metric_cards(m$summary, m$prev, .ov_counts(m$lines),
          names(ctl_t), "trap", ctl_t_dir, session$ns("drill"), headline = "count",
          rate_unit = unit, colour = FALSE, digits = 3, min_digits = 2, sg = sg, sort_by = sort_by,
          extra = .ov_more_card("trapping-overview", "Trapping"))   # → full overview (every species caught)
        blocks[["trap"]] <- .ov_compact_block("Trapping", "trap",
          "Catches", .ov_num(nrow(o)), .ov_num(n_spp(o)), cards, session$ns("box_drill"),
          count_label = "Active traps", count_value = .ov_num(length(unique(trp()$deployments$locationID))),
          help = .ik_info(session$ns("trap_help_brief"), "Trapping",
                          overview_trap_help_brief(ik_data$meta$trapping$rate$norm_trap_days %||% 100)))
      }
      if (!length(blocks))
        return(.ov_empty_section("Overview", "camera",
          if (has_camera || has_trap) "No data for the current selection." else "No data loaded."))
      card(card_body(class = "ik-ov-compact", do.call(tagList, unname(blocks))))
    })

    # Drill-down: a clicked metric cell opens its breakdown (the auditable basis) — a
    # reserve cell drills to its per-LINE values; the "Combined" cell drills to the
    # per-RESERVE values the combined mean averages. When a comparison is active, each row
    # also shows the prior-period value + a desirability-coloured arrow (same key as the
    # table), so the comparison number is verifiable too.
    # The arrow describes how the CURRENT value moved, so it binds to the current cell (next
    # to the RAI/rate value), with the prior shown plainly in its own column as the reference.
    arrow_span <- function(cur, prv, want, digits = 2, colour = TRUE) {  # arrow, NULL if no prior
      if (length(prv) == 0 || is.na(prv)) return(NULL)
      dir <- .ov_dir(cur, prv, digits)                   # at the cell's display precision
      cls <- .ov_change_cls(dir, want, colour)
      tagList(" ", tags$span(class = paste0("ik-arrow ik-arrow-", cls),
                             c(up = "▲", down = "▼", flat = "–")[[dir]]))
    }
    prior_td <- function(prv, digits) tags$td(class = "ik-metric-prev",
      if (length(prv) == 0 || is.na(prv)) "–" else sprintf(paste0("%.", digits, "f"), prv))
    # State for the ONE tabbed drill modal (Breakdown → Records → Record), so a deeper click
    # switches tabs in place instead of stacking modals. Mirrors the Bait effectiveness drill.
    drill_spec <- reactiveVal(NULL)   # the clicked metric cell: {taxon, reserve, kind}
    records    <- reactiveVal(NULL)   # the records behind a clicked Breakdown count
    rec_ctx    <- reactiveVal(NULL)   # caption for the Records tab (which line)
    rec_obs    <- reactiveVal(NULL)   # observationID open in the Record tab (also the highlight)

    observeEvent(input$drill, {
      drill_spec(input$drill)
      records(NULL); rec_ctx(NULL); rec_obs(NULL)         # fresh drill
      d <- input$drill
      rlab <- if (identical(d$reserve, "Combined")) "all reserves" else d$reserve   # plainer than "Combined"
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s · %s", d$taxon, rlab),
          "the auditable basis — open a count for its records, then a record for the full detail"),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("drill_tabs"),
          tabPanel("Summary",        icon = icon("table-list"),  uiOutput(session$ns("drill_breakdown"))),
          tabPanel("Records",        icon = icon("list"),        uiOutput(session$ns("drill_records_cap")),
                                                                 DT::dataTableOutput(session$ns("drill_records"))),
          tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("drill_record"))))))
      hideTab(session = session, inputId = "drill_tabs", target = "Records")        # revealed on drill
      hideTab(session = session, inputId = "drill_tabs", target = "Record details")
    })

    # Breakdown tab: the per-line (reserve cell) or per-reserve (Combined cell) basis the metric
    # averages, with comparison arrows. A non-zero count cell is clickable → sets input$drill_obs
    # (handled below → Records tab).
    output$drill_breakdown <- renderUI({
      d <- drill_spec(); req(d)
      is_cam <- identical(d$kind, "camera")
      res    <- if (is_cam) rai_r() else rate_r()
      sm     <- res$summary[res$summary$taxon == d$taxon & res$summary$reserve == d$reserve, ]
      name   <- if (is_cam) "RAI" else "rate"
      want   <- (if (is_cam) mon_dir else ctl_dir)[[d$taxon]] %||% "down"
      cmp    <- !is.null(res$prev)                       # comparison active?
      clab   <- switch(selection()$compare %||% "none", prior = "Prior", last_year = "Last yr", "Prev")
      colour <- is_cam                                   # trap arrows neutral (per project decision)
      dg     <- if (is_cam) 1L else 3L                   # reserve/summary precision (rate 3 dp)
      dg_ln  <- if (is_cam) 2L else 3L                   # per-line precision
      fd     <- paste0("%.", dg, "f")                    # reserve/summary fmt
      fl     <- paste0("%.", dg_ln, "f")                 # per-line fmt

      if (identical(d$reserve, "Combined")) {            # combined cell → per-reserve breakdown
        R  <- res$summary[res$summary$taxon == d$taxon & res$summary$reserve != "Combined", , drop = FALSE]
        R  <- R[order(R$reserve), , drop = FALSE]
        pv <- if (cmp) res$prev[res$prev$taxon == d$taxon & res$prev$reserve != "Combined", , drop = FALSE]
        # records per reserve (sum over its lines) → a reserve row is clickable when it has any,
        # drilling to that reserve's records across ALL its lines (line omitted).
        cnt     <- if (is_cam) "individuals" else "captures"
        cnt_lab <- if (is_cam) "Detections" else "Catches"        # concrete count beside the metric
        Ld   <- res$lines[res$lines$taxon == d$taxon, , drop = FALSE]
        nrec <- tapply(Ld[[cnt]], Ld$reserve, sum)
        tbl <- tags$table(class = "ik-drill-table",
          tags$thead(tags$tr(tags$th("Reserve"), tags$th(cnt_lab), tags$th(tools::toTitleCase(name)),
                             if (cmp) tags$th(clab), tags$th("Lines"))),
          tags$tbody(lapply(seq_len(nrow(R)), function(i) {
            prv   <- if (cmp) pv$metric[pv$reserve == R$reserve[i]] else NULL
            cn    <- nrec[R$reserve[i]]; cn <- if (length(cn) == 0 || is.na(cn)) 0L else as.integer(cn)
            drill <- cn > 0
            tags$tr(
              class   = if (drill) "ik-drill-row" else NULL,
              title   = if (drill) "Show this reserve's records" else NULL,
              onclick = if (drill) sprintf(
                "Shiny.setInputValue('%s',{taxon:%s,reserve:%s,kind:'%s'},{priority:'event'})",
                session$ns("drill_obs"), .ik_jsq(d$taxon), .ik_jsq(R$reserve[i]), d$kind) else NULL,
              tags$td(R$reserve[i]),
              tags$td(.ov_num(cn)),
              tags$td(sprintf(fd, R$metric[i]),
                      if (!is.na(R$se[i])) sprintf(paste0(" ± ", fd), R$se[i]),
                      if (cmp) arrow_span(R$metric[i], prv, want, dg, colour)),
              if (cmp) prior_td(prv, dg),
              tags$td(.ov_num(R$n_lines[i])))
          })))
        return(tagList(
          tags$p(class = "ik-drill-summary", sprintf(
            paste0("Combined = ", fd, "%s over %d reserves — mean of the per-reserve %s below."),
            sm$metric, if (!is.na(sm$se)) sprintf(paste0(" ± ", fd, " SE"), sm$se) else "", sm$n_lines, name)),
          tbl))
      }

      L   <- res$lines[res$lines$taxon == d$taxon & res$lines$reserve == d$reserve, , drop = FALSE]
      L   <- L[order(suppressWarnings(as.numeric(L$line)), L$line), , drop = FALSE]
      pl  <- if (cmp) res$prev_lines[res$prev_lines$taxon == d$taxon &
                                     res$prev_lines$reserve == d$reserve, , drop = FALSE]
      cnt <- if (identical(d$kind, "camera")) "individuals" else "captures"
      eff <- if (identical(d$kind, "camera")) "camera_hours" else "trap_days"
      hdr <- if (identical(d$kind, "camera")) c("Individuals", "Camera-hrs", "RAI")
             else c("Captures", "Trap-days", "Rate")
      tbl <- tags$table(class = "ik-drill-table",
        tags$thead(tags$tr(tags$th("Line"), tags$th(hdr[1]), tags$th(hdr[2]), tags$th(hdr[3]),
                           if (cmp) tags$th(clab))),
        tags$tbody(lapply(seq_len(nrow(L)), function(i) {
          prv  <- if (cmp) pl$metric[pl$line == L$line[i]] else NULL
          drill <- L[[cnt]][i] > 0                          # one action per row → whole row clickable
          tags$tr(
            class   = if (drill) "ik-drill-row" else NULL,
            title   = if (drill) "Show these records" else NULL,
            onclick = if (drill) sprintf(
              "Shiny.setInputValue('%s',{taxon:%s,reserve:%s,line:%s,kind:'%s'},{priority:'event'})",
              session$ns("drill_obs"), .ik_jsq(d$taxon), .ik_jsq(d$reserve), .ik_jsq(L$line[i]), d$kind) else NULL,
            tags$td(L$line[i]),
            tags$td(.ov_num(L[[cnt]][i])),
            tags$td(.ov_num(round(L[[eff]][i]))),
            tags$td(sprintf(fl, L$metric[i]), if (cmp) arrow_span(L$metric[i], prv, want, dg_ln, colour)),
            if (cmp) prior_td(prv, dg_ln))
        })))
      tagList(
        tags$p(class = "ik-drill-summary", sprintf(
          paste0("Reserve = ", fd, "%s over %d lines — mean of the per-line %s below."),
          sm$metric, if (!is.na(sm$se)) sprintf(paste0(" ± ", fd, " SE"), sm$se) else "", sm$n_lines, name)),
        tbl)
    })

    # A clicked Breakdown count → list its records in the Records tab (no new modal).
    # One handler for all "← Back" links in this module's drill modals.
    observeEvent(input$tab_back, updateTabsetPanel(session, input$tab_back$tabset, selected = input$tab_back$to))

    observeEvent(input$drill_obs, {
      d    <- input$drill_obs
      is_cam <- identical(d$kind, "camera")
      taxa <- if (is_cam) mon_targets else ctl_targets
      records(ik_metric_obs(ik_data, selection(), taxa, d$taxon,
                            reserve = d$reserve, line = d$line,
                            source_type = if (is_cam) "camera" else "trap"))
      rec_obs(NULL)
      noun  <- if (is_cam) "detections" else "captures"
      where <- if (!is.null(d$line)) sprintf("%s · Line %s", d$reserve, d$line) else d$reserve
      rec_ctx(sprintf("The %s behind %s · %s. ", noun, d$taxon, where))
      showTab(session = session, inputId = "drill_tabs", target = "Records", select = TRUE)
    })

    # Caption only — the DTOutput itself is static in the tabPanel (created once when the modal opens),
    # so the renderDT below binds once and just refreshes data. (Nesting the DTOutput in this
    # re-rendering uiOutput recreated the element on every drill and raced the tab-switch → an
    # intermittent blank first render. The box/trap drills keep their DTOutput static for the same reason.)
    output$drill_records_cap <- renderUI({
      if (is.null(records()))
        return(tags$p(class = "ik-drill-summary",
          "Open a non-zero count in the Summary tab to list the records behind it here."))
      tagList(
        .ik_tab_back(session$ns("tab_back"), "drill_tabs", "Summary", "Back to summary"),
        tags$p(class = "ik-drill-summary", rec_ctx(),
               tags$b("Click a row"), " for that record's full detail — then come back for another."))
    })

    output$drill_records <- DT::renderDT({
      o <- records()
      validate(need(!is.null(o) && nrow(o), "No records."))
      prefer  <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      when_lab <- .ik_when_label(o$when)
      df <- data.frame(When = when_lab,
                       Species = ik_species_label(o$scientificName, ik_data, prefer),
                       Count = o$count, Location = o$locationName, ObsID = o$observationID,
                       check.names = FALSE, stringsAsFactors = FALSE)
      df$.when_sort <- as.numeric(o$when)                            # chronological sort key for "When"
      dt <- DT::datatable(df, rownames = FALSE, selection = "single",   # whole row → the record
                    class = "stripe hover row-border ik-row-click",
                    options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
                      columnDefs = .ik_dt_when_defs(df, "When", "ObsID")))
      .ik_dt_highlight_row(dt, "ObsID", rec_obs())                  # the record you're viewing
    })

    observeEvent(input$drill_records_rows_selected, {
      i <- input$drill_records_rows_selected; o <- records()
      if (length(i) && !is.null(o) && i <= nrow(o)) {
        rec_obs(o$observationID[i]); showTab(session = session, inputId = "drill_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("drill_records"), NULL)      # clear → same row re-clickable
    })

    # Record Details tab: the full observation inline, with the SAME Photos/Details/Provenance
    # sub-tabs as the standalone record viewer (reused via .ovw_tabs). Mirrors the Bait drill.
    output$drill_record <- renderUI({
      if (is.null(rec_obs()))
        return(tags$p(class = "ik-drill-summary", "Click a record in the Records tab to see it in full here."))
      ob <- ik_observation(ik_data, rec_obs())
      if (is.null(ob)) return(tags$p("Record not found."))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      tagList(.ik_tab_back(session$ns("tab_back"), "drill_tabs", "Records", "Back to records"),
              .ovw_title(ik_data, ob, prefer),
              .ovw_tabs(ik_data, ob, prefer, tabset_id = session$ns("rec_subtabs")))
    })

    # --- value-box drills: each box opens the rows behind its number for the current selection.
    # records (Detections/Catches) → the observations (whole-row → the record viewer); species →
    # species × count; deployments/effort → the deployment list with per-deployment effort.
    box_records <- reactiveVal(NULL)   # prepared records df (When·Species·Count·Location·ObsID)
    box_deps    <- reactiveVal(NULL)   # prepared CAMERA deployments df
    box_traps   <- reactiveVal(NULL)   # prepared per-TRAP summary df (trap drill: Traps tab)
    box_checks  <- reactiveVal(NULL)   # the selected trap's individual checks df (Checks tab)
    box_checks_ctx <- reactiveVal(NULL)# caption for the Checks tab (which trap)
    box_rec_obs <- reactiveVal(NULL)   # observationID open in the box drill's Record Details tab
    fmt_dt <- function(x) ifelse(is.na(x), "—",
      ifelse(format(x, "%H:%M:%S") == "00:00:00", format(x, "%d %b %Y"), format(x, "%d %b %Y · %H:%M")))

    # Trap drill is grouped by TRAP (locationID): one row per trap (Traps tab) → its checks (Checks
    # tab) → the check record. Both derive from the resolved trap selection `trp()` so they match the
    # value boxes exactly. Per-trap servicing health reuses the canonical .trap_mean_intervals/
    # ik_trap_health; per-check outcome/bait/volunteer come from the full observation (one per check).
    build_trap_traps <- function() {
      deps <- trp()$deployments
      if (is.null(deps) || !nrow(deps)) return(NULL)
      det  <- trp()$observations
      det  <- det[!is.na(det$observationType) & det$observationType == "animal", , drop = FALSE]
      cap  <- table(deps$locationID[match(det$deploymentID, deps$deploymentID)])
      mi   <- .trap_mean_intervals(deps)
      nights <- as.numeric(difftime(deps$deploymentEnd, deps$deploymentStart, units = "hours")) / 24
      ag <- dplyr::summarise(dplyr::group_by(deps, .data$locationID),
        Location = dplyr::first(.data$locationName), Reserve = dplyr::first(.data$reserve),
        Line = dplyr::first(.data$line), Checks = dplyr::n(),
        .last = suppressWarnings(max(.data$deploymentEnd, na.rm = TRUE)), .groups = "drop")
      tn <- tapply(nights, deps$locationID, sum, na.rm = TRUE)
      ag$Nights  <- round(as.numeric(tn[as.character(ag$locationID)]), 1)
      ag$Catches <- as.integer(cap[as.character(ag$locationID)]); ag$Catches[is.na(ag$Catches)] <- 0L
      hh <- as.character(ik_trap_health(mi[as.character(ag$locationID)], ik_data)); hh[is.na(hh)] <- "—"
      df <- data.frame(Location = ag$Location, Reserve = ag$Reserve, Line = ag$Line,
        Checks = ag$Checks, Nights = ag$Nights, Catches = ag$Catches, Health = tools::toTitleCase(hh),
        Last = fmt_dt(ag$.last), check.names = FALSE, stringsAsFactors = FALSE)
      df$.last_sort <- as.numeric(ag$.last); df$.locid <- ag$locationID
      df
    }
    build_trap_checks <- function(locid) {
      deps <- trp()$deployments
      here <- deps[!is.na(deps$locationID) & deps$locationID == locid, , drop = FALSE]
      if (!nrow(here)) return(NULL)
      here <- here[order(here$deploymentEnd, decreasing = TRUE), , drop = FALSE]
      ao  <- ik_observations(ik_data, with_location = FALSE)
      oi  <- match(here$deploymentID, ao$deploymentID)             # exactly one observation per check
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      lab <- ik_species_label(ao$scientificName[oi], ik_data, prefer)
      status <- vapply(ao$observationTags[oi], .ovw_tag, character(1), key = "status")
      outcome <- .ik_trap_outcome(lab, ao$scientificName[oi], status, ao$observationType[oi])
      bait <- vapply(ao$observationTags[oi], .ovw_tag, character(1), key = "bait")
      vol  <- vapply(ao$observationTags[oi], .ovw_tag, character(1), key = "volunteer")
      bait[is.na(bait) | !nzchar(bait)] <- "—"; vol[is.na(vol) | !nzchar(vol)] <- "—"
      df <- data.frame(
        End = fmt_dt(here$deploymentEnd),
        Nights = round(as.numeric(difftime(here$deploymentEnd, here$deploymentStart, units = "hours")) / 24, 1),
        Outcome = outcome, Bait = bait, Volunteer = vol, check.names = FALSE, stringsAsFactors = FALSE)
      df$.end_sort <- as.numeric(here$deploymentEnd); df$.obsid <- ao$observationID[oi]
      df
    }

    # Shared records modal (value-box "records" + the species-panel drill): a 2-tab modal —
    # Records (a DT) → Record Details (the inline viewer) — so you can step into a record AND tab
    # back to the list, same as the metric-cell drill (no replace-modal dead end).
    show_records_modal <- function(obs, is_cam, title, subtitle) {
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      when   <- if (is_cam) obs$eventStart else obs$eventEnd
      box_records(data.frame(
        When = fmt_dt(when), Species = ik_species_label(obs$scientificName, ik_data, prefer),
        Count = obs$count, Location = obs$locationName, ObsID = obs$observationID,
        .when_sort = as.numeric(when), check.names = FALSE, stringsAsFactors = FALSE))
      box_rec_obs(NULL)
      showModal(modalDialog(
        title = .ik_modal_title(title, subtitle),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("box_tabs"),
          tabPanel("Records", icon = icon("list"),
                   DT::dataTableOutput(session$ns("box_records_table"))),
          tabPanel("Record details", icon = icon("circle-info"),
                   uiOutput(session$ns("box_record"))))))
      hideTab(session = session, inputId = "box_tabs", target = "Record details")   # revealed on row click
    }

    observeEvent(input$box_drill, {
      bd     <- input$box_drill
      box_records(NULL); box_deps(NULL); box_traps(NULL); box_checks(NULL); box_rec_obs(NULL)  # drop last modal
      is_cam <- identical(bd$kind, "camera")
      r      <- if (is_cam) cam() else trp()
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      dev    <- if (is_cam) "Camera monitoring" else "Trapping (control)"
      obs    <- r$observations
      obs    <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
      obs    <- .ov_net_obs(obs, ik_data)              # drills show the NET (de-duplicated) records

      if (identical(bd$metric, "records")) {
        noun <- if (is_cam) "Detections" else "Catches"
        show_records_modal(obs, is_cam, sprintf("%s · %s", dev, noun),
          "every record in the current selection — click a row for the full record")

      } else if (identical(bd$metric, "species")) {
        noun <- if (is_cam) "Detections" else "Catches"
        lab  <- ik_species_label(obs$scientificName, ik_data, prefer); lab[is.na(lab)] <- "Unidentified"
        tb   <- sort(table(lab), decreasing = TRUE)
        tbl  <- tags$table(class = "ik-drill-table",
          tags$thead(tags$tr(tags$th("Species"), tags$th(noun))),
          tags$tbody(lapply(seq_along(tb), function(i)
            tags$tr(tags$td(names(tb)[i]), tags$td(.ov_num(as.integer(tb[i])))))))
        showModal(modalDialog(
          title = .ik_modal_title(sprintf("%s · Species", dev),
            sprintf("%d species · %s %s", length(tb), .ov_num(nrow(obs)), tolower(noun))),
          if (length(tb)) tbl else tags$p("No animals recorded for this selection."),
          size = "m", easyClose = TRUE, footer = modalButton("Close")))

      } else if (is_cam) {                                # CAMERA deployments → one row per deployment
        dep <- r$deployments
        eff <- as.numeric(difftime(dep$deploymentEnd, dep$deploymentStart, units = "hours"))
        df  <- data.frame(
          Location = dep$locationName, Reserve = dep$reserve, Line = dep$line,
          Start = fmt_dt(dep$deploymentStart), End = fmt_dt(dep$deploymentEnd),
          check.names = FALSE, stringsAsFactors = FALSE)
        df$Effort      <- round(eff)                       # camera-hours (unit in the subtitle)
        df$.start_sort <- as.numeric(dep$deploymentStart)  # hidden: chronological sort + the row drill key
        df$.end_sort   <- as.numeric(dep$deploymentEnd)
        df$.depid      <- dep$deploymentID
        box_deps(df)
        showModal(modalDialog(
          title = .ik_modal_title("Camera monitoring · Deployments",
            sprintf("%s deployments · effort in camera-hours — click one for its detections", .ov_num(nrow(dep)))),
          size = "l", easyClose = TRUE, footer = modalButton("Close"),
          DT::dataTableOutput(session$ns("box_deps_table"))))

      } else {                                            # TRAP checks → grouped by trap: Traps → Checks → Record
        df <- build_trap_traps()
        box_traps(df); box_checks(NULL); box_checks_ctx(NULL)
        showModal(modalDialog(
          title = .ik_modal_title("Trapping (control) · Traps",
            sprintf("%s traps · %s checks — open a trap for its checks, then a check for the record",
                    .ov_num(if (is.null(df)) 0L else nrow(df)), .ov_num(nrow(r$deployments)))),
          size = "l", easyClose = TRUE, footer = modalButton("Close"),
          tabsetPanel(id = session$ns("trap_tabs"),
            tabPanel("Traps",  icon = icon("location-dot"), DT::dataTableOutput(session$ns("box_traps_table"))),
            tabPanel("Checks", icon = icon("list"),         uiOutput(session$ns("box_checks_ui"))),
            tabPanel("Record", icon = icon("circle-info"),  uiOutput(session$ns("box_record"))))))
        hideTab(session = session, inputId = "trap_tabs", target = "Checks")        # revealed on drill
        hideTab(session = session, inputId = "trap_tabs", target = "Record")
      }
    })

    output$box_records_table <- DT::renderDT({
      df <- box_records()
      validate(need(!is.null(df) && nrow(df), "No records."))
      dt <- DT::datatable(df, rownames = FALSE, selection = "single",  # whole row → Record Details
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          columnDefs = .ik_dt_when_defs(df, "When", "ObsID")))
      .ik_dt_highlight_row(dt, "ObsID", box_rec_obs())             # the record you're viewing
    })

    observeEvent(input$box_records_table_rows_selected, {
      i <- input$box_records_table_rows_selected; df <- box_records()
      if (length(i) && !is.null(df) && i <= nrow(df)) {
        box_rec_obs(df$ObsID[i]); showTab(session = session, inputId = "box_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("box_records_table"), NULL)
    })

    output$box_record <- renderUI({
      if (is.null(box_rec_obs()))
        return(tags$p(class = "ik-drill-summary", "Click a record in the Records tab to see it in full here."))
      ob <- ik_observation(ik_data, box_rec_obs())
      if (is.null(ob)) return(tags$p("Record not found."))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      # box_record is shared by the camera Records modal (box_tabs) and the trap Checks modal
      # (trap_tabs) — point the back link at whichever list this record was opened from.
      back <- if (!is.null(box_traps())) .ik_tab_back(session$ns("tab_back"), "trap_tabs", "Checks", "Back to checks")
              else                       .ik_tab_back(session$ns("tab_back"), "box_tabs", "Records", "Back to records")
      tagList(back,
              .ovw_title(ik_data, ob, prefer),
              .ovw_tabs(ik_data, ob, prefer, tabset_id = session$ns("box_rec_subtabs")))
    })

    output$box_deps_table <- DT::renderDT({
      df <- box_deps()
      validate(need(!is.null(df) && nrow(df), "No deployments."))
      ix <- function(col) match(col, names(df)) - 1L                  # 0-based column index for DT
      DT::datatable(df, rownames = FALSE, selection = "single",       # row → its record(s)
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          order = list(list(ix(".start_sort"), "asc")),               # default: oldest → newest
          columnDefs = list(
            list(targets = ix("Start"), orderData = ix(".start_sort")),  # sort Start by its key
            list(targets = ix("End"),   orderData = ix(".end_sort")),
            list(targets = c(ix(".start_sort"), ix(".end_sort"), ix(".depid")), visible = FALSE))))
    })

    # Camera deployment drill: a deployment has many detections → list them (a row → the full record).
    observeEvent(input$box_deps_table_rows_selected, {
      i <- input$box_deps_table_rows_selected; df <- box_deps()
      if (!length(i) || is.null(df) || i > nrow(df)) return()
      obs <- cam()$observations
      obs <- obs[!is.na(obs$deploymentID) & obs$deploymentID == df$.depid[i] &
                   !is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
      obs <- .ov_net_obs(obs, ik_data)                  # net: exclude possible duplicates at this deployment
      show_records_modal(obs, TRUE, sprintf("Camera deployment · %s", df$Location[i]),
        sprintf("%s detection%s at this deployment — click a row for the full record",
                .ov_num(nrow(obs)), if (nrow(obs) == 1) "" else "s"))
      DT::selectRows(DT::dataTableProxy("box_deps_table"), NULL)
    })

    # Trap drill, level 1 — the TRAPS (one row per locationID). Click a trap → its checks (level 2).
    output$box_traps_table <- DT::renderDT({
      df <- box_traps()
      validate(need(!is.null(df) && nrow(df), "No traps."))
      ix <- function(col) match(col, names(df)) - 1L
      DT::datatable(df, rownames = FALSE, selection = "single",
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          order = list(list(ix("Catches"), "desc")),               # most productive traps first
          columnDefs = list(
            list(targets = ix("Last"), orderData = ix(".last_sort")),
            list(targets = c(ix(".last_sort"), ix(".locid")), visible = FALSE))))
    })
    observeEvent(input$box_traps_table_rows_selected, {
      i <- input$box_traps_table_rows_selected; df <- box_traps()
      if (!length(i) || is.null(df) || i > nrow(df)) return()
      box_checks(build_trap_checks(df$.locid[i])); box_rec_obs(NULL)
      box_checks_ctx(sprintf("The %s check%s of %s · %s. ", .ov_num(df$Checks[i]),
        if (df$Checks[i] == 1) "" else "s", df$Location[i],
        if (!is.na(df$Line[i])) paste("Line", df$Line[i]) else df$Reserve[i]))
      showTab(session = session, inputId = "trap_tabs", target = "Checks", select = TRUE)
      DT::selectRows(DT::dataTableProxy("box_traps_table"), NULL)
    })

    # Trap drill, level 2 — the selected trap's CHECKS. Click a check → its record (level 3).
    output$box_checks_ui <- renderUI({
      if (is.null(box_checks()))
        return(tags$p(class = "ik-drill-summary", "Open a trap in the Traps tab to list its checks here."))
      tagList(
        .ik_tab_back(session$ns("tab_back"), "trap_tabs", "Traps", "Back to traps"),
        tags$p(class = "ik-drill-summary", box_checks_ctx(),
               tags$b("Click a check"), " for its full record (catch or empty)."),
        DT::dataTableOutput(session$ns("box_checks_table")))
    })
    output$box_checks_table <- DT::renderDT({
      df <- box_checks()
      validate(need(!is.null(df) && nrow(df), "No checks."))
      ix <- function(col) match(col, names(df)) - 1L
      DT::datatable(df, rownames = FALSE, selection = "single",
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          order = list(list(ix("End"), "desc")),                   # most recent check first
          columnDefs = list(
            list(targets = ix("End"), orderData = ix(".end_sort")),
            list(targets = c(ix(".end_sort"), ix(".obsid")), visible = FALSE))))
    })
    observeEvent(input$box_checks_table_rows_selected, {
      i <- input$box_checks_table_rows_selected; df <- box_checks()
      if (length(i) && !is.null(df) && i <= nrow(df)) {
        box_rec_obs(df$.obsid[i]); showTab(session = session, inputId = "trap_tabs", target = "Record", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("box_checks_table"), NULL)
    })

    # Species-panel drill: click a species/group item below a device card → its records (the same
    # records modal as the Detections/Catches box, filtered to that item's scientificName set).
    observeEvent(input$spp_drill, {
      sd     <- input$spp_drill
      is_cam <- identical(sd$kind, "camera")
      r      <- if (is_cam) cam() else trp()
      dev    <- if (is_cam) "Camera monitoring" else "Trapping (control)"
      obs    <- r$observations
      obs    <- obs[!is.na(obs$observationType) & obs$observationType == "animal" &
                      obs$scientificName %in% sd$sci, , drop = FALSE]
      obs    <- .ov_net_obs(obs, ik_data)               # net: panel counts above are net, so is the list
      show_records_modal(obs, is_cam, sprintf("%s · %s", dev, sd$label),
        sprintf("%s %s in the current selection — click a row for the full record",
                .ov_num(nrow(obs)), if (is_cam) "detections" else "catches"))
    })
  })
}
