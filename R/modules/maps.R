# maps.R — the Maps view: a designed Leaflet map of the current selection, built as a CATALOGUE
# OF TOGGLEABLE LAYERS (the layers control is the toggle surface). Pick a DEVICE (camera/trap), a
# MEASURE, and a species GROUP; the map draws the relevant layers, and you can DRILL: click a
# marker to filter the records table to that place; click a record to pan/highlight its marker and
# open the observation viewer.
#
# Two tiers of number (deliberate): line RAI ± SE (ik_rai) is the CANONICAL/inferential metric;
# everything on the map (per-location/line values, surfaces, heatmap) is EXPLORATORY — for finding
# where to look and drilling in, not for citing.
#
# Measures & GRAIN:
#   • Camera / "Relative activity (RAI)" — RAI is a per-LINE index (DOC protocol). Shown at the LINE
#     grain when zoomed out (ik_rai()$lines + centroids), per-CAMERA detection rate when zoomed in
#     (norm camera_hours ≈ 500; NOT the 2000-CH line yardstick). Surface = per-camera rate field.
#   • Trap / "Captures" (default) — raw catch COUNT per trap (robust to effort/sparsity; "where are
#     catches"). Trap captures are sparse, so count beats rate as the activity signal.
#   • Trap / "Capture rate" — captures / trap-days × norm_trap_days (default 100; noisy at low effort — robust colour helps).
#   (Servicing health — check-frequency status — lives on its own page: Trap review → Map.)
# Robust sizing: marker radius is clamped at a high percentile (.MAPS_CAP_PCTL) so a single
# low-effort outlier (e.g. 1 capture in 7 trap-days → 14.29/100td) can't shrink the rest — but high
# enough that genuine high-activity sites still differ in size. Colour uses the TRUE max (legend matches).
# Rendering: one base render (panes + preferCanvas + theme basemap), then per-layer leafletProxy
# observers. Spatial maths in R/functions/spatial.R; per-location/line values in metrics.R.

MAPS_LINE_ZOOM <- 13   # at/above this zoom, camera activity shows per-camera; below, per-line
.MAPS_CAP_PCTL <- 0.98 # marker-SIZE clamp percentile — high enough that genuine high-activity sites
                       # still differ in size (only a true extreme outlier is clamped). Was 0.95, which
                       # flattened everything above ~the top 5% to one max radius.

.MAPS_STATUS <- list(
  light = c(good = "#2e7d32", watch = "#f9a825", neglected = "#c62828"),
  dark  = c(good = "#66bb6a", watch = "#fdd835", neglected = "#ef5350"))
.MAPS_STATUS_LEVELS <- c("good", "watch", "neglected")

# Measures per device — the map is instantiated device-locked (a camera "Monitoring map" + a trap
# "Trapping map"), so the device toggle is dropped and the measures come from here.
.MAPS_MEASURES <- list(
  camera = c("Relative activity" = "rate"),
  # "Priority (predator vs protected)" moved to its own Insights feature — "Predator pressure"
  #   (the per-location predator-vs-protected surface + a toggleable trapping overlay).
  # "Timing (predator ↔ protected)" moved to its own page — Co-occurrence → Map.
  # "Servicing health" lives on its own page (Trap review → Map, the richer status map), not a measure here.
  # "Captures" (raw count) was dropped — Capture rate (effort-adjusted) is the honest activity signal and
  #   mirrors the camera map's single rate measure; the raw catch count is still in each trap's popup + the
  #   records drill. (Its original reason — a short-effort outlier dominating the surface — was an
  #   Unassigned-area trap, now excluded from the surface.)
  trap   = c("Capture rate" = "rate"))

#' The "Measure" help body, written for the map's OWN device (each map is device-locked, so the
#' camera map shouldn't explain trap measures or vice versa). NULL device → both (combined map).
#' @keywords internal
.maps_measure_help <- function(device = NULL, norm = 100) {
  ntn <- paste0(format(norm, big.mark = ","), " trap-nights")
  cam <- list(
    tags$p("What the camera markers and shaded surface show."),
    tags$p(tags$b("Relative activity (RAI)"), " — detection rate for the chosen species group, effort-adjusted ",
      "by camera-hours. Zoom ", tags$b("out"), " for a line-level RAI (the DOC protocol's per-line index); ",
      "zoom ", tags$b("in"), " for the per-camera rate. The shaded surface interpolates it between cameras."),
    tags$p(tags$em("Where to act"), " — the predator-vs-protected ", tags$b("Predator pressure"),
      " surface now lives on its own ", tags$b("Insights → Predator pressure"), " feature, with a toggleable ",
      "trapping overlay."))
  trp <- list(
    tags$p("How each trap is coloured and sized on the map."),
    tags$p(tags$b("Capture rate"), " — predators of the chosen group caught per ", tags$b(ntn),
      " (effort-adjusted), so heavily- and lightly-checked traps compare fairly. Marker size is clamped ",
      "near the top of the range so one low-effort outlier (e.g. 1 catch in a few trap-nights) can't ",
      "shrink the rest. The raw catch ", tags$b("count"), " for each trap is in its popup and the records below."),
    tags$p(tags$em("Servicing health — how often each trap is checked — has its own map on the ",
      tags$b("Trap review"), " page (the Map tab).")))
  if (identical(device, "camera"))    do.call(tagList, cam)
  else if (identical(device, "trap")) do.call(tagList, trp)
  else tagList(tags$h6("Camera"), do.call(tagList, cam), tags$h6("Traps"), do.call(tagList, trp))
}

#' Choices + default for the in-panel Species select. Shared by maps_controls (BAKED into the UI so the
#' default shows — dropdown nav panels render their selectize lazily, so a server-side updateSelectInput
#' at startup misses them) and maps_server (relabels on name pref). (The predator-vs-protected pickers
#' left with the Priority measure — see the Predator pressure feature.) @keywords internal
.maps_picker_defs <- function(ik_data, prefer = "vernacular") {
  sg <- ik_species_groups(ik_data)
  groups <- c(ik_taxa_groups(sg, "monitor", "target"), ik_taxa_groups(sg, "monitor", "interesting"))
  list(species = ik_species_choices_full(ik_data, prefer),
       group_default = paste0("grp:", names(groups)[1]))   # default = highest-concern monitor group (config order)
}

#' The Maps sidebar controls — Measure + the measure-specific Species / Predator / Protected pickers.
#' Built in the MAP module's OWN namespace but rendered in the global sidebar (ui.R), beside
#' Period/Reserve; the maps server reads them by this namespace wherever they sit — the same relocation
#' `cooccurrence_controls()` uses. Choices are BAKED here (dropdown-lazy-safe); the server only relabels
#' them on a name-preference change. The Device toggle appears only for a non-device-locked (combined)
#' map — the Monitoring/Trapping instances are device-locked, so it's dropped.
#' @param id The MAP module's id (e.g. "monitoring_map"). @param device "camera"/"trap"/NULL.
#' @keywords internal
maps_controls <- function(id, device = NULL, ik_data = NULL) {
  ns   <- NS(id)
  meas <- .MAPS_MEASURES[[device %||% "camera"]]
  pk   <- .maps_picker_defs(ik_data)                            # baked picker defaults (dropdown-lazy-safe)
  multi <- length(meas) > 1                                     # >1 measure → keep a Measure radio (traps)
  div(class = "ik-selection ik-view-controls",
    tags$div(class = "ik-view-controls-h", "View options"),
    if (is.null(device))                                        # device-locked instances drop the toggle
      radioButtons(ns("source"), "Device",
                   choices = c(Camera = "camera", Trap = "trap"), selected = "camera", inline = TRUE),
    # The Measure radio only appears when the device has more than one (traps: Captures / Capture rate).
    # The camera map has a single measure (Relative activity, since Priority moved to its own feature).
    # The measure/RAI help lives on the PAGE HEADING (maps_panel_body), not on a sidebar control.
    if (multi) radioButtons(ns("measure"), "Measure", choices = meas, selected = unname(meas)[1]),
    # Species is the GROUPING choice; both remaining measures (camera RAI, trap captures/rate) group by it.
    selectInput(ns("species"), "Species", choices = pk$species, selected = pk$group_default, multiple = TRUE)
  )
}

#' Maps nav panel UI.
#' @param id Module id.
#' @return A bslib nav_panel for page_navbar().
#' The map's inner content (leaflet + records) — split out of `maps_ui` so it can be a standalone nav
#' panel OR embedded elsewhere (the Species page locks it to one species). The Measure/Species controls
#' live in the global sidebar (`maps_controls()`, wired in ui.R), not here.
#' @param fixed When TRUE (an embed, e.g. the Species page Map tab), the page header is dropped and there
#'   are no sidebar controls — the caller fixes the species via `maps_server(fixed_species=)` and the
#'   measure locks to the device default. @param height Leaflet height. @keywords internal
maps_panel_body <- function(id, device = NULL, ik_data = NULL, fixed = FALSE, height = "70vh") {
  ns   <- NS(id)
  # Standalone maps fill the viewport (height 100% inside the .ik-map-fill flex column); the embedded
  # species maps (fixed) keep their passed fixed height and stack.
  map_h   <- if (fixed) height else "100%"
  map     <- leaflet::leafletOutput(ns("map"), height = map_h)
  records <- div(
    class = "ik-maps-records",
    uiOutput(ns("drill_chip")),
    div(class = "ik-maps-records-header",
        uiOutput(ns("records_caption")),
        downloadButton(ns("download_csv"), "Download CSV", class = "btn-sm")),
    DT::DTOutput(ns("table"))
  )
  norm  <- (ik_data$meta$trapping$rate %||% list())$norm_trap_days %||% 100   # rate normalisation (config)
  ttl   <- if (identical(device, "trap")) "Trapping map" else if (identical(device, "camera")) "Monitoring map" else "Map"
  help_ttl <- if (identical(device, "trap")) "Trapping map — capture rate"
              else if (identical(device, "camera")) "Monitoring map — relative activity" else "Map measures"
  div(
    class = if (fixed) "ik-maps" else "ik-maps ik-map-fill",     # fill the viewport on standalone pages
    if (!fixed) .ik_page_header(                                 # page heading (the embedded species maps skip it)
      ttl,
      help = .ik_info(ns("measure_help"), help_ttl, .maps_measure_help(device, norm)),  # measure/RAI help on the title
      banner = div(class = "ik-page-period", uiOutput(ns("period_banner")))),
    # Standalone maps put the records table BESIDE the map (wide screens) — spending the surplus
    # horizontal space to buy vertical, so map + table fit one screen and the drill (click a marker →
    # filter the table) is visible at a glance. layout_columns wraps back to stacked on narrow windows.
    # The embedded species maps (fixed = TRUE) keep the simple stacked body — they already sit two-to-a-row.
    if (fixed) tagList(map, records)
    else layout_columns(
      class = "ik-maps-split", col_widths = breakpoints(sm = 12, lg = c(8, 4)),
      map,
      div(class = "ik-maps-side", records)),                     # height comes from .ik-map-fill (fills the row)
    uiOutput(ns("unplaced"))   # coordless-records note: a footnote below the table/columns
  )
}

maps_ui <- function(id, device = NULL, label = "Map", value = "map", ik_data = NULL) {
  nav_panel(
    label, value = value, icon = icon("map"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),
    tags$script(src = .ik_asset("js/maps.js")),
    maps_panel_body(id, device = device, ik_data = ik_data, fixed = FALSE, height = "80vh")
  )
}

#' Maps server.
#' @param id Module id. @param ik_data The ik_data container.
#' @param prefer_scientific Reactive: TRUE to show scientific names.
#' @param selection Reactive selection SPEC. @param color_mode Reactive theme ("light"/"dark").
#' @param active Reactive, TRUE when this map's tab is the current nav value. The base leaflet widget
#'   still renders from load (suspendWhenHidden = FALSE) so proxy layer-updates always land — but the
#'   HEAVY work (every metric/IDW *data* reactive `req(active())`s) is gated, exactly like coverage.R,
#'   so a map on a not-yet-viewed tab costs nothing at session start: the overview landing isn't slowed
#'   building two maps it isn't showing. The layers populate on first view (maps.js re-fits on show);
#'   navigating away leaves them drawn, so coming back is instant. NB: gate the DATA reactives, NEVER
#'   the base renderLeaflet — an active-gate inside the render aborts the whole widget (blank tiles).
#' @param fixed_species Optional named list `label = scientificNames` — locks the map to ONE species
#'   (the Species page embeds it this way): the in-panel pickers are dropped, the measure is the device
#'   default (camera rate / trap captures), and the surface groups by this taxon. NULL = the normal
#'   interactive map driven by the in-panel Species picker.
maps_server <- function(id, ik_data, prefer_scientific, selection, color_mode = reactive("light"),
                        device = NULL, active = reactive(TRUE), fixed_species = NULL) {
  moduleServer(id, function(input, output, session) {

    # Read-only period banner under the title (the sidebar holding Period/Compare can be collapsed).
    output$period_banner <- renderUI(.ik_period_banner(ik_data, selection()))

    # The all-locations overlay is named for what it actually holds: "Cameras" / "Traps" on a
    # device-locked map, "Devices" on a combined one. (Future device types extend this.)
    dev_label <- if (is.null(device)) "Devices" else if (identical(device, "trap")) "Traps" else "Cameras"

    prefer <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")

    observeEvent(input$source, {
      ch  <- if (identical(input$source, "trap")) c("Capture rate" = "rate")
             else c("Relative activity" = "rate")
      sel <- if (isTRUE(isolate(input$measure) %in% ch)) isolate(input$measure) else ch[[1]]
      updateRadioButtons(session, "measure", choices = ch, selected = sel)
    }, ignoreInit = TRUE)

    src      <- reactive(device %||% input$source %||% "camera")   # device-locked when set
    # measure: each device-locked map now has a SINGLE measure (camera "rate" / trap "rate"), so there's no
    # radio — fall back to the device default. (Captures dropped from the trap map; Priority/Timing moved to
    # their own features — Predator pressure / Co-occurrence.) A combined map would still read input$measure.
    measure  <- reactive(if (!is.null(fixed_species)) unname(.MAPS_MEASURES[[device %||% "camera"]])[1]
                         else input$measure %||% unname(.MAPS_MEASURES[[device %||% "camera"]])[1])
    # Same picker defs the UI bakes in; the observe below only RELABELS the Species choices on a name
    # preference change (the default group order comes from species_groups).
    pk <- .maps_picker_defs(ik_data)
    .group_default <- pk$group_default
    if (is.null(fixed_species)) observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      # Species offers the full set (groups, split per project flag, + every ungrouped species);
      # default = the top monitor-target group (config order).
      updateSelectInput(session, "species", choices = ik_species_choices_full(ik_data, p), selected = keep(isolate(input$species), .group_default))
    })
    # The map surface's taxa come from the in-panel Species pick (input$species), combined into one
    # labelled surface; default to the top monitor-target group. (The map uses Species as a GROUPING,
    # never as a spec filter — the dual role, like device.)
    group_taxa <- reactive({
      if (!is.null(fixed_species)) return(fixed_species)          # Species page: the page's taxon, locked
      v <- input$species; if (!length(v)) v <- .group_default
      sci <- ik_resolve_species_choice(v, ik_group_taxa(ik_data)); if (!length(sci)) return(NULL)
      stats::setNames(list(sci), paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ")) })
    group_lab  <- reactive({ g <- group_taxa(); if (is.null(g)) "group" else names(g) })
    # Species never enters the metrics spec (the map groups by it instead), so the selection spec is used
    # as-is; defensively drop any stray species axis so it can't double-filter to it.
    sel <- reactive({ s <- selection(); s$species <- NULL; s })
    is_dark  <- reactive(identical(color_mode(), "dark"))
    halo     <- reactive(if (is_dark()) "#1a1a1a" else "#ffffff")
    per_cam  <- reactive((ik_data$meta$camera$rai %||% list())$camera_hours %||% 500)
    norm     <- ik_data$meta$trapping$rate$norm_trap_days %||% 100   # trap rate normalisation (config)
    has_group <- reactive(!is.null(group_taxa()))
    # the per-location value column + its label (always the rate `metric` now — both maps are rate-only)
    valcol   <- reactive("metric")
    vallabel <- reactive(if (src() == "trap") "Capture rate"
                         else "Detections")   # per-camera (zoomed in); zoom out rolls up to "RAI / line"

    grain_rv <- reactiveVal("line")
    observeEvent(input$map_zoom, {
      g <- if (isTRUE(input$map_zoom >= MAPS_LINE_ZOOM)) "camera" else "line"
      if (!identical(g, grain_rv())) grain_rv(g)
    })
    lines_tbl <- reactiveVal(NULL)   # the lines shown in the table (line grain) — for the row drill
    # Show the LINE table (not per-camera records) when the camera-activity map is at line grain and
    # no single line is drilled into yet; clicking a line then drills to its records (via `selected`).
    show_lines <- reactive(src() == "camera" && measure() == "rate" && grain_rv() == "line" && is.null(selected()))

    # ---- data ----
    cam_all <- reactive({ req(active(), src() == "camera", has_group())
      ik_location_metric(ik_data, sel(), group_taxa(), "camera", norm = per_cam()) })
    cam_pts      <- reactive({ m <- cam_all(); if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
    cam_unplaced <- reactive({ m <- cam_all(); if (is.null(m)) NULL else m[!(is.finite(m$latitude) & is.finite(m$longitude)), , drop = FALSE] })

    line_metric <- reactive({ req(active(), src() == "camera", has_group())
      rr <- ik_rai(ik_data, sel(), group_taxa()); ln <- rr$lines
      if (is.null(ln) || !nrow(ln)) return(NULL)
      ce <- ik_group_centroids(cam_pts(), c("reserve", "line")); if (is.null(ce)) return(NULL)
      m <- merge(ln, ce, by = c("reserve", "line")); if (nrow(m)) m else NULL })

    trap_all <- reactive({ req(active(), src() == "trap", measure() == "rate", has_group())
      ik_location_metric(ik_data, sel(), group_taxa(), "trap") })
    trap_pts      <- reactive({ m <- trap_all(); if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
    trap_unplaced <- reactive({ m <- trap_all(); if (is.null(m)) NULL else m[!(is.finite(m$latitude) & is.finite(m$longitude)), , drop = FALSE] })

    rate_loc_pts <- reactive(if (src() == "trap") trap_pts() else cam_pts())
    frame_pts    <- reactive(rate_loc_pts())

    # ---- helpers ----
    # Colour/size scale: the TRUE max, so the legend always matches the markers. (A 95th-pctl robust cap
    # spread the colours better on a heavy tail, but it left markers above the cap reading the top colour
    # while the legend topped out lower — a recurring "key doesn't match reality" confusion.)
    .robust_cap  <- function(v) { m <- suppressWarnings(max(v, na.rm = TRUE)); if (is.finite(m) && m > 0) m else 1 }
    # Marker AREA scaled from 0 (area-fair: a 2 is ~twice the ink of a 1), not the data range — the old
    # range-rescale forced the smallest value to the floor and the largest to the ceiling, so 1 vs 2 blew
    # out to ~16x the area. Capped at .MAPS_CAP_PCTL so one big outlier can't shrink everyone. (Size only;
    # the colour key uses the true max — they're independent.)
    .area_radius <- function(v, lo, hi) ik_marker_radius(v, lo, hi, cap_pctl = .MAPS_CAP_PCTL)
    surf_pal <- function(cap) leaflet::colorNumeric("viridis", c(0, cap))
    # Hover card — the SAME compact popup whether you hover a MARKER or its row in the records table below.
    # Concise (no Line/Reserve line — click for the full record). One row in, one popup out.
    hover_popup <- function(d) {
      body <- if (src() == "trap")
        sprintf("<b>Capture rate: %.2f</b> / %s trap-nights<br/>Captures: %d &middot; Trap-days: %s",
                d$metric, format(norm, big.mark = ","), as.integer(d$captures), format(round(d$trap_days), big.mark = ","))
      else
        sprintf("<b>Detections: %d</b> &middot; Camera-hours: %s",
                as.integer(d$individuals), format(round(d$camera_hours), big.mark = ","))
      sprintf("<b>%s</b> &mdash; %s", d$name, body)
    }
    line_popup <- function(d) sprintf(
      "<b>Line %s</b> &middot; %s<br/>RAI: %.2f (per 2000 CH)<br/>%d cameras &middot; %d detections", d$line, d$reserve, d$metric, as.integer(d$n), as.integer(d$individuals))
    proxy <- function() leaflet::leafletProxy("map", session)

    # ---- drill state (map ↔ table) ----
    selected <- reactiveVal(NULL)   # NULL | list(kind="location"/"line", id|reserve|line, label)
    maprec_obs  <- reactiveVal(NULL)  # the record open in the camera/trap records modal's detail tab
    maprec_loc  <- reactiveVal(NULL)  # a clicked marker scopes the modal's Records tab to that location (NULL = all)
    # reset the drill when the data context changes (a clicked place may not exist in the new view)
    observeEvent(list(src(), measure(), input$species, selection()), selected(NULL), ignoreInit = TRUE)
    sel_coords <- function(sel) {
      if (is.null(sel)) return(NULL)
      if (identical(sel$kind, "location")) {
        locs <- ik_data$app$geography$locations; i <- match(sel$id, locs$location_id)
        if (is.na(i) || !is.finite(locs$longitude[i])) return(NULL)
        list(lng = locs$longitude[i], lat = locs$latitude[i])
      } else {
        d <- line_metric(); if (is.null(d)) return(NULL)
        r <- d[d$reserve == sel$reserve & d$line == sel$line, , drop = FALSE]
        if (!nrow(r)) return(NULL); list(lng = r$longitude[1], lat = r$latitude[1])
      }
    }

    # ---- base map ----
    output$map <- leaflet::renderLeaflet({
      # Initial frame uses THIS map's own device (device-locked instance) — a trap map must fit to trap
      # locations, not camera ones; the old camera-only filter left a trap-only org's map unframed.
      dev_types <- if (is.null(device)) c("camera", "trap") else device
      dev_ds <- names(ik_data$datasets)[vapply(ik_data$datasets,
                  function(d) isTRUE(d$meta$source_type %in% dev_types), logical(1))]
      locs <- ik_data$app$geography$locations
      locs <- locs[locs$dataset %in% dev_ds & is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
      m <- leaflet::addProviderTiles(m, canvas, group = "Map")
      m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
      # Boundary sits ABOVE the surface fill but BELOW every marker layer, so markers always win
      # the hover/click (a boundary on top would steal a marker's tooltip — see the Coverage map).
      pns <- c("surface", "boundary", "device", "points", "selected")
      for (pn in pns) m <- leaflet::addMapPane(m, pn, zIndex = 400 + 10 * match(pn, pns))
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
        overlayGroups = c("Surface", "Points", dev_label, "Boundary"),
        options = leaflet::layersControlOptions(collapsed = FALSE))
      if (!is.null(fixed_species)) m <- leaflet::hideGroup(m, "Surface")   # species maps: clean by default, surface opt-in
      if (nrow(locs)) m <- leaflet::fitBounds(m, min(locs$longitude), min(locs$latitude), max(locs$longitude), max(locs$latitude))
      m
    })
    # Render the map even while its tab is hidden. Maps live in dropdown nav panels that aren't the
    # landing tab, so without this the leaflet widget isn't created until first view — and the layer
    # observers below push their markers/surface via leafletProxy to a map that doesn't exist yet, so
    # they're dropped and the map shows blank (records table still fills, since it isn't proxy-based).
    # With the widget present from load, every proxy update lands; maps.js re-fits/sizes it on show.
    # The widget is eager, but the LAYERS it draws are NOT: every metric/IDW data reactive is gated on
    # active() (see the data section), so the eager widget stays empty — and cheap — until first viewed.
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    observeEvent(color_mode(), {
      p <- proxy(); leaflet::clearGroup(p, "Map")
      leaflet::addProviderTiles(p, if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron, group = "Map")
    }, ignoreInit = TRUE)

    observe({                                                   # re-frame on data change
      d <- frame_pts(); if (is.null(d) || !nrow(d)) return(); p <- proxy()
      if (nrow(d) == 1 || (diff(range(d$longitude)) == 0 && diff(range(d$latitude)) == 0))
        leaflet::setView(p, d$longitude[1], d$latitude[1], zoom = 14)
      else leaflet::fitBounds(p, min(d$longitude), min(d$latitude), max(d$longitude), max(d$latitude), options = list(padding = c(30, 30)))
    })

    # The CONSTANT reserve footprint, from the SHARED helper every map uses (ik_reserve_boundary): the hull
    # of ALL active devices per reserve, scoped to the sidebar Reserve. Independent of measure/species/period,
    # so the Boundary is the same on every map; the Surface is clipped to it (below).
    reserve_hulls <- reactive({ req(active()); ik_reserve_boundary(ik_data, selection()$reserve) })

    # ---- Surface (rate/count field; robust clamp) ----
    # IDW interpolation is the heaviest map computation. Cache it on the points + value column (the
    # points already encode the selection + active datasets, so they fully determine the surface);
    # revisiting a view — or another session on the same selection — then skips the recompute. The
    # theme is deliberately NOT in the key: it only affects the redraw below, not the interpolation.
    # Only interpolate when the Surface layer is actually shown — the IDW is the heaviest computation,
    # so a hidden surface (species maps default it off) shouldn't pay for it. `input$map_groups` is the
    # leaflet layers-control state; NULL before the client reports → fall back to the default (standalone
    # on, species off). Toggling Surface on updates the input → the interpolation runs (then caches).
    # `input$map_groups` fires on EVERY layer toggle (and once when the client first reports the control
    # state on load). A plain reactive here would therefore re-invalidate the surface — clearing and
    # redrawing it (a flicker) — whenever ANY other layer is toggled, or on that first NULL->reported
    # transition. Hold the answer in a reactiveVal that only updates when the SURFACE membership actually
    # flips, so unrelated toggles leave the surface untouched.
    surface_on <- reactiveVal(NULL)
    observe({
      v <- { g <- input$map_groups; if (is.null(g)) is.null(fixed_species) else "Surface" %in% g }
      if (!identical(v, surface_on())) surface_on(v)
    })
    surface_compute <- reactive({
      d <- rate_loc_pts(); if (is.null(d) || !nrow(d)) return(NULL)
      vc <- valcol()
      # Drop the catch-all pseudo-reserves (Unknown / Unassigned): their scattered devices aren't a real
      # footprint, so an IDW over them spreads a meaningless blob across the map (the Boundary already
      # excludes them — ik_selection_hulls).
      d <- d[!d$reserve %in% c(GEO_UNPLACED_RESERVE, GEO_OUTSIDE_RESERVE), , drop = FALSE]
      # A surface needs a SIGNAL: drop reserves whose value is 0 at every device. Otherwise IDW returns a
      # flat field of zeros that paints the whole reserve the lowest colour even though nothing was
      # detected/caught there (the "Mokoroa has a surface but no captures" report).
      if (nrow(d)) {
        pos <- names(which(tapply(d[[vc]], d$reserve, function(v) any(is.finite(v) & v > 0))))
        d <- d[d$reserve %in% pos, , drop = FALSE]
      }
      if (!nrow(d)) return(NULL)
      ik_idw_surface(d, vc, "reserve")
    }) |> bindCache(rate_loc_pts(), valcol())
    surface_idw <- reactive(if (isTRUE(surface_on())) surface_compute() else NULL)
    observe({
      p <- proxy(); leaflet::clearGroup(p, "Surface")
      s <- surface_idw(); if (is.null(s) || !nrow(s)) return()
      # Confine each reserve's field to its own reserve hull (the same hull the Boundary draws) — the shared
      # per-reserve clip, so an interpolation buffer can't bleed across a touching/overlapping boundary.
      s <- ik_clip_surface_to_reserves(s, reserve_hulls())
      if (is.null(s) || !nrow(s)) return()
      cap <- .robust_cap(s$predicted); pf <- surf_pal(cap)
      leaflet::addPolygons(p, data = s, group = "Surface", weight = 0.5, color = ~pf(pmin(predicted, cap)),
        fillColor = ~pf(pmin(predicted, cap)), fillOpacity = if (is_dark()) 0.5 else 0.4,
        options = leaflet::pathOptions(pane = "surface"))
    })

    # ---- Points (line RAI · per-camera rate · trap count/rate) ----
    observe({
      p <- proxy(); leaflet::clearGroup(p, "Points")
      if (src() == "camera" && grain_rv() == "line") {
        d <- line_metric(); if (is.null(d) || !nrow(d)) return()
        cap <- .robust_cap(d$metric); v <- pmin(d$metric, cap); pf <- leaflet::colorNumeric("viridis", c(0, cap))
        leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Points",
          layerId = paste0("L|", d$reserve, "|", d$line),
          radius = .area_radius(v, 8, 26), fillColor = pf(v), fillOpacity = 0.85, stroke = TRUE, color = halo(), weight = 1.5,
          label = sprintf("Line %s (%s) — RAI %.2f", d$line, d$reserve, d$metric),
          popup = line_popup(d), popupOptions = leaflet::popupOptions(autoPan = FALSE),
          options = leaflet::pathOptions(pane = "points"))
      } else {
        d <- rate_loc_pts(); vc <- valcol()
        d <- if (is.null(d)) NULL else d[is.finite(d[[vc]]) & d[[vc]] > 0, , drop = FALSE]
        if (is.null(d) || !nrow(d)) return()
        cap <- .robust_cap(d[[vc]]); v <- pmin(d[[vc]], cap); pf <- surf_pal(cap)
        # No label / no click-popup: hovering shows the same popup as a table row (mouseover handler below),
        # and clicking opens the records modal for the location (no leaflet popup — you're going to the modal).
        leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Points", layerId = d$location_id,
          radius = .area_radius(v, 5, 20), fillColor = pf(v), fillOpacity = 0.8, stroke = TRUE, color = halo(), weight = 1.5,
          options = leaflet::pathOptions(pane = "points"))
      }
    })

    # ---- Device (every camera/trap LOCATION; the bottom context field) ----
    # Replaces the old "No records": a trap with no captures still has activity records (checks), so a
    # location layer is the honest framing. Shows ALL device locations (camera = blue, trap = grey); the
    # Points above swallow any that have a record, so a bare dot = "device here, nothing of the group".
    all_dev_locs <- reactive({ req(active())
      dev_types <- if (is.null(device)) c("camera", "trap") else device
      dev_ds <- names(ik_data$datasets)[vapply(ik_data$datasets,
                  function(d) isTRUE(d$meta$source_type %in% dev_types), logical(1))]
      al <- ik_active_locations(ik_data)
      rsv <- .ik_nz(selection()$reserve)                       # scope the device context to the selected reserve(s),
      if (!is.null(rsv)) al <- al[al$reserve %in% rsv, , drop = FALSE]   # like Co-occurrence / Coverage — no stray off-reserve devices
      al[al$dataset %in% dev_ds & is.finite(al$latitude) & is.finite(al$longitude), , drop = FALSE]
    })
    observe({
      p <- proxy(); leaflet::clearGroup(p, dev_label)
      d <- all_dev_locs(); if (is.null(d) || !nrow(d)) return()
      col <- if (identical(src(), "trap")) "#8a8a8a" else "#2c7fb8"   # grey trap · blue camera
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = dev_label,
        radius = 3, fill = TRUE, fillColor = col, fillOpacity = 0.55, stroke = FALSE,   # no layerId: Points (same id) must win on top
        label = ~sprintf("%s — %s", name, if (identical(src(), "trap")) "trap" else "camera"),
        options = leaflet::pathOptions(pane = "device"))
    })

    observe({                                                   # Boundary — the CONSTANT reserve footprint (shared draw)
      p <- proxy(); leaflet::clearGroup(p, "Boundary")
      ik_add_reserve_boundary(p, reserve_hulls(), color = if (is_dark()) "#9ccc65" else "#2e7d32")
    })

    observe({                                                   # Selected highlight (drill)
      p <- proxy(); leaflet::clearGroup(p, "Selected")
      co <- sel_coords(selected()); if (is.null(co)) return()
      leaflet::addCircleMarkers(p, lng = co$lng, lat = co$lat, group = "Selected",
        radius = 16, fill = FALSE, stroke = TRUE, color = "#ff5722", weight = 3, opacity = 0.95, options = leaflet::pathOptions(pane = "selected"))
    })

    observe({                                                   # Legend
      p <- proxy(); leaflet::clearControls(p)
      line_grain <- src() == "camera" && grain_rv() == "line"
      d <- if (line_grain) line_metric() else rate_loc_pts()
      vc <- if (line_grain) "metric" else valcol()
      if (is.null(d) || !nrow(d)) return()
      cap <- .robust_cap(d[[vc]]); pf <- surf_pal(cap)
      ttl <- if (line_grain) sprintf("RAI / line &middot; %s", group_lab())
             else sprintf("%s &middot; %s", vallabel(), group_lab())
      leaflet::addLegend(p, "bottomright", pal = pf, values = pmin(d[[vc]], cap), title = ttl, opacity = 0.9)
    })

    observeEvent(input$map_marker_click, {
      cid <- input$map_marker_click$id
      if (is.null(cid)) return()
      if (startsWith(cid, "L|")) {                              # a LINE marker (camera line grain) → drill the table
        pr <- strsplit(cid, "|", fixed = TRUE)[[1]]
        selected(list(kind = "line", reserve = pr[2], line = pr[3], label = sprintf("Line %s · %s", pr[3], pr[2])))
        return()
      }
      # A LOCATION marker → open its records in the modal (NOT a hard filter of the table beside the map):
      # one record goes straight to its details, many open the Records list to pick from.
      o <- records_base(); if (is.null(o)) return()
      recs <- o[!is.na(o$location_id) & o$location_id == cid, , drop = FALSE]
      if (!nrow(recs)) return()
      locs <- ik_data$app$geography$locations
      maprec_loc(cid); .maprec_modal(locs$name[match(cid, locs$location_id)] %||% cid)
      if (nrow(recs) == 1) {                                    # single record → its details directly
        maprec_obs(recs$observationID[1]); updateTabsetPanel(session, "maprec_tabs", selected = "Record details")
      } else {                                                  # many → the Records list (click a row for details)
        maprec_obs(NULL); updateTabsetPanel(session, "maprec_tabs", selected = "Records")
      }
    })

    # ---- records table + CSV ----
    records_base <- reactive({ req(measure() == "rate", has_group())
      ik_metric_obs(ik_data, sel(), group_taxa(), group_lab(), source_type = src()) })
    records <- reactive({
      o <- records_base(); if (is.null(o)) return(NULL); sel <- selected(); if (is.null(sel)) return(o)
      if (identical(sel$kind, "location")) o[!is.na(o$location_id) & o$location_id == sel$id, , drop = FALSE]
      else o[!is.na(o$reserve) & !is.na(o$line) & o$reserve == sel$reserve & o$line == sel$line, , drop = FALSE]
    })

    output$drill_chip <- renderUI({
      sel <- selected(); if (is.null(sel)) return(NULL)
      div(class = "ik-maps-drill", icon("location-dot"), sprintf(" Showing: %s ", sel$label),
          actionLink(session$ns("drill_clear"), "show all", class = "ik-maps-drill-clear"))
    })
    observeEvent(input$drill_clear, selected(NULL))

    output$records_caption <- renderUI({
      if (show_lines()) {
        d <- line_metric(); n <- if (is.null(d)) 0L else nrow(d)
        tags$small(sprintf("%s camera line%s · RAI per line of %s. Click a line for its records (or zoom in for per-camera).",
                           format(n, big.mark = ","), if (n == 1L) "" else "s", group_lab()))
      } else {
        o <- records(); n <- if (is.null(o)) 0L else nrow(o); noun <- if (src() == "trap") "capture" else "detection"
        tags$small(sprintf("%s %s record%s of %s%s.", format(n, big.mark = ","), noun, if (n == 1L) "" else "s", group_lab(),
                           if (is.null(selected())) " in this selection" else " here"))
      }
    })

    output$table <- DT::renderDT({
      if (show_lines()) {                                     # zoomed out → per-LINE RAI; click a line to drill
        d <- line_metric(); validate(need(!is.null(d) && nrow(d), "No camera lines in the current selection."))
        d <- d[order(-d$metric), , drop = FALSE]; lines_tbl(d)
        df <- data.frame(Line = d$line, Reserve = d$reserve, RAI = round(d$metric, 2),
          Cameras = as.integer(d$n), Detections = as.integer(d$individuals),
          check.names = FALSE, stringsAsFactors = FALSE)
        return(DT::datatable(df, rownames = FALSE, selection = "single",
          class = "stripe hover row-border ik-row-click",
          options = list(pageLength = 7, scrollX = TRUE, dom = "ftip", order = list(list(2, "desc")))))
      }
      o <- records()
      validate(need(!is.null(o) && nrow(o), sprintf("No %ss of this group here.", if (src() == "trap") "capture" else "detection")))
      .records_dt(o, hover = TRUE, page = 7)
    })

    rec_proxy <- DT::dataTableProxy("table")

    # Records DT, shared by the table below the map (hover = TRUE → hovering a row shows that
    # location's popup on the map) and the records modal's "Records" tab. A hidden .loc column
    # carries the location_id; createdRow stashes it on the row + wires the hover input.
    .records_dt <- function(o, hover = FALSE, dom = "tip", page = 10) {
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      has_t <- format(o$when, "%H:%M:%S") != "00:00:00"
      when_lab <- ifelse(has_t, format(o$when, "%Y-%m-%d %H:%M"), format(o$when, "%Y-%m-%d"))
      # Count is folded into Species as " ×N", and only when >1 (≈95% are 1) — it frees a whole column for
      # the wrapping When/place columns without the noise of "×1" everywhere; the exact count still shows in
      # the record-details drill.
      sp <- ik_species_label(o$scientificName, ik_data, prefer)
      sp <- ifelse(!is.na(o$count) & o$count > 1, sprintf("%s ×%d", sp, as.integer(o$count)), sp)
      df <- data.frame(When = when_lab, Species = sp,
        Reserve = o$reserve, Line = o$line, Location = o$locationName,
        .loc = o$location_id, check.names = FALSE, stringsAsFactors = FALSE)
      # Device-specific column label (the map is device-locked): Camera / Trap, or Location if combined.
      names(df)[names(df) == "Location"] <-
        if (identical(src(), "trap")) "Trap" else if (identical(src(), "camera")) "Camera" else "Location"
      loc_i <- ncol(df) - 1L                                   # 0-based index of the hidden .loc column
      opts <- list(pageLength = page, scrollX = TRUE, dom = dom,
                   columnDefs = list(list(visible = FALSE, targets = loc_i)))
      if (hover) opts$createdRow <- DT::JS(sprintf(
        "function(row,data,i){var L=data[%d];row.setAttribute('data-loc',L);row.addEventListener('mouseenter',function(){Shiny.setInputValue('%s',L,{priority:'event'});});row.addEventListener('mouseleave',function(){Shiny.setInputValue('%s','',{priority:'event'});});}",
        loc_i, session$ns("table_hover"), session$ns("table_hover")))
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = opts)
    }

    # The records modal: "Records" (the records at the clicked location, or the current list) + "Record
    # details" (the clicked record, with a back link). `loc_name` titles it with the place when opened
    # from a marker; the Records tab is scoped to `maprec_loc()` (set by the marker click) via maprec_records.
    .maprec_modal <- function(loc_name = NULL) showModal(modalDialog(
      title = .ik_modal_title(loc_name %||% sprintf("%s records", if (src() == "trap") "Trap" else "Camera"),
                              sprintf("%s · click a record for its details, then Back for the list", group_lab())),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      tabsetPanel(id = session$ns("maprec_tabs"),
        tabPanel("Records",        icon = icon("list"),        DT::dataTableOutput(session$ns("maprec_table"))),
        tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("maprec_record"))))))
    # The records shown IN the modal: a clicked marker's location (from records_base, so it matches what the
    # click counted, independent of any line drill), else the current list (the side-table-row-click flow).
    maprec_records <- reactive({
      loc <- maprec_loc()
      if (is.null(loc)) return(records())
      o <- records_base(); if (is.null(o)) NULL else o[!is.na(o$location_id) & o$location_id == loc, , drop = FALSE]
    })

    # A clicked record row opens that modal on its detail tab (no map pan/zoom — that was
    # disorienting; hovering a row previews the location instead). Line-grain rows still drill.
    observeEvent(input$table_rows_selected, {
      i <- input$table_rows_selected
      if (show_lines()) {                                       # line-grain table: row → drill into that line
        d <- lines_tbl()
        if (length(i) && !is.null(d) && i <= nrow(d))
          selected(list(kind = "line", reserve = d$reserve[i], line = d$line[i],
                        label = sprintf("Line %s · %s", d$line[i], d$reserve[i])))
        DT::selectRows(rec_proxy, NULL); return()
      }
      o <- records(); if (!length(i) || is.null(o) || i > nrow(o)) return()
      maprec_loc(NULL)                                          # a row click → the full list in the modal, not a place
      maprec_obs(o$observationID[i]); .maprec_modal()
      updateTabsetPanel(session, "maprec_tabs", selected = "Record details")
      DT::selectRows(rec_proxy, NULL)
    }, ignoreInit = TRUE)

    output$maprec_table <- DT::renderDT({ o <- maprec_records(); validate(need(!is.null(o) && nrow(o), "No records.")); .records_dt(o, dom = "ftip") })
    observeEvent(input$maprec_table_rows_selected, {
      i <- input$maprec_table_rows_selected; o <- maprec_records()
      if (length(i) && !is.null(o) && i <= nrow(o)) {
        maprec_obs(o$observationID[i]); showTab(session = session, inputId = "maprec_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("maprec_table"), NULL)
    })
    observeEvent(input$maprec_back, updateTabsetPanel(session, input$maprec_back$tabset, selected = input$maprec_back$to))
    output$maprec_record <- renderUI({
      if (is.null(maprec_obs())) return(tags$p(class = "ik-maps-hint", "Pick a record from the Records tab."))
      ob <- ik_observation(ik_data, maprec_obs()); if (is.null(ob)) return(tags$p("Record not found."))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      tagList(.ik_tab_back(session$ns("maprec_back"), "maprec_tabs", "Records", "Back to records"),
              .ovw_title(ik_data, ob, prefer),
              .ovw_tabs(ik_data, ob, prefer, tabset_id = session$ns("maprec_subtabs")))
    })

    # ONE hover preview, driven from EITHER side: hovering a record row OR hovering its marker shows the
    # same compact popup at the location (the table_hover input carries a location_id from the row; the
    # marker mouseover carries the marker's layerId). Debounced so fast passes don't spam the server.
    .show_hover <- function(p, loc) {
      leaflet::clearGroup(p, "Hover")
      if (is.null(loc) || !nzchar(loc) || show_lines()) return()
      d <- rate_loc_pts(); if (is.null(d)) return()
      r <- d[d$location_id == loc, , drop = FALSE]
      if (!nrow(r) || !is.finite(r$longitude[1])) return()
      # className → pointer-events:none (maps.css): the preview must not capture the mouse, or the bubble
      # appearing over a hovered marker steals the hover (mouseout↔mouseover flicker) and the click.
      leaflet::addPopups(p, lng = r$longitude[1], lat = r$latitude[1], popup = hover_popup(r[1, , drop = FALSE]),
        group = "Hover", options = leaflet::popupOptions(closeButton = FALSE, autoPan = FALSE, className = "ik-hover-popup"))
    }
    hover_loc <- shiny::debounce(reactive(input$table_hover), 120)
    observeEvent(hover_loc(), .show_hover(proxy(), hover_loc()), ignoreNULL = FALSE)   # from the table row
    marker_over <- shiny::debounce(reactive(input$map_marker_mouseover), 90)
    observeEvent(marker_over(), {                                                       # from the marker
      ev <- marker_over(); loc <- if (is.null(ev)) NULL else ev$id
      if (!is.null(loc) && startsWith(loc, "L|")) loc <- NULL    # line markers aren't location previews
      .show_hover(proxy(), loc)
    }, ignoreNULL = FALSE)
    observeEvent(input$map_marker_mouseout, leaflet::clearGroup(proxy(), "Hover"), ignoreInit = TRUE)

    output$unplaced <- renderUI({
      u <- if (src() == "trap") trap_unplaced() else cam_unplaced()
      if (is.null(u) || !nrow(u)) return(NULL)
      div(class = "ik-maps-unplaced", icon("triangle-exclamation"),
          sprintf(" %s coordless %s not shown on the map (no location fix).", format(nrow(u), big.mark = ","), if (src() == "trap") "traps" else "sites"))
    })

    output$download_csv <- downloadHandler(
      filename = function() sprintf("maps-%s-%s-%s-%s.csv", src(), measure(), gsub("[^A-Za-z0-9]+", "-", tolower(group_lab())), Sys.Date()),
      content = function(file) {
        d <- records(); if (is.null(d)) d <- data.frame(); utils::write.csv(d, file, row.names = FALSE)
      }
    )
  })
}
