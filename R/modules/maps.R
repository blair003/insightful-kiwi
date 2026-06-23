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
#   • Trap / "Servicing health" — per-trap glyph: size = trap-nights, fill/ring = check-frequency
#     status (good/watch/neglected) from ik_trap_review(). Two honest channels.
# Robust colour/size: per-location colour + radius are clamped at the 95th percentile so a single
# low-effort outlier (e.g. 1 capture in 7 trap-days → 14.29/100td) can't flatten the rest.
# Rendering: one base render (panes + preferCanvas + theme basemap), then per-layer leafletProxy
# observers. Spatial maths in R/functions/spatial.R; per-location/line values in metrics.R.

MAPS_LINE_ZOOM <- 13   # at/above this zoom, camera activity shows per-camera; below, per-line
.MAPS_CAP_PCTL <- 0.95 # colour/size clamp percentile (robust to low-effort outliers)

.MAPS_STATUS <- list(
  light = c(good = "#2e7d32", watch = "#f9a825", neglected = "#c62828", insufficient_data = "#7e57c2"),
  dark  = c(good = "#66bb6a", watch = "#fdd835", neglected = "#ef5350", insufficient_data = "#b39ddb"))
.MAPS_STATUS_LEVELS <- c("good", "watch", "neglected")

# Measures per device — the map is instantiated device-locked (a camera "Monitoring map" + a trap
# "Trapping map"), so the device toggle is dropped and the measures come from here.
.MAPS_MEASURES <- list(
  camera = c("Relative activity" = "rate", "Priority (predator vs protected)" = "priority",
             "Timing (predator ↔ protected)" = "timing"),
  trap   = c("Captures" = "captures", "Capture rate" = "rate", "Servicing health" = "servicing"))

#' The "Measure" help body, written for the map's OWN device (each map is device-locked, so the
#' camera map shouldn't explain trap measures or vice versa). NULL device → both (combined map).
#' @keywords internal
.maps_measure_help <- function(device = NULL, norm = 100) {
  ntn <- paste0(format(norm, big.mark = ","), " trap-nights")
  cam <- list(
    tags$p("What the camera markers and shaded surface show — switch with the ", tags$b("Measure"), " buttons."),
    tags$p(tags$b("Relative activity (RAI)"), " — detection rate for the chosen species group, effort-adjusted ",
      "by camera-hours. Zoom ", tags$b("out"), " for a line-level RAI (the DOC protocol's per-line index); ",
      "zoom ", tags$b("in"), " for the per-camera rate. The shaded surface interpolates it between cameras."),
    tags$p(tags$b("Priority (predator vs protected)"), " — a composite ", tags$em("where to act"), " score: ",
      "high (red) where the chosen ", tags$b("predator"), " is active ", tags$b("and"), " the chosen ",
      tags$b("protected"), " species is scarce (predator RAI high × protected RAI low). Exploratory — click a ",
      "row in the table for the detections behind it."),
    tags$p(tags$b("Timing (predator ↔ protected)"), " — per camera, the ", tags$b("median time"), " between a ",
      "protected-species detection and the nearest ", tags$b("predator"), " detection. Red = they use the same ",
      "ground close in time; the popup shows the real median gap. Exploratory."))
  trp <- list(
    tags$p("How each trap is coloured and sized — switch with the ", tags$b("Measure"), " buttons."),
    tags$p(tags$b("Captures"), " — total predators of the chosen group caught per trap in the selected period. ",
      "Raw counts: catches are sparse, so a count reads more honestly than a rate at low effort."),
    tags$p(tags$b("Capture rate"), " — captures per ", tags$b(ntn), " (effort-adjusted), so heavily- ",
      "and lightly-checked traps compare fairly. Colour and size are clamped at the 95th percentile so one ",
      "low-effort outlier (e.g. 1 catch in a few trap-nights) can't flatten the rest."),
    tags$p(tags$b("Servicing health"), " — each trap judged ", tags$b("as of the period end"), " by its gap ",
      "since last check — ", tags$b("good"), " / ", tags$b("watch"), " / ", tags$b("neglected"), " (including ",
      "never checked); too few checks to judge a cadence → ", tags$b("insufficient data"), ". The buckets are ",
      "calibrated to ", tags$b("each reserve's own"), " typical check cadence, not one global yardstick. Sized ",
      "by days since checked (bigger = staler); ", tags$b("dormant"), " (6 mo+) and ", tags$b("historic"),
      " (12 mo+) traps are faint — toggle ", tags$b("No records"), " to see them."))
  if (identical(device, "camera"))    do.call(tagList, cam)
  else if (identical(device, "trap")) do.call(tagList, trp)
  else tagList(tags$h6("Camera"), do.call(tagList, cam), tags$h6("Traps"), do.call(tagList, trp))
}

#' Choices + defaults for the in-panel Species / Predator(s) / Protected selects. Shared by maps_ui
#' (BAKED into the UI so the default shows — dropdown nav panels render their selectize lazily, so a
#' server-side updateSelectInput at startup misses them) and maps_server (relabels on name pref).
#' @keywords internal
.maps_picker_defs <- function(ik_data, prefer = "vernacular") {
  sg <- ik_species_groups(ik_data)
  groups <- c(ik_taxa_groups(sg, "monitor", "target"), ik_taxa_groups(sg, "monitor", "interesting"))
  rt <- function(role) { l <- unique(sg$label[sg$role == role & !is.na(sg$monitor)])
    stats::setNames(lapply(l, function(x) sg$scientificName[sg$label == x & !is.na(sg$scientificName)]), l) }
  pred_taxa <- rt("predator"); prot_taxa <- rt("protected"); splits <- unique(sg$label[which(sg$split)])
  gf <- function(t, w) paste0("grp:", if (w %in% names(t)) w else names(t)[1])
  list(species = ik_species_choices_full(ik_data, prefer),
       predators = ik_species_choices(pred_taxa, ik_data, prefer, splits),
       protected = ik_species_choices(prot_taxa, ik_data, prefer, splits),
       group_default = gf(groups, "Mustelids"), pred_default = gf(pred_taxa, "Mustelids"),
       prot_default = gf(prot_taxa, "Kiwi"),
       pred_taxa = pred_taxa, prot_taxa = prot_taxa, groups = groups, splits = splits)
}

#' Maps nav panel UI.
#' @param id Module id.
#' @return A bslib nav_panel for page_navbar().
maps_ui <- function(id, device = NULL, label = "Map", value = "map", ik_data = NULL) {
  ns <- NS(id)
  meas <- .MAPS_MEASURES[[device %||% "camera"]]
  pk   <- .maps_picker_defs(ik_data)                            # baked picker defaults (dropdown-lazy-safe)
  norm <- (ik_data$meta$trapping$rate %||% list())$norm_trap_days %||% 100   # rate normalisation (config)
  nav_panel(
    label, value = value, icon = icon("map"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),
    tags$script(src = .ik_asset("js/maps.js")),
    div(
      class = "ik-maps",
      div(
        class = "ik-maps-controls",
        if (is.null(device))                                    # device-locked instances drop the toggle
          radioButtons(ns("source"), "Device",
                       choices = c(Camera = "camera", Trap = "trap"), selected = "camera", inline = TRUE),
        div(class = "ik-maps-measure",
          radioButtons(ns("measure"), tagList("Measure", .ik_info(ns("measure_help"),
            if (identical(device, "trap")) "Trapping map — measures"
            else if (identical(device, "camera")) "Monitoring map — measures" else "Map measures",
            .maps_measure_help(device, norm))),
            choices = meas, selected = unname(meas)[1], inline = TRUE)),
        # Species sits with the other map controls (it's a GROUPING choice, like Device/Measure) and only
        # for the measures that group by it (camera RAI, trap captures/rate); priority & timing instead
        # use their own predator/protected pickers, servicing ignores species — so it's hidden for those.
        conditionalPanel("input.measure == 'rate' || input.measure == 'captures'", ns = ns,
          selectInput(ns("species"), "Species", choices = pk$species, selected = pk$group_default, multiple = TRUE)),
        conditionalPanel("input.measure == 'priority' || input.measure == 'timing'", ns = ns,
          selectInput(ns("predators"), "Predator(s)", choices = pk$predators, selected = pk$pred_default, multiple = TRUE),
          selectInput(ns("protected"), "Protected",   choices = pk$protected, selected = pk$prot_default, multiple = TRUE)),
        conditionalPanel("input.measure == 'timing'", ns = ns,
          div(class = "ik-maps-measure",
            checkboxInput(ns("after_only"),
              tagList("Predator ", tags$b("after"), " only ", tags$span(class = "ik-maps-hint", "(stalking)")),
              value = FALSE)))
      ),
      uiOutput(ns("unplaced")),
      leaflet::leafletOutput(ns("map"), height = "70vh"),
      div(
        class = "ik-maps-records",
        uiOutput(ns("drill_chip")),
        div(class = "ik-maps-records-header",
            uiOutput(ns("records_caption")),
            downloadButton(ns("download_csv"), "Download CSV", class = "btn-sm")),
        DT::DTOutput(ns("table"))
      )
    )
  )
}

#' Maps server.
#' @param id Module id. @param ik_data The ik_data container.
#' @param prefer_scientific Reactive: TRUE to show scientific names.
#' @param selection Reactive selection SPEC. @param color_mode Reactive theme ("light"/"dark").
#' @param active Accepted for symmetry with the other tab modules, but the map layers no longer gate
#'   on it: the leaflet widget renders from load (suspendWhenHidden = FALSE) and its layers populate
#'   like the records table, so a map on a not-yet-viewed tab is already drawn. Gating on it left the
#'   map blank when this instance's tab wasn't the server-side "current" nav value.
maps_server <- function(id, ik_data, prefer_scientific, selection, color_mode = reactive("light"),
                        device = NULL, active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {

    prefer <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")

    observeEvent(input$source, {
      ch  <- if (identical(input$source, "trap"))
               c("Captures" = "captures", "Capture rate" = "rate", "Servicing health" = "servicing")
             else c("Relative activity" = "rate", "Priority (predator vs protected)" = "priority",
                    "Timing (predator ↔ protected)" = "timing")
      sel <- if (isTRUE(isolate(input$measure) %in% ch)) isolate(input$measure) else ch[[1]]
      updateRadioButtons(session, "measure", choices = ch, selected = sel, inline = TRUE)
    }, ignoreInit = TRUE)

    src      <- reactive(device %||% input$source %||% "camera")   # device-locked when set
    measure  <- reactive(input$measure %||% unname(.MAPS_MEASURES[[device %||% "camera"]])[1])
    # Priority composite (camera only): predators-high × kiwi-low → "where to act". Predators are the
    # predator-role groups, kiwi the protected-role group (both as seen on camera).
    is_priority <- reactive(src() == "camera" && measure() == "priority")
    is_timing   <- reactive(src() == "camera" && measure() == "timing")
    has_pred <- reactive(length(input$predators) > 0)   # predator/protected pickers actually populated?
    has_prot <- reactive(length(input$protected) > 0)   # (unselecting all → empty state, not stale default)
    # all trap locations that exist in the active datasets (across ALL seasons) — the denominator the
    # servicing caption cites so the per-period count isn't mistaken for the whole network.
    n_trap_total <- reactive({ dp <- ik_deployment_period(ik_data)
      length(unique(dp$locationID[!is.na(dp$source_type) & dp$source_type == "trap"])) })
    # Composite = predator-role vs protected-role groups (split-aware), with Mustelids/Kiwi defaults.
    # Same picker defs the UI bakes in; the observe below only RELABELS on the name preference.
    pk <- .maps_picker_defs(ik_data)
    pred_taxa <- pk$pred_taxa; prot_taxa <- pk$prot_taxa; splits <- pk$splits
    .group_default <- pk$group_default; .pred_default <- pk$pred_default; .prot_default <- pk$prot_default
    observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      # All three taxa pickers live in-panel (Species is a grouping choice, like Device/Measure).
      # Species offers the full set (groups, split per project flag, + every ungrouped species);
      # predator/protected stay role-scoped. Defaults: Species/predator = Mustelids, protected = Kiwi.
      updateSelectInput(session, "species",   choices = ik_species_choices_full(ik_data, p),                 selected = keep(isolate(input$species), .group_default))
      updateSelectInput(session, "predators", choices = ik_species_choices(pred_taxa, ik_data, p, splits),    selected = keep(isolate(input$predators), .pred_default))
      updateSelectInput(session, "protected", choices = ik_species_choices(prot_taxa, ik_data, p, splits),    selected = keep(isolate(input$protected), .prot_default))
    })
    # The map surface's taxa come from the in-panel Species pick (input$species), combined into one
    # labelled surface; default to the top monitor-target group. (The map uses Species as a GROUPING,
    # never as a spec filter — the dual role, like device.)
    group_taxa <- reactive({ v <- input$species; if (!length(v)) v <- .group_default
      sci <- ik_resolve_species_choice(v, ik_group_taxa(ik_data)); if (!length(sci)) return(NULL)
      stats::setNames(list(sci), paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ")) })
    group_lab  <- reactive({ g <- group_taxa(); if (is.null(g)) "group" else names(g) })
    # Species never enters the metrics spec (the map groups by it instead), so the selection spec is used
    # as-is; defensively drop any stray species axis so priority/timing can't double-filter to it.
    sel <- reactive({ s <- selection(); s$species <- NULL; s })
    pred_label <- reactive({ v <- input$predators; if (!length(v)) v <- .pred_default; paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })
    prot_label <- reactive({ v <- input$protected; if (!length(v)) v <- .prot_default; paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })
    is_dark  <- reactive(identical(color_mode(), "dark"))
    halo     <- reactive(if (is_dark()) "#1a1a1a" else "#ffffff")
    per_cam  <- reactive((ik_data$meta$camera$rai %||% list())$camera_hours %||% 500)
    norm     <- ik_data$meta$trapping$rate$norm_trap_days %||% 100   # trap rate normalisation (config)
    has_group <- reactive(!is.null(group_taxa()))
    # the per-location value column + its label, for the active rate/count measure
    valcol   <- reactive(if (src() == "trap" && measure() == "captures") "captures" else "metric")
    vallabel <- reactive(if (is_priority()) "Priority"
                         else if (is_timing()) "Predator proximity"
                         else if (src() == "trap" && measure() == "captures") "Captures"
                         else if (src() == "trap") "Capture rate"
                         else "Detections / deployment")

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
    cam_all <- reactive({ req(src() == "camera", has_group())
      ik_location_metric(ik_data, sel(), group_taxa(), "camera", norm = per_cam()) })
    cam_pts      <- reactive({ m <- cam_all(); if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
    cam_unplaced <- reactive({ m <- cam_all(); if (is.null(m)) NULL else m[!(is.finite(m$latitude) & is.finite(m$longitude)), , drop = FALSE] })

    # Priority = norm(predator RAI) × (1 − norm(kiwi RAI)) per camera; both metrics share the
    # deployed-camera set (0 where none), so they join on location_id. Normalised across the
    # selection's cameras; high only where predators are high AND kiwi low.
    prio_all <- reactive({ req(is_priority(), has_pred(), has_prot())   # empty pickers → NULL (empty state)
      preds <- input$predators; prots <- input$protected
      pred <- ik_location_metric(ik_data, sel(), list(Predators = ik_resolve_species_choice(preds, pred_taxa)), "camera", norm = per_cam())
      prot <- ik_location_metric(ik_data, sel(), list(Protected = ik_resolve_species_choice(prots, prot_taxa)), "camera", norm = per_cam())
      if (is.null(pred) || !nrow(pred)) return(NULL)
      m <- pred; m$predator <- m$metric
      m$protected <- if (is.null(prot)) 0 else prot$metric[match(m$location_id, prot$location_id)]
      m$protected[is.na(m$protected)] <- 0
      pmx <- function(x) { v <- suppressWarnings(max(x[x > 0], na.rm = TRUE)); if (is.finite(v)) v else 1 }
      m$metric <- (m$predator / pmx(m$predator)) * (1 - m$protected / pmx(m$protected))
      m })
    prio_pts      <- reactive({ m <- prio_all(); if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
    prio_unplaced <- reactive({ m <- prio_all(); if (is.null(m)) NULL else m[!(is.finite(m$latitude) & is.finite(m$longitude)), , drop = FALSE] })

    # Timing layer (camera only): per-camera median gap between a protected detection and the nearest
    # predator detection at that camera (ik_predator_protected_gaps, grouped by camera). Mapped as a
    # "proximity" score — metric = 1/(1+gap_days), so SHORT gaps (predators & protected sharing the
    # ground close in time = act) score HIGH and read red, mirroring the priority layer's high=act.
    # The popup carries the real median gap + pair count. Same predator/protected selectors as priority.
    timing_all <- reactive({ req(is_timing(), has_pred(), has_prot())   # empty pickers → NULL (empty state)
      preds <- input$predators; prots <- input$protected
      g <- ik_predator_protected_gaps(ik_data, ik_resolve_species_choice(preds, pred_taxa),
                                      ik_resolve_species_choice(prots, prot_taxa), .ik_nz(selection()$season))
      if (is.null(g) || !nrow(g)) return(NULL)
      if (isTRUE(input$after_only)) { g <- g[g$signed_h > 0, , drop = FALSE]; if (!nrow(g)) return(NULL) }  # stalking
      agg <- do.call(rbind, lapply(split(g, g$location_id), function(s)
        data.frame(location_id = s$location_id[1], median_gap_h = stats::median(s$gap_h), n = nrow(s),
                   stringsAsFactors = FALSE)))
      locs <- ik_data$app$geography$locations; mi <- match(agg$location_id, locs$location_id)
      agg$name <- locs$name[mi]; agg$reserve <- locs$reserve[mi]; agg$line <- locs$line[mi]
      agg$latitude <- locs$latitude[mi]; agg$longitude <- locs$longitude[mi]
      agg$median_gap_d <- agg$median_gap_h / 24
      agg$metric <- 1 / (1 + agg$median_gap_d)            # high = close in time = act (red)
      agg })
    timing_pts      <- reactive({ m <- timing_all(); if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
    timing_unplaced <- reactive({ m <- timing_all(); if (is.null(m)) NULL else m[!(is.finite(m$latitude) & is.finite(m$longitude)), , drop = FALSE] })

    line_metric <- reactive({ req(src() == "camera", has_group())
      rr <- ik_rai(ik_data, sel(), group_taxa()); ln <- rr$lines
      if (is.null(ln) || !nrow(ln)) return(NULL)
      ce <- ik_group_centroids(cam_pts(), c("reserve", "line")); if (is.null(ce)) return(NULL)
      m <- merge(ln, ce, by = c("reserve", "line")); if (nrow(m)) m else NULL })

    trap_all <- reactive({ req(src() == "trap", measure() %in% c("captures", "rate"), has_group())
      ik_location_metric(ik_data, sel(), group_taxa(), "trap") })
    trap_pts      <- reactive({ m <- trap_all(); if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
    trap_unplaced <- reactive({ m <- trap_all(); if (is.null(m)) NULL else m[!(is.finite(m$latitude) & is.finite(m$longitude)), , drop = FALSE] })

    serv_all <- reactive({ req(src() == "trap", measure() == "servicing")
      spec <- selection()
      tr <- ik_trap_review(ik_data, seasons = .ik_nz(spec$season))
      if (is.null(tr) || !nrow(tr)) return(NULL)
      locs <- ik_data$app$geography$locations
      tr$dataset <- locs$dataset[match(tr$location, locs$location_id)]
      active <- ik_active_datasets()
      if (!is.null(active))                tr <- tr[tr$dataset %in% active, , drop = FALSE]
      if (!is.null(.ik_nz(spec$reserve)))  tr <- tr[tr$reserve  %in% spec$reserve,  , drop = FALSE]
      if (!is.null(.ik_nz(spec$line)))     tr <- tr[tr$line     %in% spec$line,     , drop = FALSE]
      if (!is.null(.ik_nz(spec$location))) tr <- tr[tr$location %in% spec$location, , drop = FALSE]
      if (nrow(tr)) tr else NULL })
    .SERV_FAINT <- c("dormant", "historic")               # not in service this period → faint layer
    serv_pts      <- reactive({ tr <- serv_all(); if (is.null(tr)) return(NULL)    # active, mapped
      d <- tr[!tr$status %in% .SERV_FAINT & is.finite(tr$latitude) & is.finite(tr$longitude), , drop = FALSE]; if (nrow(d)) d else NULL })
    serv_unplaced <- reactive({ tr <- serv_all(); if (is.null(tr)) return(NULL)    # active, no coords
      d <- tr[!tr$status %in% .SERV_FAINT & !(is.finite(tr$latitude) & is.finite(tr$longitude)), , drop = FALSE]; if (nrow(d)) d else NULL })
    # Faint "not in service" layer = DORMANT/historic traps for this period (gap since last check ≥
    # the dormancy/historic windows — see ik_trap_review). Skipped-but-active traps are red in serv_pts.
    serv_absent <- reactive({ tr <- serv_all(); if (is.null(tr)) return(NULL)
      d <- tr[tr$status %in% .SERV_FAINT & is.finite(tr$latitude) & is.finite(tr$longitude), , drop = FALSE]; if (nrow(d)) d else NULL })

    rate_loc_pts <- reactive(if (is_priority()) prio_pts() else if (is_timing()) timing_pts()
                             else if (src() == "trap") trap_pts() else cam_pts())
    frame_pts    <- reactive(if (src() == "trap" && measure() == "servicing") serv_pts() else rate_loc_pts())

    # ---- helpers ----
    .robust_cap  <- function(v) ik_robust_cap(v, .MAPS_CAP_PCTL)      # shared impl in spatial.R
    .area_radius <- function(v, lo, hi) ik_marker_radius(v, lo, hi)
    .prio_ramp <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")   # yellow→red — priority/danger
    surf_pal <- function(cap) leaflet::colorNumeric(if (is_priority() || is_timing()) .prio_ramp else "viridis", c(0, cap))
    # median gap (hours) → readable "Xh / Xd / Xmo" — VECTORISED (leaflet builds every popup at once)
    .fmt_gap_lab <- function(h) ifelse(!is.finite(h), "—",
      ifelse(h < 48, sprintf("%.0f h", h),
        ifelse(h < 720, sprintf("%.1f d", h / 24), sprintf("%.1f mo", h / 720))))
    timing_popup <- function(d) sprintf(
      "<b>%s</b><br/>Line %s &middot; %s<br/><b>Median gap: %s</b><br/>%s &harr; %s &middot; %d pair%s",
      d$name, ifelse(is.na(d$line), "—", d$line), d$reserve, .fmt_gap_lab(d$median_gap_h),
      prot_label(), pred_label(), as.integer(d$n), ifelse(d$n == 1, "", "s"))
    prio_popup <- function(d) sprintf(
      "<b>%s</b><br/>Line %s &middot; %s<br/><b>Priority: %.2f</b><br/>%s RAI: %.2f &middot; %s RAI: %.2f",
      d$name, ifelse(is.na(d$line), "—", d$line), d$reserve, d$metric, pred_label(), d$predator, prot_label(), d$protected)
    rate_popup <- function(d) {
      if (src() == "trap") {
        head  <- if (measure() == "captures") sprintf("Captures: %d", as.integer(d$captures)) else sprintf("Capture rate: %.2f / %s trap-nights", d$metric, format(norm, big.mark = ","))
        extra <- sprintf("Captures: %d &middot; Trap-days: %s", as.integer(d$captures), format(round(d$trap_days), big.mark = ","))
      } else {
        head  <- sprintf("Detections / deployment: %.2f", d$metric)
        extra <- sprintf("Detections: %d &middot; Camera-hours: %s", as.integer(d$individuals), format(round(d$camera_hours), big.mark = ","))
      }
      sprintf("<b>%s</b><br/>Line %s &middot; %s<br/>%s<br/>%s", d$name, ifelse(is.na(d$line), "—", d$line), d$reserve, head, extra)
    }
    line_popup <- function(d) sprintf(
      "<b>Line %s</b> &middot; %s<br/>RAI: %.2f (per 2000 CH)<br/>%d cameras &middot; %d detections", d$line, d$reserve, d$metric, as.integer(d$n), as.integer(d$individuals))
    serv_popup <- function(d) {
      overdue <- as.integer(difftime(max(d$last_check, na.rm = TRUE), d$last_check, units = "days"))
      sprintf(paste0("<b>%s</b><br/>Line %s &middot; %s<br/><b>Servicing: %s</b><br/>Checks: %d &middot; mean interval %.0f d<br/>",
                     "Trap-days: %s<br/>Last check %s (%d d ago)<br/>Captures: %d"),
              d$name, ifelse(is.na(d$line), "—", d$line), d$reserve, toupper(as.character(d$status)),
              d$n_checks, d$mean_interval_days, format(round(d$trap_days), big.mark = ","), format(d$last_check, "%Y-%m-%d"), overdue, as.integer(d$captures))
    }
    proxy <- function() leaflet::leafletProxy("map", session)

    # ---- drill state (map ↔ table) ----
    selected <- reactiveVal(NULL)   # NULL | list(kind="location"/"line", id|reserve|line, label)
    prio_tbl    <- reactiveVal(NULL)  # the priority rows currently shown (ordered) — for the row drill
    prio_rec    <- reactiveVal(NULL)  # predator+protected detections behind a clicked priority camera
    prio_recobs <- reactiveVal(NULL)  # the record open in the priority modal's Record-details tab
    maprec_obs  <- reactiveVal(NULL)  # the record open in the camera/trap records modal's detail tab
    # reset the drill when the data context changes (a clicked place may not exist in the new view)
    observeEvent(list(src(), measure(), input$species, input$predators, input$protected, input$after_only, selection()), selected(NULL), ignoreInit = TRUE)
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
      pns <- c("surface", "boundary", "heat", "zeros", "points", "selected")
      for (pn in pns) m <- leaflet::addMapPane(m, pn, zIndex = 400 + 10 * match(pn, pns))
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
        overlayGroups = c("Surface", "Points", "No records", "Boundary", "Activity"),
        options = leaflet::layersControlOptions(collapsed = FALSE))
      m <- leaflet::hideGroup(m, "Activity")
      if (nrow(locs)) m <- leaflet::fitBounds(m, min(locs$longitude), min(locs$latitude), max(locs$longitude), max(locs$latitude))
      m
    })
    # Render the map even while its tab is hidden. Maps live in dropdown nav panels that aren't the
    # landing tab, so without this the leaflet widget isn't created until first view — and the layer
    # observers below push their markers/surface via leafletProxy to a map that doesn't exist yet, so
    # they're dropped and the map shows blank (records table still fills, since it isn't proxy-based).
    # With the widget present from load, every proxy update lands; maps.js re-fits/sizes it on show.
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

    # ---- Surface (rate/count field; robust clamp) ----
    # IDW interpolation is the heaviest map computation. Cache it on the points + value column (the
    # points already encode the selection + active datasets, so they fully determine the surface);
    # revisiting a view — or another session on the same selection — then skips the recompute. The
    # theme is deliberately NOT in the key: it only affects the redraw below, not the interpolation.
    surface_idw <- reactive({
      if (src() == "trap" && measure() == "servicing") return(NULL)
      d <- rate_loc_pts(); if (is.null(d) || !nrow(d)) return(NULL)
      ik_idw_surface(d, valcol(), "reserve")
    }) |> bindCache(rate_loc_pts(), valcol())
    observe({
      p <- proxy(); leaflet::clearGroup(p, "Surface")
      s <- surface_idw(); if (is.null(s) || !nrow(s)) return()
      cap <- .robust_cap(s$predicted); pf <- surf_pal(cap)
      leaflet::addPolygons(p, data = s, group = "Surface", weight = 0.5, color = ~pf(pmin(predicted, cap)),
        fillColor = ~pf(pmin(predicted, cap)), fillOpacity = if (is_dark()) 0.5 else 0.4,
        options = leaflet::pathOptions(pane = "surface"))
    })

    # ---- Points (line RAI · per-camera rate · trap count/rate · servicing) ----
    observe({
      p <- proxy(); leaflet::clearGroup(p, "Points")
      if (src() == "trap" && measure() == "servicing") {
        d <- serv_pts(); if (is.null(d) || !nrow(d)) return()
        cols <- if (is_dark()) .MAPS_STATUS$dark else .MAPS_STATUS$light; stc <- as.character(d$status)
        sz  <- pmin(d$mean_interval_days, .robust_cap(d$mean_interval_days))   # size by STALENESS, capped
        lbl <- c(good = "GOOD", watch = "WATCH", neglected = "NEGLECTED", insufficient_data = "INSUFFICIENT DATA")[stc]
        leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Points", layerId = d$location,
          radius = .area_radius(sz, 6, 20), fillColor = unname(cols[stc]),
          fillOpacity = unname(c(good = 0.5, watch = 0.72, neglected = 0.92, insufficient_data = 0.6)[stc]),  # worst = boldest
          stroke = TRUE, color = unname(cols[stc]), weight = unname(c(good = 1, watch = 2, neglected = 3, insufficient_data = 1.5)[stc]),
          label = ifelse(d$n_checks == 0, sprintf("%s — NEGLECTED · no check this period", d$name),
                         sprintf("%s — %s · %.0f d since checked", d$name, lbl, d$mean_interval_days)),
          popup = serv_popup(d), popupOptions = leaflet::popupOptions(autoPan = FALSE),
          options = leaflet::pathOptions(pane = "points"))
      } else if (src() == "camera" && !is_priority() && !is_timing() && grain_rv() == "line") {
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
        leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Points", layerId = d$location_id,
          radius = .area_radius(v, 5, 20), fillColor = pf(v), fillOpacity = 0.8, stroke = TRUE, color = halo(), weight = 1.5,
          label = sprintf("%s — %s %s", d$name, vallabel(), formatC(d[[vc]], format = "fg", digits = 3)),
          popup = if (is_priority()) prio_popup(d) else if (is_timing()) timing_popup(d) else rate_popup(d),
          popupOptions = leaflet::popupOptions(autoPan = FALSE),
          options = leaflet::pathOptions(pane = "points"))
      }
    })

    # ---- No records (rate/count sites with effort but zero of the group; servicing: no check this period) ----
    observe({
      p <- proxy(); leaflet::clearGroup(p, "No records")
      if (src() == "trap" && measure() == "servicing") {        # servicing: faint = inactive/dormant traps
        d <- serv_absent(); if (is.null(d) || !nrow(d)) return()
        leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "No records", layerId = d$location,
          radius = 3, fill = FALSE, stroke = TRUE, weight = 1, color = if (is_dark()) "#8a8a8a" else "#9a9a9a", opacity = 0.4,
          label = sprintf("%s — dormant (last checked %s)", d$name,
                          ifelse(is.finite(d$last_check), format(d$last_check, "%d %b %Y"), "—")),
          options = leaflet::pathOptions(pane = "zeros"))
        return()
      }
      if (is_priority() || is_timing()) return()               # composite 0/score is not "no records"
      if (src() == "camera" && grain_rv() == "line") return()
      d <- rate_loc_pts(); d <- if (is.null(d)) NULL else d[d$metric == 0, , drop = FALSE]
      if (is.null(d) || !nrow(d)) return()
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "No records", layerId = d$location_id,
        radius = 3, fill = FALSE, stroke = TRUE, weight = 1, color = if (is_dark()) "#8a8a8a" else "#9a9a9a", opacity = 0.4,
        label = sprintf("%s — no records", d$name), options = leaflet::pathOptions(pane = "zeros"))
    })

    observe({                                                   # Boundary — names the reserve on hover of the line
      p <- proxy(); leaflet::clearGroup(p, "Boundary")
      h <- ik_selection_hulls(frame_pts(), "reserve"); if (is.null(h) || !nrow(h)) return()
      # Outline only (fill = FALSE), so the reserve label fires only when you hover the dashed line —
      # not the interior gaps between markers (same behaviour as the Coverage map).
      leaflet::addPolygons(p, data = h, group = "Boundary", label = ~reserve,
        labelOptions = leaflet::labelOptions(textsize = "12px", direction = "auto", sticky = TRUE),
        highlightOptions = leaflet::highlightOptions(weight = 3.5, color = "#1565c0", bringToFront = TRUE),
        fill = FALSE, color = if (is_dark()) "#9ccc65" else "#2e7d32", weight = 2, dashArray = "5,5",
        options = leaflet::pathOptions(pane = "boundary"))
    })

    observe({                                                   # Activity heatmap (default off)
      p <- proxy(); leaflet::clearGroup(p, "Activity")
      d <- frame_pts(); if (is.null(d) || !nrow(d)) return()
      intensity <- if (src() == "trap" && measure() == "servicing") d$trap_days else d[[valcol()]]
      keep <- is.finite(intensity) & intensity > 0; if (!any(keep)) return()
      leaflet.extras::addHeatmap(p, lng = d$longitude[keep], lat = d$latitude[keep], intensity = intensity[keep],
        group = "Activity", blur = 22, radius = 16, max = max(intensity[keep], na.rm = TRUE), minOpacity = 0.3)
    })

    observe({                                                   # Selected highlight (drill)
      p <- proxy(); leaflet::clearGroup(p, "Selected")
      co <- sel_coords(selected()); if (is.null(co)) return()
      leaflet::addCircleMarkers(p, lng = co$lng, lat = co$lat, group = "Selected",
        radius = 16, fill = FALSE, stroke = TRUE, color = "#ff5722", weight = 3, opacity = 0.95, options = leaflet::pathOptions(pane = "selected"))
    })

    observe({                                                   # Legend
      p <- proxy(); leaflet::clearControls(p)
      if (src() == "trap" && measure() == "servicing") {
        d <- serv_pts(); if (is.null(d) || !nrow(d)) return()
        cols <- if (is_dark()) .MAPS_STATUS$dark else .MAPS_STATUS$light
        leaflet::addLegend(p, "bottomright", colors = unname(cols),
          labels = c("Good", "Watch", "Neglected", "Insufficient data"),
          title = "Servicing &middot; size = days since checked", opacity = 0.9)
      } else {
        line_grain <- src() == "camera" && !is_priority() && !is_timing() && grain_rv() == "line"
        d <- if (line_grain) line_metric() else rate_loc_pts()
        vc <- if (line_grain) "metric" else valcol()
        if (is.null(d) || !nrow(d)) return()
        cap <- .robust_cap(d[[vc]]); pf <- surf_pal(cap)
        ttl <- if (is_priority()) sprintf("Priority &middot; %s high, %s low", pred_label(), prot_label())
               else if (is_timing()) sprintf("Predator proximity &middot; red = %s close to %s in time", pred_label(), prot_label())
               else if (line_grain) sprintf("RAI / line &middot; %s", group_lab())
               else sprintf("%s &middot; %s", vallabel(), group_lab())
        leaflet::addLegend(p, "bottomright", pal = pf, values = pmin(d[[vc]], cap), title = ttl, opacity = 0.9)
      }
    })

    observeEvent(input$map_marker_click, {                      # marker → filter table
      cid <- input$map_marker_click$id
      if (is.null(cid) || (src() == "trap" && measure() == "servicing")) return()
      if (startsWith(cid, "L|")) {
        pr <- strsplit(cid, "|", fixed = TRUE)[[1]]
        selected(list(kind = "line", reserve = pr[2], line = pr[3], label = sprintf("Line %s · %s", pr[3], pr[2])))
      } else {
        locs <- ik_data$app$geography$locations
        selected(list(kind = "location", id = cid, label = locs$name[match(cid, locs$location_id)] %||% cid))
      }
      # Scroll the map to the top of the viewport so the in-panel controls roll off and the now-filtered
      # table below the map comes into view (the map is ~70vh, so it stays visible above the table).
      session$sendCustomMessage("ik-maps-scroll", session$ns("map"))
    })

    # ---- records / servicing table + CSV ----
    records_base <- reactive({ req(measure() %in% c("rate", "captures"), has_group())
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
      if ((is_priority() || is_timing()) && (!has_pred() || !has_prot()))
        return(tags$small("Pick a predator and a protected species to compare."))
      if (is_priority()) {
        d <- prio_pts(); n <- if (is.null(d)) 0L else nrow(d)
        return(tags$small(sprintf("%s camera sites · priority = %s high & %s low (exploratory). Click a row for the detections behind it.",
                                  format(n, big.mark = ","), pred_label(), prot_label())))
      }
      if (is_timing()) {
        d <- timing_pts(); n <- if (is.null(d)) 0L else nrow(d)
        gap_phrase <- if (isTRUE(input$after_only))
          sprintf("median time until a %s arrives AFTER a %s (stalking)", pred_label(), prot_label())
        else sprintf("median time between a %s and the nearest %s", prot_label(), pred_label())
        return(tags$small(sprintf("%s camera sites · %s (exploratory). Click a row for the detections behind it.",
                                  format(n, big.mark = ","), gap_phrase)))
      }
      if (src() == "trap" && measure() == "servicing") {
        d <- serv_pts(); n <- if (is.null(d)) 0L else nrow(d)
        nneg  <- if (is.null(d)) 0L else sum(d$status == "neglected", na.rm = TRUE)
        nskip <- if (is.null(d)) 0L else sum(d$n_checks == 0, na.rm = TRUE)
        nins  <- if (is.null(d)) 0L else sum(d$status == "insufficient_data", na.rm = TRUE)
        u <- serv_unplaced(); nu <- if (is.null(u)) 0L else nrow(u)
        a <- serv_absent(); na <- if (is.null(a)) 0L else nrow(a)
        seas <- .ik_nz(selection()$season); per_lab <- if (is.null(seas)) "all seasons" else paste(seas, collapse = ", ")
        tags$small(sprintf(
          "%s traps assessed in %s · %s neglected%s%s%s%s. %s trap locations exist overall.",
          format(n, big.mark = ","), per_lab, format(nneg, big.mark = ","),
          if (nskip) sprintf(" (incl. %s never checked)", format(nskip, big.mark = ",")) else "",
          if (nins) sprintf(" · %s insufficient data", format(nins, big.mark = ",")) else "",
          if (nu) sprintf(" · %s unmapped (no coords)", format(nu, big.mark = ",")) else "",
          if (na) sprintf(" · %s dormant/historic (faint — toggle “No records”)", format(na, big.mark = ",")) else "",
          format(n_trap_total(), big.mark = ",")))
      } else if (show_lines()) {
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
      if ((is_priority() || is_timing()))
        validate(need(has_pred() && has_prot(), "Pick a predator and a protected species to compare."))
      if (is_priority()) {
        d <- prio_pts(); sel <- selected()
        if (!is.null(sel) && identical(sel$kind, "location")) d <- d[d$location_id == sel$id, , drop = FALSE]
        validate(need(!is.null(d) && nrow(d), "No camera sites in the current selection."))
        d <- d[order(-d$metric), , drop = FALSE]
        prio_tbl(d)                                            # remember the order for the row drill
        df <- data.frame(Location = d$name, Reserve = d$reserve, Line = d$line,
          predator = round(d$predator, 2), protected = round(d$protected, 2),
          Priority = round(d$metric, 2), check.names = FALSE, stringsAsFactors = FALSE)
        names(df)[4:5] <- c(paste(pred_label(), "RAI"), paste(prot_label(), "RAI"))
        return(DT::datatable(df, rownames = FALSE, selection = "single",
          class = "stripe hover row-border ik-row-click",
          options = list(pageLength = 10, scrollX = TRUE, dom = "ftip", order = list(list(5, "desc")))))
      }
      if (is_timing()) {
        d <- timing_pts(); sel <- selected()
        if (!is.null(sel) && identical(sel$kind, "location")) d <- d[d$location_id == sel$id, , drop = FALSE]
        validate(need(!is.null(d) && nrow(d), "No predator/protected co-detections in the current selection."))
        d <- d[order(d$median_gap_h), , drop = FALSE]         # shortest gap (most urgent) first
        prio_tbl(d)                                            # same row drill as priority
        df <- data.frame(Location = d$name, Reserve = d$reserve, Line = d$line,
          `Median gap` = vapply(d$median_gap_h, .fmt_gap_lab, character(1)),
          `Median gap (h)` = round(d$median_gap_h, 1), Pairs = as.integer(d$n),
          check.names = FALSE, stringsAsFactors = FALSE)
        return(DT::datatable(df, rownames = FALSE, selection = "single",
          class = "stripe hover row-border ik-row-click",
          options = list(pageLength = 10, scrollX = TRUE, dom = "ftip", order = list(list(4, "asc")),
            columnDefs = list(list(visible = FALSE, targets = 4)))))   # hide the numeric sort helper
      }
      if (src() == "trap" && measure() == "servicing") {
        d <- serv_pts(); validate(need(!is.null(d) && nrow(d), "No traps in the current selection."))
        df <- data.frame(Trap = d$name, Line = d$line, Reserve = d$reserve, Checks = d$n_checks,
          `Trap-days` = round(d$trap_days), `Mean interval (d)` = round(d$mean_interval_days, 1),
          Status = tools::toTitleCase(as.character(d$status)), Captures = d$captures, check.names = FALSE, stringsAsFactors = FALSE)
        return(DT::datatable(df, rownames = FALSE, class = "stripe hover row-border",
          options = list(pageLength = 10, scrollX = TRUE, dom = "tip", order = list(list(5, "desc")))))
      }
      if (show_lines()) {                                     # zoomed out → per-LINE RAI; click a line to drill
        d <- line_metric(); validate(need(!is.null(d) && nrow(d), "No camera lines in the current selection."))
        d <- d[order(-d$metric), , drop = FALSE]; lines_tbl(d)
        df <- data.frame(Line = d$line, Reserve = d$reserve, RAI = round(d$metric, 2),
          Cameras = as.integer(d$n), Detections = as.integer(d$individuals),
          check.names = FALSE, stringsAsFactors = FALSE)
        return(DT::datatable(df, rownames = FALSE, selection = "single",
          class = "stripe hover row-border ik-row-click",
          options = list(pageLength = 10, scrollX = TRUE, dom = "ftip", order = list(list(2, "desc")))))
      }
      o <- records()
      validate(need(!is.null(o) && nrow(o), sprintf("No %ss of this group here.", if (src() == "trap") "capture" else "detection")))
      .records_dt(o, hover = TRUE)
    })

    rec_proxy <- DT::dataTableProxy("table")

    # Records DT, shared by the table below the map (hover = TRUE → hovering a row shows that
    # location's popup on the map) and the records modal's "Records" tab. A hidden .loc column
    # carries the location_id; createdRow stashes it on the row + wires the hover input.
    .records_dt <- function(o, hover = FALSE, dom = "tip") {
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      has_t <- format(o$when, "%H:%M:%S") != "00:00:00"
      when_lab <- ifelse(has_t, format(o$when, "%Y-%m-%d %H:%M"), format(o$when, "%Y-%m-%d"))
      df <- data.frame(When = when_lab, Species = ik_species_label(o$scientificName, ik_data, prefer),
        Count = o$count, Reserve = o$reserve, Line = o$line, Location = o$locationName,
        .loc = o$location_id, check.names = FALSE, stringsAsFactors = FALSE)
      loc_i <- ncol(df) - 1L                                   # 0-based index of the hidden .loc column
      opts <- list(pageLength = 10, scrollX = TRUE, dom = dom,
                   columnDefs = list(list(visible = FALSE, targets = loc_i)))
      if (hover) opts$createdRow <- DT::JS(sprintf(
        "function(row,data,i){var L=data[%d];row.setAttribute('data-loc',L);row.addEventListener('mouseenter',function(){Shiny.setInputValue('%s',L,{priority:'event'});});row.addEventListener('mouseleave',function(){Shiny.setInputValue('%s','',{priority:'event'});});}",
        loc_i, session$ns("table_hover"), session$ns("table_hover")))
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = opts)
    }

    # Priority row → the predator + protected detections at that camera that drive its score, in a
    # tabbed modal (Detections → Record details) like Co-occurrence, so the number is auditable.
    .prio_detections <- function(loc_id) {
      preds <- input$predators; if (!length(preds)) preds <- .pred_default
      prots <- input$protected; if (!length(prots)) prots <- .prot_default
      pred_sci <- ik_resolve_species_choice(preds, pred_taxa); prot_sci <- ik_resolve_species_choice(prots, prot_taxa)
      obs <- ik_observations(ik_data, with_location = TRUE)
      obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal" &
                   !is.na(obs$locationID) & obs$locationID == loc_id & !is.na(obs$eventStart) &
                   obs$scientificName %in% c(pred_sci, prot_sci), , drop = FALSE]
      seas <- .ik_nz(selection()$season)                   # scope the drill to the selected period too
      if (!is.null(seas) && nrow(obs)) {
        op <- ik_observation_period(ik_data)
        osea <- op$calendar_season[match(obs$observationID, op$observationID)]
        obs <- obs[!is.na(osea) & osea %in% seas, , drop = FALSE]
      }
      if (!nrow(obs)) return(NULL)
      obs$role <- ifelse(obs$scientificName %in% pred_sci, pred_label(), prot_label())
      obs[order(obs$eventStart), , drop = FALSE]
    }
    observeEvent(input$table_rows_selected, {                   # priority/timing drill: detections behind a camera
      req(is_priority() || is_timing()); i <- input$table_rows_selected; d <- prio_tbl()
      if (!length(i) || is.null(d) || i > nrow(d)) return()
      row <- d[i, , drop = FALSE]; prio_rec(.prio_detections(row$location_id)); prio_recobs(NULL)
      sub <- if (is_timing())
        sprintf("%s · Line %s — median gap %s (%s ↔ %s). The detections behind it:",
                row$reserve, ifelse(is.na(row$line), "—", row$line), .fmt_gap_lab(row$median_gap_h),
                prot_label(), pred_label())
      else
        sprintf("%s · Line %s — priority %.2f (%s RAI %.2f vs %s RAI %.2f). The detections behind it:",
                row$reserve, ifelse(is.na(row$line), "—", row$line), row$metric,
                pred_label(), row$predator, prot_label(), row$protected)
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("Detections at %s", row$name), sub),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("prio_tabs"),
          tabPanel("Detections",     icon = icon("list"),        DT::dataTableOutput(session$ns("prio_detail"))),
          tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("prio_record"))))))
      hideTab(session = session, inputId = "prio_tabs", target = "Record details")   # appears on first drill
      DT::selectRows(rec_proxy, NULL)
    }, ignoreInit = TRUE)

    output$prio_detail <- DT::renderDT({
      det <- prio_rec(); validate(need(!is.null(det) && nrow(det), "No predator or protected detections at this camera."))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      .lnk <- function(label, id) sprintf(
        "<a class='ik-link' onclick=\"Shiny.setInputValue('%s',{id:'%s'},{priority:'event'})\">%s</a>",
        session$ns("prio_view"), id, htmltools::htmlEscape(label))
      df <- data.frame(
        Role = det$role,
        Species = mapply(.lnk, ik_species_label(det$scientificName, ik_data, prefer), det$observationID),
        When = .ik_when_label(det$eventStart),
        .when_sort = as.numeric(det$eventStart), check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "none", escape = -2,
        class = "stripe hover row-border",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          columnDefs = .ik_dt_when_defs(df, "When")))
    })
    observeEvent(input$prio_view, {
      id <- input$prio_view$id
      if (length(id) && nzchar(id)) { prio_recobs(id); showTab(session = session, inputId = "prio_tabs", target = "Record details", select = TRUE) }
    })
    observeEvent(input$prio_tab_back, updateTabsetPanel(session, input$prio_tab_back$tabset, selected = input$prio_tab_back$to))
    output$prio_record <- renderUI({
      if (is.null(prio_recobs()))
        return(tags$p(class = "ik-maps-hint", "Click a species in the Detections tab to see its record here."))
      ob <- ik_observation(ik_data, prio_recobs()); if (is.null(ob)) return(tags$p("Record not found."))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      tagList(.ik_tab_back(session$ns("prio_tab_back"), "prio_tabs", "Detections", "Back to detections"),
              .ovw_title(ik_data, ob, prefer),
              .ovw_tabs(ik_data, ob, prefer, tabset_id = session$ns("prio_rec_subtabs")))
    })

    # The records modal: "Records" (the same list as below the map) + "Record details" (the clicked
    # record, with a back link). Opens on Record details; from Records you can drill into any row.
    .maprec_modal <- function() showModal(modalDialog(
      title = .ik_modal_title(sprintf("%s record", if (src() == "trap") "Trap" else "Camera"),
                              sprintf("%s · click a record for its details, then Back for the list", group_lab())),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      tabsetPanel(id = session$ns("maprec_tabs"),
        tabPanel("Records",        icon = icon("list"),        DT::dataTableOutput(session$ns("maprec_table"))),
        tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("maprec_record"))))))

    # A clicked record row opens that modal on its detail tab (no map pan/zoom — that was
    # disorienting; hovering a row previews the location instead). Line-grain rows still drill.
    observeEvent(input$table_rows_selected, {
      if (is_priority() || is_timing() || (src() == "trap" && measure() == "servicing")) return()
      i <- input$table_rows_selected
      if (show_lines()) {                                       # line-grain table: row → drill into that line
        d <- lines_tbl()
        if (length(i) && !is.null(d) && i <= nrow(d))
          selected(list(kind = "line", reserve = d$reserve[i], line = d$line[i],
                        label = sprintf("Line %s · %s", d$line[i], d$reserve[i])))
        DT::selectRows(rec_proxy, NULL); return()
      }
      o <- records(); if (!length(i) || is.null(o) || i > nrow(o)) return()
      maprec_obs(o$observationID[i]); .maprec_modal()
      updateTabsetPanel(session, "maprec_tabs", selected = "Record details")
      DT::selectRows(rec_proxy, NULL)
    }, ignoreInit = TRUE)

    output$maprec_table <- DT::renderDT({ o <- records(); validate(need(!is.null(o) && nrow(o), "No records.")); .records_dt(o, dom = "ftip") })
    observeEvent(input$maprec_table_rows_selected, {
      i <- input$maprec_table_rows_selected; o <- records()
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

    # Hover a record row → preview that location's popup on the map (debounced so fast passes don't
    # spam the server); leaving the row clears it. Records views only (camera rate / trap counts).
    hover_loc <- shiny::debounce(reactive(input$table_hover), 120)
    observeEvent(hover_loc(), {
      p <- proxy(); leaflet::clearGroup(p, "Hover")
      loc <- hover_loc()
      if (is.null(loc) || !nzchar(loc) || is_priority() || is_timing() ||
          (src() == "trap" && measure() == "servicing") || show_lines()) return()
      d <- rate_loc_pts(); if (is.null(d)) return()
      r <- d[d$location_id == loc, , drop = FALSE]
      if (!nrow(r) || !is.finite(r$longitude[1])) return()
      leaflet::addPopups(p, lng = r$longitude[1], lat = r$latitude[1], popup = rate_popup(r[1, , drop = FALSE]),
        group = "Hover", options = leaflet::popupOptions(closeButton = FALSE, autoPan = FALSE))
    }, ignoreNULL = FALSE)

    output$unplaced <- renderUI({
      u <- if (is_priority()) prio_unplaced() else if (is_timing()) timing_unplaced() else if (src() == "trap" && measure() == "servicing") serv_unplaced() else if (src() == "trap") trap_unplaced() else cam_unplaced()
      if (is.null(u) || !nrow(u)) return(NULL)
      div(class = "ik-maps-unplaced", icon("triangle-exclamation"),
          sprintf(" %s coordless %s not shown on the map (no location fix).", format(nrow(u), big.mark = ","), if (src() == "trap") "traps" else "sites"))
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        tag <- if (is_priority()) "camera-priority"
               else if (is_timing()) "camera-timing"
               else if (src() == "trap" && measure() == "servicing") "trap-servicing"
               else paste0(src(), "-", measure(), "-", gsub("[^A-Za-z0-9]+", "-", tolower(group_lab())))
        sprintf("maps-%s-%s.csv", tag, Sys.Date())
      },
      content = function(file) {
        d <- if (is_priority()) prio_pts() else if (is_timing()) timing_pts() else if (src() == "trap" && measure() == "servicing") serv_pts() else records()
        if (is.null(d)) d <- data.frame(); utils::write.csv(d, file, row.names = FALSE)
      }
    )
  })
}
