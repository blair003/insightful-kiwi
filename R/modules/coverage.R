# coverage.R (module) — "Coverage": the SPATIAL counterpart to the Neighbourhood panel. One leaflet
# carrying BOTH devices at once as toggleable layers, so you can see where the protected species are
# and whether predator control is reaching them:
#   • Protected (camera)  — green circles, size ∝ detections (the hotspots)
#   • Predators (camera)  — red circles, size ∝ detections (where predators roam; off by default)
#   • Catches (traps)     — purple circles, size ∝ predators caught (where removal happens)
# Camera dots share ONE count scale (predator vs protected compare like-for-like; the effort-adjusted
# rate is in each marker's hover); catches have their own (smaller) count scale.
#   • Traps (effort)      — small grey dots, every trap deployed (the trapping field / coverage)
# A green hotspot sitting in purple + grey is covered; a green hotspot with little around it is a gap.
# Colour encodes WHAT a marker is, size encodes magnitude (so one legend, no clashing colour scales).
# Period + Reserve come from the sidebar; the predator/protected role pickers are in-panel. Reuses
# ik_location_metric() for every layer (camera rate / trap captures), so no new data layer.

#' Body of the whole-map "how to read this" help modal — tabbed. `cam_norm` = camera-hours the
#' detection rate is scaled to (project config). @keywords internal
coverage_help_body <- function(cam_norm = 500) {
  P  <- function(...) tags$p(...)
  ch <- format(cam_norm, big.mark = ",")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "The map", icon = icon("layer-group"),
      P(tags$br(), "One map carrying ", tags$b("both devices"), " so you can see where the protected ",
        "species are and whether predator control is reaching them. ", tags$b("Colour says what a marker "),
        tags$b("is; size says how much"), " — one legend, no clashing colour scales."),
      tags$h6("The layers (toggle top-right)"),
      tags$ul(
        tags$li(tags$b(tags$span(style = "color:#2e7d32", "Protected")), " (camera) — green dots, size ∝ detections. The hotspots you're protecting."),
        tags$li(tags$b(tags$span(style = "color:#c62828", "Predators")), " (camera) — red rings, size ∝ detections, on the SAME scale as the green dots so the two compare directly. Where predators roam. Off by default."),
        tags$li(tags$b(tags$span(style = "color:#6a3d9a", "Catches")), " (traps) — purple dots, size ∝ predators caught. Where removal is actually happening."),
        tags$li(tags$b("Device"), " — small dots for every device in service this period: ",
                tags$span(style = "color:#8a8a8a", "grey traps"), " (",
                tags$span(style = "color:#f59f00", "amber"), " = neglected) and ",
                tags$span(style = "color:#2c7fb8", "blue cameras"),
                ". This is the active trapping field — the same traps the Coverage-gaps table counts; out-of-service traps (dormant/historic) are hidden."),
        tags$li(tags$b("Boundary"), " — the monitored footprint (convex hull of the devices) per reserve; hover it for the reserve name."))),
    tabPanel(
      "Reading it", icon = icon("magnifying-glass-chart"),
      P(tags$br(), "A green hotspot ringed by purple and grey is ", tags$b("covered"), "; a green hotspot ",
        "with little around it is a ", tags$b("gap"), " — somewhere a protected species is active but ",
        "trapping isn't reaching."),
      P("Camera markers are sized by the ", tags$b("number of detections"), " — a count of 1 is the same small ",
        "dot everywhere, and the green and red dots share ", tags$b("one scale"), " so predator and protected ",
        "compare like-for-like (a single hotspot can't shrink the rest — the scale clamps at the busiest 10%). ",
        "The effort-adjusted ", tags$b("rate"), " (per ", ch, " camera-hours) is in each marker's hover, where the ",
        "comparison across differently-run cameras matters. Catch markers are a ", tags$b("count"), " of predators ",
        "removed, on their own scale. Period and reserve come from the sidebar; click any marker for its detail."),
      P("Whether the network is even dense enough to work — footprint, devices per km², spacing — now lives ",
        "on the main ", tags$b("Overview → Network density"), " tab; the ", tags$b("Coverage gaps"),
        " table below ranks the gaps that remain numerically.")),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Camera rate"), " — per location, detections of the chosen species ÷ its camera-hours ",
                "× ", ch, " (the per-camera scale from config), net of likely duplicates."),
        tags$li(tags$b("Catches"), " — predators of the chosen species caught at each trap in the period."),
        tags$li(tags$b("Boundary"), " — the convex hull of a reserve's devices (its monitored footprint)."),
        tags$li(tags$b("Marker size"), " — scaled to the value and clamped at the 95th percentile, so one ",
                "outlier can't flatten the rest.")),
      P(tags$em("All for the sidebar's period & reserve; everything redraws when you change them.")))
  )
}

#' Body of the Coverage-gaps "how to read this" help modal — tabbed. `cam_norm` = camera-hours the
#' detection rates are scaled to (project config). @keywords internal
coverage_gaps_help_body <- function(cam_norm = 500) {
  P  <- function(...) tags$p(...)
  ch <- format(cam_norm, big.mark = ",")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "Every monitoring ", tags$b("line"), " (its cameras) is ranked ", tags$b("worst-first"),
        " by how well predator control reaches it — turning the map's eyeball judgement into a list you ",
        "can work down."),
      P("Each line gets a ", tags$b("status"), " (next tab) and the numbers behind it. Use it to find the ",
        "protected hotspots where the trapping is thin, absent, or neglected."),
      
      P(tags$b("Hint:"), " Select a single Reserve. Mousever the table entry",
        "to see the traps within the Gap radius of that line.")),
    tabPanel(
      "Columns & status", icon = icon("table-list"),
      tags$h6("Columns"),
      tags$ul(
        tags$li(tags$b("Protected / Predator"), " — detections pooled across the line's cameras, as a rate per ", ch,
                " camera-hours (the line's RAI — a per-line figure, so a larger unit than the per-camera map). 0 = none seen on camera."),
        tags$li(tags$b("Traps"), " — traps running within the gap radius (how much trapping reaches the line)."),
        tags$li(tags$b("Caught"), " — predators caught in those nearby traps this period."),
        tags$li(tags$b("Neglected"), " — of the nearby active traps, how many are unserviced this period."),
        tags$li(tags$b("Traps/km²"), " — trap density for THIS line: the traps above divided by the area within the gap radius of its cameras (so it moves with the line and the radius).")),
      tags$h6("Status"),
      tags$ul(
        tags$li(tags$b("No trapping"), " — protected on camera, but no traps running nearby."),
        tags$li(tags$b("Predators uncaught"), " — predators on camera nearby, but none being caught."),
        tags$li(tags$b("Trapping neglected"), " — traps are nearby but mostly unserviced."),
        tags$li(tags$b("Covered"), " — protected hotspot with serviced trapping reaching it."),
        tags$li(tags$b("No protected here"), " — no protected species on camera on this line."))),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Neighbourhood"), " — a line's “nearby” is the traps within the ", tags$b("gap radius"),
                " of its cameras. A bigger radius counts more distant traps as nearby, so more lines look ",
                "covered — it drives the Traps, Caught and Neglected columns. With a reserve selected, ",
                tags$b("Include nearby traps beyond the reserve"), " (on by default) also counts traps within ",
                "the radius tagged to another reserve (a boundary buffer); switch it off for a strict in-reserve view."),
        tags$li(tags$b("Rates"), " — protected/predator are the line's detections ÷ its cameras' camera-hours × ", ch, " (pooled into one per-line RAI)."),
        tags$li(tags$b("Neglected"), " — from the trap servicing assessment (a nearby active trap unchecked ",
                "this period); see the Trap review help for the cadence buckets."),
        tags$li(tags$b("Ranking"), " — worst gap first (protected present, control weakest).")),
      P(tags$em("Why isn't the radius drawn on the map?"), " It's applied ", tags$b("per line"), " — each line ",
        "has its own circle around its own cameras, so a single circle would mislead. The table is where the ",
        "radius lives."))
  )
}

# Coverage-gap status → colour + label. SHARED by the table's per-row tint (a row class ik-gaprow-<status>,
# styled in coverage.css) and the colour KEY beneath the lead — so the Status column can go (no horizontal
# room beside the map) while the status still reads off each row.
.COV_GAP_COL <- c(no_trapping = "#c62828", predators_uncaught = "#e8590c",
                  neglected = "#f59f00", covered = "#2e7d32", no_protected = "#868e96")
.COV_GAP_LAB <- c(no_trapping = "No trapping", predators_uncaught = "Predators uncaught",
                  neglected = "Trapping neglected", covered = "Covered", no_protected = "No protected here")

#' Small colour key for the gap-status row tints — a swatch + label per status. @keywords internal
.cov_gap_key <- function() div(class = "ik-cov-gaps-key",
  lapply(names(.COV_GAP_COL), function(s)
    tags$span(class = "ik-gap-key-item",
      tags$span(class = "ik-gap-key-sw", style = sprintf("background:%s", .COV_GAP_COL[[s]])),
      .COV_GAP_LAB[[s]])))

#' DT header container for the gaps table, with a hover-info (ⓘ) tooltip on the metric columns.
#' @keywords internal
.cov_gaps_header <- function(norm) {
  n  <- format(norm, big.mark = ",")
  # NB: build with explicit tags$* — htmltools::withTags() would shadow a local `th` helper with the
  # built-in tag function, dropping the tooltip span.
  thx <- function(label, tip = NULL) tags$th(label, if (!is.null(tip)) .ik_hint(tip))
  tags$table(class = "display", tags$thead(tags$tr(
    thx("Line"), thx("Reserve"),
    thx("Protected", sprintf("Protected detections pooled across this line's cameras, as a rate per %s camera-hours (the line's RAI). 0 = none on camera.", n)),
    thx("Predator",  sprintf("Predator detections pooled across this line's cameras, per %s camera-hours. Higher = more predator activity on camera.", n)),
    thx("Traps", "Traps running within the gap radius of this line's cameras — how much trapping reaches it. Set by the Gap radius control."),
    thx("Caught", "Predators caught in those nearby traps this period."),
    thx("Neglected", "Of the nearby active traps, how many are unserviced (neglected) this period."),
    thx("Traps/km²", "Per-line trap density: the traps above ÷ the area within the gap radius of this line's cameras (moves with the line and the radius).")
  )))   # Status column dropped — each row is now tinted by its status (see the colour key under the lead)
}

#' Coverage nav panel. @param id Module id. @param ik_data The container (camera-hour norm for help).
#' Coverage "View options" sidebar controls — Protected / Predator (the map measure) + Gap radius /
#' Include-nearby-traps (the gaps table). Built in the module's namespace, rendered in the global sidebar
#' (ui.R) above the shared Filters; the species pickers' choices are populated server-side. @keywords internal
coverage_controls <- function(id, ik_data = NULL) {
  ns <- NS(id)
  div(class = "ik-selection ik-view-controls",
    tags$div(class = "ik-view-controls-h", "View options"),
    selectInput(ns("prot"), "Protected", choices = NULL, multiple = TRUE),
    selectInput(ns("pred"), "Predator",  choices = NULL, multiple = TRUE),
    selectInput(ns("radius"), "Gap radius",
                choices = c("250 m" = 250, "500 m" = 500, "750 m" = 750, "1 km" = 1000), selected = 500),
    .ik_cross_boundary_input(ns("cross_boundary"), "traps"))
}

coverage_ui <- function(id, ik_data = NULL) {
  ns <- NS(id)
  cam_norm  <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500    # per-camera (map markers)
  line_norm <- (ik_data$meta$camera$rai %||% list())$norm_hours   %||% 2000   # per-line (gaps table, pooled cameras)
  nav_panel(
    "Coverage Gaps", value = "coverage", icon = icon("shield-halved"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/coverage.css")),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),  # .ik-maps-split / -side
    tags$script(src = .ik_asset("js/maps.js")),                            # reuse the resize-on-tab-show fix
    div(class = "ik-cov",
        .ik_page_header("Coverage Gaps",
            description = "Where are the protected species, and is predator control reaching them?",
            help = .ik_info(ns("cov_help"), "Coverage Gaps — how to read this", coverage_help_body(cam_norm)),
            banner = div(class = "ik-page-period", uiOutput(ns("period_banner")))),   # date on the title line
        # The map (protected hotspots vs predator control) beside the gaps table; View options → sidebar.
        uiOutput(ns("caption")),
        layout_columns(class = "ik-maps-split", col_widths = breakpoints(sm = 12, lg = c(8, 4)),
          leaflet::leafletOutput(ns("map"), height = "62vh"),
          # Coverage gaps beside the map (they drive its hover/click); the gap-radius control is in the sidebar.
          div(class = "ik-maps-side", style = "max-height:62vh;",
            div(class = "ik-cov-gaps",
                # No heading (the page title already says it) — start straight at the lead, help inline.
                tags$p(class = "ik-cov-gaps-lead",
                  "Protected hotspots ranked worst-first by how well predator control reaches them, within the gap radius. ",
                  .ik_info(ns("gaps_help"), "Coverage gaps — how to read this", coverage_gaps_help_body(line_norm))),
                .cov_gap_key(),
                DT::DTOutput(ns("gaps"))))))
  )
}

#' Coverage server.
#' @param id Module id. @param ik_data The container. @param prefer_scientific reactive name pref.
#' @param selection reactive selection SPEC (Period + Reserve from sidebar). @param color_mode theme.
coverage_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                            selection = reactive(list()), color_mode = reactive("light"),
                            active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    output$period_banner <- renderUI(.ik_period_banner(ik_data, selection()))
    is_dark <- reactive(identical(color_mode(), "dark"))
    sg <- ik_species_groups(ik_data)
    .role_taxa <- function(role) { l <- unique(sg$label[sg$role == role & !is.na(sg$monitor)])
      stats::setNames(lapply(l, function(x) sg$scientificName[sg$label == x & !is.na(sg$scientificName)]), l) }
    pred_taxa <- .role_taxa("predator"); prot_taxa <- .role_taxa("protected")
    splits <- unique(sg$label[which(sg$split)])
    prefer <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    .pred_def <- paste0("grp:", names(pred_taxa)[1])
    .prot_def <- paste0("grp:", names(prot_taxa)[1])
    per_cam   <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500    # per-camera (map markers)
    line_norm <- (ik_data$meta$camera$rai %||% list())$norm_hours   %||% 2000   # per-line (gaps table header)

    observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      updateSelectInput(session, "prot", choices = ik_species_choices(prot_taxa, ik_data, p, splits), selected = keep(isolate(input$prot), .prot_def))
      updateSelectInput(session, "pred", choices = ik_species_choices(pred_taxa, ik_data, p, splits), selected = keep(isolate(input$pred), .pred_def))
    })

    prot_sci <- reactive(ik_resolve_species_choice(input$prot, prot_taxa))
    pred_sci <- reactive(ik_resolve_species_choice(input$pred, pred_taxa))
    prot_lab <- reactive({ v <- input$prot; if (!length(v)) "protected" else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })
    pred_lab <- reactive({ v <- input$pred; if (!length(v)) "predator"  else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })

    # (The inline "Green = … / Works best filtered by Reserve" legend lead was removed to give the map more
    #  vertical room — the marker key is on the map legend + in the help modal.)

    .pts <- function(m) if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE]
    cam_prot <- reactive({ req(active()); if (!length(prot_sci())) return(NULL); .pts(ik_location_metric(ik_data, selection(), list(P = prot_sci()), "camera", norm = per_cam)) })
    cam_pred <- reactive({ req(active()); if (!length(pred_sci())) return(NULL); .pts(ik_location_metric(ik_data, selection(), list(P = pred_sci()), "camera", norm = per_cam)) })
    trap_pred <- reactive({ req(active()); if (!length(pred_sci())) return(NULL); .pts(ik_location_metric(ik_data, selection(), list(P = pred_sci()), "trap")) })
    # The ACTIVE trapping field for the period — every trap in service (good/watch/neglected), from the
    # SAME servicing review the Coverage-gaps "Traps" column counts. ONE trap set drives the grey/amber
    # Device markers, the row-hover rings, the drill modal AND the table count, so they can't disagree.
    # Out-of-service traps (dormant/historic) are excluded; captures are the selected predator's (so a
    # grey dot's "caught" matches the purple Catches layer and the table's "Caught").
    # Cross-boundary trap reach: when a Reserve is selected with the toggle ON, the traps within the gap
    # radius of ANY of the reserve's cameras — wherever tagged (a buffer / the next reserve). NULL when
    # no reserve, toggle off, or no cameras. Mirrors the gaps-table neighbourhood, so map == count.
    reach_locs <- reactive({
      rsv <- .ik_nz(selection()$reserve)
      if (is.null(rsv) || !isTRUE(input$cross_boundary)) return(NULL)
      cams <- cam_locs_all(); if (is.null(cams) || !nrow(cams)) return(NULL)
      nb <- ik_within_distance(ik_data, cams$location_id, radius_m = as.numeric(input$radius %||% 500), of = "trap")
      if (!nrow(nb)) return(NULL)
      unique(nb$to_id)
    })
    trap_active <- reactive({ req(active())
      sv <- ik_trap_review(ik_data, seasons = .ik_nz(selection()$season)); if (is.null(sv)) return(NULL)
      sv <- sv[!sv$status %in% c("dormant", "historic") & is.finite(sv$latitude) & is.finite(sv$longitude), , drop = FALSE]
      rsv <- .ik_nz(selection()$reserve)
      if (!is.null(rsv)) {                                   # honour the sidebar Reserve (+ optional cross-boundary reach)
        keep <- sv$reserve %in% rsv
        rl <- reach_locs(); if (!is.null(rl)) keep <- keep | sv$location %in% rl
        sv <- sv[keep, , drop = FALSE]
      }
      if (!nrow(sv)) return(NULL)
      tp <- trap_pred(); sv$captures <- if (is.null(tp)) 0L else tp$captures[match(sv$location, tp$location_id)]
      sv$captures[is.na(sv$captures)] <- 0L
      sv
    })
    cam_locs_all <- reactive({ req(active())                    # camera locations — the blue half of Device
      st <- ik_dataset_source_types(ik_data$datasets); al <- ik_active_locations(ik_data)
      al$st <- unname(st[al$dataset])
      al <- al[!is.na(al$st) & al$st == "camera" & is.finite(al$latitude) & is.finite(al$longitude), , drop = FALSE]
      rsv <- .ik_nz(selection()$reserve); if (!is.null(rsv)) al <- al[al$reserve %in% rsv, , drop = FALSE]   # honour the sidebar Reserve
      al
    })

    .radius <- function(v, lo, hi) ik_marker_radius(v, lo, hi)        # shared impl in spatial.R
    # Camera markers are sized by raw DETECTION COUNT on ONE shared, fixed scale across the Protected and
    # Predator layers — so a marker size means the same number of detections whether it's green or red, and
    # "1 detection" is the same small dot on every line (rate stays in the hover, where effort matters). The
    # cap is the 90th-pctl of the combined counts so a single hotspot can't shrink everything else.
    cam_count_cap <- reactive({
      vals <- c(cam_prot()$individuals, cam_pred()$individuals)
      vals <- vals[is.finite(vals) & vals > 0]
      if (!length(vals)) 1 else ik_robust_cap(vals, 0.9)
    })
    proxy <- function() leaflet::leafletProxy("map", session)

    output$map <- leaflet::renderLeaflet({
      locs <- ik_data$app$geography$locations
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
      m <- leaflet::addProviderTiles(m, canvas, group = "Map")
      m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
      pns <- c("boundary", "device", "catches", "protected", "predators", "highlight")  # boundary lowest; highlight on TOP
      for (pn in pns) m <- leaflet::addMapPane(m, pn, zIndex = 410 + 10 * match(pn, pns))
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
        overlayGroups = c("Protected", "Predators", "Catches", "Device", "Boundary"),
        options = leaflet::layersControlOptions(collapsed = FALSE))
      m <- leaflet::hideGroup(m, "Predators")                   # off by default — the coverage story is Protected vs Catches/Traps
      if (nrow(locs)) m <- leaflet::fitBounds(m, min(locs$longitude), min(locs$latitude), max(locs$longitude), max(locs$latitude))
      m
    })
    # Render from load (not just when the Coverage tab is first shown) so the proxy layers below land
    # on a live widget — otherwise the first marker draw races the render and the markers are dropped,
    # leaving the map blank until a data-selection change re-fires the observers (the gap radius, which
    # only drives the table, wouldn't).
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    observeEvent(color_mode(), {
      p <- proxy(); leaflet::clearGroup(p, "Map")
      leaflet::addProviderTiles(p, if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron, group = "Map")
    }, ignoreInit = TRUE)

    observe({                                                   # Device — active traps (grey, neglected amber) + cameras (blue)
      p <- proxy(); leaflet::clearGroup(p, "Device")
      d <- trap_active()                                        # the in-service field — click for check history
      if (!is.null(d) && nrow(d)) {
        st_lab <- c(good = "Good", watch = "Watch", neglected = "Neglected")
        lab <- sprintf("%s — %s · %d check%s · %s trap-days · %d caught · click for check history",
                       d$name, ifelse(is.na(d$status), "—", unname(st_lab[d$status])),
                       as.integer(d$n_checks), ifelse(d$n_checks == 1L, "", "s"),
                       format(round(d$trap_days), big.mark = ","), as.integer(d$captures))
        leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Device", layerId = paste0("T|", d$location),
          radius = 3, fill = TRUE, fillColor = ifelse(d$status == "neglected", "#f59f00", "#8a8a8a"), fillOpacity = 0.6, stroke = FALSE,
          label = lab, options = leaflet::pathOptions(pane = "device"))
      }
      cl <- cam_locs_all()                                      # cameras (blue) — swallowed by any detection marker above
      if (!is.null(cl) && nrow(cl))
        leaflet::addCircleMarkers(p, data = cl, lng = ~longitude, lat = ~latitude, group = "Device",
          radius = 3, fill = TRUE, fillColor = "#2c7fb8", fillOpacity = 0.7, stroke = FALSE,
          label = sprintf("%s — camera", cl$name), options = leaflet::pathOptions(pane = "device"))
    })
    observe({                                                   # Catches — traps that caught the predator
      p <- proxy(); leaflet::clearGroup(p, "Catches")
      d <- trap_pred(); d <- if (is.null(d)) NULL else d[is.finite(d$captures) & d$captures > 0, , drop = FALSE]
      if (is.null(d) || !nrow(d)) return()
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Catches", layerId = paste0("T|", d$location_id),
        radius = ik_marker_radius(d$captures, 5, 16, cap = ik_robust_cap(d$captures, 0.9)),   # count-based, trap scale (smaller)
        fillColor = "#6a3d9a", fillOpacity = 0.85, stroke = TRUE, color = "#ffffff", weight = 1,
        label = sprintf("%s — %s caught: %d · click for check history", d$name, pred_lab(), as.integer(d$captures)),
        options = leaflet::pathOptions(pane = "catches"))   # popup dropped: click now opens the trap's check history
    })
    observe({                                                   # Predators on camera (off by default)
      p <- proxy(); leaflet::clearGroup(p, "Predators")
      d <- cam_pred(); d <- if (is.null(d)) NULL else d[is.finite(d$metric) & d$metric > 0, , drop = FALSE]
      if (is.null(d) || !nrow(d)) return()
      # drawn as a hollow RING on the top pane (+2px), so when toggled on it sits over the protected
      # dots without hiding them — green fill shows through a red ring where both are present.
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Predators",
        radius = ik_marker_radius(d$individuals, 6, 22, cap = cam_count_cap()) + 2,   # shared camera count scale (+2 = ring offset)
        fill = FALSE, stroke = TRUE, color = "#c62828", weight = 2.5, opacity = 0.95,
        label = sprintf("%s — %s %.2f / %s ch · %d detections", d$name, pred_lab(), d$metric,
                        format(per_cam, big.mark = ","), as.integer(d$individuals)),
        popup = ~sprintf("<b>%s</b><br/>Line %s &middot; %s<br/><b>%s: %.2f</b> / %s ch<br/>%d detections",
                         name, ifelse(is.na(line), "—", line), reserve, pred_lab(), metric, format(per_cam, big.mark = ","), as.integer(individuals)),
        options = leaflet::pathOptions(pane = "predators"))
    })
    observe({                                                   # Protected on camera — the hotspots (top)
      p <- proxy(); leaflet::clearGroup(p, "Protected")
      d <- cam_prot(); d <- if (is.null(d)) NULL else d[is.finite(d$metric) & d$metric > 0, , drop = FALSE]
      if (is.null(d) || !nrow(d)) return()
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Protected",
        radius = ik_marker_radius(d$individuals, 6, 22, cap = cam_count_cap()),   # shared camera count scale (same as Predators)
        fillColor = "#2e7d32", fillOpacity = 0.85, stroke = TRUE, color = "#ffffff", weight = 1.5,
        label = sprintf("%s — %s %.2f / %s ch · %d detections", d$name, prot_lab(), d$metric,
                        format(per_cam, big.mark = ","), as.integer(d$individuals)),
        popup = ~sprintf("<b>%s</b><br/>Line %s &middot; %s<br/><b>%s: %.2f</b> / %s ch<br/>%d detections",
                         name, ifelse(is.na(line), "—", line), reserve, prot_lab(), metric, format(per_cam, big.mark = ","), as.integer(individuals)),
        options = leaflet::pathOptions(pane = "protected"))
    })
    observe({                                                   # Boundary — monitored footprint (convex hull) per reserve
      p <- proxy(); leaflet::clearGroup(p, "Boundary"); req(active())
      locs <- ik_active_locations(ik_data)                      # active datasets only → no hidden reserves
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      rsv <- .ik_nz(selection()$reserve); if (!is.null(rsv)) locs <- locs[locs$reserve %in% rsv, , drop = FALSE]
      h <- ik_selection_hulls(locs, "reserve"); if (is.null(h) || !nrow(h)) return()
      edge <- if (is_dark()) "#cfd8dc" else "#37474f"
      # Two layers in the Boundary group: a faint tint (pure visual context, NON-interactive so it
      # never captures hover) + the dashed OUTLINE which carries the reserve label. So the name only
      # appears when you're ON the boundary line — not in the empty gaps between dense interior markers,
      # which was annoying. (The 8px renderer tolerance keeps the thin line easy to hover.)
      leaflet::addPolygons(p, data = h, group = "Boundary",
        fill = TRUE, fillColor = edge, fillOpacity = 0.05, stroke = FALSE,
        options = leaflet::pathOptions(pane = "boundary", interactive = FALSE))
      leaflet::addPolygons(p, data = h, group = "Boundary", label = ~reserve,
        labelOptions = leaflet::labelOptions(textsize = "12px", direction = "auto", sticky = TRUE),
        highlightOptions = leaflet::highlightOptions(weight = 3, color = "#1565c0", bringToFront = TRUE),
        fill = FALSE, stroke = TRUE, color = edge, weight = 1.5, dashArray = "5,6",
        options = leaflet::pathOptions(pane = "boundary"))
    })
    observe({                                                   # re-frame to the deployed extent of the selection
      p <- proxy(); d <- trap_active()
      ext <- rbind(cam_prot()[, c("longitude", "latitude")], cam_pred()[, c("longitude", "latitude")], if (!is.null(d)) d[, c("longitude", "latitude")])
      if (is.null(ext) || !nrow(ext)) return()
      leaflet::fitBounds(p, min(ext$longitude), min(ext$latitude), max(ext$longitude), max(ext$latitude), options = list(padding = c(30, 30)))
    })
    observe({                                                   # one colour-key legend (colour = what; size = magnitude)
      p <- proxy(); leaflet::clearControls(p)
      leaflet::addLegend(p, "bottomright", colors = c("#2e7d32", "#c62828", "#6a3d9a", "#8a8a8a", "#f59f00"),
        labels = c(sprintf("%s (camera)", prot_lab()), sprintf("%s (camera)", pred_lab()),
                   sprintf("%s caught (traps)", pred_lab()), "Traps in service", "Traps neglected"),
        title = "Coverage &middot; size = magnitude", opacity = 0.9)
    })

    output$caption <- renderUI({
      hot <- cam_prot(); nh <- if (is.null(hot)) 0L else sum(hot$metric > 0, na.rm = TRUE)
      tr  <- trap_active(); nt <- if (is.null(tr)) 0L else nrow(tr)
      cau <- trap_pred(); nc <- if (is.null(cau)) 0L else sum(cau$captures > 0, na.rm = TRUE)
      # The period is already on the calendar banner above, so don't re-list the seasons here — lead with the count.
      tags$p(class = "ik-cov-meta", sprintf(
        "%s camera hotspot%s with %s · %s traps (%s caught %s). Toggle layers top-right; click a marker for detail.",
        format(nh, big.mark = ","), if (identical(nh, 1L)) "" else "s", prot_lab(),
        format(nt, big.mark = ","), format(nc, big.mark = ","), pred_lab()))
    })

    # ---- coverage-gap table: protected hotspots ranked by weak nearby trapping (worst first) ----
    gaps <- reactive({
      ik_coverage_gaps(ik_data, .ik_nz(selection()$season), pred_sci(), prot_sci(),
                       as.numeric(input$radius %||% 500), reserve = .ik_nz(selection()$reserve),
                       cross_boundary = isTRUE(input$cross_boundary))
    }) |> bindCache(.ik_nz(selection()$season), .ik_nz(selection()$reserve), input$radius, input$pred, input$prot, isTRUE(input$cross_boundary), ik_active_datasets())
    gaps_shown <- reactiveVal(NULL)   # the gaps rows currently DISPLAYED (filtered/ordered) — for row hover/click

    # The traps reaching a line = the ACTIVE traps (trap_active) within the gap radius of its cameras —
    # the SAME set, by the SAME definition, that the "Traps" column counts (n_traps = active in radius).
    # So the row-hover rings, the drill modal and the table count always agree, and every ringed trap is
    # one of the grey/amber Device markers (no rings over empty ground). Shared by the drill + the hover.
    .line_traps <- function(reserve, line) {
      nbr <- .nbhd_resolve(ik_data, "line", paste(reserve, line, sep = "|"), as.numeric(input$radius %||% 500))
      if (is.null(nbr) || !length(nbr$trap_locs)) return(NULL)
      ta <- trap_active(); if (is.null(ta)) return(NULL)
      d <- ta[ta$location %in% nbr$trap_locs, , drop = FALSE]   # active traps within the radius of the line
      if (!nrow(d)) return(NULL)
      d
    }

    has_camera <- ik_has_source_type(ik_data, "camera")
    output$gaps <- DT::renderDT({
      g <- gaps()
      validate(need(!is.null(g) && nrow(g), if (!has_camera)
        "Coverage gaps compare protected detections on camera with nearby trapping — they need camera monitoring data, which isn't loaded here."
        else "Pick a predator and a protected species."))
      rsv <- .ik_nz(selection()$reserve); if (!is.null(rsv)) g <- g[g$reserve %in% rsv, , drop = FALSE]
      validate(need(nrow(g), "No camera lines in this reserve."))
      gaps_shown(g)                                            # remember the displayed order for row hover/click
      dens <- g$traps_per_km2                                   # PER-LINE density within the gap radius
      df <- data.frame(Line = g$line, Reserve = g$reserve,
        `Protected` = ifelse(is.na(g$prot_rate), "—", sprintf("%.2f", g$prot_rate)),
        `Predator`  = ifelse(is.na(g$pred_rate), "—", sprintf("%.2f", g$pred_rate)),
        `Traps` = g$n_traps, `Caught` = g$catches, `Neglected` = g$n_neglected,
        `Traps/km²` = ifelse(is.na(dens), "—", sprintf("%.0f", dens)),
        .status = g$status, .statuslab = unname(.COV_GAP_LAB[g$status]),   # hidden: drive the row tint + hover title
        check.names = FALSE, stringsAsFactors = FALSE)
      si <- ncol(df) - 2L                                      # 0-based index of .status (then .statuslab)
      DT::datatable(df, container = .cov_gaps_header(line_norm), rownames = FALSE,
        selection = "single", class = "hover row-border ik-row-click",     # no "stripe" — rows are tinted by status
        # row index drives the drill (click) + map highlight (hover); ordering/paging off so the display
        # order matches gaps_shown() one-to-one. createdRow tints the row by status + shows it on hover.
        options = list(dom = "t", ordering = FALSE, paging = FALSE,
          columnDefs = list(list(visible = FALSE, targets = c(si, si + 1L))),
          createdRow = DT::JS(sprintf(
            "function(row,data,i){ if(data[%d]) row.classList.add('ik-gaprow-'+data[%d]); row.title=data[%d]||''; row.addEventListener('mouseenter',function(){Shiny.setInputValue('%s',i+1,{priority:'event'});}); row.addEventListener('mouseleave',function(){Shiny.setInputValue('%s',0,{priority:'event'});}); }",
            si, si, si + 1L, session$ns("gaps_hover"), session$ns("gaps_hover")))))
    })
    gaps_dt_proxy <- DT::dataTableProxy("gaps")

    # Hover a gaps row → highlight that line's neighbourhood traps on the map (bright rings on the top
    # pane), so you can see WHICH traps the Traps/Caught/Neglected numbers count. 0 / empty → clear.
    observeEvent(input$gaps_hover, {
      p <- proxy(); leaflet::clearGroup(p, "GapHighlight")
      i <- input$gaps_hover; g <- gaps_shown()
      if (is.null(i) || i < 1 || is.null(g) || i > nrow(g)) return()
      d <- .line_traps(g$reserve[i], g$line[i]); if (is.null(d) || !nrow(d)) return()
      col <- if (is_dark()) "#4dabf7" else "#1565c0"
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "GapHighlight",
        radius = 9, fill = FALSE, stroke = TRUE, color = col, weight = 3, opacity = 0.95,
        options = leaflet::pathOptions(pane = "highlight"))
    }, ignoreNULL = FALSE)

    # Click a gaps row → the traps reaching that line, in a modal (status · checks · trap-days · caught).
    # The table is a DTOutput rendered server-side (NOT a static DT in the modal body — a freshly-shown
    # modal initialises an htmlwidget while it's still hidden, so a static table renders blank).
    gaps_drill <- reactiveVal(NULL)                           # the clicked line's neighbourhood traps
    observeEvent(input$gaps_rows_selected, {
      i <- input$gaps_rows_selected; g <- gaps_shown(); req(i, !is.null(g), i <= nrow(g))
      DT::selectRows(gaps_dt_proxy, NULL)                     # clear so the same row can be re-clicked
      row <- g[i, , drop = FALSE]
      d <- .line_traps(row$reserve, row$line); gaps_drill(d)
      rad <- as.numeric(input$radius %||% 500)
      rad_lab <- if (rad >= 1000) sprintf("%g km", rad / 1000) else sprintf("%g m", rad)
      body <- if (is.null(d) || !nrow(d))
        tags$p(class = "ik-spp-other", "No traps within this radius of the line's cameras.")
      else DT::DTOutput(session$ns("gaps_drill_table"))
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("Line %s · %s", row$line, row$reserve),
                                sprintf("%d trap%s within %s of the line's cameras", if (is.null(d)) 0L else nrow(d),
                                        if (!is.null(d) && nrow(d) == 1) "" else "s", rad_lab)),
        size = "l", easyClose = TRUE, footer = modalButton("Close"), body))
    })

    output$gaps_drill_table <- DT::renderDT({
      d <- gaps_drill(); req(!is.null(d) && nrow(d))
      st_lab <- c(good = "Good", watch = "Watch", neglected = "Neglected",
                  dormant = "Dormant", historic = "Historic")
      d <- d[order(match(d$status, c("neglected", "watch", "good", "dormant", "historic"))), , drop = FALSE]
      tbl <- data.frame(Trap = d$name, Line = ifelse(is.na(d$line), "—", d$line),
        Status = ifelse(is.na(d$status), "—", unname(st_lab[d$status])),
        Checks = ifelse(is.na(d$n_checks), 0L, as.integer(d$n_checks)),
        `Trap-days` = ifelse(is.na(d$trap_days), 0, round(d$trap_days)),
        Caught = as.integer(d$captures), check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(tbl, rownames = FALSE, class = "stripe hover row-border",
        options = list(dom = "tp", pageLength = 15, order = list(list(5, "desc"))))
    })

    # (The per-reserve network-density table moved to the main Overview → Network density tab.)

    # ---- click a TRAP marker → its full check history (all-time) → Record Details (as on Neighbourhood) ----
    th_loc <- reactiveVal(NULL); th_open <- reactiveVal(NULL)
    observeEvent(input$map_marker_click, {
      cid <- input$map_marker_click$id
      if (is.null(cid) || !startsWith(cid, "T|")) return()       # trap markers only (layerId "T|<loc>")
      loc <- sub("^T\\|", "", cid); th_loc(loc); th_open(NULL)
      al  <- ik_active_locations(ik_data); nm <- al$name[match(loc, al$location_id)]
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s — check history", nm %||% loc),
                                "Every check at this trap (all-time) — click one for its full record."),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("th_tabs"),
          tabPanel("Trap history",   icon = icon("clock-rotate-left"), DT::dataTableOutput(session$ns("th_table"))),
          tabPanel("Record details", icon = icon("circle-info"),       uiOutput(session$ns("th_record"))))))
      hideTab(session = session, inputId = "th_tabs", target = "Record details")
    })
    output$th_table <- DT::renderDT({
      req(th_loc())
      ch <- ik_trap_checks(ik_data, th_loc(), NULL)              # full history, newest first
      validate(need(!is.null(ch) && nrow(ch), "No checks recorded for this trap."))
      df <- data.frame(
        Date = format(ch$check_date, "%d %b %Y"),
        Interval = ifelse(ch$is_first, "—", paste0(ch$interval_days, " d")),
        Outcome = ch$outcome, Bait = ifelse(is.na(ch$bait), "—", ch$bait),
        Volunteer = ifelse(is.na(ch$volunteer), "—", ch$volunteer), ObsID = ch$observationID,
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip",
          columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))   # hide ObsID
    })
    observeEvent(input$th_table_rows_selected, {                 # a check → its record
      i <- input$th_table_rows_selected; ch <- ik_trap_checks(ik_data, th_loc(), NULL)
      if (length(i) && !is.null(ch) && i <= nrow(ch)) {
        th_open(ch$observationID[i]); showTab(session = session, inputId = "th_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("th_table"), NULL)
    })
    observeEvent(input$th_back, updateTabsetPanel(session, input$th_back$tabset, selected = input$th_back$to))
    output$th_record <- renderUI({
      if (is.null(th_open())) return(tags$p(class = "ik-spp-other", "Pick a check from the Trap history tab."))
      ob <- ik_observation(ik_data, th_open()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(session$ns("th_back"), "th_tabs", "Trap history", "Back to trap history"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("th_subtabs")))
    })
  })
}
