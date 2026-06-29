# spatial_explorer.R (module) — "Spatial explorer": an Insights → Combined-analysis CUSTOM map that
# composites camera (Monitoring) + trap (Trapping) data for the SAME species selection onto one
# canvas. Where Predator pressure answers a FIXED question (predators-high × protected-low), the
# explorer is the open-ended "expert mode": pick any species (or group) and read camera DETECTIONS
# against trap CATCHES as separate, value-sized layers over the device field.
#
# Built in phases on the shared map_draw.R draw tier — it owns its own leaflet base and DRAWS only
# through the helpers, exactly like predator_pressure.R (NOT maps_server). Phase 7a (THIS FILE):
# Standard display + Combined species mode — the selected species merged to ONE camera-detection
# value and ONE trap-catch value per location. Later phases add: Separate species mode (per-entry
# value-sized tinted-disc icons), Predator-vs-Protected, the side-by-side + swipe display modes, and
# the chain/padlock comparison lock. Marker → drill modal is stage-two (out of v1).
#
# Layers (native addLayersControl, top-right): Detections (camera RAI) · Catches (trap count) ·
# Cameras / Traps (the device field — faint context, off by default) · Boundary. Detections and
# Catches are ALWAYS separate layers + legends: their units differ (rate vs count), and the device
# is read by COLOUR (teal = camera, amber = trap) with size ∝ value.

.SPEX_DETECT <- "#0d9488"   # teal — camera detections (device colour; value = marker size)
.SPEX_CATCH  <- "#d97706"   # amber — trap catches

#' Body of the Spatial-explorer "how to read this" help modal — tabbed, matching the app help format.
#' `cam_norm` = the per-camera scale the detection rates are shown at (project config). @keywords internal
spatial_explorer_help_body <- function(cam_norm = 500) {
  P  <- function(...) tags$p(...)
  ch <- format(cam_norm, big.mark = ",")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "An ", tags$b("expert-mode"), " map: pick any species (or a group like Mustelids) and see ",
        "where the ", tags$b("cameras"), " detect it against where the ", tags$b("traps"), " catch it — ",
        "the two devices side by side on one canvas, for the sidebar's period and reserve."),
      P(tags$b(tags$span(style = "color:#0d9488", "Detections")), " (teal) are camera relative-activity; ",
        tags$b(tags$span(style = "color:#d97706", "Catches")), " (amber) are trap captures. ",
        tags$b("Colour says which device, size says how much"), "."),
      P(tags$b("Combined"), " mode (this view) merges everything you pick into ", tags$b("one"),
        " detection value and ", tags$b("one"), " catch value per location — so cats + ferrets read as a single ",
        "pest signal. (Per-species icons, predator-vs-protected, and side-by-side comparison are coming.)"),
      P(tags$b("Hint:"), " select a single ", tags$b("Reserve"), " and zoom in — the markers are most readable at reserve scale.")),
    tabPanel(
      "The layers", icon = icon("layer-group"),
      P(tags$br(), "Toggle these top-right."),
      tags$ul(
        tags$li(tags$b(tags$span(style = "color:#0d9488", "Detections")), " — camera relative-activity for the picked species, one marker per camera; size ∝ RAI."),
        tags$li(tags$b(tags$span(style = "color:#d97706", "Catches")), " — trap captures of the picked species, one marker per trap; size ∝ count."),
        tags$li(tags$b("Cameras / Traps"), " — the device field (every active camera / trap), faint context dots. Off by default."),
        tags$li(tags$b("Boundary"), " — the monitored footprint (convex hull of the devices) per reserve; hover for its name."))),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Detections (RAI)"), " — detections of the picked species ÷ that camera's camera-hours × ",
                ch, " (the per-camera scale from config), net of likely duplicates. Combined = all picked species summed."),
        tags$li(tags$b("Catches"), " — captures of the picked species at each trap in the period (raw count). Combined = all picked species summed."),
        tags$li(tags$b("Boundary"), " — the convex hull of the deployed devices, per reserve.")),
      P(tags$em("All for the sidebar's period & reserve; everything redraws when you change them.")))
  )
}

#' Spatial-explorer "View options" sidebar controls — the unified Species picker (the map's subject).
#' Built in the module's namespace, rendered in the global sidebar (ui.R) above the shared Filters;
#' choices are populated server-side. @keywords internal
spatial_explorer_controls <- function(id, ik_data = NULL) {
  ns <- NS(id)
  div(class = "ik-selection ik-view-controls",
    tags$div(class = "ik-view-controls-h", "View options"),
    selectInput(ns("species"), "Species", choices = NULL, multiple = TRUE))
}

#' Spatial-explorer nav panel. @param id Module id. @param ik_data The container (camera-hour scale).
#' @keywords internal
spatial_explorer_ui <- function(id, ik_data = NULL) {
  ns <- NS(id)
  cam_norm <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500
  nav_panel(
    "Spatial explorer", value = "spatial-explorer", icon = icon("layer-group"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),   # .ik-maps-split / -side / records
    tags$script(src = .ik_asset("js/maps.js")),                                              # resize-on-tab-show fix
    div(class = "ik-maps ik-spex ik-map-fill",                  # fill the viewport (map row grows under the header)
        .ik_page_header("Spatial explorer",
            description = "Composite the cameras and the traps for any species onto one map — where it's detected against where it's caught.",
            help = .ik_info(ns("spex_help"), "Spatial explorer — how to read this", spatial_explorer_help_body(cam_norm)),
            banner = div(class = "ik-page-period", uiOutput(ns("period_banner")))),
        uiOutput(ns("caption")),
        layout_columns(class = "ik-maps-split", col_widths = breakpoints(sm = 12, lg = c(8, 4)),
          leaflet::leafletOutput(ns("map"), height = "100%"),
          div(class = "ik-maps-side",                           # height fills the row via .ik-map-fill
            div(class = "ik-maps-records",
                uiOutput(ns("drill_chip")),
                div(class = "ik-maps-records-header",
                    uiOutput(ns("records_caption")),
                    downloadButton(ns("download_csv"), "Download CSV", class = "btn-sm")),
                DT::DTOutput(ns("table"))))),
        uiOutput(ns("unplaced")))
  )
}

#' Spatial-explorer server.
#' @param id Module id. @param ik_data The container. @param prefer_scientific reactive name pref.
#' @param selection reactive selection SPEC (Period + Reserve from the sidebar). @param color_mode theme.
#' @param active reactive, TRUE when this tab is current (gates the heavy metric reactives).
spatial_explorer_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                                    selection = reactive(list()), color_mode = reactive("light"),
                                    active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    output$period_banner <- renderUI(.ik_period_banner(ik_data, selection()))
    is_dark <- reactive(identical(color_mode(), "dark"))
    prefer  <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")

    per_cam  <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500   # per-camera marker scale
    grp_taxa <- ik_group_taxa(ik_data)
    all_sci  <- sort(unique(stats::na.omit(ik_observations(ik_data, with_location = FALSE)$scientificName)))

    # ---- the unified Species picker (group whole / split sub-species / ungrouped); "All species" default ----
    observe({ p <- prefer()
      sel <- isolate(input$species); if (!length(sel)) sel <- "__all__"
      updateSelectInput(session, "species",
        choices  = ik_species_choices_full(ik_data, p, all_label = "All species", all_value = "__all__"),
        selected = sel)
    })
    sp_sci <- reactive({ v <- input$species
      if (!length(v) || "__all__" %in% v) all_sci else ik_resolve_species_choice(v, grp_taxa) })
    sp_lab <- reactive({ v <- input$species
      if (!length(v) || "__all__" %in% v) "all species" else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })

    # ---- combined per-location values: ONE camera RAI + ONE trap count per location ----
    # The whole picked set is passed as a single taxa group, so .metrics_grouped sums it to one value.
    detect_all <- reactive({ req(active()); sci <- sp_sci(); if (!length(sci)) return(NULL)
      ik_location_metric(ik_data, selection(), list(Selected = sci), "camera", norm = per_cam) })
    catch_all  <- reactive({ req(active()); sci <- sp_sci(); if (!length(sci)) return(NULL)
      ik_location_metric(ik_data, selection(), list(Selected = sci), "trap") })
    .placed    <- function(m) if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE]
    detect_pts <- reactive(.placed(detect_all()))
    catch_pts  <- reactive(.placed(catch_all()))

    proxy <- function() leaflet::leafletProxy("map", session)

    # ---- drill state (map ↔ table) ----
    selected <- reactiveVal(NULL)   # NULL | list(id, label) — a clicked marker's location
    observeEvent(list(input$species, selection()), selected(NULL), ignoreInit = TRUE)

    # ---- base map ----
    output$map <- leaflet::renderLeaflet({
      locs <- ik_data$app$geography$locations
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      ik_map_base(panes = c("boundary", "cameras", "traps", "detections", "catches", "selected"),  # value markers above context
        overlay_groups = c("Detections", "Catches", "Cameras", "Traps", "Boundary"),
        is_dark = isolate(is_dark()), fit = locs, hide_groups = c("Cameras", "Traps"), pane_z0 = 410)
    })
    outputOptions(output, "map", suspendWhenHidden = FALSE)   # render from load so proxy layers land

    observeEvent(color_mode(), ik_swap_theme_tiles(proxy(), is_dark()), ignoreInit = TRUE)

    observe({                                                 # re-frame to the data extent of the selection
      pts <- rbind(
        { d <- detect_pts(); if (is.null(d)) NULL else d[, c("longitude", "latitude")] },
        { c <- catch_pts();  if (is.null(c)) NULL else c[, c("longitude", "latitude")] })
      if (is.null(pts) || !nrow(pts)) return(); p <- proxy()
      if (nrow(pts) == 1 || (diff(range(pts$longitude)) == 0 && diff(range(pts$latitude)) == 0))
        leaflet::setView(p, pts$longitude[1], pts$latitude[1], zoom = 14)
      else leaflet::fitBounds(p, min(pts$longitude), min(pts$latitude), max(pts$longitude), max(pts$latitude),
        options = list(padding = c(30, 30)))
    })

    reserve_hulls <- reactive({ req(active()); ik_reserve_boundary(ik_data, selection()$reserve) })

    # ---- Cameras / Traps — the device field, faint context (off by default) ----
    .dev_context <- function(source) {
      locs <- ik_active_locations(ik_data, source)
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      rsv  <- .ik_nz(selection()$reserve); if (!is.null(rsv)) locs <- locs[locs$reserve %in% rsv, , drop = FALSE]
      if (!nrow(locs)) NULL else locs
    }
    observe(ik_draw_device_layer(proxy(), .dev_context("camera"), fill_color = .SPEX_DETECT,
      group = "Cameras", pane = "cameras", radius = 3, fill_opacity = 0.35, label = ~name))
    observe(ik_draw_device_layer(proxy(), .dev_context("trap"), fill_color = .SPEX_CATCH,
      group = "Traps", pane = "traps", radius = 3, fill_opacity = 0.35, label = ~name))

    # ---- Detections — combined camera RAI (teal, size ∝ RAI) ----
    observe({
      d <- detect_pts(); d <- if (is.null(d)) NULL else d[is.finite(d$metric) & d$metric > 0, , drop = FALSE]
      ik_draw_metric_markers(proxy(), d, value = d$metric, group = "Detections",
        layerId = paste0("C|", d$location_id), lo = 4, hi = 15, cap_pctl = 0.98,
        fill_color = .SPEX_DETECT, fill_opacity = 0.85, color = if (is_dark()) "#1a1a1a" else "#ffffff",
        weight = 1.4, pane = "detections",
        label = sprintf("%s — %s RAI %.2f", d$name, sp_lab(), d$metric))
    })

    # ---- Catches — combined trap captures (amber, size ∝ count) ----
    observe({
      d <- catch_pts(); d <- if (is.null(d)) NULL else d[is.finite(d$captures) & d$captures > 0, , drop = FALSE]
      ik_draw_metric_markers(proxy(), d, value = d$captures, group = "Catches",
        layerId = paste0("K|", d$location_id), lo = 3, hi = 12, cap = ik_robust_cap(d$captures, 0.9),
        fill_color = .SPEX_CATCH, fill_opacity = 0.8, color = "#ffffff", weight = 1, pane = "catches",
        label = sprintf("%s — %s caught %d", d$name, sp_lab(), as.integer(d$captures)))
    })

    observe({                                                 # Boundary — the shared reserve footprint
      p <- proxy(); leaflet::clearGroup(p, "Boundary")
      ik_add_reserve_boundary(p, reserve_hulls(), color = if (is_dark()) "#cfd8dc" else "#37474f")
    })

    observe({                                                 # Selected highlight (a clicked location)
      sel <- selected(); locs <- ik_data$app$geography$locations
      i <- if (is.null(sel)) NA_integer_ else match(sel$id, locs$location_id)
      ik_draw_selection_ring(proxy(),
        if (is.na(i)) NULL else locs$longitude[i],
        if (is.na(i)) NULL else locs$latitude[i])
    })

    observe({                                                 # legend: the device key (colour = device, size = value)
      p <- proxy(); leaflet::clearControls(p)
      d <- detect_pts(); cc <- catch_pts()
      has_d <- !is.null(d)  && any(d$metric   > 0, na.rm = TRUE)
      has_c <- !is.null(cc) && any(cc$captures > 0, na.rm = TRUE)
      if (!has_d && !has_c) return()
      leaflet::addLegend(p, "bottomright", colors = c(.SPEX_DETECT, .SPEX_CATCH),
        labels = c("Camera detections (size &prop; RAI)", "Trap catches (size &prop; count)"),
        title = sprintf("%s &middot; combined", sp_lab()), opacity = 0.9)
    })

    observeEvent(input$map_marker_click, {                    # marker → filter table to that location
      cid <- input$map_marker_click$id; if (is.null(cid)) return()
      loc <- sub("^[CK]\\|", "", cid); locs <- ik_data$app$geography$locations
      selected(list(id = loc, label = locs$name[match(loc, locs$location_id)] %||% loc))
    })

    # ---- combined records table: a UNION of locations (camera RAI and/or trap count) ----
    combined <- reactive({
      d <- detect_pts(); cc <- catch_pts()
      dd <- if (is.null(d))  NULL else data.frame(location_id = d$location_id, name = d$name,
        reserve = d$reserve, line = d$line, rai = d$metric, captures = NA_real_, stringsAsFactors = FALSE)
      tt <- if (is.null(cc)) NULL else data.frame(location_id = cc$location_id, name = cc$name,
        reserve = cc$reserve, line = cc$line, rai = NA_real_, captures = cc$captures, stringsAsFactors = FALSE)
      m <- rbind(dd, tt); if (is.null(m) || !nrow(m)) return(NULL)
      # a site with BOTH a camera and a trap appears twice — collapse to one row, keeping each value.
      do.call(rbind, lapply(split(seq_len(nrow(m)), m$location_id), function(ix) {
        g <- m[ix, , drop = FALSE]
        g$rai[1]      <- suppressWarnings(max(g$rai,      na.rm = TRUE))
        g$captures[1] <- suppressWarnings(max(g$captures, na.rm = TRUE))
        g[1, , drop = FALSE]
      })) -> ag
      ag$rai[!is.finite(ag$rai)] <- NA; ag$captures[!is.finite(ag$captures)] <- NA
      ag[order(-ifelse(is.na(ag$rai), 0, ag$rai), -ifelse(is.na(ag$captures), 0, ag$captures)), , drop = FALSE]
    })

    output$drill_chip <- renderUI({
      sel <- selected(); if (is.null(sel)) return(NULL)
      div(class = "ik-maps-drill", icon("location-dot"), sprintf(" Showing: %s ", sel$label),
          actionLink(session$ns("drill_clear"), "show all", class = "ik-maps-drill-clear"))
    })
    observeEvent(input$drill_clear, selected(NULL))

    output$records_caption <- renderUI({
      d <- combined(); n <- if (is.null(d)) 0L else nrow(d)
      tags$small(sprintf("%s location%s with %s detections or catches. Click a marker to filter.",
                         format(n, big.mark = ","), if (n == 1L) "" else "s", sp_lab()))
    })

    output$table <- DT::renderDT({
      d <- combined(); sel <- selected()
      if (!is.null(sel)) d <- d[d$location_id == sel$id, , drop = FALSE]
      validate(need(!is.null(d) && nrow(d), "No detections or catches in the current selection."))
      df <- data.frame(Location = d$name, Reserve = d$reserve, Line = d$line,
        Detections = ifelse(is.na(d$rai), NA, round(d$rai, 2)),
        Catches = ifelse(is.na(d$captures), NA, as.integer(d$captures)),
        check.names = FALSE, stringsAsFactors = FALSE)
      names(df)[4] <- "Detections (RAI)"; names(df)[5] <- "Catches (n)"
      DT::datatable(df, rownames = FALSE, selection = "none", class = "stripe hover row-border",
        options = list(pageLength = 8, scrollX = TRUE, dom = "ftip"))
    })

    output$caption <- renderUI({
      d <- detect_pts(); cc <- catch_pts()
      nd <- if (is.null(d))  0L else sum(d$metric   > 0, na.rm = TRUE)
      nc <- if (is.null(cc)) 0L else sum(cc$captures > 0, na.rm = TRUE)
      tags$p(class = "ik-maps-meta", sprintf(
        "%s camera site%s detecting %s · %s trap%s catching it. Toggle layers top-right; click a marker to filter the table.",
        format(nd, big.mark = ","), if (identical(nd, 1L)) "" else "s", sp_lab(),
        format(nc, big.mark = ","), if (identical(nc, 1L)) "" else "s"))
    })

    output$unplaced <- renderUI({
      du <- { m <- detect_all(); if (is.null(m)) 0L else sum(!(is.finite(m$latitude) & is.finite(m$longitude))) }
      cu <- { m <- catch_all();  if (is.null(m)) 0L else sum(!(is.finite(m$latitude) & is.finite(m$longitude))) }
      n <- du + cu; if (!n) return(NULL)
      div(class = "ik-maps-unplaced", icon("triangle-exclamation"),
          sprintf(" %s coordless site%s not shown on the map (no location fix).",
                  format(n, big.mark = ","), if (n == 1L) "" else "s"))
    })

    output$download_csv <- downloadHandler(
      filename = function() sprintf("spatial-explorer-%s.csv", Sys.Date()),
      content = function(file) { d <- combined(); if (is.null(d)) d <- data.frame(); utils::write.csv(d, file, row.names = FALSE) })
  })
}
