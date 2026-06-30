# predator_pressure.R (module) — "Predator pressure": an Insights → Combined-analysis map that
# synthesises camera + trap data into a single "where to act" picture. The core is the camera-derived
# PRIORITY surface (the frontier the Monitoring map used to carry as its "Priority" measure):
#
#   priority(camera) = norm(predator RAI) × (1 − norm(protected RAI))
#
# i.e. HIGH (red) where the chosen predator is active AND the chosen protected species is scarce —
# the places a manager should worry about. It's interpolated between cameras (IDW) into a shaded
# field, with the per-camera scores as markers on top. Onto that camera story we lay a TOGGLEABLE
# TRAPPING OVERLAY — where catches are actually happening (purple) and the active trap field (grey/
# amber neglected) — so you can read pressure against the control that's reaching it: red surface with
# no purple/grey over it is an uncovered frontier. Best zoomed into one reserve.
#
# Camera priority = per LOCATION (its own selling point vs the per-LINE Coverage-gaps view). Period +
# Reserve come from the sidebar; the predator/protected role pickers are the in-panel "View options".
# Reuses ik_location_metric() (camera rate / trap captures), ik_idw_surface(), ik_trap_review() — no
# new data layer; the priority composite is the only bespoke maths, lifted verbatim from maps.R.

.PP_RAMP <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c")   # yellow→red — pressure/danger (shared with maps priority)

#' Body of the Predator-pressure "how to read this" help modal — tabbed, matching the app help format.
#' `cam_norm` = camera-hours the detection rates are scaled to (project config). @keywords internal
predator_pressure_help_body <- function(cam_norm = 500) {
  P  <- function(...) tags$p(...)
  ch <- format(cam_norm, big.mark = ",")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "A single ", tags$b("where to act"), " picture. The shaded field is ", tags$b("predator pressure"),
        " from the cameras — high (", tags$span(style = "color:#e31a1c", "red"), ") where the chosen ",
        tags$b("predator"), " is active ", tags$b("and"), " the chosen ", tags$b("protected"),
        " species is scarce. Onto it you can lay where trapping is actually reaching."),
      P("It synthesises both devices: the ", tags$b("cameras"), " say where the pressure is, the ", tags$b("traps"),
        " say where control is happening. A red patch with no catches or traps over it is an ",
        tags$b("uncovered frontier"), " — somewhere predators are winning and nothing's pushing back."),
      P(tags$b("Hint:"), " select a single ", tags$b("Reserve"), " and zoom in — the surface and the per-camera ",
        "scores are most readable at reserve scale.")),
    tabPanel(
      "The layers", icon = icon("layer-group"),
      P(tags$br(), "Toggle these top-right; ", tags$b("colour says what a marker is, size says how much"), "."),
      tags$ul(
        tags$li(tags$b("Pressure surface"), " — the camera priority field, interpolated between cameras (",
                tags$span(style = "color:#fecc5c", "yellow"), " → ", tags$span(style = "color:#e31a1c", "red"),
                "). Exploratory — a guide to where to look, not a number to cite."),
        tags$li(tags$b("Cameras"), " — each camera's own priority score, same colour ramp. Click one for the ",
                "detections behind it."),
        tags$li(tags$b(tags$span(style = "color:#6a3d9a", "Catches")), " (traps) — purple dots, size ∝ predators caught. Where removal is happening."),
        tags$li(tags$b("Traps"), " — the active trap field this period: ", tags$span(style = "color:#8a8a8a", "grey"),
                " in service, ", tags$span(style = "color:#f59f00", "amber"), " neglected. Off by default (it's the dense context). Click for check history."),
        tags$li(tags$b("Boundary"), " — the monitored footprint (convex hull of the devices) per reserve; hover for its name."))),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Predator / protected RAI"), " — per camera, detections of the chosen group ÷ its camera-hours × ",
                ch, " (the per-camera scale from config), net of likely duplicates."),
        tags$li(tags$b("Priority"), " — each camera's predator RAI and protected RAI are scaled to the busiest camera ",
                "in the current selection, then ", tags$b("priority = predator(scaled) × (1 − protected(scaled))"),
                " — high only where predators are high AND protected low."),
        tags$li(tags$b("Surface"), " — inverse-distance interpolation of those per-camera scores, per reserve."),
        tags$li(tags$b("Catches"), " — the chosen predator caught at each trap in the period."),
        tags$li(tags$b("Traps"), " — the active field from the servicing review (good / watch / neglected; dormant & historic excluded).")),
      P(tags$em("All for the sidebar's period & reserve; everything redraws when you change them.")))
  )
}

#' Predator-pressure "View options" sidebar controls — the Predator / Protected role pickers (the map
#' measure). Built in the module's namespace, rendered in the global sidebar (ui.R) above the shared
#' Filters; choices are populated server-side. @keywords internal
predator_pressure_controls <- function(id, ik_data = NULL) {
  ns <- NS(id)
  div(class = "ik-selection ik-view-controls",
    tags$div(class = "ik-view-controls-h", "View options"),
    selectInput(ns("pred"), "Predator(s)", choices = NULL, multiple = TRUE),
    selectInput(ns("prot"), "Protected",   choices = NULL, multiple = TRUE))
}

#' Predator-pressure nav panel. @param id Module id. @param ik_data The container (camera-hour norm).
#' @keywords internal
predator_pressure_ui <- function(id, ik_data = NULL) {
  ns <- NS(id)
  cam_norm <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500
  nav_panel(
    "Predator pressure", value = "predator-pressure", icon = icon("crosshairs"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),   # .ik-maps-split / -side / records
    tags$script(src = .ik_asset("js/maps.js")),                                              # resize-on-tab-show fix
    div(class = "ik-maps ik-pp ik-map-fill",                     # fill the viewport (map row grows under the header)
        .ik_page_header("Predator pressure",
            description = "Where are predators winning ground from the protected species — and is trapping reaching those places?",
            help = .ik_info(ns("pp_help"), "Predator pressure — how to read this", predator_pressure_help_body(cam_norm)),
            banner = div(class = "ik-page-period", uiOutput(ns("period_banner")))),
        uiOutput(ns("caption")),
        layout_columns(class = "ik-maps-split", col_widths = breakpoints(sm = 12, lg = c(8, 4)),
          leaflet::leafletOutput(ns("map"), height = "100%"),
          div(class = "ik-maps-side",                            # height fills the row via .ik-map-fill
            div(class = "ik-maps-records",
                uiOutput(ns("drill_chip")),
                div(class = "ik-maps-records-header",
                    uiOutput(ns("records_caption")),
                    downloadButton(ns("download_csv"), "Download CSV", class = "btn-sm")),
                DT::DTOutput(ns("table"))))),
        uiOutput(ns("unplaced")))
  )
}

#' Predator-pressure server.
#' @param id Module id. @param ik_data The container. @param prefer_scientific reactive name pref.
#' @param selection reactive selection SPEC (Period + Reserve from the sidebar). @param color_mode theme.
#' @param active reactive, TRUE when this tab is current (gates the heavy metric/IDW reactives).
predator_pressure_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                                     selection = reactive(list()), color_mode = reactive("light"),
                                     active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    output$period_banner <- renderUI(.ik_period_banner(ik_data, selection()))
    is_dark <- reactive(identical(color_mode(), "dark"))
    prefer  <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")

    sg <- ik_species_groups(ik_data)
    .role_taxa <- function(role) { l <- unique(sg$label[sg$role == role & !is.na(sg$monitor)])
      stats::setNames(lapply(l, function(x) sg$scientificName[sg$label == x & !is.na(sg$scientificName)]), l) }
    pred_taxa <- .role_taxa("predator"); prot_taxa <- .role_taxa("protected")
    splits    <- unique(sg$label[which(sg$split)])
    .pred_def <- paste0("grp:", names(pred_taxa)[1])
    .prot_def <- paste0("grp:", names(prot_taxa)[1])
    per_cam   <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500    # per-camera marker scale

    observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      updateSelectInput(session, "pred", choices = ik_species_choices(pred_taxa, ik_data, p, splits), selected = keep(isolate(input$pred), .pred_def))
      updateSelectInput(session, "prot", choices = ik_species_choices(prot_taxa, ik_data, p, splits), selected = keep(isolate(input$prot), .prot_def))
    })

    pred_sci <- reactive(ik_resolve_species_choice(input$pred, pred_taxa))
    prot_sci <- reactive(ik_resolve_species_choice(input$prot, prot_taxa))
    pred_lab <- reactive({ v <- input$pred; if (!length(v)) "predator"  else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })
    prot_lab <- reactive({ v <- input$prot; if (!length(v)) "protected" else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })
    has_pick <- reactive(length(pred_sci()) > 0 && length(prot_sci()) > 0)

    # ---- the camera priority composite (per location) — the shared ik_priority_metric() ----
    # predators-high × protected-low → "where to act", normalised across the selection's cameras.
    prio_all <- reactive({ req(active(), has_pick())
      ik_priority_metric(ik_data, selection(), pred_sci(), prot_sci(), norm = per_cam) })
    prio_pts      <- reactive({ m <- prio_all(); if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
    prio_unplaced <- reactive({ m <- prio_all(); if (is.null(m)) NULL else m[!(is.finite(m$latitude) & is.finite(m$longitude)), , drop = FALSE] })

    # ---- the trapping overlay (where control is reaching) ----
    trap_pred <- reactive({ req(active()); if (!length(pred_sci())) return(NULL)
      m <- ik_location_metric(ik_data, selection(), list(P = pred_sci()), "trap")
      if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
    # the active trap FIELD for the period (good/watch/neglected), from the servicing review — the same
    # set Coverage uses; captures of the chosen predator joined on, so a grey dot's catch matches Catches.
    trap_active <- reactive({ req(active())
      sv <- ik_trap_review(ik_data, seasons = .ik_nz(selection()$season)); if (is.null(sv)) return(NULL)
      sv <- sv[!sv$status %in% c("dormant", "historic") & is.finite(sv$latitude) & is.finite(sv$longitude), , drop = FALSE]
      rsv <- .ik_nz(selection()$reserve); if (!is.null(rsv)) sv <- sv[sv$reserve %in% rsv, , drop = FALSE]
      if (!nrow(sv)) return(NULL)
      tp <- trap_pred(); sv$captures <- if (is.null(tp)) 0L else tp$captures[match(sv$location, tp$location_id)]
      sv$captures[is.na(sv$captures)] <- 0L
      sv })

    proxy    <- function() leaflet::leafletProxy("map", session)   # .robust_cap/surf_pal removed — now ik_max_cap + the shared draw helpers

    # ---- drill state (map ↔ table) ----
    selected    <- reactiveVal(NULL)   # NULL | list(kind="location", id, label) — a clicked camera
    prio_tbl    <- reactiveVal(NULL)   # the priority rows currently shown (ordered) — for the row drill
    prio_rec    <- reactiveVal(NULL)   # predator+protected detections behind a clicked priority camera
    prio_recobs <- reactiveVal(NULL)   # the record open in the drill modal's Record-details tab
    observeEvent(list(input$pred, input$prot, selection()), selected(NULL), ignoreInit = TRUE)

    # ---- base map ----
    output$map <- leaflet::renderLeaflet({
      locs <- ik_data$app$geography$locations
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      ik_map_base(panes = c("surface", "boundary", "traps", "catches", "cameras", "selected"),   # surface lowest; selected on TOP
        overlay_groups = c("Pressure surface", "Cameras", "Catches", "Traps", "Boundary"),
        is_dark = isolate(is_dark()), fit = locs, hide_groups = "Traps", pane_z0 = 410)
    })
    outputOptions(output, "map", suspendWhenHidden = FALSE)      # render from load so proxy layers land

    observeEvent(color_mode(), ik_swap_theme_tiles(proxy(), is_dark()), ignoreInit = TRUE)

    observe({                                                    # re-frame to the camera extent of the selection
      d <- prio_pts(); if (is.null(d) || !nrow(d)) return(); p <- proxy()
      if (nrow(d) == 1 || (diff(range(d$longitude)) == 0 && diff(range(d$latitude)) == 0))
        leaflet::setView(p, d$longitude[1], d$latitude[1], zoom = 14)
      else leaflet::fitBounds(p, min(d$longitude), min(d$latitude), max(d$longitude), max(d$latitude), options = list(padding = c(30, 30)))
    })

    # The constant reserve footprint (shared helper) — the Boundary outline + the Surface clip.
    reserve_hulls <- reactive({ req(active()); ik_reserve_boundary(ik_data, selection()$reserve) })

    # ---- Pressure surface (IDW; only computed when the layer is shown) ----
    surface_on <- reactiveVal(TRUE)
    observe({ v <- { g <- input$map_groups; if (is.null(g)) TRUE else "Pressure surface" %in% g }
      if (!identical(v, surface_on())) surface_on(v) })
    surface_compute <- reactive({ d <- prio_pts(); if (is.null(d) || !nrow(d)) return(NULL)
      d <- d[!d$reserve %in% c(GEO_UNPLACED_RESERVE, GEO_OUTSIDE_RESERVE), , drop = FALSE]   # no scattered pseudo-reserve blob
      if (!nrow(d)) return(NULL)
      ik_idw_surface(d, "metric", "reserve") }) |> bindCache(prio_pts())
    surface_idw <- reactive(if (isTRUE(surface_on())) surface_compute() else NULL)
    observe({
      s <- ik_clip_surface_to_reserves(surface_idw(), reserve_hulls())   # confine the field to the reserve
      ik_draw_idw_surface(proxy(), s, group = "Pressure surface", pal = .PP_RAMP, is_dark = is_dark())
    })

    # ---- Cameras — per-camera priority score (same ramp as the surface) ----
    observe({
      d <- prio_pts(); d <- if (is.null(d)) NULL else d[is.finite(d$metric) & d$metric > 0, , drop = FALSE]
      ik_draw_metric_markers(proxy(), d, value = d$metric, group = "Cameras", layerId = paste0("C|", d$location_id),
        lo = 5, hi = 20, cap_pctl = 0.98, pal = .PP_RAMP,
        fill_opacity = 0.85, color = if (is_dark()) "#1a1a1a" else "#ffffff", weight = 1.5, pane = "cameras",
        label = sprintf("%s — Line %s", d$name, ifelse(is.na(d$line), "—", d$line)))
        # No click popup — a marker click opens the detections modal; the detail popup shows on table-row hover.
    })

    # ---- Catches — traps that caught the chosen predator (purple, count scale) ----
    # NB: layerId prefix "K|" (not "T|") so a catch marker and the SAME trap's grey field marker (group
    # "Traps", "T|") don't share an id — a shared id lets the hidden Traps layer clobber the catch (leaflet
    # replaces a duplicate layerId), which made the whole Catches layer vanish. Both still open check history.
    observe({
      d <- trap_pred(); d <- if (is.null(d)) NULL else d[is.finite(d$captures) & d$captures > 0, , drop = FALSE]
      ik_draw_metric_markers(proxy(), d, value = d$captures, group = "Catches", layerId = paste0("K|", d$location_id),
        lo = 5, hi = 16, cap = ik_robust_cap(d$captures, 0.9), fill_color = "#6a3d9a",
        fill_opacity = 0.85, color = "#ffffff", weight = 1, pane = "catches",
        label = sprintf("%s — %s caught: %d · click for check history", d$name, pred_lab(), as.integer(d$captures)))
    })

    # ---- Traps — the active field (grey, neglected amber); off by default ----
    observe({
      d <- trap_active(); if (is.null(d) || !nrow(d)) { leaflet::clearGroup(proxy(), "Traps"); return() }
      st_lab <- c(good = "Good", watch = "Watch", neglected = "Neglected")
      lab <- sprintf("%s — %s · %d check%s · %s trap-days · %d caught · click for check history",
                     d$name, ifelse(is.na(d$status), "—", unname(st_lab[d$status])),
                     as.integer(d$n_checks), ifelse(d$n_checks == 1L, "", "s"),
                     format(round(d$trap_days), big.mark = ","), as.integer(d$captures))
      ik_draw_device_layer(proxy(), d, fill_color = ifelse(d$status == "neglected", "#f59f00", "#8a8a8a"),
        group = "Traps", pane = "traps", radius = 3, fill_opacity = 0.6, label = lab, layerId = paste0("T|", d$location))
    })

    observe({                                                    # Boundary — the shared reserve footprint draw
      p <- proxy(); leaflet::clearGroup(p, "Boundary")
      ik_add_reserve_boundary(p, reserve_hulls(), color = if (is_dark()) "#cfd8dc" else "#37474f")
    })

    observe({                                                    # Selected highlight (a clicked camera)
      sel <- selected(); locs <- ik_data$app$geography$locations
      i <- if (is.null(sel)) NA_integer_ else match(sel$id, locs$location_id)
      ik_draw_selection_ring(proxy(),
        if (is.na(i)) NULL else locs$longitude[i],
        if (is.na(i)) NULL else locs$latitude[i])
    })

    observe({                                                    # legends: pressure gradient + trapping key
      p <- proxy(); leaflet::clearControls(p)
      d <- prio_pts(); if (is.null(d) || !nrow(d)) return()
      cap <- ik_max_cap(d$metric); pf <- leaflet::colorNumeric(.PP_RAMP, c(0, cap))
      ik_draw_metric_legend(p, pf, pmin(d$metric, cap),
        sprintf("Predator pressure<br><span style='font-weight:400;font-size:.85em'>%s high</span><br><span style='font-weight:400;font-size:.85em'>%s low</span>",
                htmltools::htmlEscape(pred_lab()), htmltools::htmlEscape(prot_lab())))
      leaflet::addLegend(p, "bottomleft", colors = c("#6a3d9a", "#8a8a8a", "#f59f00"),
        labels = c(sprintf("%s caught (traps)", pred_lab()), "Traps in service", "Traps neglected"),
        title = "Trapping overlay", opacity = 0.9)
    })

    observeEvent(input$map_marker_click, {                       # camera → detections modal · trap → check history
      cid <- input$map_marker_click$id; if (is.null(cid)) return()
      if (startsWith(cid, "C|")) .open_pp_modal(sub("^C\\|", "", cid))   # straight to the modal, no table step
      # trap markers (T|...) are handled by the check-history observer below.
    })

    # ---- records table: the per-camera priority rows ----
    output$drill_chip <- renderUI({
      sel <- selected(); if (is.null(sel)) return(NULL)
      div(class = "ik-maps-drill", icon("location-dot"), sprintf(" Showing: %s ", sel$label),
          actionLink(session$ns("drill_clear"), "show all", class = "ik-maps-drill-clear"))
    })
    observeEvent(input$drill_clear, selected(NULL))

    output$records_caption <- renderUI({
      if (!has_pick()) return(tags$small("Pick a predator and a protected species to compare."))
      d <- prio_pts(); n <- if (is.null(d)) 0L else nrow(d)
      tags$small(sprintf("%s camera site%s — reddest = highest predator pressure (more %s, fewer %s). Click a row for the detections behind it.",
                         format(n, big.mark = ","), if (n == 1L) "" else "s", pred_lab(), prot_lab()))
    })

    output$table <- DT::renderDT({
      validate(need(has_pick(), "Pick a predator and a protected species to compare."))
      d <- prio_pts(); sel <- selected()
      if (!is.null(sel) && identical(sel$kind, "location")) d <- d[d$location_id == sel$id, , drop = FALSE]
      validate(need(!is.null(d) && nrow(d), "No camera sites in the current selection."))
      d <- d[order(-d$metric), , drop = FALSE]; prio_tbl(d)        # priority order — highest (reddest) first
      cap <- ik_max_cap(d$metric); if (!is.finite(cap) || cap <= 0) cap <- 1
      rg  <- grDevices::col2rgb(leaflet::colorNumeric(.PP_RAMP, c(0, cap))(pmin(d$metric, cap)))
      df <- data.frame(
        Camera = d$name, Reserve = d$reserve, Line = ifelse(is.na(d$line), "—", as.character(d$line)),
        `Predator RAI` = round(d$predator, 2), `Protected RAI` = round(d$protected, 2),
        .col = sprintf("rgba(%d,%d,%d,0.5)", rg["red", ], rg["green", ], rg["blue", ]),  # hidden: priority tint, red=high
        .loc = d$location_id,                                       # hidden: drives the hover → marker highlight
        check.names = FALSE, stringsAsFactors = FALSE)
      ci <- which(names(df) == ".col") - 1L; li <- which(names(df) == ".loc") - 1L        # 0-based DT targets
      # FIXED column names (not the picked species, which change with the selection and tripped DataTables
      # — "column 'Stoat RAI' not found"); the species are named in the caption + legend instead. Rows
      # arrive priority-sorted, ordering off so the red→yellow tint reads top-to-bottom. No Pressure column
      # (the raw score is opaque) — createdRow paints each row by priority and wires hover → map highlight.
      DT::datatable(df, rownames = FALSE, selection = "single", class = "hover row-border ik-row-click",
        options = list(pageLength = 8, scrollX = TRUE, dom = "ftip", ordering = FALSE,
          columnDefs = list(list(visible = FALSE, targets = c(ci, li))),
          # tint the CELLS, not the <tr> (bslib paints td backgrounds over the row, hiding a tr tint)
          createdRow = DT::JS(sprintf(
            "function(row,data,i){var bg=data[%d];for(var k=0;k<row.children.length;k++){row.children[k].style.backgroundColor=bg;}var L=data[%d];row.addEventListener('mouseenter',function(){Shiny.setInputValue('%s',L,{priority:'event'});});row.addEventListener('mouseleave',function(){Shiny.setInputValue('%s','',{priority:'event'});});}",
            ci, li, session$ns("pp_hover"), session$ns("pp_hover")))))
    })
    prio_dt_proxy <- DT::dataTableProxy("table")

    # Hover a priority row → ring that camera's marker AND pop its detail on the map (so the hovered row
    # is obvious, not just a faint ring). Empty clears both.
    pp_hover <- shiny::debounce(reactive(input$pp_hover), 100)
    observeEvent(pp_hover(), {
      p <- proxy(); leaflet::clearGroup(p, "PPHighlight")
      loc <- pp_hover(); if (is.null(loc) || !nzchar(loc)) return()
      d <- prio_pts(); if (is.null(d)) return()
      r <- d[d$location_id == loc, , drop = FALSE]
      if (!nrow(r) || !is.finite(r$longitude[1])) return()
      leaflet::addCircleMarkers(p, lng = r$longitude[1], lat = r$latitude[1], group = "PPHighlight",
        radius = 11, fill = FALSE, stroke = TRUE, weight = 3, opacity = 0.95,
        color = if (is_dark()) "#4dabf7" else "#1565c0", options = leaflet::pathOptions(pane = "selected"))
      leaflet::addPopups(p, lng = r$longitude[1], lat = r$latitude[1], group = "PPHighlight",
        popup = sprintf("<b>%s</b><br/>Line %s &middot; %s<br/>%s RAI: %.2f &middot; %s RAI: %.2f",
          r$name[1], ifelse(is.na(r$line[1]), "—", r$line[1]), r$reserve[1], pred_lab(), r$predator[1], prot_lab(), r$protected[1]),
        options = leaflet::popupOptions(closeButton = FALSE, autoPan = FALSE))
    }, ignoreNULL = FALSE)

    output$caption <- renderUI({
      if (!has_pick()) return(NULL)
      d <- prio_pts(); nh <- if (is.null(d)) 0L else sum(d$metric > 0, na.rm = TRUE)
      ca <- trap_pred(); nc <- if (is.null(ca)) 0L else sum(ca$captures > 0, na.rm = TRUE)
      tags$p(class = "ik-maps-meta", sprintf(
        "%s camera site%s under pressure from %s · %s trap%s caught %s. Toggle layers top-right; click a marker for detail.",
        format(nh, big.mark = ","), if (identical(nh, 1L)) "" else "s", pred_lab(),
        format(nc, big.mark = ","), if (identical(nc, 1L)) "" else "s", pred_lab()))
    })

    output$unplaced <- renderUI({
      u <- prio_unplaced(); if (is.null(u) || !nrow(u)) return(NULL)
      div(class = "ik-maps-unplaced", icon("triangle-exclamation"),
          sprintf(" %s coordless site%s not shown on the map (no location fix).",
                  format(nrow(u), big.mark = ","), if (nrow(u) == 1L) "" else "s"))
    })

    output$download_csv <- downloadHandler(
      filename = function() sprintf("predator-pressure-%s.csv", Sys.Date()),
      content = function(file) { d <- prio_pts(); if (is.null(d)) d <- data.frame(); utils::write.csv(d, file, row.names = FALSE) })

    # ---- camera row → the predator + protected detections behind its score (tabbed modal) ----
    .pp_detections <- function(loc_id) {
      obs <- ik_observations(ik_data, with_location = TRUE)
      obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal" &
                   !is.na(obs$locationID) & obs$locationID == loc_id & !is.na(obs$eventStart) &
                   obs$scientificName %in% c(pred_sci(), prot_sci()), , drop = FALSE]
      seas <- .ik_nz(selection()$season)
      if (!is.null(seas) && nrow(obs)) {
        op <- ik_observation_period(ik_data)
        osea <- op$calendar_season[match(obs$observationID, op$observationID)]
        obs <- obs[!is.na(osea) & osea %in% seas, , drop = FALSE]
      }
      if (!nrow(obs)) return(NULL)
      obs$role <- ifelse(obs$scientificName %in% pred_sci(), "Predator", "Protected")   # the side, not the group label
      obs[order(obs$eventStart), , drop = FALSE]
    }
    # Open the per-camera detections modal — from a map marker (direct) OR a table row. The subtitle
    # is the customised context line: reserve · line — the predator/protected RAIs in bold (the opaque
    # pressure score itself is not shown), with the active period spelled out.
    .open_pp_modal <- function(loc_id) {
      d <- prio_pts(); if (is.null(d)) return()
      row <- d[d$location_id == loc_id, , drop = FALSE]; if (!nrow(row)) return()
      row <- row[1, , drop = FALSE]
      prio_rec(.pp_detections(loc_id)); prio_recobs(NULL)
      plab <- .ik_period_label(ik_data, selection()); span <- .ov_period_span(ik_data, selection())
      ptxt <- if (length(plab) && nzchar(plab) && length(span) && nzchar(span)) sprintf("%s (%s)", plab, span)
              else if (length(span) && nzchar(span)) span else if (length(plab)) plab else ""
      sub <- HTML(sprintf(
        "Camera &middot; %s &middot; Line %s &mdash; %s RAI <b>%.2f</b> vs %s RAI <b>%.2f</b>%s",
        htmltools::htmlEscape(row$reserve), ifelse(is.na(row$line), "—", htmltools::htmlEscape(as.character(row$line))),
        htmltools::htmlEscape(pred_lab()), row$predator, htmltools::htmlEscape(prot_lab()), row$protected,
        if (nzchar(ptxt)) sprintf(" &middot; %s", htmltools::htmlEscape(ptxt)) else ""))
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("Detections at %s", row$name), sub),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("pp_tabs"),
          tabPanel("Detections",     icon = icon("list"),        DT::dataTableOutput(session$ns("pp_detail"))),
          tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("pp_record"))))))
      hideTab(session = session, inputId = "pp_tabs", target = "Record details")
    }
    observeEvent(input$table_rows_selected, {                    # table row → the same modal as a marker
      i <- input$table_rows_selected; d <- prio_tbl()
      if (!length(i) || is.null(d) || i > nrow(d)) return()
      .open_pp_modal(d$location_id[i]); DT::selectRows(prio_dt_proxy, NULL)
    }, ignoreInit = TRUE)

    output$pp_detail <- DT::renderDT({
      det <- prio_rec(); validate(need(!is.null(det) && nrow(det), "No predator or protected detections at this camera."))
      df <- data.frame(Role = det$role,
        Species = ik_species_label(det$scientificName, ik_data, prefer()),
        When = .ik_when_label(det$eventStart),
        ObsID = det$observationID, .when_sort = as.numeric(det$eventStart),
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          columnDefs = .ik_dt_when_defs(df, "When", hide = "ObsID")))
    })
    observeEvent(input$pp_detail_rows_selected, {                # whole row → its record
      i <- input$pp_detail_rows_selected; det <- prio_rec()
      if (length(i) && !is.null(det) && i <= nrow(det)) {
        prio_recobs(det$observationID[i])
        showTab(session = session, inputId = "pp_tabs", target = "Record details", select = TRUE)
      }
    })
    observeEvent(input$pp_tab_back, updateTabsetPanel(session, input$pp_tab_back$tabset, selected = input$pp_tab_back$to))
    output$pp_record <- renderUI({
      if (is.null(prio_recobs())) return(tags$p(class = "ik-maps-hint", "Click a detection in the Detections tab to see its record here."))
      ob <- ik_observation(ik_data, prio_recobs()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(session$ns("pp_tab_back"), "pp_tabs", "Detections", "Back to detections"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("pp_rec_subtabs")))
    })

    # ---- trap marker → its full check history (all-time) → Record details ----
    th_loc <- reactiveVal(NULL); th_open <- reactiveVal(NULL)
    observeEvent(input$map_marker_click, {
      cid <- input$map_marker_click$id
      if (is.null(cid) || !(startsWith(cid, "T|") || startsWith(cid, "K|"))) return()   # a Trap-field or Catch marker
      loc <- sub("^[KT]\\|", "", cid); th_loc(loc); th_open(NULL)
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
      req(th_loc()); ch <- ik_trap_checks(ik_data, th_loc(), NULL)
      validate(need(!is.null(ch) && nrow(ch), "No checks recorded for this trap."))
      ik_trap_history_dt(ch)
    })
    observeEvent(input$th_table_rows_selected, {
      i <- input$th_table_rows_selected; ch <- ik_trap_checks(ik_data, th_loc(), NULL)
      if (length(i) && !is.null(ch) && i <= nrow(ch)) {
        th_open(ch$observationID[i]); showTab(session = session, inputId = "th_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("th_table"), NULL)
    })
    observeEvent(input$th_back, updateTabsetPanel(session, input$th_back$tabset, selected = input$th_back$to))
    output$th_record <- renderUI({
      if (is.null(th_open())) return(tags$p(class = "ik-maps-hint", "Pick a check from the Trap history tab."))
      ob <- ik_observation(ik_data, th_open()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(session$ns("th_back"), "th_tabs", "Trap history", "Back to trap history"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("th_subtabs")))
    })
  })
}
