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
#   • Trap / "Capture rate" — captures / trap-days × 100 (noisy at low effort — robust colour helps).
#   • Trap / "Servicing health" — per-trap glyph: size = trap-nights, fill/ring = check-frequency
#     status (good/watch/neglected) from ik_trap_review(). Two honest channels.
# Robust colour/size: per-location colour + radius are clamped at the 95th percentile so a single
# low-effort outlier (e.g. 1 capture in 7 trap-days → 14.29/100td) can't flatten the rest.
# Rendering: one base render (panes + preferCanvas + theme basemap), then per-layer leafletProxy
# observers. Spatial maths in R/functions/spatial.R; per-location/line values in metrics.R.

MAPS_LINE_ZOOM <- 13   # at/above this zoom, camera activity shows per-camera; below, per-line
.MAPS_CAP_PCTL <- 0.95 # colour/size clamp percentile (robust to low-effort outliers)

.MAPS_STATUS <- list(
  light = c(good = "#2e7d32", watch = "#f9a825", neglected = "#c62828"),
  dark  = c(good = "#66bb6a", watch = "#fdd835", neglected = "#ef5350"))
.MAPS_STATUS_LEVELS <- c("good", "watch", "neglected")

#' Maps nav panel UI.
#' @param id Module id.
#' @return A bslib nav_panel for page_navbar().
maps_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Maps", value = "maps", icon = icon("map"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/maps.css"),
    tags$script(src = "js/maps.js"),
    div(
      class = "ik-maps",
      div(
        class = "ik-maps-controls",
        radioButtons(ns("source"), "Device",
                     choices = c(Camera = "camera", Trap = "trap"), selected = "camera", inline = TRUE),
        conditionalPanel("input.source == 'trap'", ns = ns,
          radioButtons(ns("measure"), "Measure",
                       choices = c("Captures" = "captures", "Capture rate" = "rate", "Servicing health" = "servicing"),
                       selected = "captures", inline = TRUE)),
        conditionalPanel("!(input.source == 'trap' && input.measure == 'servicing')", ns = ns,
          selectInput(ns("group"), "Species group", choices = NULL))
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
maps_server <- function(id, ik_data, prefer_scientific, selection, color_mode = reactive("light")) {
  moduleServer(id, function(input, output, session) {

    sg     <- ik_species_groups(ik_data)
    groups <- c(ik_taxa_groups(sg, "monitor", "target"),
                ik_taxa_groups(sg, "monitor", "interesting"))
    updateSelectInput(session, "group", choices = names(groups), selected = names(groups)[1])

    observeEvent(input$source, {
      if (identical(input$source, "trap"))
        updateRadioButtons(session, "measure", selected = isolate(input$measure) %||% "captures",
          choices = c("Captures" = "captures", "Capture rate" = "rate", "Servicing health" = "servicing"), inline = TRUE)
    }, ignoreInit = TRUE)

    src      <- reactive(input$source %||% "camera")
    measure  <- reactive(if (src() == "camera") "rate" else (input$measure %||% "captures"))
    is_dark  <- reactive(identical(color_mode(), "dark"))
    halo     <- reactive(if (is_dark()) "#1a1a1a" else "#ffffff")
    per_cam  <- reactive((ik_data$meta$camera$rai %||% list())$camera_hours %||% 500)
    has_group <- reactive(isTRUE(input$group %in% names(groups)))
    # the per-location value column + its label, for the active rate/count measure
    valcol   <- reactive(if (src() == "trap" && measure() == "captures") "captures" else "metric")
    vallabel <- reactive(if (src() == "trap" && measure() == "captures") "Captures"
                         else if (src() == "trap") "Capture rate"
                         else "Detections / deployment")

    grain_rv <- reactiveVal("line")
    observeEvent(input$map_zoom, {
      g <- if (isTRUE(input$map_zoom >= MAPS_LINE_ZOOM)) "camera" else "line"
      if (!identical(g, grain_rv())) grain_rv(g)
    })

    # ---- data ----
    cam_all <- reactive({ req(src() == "camera", has_group())
      ik_location_metric(ik_data, selection(), groups[input$group], "camera", norm = per_cam()) })
    cam_pts      <- reactive({ m <- cam_all(); if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
    cam_unplaced <- reactive({ m <- cam_all(); if (is.null(m)) NULL else m[!(is.finite(m$latitude) & is.finite(m$longitude)), , drop = FALSE] })

    line_metric <- reactive({ req(src() == "camera", has_group())
      rr <- ik_rai(ik_data, selection(), groups[input$group]); ln <- rr$lines
      if (is.null(ln) || !nrow(ln)) return(NULL)
      ce <- ik_group_centroids(cam_pts(), c("reserve", "line")); if (is.null(ce)) return(NULL)
      m <- merge(ln, ce, by = c("reserve", "line")); if (nrow(m)) m else NULL })

    trap_all <- reactive({ req(src() == "trap", measure() %in% c("captures", "rate"), has_group())
      ik_location_metric(ik_data, selection(), groups[input$group], "trap") })
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
      tr$status <- factor(tr$status, levels = .MAPS_STATUS_LEVELS)
      if (nrow(tr)) tr else NULL })
    serv_pts      <- reactive({ tr <- serv_all(); if (is.null(tr)) NULL else tr[is.finite(tr$latitude) & is.finite(tr$longitude), , drop = FALSE] })
    serv_unplaced <- reactive({ tr <- serv_all(); if (is.null(tr)) NULL else tr[!(is.finite(tr$latitude) & is.finite(tr$longitude)), , drop = FALSE] })

    rate_loc_pts <- reactive(if (src() == "trap") trap_pts() else cam_pts())
    frame_pts    <- reactive(if (src() == "trap" && measure() == "servicing") serv_pts() else rate_loc_pts())

    # ---- helpers ----
    .robust_cap <- function(v) {
      pos <- v[is.finite(v) & v > 0]; if (!length(pos)) return(1)
      max(as.numeric(stats::quantile(pos, .MAPS_CAP_PCTL, names = FALSE, type = 7)), min(pos), 1e-9)
    }
    .area_radius <- function(v, lo, hi) {
      v <- pmax(v, 0); r <- range(v, na.rm = TRUE)
      if (!is.finite(diff(r)) || diff(r) == 0) return(rep((lo + hi) / 2, length(v)))
      scales::rescale(sqrt(v), to = c(lo, hi))
    }
    rate_popup <- function(d) {
      if (src() == "trap") {
        head  <- if (measure() == "captures") sprintf("Captures: %d", as.integer(d$captures)) else sprintf("Capture rate: %.2f / 100 trap-days", d$metric)
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
    # reset the drill when the data context changes (a clicked place may not exist in the new view)
    observeEvent(list(src(), measure(), input$group, selection()), selected(NULL), ignoreInit = TRUE)
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
      cam_ds <- names(ik_data$datasets)[vapply(ik_data$datasets, function(d) identical(d$meta$source_type, "camera"), logical(1))]
      locs <- ik_data$app$geography$locations
      locs <- locs[locs$dataset %in% cam_ds & is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
      m <- leaflet::addProviderTiles(m, canvas, group = "Map")
      m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
      pns <- c("surface", "heat", "zeros", "points", "boundary", "selected")
      for (pn in pns) m <- leaflet::addMapPane(m, pn, zIndex = 400 + 10 * match(pn, pns))
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
        overlayGroups = c("Surface", "Points", "No records", "Boundary", "Activity"),
        options = leaflet::layersControlOptions(collapsed = FALSE))
      m <- leaflet::hideGroup(m, "Activity")
      if (nrow(locs)) m <- leaflet::fitBounds(m, min(locs$longitude), min(locs$latitude), max(locs$longitude), max(locs$latitude))
      m
    })
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
    observe({
      p <- proxy(); leaflet::clearGroup(p, "Surface")
      if (src() == "trap" && measure() == "servicing") return()
      d <- rate_loc_pts(); if (is.null(d) || !nrow(d)) return()
      s <- ik_idw_surface(d, valcol(), "reserve"); if (is.null(s) || !nrow(s)) return()
      cap <- .robust_cap(s$predicted); pf <- leaflet::colorNumeric("viridis", c(0, cap))
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
        leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Points", layerId = d$location,
          radius = .area_radius(d$trap_days, 4, 16), fillColor = unname(cols[stc]),
          fillOpacity = unname(c(good = 0.85, watch = 0.7, neglected = 0.25)[stc]),
          stroke = TRUE, color = unname(cols[stc]), weight = unname(c(good = 1, watch = 2, neglected = 3)[stc]),
          label = sprintf("%s — %s (%.0f d interval)", d$name, toupper(stc), d$mean_interval_days),
          popup = serv_popup(d), options = leaflet::pathOptions(pane = "points"))
      } else if (src() == "camera" && grain_rv() == "line") {
        d <- line_metric(); if (is.null(d) || !nrow(d)) return()
        cap <- .robust_cap(d$metric); v <- pmin(d$metric, cap); pf <- leaflet::colorNumeric("viridis", c(0, cap))
        leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Points",
          layerId = paste0("L|", d$reserve, "|", d$line),
          radius = .area_radius(v, 8, 26), fillColor = pf(v), fillOpacity = 0.85, stroke = TRUE, color = halo(), weight = 1.5,
          label = sprintf("Line %s (%s) — RAI %.2f", d$line, d$reserve, d$metric),
          popup = line_popup(d), options = leaflet::pathOptions(pane = "points"))
      } else {
        d <- rate_loc_pts(); vc <- valcol()
        d <- if (is.null(d)) NULL else d[is.finite(d[[vc]]) & d[[vc]] > 0, , drop = FALSE]
        if (is.null(d) || !nrow(d)) return()
        cap <- .robust_cap(d[[vc]]); v <- pmin(d[[vc]], cap); pf <- leaflet::colorNumeric("viridis", c(0, cap))
        leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Points", layerId = d$location_id,
          radius = .area_radius(v, 5, 20), fillColor = pf(v), fillOpacity = 0.8, stroke = TRUE, color = halo(), weight = 1.5,
          label = sprintf("%s — %s %s", d$name, vallabel(), formatC(d[[vc]], format = "fg", digits = 3)),
          popup = rate_popup(d), options = leaflet::pathOptions(pane = "points"))
      }
    })

    # ---- No records (rate/count sites with effort but zero of the group) ----
    observe({
      p <- proxy(); leaflet::clearGroup(p, "No records")
      if (src() == "trap" && measure() == "servicing") return()
      if (src() == "camera" && grain_rv() == "line") return()
      d <- rate_loc_pts(); d <- if (is.null(d)) NULL else d[d$metric == 0, , drop = FALSE]
      if (is.null(d) || !nrow(d)) return()
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "No records", layerId = d$location_id,
        radius = 3, fill = FALSE, stroke = TRUE, weight = 1, color = if (is_dark()) "#8a8a8a" else "#9a9a9a", opacity = 0.4,
        label = sprintf("%s — no records", d$name), options = leaflet::pathOptions(pane = "zeros"))
    })

    observe({                                                   # Boundary
      p <- proxy(); leaflet::clearGroup(p, "Boundary")
      h <- ik_selection_hulls(frame_pts(), "reserve"); if (is.null(h) || !nrow(h)) return()
      leaflet::addPolygons(p, data = h, group = "Boundary", fill = FALSE,
        color = if (is_dark()) "#9ccc65" else "#2e7d32", weight = 2, dashArray = "5,5", options = leaflet::pathOptions(pane = "boundary"))
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
        leaflet::addLegend(p, "bottomright", colors = unname(cols), labels = names(cols), title = "Servicing &middot; size = trap-nights", opacity = 0.9)
      } else {
        d <- if (src() == "camera" && grain_rv() == "line") line_metric() else rate_loc_pts()
        vc <- if (src() == "camera" && grain_rv() == "line") "metric" else valcol()
        if (is.null(d) || !nrow(d)) return()
        cap <- .robust_cap(d[[vc]]); pf <- leaflet::colorNumeric("viridis", c(0, cap))
        ttl <- if (src() == "camera" && grain_rv() == "line") sprintf("RAI / line &middot; %s", input$group)
               else sprintf("%s &middot; %s", vallabel(), input$group)
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
    })

    # ---- records / servicing table + CSV ----
    records_base <- reactive({ req(measure() %in% c("rate", "captures"), has_group())
      ik_metric_obs(ik_data, selection(), groups[input$group], input$group, source_type = src()) })
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
      if (src() == "trap" && measure() == "servicing") {
        d <- serv_pts(); n <- if (is.null(d)) 0L else nrow(d); nneg <- if (is.null(d)) 0L else sum(d$status == "neglected", na.rm = TRUE)
        tags$small(sprintf("%s traps in this selection &middot; %s neglected.", format(n, big.mark = ","), format(nneg, big.mark = ",")))
      } else {
        o <- records(); n <- if (is.null(o)) 0L else nrow(o); noun <- if (src() == "trap") "capture" else "detection"
        tags$small(sprintf("%s %s record%s of %s%s.", format(n, big.mark = ","), noun, if (n == 1L) "" else "s", input$group,
                           if (is.null(selected())) " in this selection" else " here"))
      }
    })

    output$table <- DT::renderDT({
      if (src() == "trap" && measure() == "servicing") {
        d <- serv_pts(); validate(need(!is.null(d) && nrow(d), "No traps in the current selection."))
        df <- data.frame(Trap = d$name, Line = d$line, Reserve = d$reserve, Checks = d$n_checks,
          `Trap-days` = round(d$trap_days), `Mean interval (d)` = round(d$mean_interval_days, 1),
          Status = tools::toTitleCase(as.character(d$status)), Captures = d$captures, check.names = FALSE, stringsAsFactors = FALSE)
        return(DT::datatable(df, rownames = FALSE, class = "stripe hover row-border",
          options = list(pageLength = 10, scrollX = TRUE, dom = "tip", order = list(list(5, "desc")))))
      }
      o <- records()
      validate(need(!is.null(o) && nrow(o), sprintf("No %ss of this group here.", if (src() == "trap") "capture" else "detection")))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      has_t <- format(o$when, "%H:%M:%S") != "00:00:00"
      when_lab <- ifelse(has_t, format(o$when, "%Y-%m-%d %H:%M"), format(o$when, "%Y-%m-%d"))
      df <- data.frame(When = when_lab, Species = ik_species_label(o$scientificName, ik_data, prefer),
        Count = o$count, Reserve = o$reserve, Line = o$line, Location = o$locationName, check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
    })

    rec_proxy <- DT::dataTableProxy("table")
    observeEvent(input$table_rows_selected, {                   # record → pan/highlight marker + viewer
      if (src() == "trap" && measure() == "servicing") return()
      i <- input$table_rows_selected; o <- records(); if (!length(i) || is.null(o) || i > nrow(o)) return()
      locs <- ik_data$app$geography$locations; gi <- match(o$location_id[i], locs$location_id)
      if (!is.na(gi) && is.finite(locs$longitude[gi])) {
        p <- proxy()
        leaflet::setView(p, locs$longitude[gi], locs$latitude[gi], zoom = max(MAPS_LINE_ZOOM, 14))
        leaflet::clearGroup(p, "Selected")
        leaflet::addCircleMarkers(p, lng = locs$longitude[gi], lat = locs$latitude[gi], group = "Selected",
          radius = 16, fill = FALSE, stroke = TRUE, color = "#ff5722", weight = 3, options = leaflet::pathOptions(pane = "selected"))
      }
      show_observation_modal(ik_data, o$observationID[i], isTRUE(prefer_scientific()))
      DT::selectRows(rec_proxy, NULL)
    }, ignoreInit = TRUE)

    output$unplaced <- renderUI({
      u <- if (src() == "trap" && measure() == "servicing") serv_unplaced() else if (src() == "trap") trap_unplaced() else cam_unplaced()
      if (is.null(u) || !nrow(u)) return(NULL)
      div(class = "ik-maps-unplaced", icon("triangle-exclamation"),
          sprintf(" %s coordless %s not shown on the map (no location fix).", format(nrow(u), big.mark = ","), if (src() == "trap") "traps" else "sites"))
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        tag <- if (src() == "trap" && measure() == "servicing") "trap-servicing"
               else paste0(src(), "-", measure(), "-", gsub("[^A-Za-z0-9]+", "-", tolower(input$group %||% "group")))
        sprintf("maps-%s-%s.csv", tag, Sys.Date())
      },
      content = function(file) {
        d <- if (src() == "trap" && measure() == "servicing") serv_pts() else records()
        if (is.null(d)) d <- data.frame(); utils::write.csv(d, file, row.names = FALSE)
      }
    )
  })
}
