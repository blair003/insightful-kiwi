# coverage.R (module) — "Coverage": the SPATIAL counterpart to the Neighbourhood panel. One leaflet
# carrying BOTH devices at once as toggleable layers, so you can see where the protected species are
# and whether predator control is reaching them:
#   • Protected (camera)  — green circles, size ∝ detection rate (the hotspots)
#   • Predators (camera)  — red circles, size ∝ detection rate (where predators roam; off by default)
#   • Catches (traps)     — purple circles, size ∝ predators caught (where removal happens)
#   • Traps (effort)      — small grey dots, every trap deployed (the trapping field / coverage)
# A green hotspot sitting in purple + grey is covered; a green hotspot with little around it is a gap.
# Colour encodes WHAT a marker is, size encodes magnitude (so one legend, no clashing colour scales).
# Period + Reserve come from the sidebar; the predator/protected role pickers are in-panel. Reuses
# ik_location_metric() for every layer (camera rate / trap captures), so no new data layer.

#' Coverage nav panel. @param id Module id.
coverage_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Coverage", value = "coverage", icon = icon("shield-halved"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/coverage.css"),
    tags$script(src = "js/maps.js"),                            # reuse the resize-on-tab-show fix
    div(class = "ik-cov",
        tags$h3(class = "ik-cov-title", "Coverage — protected hotspots vs predator control"),
        tags$p(class = "ik-cov-lead",
          "Where are the protected species, and is predator control reaching them? ",
          tags$b(tags$span(style = "color:#2e7d32", "Green")), " = protected on camera; ",
          tags$b(tags$span(style = "color:#c62828", "red")), " = predators on camera; ",
          tags$b(tags$span(style = "color:#6a3d9a", "purple")), " = predators caught in traps; ",
          tags$b(tags$span(style = "color:#8a8a8a", "grey")), " = traps (effort). A green hotspot ",
          "ringed by purple/grey is covered; one with little around it is a gap. Period & reserve from the sidebar."),
        div(class = "ik-cov-controls",
            selectInput(ns("prot"), "Protected", choices = NULL, multiple = TRUE, width = "230px"),
            selectInput(ns("pred"), "Predator",  choices = NULL, multiple = TRUE, width = "230px"),
            selectInput(ns("radius"), "Gap radius", width = "120px",
                        choices = c("250 m" = 250, "500 m" = 500, "1 km" = 1000), selected = 500)),
        uiOutput(ns("caption")),
        leaflet::leafletOutput(ns("map"), height = "62vh"),
        div(class = "ik-cov-gaps",
            tags$h5(class = "ik-cov-gaps-title", "Coverage gaps"),
            tags$p(class = "ik-cov-gaps-lead",
              "Protected hotspots ranked worst-first by how well predator control reaches them, ",
              "within the ", tags$b("gap radius"), ". ", tags$b("No trapping"), " = no traps running nearby; ",
              tags$b("Predators uncaught"), " = predators on camera but none caught nearby; ",
              tags$b("Neglected"), " = nearby traps mostly unserviced."),
            DT::DTOutput(ns("gaps"))))
  )
}

#' Coverage server.
#' @param id Module id. @param ik_data The container. @param prefer_scientific reactive name pref.
#' @param selection reactive selection SPEC (Period + Reserve from sidebar). @param color_mode theme.
coverage_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                            selection = reactive(list()), color_mode = reactive("light")) {
  moduleServer(id, function(input, output, session) {
    is_dark <- reactive(identical(color_mode(), "dark"))
    sg <- ik_species_groups(ik_data)
    .role_taxa <- function(role) { l <- unique(sg$label[sg$role == role & !is.na(sg$monitor)])
      stats::setNames(lapply(l, function(x) sg$scientificName[sg$label == x & !is.na(sg$scientificName)]), l) }
    pred_taxa <- .role_taxa("predator"); prot_taxa <- .role_taxa("protected")
    splits <- unique(sg$label[which(sg$split)])
    prefer <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    .pred_def <- paste0("grp:", if ("Mustelids" %in% names(pred_taxa)) "Mustelids" else names(pred_taxa)[1])
    .prot_def <- paste0("grp:", if ("Kiwi" %in% names(prot_taxa)) "Kiwi" else names(prot_taxa)[1])
    per_cam   <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500

    observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      updateSelectInput(session, "prot", choices = ik_species_choices(prot_taxa, ik_data, p, splits), selected = keep(isolate(input$prot), .prot_def))
      updateSelectInput(session, "pred", choices = ik_species_choices(pred_taxa, ik_data, p, splits), selected = keep(isolate(input$pred), .pred_def))
    })

    prot_sci <- reactive(ik_resolve_species_choice(input$prot, prot_taxa))
    pred_sci <- reactive(ik_resolve_species_choice(input$pred, pred_taxa))
    prot_lab <- reactive({ v <- input$prot; if (!length(v)) "protected" else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })
    pred_lab <- reactive({ v <- input$pred; if (!length(v)) "predator"  else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })

    .pts <- function(m) if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE]
    cam_prot <- reactive({ if (!length(prot_sci())) return(NULL); .pts(ik_location_metric(ik_data, selection(), list(P = prot_sci()), "camera", norm = per_cam)) })
    cam_pred <- reactive({ if (!length(pred_sci())) return(NULL); .pts(ik_location_metric(ik_data, selection(), list(P = pred_sci()), "camera", norm = per_cam)) })
    trap_pred <- reactive({ if (!length(pred_sci())) return(NULL); .pts(ik_location_metric(ik_data, selection(), list(P = pred_sci()), "trap")) })

    .radius <- function(v, lo, hi) { v <- pmax(as.numeric(v), 0)
      if (!length(v) || !is.finite(diff(range(v))) || diff(range(v)) == 0) return(rep((lo + hi) / 2, length(v)))
      scales::rescale(sqrt(v), to = c(lo, hi)) }
    proxy <- function() leaflet::leafletProxy("map", session)

    output$map <- leaflet::renderLeaflet({
      locs <- ik_data$app$geography$locations
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
      m <- leaflet::addProviderTiles(m, canvas, group = "Map")
      m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
      pns <- c("traps", "catches", "predators", "protected")
      for (pn in pns) m <- leaflet::addMapPane(m, pn, zIndex = 410 + 10 * match(pn, pns))
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
        overlayGroups = c("Protected", "Predators", "Catches", "Traps"),
        options = leaflet::layersControlOptions(collapsed = FALSE))
      m <- leaflet::hideGroup(m, "Predators")                   # off by default — the coverage story is Protected vs Catches/Traps
      if (nrow(locs)) m <- leaflet::fitBounds(m, min(locs$longitude), min(locs$latitude), max(locs$longitude), max(locs$latitude))
      m
    })
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    observeEvent(color_mode(), {
      p <- proxy(); leaflet::clearGroup(p, "Map")
      leaflet::addProviderTiles(p, if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron, group = "Map")
    }, ignoreInit = TRUE)

    observe({                                                   # Traps (effort) — every deployed trap
      p <- proxy(); leaflet::clearGroup(p, "Traps")
      d <- trap_pred(); if (is.null(d) || !nrow(d)) return()
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Traps", layerId = paste0("T|", d$location_id),
        radius = 3, fill = TRUE, fillColor = "#8a8a8a", fillOpacity = 0.5, stroke = FALSE,
        label = ~sprintf("%s — trap (%s trap-days, %d caught)", name, format(round(trap_days), big.mark = ","), as.integer(captures)),
        options = leaflet::pathOptions(pane = "traps"))
    })
    observe({                                                   # Catches — traps that caught the predator
      p <- proxy(); leaflet::clearGroup(p, "Catches")
      d <- trap_pred(); d <- if (is.null(d)) NULL else d[is.finite(d$captures) & d$captures > 0, , drop = FALSE]
      if (is.null(d) || !nrow(d)) return()
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Catches",
        radius = .radius(d$captures, 6, 22), fillColor = "#6a3d9a", fillOpacity = 0.85, stroke = TRUE, color = "#ffffff", weight = 1,
        popup = ~sprintf("<b>%s</b><br/>Line %s &middot; %s<br/><b>%s caught: %d</b><br/>Trap-days: %s",
                         name, ifelse(is.na(line), "—", line), reserve, pred_lab(), as.integer(captures), format(round(trap_days), big.mark = ",")),
        options = leaflet::pathOptions(pane = "catches"))
    })
    observe({                                                   # Predators on camera (off by default)
      p <- proxy(); leaflet::clearGroup(p, "Predators")
      d <- cam_pred(); d <- if (is.null(d)) NULL else d[is.finite(d$metric) & d$metric > 0, , drop = FALSE]
      if (is.null(d) || !nrow(d)) return()
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Predators",
        radius = .radius(d$metric, 6, 22), fillColor = "#c62828", fillOpacity = 0.8, stroke = TRUE, color = "#ffffff", weight = 1,
        popup = ~sprintf("<b>%s</b><br/>Line %s &middot; %s<br/><b>%s: %.2f</b> / %s ch<br/>%d detections",
                         name, ifelse(is.na(line), "—", line), reserve, pred_lab(), metric, format(per_cam, big.mark = ","), as.integer(individuals)),
        options = leaflet::pathOptions(pane = "predators"))
    })
    observe({                                                   # Protected on camera — the hotspots (top)
      p <- proxy(); leaflet::clearGroup(p, "Protected")
      d <- cam_prot(); d <- if (is.null(d)) NULL else d[is.finite(d$metric) & d$metric > 0, , drop = FALSE]
      if (is.null(d) || !nrow(d)) return()
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Protected",
        radius = .radius(d$metric, 7, 24), fillColor = "#2e7d32", fillOpacity = 0.85, stroke = TRUE, color = "#ffffff", weight = 1.5,
        popup = ~sprintf("<b>%s</b><br/>Line %s &middot; %s<br/><b>%s: %.2f</b> / %s ch<br/>%d detections",
                         name, ifelse(is.na(line), "—", line), reserve, prot_lab(), metric, format(per_cam, big.mark = ","), as.integer(individuals)),
        options = leaflet::pathOptions(pane = "protected"))
    })
    observe({                                                   # re-frame to the deployed extent of the selection
      p <- proxy(); d <- trap_pred()
      ext <- rbind(cam_prot()[, c("longitude", "latitude")], cam_pred()[, c("longitude", "latitude")], if (!is.null(d)) d[, c("longitude", "latitude")])
      if (is.null(ext) || !nrow(ext)) return()
      leaflet::fitBounds(p, min(ext$longitude), min(ext$latitude), max(ext$longitude), max(ext$latitude), options = list(padding = c(30, 30)))
    })
    observe({                                                   # one colour-key legend (colour = what; size = magnitude)
      p <- proxy(); leaflet::clearControls(p)
      leaflet::addLegend(p, "bottomright", colors = c("#2e7d32", "#c62828", "#6a3d9a", "#8a8a8a"),
        labels = c(sprintf("%s (camera)", prot_lab()), sprintf("%s (camera)", pred_lab()),
                   sprintf("%s caught (traps)", pred_lab()), "Traps (effort)"),
        title = "Coverage &middot; size = magnitude", opacity = 0.9)
    })

    output$caption <- renderUI({
      hot <- cam_prot(); nh <- if (is.null(hot)) 0L else sum(hot$metric > 0, na.rm = TRUE)
      tr  <- trap_pred(); nt <- if (is.null(tr)) 0L else nrow(tr); nc <- if (is.null(tr)) 0L else sum(tr$captures > 0, na.rm = TRUE)
      seas <- .ik_nz(selection()$season); per <- if (is.null(seas)) "all seasons" else paste(seas, collapse = ", ")
      tags$p(class = "ik-cov-meta", sprintf(
        "%s · %s camera hotspot%s with %s · %s traps (%s caught %s). Toggle layers top-right; click a marker for detail.",
        per, format(nh, big.mark = ","), if (identical(nh, 1L)) "" else "s", prot_lab(),
        format(nt, big.mark = ","), format(nc, big.mark = ","), pred_lab()))
    })

    # ---- coverage-gap table: protected hotspots ranked by weak nearby trapping (worst first) ----
    gaps <- reactive({
      ik_coverage_gaps(ik_data, .ik_nz(selection()$season), pred_sci(), prot_sci(),
                       as.numeric(input$radius %||% 500))
    }) |> bindCache(.ik_nz(selection()$season), .ik_nz(selection()$reserve), input$radius, input$pred, input$prot, ik_active_datasets())

    output$gaps <- DT::renderDT({
      g <- gaps(); validate(need(!is.null(g) && nrow(g), "Pick a predator and a protected species."))
      rsv <- .ik_nz(selection()$reserve); if (!is.null(rsv)) g <- g[g$reserve %in% rsv, , drop = FALSE]
      validate(need(nrow(g), "No camera lines in this reserve."))
      lab <- c(no_trapping = "No trapping", predators_uncaught = "Predators uncaught",
               neglected = "Trapping neglected", covered = "Covered", no_protected = "No protected here")
      col <- c(no_trapping = "#c62828", predators_uncaught = "#e8590c",
               neglected = "#f59f00", covered = "#2e7d32", no_protected = "#868e96")
      badge <- sprintf("<span class='ik-gap-badge' style='background:%s'>%s</span>", col[g$status], lab[g$status])
      df <- data.frame(Line = g$line, Reserve = g$reserve,
        `Protected` = ifelse(is.na(g$prot_rate), "—", sprintf("%.2f", g$prot_rate)),
        `Predator`  = ifelse(is.na(g$pred_rate), "—", sprintf("%.2f", g$pred_rate)),
        `Traps` = g$n_traps, `Caught` = g$catches, `Neglected` = g$n_neglected,
        Status = badge, check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, escape = -ncol(df), selection = "none",
        class = "stripe hover row-border", options = list(pageLength = 15, dom = "t", ordering = FALSE))
    })
  })
}
