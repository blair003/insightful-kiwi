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

#' Body of the whole-map "how to read this" help modal. @keywords internal
coverage_help_body <- function() {
  tagList(
    tags$p("One map carrying ", tags$b("both devices"), " so you can see where the protected species ",
           "are and whether predator control is reaching them. ", tags$b("Colour says what a marker is; ",
           "size says how much"), " — so there's one legend and no clashing colour scales."),
    tags$h6("The layers (toggle top-right)"),
    tags$ul(
      tags$li(tags$b(tags$span(style = "color:#2e7d32", "Protected")), " (camera) — green dots, size ∝ detection rate. The hotspots you're protecting."),
      tags$li(tags$b(tags$span(style = "color:#c62828", "Predators")), " (camera) — red rings, size ∝ detection rate. Where predators roam. Off by default (turn it on to compare)."),
      tags$li(tags$b(tags$span(style = "color:#6a3d9a", "Catches")), " (traps) — purple dots, size ∝ predators caught. Where removal is actually happening."),
      tags$li(tags$b(tags$span(style = "color:#8a8a8a", "Traps")), " — small grey dots, every trap deployed: the trapping field."),
      tags$li(tags$b("Boundary"), " — the dashed monitored footprint (convex hull of the devices) per reserve.")),
    tags$h6("Reading it"),
    tags$p("A green hotspot ringed by purple and grey is ", tags$b("covered"), "; a green hotspot with ",
           "little around it is a ", tags$b("gap"), ". Camera markers are a detection ", tags$b("rate"),
           " (per camera-hour norm), so big vs small is comparable across cameras regardless of how long each ran."),
    tags$p("Period and reserve come from the sidebar. Click any marker for its detail. The ",
           tags$b("Coverage gaps"), " table below ranks the gaps numerically.")
  )
}

#' Body of the Coverage-gaps "how to read this" help modal. @keywords internal
coverage_gaps_help_body <- function() {
  tagList(
    tags$p("Every monitoring ", tags$b("line"), " (its cameras) is ranked ", tags$b("worst-first"),
           " by how well predator control reaches it. It turns the map's eyeball judgement into a list."),
    tags$h6("The gap radius"),
    tags$p("A line's “neighbourhood” is the traps within the ", tags$b("gap radius"), " of its cameras. ",
           "A bigger radius counts more distant traps as nearby, so more lines look covered. It drives the ",
           tags$b("Traps"), ", ", tags$b("Caught"), " and ", tags$b("Neglected"), " columns."),
    tags$p(tags$em("Why isn't the radius drawn on the map?"), " Because it's applied ", tags$b("per line"),
           " — every line has its own circle around its own cameras, so a single circle would be misleading. ",
           "The table is where the radius lives."),
    tags$h6("The columns"),
    tags$ul(
      tags$li(tags$b("Protected / Predator"), " — detections on the line's cameras, as a rate per the camera-hour norm (same units as the map). 0 = none seen on camera."),
      tags$li(tags$b("Traps"), " — traps running within the gap radius (how much trapping reaches the line)."),
      tags$li(tags$b("Caught"), " — predators caught in those nearby traps this period."),
      tags$li(tags$b("Neglected"), " — of the nearby active traps, how many are unserviced this period."),
      tags$li(tags$b("Traps/km²"), " — trap density across the whole reserve, for context.")),
    tags$h6("Status"),
    tags$ul(
      tags$li(tags$b("No trapping"), " — protected on camera, but no traps running nearby."),
      tags$li(tags$b("Predators uncaught"), " — predators on camera nearby, but none being caught."),
      tags$li(tags$b("Trapping neglected"), " — traps are nearby but mostly unserviced."),
      tags$li(tags$b("Covered"), " — protected hotspot with serviced trapping reaching it."),
      tags$li(tags$b("No protected here"), " — no protected species on camera on this line."))
  )
}

#' DT header container for the gaps table, with a hover-info (ⓘ) tooltip on the metric columns.
#' @keywords internal
.cov_gaps_header <- function(norm) {
  n  <- format(norm, big.mark = ",")
  # NB: build with explicit tags$* — htmltools::withTags() would shadow a local `th` helper with the
  # built-in tag function, dropping the tooltip span.
  thx <- function(label, tip = NULL) tags$th(label,
    if (!is.null(tip)) tags$span(class = "ik-th-i", title = tip, HTML("&#9432;")))
  tags$table(class = "display", tags$thead(tags$tr(
    thx("Line"), thx("Reserve"),
    thx("Protected", sprintf("Protected detections on this line's cameras, as a rate per %s camera-hours (same as the map). 0 = none on camera.", n)),
    thx("Predator",  sprintf("Predator detections on this line's cameras, per %s camera-hours. Higher = more predator activity on camera.", n)),
    thx("Traps", "Traps running within the gap radius of this line's cameras — how much trapping reaches it. Set by the Gap radius control."),
    thx("Caught", "Predators caught in those nearby traps this period."),
    thx("Neglected", "Of the nearby active traps, how many are unserviced (neglected) this period."),
    thx("Traps/km²", "Trap density across the whole reserve (structural context)."),
    thx("Status")
  )))
}

#' Coverage nav panel. @param id Module id.
coverage_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Coverage", value = "coverage", icon = icon("shield-halved"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/coverage.css")),
    tags$script(src = .ik_asset("js/maps.js")),                            # reuse the resize-on-tab-show fix
    div(class = "ik-cov",
        div(class = "ik-cov-titlebar",
            tags$h3(class = "ik-cov-title", "Coverage — protected hotspots vs predator control"),
            .ik_info(ns("cov_help"), "Coverage — how to read this map", coverage_help_body())),
        tags$p(class = "ik-cov-lead",
          "Where are the protected species, and is predator control reaching them? ",
          tags$b(tags$span(style = "color:#2e7d32", "Green")), " = protected on camera; ",
          tags$b(tags$span(style = "color:#c62828", "red")), " = predators on camera; ",
          tags$b(tags$span(style = "color:#6a3d9a", "purple")), " = predators caught in traps; ",
          tags$b(tags$span(style = "color:#8a8a8a", "grey")), " = traps. A green hotspot ",
          "ringed by purple/grey is covered; one with little around it is a gap. Period & reserve from the sidebar."),
        div(class = "ik-cov-controls",
            selectInput(ns("prot"), "Protected", choices = NULL, multiple = TRUE, width = "230px"),
            selectInput(ns("pred"), "Predator",  choices = NULL, multiple = TRUE, width = "230px")),
        uiOutput(ns("caption")),
        leaflet::leafletOutput(ns("map"), height = "55vh"),
        div(class = "ik-cov-density",
            tags$h5(class = "ik-cov-gaps-title", "Network density by reserve"),
            tags$p(class = "ik-cov-gaps-lead",
              "Is the network even dense enough to work? Footprint area, traps & cameras per km², and ",
              "the typical (nearest-neighbour) spacing — structural coverage, independent of any period."),
            DT::DTOutput(ns("density"))),
        div(class = "ik-cov-gaps",
            div(class = "ik-cov-gaps-head",
                div(class = "ik-cov-gaps-head-l",
                    tags$h5(class = "ik-cov-gaps-title", "Coverage gaps"),
                    .ik_info(ns("gaps_help"), "Coverage gaps — how to read this", coverage_gaps_help_body())),
                selectInput(ns("radius"), "Gap radius", width = "120px",  # drives THIS table (not the map)
                            choices = c("250 m" = 250, "500 m" = 500, "1 km" = 1000), selected = 500)),
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
                            selection = reactive(list()), color_mode = reactive("light"),
                            active = reactive(TRUE)) {
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
    cam_prot <- reactive({ req(active()); if (!length(prot_sci())) return(NULL); .pts(ik_location_metric(ik_data, selection(), list(P = prot_sci()), "camera", norm = per_cam)) })
    cam_pred <- reactive({ req(active()); if (!length(pred_sci())) return(NULL); .pts(ik_location_metric(ik_data, selection(), list(P = pred_sci()), "camera", norm = per_cam)) })
    trap_pred <- reactive({ req(active()); if (!length(pred_sci())) return(NULL); .pts(ik_location_metric(ik_data, selection(), list(P = pred_sci()), "trap")) })
    # checks per trap in the period (check-date) — the servicing-effort number, distinct from trap-days
    trap_checks <- reactive({ req(active())
      dp <- ik_deployment_period(ik_data); dp <- dp[!is.na(dp$source_type) & dp$source_type == "trap", , drop = FALSE]
      seas <- .ik_nz(selection()$season); if (!is.null(seas)) dp <- .trap_in_period(ik_data, dp, seas)
      table(dp$locationID)
    })

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
      pns <- c("boundary", "traps", "catches", "protected", "predators")   # boundary lowest, predators on TOP
      for (pn in pns) m <- leaflet::addMapPane(m, pn, zIndex = 410 + 10 * match(pn, pns))
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
        overlayGroups = c("Protected", "Predators", "Catches", "Traps", "Boundary"),
        options = leaflet::layersControlOptions(collapsed = FALSE))
      m <- leaflet::hideGroup(m, "Predators")                   # off by default — the coverage story is Protected vs Catches/Traps
      if (nrow(locs)) m <- leaflet::fitBounds(m, min(locs$longitude), min(locs$latitude), max(locs$longitude), max(locs$latitude))
      m
    })

    observeEvent(color_mode(), {
      p <- proxy(); leaflet::clearGroup(p, "Map")
      leaflet::addProviderTiles(p, if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron, group = "Map")
    }, ignoreInit = TRUE)

    observe({                                                   # Traps — every deployed trap (the network)
      p <- proxy(); leaflet::clearGroup(p, "Traps")
      d <- trap_pred(); if (is.null(d) || !nrow(d)) return()
      chk <- trap_checks(); nck <- as.integer(chk[as.character(d$location_id)]); nck[is.na(nck)] <- 0L
      lab <- sprintf("%s — %d check%s · %s trap-days · %d caught", d$name, nck, ifelse(nck == 1L, "", "s"),
                     format(round(d$trap_days), big.mark = ","), as.integer(d$captures))
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Traps", layerId = paste0("T|", d$location_id),
        radius = 3, fill = TRUE, fillColor = "#8a8a8a", fillOpacity = 0.5, stroke = FALSE,
        label = lab, options = leaflet::pathOptions(pane = "traps"))
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
      # drawn as a hollow RING on the top pane (+2px), so when toggled on it sits over the protected
      # dots without hiding them — green fill shows through a red ring where both are present.
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Predators",
        radius = .radius(d$metric, 6, 22) + 2, fill = FALSE, stroke = TRUE, color = "#c62828", weight = 2.5, opacity = 0.95,
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
    observe({                                                   # Boundary — monitored footprint (convex hull) per reserve
      p <- proxy(); leaflet::clearGroup(p, "Boundary"); req(active())
      locs <- ik_data$app$geography$locations
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      rsv <- .ik_nz(selection()$reserve); if (!is.null(rsv)) locs <- locs[locs$reserve %in% rsv, , drop = FALSE]
      h <- ik_selection_hulls(locs, "reserve"); if (is.null(h) || !nrow(h)) return()
      edge <- if (is_dark()) "#cfd8dc" else "#37474f"
      # A faint fill makes the whole reserve hoverable (not just the dashed line); the label names it
      # and hover brings it to the front — so foreign / nested reserves can be told apart.
      leaflet::addPolygons(p, data = h, group = "Boundary", label = ~reserve,
        labelOptions = leaflet::labelOptions(textsize = "12px", direction = "auto", sticky = TRUE),
        highlightOptions = leaflet::highlightOptions(weight = 3, color = "#1565c0", fillOpacity = 0.12, bringToFront = TRUE),
        fill = TRUE, fillColor = edge, fillOpacity = 0.05, stroke = TRUE,
        color = edge, weight = 1.5, dashArray = "5,6", options = leaflet::pathOptions(pane = "boundary"))
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
                   sprintf("%s caught (traps)", pred_lab()), "Traps (network)"),
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

    has_camera <- any(vapply(ik_data$datasets, function(d) identical(d$meta$source_type, "camera"), logical(1)))
    output$gaps <- DT::renderDT({
      g <- gaps()
      validate(need(!is.null(g) && nrow(g), if (!has_camera)
        "Coverage gaps compare protected detections on camera with nearby trapping — they need camera monitoring data, which isn't loaded here."
        else "Pick a predator and a protected species."))
      rsv <- .ik_nz(selection()$reserve); if (!is.null(rsv)) g <- g[g$reserve %in% rsv, , drop = FALSE]
      validate(need(nrow(g), "No camera lines in this reserve."))
      lab <- c(no_trapping = "No trapping", predators_uncaught = "Predators uncaught",
               neglected = "Trapping neglected", covered = "Covered", no_protected = "No protected here")
      col <- c(no_trapping = "#c62828", predators_uncaught = "#e8590c",
               neglected = "#f59f00", covered = "#2e7d32", no_protected = "#868e96")
      badge <- sprintf("<span class='ik-gap-badge' style='background:%s'>%s</span>", col[g$status], lab[g$status])
      cov  <- ik_coverage(ik_data)                              # reserve network density, for context
      dens <- if (is.null(cov)) rep(NA_real_, nrow(g)) else cov$traps_per_km2[match(g$reserve, cov$reserve)]
      df <- data.frame(Line = g$line, Reserve = g$reserve,
        `Protected` = ifelse(is.na(g$prot_rate), "—", sprintf("%.2f", g$prot_rate)),
        `Predator`  = ifelse(is.na(g$pred_rate), "—", sprintf("%.2f", g$pred_rate)),
        `Traps` = g$n_traps, `Caught` = g$catches, `Neglected` = g$n_neglected,
        `Traps/km²` = ifelse(is.na(dens), "—", sprintf("%.0f", dens)),
        Status = badge, check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, container = .cov_gaps_header(per_cam), rownames = FALSE, escape = -ncol(df),
        selection = "none", class = "stripe hover row-border",
        options = list(dom = "t", ordering = FALSE, paging = FALSE))   # show every line, no hidden rows
    })

    # ---- per-reserve network density (structural coverage — is it dense enough?) ----
    output$density <- DT::renderDT({
      cov <- ik_coverage(ik_data); validate(need(!is.null(cov) && nrow(cov), "No coverage stats."))
      rsv <- .ik_nz(selection()$reserve); if (!is.null(rsv)) cov <- cov[cov$reserve %in% rsv, , drop = FALSE]
      validate(need(nrow(cov), "No reserves in this selection."))
      df <- data.frame(Reserve = cov$reserve,
        `Area (ha)`   = ifelse(is.na(cov$area_km2), "—", format(round(cov$area_km2 * 100), big.mark = ",")),
        Cameras = cov$n_cameras, Traps = cov$n_traps,
        `Cameras/km²` = ifelse(is.na(cov$cameras_per_km2), "—", sprintf("%.1f", cov$cameras_per_km2)),
        `Traps/km²`   = ifelse(is.na(cov$traps_per_km2),   "—", sprintf("%.1f", cov$traps_per_km2)),
        `Trap spacing (m)` = ifelse(is.na(cov$mean_trap_spacing_m), "—", format(round(cov$mean_trap_spacing_m), big.mark = ",")),
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "none", class = "stripe hover row-border",
        options = list(dom = "t", ordering = FALSE, paging = FALSE))   # every reserve, no hidden rows
    })
  })
}
