# neighbourhood.R (module) — "Neighbourhood": pick an ANCHOR (a monitoring LINE or a whole RESERVE).
# For a line, the camera story is the LINE's own cameras and a RADIUS sets how far out to count nearby
# TRAPS (distance-only, so a trap just over a reserve boundary still counts); a reserve uses all of its
# own cameras + traps (no radius). Track season by season the protected & predator presence on those
# cameras AND the predators caught in the nearby traps. Tests the local story (line) and, at reserve
# level, "does camera predator activity align with what the traps catch?". A deeper analysis under the
# Outcomes menu. Camera presence is a pooled per-camera-hour rate (exploratory); trap catches are a
# seasonal count (a kill is only known to its check window). Data + click-drill from
# ik_neighbourhood_series() / ik_neighbourhood_records().

#' "How to read this" help body for Neighbourhood — a tabbed walkthrough. `line_norm` is the
#' per-LINE camera-hour scale (project config) woven into the calculation tab. @keywords internal
neighbourhood_help_body <- function(line_norm = 2000) {
  P  <- function(...) tags$p(...)
  ch <- format(line_norm, big.mark = ",")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "Pick an ", tags$b("anchor"), " — a monitoring ", tags$b("line"), " or a whole ",
        tags$b("reserve"), ". For a line, the cameras are the ", tags$b("line's own"), " and the ",
        tags$b("radius"), " sets how far out to count ", tags$b("nearby traps"), "; a reserve uses all of ",
        "its own cameras and traps (no radius). The chart then tracks, season by season, three things:"),
      tags$ul(
        tags$li(tags$b("Protected"), " activity on the line's cameras (your chosen protected species)."),
        tags$li(tags$b("Predator"), " activity on the line's cameras (your chosen predators)."),
        tags$li(tags$b("Predators caught"), " in the nearby traps — those within the radius (or, for a reserve, all of its traps).")),
      P("Click any point to see the records behind it. It's a focused, local companion to the ",
        tags$b("Are we winning?"), " network view — zoomed to one place.")),
    tabPanel(
      "Reading it", icon = icon("chart-line"),
      P(tags$br(), "Two questions, depending on the anchor:"),
      tags$ul(
        tags$li(tags$b("Line"), " — the ", tags$em("local story"), ": is a protected species holding on ",
                "here, and what are the cameras and nearby traps seeing of predators around it?"),
        tags$li(tags$b("Reserve"), " — the ", tags$em("alignment check"), ": does camera predator activity move ",
                "with what the traps catch? If cameras show lots of predator activity but traps catch little, ",
                "the control may be thin or ineffective there; if both fall together, it may be working.")),
      P("The two camera lines are ", tags$b("rates"), " (comparable across seasons); the trap line is a ",
        tags$b("count"), " of catches. Read them for ", tags$b("shape and timing"), ", not a one-to-one scale — ",
        "they're different measures. Treat it as ", tags$b("exploratory"), ": it points you at places to look, ",
        "not proof of cause.")),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Line anchor"), " — the line's ", tags$b("own cameras"), ", plus the ",
                tags$b("traps within the chosen radius"), " of them (from the pre-computed spatial adjacency). ",
                "Traps are counted by ", tags$b("distance"), ", so a trap just over a reserve boundary still ",
                "counts — predators don't respect boundaries."),
        tags$li(tags$b("Reserve anchor"), " — every camera and trap ", tags$b("tagged to that reserve"),
                " in the data (its ", tags$em("membership"), ", not a spatial test or radius — that's why ",
                "the radius control disappears). The dashed ", tags$b("Boundary"), " on the map is just the ",
                "hull of those points, for context; it isn't what selects them, so a reserve's trap can sit ",
                "outside that hull and still count."),
        tags$li(tags$b("Camera rate"), " — detections pooled over the line's cameras ÷ their combined ",
                "camera-hours, × ", ch, " (the per-", tags$b("line"), " scale — ≈ a full line of ~4 cameras — so it ",
                "reads on the same RAI scale as the rest of the app); net of likely duplicates."),
        tags$li(tags$b("Trap catches"), " — predators of the chosen species caught at trap checks in the ",
                "neighbourhood, as a ", tags$b("per-season count.")),
        tags$li(tags$b("Why trap is a count, not a timing"), " — a kill is only known to the ", tags$b("check "),
                tags$b("window"), " (we know it happened sometime between two checks, not the day), so a within-",
                "season count is the honest resolution; it can't be lined up to a camera detection in time."),
        tags$li(tags$b("Grain"), " — toggle ", tags$b("By season"), " (every summer pooled, etc.) or ",
                tags$b("By year"), " (each season-year separately).")),
      P(tags$em("All exploratory and observational — neighbourhoods overlap and traplines aren't randomly ",
                "placed, so read alignment as suggestive.")))
  )
}

#' Neighbourhood nav panel. @param id Module id. @param ik_data The container (anchor choices are
#'   baked into the UI — single-selects in this dropdown nav panel won't keep a server-set default).
#' Neighbourhood sidebar controls — the anchor (Reserve + optional Line), the Traps-within radius (line
#' anchor only), Protected/Predator, and the time grain. Built in the module's namespace, rendered in the
#' global sidebar (ui.R). The Line list CASCADES from the Reserve (populated server-side); "(whole reserve)"
#' anchors the reserve itself. ALL of these — Reserve + Line included — go in the tinted "View options"
#' group: they'd normally be shared "Filters", so grouping them as view settings is exactly what flags
#' that here they aren't. The "(whole reserve)" sentinel is a NON-empty value so selectize lets you pick it
#' back after choosing a line (an empty value reads as the cleared/placeholder state). @keywords internal
neighbourhood_controls <- function(id, ik_data) {
  ns     <- NS(id)
  loc    <- .nbhd_locations(ik_data)
  cams   <- loc[!is.na(loc$source_type) & loc$source_type == "camera" & is.finite(loc$latitude), , drop = FALSE]
  res_ch <- sort(unique(cams$reserve)); res_ch <- stats::setNames(res_ch, res_ch)
  max_r  <- (ik_data$meta$proximity %||% list())$max_radius_m %||% 2000
  rad_ch <- c("250 m" = 250, "500 m" = 500, "750 m" = 750, "1 km" = 1000); rad_ch <- rad_ch[rad_ch <= max_r]
  div(class = "ik-selection ik-view-controls",
    tags$div(class = "ik-view-controls-h", "View options"),
    selectInput(ns("anchor_reserve"), "Reserve", choices = res_ch, selected = unname(res_ch)[1]),
    # Line cascades from the Reserve (populated server-side); "(whole reserve)" anchors the reserve itself.
    selectInput(ns("anchor_line"), "Line", choices = c("Whole reserve (all lines)" = "__all__"), selected = "__all__"),
    conditionalPanel("input.anchor_line != '__all__'", ns = ns,  # a line anchor uses the Traps-within radius
      selectInput(ns("radius"), "Traps within", choices = rad_ch,
                  selected = if (500 %in% rad_ch) 500 else max(rad_ch))),
    selectInput(ns("prot"), "Protected", choices = NULL, multiple = TRUE),
    selectInput(ns("pred"), "Predator",  choices = NULL, multiple = TRUE),
    radioButtons(ns("grain"), "Time grain", inline = TRUE,
                 choices = c("By season" = "season", "By year" = "year"), selected = "season"))
}

neighbourhood_ui <- function(id, ik_data) {
  ns <- NS(id)
  nav_panel(
    "Neighbourhood", value = "neighbourhood", icon = icon("circle-nodes"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/neighbourhood.css")),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),
    tags$script(src = .ik_asset("js/maps.js")),                            # leaflet resize-on-tab-show
    div(class = "ik-nbhd",
        .ik_page_header("Neighbourhood — protected, predators & nearby trapping",
            description = tagList(
              "Pick a ", tags$b("reserve"), " in the sidebar — and optionally a ", tags$b("line"), " within it. ",
              "See how ", tags$b("protected"), " and ", tags$b("predator"), " activity on the cameras moves over ",
              "time, alongside the predators caught in the ", tags$b("traps nearby"), " — for a line, those within ",
              "a radius you set; a whole reserve uses all its own traps."),
            help = .ik_info(ns("nbhd_help"), "Neighbourhood — how to read this",
                     neighbourhood_help_body((ik_data$meta$camera$rai %||% list())$norm_hours %||% 2000))),
        uiOutput(ns("intro")),
        tags$p(class = "ik-nbhd-hint", "Click a point to see the records behind it."),
        plotOutput(ns("panel"), height = "440px", click = ns("panel_click")),
        # Spatial view of the same neighbourhood (all-time), with hover-to-highlight traps.
        tags$h5(class = "ik-nbhd-maptitle", "On the map — the cameras and nearby traps"),
        tags$p(class = "ik-nbhd-hint",
          tags$b(tags$span(style = "color:#2e7d32", "Green")), " = protected on camera · ",
          tags$b(tags$span(style = "color:#c62828", "red")), " = predators on camera · ",
          tags$b(tags$span(style = "color:#6a3d9a", "purple")), " = predators caught in traps · ",
          tags$b(tags$span(style = "color:#2c7fb8", "blue")), " = camera location · ",
          tags$b(tags$span(style = "color:#8a8a8a", "grey")), " = trap location (the ", tags$b("Device"),
          " layer, underneath). All-time. ", tags$b("Hover a trap in the table"), " to highlight it on the map."),
        # Map beside the nearby-traps table (wraps to stacked on narrow).
        layout_columns(class = "ik-maps-split", col_widths = breakpoints(sm = 12, lg = c(8, 4)),
          leaflet::leafletOutput(ns("map"), height = "60vh"),
          div(class = "ik-maps-side", style = "max-height:60vh;",
            uiOutput(ns("map_note")),
            DT::DTOutput(ns("traps_table")))))
  )
}

#' Neighbourhood server.
#' @param id Module id. @param ik_data The container.
#' @param prefer_scientific reactive name preference. @param color_mode reactive theme.
neighbourhood_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                                 color_mode = reactive("light")) {
  moduleServer(id, function(input, output, session) {
    is_dark <- reactive(identical(color_mode(), "dark"))
    sg <- ik_species_groups(ik_data)
    .role_taxa <- function(role) { l <- unique(sg$label[sg$role == role & !is.na(sg$monitor)])
      stats::setNames(lapply(l, function(x) sg$scientificName[sg$label == x & !is.na(sg$scientificName)]), l) }
    pred_taxa <- .role_taxa("predator"); prot_taxa <- .role_taxa("protected")
    splits <- unique(sg$label[which(sg$split)])
    prefer <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    .pred_def <- paste0("grp:", names(pred_taxa)[1])
    .prot_def <- paste0("grp:", names(prot_taxa)[1])
    rlab  <- function(r) { r <- as.numeric(r); if (r >= 1000) sprintf("%g km", r / 1000) else sprintf("%g m", r) }

    observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      updateSelectInput(session, "prot", choices = ik_species_choices(prot_taxa, ik_data, p, splits), selected = keep(isolate(input$prot), .prot_def))
      updateSelectInput(session, "pred", choices = ik_species_choices(pred_taxa, ik_data, p, splits), selected = keep(isolate(input$pred), .pred_def))
    })
    observe({                                                  # Reserve choices follow the active datasets
      rc <- sort(unique(ik_neighbourhood_lines(ik_data)$reserve))
      updateSelectInput(session, "anchor_reserve", choices = stats::setNames(rc, rc),
        selected = if ((isolate(input$anchor_reserve) %||% "") %in% rc) isolate(input$anchor_reserve) else rc[1])
    })
    observeEvent(input$anchor_reserve, {                       # Line CASCADES from the Reserve ("__all__" = whole reserve)
      ll <- ik_neighbourhood_lines(ik_data); ll <- ll[ll$reserve == input$anchor_reserve, , drop = FALSE]
      lv <- paste(ll$reserve, ll$line, sep = "|")
      updateSelectInput(session, "anchor_line",
        choices  = c("Whole reserve (all lines)" = "__all__", stats::setNames(lv, sprintf("Line %s", ll$line))),
        selected = if ((isolate(input$anchor_line) %||% "") %in% lv) isolate(input$anchor_line) else "__all__")
    }, ignoreNULL = FALSE)

    # Anchor model: a Line picked = anchor that line (with the Traps-within radius); "__all__" = the whole
    # reserve (uses all its traps). lvl/akey derive from the pickers — no separate Anchor radio.
    lvl  <- reactive(if (nzchar(input$anchor_line %||% "") && !identical(input$anchor_line, "__all__")) "line" else "reserve")
    akey <- reactive(if (identical(lvl(), "line")) input$anchor_line else input$anchor_reserve)
    scope_lab <- function(s) if (identical(attr(s, "level"), "reserve")) sprintf("Across %s", attr(s, "label"))
                             else sprintf("%s · traps within %s", attr(s, "label"), rlab(input$radius))

    series <- reactive({
      k <- akey(); if (is.null(k) || !nzchar(k)) return(NULL)
      ik_neighbourhood_series(ik_data, lvl(), k, as.numeric(input$radius %||% 500),
        predator_sci  = ik_resolve_species_choice(input$pred, pred_taxa),
        protected_sci = ik_resolve_species_choice(input$prot, prot_taxa),
        by = input$grain %||% "season")
    }) |> bindCache(lvl(), akey(), input$radius, input$pred, input$prot, input$grain, ik_active_datasets())

    output$intro <- renderUI({
      s <- series()
      if (is.null(s)) return(tags$p(class = "ik-nbhd-hint", "Pick an anchor and at least one species."))
      n_cam <- attr(s, "n_cam"); n_trap <- attr(s, "n_trap")
      pl <- function(n) if (identical(as.integer(n), 1L)) "" else "s"
      where <- if (identical(attr(s, "level"), "reserve"))
        sprintf("Across %s: %d camera%s and %d trap%s.", attr(s, "label"), n_cam, pl(n_cam), n_trap, pl(n_trap))
      else
        sprintf("%s: %d camera%s on the line, and %d trap%s within %s (counted by distance, so traps just over a reserve boundary still count).",
                attr(s, "label"), n_cam, pl(n_cam), n_trap, pl(n_trap), rlab(input$radius))
      tags$p(class = "ik-nbhd-meta", where,
        " Camera lines are an effort-adjusted detection rate (net, exploratory); the trap line counts predators caught that season.")
    })

    output$panel <- renderPlot({
      s <- series(); validate(need(!is.null(s) && nrow(s), "Pick an anchor and at least one species."))
      cam_facet <- attr(s, "cam_facet")
      s$period <- factor(s$period, levels = unique(s$period[order(s$order)]))
      s$facet  <- factor(s$facet, levels = c(cam_facet, "Predators caught (nearby traps)"))
      pal <- c(Protected = "#2e7d32", Predator = "#c62828", Catches = "#6a3d9a")
      ggplot2::ggplot(s, ggplot2::aes(.data$period, .data$value, colour = .data$series, group = .data$series)) +
        ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) + ggplot2::geom_point(size = 1.9, na.rm = TRUE) +
        ggplot2::facet_wrap(ggplot2::vars(.data$facet), ncol = 1, scales = "free_y") +
        ggplot2::scale_colour_manual(values = pal, breaks = c("Protected", "Predator", "Catches")) +
        ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom",
          strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.05), colour = ik_plot_ink(is_dark())),
          strip.background = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.1, "lines"))
    }, bg = "transparent")

    # ---- click a point (or a trap row) → the records behind it (Records ↔ Record details) ----
    nb_obs  <- reactiveVal(NULL)   # the records df shown in the modal
    nb_open <- reactiveVal(NULL)   # observationID open in the detail tab
    .nb_modal <- function(title, subtitle) {                    # the shared records modal (plot + trap clicks)
      showModal(modalDialog(
        title = .ik_modal_title(title, subtitle),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("nb_tabs"),
          tabPanel("Records",        icon = icon("list"),        DT::dataTableOutput(session$ns("nb_table"))),
          tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("nb_record"))))))
      hideTab(session = session, inputId = "nb_tabs", target = "Record details")
    }

    observeEvent(input$panel_click, {
      cl <- input$panel_click; s <- series(); k <- akey()
      if (is.null(cl) || is.null(s) || is.null(k) || !nzchar(k)) return()
      lv <- unique(s$period[order(s$order)]); pidx <- round(cl$x)
      if (is.na(pidx) || pidx < 1 || pidx > length(lv)) return()
      period <- lv[pidx]; facet <- cl$panelvar1
      if (is.null(facet) || is.na(facet)) return()
      cand <- s[s$period == period & s$facet == facet, , drop = FALSE]; if (!nrow(cand)) return()
      ser  <- cand$series[which.min(abs(cand$value - cl$y))]               # nearest series by y in this facet
      seasons <- .nbhd_period_seasons(ik_data, period, input$grain %||% "season")
      if (identical(facet, attr(s, "cam_facet"))) {
        src <- "camera"
        sci <- if (identical(ser, "Protected")) ik_resolve_species_choice(input$prot, prot_taxa)
               else                             ik_resolve_species_choice(input$pred, pred_taxa)
        what <- if (identical(ser, "Protected")) "Protected detections" else "Predator detections"
      } else {
        src <- "trap"; sci <- ik_resolve_species_choice(input$pred, pred_taxa); what <- "Predators caught"
      }
      nb_obs(ik_neighbourhood_records(ik_data, lvl(), k, as.numeric(input$radius %||% 500), sci, seasons, src))
      nb_open(NULL)
      .nb_modal(sprintf("%s — %s", what, period),
                sprintf("%s — click a record for its full detail.", scope_lab(s)))
    })

    output$nb_table <- DT::renderDT({
      o <- nb_obs(); validate(need(!is.null(o) && nrow(o), "No records behind this point."))
      p <- prefer()
      has_t <- format(o$when, "%H:%M:%S") != "00:00:00"
      when_lab <- ifelse(has_t, format(o$when, "%Y-%m-%d %H:%M"), format(o$when, "%Y-%m-%d"))
      df <- data.frame(When = when_lab, Species = ik_species_label(o$scientificName, ik_data, p),
        Count = o$count, Location = o$name, `Dist (m)` = round(o$distance_m), .obs = o$observationID,
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1L))))
    })
    observeEvent(input$nb_table_rows_selected, {
      i <- input$nb_table_rows_selected; o <- nb_obs()
      if (length(i) && !is.null(o) && i <= nrow(o)) {
        nb_open(o$observationID[i]); showTab(session = session, inputId = "nb_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("nb_table"), NULL)
    })
    observeEvent(input$nb_back, updateTabsetPanel(session, input$nb_back$tabset, selected = input$nb_back$to))
    output$nb_record <- renderUI({
      if (is.null(nb_open())) return(tags$p(class = "ik-nbhd-hint", "Pick a record from the Records tab."))
      ob <- ik_observation(ik_data, nb_open()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(session$ns("nb_back"), "nb_tabs", "Records", "Back to records"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("nb_subtabs")))
    })

    # ---- Map: the same neighbourhood, spatially (all-time), with the traps table hover-linked to it ----
    map_data <- reactive({
      k <- akey(); if (is.null(k) || !nzchar(k)) return(NULL)
      ik_neighbourhood_map(ik_data, lvl(), k, as.numeric(input$radius %||% 500),
        predator_sci  = ik_resolve_species_choice(input$pred, pred_taxa),
        protected_sci = ik_resolve_species_choice(input$prot, prot_taxa))
    }) |> bindCache(lvl(), akey(), input$radius, input$pred, input$prot, ik_active_datasets())

    traps_shown <- reactiveVal(NULL)
    mproxy <- function() leaflet::leafletProxy("map", session)

    output$map <- leaflet::renderLeaflet({
      locs <- ik_data$app$geography$locations
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
      m <- leaflet::addProviderTiles(m, canvas, group = "Map")
      m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
      pns <- c("boundary", "device", "caught", "protected", "predators", "highlight")  # device lowest; predator RING above protected dot
      for (pn in pns) m <- leaflet::addMapPane(m, pn, zIndex = 410 + 10 * match(pn, pns))
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
        overlayGroups = c("Protected", "Predators", "Catches", "Device", "Boundary"),
        options = leaflet::layersControlOptions(collapsed = FALSE))   # toggles visible (like Coverage)
      if (nrow(locs)) m <- leaflet::fitBounds(m, min(locs$longitude), min(locs$latitude), max(locs$longitude), max(locs$latitude))
      m
    })
    outputOptions(output, "map", suspendWhenHidden = FALSE)   # render from load so proxy layers land

    observeEvent(color_mode(), {
      leaflet::addProviderTiles(leaflet::clearGroup(mproxy(), "Map"),
        if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron, group = "Map")
    }, ignoreInit = TRUE)

    plab <- function() {                                        # readable label for the selected predator(s)
      pc <- ik_species_choices(pred_taxa, ik_data, prefer(), splits)
      nm <- names(pc)[match(input$pred, pc)]; nm <- nm[!is.na(nm)]
      if (length(nm)) paste(nm, collapse = ", ") else "predators"
    }
    observe({                                                   # draw the neighbourhood + fit to it
      p <- mproxy()
      for (g in c("Protected", "Predators", "Catches", "Device", "TrapHighlight")) leaflet::clearGroup(p, g)
      leaflet::clearControls(p)
      md <- map_data(); req(!is.null(md))
      cam <- md$cams; trp <- md$traps
      rad <- function(v, lo, hi) ik_marker_radius(v, lo, hi)     # catches: own (count) scale
      # Protected & predator share ONE absolute RAI→radius scale (area-fair sqrt, capped at the combined
      # max), so a predator circle reads correctly against the protected ones — NOT each rescaled to its
      # own min/max (which, with a line's ~4 cameras, blew the predators up regardless of their RAI).
      rai_cap <- max(c(cam$prot_rai, cam$pred_rai, 1e-9), na.rm = TRUE)
      rai_rad <- function(v, lo, hi) lo + (hi - lo) * sqrt(pmax(v, 0)) / sqrt(rai_cap)
      cmh <- format(md$per_cam, big.mark = ","); pl <- plab()
      gap <- ifelse(is.na(trp$mean_interval_days), "—", paste0("~", round(trp$mean_interval_days), " d gap"))
      # Device (bottom) — every camera (blue) and every nearby trap (grey). Detection / capture markers
      # sit on top and swallow the dot where there's a record; a bare dot = "device here, nothing of the
      # selection". Cameras blue, traps grey, so the two device kinds stay distinguishable.
      if (nrow(cam)) leaflet::addCircleMarkers(p, data = cam, lng = ~longitude, lat = ~latitude, group = "Device",
        radius = 3, stroke = FALSE, fillColor = "#2c7fb8", fillOpacity = 0.7,
        label = lapply(sprintf("<b>%s</b><br>Camera", cam$name), htmltools::HTML),
        options = leaflet::pathOptions(pane = "device"))
      if (nrow(trp)) leaflet::addCircleMarkers(p, data = trp, lng = ~longitude, lat = ~latitude, group = "Device",
        radius = 3, stroke = FALSE, fillColor = "#8a8a8a", fillOpacity = 0.55,
        label = lapply(sprintf("<b>%s</b><br>Trap &middot; %d %s caught<br>%s checks &middot; %s &middot; %s", trp$name, trp$catches, pl,
                 ifelse(is.na(trp$n_checks), "—", trp$n_checks), gap, ifelse(is.na(trp$status), "—", trp$status)), htmltools::HTML),
        options = leaflet::pathOptions(pane = "device"))
      tc <- trp[trp$catches > 0, , drop = FALSE]               # Caught (purple) — sized by catches
      if (nrow(tc)) leaflet::addCircleMarkers(p, data = tc, lng = ~longitude, lat = ~latitude, group = "Catches",
        radius = rad(tc$catches, 6, 22), stroke = TRUE, color = "#ffffff", weight = 1, fillColor = "#6a3d9a", fillOpacity = 0.85,
        label = lapply(sprintf("<b>%s</b><br><b>%d %s caught</b> (all-time)", tc$name, tc$catches, pl), htmltools::HTML),
        options = leaflet::pathOptions(pane = "caught"))
      rc <- cam[cam$pred_rai > 0, , drop = FALSE]              # Predators (red RING on top, so green shows through)
      if (nrow(rc)) leaflet::addCircleMarkers(p, data = rc, lng = ~longitude, lat = ~latitude, group = "Predators",
        radius = rai_rad(rc$pred_rai, 7, 24), fill = FALSE, stroke = TRUE, color = "#c62828", weight = 2.5, opacity = 0.95,
        label = lapply(sprintf("<b>%s</b><br>Predator RAI %.2f / %s ch", rc$name, rc$pred_rai, cmh), htmltools::HTML),
        options = leaflet::pathOptions(pane = "predators"))
      pc <- cam[cam$prot_rai > 0, , drop = FALSE]              # Protected (solid green dot, same RAI scale)
      if (nrow(pc)) leaflet::addCircleMarkers(p, data = pc, lng = ~longitude, lat = ~latitude, group = "Protected",
        radius = rai_rad(pc$prot_rai, 7, 24), stroke = TRUE, color = "#ffffff", weight = 1.5, fillColor = "#2e7d32", fillOpacity = 0.85,
        label = lapply(sprintf("<b>%s</b><br>Protected RAI %.2f / %s ch", pc$name, pc$prot_rai, cmh), htmltools::HTML),
        options = leaflet::pathOptions(pane = "protected"))
      leaflet::addLegend(p, "bottomright", colors = c("#2e7d32", "#c62828", "#6a3d9a", "#2c7fb8", "#8a8a8a"),
        labels = c("Protected (cam)", "Predator (cam)", "Catches (trap)", "Camera", "Trap"), opacity = 0.9)
      allc <- rbind(cam[, c("longitude", "latitude")], trp[, c("longitude", "latitude")])
      if (nrow(allc)) leaflet::fitBounds(p, min(allc$longitude), min(allc$latitude),
        max(allc$longitude), max(allc$latitude), options = list(padding = c(25, 25)))
    })

    observe({                                                   # Boundary — the anchor's reserve footprint (hull)
      p <- mproxy(); leaflet::clearGroup(p, "Boundary")
      k <- akey(); if (is.null(k) || !nzchar(k)) return()
      rn <- if (identical(lvl(), "reserve")) k else strsplit(k, "|", fixed = TRUE)[[1]][1]   # line: its reserve
      locs <- ik_active_locations(ik_data)
      locs <- locs[!is.na(locs$reserve) & locs$reserve == rn &
                     is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      h <- ik_selection_hulls(locs, "reserve"); if (is.null(h) || !nrow(h)) return()
      edge <- if (is_dark()) "#cfd8dc" else "#37474f"
      leaflet::addPolygons(p, data = h, group = "Boundary", fill = TRUE, fillColor = edge, fillOpacity = 0.05,
        stroke = FALSE, options = leaflet::pathOptions(pane = "boundary", interactive = FALSE))
      leaflet::addPolygons(p, data = h, group = "Boundary", label = ~reserve,
        labelOptions = leaflet::labelOptions(textsize = "12px", direction = "auto", sticky = TRUE),
        highlightOptions = leaflet::highlightOptions(weight = 3, color = "#1565c0", bringToFront = TRUE),
        fill = FALSE, stroke = TRUE, color = edge, weight = 1.5, dashArray = "5,6",
        options = leaflet::pathOptions(pane = "boundary"))
    })

    output$map_note <- renderUI({
      md <- map_data(); if (is.null(md)) return(NULL)
      tags$p(class = "ik-nbhd-meta", tags$b("Nearby traps"), sprintf(" — %d trap%s, %d with a catch (all-time). ",
        nrow(md$traps), if (identical(nrow(md$traps), 1L)) "" else "s", sum(md$traps$catches > 0)),
        tags$b("Hover"), " a row to locate it on the map; ", tags$b("click"), " it for its catch records.")
    })

    output$traps_table <- DT::renderDT({
      md <- map_data(); validate(need(!is.null(md) && nrow(md$traps), "No traps in this neighbourhood."))
      t <- md$traps[order(md$traps$distance_m, -md$traps$catches), , drop = FALSE]
      traps_shown(t)
      df <- data.frame(
        Trap = t$name,
        `Dist (m)` = ifelse(is.na(t$distance_m), NA_real_, round(t$distance_m)),
        Catches = t$catches,
        Checks = ifelse(is.na(t$n_checks), 0L, as.integer(t$n_checks)),
        `Mean gap (d)` = ifelse(is.na(t$mean_interval_days), NA_real_, round(t$mean_interval_days)),
        Status = ifelse(is.na(t$status), "—", t$status),
        .loc = t$location_id, check.names = FALSE, stringsAsFactors = FALSE)
      thx <- function(label, tip = NULL) tags$th(label, if (!is.null(tip)) .ik_hint(tip))
      cont <- tags$table(class = "display", tags$thead(tags$tr(
        thx("Trap"),
        thx("Dist (m)", "Straight-line distance to the nearest camera on the anchor line."),
        thx("Catches", "Selected predator(s) caught at this trap, all-time."),
        thx("Checks", "Times this trap has been checked."),
        thx("Mean gap (d)", "Average days between checks — the servicing cadence (lower = checked more often); the activity measure that still works when there are no catches."),
        thx("Status", "Servicing health as at the most recent check data — good / watch / neglected (overdue) / dormant / historic."),
        thx(""))))
      DT::datatable(df, container = cont, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 7, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(visible = FALSE, targets = 6)),     # hide .loc (index 6)
          rowCallback = DT::JS(sprintf(
            "function(r,d){r.addEventListener('mouseenter',function(){Shiny.setInputValue('%s',d[6],{priority:'event'});});r.addEventListener('mouseleave',function(){Shiny.setInputValue('%s','',{priority:'event'});});}",
            session$ns("trap_hover"), session$ns("trap_hover")))))
    })

    observeEvent(input$trap_hover, {                            # hover a trap row → ring THAT trap (by id, robust to sort/page)
      p <- mproxy(); leaflet::clearGroup(p, "TrapHighlight")
      loc <- input$trap_hover; t <- traps_shown()
      if (is.null(loc) || !nzchar(loc) || is.null(t)) return()
      d <- t[t$location_id == loc, , drop = FALSE]
      if (!nrow(d) || !is.finite(d$latitude[1]) || !is.finite(d$longitude[1])) return()
      leaflet::addCircleMarkers(p, data = d[1, , drop = FALSE], lng = ~longitude, lat = ~latitude, group = "TrapHighlight",
        radius = 11, stroke = TRUE, color = "#1565c0", weight = 3, fill = FALSE,
        options = leaflet::pathOptions(pane = "highlight"))
    })

    # ---- click a trap row → its full CHECK HISTORY (all-time) → Record Details (like the Bait modal) ----
    # The table is trap-centric (Checks, Mean gap, Status), so the drill is the trap's check log — not
    # just its kills (a 0-catch trap still has a meaningful history of "still set" checks).
    th_loc  <- reactiveVal(NULL); th_name <- reactiveVal(NULL); th_open <- reactiveVal(NULL)
    observeEvent(input$traps_table_rows_selected, {
      i <- input$traps_table_rows_selected; t <- traps_shown()
      if (!length(i) || is.null(t) || i > nrow(t)) return()
      th_loc(t$location_id[i]); th_name(t$name[i]); th_open(NULL)
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s — check history", t$name[i]),
                                "Every check at this trap (all-time) — click one for its full record."),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("th_tabs"),
          tabPanel("Trap history",   icon = icon("clock-rotate-left"), DT::dataTableOutput(session$ns("th_table"))),
          tabPanel("Record details", icon = icon("circle-info"),       uiOutput(session$ns("th_record"))))))
      hideTab(session = session, inputId = "th_tabs", target = "Record details")
      DT::selectRows(DT::dataTableProxy("traps_table"), NULL)
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
        th_open(ch$observationID[i])
        showTab(session = session, inputId = "th_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("th_table"), NULL)
    })
    observeEvent(input$th_back, updateTabsetPanel(session, input$th_back$tabset, selected = input$th_back$to))
    output$th_record <- renderUI({
      if (is.null(th_open())) return(tags$p(class = "ik-nbhd-hint", "Pick a check from the Trap history tab."))
      ob <- ik_observation(ik_data, th_open()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(session$ns("th_back"), "th_tabs", "Trap history", "Back to trap history"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("th_subtabs")))
    })
  })
}
