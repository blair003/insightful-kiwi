# species.R (module) — a per-species dashboard page. One instance per taxon spec (a species GROUP or
# a split sub-species, from ik_species_taxa). Tabs are by QUESTION with camera + trap COMBINED inside
# (not separate device tabs): Trend · Records · Map [· Behaviour · Where · Bait · Co-occurrence — later].
# Reuses the existing metric/records/map helpers; respects the active-datasets toggle via those.
# Phase 1: Trend (RAI + catch-rate over season/year, optional 2nd-species overlay) · Records · Map.

#' "How to read this" help body for a species page. @keywords internal
species_help_body <- function(norm_hours = 2000, norm_trap = 100) {
  P <- function(...) tags$p(...)
  tabsetPanel(type = "tabs",
    tabPanel("What it shows", icon = icon("circle-question"),
      P(tags$br(), "Everything this project knows about one species (or species group), camera and trap ",
        "together. ", tags$b("Trend"), " — how its activity and catches move over time; ", tags$b("Records"),
        " — every detection/capture; ", tags$b("Map"), " — where it's seen and caught."),
      P("Period & reserve come from the sidebar (the Trend spans all time by design; the others honour ",
        "the period).")),
    tabPanel("Trend", icon = icon("chart-line"),
      P(tags$br(), tags$b("Camera activity"), " is RAI (detections per ", format(norm_hours, big.mark = ","),
        " camera-hours); ", tags$b("Catch rate"), " is captures per ", format(norm_trap, big.mark = ","),
        " trap-nights — each its own panel (different units, don't compare heights between them). Lines are ",
        "the network mean across reserves; toggle ", tags$b("By season / By year"), ". Add a ",
        tags$b("compare"), " species to overlay a second line on the matching panel.")),
    tabPanel("How it's calculated", icon = icon("calculator"),
      tags$ul(tags$br(),
        tags$li(tags$b("RAI / catch-rate"), " — the same effort-adjusted metrics as the Overview, computed ",
                "per reserve then averaged to a network mean ± SE."),
        tags$li(tags$b("A group"), " (e.g. Mustelids) pools its members; a split sub-species (Stoat, Ferret) ",
                "counts only its own. Ambiguous IDs (e.g. weasel/stoat) stay in the group total."),
        tags$li(tags$b("Map"), " — per-location RAI (camera) and catches (trap) for the selected period."))))
}

#' Species dashboard nav panel. @param id Module id. @param spec One taxon spec (ik_species_taxa).
#'   @param ik_data The container (overlay choices + norms baked into the UI).
species_dashboard_ui <- function(id, spec, ik_data = NULL) {
  ns <- NS(id)
  nh <- (ik_data$meta$camera$rai %||% list())$norm_hours %||% 2000
  nt <- (ik_data$meta$trapping$rate %||% list())$norm_trap_days %||% 100
  # Overlay choices: every OTHER taxon page (compare this species to another). Baked (dropdown-lazy).
  others <- Filter(function(s) s$key != spec$key, ik_species_taxa(ik_data))
  ov_ch  <- c("None" = "__none__", stats::setNames(vapply(others, `[[`, "", "key"), vapply(others, `[[`, "", "label")))
  sci_lab <- paste(sort(unique(spec$sci)), collapse = " · ")
  nav_panel(
    spec$label, value = spec$key, icon = icon(if (isTRUE(spec$member)) "paw" else "dna"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/species.css")),
    tags$script(src = .ik_asset("js/maps.js")),
    div(class = "ik-species",
        div(class = "ik-species-titlebar",
            tags$h3(class = "ik-species-title", spec$label),
            .ik_info(ns("help"), paste(spec$label, "— how to read this"), species_help_body(nh, nt))),
        tags$p(class = "ik-species-sub", tags$em(sci_lab)),
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Trend", icon = icon("chart-line"),
            div(class = "ik-species-controls",
                radioButtons(ns("by"), NULL, inline = TRUE,
                             choices = c("By season" = "season", "By year" = "year"), selected = "season"),
                selectInput(ns("overlay"), "Compare", choices = ov_ch, selected = "__none__", width = "220px")),
            plotOutput(ns("trend"), height = "460px")),
          tabPanel("Records", icon = icon("list"),
            tags$p(class = "ik-species-hint", "Every detection (camera) and capture (trap) of this species in the selection."),
            DT::DTOutput(ns("records"))),
          tabPanel("Map", icon = icon("map"),
            tags$p(class = "ik-species-hint",
                   "Where this species is detected on camera and caught in traps (toggle layers top-right)."),
            leaflet::leafletOutput(ns("map"), height = "62vh"))
        ))
  )
}

#' Species dashboard server.
#' @param id Module id. @param spec One taxon spec. @param ik_data The container.
#' @param selection Reactive selection SPEC (period + reserve from the sidebar).
#' @param prefer_scientific reactive name preference. @param color_mode reactive theme.
#' @param active reactive: is this page's nav visible (gates the heavy tabs).
species_dashboard_server <- function(id, spec, ik_data, selection, prefer_scientific = reactive(FALSE),
                                     color_mode = reactive("light"), active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    is_dark <- reactive(identical(color_mode(), "dark"))
    prefer  <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    taxa    <- stats::setNames(list(spec$sci), spec$label)        # this page's taxon (constant)
    nh <- (ik_data$meta$camera$rai %||% list())$norm_hours %||% 2000
    nt <- (ik_data$meta$trapping$rate %||% list())$norm_trap_days %||% 100
    per_cam <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500

    # overlay taxon (another page's sci) → a 2nd line on the matching panel
    overlay_taxa <- reactive({
      v <- input$overlay %||% "__none__"; if (identical(v, "__none__")) return(NULL)
      o <- Filter(function(s) s$key == v, ik_species_taxa(ik_data))[[1]]
      if (is.null(o)) NULL else stats::setNames(list(o$sci), o$label)
    })

    # ---- Trend ----
    trend <- reactive({
      req(active())
      ik_species_trend(ik_data, c(taxa, overlay_taxa()), by = input$by %||% "season",
                       reserve = .ik_nz(selection()$reserve))
    }) |> bindCache(spec$key, input$by, input$overlay, .ik_nz(selection()$reserve), ik_active_datasets())

    output$trend <- renderPlot({
      d <- trend()
      validate(need(!is.null(d) && nrow(d), "No detections or captures to chart for this species."))
      flab <- c(camera_rai = sprintf("Camera activity (RAI / %s ch)", format(nh, big.mark = ",")),
                trap_rate   = sprintf("Catch rate (/ %s trap-nights)", format(nt, big.mark = ",")))
      d$facet <- factor(flab[d$metric_type], levels = unname(flab[c("camera_rai", "trap_rate")]))
      d <- d[!is.na(d$facet), , drop = FALSE]
      ggplot2::ggplot(d, ggplot2::aes(.data$period, .data$value, colour = .data$taxon, group = .data$taxon)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = pmax(.data$value - .data$se, 0), ymax = .data$value + .data$se,
                             fill = .data$taxon), alpha = 0.15, colour = NA, na.rm = TRUE) +
        ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) + ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
        ggplot2::facet_wrap(ggplot2::vars(.data$facet), ncol = 1, scales = "free_y") +
        ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.08))) +
        ggplot2::labs(x = NULL, y = NULL, colour = NULL, fill = NULL) +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = if (is.null(overlay_taxa())) "none" else "bottom",
          strip.text = ggplot2::element_text(face = "bold", colour = ik_plot_ink(is_dark())),
          panel.spacing = ggplot2::unit(1, "lines"))
    }, bg = "transparent")

    # ---- Records (camera + trap combined) ----
    records <- reactive({
      req(active())
      one <- function(src) {
        o <- tryCatch(ik_metric_obs(ik_data, selection(), taxa, spec$label, source_type = src),
                      error = function(e) NULL)
        if (is.null(o) || !nrow(o)) return(NULL)
        o$device <- if (src == "camera") "Camera" else "Trap"; o
      }
      rows <- dplyr::bind_rows(if (spec$camera) one("camera"), if (spec$trapped) one("trap"))
      if (is.null(rows) || !nrow(rows)) return(NULL)
      rows[order(rows$when, decreasing = TRUE), , drop = FALSE]
    }) |> bindCache(spec$key, selection()$period, selection()$season, selection()$reserve,
                    selection()$line, selection()$location, ik_active_datasets())

    output$records <- DT::renderDT({
      o <- records(); validate(need(!is.null(o) && nrow(o), "No records of this species in the selection."))
      p <- prefer()
      has_t <- format(o$when, "%H:%M:%S") != "00:00:00"
      when_lab <- ifelse(has_t, format(o$when, "%d %b %Y · %H:%M"), format(o$when, "%d %b %Y"))
      df <- data.frame(When = when_lab, Device = o$device,
                       Species = ik_species_label(o$scientificName, ik_data, p), Count = o$count,
                       Reserve = o$reserve, Line = ifelse(is.na(o$line), "—", o$line), Location = o$locationName,
                       .when_sort = as.numeric(o$when), check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "none", class = "stripe hover row-border",
        options = list(pageLength = 15, scrollX = TRUE, dom = "ftip",
                       order = list(list(0, "desc")), columnDefs = .ik_dt_when_defs(df, "When")))
    })

    # ---- Map (camera RAI + trap catches as layers; drawn in-render so it's lazy + no proxy race) ----
    output$map <- leaflet::renderLeaflet({
      req(active())
      cam <- if (spec$camera) tryCatch(ik_location_metric(ik_data, selection(), taxa, "camera", norm = per_cam), error = function(e) NULL) else NULL
      trp <- if (spec$trapped) tryCatch(ik_location_metric(ik_data, selection(), taxa, "trap"), error = function(e) NULL) else NULL
      fin <- function(d) if (is.null(d)) NULL else d[is.finite(d$latitude) & is.finite(d$longitude), , drop = FALSE]
      cam <- fin(cam); trp <- fin(trp)
      camp <- if (is.null(cam)) NULL else cam[cam$metric > 0, , drop = FALSE]
      trpp <- if (is.null(trp)) NULL else trp[trp$captures > 0, , drop = FALSE]
      canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
      m <- leaflet::addProviderTiles(m, canvas, group = "Map")
      m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
             overlayGroups = c("Camera activity", "Catches"),
             options = leaflet::layersControlOptions(collapsed = FALSE))
      rad <- function(v, lo, hi) { v <- pmax(as.numeric(v), 0); cap <- stats::quantile(v[v > 0], 0.95, na.rm = TRUE)
        if (!is.finite(cap) || cap <= 0) return(rep((lo + hi) / 2, length(v)))
        scales::rescale(sqrt(pmin(v, cap)), to = c(lo, hi), from = c(0, sqrt(cap))) }
      if (!is.null(camp) && nrow(camp))
        m <- leaflet::addCircleMarkers(m, data = camp, lng = ~longitude, lat = ~latitude, group = "Camera activity",
               radius = rad(camp$metric, 6, 22), fillColor = "#1f78b4", fillOpacity = 0.8, stroke = TRUE,
               color = "#ffffff", weight = 1, label = sprintf("%s — RAI %.2f / %s ch · %d detections",
                 camp$name, camp$metric, format(per_cam, big.mark = ","), as.integer(camp$individuals)))
      if (!is.null(trpp) && nrow(trpp))
        m <- leaflet::addCircleMarkers(m, data = trpp, lng = ~longitude, lat = ~latitude, group = "Catches",
               radius = rad(trpp$captures, 6, 22), fillColor = "#6a3d9a", fillOpacity = 0.85, stroke = TRUE,
               color = "#ffffff", weight = 1, label = sprintf("%s — %d caught", trpp$name, as.integer(trpp$captures)))
      ext <- rbind(if (!is.null(camp)) camp[, c("longitude", "latitude")],
                   if (!is.null(trpp)) trpp[, c("longitude", "latitude")])
      if (!is.null(ext) && nrow(ext))
        m <- leaflet::fitBounds(m, min(ext$longitude), min(ext$latitude), max(ext$longitude), max(ext$latitude),
                                options = list(padding = c(30, 30)))
      m
    })
  })
}
