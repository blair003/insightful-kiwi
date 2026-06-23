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
  # Co-occurrence applies to a predator/protected species with camera data + an opposing role present.
  opp_role  <- if (identical(spec$role, "predator")) "protected" else if (identical(spec$role, "protected")) "predator" else NA_character_
  opp_sci   <- if (!is.na(opp_role)) unique(ik_species_groups(ik_data)$scientificName[ik_species_groups(ik_data)$role == opp_role]) else character(0)
  .cooc_ok  <- isTRUE(spec$camera) && !is.na(opp_role) && length(opp_sci) > 0
  .cooc_hint <- if (.cooc_ok) sprintf(
    "Time on camera between this %s and the nearest %s at the same camera — shorter gaps mean they share ground close in time.",
    spec$role, opp_role) else ""
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
          tabPanel("Where", icon = icon("ranking-star"),
            tags$p(class = "ik-species-hint", "This species' hotspots — top camera lines by activity and reserves by catches."),
            if (spec$camera) plotOutput(ns("where_cam"), height = "320px"),
            if (spec$trapped) plotOutput(ns("where_trap"), height = "320px")),
          if (spec$camera) tabPanel("Behaviour", icon = icon("clock"),
            tags$p(class = "ik-species-hint",
                   "When this species is active on camera. Diel activity is effort-normalised (detections per available hour)."),
            plotOutput(ns("tod"), height = "300px"),
            plotOutput(ns("diel"), height = "300px")),
          if (spec$trapped) tabPanel("Bait", icon = icon("drumstick-bite"),
            tags$p(class = "ik-species-hint", "Which baits catch this species best (captures per trap-night, top recipes)."),
            plotOutput(ns("bait"), height = "420px")),
          if (.cooc_ok) tabPanel("Co-occurrence", icon = icon("hourglass-half"),
            tags$p(class = "ik-species-hint", .cooc_hint),
            plotOutput(ns("cooc"), height = "340px")),
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

    # ---- Where (hotspots): top camera lines by RAI · reserves by catch rate ----
    .hbar <- function(d, xcol, lab, xtitle, fill) {            # horizontal ranked bar, top 15
      d <- d[is.finite(d[[xcol]]) & d[[xcol]] > 0, , drop = FALSE]
      validate(need(nrow(d), paste0("No ", lab, " for this species in the selection.")))
      d <- utils::head(d[order(-d[[xcol]]), , drop = FALSE], 15)
      d$.lab <- factor(d$.lab, levels = rev(d$.lab))
      ggplot2::ggplot(d, ggplot2::aes(.data[[xcol]], .data$.lab)) +
        ggplot2::geom_col(fill = fill, width = 0.72) +
        ggplot2::labs(x = xtitle, y = NULL, title = lab) +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(face = "bold", colour = ik_plot_ink(is_dark())))
    }
    where <- reactive({ req(active())
      list(cam  = if (spec$camera)  tryCatch(ik_rai(ik_data, selection(), taxa, level = "reserve")$lines, error = function(e) NULL) else NULL,
           trap = if (spec$trapped) tryCatch(ik_trap_rate(ik_data, selection(), taxa, level = "reserve")$summary, error = function(e) NULL) else NULL)
    }) |> bindCache(spec$key, selection()$period, selection()$season, selection()$reserve, selection()$line, selection()$location, ik_active_datasets())

    output$where_cam <- renderPlot({
      d <- where()$cam; validate(need(!is.null(d) && nrow(d), "No camera activity for this species in the selection."))
      d$.lab <- ifelse(is.na(d$line), "(unlined)", sprintf("%s · %s", d$reserve, d$line))
      .hbar(d, "metric", "Camera activity by line (RAI)", sprintf("RAI / %s ch", format(per_cam, big.mark = ",")), "#1f78b4")
    }, bg = "transparent")

    output$where_trap <- renderPlot({
      d <- where()$trap; validate(need(!is.null(d) && nrow(d), "No catches for this species in the selection."))
      d <- d[d$reserve != "Combined", , drop = FALSE]; d$.lab <- d$reserve
      .hbar(d, "metric", "Catch rate by reserve", sprintf("catches / %s TN", format(nt, big.mark = ",")), "#6a3d9a")
    }, bg = "transparent")

    # ---- Behaviour (camera): time-of-day histogram + effort-normalised diel ----
    cam_events <- reactive({ req(active(), spec$camera); ik_species_camera_events(ik_data, taxa, .ik_nz(selection()$reserve)) }) |>
      bindCache(spec$key, selection()$period, selection()$season, selection()$reserve, selection()$line, selection()$location, ik_active_datasets())

    output$tod <- renderPlot({
      ev <- cam_events(); validate(need(!is.null(ev) && nrow(ev), "No camera detections to chart."))
      h <- as.data.frame(table(factor(ev$hour, levels = 0:23))); names(h) <- c("hour", "n"); h$hour <- as.integer(as.character(h$hour))
      ggplot2::ggplot(h, ggplot2::aes(.data$hour, .data$n)) +
        ggplot2::geom_col(fill = "#1f78b4", width = 0.9) +
        ggplot2::scale_x_continuous(breaks = seq(0, 24, 6), limits = c(-0.5, 23.5)) +
        ggplot2::labs(x = "Hour of day", y = "Detections", title = "Activity by time of day") +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = ik_plot_ink(is_dark())))
    }, bg = "transparent")

    output$diel <- renderPlot({
      d <- ik_species_diel(ik_data, taxa, .ik_nz(selection()$reserve))
      validate(need(!is.null(d) && any(d$detections > 0), "No camera detections to chart."))
      ggplot2::ggplot(d, ggplot2::aes(.data$period, .data$rate)) +
        ggplot2::geom_col(fill = "#6a3d9a", width = 0.72, na.rm = TRUE) +
        ggplot2::geom_text(ggplot2::aes(label = .data$detections), vjust = -0.4, size = 3,
                           colour = ik_plot_ink(is_dark()), na.rm = TRUE) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
        ggplot2::labs(x = NULL, y = "Detections / available hour", title = "Diel activity (effort-normalised)") +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = ik_plot_ink(is_dark())))
    }, bg = "transparent")

    # ---- Bait (trapped species): which baits catch it best ----
    output$bait <- renderPlot({
      req(active(), spec$trapped)
      d <- tryCatch(ik_bait_effectiveness(ik_data, species = spec$sci, norm = nt), error = function(e) NULL)
      validate(need(!is.null(d) && nrow(d), "Not enough baited captures of this species to compare baits."))
      d <- utils::head(d[order(-d$rate), , drop = FALSE], 15); d$bait <- factor(d$bait, levels = rev(d$bait))
      ggplot2::ggplot(d, ggplot2::aes(.data$rate, .data$bait)) +
        ggplot2::geom_col(fill = "#b15928", width = 0.72) +
        ggplot2::labs(x = sprintf("Captures / %s trap-nights", format(nt, big.mark = ",")), y = NULL,
                      title = "Catch rate by bait") +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(face = "bold", colour = ik_plot_ink(is_dark())))
    }, bg = "transparent")

    # ---- Co-occurrence (predator/protected): time to the nearest opposing-role detection ----
    opp_role <- if (identical(spec$role, "predator")) "protected" else if (identical(spec$role, "protected")) "predator" else NA_character_
    opp_sci  <- if (!is.na(opp_role)) unique(ik_species_groups(ik_data)$scientificName[ik_species_groups(ik_data)$role == opp_role]) else character(0)
    output$cooc <- renderPlot({
      req(active(), spec$camera, !is.na(opp_role), length(opp_sci) > 0)
      pp <- if (identical(spec$role, "predator")) list(pred = spec$sci, prot = opp_sci) else list(pred = opp_sci, prot = spec$sci)
      g  <- tryCatch(ik_predator_protected_gaps(ik_data, pp$pred, pp$prot), error = function(e) NULL)
      validate(need(!is.null(g) && nrow(g), "No camera co-detections of this species with the opposing role yet."))
      bucket <- cut(g$gap_h, breaks = GAP_BREAKS, labels = GAP_LABELS, right = FALSE, include.lowest = TRUE)
      h <- as.data.frame(table(factor(bucket, levels = GAP_LABELS))); names(h) <- c("gap", "n")
      ggplot2::ggplot(h, ggplot2::aes(.data$gap, .data$n)) +
        ggplot2::geom_col(fill = "#3f6fb0", width = 0.85) +
        ggplot2::labs(x = sprintf("Time to nearest %s detection (same camera)", opp_role), y = "Co-detections",
                      title = "How close in time, on the same camera") +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = ik_plot_ink(is_dark())))
    }, bg = "transparent")
  })
}
