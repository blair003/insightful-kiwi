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

#' Help body for the Activity Pattern (time-of-day) card. @keywords internal
behaviour_tod_help_body <- function() {
  tagList(
    tags$p("Detections by hour of the local day — the species' daily rhythm on camera. Hours come ",
           "straight off each detection's time, with no effort adjustment: cameras watch around the ",
           "clock, so every hour gets the same exposure. (For the dawn/day/dusk/night split, which ",
           tags$em("does"), " correct for unequal period lengths, see the ", tags$b("Diel Activity"), " card.)"),
    tags$h6("The shading"),
    tags$p("Night and twilight are shaded behind the bars from the selected period's sun times (mean ",
           "civil dawn, sunrise, sunset and civil dusk for the chosen reserve). Because those shift ",
           "through the year, the shading has two tones:"),
    tags$ul(
      tags$li(tags$b("Solid"), " — night on ", tags$em("every"), " day in the selection (always dark)."),
      tags$li(tags$b("Faded"), " — the dawn/dusk window that ", tags$em("shifts"), " across the period ",
              "(night on some days, light on others). One season → a tight band; a full year → a wide edge.")))
}

#' Help body for the diel-activity classification card (periods · classes · rules). @keywords internal
diel_class_help_body <- function() {
  r  <- IK_DIEL_CLASS_RULES
  pc <- function(p, txt) tags$tr(tags$td(tags$b(p)), tags$td(txt))
  cl <- function(c, txt) tags$tr(tags$td(tags$b(c)), tags$td(txt))
  tabsetPanel(type = "tabs",
    tabPanel("What it shows", icon = icon("circle-question"),
      tags$p(tags$br(), "One headline ", tags$b("diel class"), " summarising when this species is active ",
             "on camera, with the four diel periods' shares beneath it."),
      tags$p("Shares come from ", tags$b("effort-normalised rates"), " — detections per ",
             tags$em("available hour"), " in each period, not raw counts. This matters most for ",
             tags$b("Matutinal"), " and ", tags$b("Vespertine"), ": civil twilight is only a thin sliver ",
             "of the day (often under an hour), so a species could be intensely active at dawn or dusk yet ",
             "log few raw detections simply because the window is so short. Dividing by each period's ",
             "available hours puts all four on an equal footing."),
      tags$h6("The four diel periods"),
      tags$table(class = "table table-sm ik-help-table", tags$tbody(
        pc("Matutinal",  "Civil dawn to sunrise (first light)."),
        pc("Diurnal",    "Sunrise to sunset (daylight)."),
        pc("Vespertine", "Sunset to civil dusk (last light)."),
        pc("Nocturnal",  "Civil dusk to civil dawn (darkness).")))),
    tabPanel("Overall classes", icon = icon("tags"),
      tags$table(class = "table table-sm ik-help-table", tags$tbody(
        cl("Diurnal",     "Day-active."),
        cl("Nocturnal",   "Night-active."),
        cl("Crepuscular", "Dawn/dusk-active."),
        cl("Cathemeral",  "Active intermittently across day and night."),
        cl("Arrhythmic",  "Enough observations, but no clear diel rhythm.")))),
    tabPanel("How it's decided", icon = icon("calculator"),
      tags$ul(tags$br(),
        tags$li("The card estimates ", tags$b("deployed camera-hours in each diel period"), " from the ",
                "deployments in scope and each reserve's sunrise, sunset and civil dawn/dusk, then ",
                "compares detections-per-deployed-hour across the four periods."),
        tags$li(tags$b(sprintf("Fewer than %d detections", r$min_obs)), " — Insufficient data."),
        tags$li(sprintf("%d to %d detections", r$min_obs, r$low_obs - 1L),
                " — a class is shown, but flagged low confidence."),
        tags$li(tags$b("Diurnal / Nocturnal"), sprintf(" — that period's rate share is at least %d%%.",
                round(100 * r$dominant))),
        tags$li(tags$b("Crepuscular"), sprintf(" — dawn plus dusk share is at least %d%%, with both ",
                round(100 * r$crepuscular)), "dawn and dusk represented."),
        tags$li(tags$b("Cathemeral"), sprintf(" — both day and night shares are at least %d%%.",
                round(100 * r$cathemeral))),
        tags$li(tags$b("Arrhythmic"), " — the sample is sufficient, but none of the above is dominant."))))
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
        .ik_titlebar(
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
            tags$p(class = "ik-species-hint", "This species' hotspots — top camera lines by activity and ",
                   "reserves by catches. ", tags$b("Click a bar"), " for the records behind it."),
            if (spec$camera) plotOutput(ns("where_cam"), height = "320px", click = ns("where_cam_click")),
            if (spec$trapped) plotOutput(ns("where_trap"), height = "320px", click = ns("where_trap_click"))),
          if (spec$camera) tabPanel("Behaviour", icon = icon("clock"),
            tags$p(class = "ik-species-hint",
                   "When this species is active on camera — the daily clock, and an overall diel class."),
            div(class = "ik-behav",
              div(class = "ik-card",
                div(class = "ik-card-head", div(class = "ik-card-title", icon("clock"),
                    tags$span("Activity Pattern")),
                    .ik_info(ns("tod_help"), "Activity Pattern — how to read this", behaviour_tod_help_body())),
                div(class = "ik-card-body", plotOutput(ns("tod"), height = "420px"),
                    div(class = "ik-tod-key",
                        tags$span(class = "ik-tod-key-item", tags$span(class = "sw sw-night"), "Night"),
                        tags$span(class = "ik-tod-key-item", tags$span(class = "sw sw-twi"), "Dawn / dusk")))),
              div(class = "ik-card",
                div(class = "ik-card-head", div(class = "ik-card-title", icon("clock"),
                    tags$span("Diel Activity")),
                    .ik_info(ns("diel_help"), "Diel activity — classification", diel_class_help_body())),
                div(class = "ik-card-body", uiOutput(ns("diel_card")))))),
          if (spec$trapped) tabPanel("Bait", icon = icon("drumstick-bite"),
            tags$p(class = "ik-species-hint",
                   "Which baits catch this species best — captures per trap-night. Group by the full recipe (the whole bait set) or by individual ingredient. ",
                   tags$b("Click a bar"), " for the captures behind it."),
            div(class = "ik-species-controls",
                radioButtons(ns("bait_group"), NULL, inline = TRUE,
                             choices = c("Full recipe" = "recipe", "Ingredient" = "ingredient"),
                             selected = "recipe")),
            plotOutput(ns("bait"), height = "440px", click = ns("bait_click"))),
          if (.cooc_ok) tabPanel("Co-occurrence", icon = icon("hourglass-half"),
            tags$p(class = "ik-species-hint", .cooc_hint, " ", tags$b("Click a bar"), " for the co-detections."),
            plotOutput(ns("cooc"), height = "340px", click = ns("cooc_click"))),
          tabPanel("Records", icon = icon("list"),
            tags$p(class = "ik-species-hint", "Every detection (camera) and capture (trap) of this species in the selection. ",
                   tags$b("Click a row"), " to open the full record."),
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
    .other_spec <- reactive({
      v <- input$overlay %||% "__none__"; if (identical(v, "__none__")) return(NULL)
      Filter(function(s) s$key == v, ik_species_taxa(ik_data))[[1]]
    })
    overlay_taxa <- reactive({
      o <- .other_spec(); if (is.null(o)) NULL else stats::setNames(list(o$sci), o$label)
    })
    # Which plotted taxa (this page + any overlay) actually have camera / trap records — used to drop a
    # metric panel that would otherwise be a flat zero line for a species never seen on that device
    # (e.g. an untrapped bird has no real catch rate). An untrapped page then gets the full plot height.
    camera_labels  <- reactive({ o <- .other_spec()
      c(if (isTRUE(spec$camera))  spec$label, if (!is.null(o) && isTRUE(o$camera))  o$label) })
    trapped_labels <- reactive({ o <- .other_spec()
      c(if (isTRUE(spec$trapped)) spec$label, if (!is.null(o) && isTRUE(o$trapped)) o$label) })

    # ---- shared "records behind the chart" drill: a 2-tab modal (Records summary → Record Details).
    #      Each plot stores its rows (a df with an observationID column) in a reactiveVal and calls the
    #      returned opener; one DT + row-click + viewer + back serves diel / where / bait / co-occurrence.
    #      `build_df(d)` returns the display frame (When first, plus a `.when_sort` numeric). ----
    .make_drill <- function(prefix, recs_rv, build_df) {
      obs_rv <- reactiveVal(NULL); tabs <- paste0(prefix, "_tabs")
      output[[paste0(prefix, "_recs")]] <- DT::renderDT({
        d <- recs_rv(); validate(need(!is.null(d) && nrow(d), "No records for this selection."))
        df <- build_df(d)
        DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
          options = list(pageLength = 15, scrollX = TRUE, dom = "ftip", order = list(list(0, "desc")),
                         columnDefs = .ik_dt_when_defs(df, "When")))
      })
      observeEvent(input[[paste0(prefix, "_recs_rows_selected")]], {
        i <- input[[paste0(prefix, "_recs_rows_selected")]]; d <- recs_rv()
        if (length(i) && !is.null(d) && i <= nrow(d)) {
          obs_rv(d$observationID[i]); updateTabsetPanel(session, tabs, selected = "Record Details")
        }
        DT::selectRows(DT::dataTableProxy(paste0(prefix, "_recs")), NULL)   # clear → same row re-clickable
      })
      output[[paste0(prefix, "_obs_ui")]] <- renderUI({
        if (is.null(obs_rv())) return(tags$p(class = "ik-species-hint", "Click a record above to open it here."))
        ob <- ik_observation(ik_data, obs_rv()); if (is.null(ob)) return(tags$p("Record not found."))
        pr <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
        tagList(.ik_tab_back(session$ns(paste0(prefix, "_back")), tabs, "Records summary", "Back to records"),
                .ovw_title(ik_data, ob, pr),
                .ovw_tabs(ik_data, ob, pr, tabset_id = session$ns(paste0(prefix, "_sub"))))
      })
      observeEvent(input[[paste0(prefix, "_back")]],
        updateTabsetPanel(session, input[[paste0(prefix, "_back")]]$tabset, selected = input[[paste0(prefix, "_back")]]$to))
      function(title, subtitle = NULL, intro = NULL) {
        obs_rv(NULL)
        showModal(modalDialog(title = .ik_modal_title(title, subtitle), size = "l", easyClose = TRUE,
          footer = modalButton("Close"),
          tabsetPanel(id = session$ns(tabs),
            tabPanel("Records summary", icon = icon("table-list"),
              if (!is.null(intro)) tags$p(class = "ik-species-hint", intro),
              DT::DTOutput(session$ns(paste0(prefix, "_recs")))),
            tabPanel("Record Details", icon = icon("magnifying-glass"),
              uiOutput(session$ns(paste0(prefix, "_obs_ui")))))))
      }
    }

    # ---- Trend ----
    trend <- reactive({
      req(active())
      ik_species_trend(ik_data, c(taxa, overlay_taxa()), by = input$by %||% "season",
                       reserve = .ik_nz(selection()$reserve))
    }) |> bindCache(spec$key, input$by, input$overlay, .ik_nz(selection()$reserve), ik_active_datasets())

    output$trend <- renderPlot({
      d <- trend()
      validate(need(!is.null(d) && nrow(d), "No detections or captures to chart for this species."))
      # drop a panel that would be a flat zero line for a species never seen on that device, so an
      # untrapped (or camera-less) species shows a single, full-height panel.
      d <- d[!(d$metric_type == "trap_rate"  & !d$taxon %in% trapped_labels()), , drop = FALSE]
      d <- d[!(d$metric_type == "camera_rai" & !d$taxon %in% camera_labels()),  , drop = FALSE]
      validate(need(nrow(d), "No detections or captures to chart for this species."))
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
      when_lab <- .ik_when_label(o$when)
      df <- data.frame(When = when_lab, Device = o$device,
                       Species = ik_species_label(o$scientificName, ik_data, p), Count = o$count,
                       Reserve = o$reserve, Line = ifelse(is.na(o$line), "—", o$line), Location = o$locationName,
                       .when_sort = as.numeric(o$when), check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 15, scrollX = TRUE, dom = "ftip",
                       order = list(list(0, "desc")), columnDefs = .ik_dt_when_defs(df, "When")))
    })

    observeEvent(input$records_rows_selected, {       # row → the full record (the shared viewer)
      i <- input$records_rows_selected; o <- records()
      if (length(i) && !is.null(o) && i <= nrow(o)) {
        ob <- ik_observation(ik_data, o$observationID[i])
        if (!is.null(ob)) { pr <- prefer()
          showModal(modalDialog(title = .ik_modal_title(spec$label, "Record"), size = "l",
            easyClose = TRUE, footer = modalButton("Close"),
            .ovw_title(ik_data, ob, pr),
            .ovw_tabs(ik_data, ob, pr, tabset_id = session$ns("rec_view_sub")))) }
      }
      DT::selectRows(DT::dataTableProxy("records"), NULL)
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
      rad <- function(v, lo, hi) ik_marker_radius(v, lo, hi, cap_pctl = 0.95)   # shared impl in spatial.R
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
    .hbar_prep <- function(d, xcol) {                          # filter >0, top 15, factor labels (best on top)
      d <- d[is.finite(d[[xcol]]) & d[[xcol]] > 0, , drop = FALSE]
      if (!nrow(d)) return(d)
      d <- utils::head(d[order(-d[[xcol]]), , drop = FALSE], 15)
      d$.lab <- factor(d$.lab, levels = rev(d$.lab)); d
    }
    .hbar_plot <- function(d, xcol, lab, xtitle, fill) {
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

    where_cam_plotted <- reactiveVal(NULL); where_trap_plotted <- reactiveVal(NULL)
    output$where_cam <- renderPlot({
      d <- where()$cam; validate(need(!is.null(d) && nrow(d), "No camera activity for this species in the selection."))
      d$.lab <- ifelse(is.na(d$line), "(unlined)", sprintf("%s · %s", d$reserve, d$line))
      d <- .hbar_prep(d, "metric"); validate(need(nrow(d), "No camera activity for this species in the selection."))
      where_cam_plotted(d)
      .hbar_plot(d, "metric", "Camera activity by line (RAI)", sprintf("RAI / %s ch", format(per_cam, big.mark = ",")), "#1f78b4")
    }, bg = "transparent")

    output$where_trap <- renderPlot({
      d <- where()$trap; validate(need(!is.null(d) && nrow(d), "No catches for this species in the selection."))
      d <- d[d$reserve != "Combined", , drop = FALSE]; d$.lab <- d$reserve
      d <- .hbar_prep(d, "metric"); validate(need(nrow(d), "No catches for this species in the selection."))
      where_trap_plotted(d)
      .hbar_plot(d, "metric", "Catch rate by reserve", sprintf("catches / %s TN", format(nt, big.mark = ",")), "#6a3d9a")
    }, bg = "transparent")

    # Where drills: click a bar → the camera detections on that line / the catches in that reserve.
    where_cam_recs <- reactiveVal(NULL); where_trap_recs <- reactiveVal(NULL)
    .where_recs_df <- function(d)
      data.frame(When = .ik_when_label(d$when), Species = ik_species_label(d$scientificName, ik_data, prefer()),
        Count = d$count, Reserve = d$reserve, Line = ifelse(is.na(d$line), "—", d$line), Location = d$locationName,
        .when_sort = as.numeric(d$when), check.names = FALSE, stringsAsFactors = FALSE)
    where_cam_open  <- .make_drill("where_cam",  where_cam_recs,  .where_recs_df)
    where_trap_open <- .make_drill("where_trap", where_trap_recs, .where_recs_df)
    .where_bar <- function(plotted, click) {                  # clicked bar's source row, or NULL
      d <- plotted(); if (is.null(d) || is.null(click) || is.null(click$y)) return(NULL)
      k <- round(click$y); lv <- levels(d$.lab); if (k < 1 || k > length(lv)) return(NULL)
      d[as.character(d$.lab) == lv[k], , drop = FALSE][1, , drop = FALSE]
    }
    observeEvent(input$where_cam_click, {
      row <- .where_bar(where_cam_plotted, input$where_cam_click); if (is.null(row)) return()
      o <- tryCatch(ik_metric_obs(ik_data, selection(), taxa, spec$label, source_type = "camera"), error = function(e) NULL)
      if (is.null(o) || !nrow(o)) return()
      keep <- !is.na(o$reserve) & o$reserve == row$reserve &
        (if (is.na(row$line)) is.na(o$line) else (!is.na(o$line) & o$line == row$line))
      r <- o[keep, , drop = FALSE]; if (!nrow(r)) return()
      where_cam_recs(r[order(r$when, decreasing = TRUE), , drop = FALSE])
      where_cam_open(sprintf("%s — %s", spec$label,
          if (is.na(row$line)) sprintf("%s · (unlined)", row$reserve) else as.character(row$.lab)),
        "Camera detections on this line",
        tagList("Every camera detection on this line in the selection — the records behind its RAI. ",
                tags$b("Click a row"), " for the full record."))
    })
    observeEvent(input$where_trap_click, {
      row <- .where_bar(where_trap_plotted, input$where_trap_click); if (is.null(row)) return()
      o <- tryCatch(ik_metric_obs(ik_data, selection(), taxa, spec$label, source_type = "trap"), error = function(e) NULL)
      if (is.null(o) || !nrow(o)) return()
      r <- o[!is.na(o$reserve) & o$reserve == row$reserve, , drop = FALSE]; if (!nrow(r)) return()
      where_trap_recs(r[order(r$when, decreasing = TRUE), , drop = FALSE])
      where_trap_open(sprintf("%s — %s", spec$label, row$reserve), "Catches in this reserve",
        tagList("Every capture in this reserve in the selection — the records behind its catch rate. ",
                tags$b("Click a row"), " for the full record."))
    })

    # ---- Behaviour (camera): time-of-day histogram + effort-normalised diel ----
    cam_events <- reactive({ req(active(), spec$camera); ik_species_camera_events(ik_data, taxa, selection()) }) |>
      bindCache(spec$key, selection()$period, selection()$season, selection()$reserve, selection()$line, selection()$location, ik_active_datasets())
    diel <- reactive({ req(active(), spec$camera); ik_diel_class(ik_data, taxa, selection()) }) |>
      bindCache(spec$key, selection()$period, selection()$season, selection()$reserve, ik_active_datasets())

    # Time-of-day: a linear 24h histogram of detections per hour, with night + twilight bands shaded
    # behind the bars from the selection's mean sun times (so a winter view shows a longer night).
    output$tod <- renderPlot({
      ev <- cam_events(); validate(need(!is.null(ev) && nrow(ev), "No camera detections to chart."))
      h <- as.data.frame(table(factor(ev$hour, levels = 0:23))); names(h) <- c("hour", "n")
      h$hour <- as.integer(as.character(h$hour))
      dk <- is_dark()
      sh <- ik_sun_hours(ik_data, .ik_nz(selection()$reserve), .ik_nz(selection()$season))
      g  <- ggplot2::ggplot()
      if (!is.null(sh) && all(is.finite(c(sh$civil_dawn, sh$civil_dusk)))) {
        dawn <- sh$civil_dawn; dusk <- sh$civil_dusk              # each c(min, max) hour over the period
        night <- if (dk) "#000000" else "#44505e"
        a_solid <- if (dk) 0.32 else 0.16; a_fade <- if (dk) 0.15 else 0.07
        # solid = night on EVERY selected day; faded = night on SOME days (the dawn/dusk window shifts)
        bands <- data.frame(
          xmin  = c(-0.5,     dawn[1], dusk[2], dusk[1]),
          xmax  = c(dawn[1],  dawn[2], 23.5,    dusk[2]),
          alpha = c(a_solid,  a_fade,  a_solid, a_fade), stringsAsFactors = FALSE)
        bands <- bands[bands$xmax > bands$xmin, , drop = FALSE]
        g <- g + ggplot2::geom_rect(data = bands,
               ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf),
               fill = night, alpha = bands$alpha)
      }
      g + ggplot2::geom_col(data = h, ggplot2::aes(.data$hour, .data$n), fill = "#4a7fb0",
                            width = 0.9, na.rm = TRUE) +
        ggplot2::scale_x_continuous(breaks = seq(0, 21, 3), labels = function(x) sprintf("%02d:00", x %% 24),
                                    limits = c(-0.5, 23.5), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
        ggplot2::labs(x = NULL, y = "Detections", title = "Detections by hour of day") +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(face = "bold", colour = ik_plot_ink(is_dark())))
    }, bg = "transparent")

    # Diel activity: an overall CLASS headline + the four periods' effort-normalised shares (a card,
    # not a plot — matches the rest of the app's summary cards). Classification in ik_diel_class().
    output$diel_card <- renderUI({
      req(active(), spec$camera)
      dc <- diel()
      validate(need(!is.null(dc) && dc$n > 0, "No camera detections to classify."))
      sh   <- dc$shares                                                   # named integer % in period order
      pick <- function(p) sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'});",
                                  session$ns("diel_pick"), .ik_jsq(p))     # click → open that period's records
      seg <- function(p) if (isTRUE(sh[[p]] > 0))
        tags$span(class = "ik-diel-seg", onclick = pick(p), title = sprintf("%s — click for records", p),
                  style = sprintf("width:%d%%;background:%s", sh[[p]], IK_DIEL_COLORS[[p]])) else NULL
      leg <- function(p) tags$div(class = "ik-diel-leg ik-diel-clickable", onclick = pick(p),
        title = sprintf("%s — click for records", p),
        tags$span(class = "dot", style = sprintf("background:%s", IK_DIEL_COLORS[[p]])),
        tags$span(class = "ik-diel-leg-lab", p), tags$b(sprintf("%d%%", sh[[p]])))
      div(class = paste0("ik-diel", if (identical(dc$confidence, "low")) " is-low" else ""),
        div(class = "ik-diel-class", dc$class),
        div(class = "ik-diel-desc", dc$desc),
        div(class = "ik-diel-n", sprintf("%s observation%s",
            format(dc$n, big.mark = ","), if (dc$n == 1) "" else "s")),
        if (identical(dc$confidence, "low"))
          div(class = "ik-diel-lowconf",
              sprintf("Shown with low confidence (under %d observations)", dc$low_obs)),
        div(class = "ik-diel-bar", lapply(IK_DIEL_PERIODS, seg)),
        div(class = "ik-diel-legend", lapply(IK_DIEL_PERIODS, leg)),
        div(class = "ik-diel-foot", sprintf(
            "Based on %s observations and effort-normalised diel rates.", format(dc$n, big.mark = ","))))
    })

    # ---- Diel drill: click a period (bar segment or legend) → its records with the sun boundaries
    #      used to classify each one, so a class can be checked by eye → click a row for the record ----
    diel_pick <- reactiveVal(NULL)            # the clicked diel period
    DIEL_DEF  <- c(Matutinal = "civil dawn → sunrise", Diurnal = "sunrise → sunset",
                   Vespertine = "sunset → civil dusk", Nocturnal = "civil dusk → civil dawn")
    diel_recs_df <- reactive({
      p <- diel_pick(); req(!is.null(p))
      d <- ik_species_diel_records(ik_data, taxa, selection())
      if (is.null(d)) return(NULL)
      d <- d[!is.na(d$diel) & d$diel == p, , drop = FALSE]
      if (!nrow(d)) return(NULL)
      d[order(d$when, decreasing = TRUE), , drop = FALSE]
    })
    diel_open <- .make_drill("diel", diel_recs_df, function(d) {
      hm <- function(t) ifelse(is.na(t), "—", format(t, "%H:%M"))
      data.frame(When = .ik_when_label(d$when), Class = d$diel,
        `Civil dawn` = hm(d$civil_dawn), Sunrise = hm(d$sunrise),
        Sunset = hm(d$sunset), `Civil dusk` = hm(d$civil_dusk),
        Reserve = d$reserve, Location = d$location,
        .when_sort = as.numeric(d$when), check.names = FALSE, stringsAsFactors = FALSE)
    })
    observeEvent(input$diel_pick, {
      p <- input$diel_pick; if (is.null(p) || !nzchar(p)) return()
      diel_pick(p)
      diel_open(sprintf("%s — %s records", spec$label, p),
        sprintf("Diel period: %s", if (p %in% names(DIEL_DEF)) DIEL_DEF[[p]] else p),
        tagList("Every detection classified ", tags$b(tolower(p)), ". Sun times are for each detection's ",
                "own date & reserve — check its time against them to verify the class. ",
                tags$b("Click a row"), " for the full record."))
    })

    # ---- Bait (trapped species): which baits catch it best ----
    # honours the sidebar period + reserve (bait recipes are network-wide, but a reserve scopes them)
    bait_data <- reactive({ req(active(), spec$trapped)
      tryCatch(ik_bait_effectiveness(ik_data, seasons = .ik_nz(selection()$season), species = spec$sci,
                 norm = nt, group = input$bait_group %||% "recipe", min_captures = 2,
                 reserve = .ik_nz(selection()$reserve)), error = function(e) NULL)
    }) |> bindCache(spec$key, input$bait_group, selection()$season, selection()$reserve, ik_active_datasets())

    bait_plotted <- reactiveVal(NULL)
    output$bait <- renderPlot({
      grp <- input$bait_group %||% "recipe"
      d <- bait_data()
      validate(need(!is.null(d) && nrow(d), "Not enough baited captures of this species to compare baits."))
      d <- utils::head(d[order(-d$rate), , drop = FALSE], 15); d$bait <- factor(d$bait, levels = rev(d$bait))
      bait_plotted(d)
      lab  <- sprintf("  %.2f  (%s caught · %s trap-days)", d$rate,
                      format(d$captures, big.mark = ","), format(round(d$trap_days), big.mark = ","))
      unit <- if (identical(grp, "ingredient")) "ingredients" else "recipes"
      ggplot2::ggplot(d, ggplot2::aes(.data$rate, .data$bait)) +
        ggplot2::geom_col(fill = "#b15928", width = 0.72) +
        ggplot2::geom_text(ggplot2::aes(label = lab), hjust = 0, size = 3.2, na.rm = TRUE,
                           colour = ik_plot_ink(is_dark())) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.5))) +
        ggplot2::labs(x = sprintf("Captures / %s trap-nights", format(nt, big.mark = ",")), y = NULL,
                      title = sprintf("Catch rate by bait — top %d %s", nrow(d), unit)) +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(face = "bold", colour = ik_plot_ink(is_dark())))
    }, bg = "transparent")

    # Bait drill: click a bar → the individual captures credited to that bait.
    bait_recs <- reactiveVal(NULL)
    bait_open <- .make_drill("bait", bait_recs, function(d)
      data.frame(When = .ik_when_label(d$check_date), Trap = d$trap, Species = d$species,
        .when_sort = as.numeric(d$check_date), check.names = FALSE, stringsAsFactors = FALSE))
    observeEvent(input$bait_click, {
      d <- bait_plotted(); cl <- input$bait_click
      if (is.null(d) || is.null(cl) || is.null(cl$y)) return()
      k <- round(cl$y); lv <- levels(d$bait); if (k < 1 || k > length(lv)) return()
      caps <- tryCatch(ik_bait_captures(ik_data, lv[k], seasons = .ik_nz(selection()$season), species = spec$sci,
        group = input$bait_group %||% "recipe", reserve = .ik_nz(selection()$reserve)), error = function(e) NULL)
      if (is.null(caps) || !nrow(caps)) return()
      bait_recs(caps[order(caps$check_date, decreasing = TRUE), , drop = FALSE])
      bait_open(sprintf("%s — %s", spec$label, lv[k]), "Captures on this bait",
        tagList("Every capture credited to this bait in the selection. ", tags$b("Click a row"),
                " for the full record."))
    })

    # ---- Co-occurrence (predator/protected): time to the nearest opposing-role detection ----
    opp_role <- if (identical(spec$role, "predator")) "protected" else if (identical(spec$role, "protected")) "predator" else NA_character_
    opp_sci  <- if (!is.na(opp_role)) unique(ik_species_groups(ik_data)$scientificName[ik_species_groups(ik_data)$role == opp_role]) else character(0)
    cooc_gaps <- reactive({ req(active(), spec$camera, !is.na(opp_role), length(opp_sci) > 0)
      pp <- if (identical(spec$role, "predator")) list(pred = spec$sci, prot = opp_sci) else list(pred = opp_sci, prot = spec$sci)
      tryCatch(ik_predator_protected_gaps(ik_data, pp$pred, pp$prot, seasons = .ik_nz(selection()$season),
                                          reserve = .ik_nz(selection()$reserve)), error = function(e) NULL)
    }) |> bindCache(spec$key, selection()$season, selection()$reserve, ik_active_datasets())

    output$cooc <- renderPlot({
      g <- cooc_gaps()
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

    # Co-occurrence drill: click a gap bucket → the co-detections in it (the anchor detection + its
    # nearest opposing-role detection). Each row → the anchor record.
    cooc_recs <- reactiveVal(NULL)
    cooc_open <- .make_drill("cooc", cooc_recs, function(d)
      data.frame(When = .ik_when_label(d$when), Species = ik_species_label(d$scientificName, ik_data, prefer()),
        `Gap (h)` = round(d$gap_h, 1), `Nearest predator` = ik_species_label(d$pred_sci, ik_data, prefer()),
        `Predator at` = .ik_when_label(d$pred_when),
        .when_sort = as.numeric(d$when), check.names = FALSE, stringsAsFactors = FALSE))
    observeEvent(input$cooc_click, {
      g <- cooc_gaps(); cl <- input$cooc_click
      if (is.null(g) || !nrow(g) || is.null(cl) || is.null(cl$x)) return()
      k <- round(cl$x); if (k < 1 || k > length(GAP_LABELS)) return()
      lab    <- GAP_LABELS[k]
      bucket <- cut(g$gap_h, breaks = GAP_BREAKS, labels = GAP_LABELS, right = FALSE, include.lowest = TRUE)
      r <- g[!is.na(bucket) & as.character(bucket) == lab, , drop = FALSE]; if (!nrow(r)) return()
      cooc_recs(r[order(r$when, decreasing = TRUE), , drop = FALSE])
      cooc_open(sprintf("%s — co-detections %s away", spec$label, lab),
        sprintf("Each detection and its nearest %s at the same camera", opp_role),
        tagList("The pairs in this time bucket. ", tags$b("Click a row"), " for the full record."))
    })
  })
}
