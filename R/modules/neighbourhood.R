# neighbourhood.R (module) — "Neighbourhood": pick an ANCHOR (a single camera SITE, a monitoring
# LINE, or a whole RESERVE) and a radius; for the cameras and traps in/around it, track season by
# season the protected & predator presence on camera AND the predators caught in nearby traps. Tests
# the local story (line/site) and, at reserve level, "does camera predator activity align with what
# the traps catch?". A deeper analysis under the Outcomes menu. Camera presence is a pooled
# per-camera-hour rate (exploratory); trap catches are a seasonal count (a kill is only known to its
# check window). Data + click-drill from ik_neighbourhood_series() / ik_neighbourhood_records().

#' Neighbourhood nav panel. @param id Module id. @param ik_data The container (anchor choices are
#'   baked into the UI — single-selects in this dropdown nav panel won't keep a server-set default).
neighbourhood_ui <- function(id, ik_data) {
  ns <- NS(id)
  loc  <- .nbhd_locations(ik_data)
  cams <- loc[!is.na(loc$source_type) & loc$source_type == "camera" & is.finite(loc$latitude), , drop = FALSE]
  cams <- cams[order(cams$reserve, cams$name), , drop = FALSE]
  site_ch  <- stats::setNames(cams$location_id, sprintf("%s · %s", cams$reserve, cams$name))
  lines_df <- ik_neighbourhood_lines(ik_data)
  line_val <- paste(lines_df$reserve, lines_df$line, sep = "|")
  line_ch  <- stats::setNames(line_val, sprintf("%s · Line %s", lines_df$reserve, lines_df$line))
  res_ch   <- sort(unique(cams$reserve)); res_ch <- stats::setNames(res_ch, res_ch)
  max_r    <- (ik_data$meta$proximity %||% list())$max_radius_m %||% 2000
  rad_ch   <- c("250 m" = 250, "500 m" = 500, "1 km" = 1000, "1.5 km" = 1500, "2 km" = 2000)
  rad_ch   <- rad_ch[rad_ch <= max_r]
  nav_panel(
    "Neighbourhood", value = "neighbourhood", icon = icon("circle-nodes"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/neighbourhood.css"),
    div(class = "ik-nbhd",
        tags$h3(class = "ik-nbhd-title", "Neighbourhood — protected, predators & nearby trapping"),
        tags$p(class = "ik-nbhd-lead",
          "Pick an anchor — a single camera ", tags$b("site"), ", a monitoring ", tags$b("line"),
          ", or a whole ", tags$b("reserve"), ". For the cameras and traps in/around it, see season ",
          "by season how ", tags$b("protected"), " and ", tags$b("predator"), " activity on camera move, ",
          "alongside the ", tags$b("predators caught"), " in nearby traps. A trap catch is only known ",
          "to its ~monthly check window, so the trap line is a per-season count — not a timing."),
        div(class = "ik-nbhd-controls",
            radioButtons(ns("level"), "Anchor", inline = TRUE,
                         choices = c("Site" = "site", "Line" = "line", "Reserve" = "reserve"), selected = "line"),
            conditionalPanel("input.level == 'site'", ns = ns,
              selectInput(ns("anchor_site"), "Camera", choices = site_ch, selected = unname(site_ch)[1], width = "210px")),
            conditionalPanel("input.level == 'line'", ns = ns,
              selectInput(ns("anchor_line"), "Line", choices = line_ch, selected = line_val[1], width = "210px")),
            conditionalPanel("input.level == 'reserve'", ns = ns,
              selectInput(ns("anchor_reserve"), "Reserve", choices = res_ch, selected = unname(res_ch)[1], width = "170px")),
            conditionalPanel("input.level != 'reserve'", ns = ns,
              selectInput(ns("radius"), "Radius", choices = rad_ch,
                          selected = if (500 %in% rad_ch) 500 else max(rad_ch), width = "110px")),
            selectInput(ns("prot"), "Protected", choices = NULL, multiple = TRUE, width = "210px"),
            selectInput(ns("pred"), "Predator",  choices = NULL, multiple = TRUE, width = "210px"),
            radioButtons(ns("grain"), NULL, inline = TRUE,
                         choices = c("By season" = "season", "By year" = "year"), selected = "season")),
        uiOutput(ns("intro")),
        tags$p(class = "ik-nbhd-hint", "Click a point to see the records behind it."),
        plotOutput(ns("panel"), height = "440px", click = ns("panel_click")))
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
    .pred_def <- paste0("grp:", if ("Mustelids" %in% names(pred_taxa)) "Mustelids" else names(pred_taxa)[1])
    .prot_def <- paste0("grp:", if ("Kiwi" %in% names(prot_taxa)) "Kiwi" else names(prot_taxa)[1])
    rlab  <- function(r) { r <- as.numeric(r); if (r >= 1000) sprintf("%g km", r / 1000) else sprintf("%g m", r) }

    observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      updateSelectInput(session, "prot", choices = ik_species_choices(prot_taxa, ik_data, p, splits), selected = keep(isolate(input$prot), .prot_def))
      updateSelectInput(session, "pred", choices = ik_species_choices(pred_taxa, ik_data, p, splits), selected = keep(isolate(input$pred), .pred_def))
    })

    lvl  <- reactive(input$level %||% "line")
    akey <- reactive(switch(lvl(), site = input$anchor_site, reserve = input$anchor_reserve, input$anchor_line))
    scope_lab <- function(s) if (identical(attr(s, "level"), "reserve")) sprintf("Across %s", attr(s, "label"))
                             else sprintf("Within %s of %s", rlab(input$radius), attr(s, "label"))

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
      tags$p(class = "ik-nbhd-meta", sprintf(
        "%s: %d camera%s and %d trap%s. Camera lines are an effort-adjusted detection rate (net, exploratory); the trap line counts predators caught that season.",
        scope_lab(s), n_cam, if (identical(n_cam, 1L)) "" else "s", n_trap, if (identical(n_trap, 1L)) "" else "s"))
    })

    output$panel <- renderPlot({
      s <- series(); validate(need(!is.null(s) && nrow(s), "Pick an anchor and at least one species."))
      cam_facet <- attr(s, "cam_facet")
      s$period <- factor(s$period, levels = unique(s$period[order(s$order)]))
      s$facet  <- factor(s$facet, levels = c(cam_facet, "Predators caught (nearby traps)"))
      pal <- c(Protected = "#2e7d32", Predator = "#c62828", Caught = "#6a3d9a")
      ggplot2::ggplot(s, ggplot2::aes(.data$period, .data$value, colour = .data$series, group = .data$series)) +
        ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) + ggplot2::geom_point(size = 1.9, na.rm = TRUE) +
        ggplot2::facet_wrap(ggplot2::vars(.data$facet), ncol = 1, scales = "free_y") +
        ggplot2::scale_colour_manual(values = pal, breaks = c("Protected", "Predator", "Caught")) +
        ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom",
          strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.05), colour = ik_plot_ink(is_dark())),
          strip.background = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.1, "lines"))
    }, bg = "transparent")

    # ---- click a point → the records behind it (Records ↔ Record details, with a back link) ----
    nb_obs  <- reactiveVal(NULL)   # the records df shown in the modal
    nb_open <- reactiveVal(NULL)   # observationID open in the detail tab

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
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s — %s", what, period),
                                sprintf("%s — click a record for its full detail.", scope_lab(s))),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("nb_tabs"),
          tabPanel("Records",        icon = icon("list"),        DT::dataTableOutput(session$ns("nb_table"))),
          tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("nb_record"))))))
      hideTab(session = session, inputId = "nb_tabs", target = "Record details")   # appears on row click
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
  })
}
