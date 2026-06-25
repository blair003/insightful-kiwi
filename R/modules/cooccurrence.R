# cooccurrence.R (module) — "Co-occurrence": protected ↔ predator timing on camera. How close in
# time do protected species and predators share the same camera? A gap-bucket distribution (click a
# bar → the detections behind it → the record viewer) plus a seasonal trend of the median gap
# (rising = they're separating in time, e.g. trapping working). A deeper analysis than the
# everyone-can-read "Are we winning?", so it sits on its own page under the Outcomes menu.
# Camera-only; both sides selectable from species_groups roles (rats are everywhere → default
# predator = Mustelids). Data from ik_predator_protected_gaps().

GAP_BREAKS <- c(0, 1, 6, 24, 168, 720, Inf)                                  # hours
GAP_LABELS <- c("< 1 h", "1–6 h", "6–24 h", "1–7 d", "1–4 wk", "> 1 mo")

#' "How to read this" help body for Co-occurrence — a tabbed walkthrough. @keywords internal
cooccurrence_help_body <- function() {
  P <- function(...) tags$p(...)
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "For every ", tags$b("protected"), "species detection on camera, this measures how long ",
        "until the nearest ", tags$b("predator"), " detection — at the ", tags$b("same camera"), ", or, if you ",
        "set a ", tags$b("Within"), " radius, anywhere in the camera's ", tags$b("neighbourhood"),
        " (the camera plus that radius around it) — the ", tags$b("time gap"),
        " between the two species sharing the same ground."),
      P("That neighbourhood reuses the Coverage map's proximity engine: cameras are sparse, so ",
        "“same camera” is strict — a wider radius counts a predator seen a few hundred metres away as nearby."),
      P("Two views: a ", tags$b("distribution"), " of those gaps (how often predator and protected are close ",
        "in time vs far apart) and a ", tags$b("seasonal trend"), " of the median gap. Click a distribution ",
        "bar to see the detections behind it, down to the record."),
      P("Rats are nearly everywhere, so the default predator is ", tags$b("Mustelids"), " — switch to the ",
        "predator that matters to you.")),
    tabPanel(
      "Reading it", icon = icon("chart-column"),
      P(tags$br(), tags$b("Short gaps"), " (left of the distribution) mean the two species share the same ",
        "ground close in time; ", tags$b("long gaps"), " mean they mostly don't overlap."),
      tags$ul(
        tags$li("The ", tags$b("median gap by season"), " ", tags$b("rising"), " over time ⇒ they're ",
                "separating in time — what you'd hope to see where control is thinning predators or shifting ",
                "their behaviour."),
        tags$li(tags$b("Falling / flat"), " ⇒ predator and protected are overlapping as much as ever."),
        tags$li(tags$b("Predator after only"), " restricts to gaps where the predator arrives ", tags$em("after"),
                " the protected animal — the “stalking” direction (following), rather than either order.")),
      P("It's ", tags$b("exploratory"), ": sharing a camera close in time is suggestive of risk, not proof of ",
        "a kill or even a meeting.")),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Pairing"), " — each protected detection is matched to the ", tags$b("nearest in time"),
                " predator detection at the same camera, or — with a ", tags$b("Within"), " radius — at any camera ",
                "within it; the gap is the hours between them, and the modal shows how far apart the cameras were."),
        tags$li(tags$b("Direction"), " — by default the nearest predator detection either side counts; ",
                tags$b("Predator after only"), " keeps just those where the predator came later."),
        tags$li(tags$b("Buckets"), " — gaps are grouped < 1 h · 1–6 h · 6–24 h · 1–7 d · 1–4 wk · > 1 mo."),
        tags$li(tags$b("Seasonal median"), " — within each season, the median gap (robust to a few extreme ",
                "values), plotted over time."),
        tags$li(tags$b("Needs both in range"), " — a protected detection only forms a pair if a predator was ",
                "seen at the same camera (or within the radius); an isolated detection never pairs.")),
      P(tags$em("Camera timestamps must be true clock times for this (trapping has no clock time, so it's ",
                "camera-only). Treat proximity in time as co-occurrence risk, not a confirmed interaction.")))
  )
}

#' Co-occurrence nav panel. @param id Module id.
cooccurrence_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Co-occurrence", value = "cooccurrence", icon = icon("hourglass-half"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/cooccurrence.css")),
    div(class = "ik-cooc",
        .ik_titlebar(
            tags$h3(class = "ik-cooc-title", "Co-occurrence: Protected ↔ predator timing"),
            .ik_info(ns("cooc_help"), "Co-occurrence — how to read this", cooccurrence_help_body())),
        div(class = "ik-page-period", uiOutput(ns("period_banner"))),
        tags$p(class = "ik-cooc-lead",
          "For every ", tags$b("protected"), "species detection on camera, how long until the nearest ", tags$b("predator"),
          " detection — at the same camera, or anywhere within its ", tags$b("neighbourhood"),
          " (the camera plus a chosen radius around it). Short gaps mean they share the same ground close in time."),
        div(class = "ik-cooc-controls",
            selectInput(ns("pred"), "Predator",  choices = NULL, multiple = TRUE, width = "260px"),
            selectInput(ns("prot"), "Protected", choices = NULL, multiple = TRUE, width = "260px"),
            selectInput(ns("radius"), "Within",
                        choices = c("Same camera" = 0, "250 m" = 250, "500 m" = 500, "750 m" = 750, "1 km" = 1000),
                        selected = 0, width = "150px"),
            div(class = "ik-cooc-check",
              checkboxInput(ns("after_only"),
                tagList("Predator ", tags$b("after"), " only ", tags$span(class = "ik-cooc-hint-inline", "(stalking — predator follows the protected animal)")),
                value = FALSE))),
        uiOutput(ns("intro")),
        tags$h6(class = "ik-cooc-sub", "Gap distribution — click a bar for the detections behind it"),
        plotOutput(ns("dist"), height = "320px", click = ns("dist_click")),
        tags$h6(class = "ik-cooc-sub", "Median gap by season — is the separation widening?"),
        plotOutput(ns("trend"), height = "280px"))
  )
}

#' Wire the co-occurrence "click a bar → co-detections" drill, SHARED by the standalone Co-occurrence
#' page and each Species page's Co-occurrence tab so they behave identically: a 2-tab modal
#' (Detections → Record details) whose Detections table links BOTH species — protected AND predator —
#' separately to their own record, shows the gap WITH direction (predator earlier / later / same time),
#' and — in a proximity (Within radius) view — which camera the predator was at and how far. Registers
#' its outputs/observers once on the module; returns an opener `function(rows, title, subtitle)` to
#' call from the bar-click handler. `rows` is the ik_predator_protected_gaps() subset for the clicked
#' bucket (always protected-anchored: observationID/when = protected, pred_id/pred_when = predator).
#' @param prefer reactive name-preference. @param radius_m reactive metres (drives the proximity cols).
#' @keywords internal
ik_cooc_drill <- function(input, output, session, ik_data, prefer, radius_m = reactive(0)) {
  locs    <- ik_data$app$geography$locations
  rows_rv <- reactiveVal(NULL)
  rec_obs <- reactiveVal(NULL)
  .fmt_gap <- function(h) if (h < 1) sprintf("%d min", round(h * 60)) else
    if (h < 48) sprintf("%.1f h", h) else sprintf("%.1f d", h / 24)
  .fmt_m   <- function(m) if (is.na(m)) "—" else if (m >= 1000) sprintf("%.1f km", m / 1000) else sprintf("%.0f m", m)

  output$cooc_bucket_table <- DT::renderDT({
    rows <- rows_rv(); validate(need(!is.null(rows) && nrow(rows), "No detections."))
    .lnk <- function(label, id) sprintf(                              # species cell → open that record
      "<a class='ik-link' onclick=\"Shiny.setInputValue('%s',{id:'%s'},{priority:'event'})\">%s</a>",
      session$ns("cooc_view_obs"), id, htmltools::htmlEscape(label))
    dir <- ifelse(rows$signed_h > 0, "later", ifelse(rows$signed_h < 0, "earlier", "same time"))
    df <- data.frame(
      Camera = locs$name[match(rows$location_id, locs$location_id)],
      Protected = mapply(.lnk, ik_species_label(rows$scientificName, ik_data, prefer()), rows$observationID),
      `Protected seen` = format(rows$when, "%d %b %Y · %H:%M"),
      Predator = mapply(.lnk, ik_species_label(rows$pred_sci, ik_data, prefer()), rows$pred_id),
      `Predator seen` = format(rows$pred_when, "%d %b %Y · %H:%M"),
      check.names = FALSE, stringsAsFactors = FALSE)
    if (isTRUE(radius_m() > 0) && "pred_dist_m" %in% names(rows)) {    # proximity view: where the predator was
      df$`Predator camera` <- locs$name[match(rows$pred_loc, locs$location_id)]
      df$Apart <- ifelse(rows$pred_dist_m <= 0, "same camera", vapply(rows$pred_dist_m, .fmt_m, character(1)))
    }
    df$Gap <- paste0("predator ", vapply(rows$gap_h, .fmt_gap, character(1)), " ", dir)
    esc <- -which(names(df) %in% c("Protected", "Predator"))         # leave the link cells un-escaped
    DT::datatable(df, rownames = FALSE, selection = "none", escape = esc,
      class = "stripe hover row-border",
      options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE))
  })

  # Click either species → load it in the Record-details tab and switch (no nested modal, so the
  # Detections list is still there to come back to).
  observeEvent(input$cooc_view_obs, {
    id <- input$cooc_view_obs$id
    if (length(id) && nzchar(id)) {
      rec_obs(id); showTab(session = session, inputId = "cooc_tabs", target = "Record details", select = TRUE)
    }
  })
  observeEvent(input$cooc_tab_back, updateTabsetPanel(session, input$cooc_tab_back$tabset, selected = input$cooc_tab_back$to))

  output$cooc_record <- renderUI({
    if (is.null(rec_obs()))
      return(tags$p(class = "ik-cooc-lead", "Click a species in the Detections tab to see its record here."))
    ob <- ik_observation(ik_data, rec_obs()); if (is.null(ob)) return(tags$p("Record not found."))
    tagList(.ik_tab_back(session$ns("cooc_tab_back"), "cooc_tabs", "Detections", "Back to detections"),
            .ovw_title(ik_data, ob, prefer()),
            .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("cooc_rec_subtabs")))
  })

  function(rows, title, subtitle = NULL) {
    rows_rv(rows[order(rows$gap_h), , drop = FALSE]); rec_obs(NULL)
    showModal(modalDialog(
      title = .ik_modal_title(title, subtitle), size = "l", easyClose = TRUE, footer = modalButton("Close"),
      tabsetPanel(id = session$ns("cooc_tabs"),
        tabPanel("Detections",     icon = icon("list"),        DT::dataTableOutput(session$ns("cooc_bucket_table"))),
        tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("cooc_record"))))))
    hideTab(session = session, inputId = "cooc_tabs", target = "Record details")   # appears on first drill
  }
}

#' Co-occurrence server.
#' @param id Module id. @param ik_data The container.
#' @param prefer_scientific reactive name preference (for the record viewer).
cooccurrence_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                                color_mode = reactive("light")) {
  moduleServer(id, function(input, output, session) {
    # All-data analysis — the banner states that (with the full range); no sidebar here, so no toggle.
    output$period_banner <- renderUI(.ik_period_banner(ik_data, NULL, all_data = TRUE, toggle = FALSE))
    is_dark <- reactive(identical(color_mode(), "dark"))
    sg <- ik_species_groups(ik_data)
    .role_taxa <- function(role) { l <- unique(sg$label[sg$role == role & !is.na(sg$monitor)])
      stats::setNames(lapply(l, function(x) sg$scientificName[sg$label == x & !is.na(sg$scientificName)]), l) }
    pred_taxa <- .role_taxa("predator"); prot_taxa <- .role_taxa("protected")
    splits <- unique(sg$label[which(sg$split)])
    .pred_def <- paste0("grp:", if ("Mustelids" %in% names(pred_taxa)) "Mustelids" else names(pred_taxa)[1])
    .prot_def <- paste0("grp:", if ("Kiwi" %in% names(prot_taxa)) "Kiwi" else names(prot_taxa)[1])
    .fmt_gap <- function(h) if (h < 1) sprintf("%d min", round(h * 60)) else
      if (h < 48) sprintf("%.1f h", h) else sprintf("%.1f d", h / 24)
    radius_m <- reactive(as.numeric(input$radius %||% 0))
    prox     <- reactive({ r <- radius_m()                      # the "where" phrase, woven into the copy
      if (r <= 0) "at the same camera" else sprintf("within %s", if (r >= 1000) sprintf("%g km", r / 1000) else sprintf("%g m", r)) })
    prefer      <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    # split-aware grouped pickers (group whole, or sub-species per the project flag) — same as the Map.
    observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      updateSelectInput(session, "pred", choices = ik_species_choices(pred_taxa, ik_data, p, splits), selected = keep(isolate(input$pred), .pred_def))
      updateSelectInput(session, "prot", choices = ik_species_choices(prot_taxa, ik_data, p, splits), selected = keep(isolate(input$prot), .prot_def))
    })
    cooc_open <- ik_cooc_drill(input, output, session, ik_data, prefer, radius_m = radius_m)   # the shared drill

    gaps <- reactive({
      preds <- input$pred; if (!length(preds)) preds <- .pred_def
      prots <- input$prot; if (!length(prots)) prots <- .prot_def
      g <- ik_predator_protected_gaps(ik_data, ik_resolve_species_choice(preds, pred_taxa),
                                      ik_resolve_species_choice(prots, prot_taxa), radius_m = radius_m())
      if (is.null(g)) return(NULL)
      if (isTRUE(input$after_only)) {                  # stalking: predator detected AFTER the protected animal
        g <- g[g$signed_h > 0, , drop = FALSE]
        if (!nrow(g)) return(NULL)
      }
      g$bucket <- cut(g$gap_h, GAP_BREAKS, GAP_LABELS, right = FALSE)
      g
    })

    output$intro <- renderUI({
      g <- gaps(); after <- isTRUE(input$after_only); px <- prox()
      if (is.null(g)) return(tags$p(class = "ik-cooc-lead",
        if (after) sprintf("No protected-species detections were followed by a predator %s.", px)
        else sprintf("No protected-species and predator detections were found %s.", px)))
      prot_l <- paste(ik_choice_labels(input$prot %||% .prot_def, ik_data, prefer()), collapse = " + ")
      pred_l <- paste(ik_choice_labels(input$pred %||% .pred_def, ik_data, prefer()), collapse = " + ")
      tags$p(class = "ik-cooc-lead", if (after) sprintf(
        "%s %s detections followed by a %s %s · median %s until the predator arrives · %d%% within 24 h.",
        format(nrow(g), big.mark = ","), prot_l, pred_l, px, .fmt_gap(stats::median(g$gap_h)), round(100 * mean(g$gap_h <= 24)))
      else sprintf(
        "%s %s detections · median %s to the nearest %s %s · %d%% within 24 h.",
        format(nrow(g), big.mark = ","), prot_l, .fmt_gap(stats::median(g$gap_h)), pred_l, px, round(100 * mean(g$gap_h <= 24))))
    })

    output$dist <- renderPlot({
      g <- gaps(); validate(need(!is.null(g) && nrow(g), "No co-detections to chart."))
      d <- as.data.frame(table(bucket = g$bucket)); d$bucket <- factor(d$bucket, levels = GAP_LABELS)
      ggplot(d, aes(.data$bucket, .data$Freq)) +
        geom_col(fill = "#6a3d9a", width = 0.8) +
        geom_text(aes(label = .data$Freq), vjust = -0.3, size = 3.6, colour = ik_plot_ink(is_dark())) +
        labs(x = sprintf("Time to nearest predator %s", prox()), y = "Protected detections") +
        ik_ggtheme(is_dark()) +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
    }, bg = "transparent")

    output$trend <- renderPlot({
      g <- gaps(); validate(need(!is.null(g) && nrow(g), "No co-detections to chart."))
      op <- ik_observation_period(ik_data)                       # deployment-anchored season (camera)
      g$season <- op$calendar_season[match(g$observationID, op$observationID)]
      g <- g[!is.na(g$season), , drop = FALSE]
      validate(need(nrow(g), "No co-detections to chart."))
      ag <- do.call(rbind, lapply(split(g, g$season), function(s)
        data.frame(season = s$season[1], med = stats::median(s$gap_h), t = mean(as.numeric(s$when)))))
      validate(need(!is.null(ag) && nrow(ag) >= 2, "Need at least two seasons for a trend."))
      ag <- ag[order(ag$t), ]; ag$season <- factor(ag$season, levels = ag$season)
      ggplot(ag, aes(.data$season, .data$med, group = 1)) +
        geom_line(colour = "#6a3d9a", linewidth = 0.9) + geom_point(colour = "#6a3d9a", size = 2.4) +
        labs(x = NULL, y = "Median gap (hours)", subtitle = "rising = protected & predators separating in time") +
        ik_ggtheme(is_dark()) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank())
    }, bg = "transparent")

    # Click a distribution bar → the shared co-occurrence drill (the bucket's co-detections, each with
    # both species linked to their record + the gap direction). See ik_cooc_drill().
    observeEvent(input$dist_click, {
      g <- gaps(); cl <- input$dist_click; if (is.null(g) || is.null(cl$x)) return()
      xi <- round(cl$x); if (xi < 1 || xi > length(GAP_LABELS)) return()
      bk <- GAP_LABELS[xi]; rows <- g[as.character(g$bucket) == bk, , drop = FALSE]
      if (!nrow(rows)) return()
      cooc_open(rows,
        sprintf("Detections %s from a predator", bk),
        sprintf("%s %s detection%s paired with the nearest predator — click either species for its record",
                format(nrow(rows), big.mark = ","),
                paste(ik_choice_labels(input$prot %||% .prot_def, ik_data, prefer()), collapse = " + "),
                if (nrow(rows) == 1) "" else "s"))
    })
  })
}
