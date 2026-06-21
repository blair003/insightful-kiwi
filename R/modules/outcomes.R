# outcomes.R — the "Are we winning?" view: the trust's causal chain on one seasonal timeline.
# Three stacked panels (read top→bottom): trap catches (the control), predator camera RAI
# (want DOWN), kiwi camera RAI (want UP). Network level (mean over reserves ± SE). Data from
# ik_outcome_series(); plotted as ggplot2 small multiples.

# Thematic palette: predators warm, kiwi green.
OUTCOME_PALETTE <- c(Mustelids = "#c62828", Rats = "#ef6c00", Cats = "#8e24aa",
                     Dogs = "#6d4c41", Hedgehogs = "#5d4037", Possums = "#795548",
                     Kiwi = "#2e7d32")

OUTCOME_PANELS <- c("Catches · per 100 trap-nights",
                    "Predators · camera RAI (lower is better)",
                    "Kiwi · camera RAI (higher is better)")

#' Outcomes nav panel UI. @param id Module id.
outcomes_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Are we winning?", value = "outcomes", icon = icon("chart-line"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/outcomes.css"),
    div(class = "ik-outcomes",
        uiOutput(ns("intro")),
        div(class = "out-controls",
            checkboxGroupInput(ns("taxa"), "Show species", choices = NULL, inline = TRUE)),
        plotOutput(ns("plot"), height = "660px", click = ns("plot_click")))
  )
}

#' Outcomes server. @param id Module id. @param ik_data The ik_data container.
#' @param prefer_scientific A reactive TRUE to show scientific names (drill records/viewer).
outcomes_server <- function(id, ik_data, prefer_scientific) {
  moduleServer(id, function(input, output, session) {
    series <- reactive(ik_outcome_series(ik_data))    # ~7s, computed once per session

    # Species toggle — default to the core stoat-vs-kiwi story so the chart reads cleanly
    # (rats otherwise dominate the predator scale); the rest are opt-in.
    sg    <- ik_species_groups(ik_data)
    avail <- { s <- sg[sg$role %in% c("predator", "protected") & !is.na(sg$monitor), , drop = FALSE]
               unique(s$label[order(s$priority)]) }
    updateCheckboxGroupInput(session, "taxa", choices = avail,
                             selected = intersect(c("Mustelids", "Kiwi"), avail))

    output$intro <- renderUI({
      projects <- unique(unlist(lapply(ik_data$datasets, function(d) d$meta$project)))
      tagList(
        tags$h3(class = "ik-out-title", "Are we winning?"),
        tags$p(class = "ik-out-lead",
          tagList(paste(projects, collapse = " · "), " — the control story across seasons. ",
            "We ", tags$b("trap predators"), " → predator detections on camera should ",
            tags$b("fall"), " → kiwi detections should ", tags$b("rise"), ". ",
            "Lines are the network mean across reserves; bands are ± 1 SE. ",
            tags$b("Click a point"), " for its per-reserve breakdown."))
      )
    })

    # the data currently plotted (panel + ordered season factor) — read by the point-click handler
    plotted <- reactiveVal(NULL)

    output$plot <- renderPlot({
      s <- series()
      validate(need(!is.null(s) && nrow(s), "Not enough data to chart outcomes."))
      s <- s[s$taxon %in% input$taxa, , drop = FALSE]
      validate(need(nrow(s) > 0, "Select at least one species to chart."))
      s$panel <- factor(ifelse(s$metric_type == "trap_rate", OUTCOME_PANELS[1],
                        ifelse(s$role == "protected", OUTCOME_PANELS[3], OUTCOME_PANELS[2])),
                        levels = OUTCOME_PANELS)
      s$season <- factor(s$season, levels = unique(s$season[order(s$season_order)]))
      plotted(s)
      pal <- OUTCOME_PALETTE[names(OUTCOME_PALETTE) %in% unique(s$taxon)]

      ggplot(s, aes(.data$season, .data$value, colour = .data$taxon,
                    fill = .data$taxon, group = .data$taxon)) +
        geom_ribbon(aes(ymin = .data$value - .data$se, ymax = .data$value + .data$se),
                    alpha = 0.13, colour = NA, na.rm = TRUE) +
        geom_line(linewidth = 0.8, na.rm = TRUE) +
        geom_point(size = 1.9, na.rm = TRUE) +
        facet_wrap(vars(.data$panel), ncol = 1, scales = "free_y", strip.position = "top") +
        scale_colour_manual(values = pal, breaks = names(pal)) +
        scale_fill_manual(values = pal, breaks = names(pal)) +
        labs(x = NULL, y = NULL, colour = NULL, fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.05)),
          strip.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1.1, "lines"))
    })

    # Click a point → the SAME tabbed drill as the Overview: Summary (per-reserve basis) → Records
    # (a reserve's auditable records, via ik_metric_obs) → Record Details (the inline viewer). The
    # series keeps only network means, so we recompute the one season's per-reserve metric here.
    # Precision follows the Overview's adaptive rule (.ov_dp): camera 1 dp, trap 2–3 dp.
    prefer  <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    drill   <- reactiveVal(NULL)   # {cand, sp, taxa, is_cam, R, nrec, dp, unit}
    records <- reactiveVal(NULL)   # records behind a clicked reserve
    rec_ctx <- reactiveVal(NULL)   # caption for the Records tab
    rec_obs <- reactiveVal(NULL)   # observationID open in Record Details

    observeEvent(input$plot_click, {
      cl <- input$plot_click; s <- plotted()
      if (is.null(cl) || is.null(s) || !nrow(s)) return()
      lv <- levels(s$season); xi <- round(cl$x)                  # discrete x → nearest season slot
      if (is.na(xi) || xi < 1 || xi > length(lv)) return()
      cand <- s[s$season == lv[xi] & (is.null(cl$panelvar1) | s$panel == cl$panelvar1) &
                  !is.na(s$value), , drop = FALSE]
      if (!nrow(cand)) return()
      cand <- cand[which.min(abs(cand$value - cl$y)), , drop = FALSE]   # nearest line by value

      is_cam <- identical(cand$metric_type, "camera_rai")
      sci    <- sg$scientificName[sg$label == cand$taxon & !is.na(sg$scientificName)]
      taxa   <- stats::setNames(list(sci), cand$taxon)
      sp     <- list(season = ik_expand_period(paste0("season:", cand$season), ik_data))
      res <- withProgress(message = "Loading breakdown…", value = 0.6,
        if (is_cam) ik_rai(ik_data, sp, taxa) else ik_trap_rate(ik_data, sp, taxa))
      R    <- res$summary[res$summary$taxon == cand$taxon & !is.na(res$summary$metric), , drop = FALSE]
      R    <- R[order(R$reserve), , drop = FALSE]
      cnt  <- if (is_cam) "individuals" else "captures"
      Ld   <- res$lines[res$lines$taxon == cand$taxon, , drop = FALSE]
      nrec <- tapply(Ld[[cnt]], Ld$reserve, sum)
      dp   <- .ov_dp(c(cand$value, cand$se, R$metric, R$se), if (is_cam) 1L else 2L, if (is_cam) 1L else 3L)
      unit <- if (is_cam) "camera RAI" else "captures / 100 trap-nights"
      drill(list(cand = cand, sp = sp, taxa = taxa, is_cam = is_cam, R = R, nrec = nrec, dp = dp, unit = unit))
      records(NULL); rec_obs(NULL); rec_ctx(NULL)
      fd <- paste0("%.", dp, "f")
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s · %s", cand$taxon, cand$season),
          sprintf("network %s = %s%s over %d reserves — open a reserve for its records", unit,
                  sprintf(fd, cand$value),
                  if (!is.na(cand$se)) sprintf(" ± %s SE", sprintf(fd, cand$se)) else "", cand$n_reserves)),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("out_tabs"),
          tabPanel("Summary",        icon = icon("table-list"),  uiOutput(session$ns("out_breakdown"))),
          tabPanel("Records",        icon = icon("list"),        uiOutput(session$ns("out_records_ui"))),
          tabPanel("Record Details", icon = icon("circle-info"), uiOutput(session$ns("out_record"))))))
    })

    # Summary tab: the per-reserve basis (Reserve · count · RAI/rate · Lines). A reserve with
    # records is clickable → its records (Records tab).
    output$out_breakdown <- renderUI({
      d <- drill(); req(d); R <- d$R; fd <- paste0("%.", d$dp, "f")
      if (!nrow(R)) return(tags$p(class = "ik-drill-summary", "No per-reserve records for this point."))
      cnt_lab <- if (d$is_cam) "Detections" else "Catches"
      tags$table(class = "ik-drill-table",
        tags$thead(tags$tr(tags$th("Reserve"), tags$th(cnt_lab),
                           tags$th(if (d$is_cam) "RAI" else "Rate"), tags$th("Lines"))),
        tags$tbody(lapply(seq_len(nrow(R)), function(i) {
          cn <- d$nrec[R$reserve[i]]; cn <- if (length(cn) == 0 || is.na(cn)) 0L else as.integer(cn)
          drillable <- cn > 0
          tags$tr(class = if (drillable) "ik-drill-row" else NULL,
            title = if (drillable) "Show this reserve's records" else NULL,
            onclick = if (drillable) sprintf(
              "Shiny.setInputValue('%s',{reserve:'%s'},{priority:'event'})",
              session$ns("out_obs"), R$reserve[i]) else NULL,
            tags$td(R$reserve[i]), tags$td(.ov_num(cn)),
            tags$td(sprintf(fd, R$metric[i]), if (!is.na(R$se[i])) sprintf(paste0(" ± ", fd), R$se[i])),
            tags$td(.ov_num(R$n_lines[i])))
        })))
    })

    observeEvent(input$out_obs, {
      d <- drill(); req(d)
      records(ik_metric_obs(ik_data, d$sp, d$taxa, d$cand$taxon, reserve = input$out_obs$reserve,
                            source_type = if (d$is_cam) "camera" else "trap"))
      rec_obs(NULL)
      rec_ctx(sprintf("The %s behind %s · %s · %s. ", if (d$is_cam) "detections" else "captures",
                      d$cand$taxon, input$out_obs$reserve, d$cand$season))
      updateTabsetPanel(session, "out_tabs", selected = "Records")
    })

    output$out_records_ui <- renderUI({
      if (is.null(records()))
        return(tags$p(class = "ik-drill-summary", "Open a reserve in the Summary tab to list its records."))
      tagList(
        tags$p(class = "ik-drill-summary", rec_ctx(), tags$b("Click a row"), " for the full record."),
        DT::dataTableOutput(session$ns("out_records_table")))
    })

    output$out_records_table <- DT::renderDT({
      o <- records(); validate(need(!is.null(o) && nrow(o), "No records."))
      has_t    <- format(o$when, "%H:%M:%S") != "00:00:00"
      when_lab <- ifelse(has_t, format(o$when, "%d %b %Y · %H:%M"), format(o$when, "%d %b %Y"))
      df <- data.frame(When = when_lab, Species = ik_species_label(o$scientificName, ik_data, prefer()),
                       Count = o$count, Location = o$locationName, ObsID = o$observationID,
                       check.names = FALSE, stringsAsFactors = FALSE)
      dt <- DT::datatable(df, rownames = FALSE, selection = "single",
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "tip", destroy = TRUE,
          columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))   # hide ObsID
      .ik_dt_highlight_row(dt, "ObsID", rec_obs())
    })

    observeEvent(input$out_records_table_rows_selected, {
      i <- input$out_records_table_rows_selected; o <- records()
      if (length(i) && !is.null(o) && i <= nrow(o)) {
        rec_obs(o$observationID[i]); updateTabsetPanel(session, "out_tabs", selected = "Record Details")
      }
      DT::selectRows(DT::dataTableProxy("out_records_table"), NULL)
    })

    output$out_record <- renderUI({
      if (is.null(rec_obs()))
        return(tags$p(class = "ik-drill-summary", "Click a record in the Records tab to see it here."))
      ob <- ik_observation(ik_data, rec_obs()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("out_rec_subtabs")))
    })
  })
}
