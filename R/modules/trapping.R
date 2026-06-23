# trapping.R — the Trapping tab of Data → Quality: trap check-frequency / effort review for a
# chosen period. A per-LINE summary (the natural review unit), each line coloured by how often
# its traps are checked (mean interval); click a line for its per-trap breakdown (the same
# per-trap data that will feed the map). Driven by ik_trap_review() / ik_trap_review_lines().

#' Trapping review tab content (lives inside the Quality nav_panel). @param id Module id.
trapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/trapping.css")),
    div(class = "ik-trapping",
        tags$h5("Trap check-frequency review", class = "ik-review-head"),
        # Two tabs keep the current-period management DETAIL apart from the cross-period TREND — the
        # table is Period-driven, the trend ignores Period, so they don't belong on one page together.
        # "By trapline" leads (default): it's the day-to-day management view and the quicker to load;
        # "Over time" is the longer-horizon trend.
        tabsetPanel(
          id = ns("trap_view"),
          tabPanel(
            "By trapline", icon = icon("table-list"),
            div(class = "trap-controls",
                checkboxGroupInput(ns("show_extra"), "Also show (otherwise active traps only)", inline = TRUE,
                  choices = c("Dormant (6 mo+)" = "dormant", "Historic (12 mo+)" = "historic"),
                  selected = "dormant")),
            uiOutput(ns("intro")),
            div(class = "ik-trapping-scroll", uiOutput(ns("table")))),
          tabPanel(
            "Over time", icon = icon("chart-line"),
            div(class = "trap-timeline",
                div(class = "trap-timeline-head",
                    tags$span(class = "trap-timeline-title", "Servicing over time"),
                    radioButtons(ns("grain"), NULL, inline = TRUE,
                                 choices = c("By season" = "season", "By year" = "year"), selected = "season")),
                uiOutput(ns("timeline_note")),
                plotOutput(ns("timeline"), height = "620px")))))
  )
}

#' Trapping review server. @param id Module id. @param ik_data The ik_data container.
#' @param selection Reactive selection SPEC (Period + Reserve from the sidebar).
#' @param color_mode Reactive theme ("light"/"dark") for the timeline plot.
trapping_server <- function(id, ik_data, selection, color_mode = reactive("light"),
                            prefer_scientific = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_dark <- reactive(identical(color_mode(), "dark"))
    prefer  <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")

    # Status cell text: cadence "X d" for good/watch/neglected; sparse → check count; else the tier name.
    .iv <- function(status, mean, n) {
      if (is.na(status))                        "—"
      else if (status %in% c("dormant", "historic")) tools::toTitleCase(status)
      else if (status == "insufficient_data")   sprintf("%d check%s", n, if (identical(n, 1L) || identical(n, 1)) "" else "s")
      else if (is.finite(mean))                 sprintf("%.0f d", mean) else "—"
    }
    review <- reactive({
      per_all <- ik_trap_review(ik_data, .ik_nz(selection()$season))
      rsv <- .ik_nz(selection()$reserve)                    # Reserve filter from the sidebar
      if (!is.null(per_all) && !is.null(rsv)) per_all <- per_all[per_all$reserve %in% rsv, , drop = FALSE]
      if (!is.null(per_all) && !nrow(per_all)) per_all <- NULL
      per <- per_all
      if (!is.null(per)) {                                   # filter: active only unless dormant/historic asked for
        hide <- setdiff(c("dormant", "historic"), input$show_extra %||% character(0))
        per <- per[!per$status %in% hide, , drop = FALSE]; if (!nrow(per)) per <- NULL
      }
      list(all = per_all, per = per, lines = ik_trap_review_lines(per, ik_data))
    })

    # Servicing-over-time chart — every season/year (cross-period), scoped to the sidebar Reserve.
    # Depends only on grain + reserve (+ active datasets), NOT the whole selection, so changing Period
    # doesn't recompute the cross-period series; cached so re-toggling grain/reserve is instant.
    rsv_t <- reactive(.ik_nz(selection()$reserve))
    series_t <- reactive(ik_trap_review_series(ik_data, input$grain %||% "season", rsv_t())) |>
      bindCache(input$grain %||% "season", rsv_t(), ik_active_datasets())
    output$timeline <- renderPlot({
      s <- series_t(); validate(need(!is.null(s) && nrow(s), "Not enough trap history to chart."))
      s$period <- factor(s$period, levels = unique(s$period[order(s$order)]))
      s$facet  <- factor(s$facet, levels = unique(s$facet))   # builder emits servicing · interval · catch rate
      pal <- c(Good = "#2e7d32", Watch = "#f9a825", Neglected = "#c62828", Interval = "#5a6b7b", `Catch rate` = "#6a3d9a")
      ggplot2::ggplot(s, ggplot2::aes(.data$period, .data$value, colour = .data$series, group = .data$series)) +
        ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) + ggplot2::geom_point(size = 1.9, na.rm = TRUE) +
        ggplot2::facet_wrap(ggplot2::vars(.data$facet), ncol = 1, scales = "free_y", strip.position = "top") +
        ggplot2::scale_colour_manual(values = pal, breaks = c("Good", "Watch", "Neglected")) +
        ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom",
          strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.05), colour = ik_plot_ink(is_dark())),
          strip.background = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1.1, "lines"))
    }, bg = "transparent")

    output$timeline_note <- renderUI({                          # flag the omitted in-progress period
      s <- series_t(); inc <- if (is.null(s)) NULL else attr(s, "incomplete_period")
      if (is.null(inc)) return(NULL)
      tags$p(class = "trap-timeline-note",
             sprintf("Completed periods only — the current period (%s) is still in progress and is omitted.", inc))
    })

    output$intro <- renderUI({
      per <- review()$all                       # counts over ALL traps (so hidden tiers still show a count)
      if (is.null(per)) return(tags$p("No trap checks in this period."))
      st <- table(factor(per$status, c("good", "watch", "neglected", "insufficient_data", "dormant", "historic")))
      # Legend cutoffs scoped to the RESERVE(s) in view — each reserve is calibrated against its own
      # cadence spread, so filtering to a reserve shows that reserve's exact cutoff (falls back to the
      # in-view dataset blend when a reserve has no own calibration).
      h  <- ik_trap_health_cutoffs(ik_data, reserves = unique(per$reserve), datasets = unique(per$dataset))
      gm <- round(max(h$good_max  %||% TRAP_GOOD_INTERVAL_DAYS, h$floor   %||% 0))
      wm <- round(min(h$watch_max %||% TRAP_WATCH_INTERVAL_DAYS, h$ceiling %||% Inf))
      leg <- function(cls, lab, n) tags$span(class = "trap-legend-item",
        tags$span(class = paste0("trap-swatch trap-", cls)), sprintf("%s (%d)", lab, n))
      legs <- list(
        leg("good", sprintf("Checked ≤%dd", gm), st[["good"]]),
        leg("watch", sprintf("%d–%dd", gm, wm), st[["watch"]]),
        leg("neglected", sprintf("Neglected >%dd", wm), st[["neglected"]]))
      if (st[["insufficient_data"]] > 0) legs <- c(legs, list(leg("insufficient_data", "Insufficient data", st[["insufficient_data"]])))
      if (st[["dormant"]]  > 0)          legs <- c(legs, list(leg("dormant",  "Dormant 6 mo+",  st[["dormant"]])))
      if (st[["historic"]] > 0)          legs <- c(legs, list(leg("historic", "Historic 12 mo+", st[["historic"]])))
      tagList(
        tags$p(class = "trap-lead", sprintf(
          "Servicing by trapline, judged as of the period end — %s traps, %s checks, %s captures this period. ",
          .ov_num(nrow(per)), .ov_num(sum(per$n_checks)), .ov_num(sum(per$captures))),
          "A trap unchecked all period reads neglected; too few checks to judge a cadence → insufficient data; ",
          "long-unchecked traps fall to dormant/historic. Click a line for its traps."),
        tags$div(class = "trap-legend", legs),
        tags$p(class = "trap-thresholds", sprintf(
          "Good ≤ the project's median check interval (%d d), capped so any gap over %d d is neglected; dormant ≥ 6 months and historic ≥ 12 months since last check (relative to the period end). Toggle dormant/historic above; the cadence buckets recalibrate only on re-import.",
          gm, wm))
      )
    })

    output$table <- renderUI({
      ln <- review()$lines
      if (is.null(ln)) return(NULL)
      hdr <- tags$tr(tags$th("Line"), tags$th("Traps"), tags$th("Checks"),
                     tags$th("Mean interval"), tags$th("Trap-days"), tags$th("Captures"))
      body <- lapply(split(ln, factor(ln$reserve, levels = unique(ln$reserve))), function(lr) {
        res_row <- tags$tr(class = "trap-reserve-row", tags$td(lr$reserve[1], colspan = 6))
        rows <- lapply(seq_len(nrow(lr)), function(i) {
          tags$tr(class = "trap-click",
            onclick = sprintf("Shiny.setInputValue('%s',{line:%s,reserve:%s},{priority:'event'})",
                              ns("line"), .ik_jsq(lr$line[i]), .ik_jsq(lr$reserve[i])),
            tags$td(class = "trap-loc", lr$line[i]),
            tags$td(.ov_num(lr$n_traps[i])),
            tags$td(.ov_num(lr$checks[i])),
            tags$td(class = paste0("trap-cell trap-", lr$status[i]), .iv(lr$status[i], lr$mean_interval_days[i], lr$checks[i])),
            tags$td(.ov_num(round(lr$trap_days[i]))),
            tags$td(.ov_num(lr$captures[i])))
        })
        tagList(res_row, rows)
      })
      tags$table(class = "ik-trapping-table", tags$thead(hdr), tags$tbody(body))
    })

    # Line → a three-tab modal: a per-trap summary ("Traps"), the individual checks of a clicked
    # trap ("Trap detail"), and the full record viewer for a clicked check ("Record"). Tabs +
    # back-links let you climb back up the drill without reopening.
    sel_trap <- reactiveVal(NULL)
    rec_obs  <- reactiveVal(NULL)   # observationID open in the Record tab (a check IS an observation)

    .summary_table <- function(t) tags$table(class = "trap-detail",
      tags$thead(tags$tr(tags$th("Trap"), tags$th("Checks"), tags$th("Mean interval"),
                         tags$th("Last check"), tags$th("Captures"))),
      tags$tbody(lapply(seq_len(nrow(t)), function(i) tags$tr(
        class = "trap-click",
        onclick = sprintf("Shiny.setInputValue('%s',{location:%s,name:%s},{priority:'event'})",
                          ns("trap"), .ik_jsq(t$location[i]), .ik_jsq(t$name[i])),
        tags$td(t$name[i]),
        tags$td(.ov_num(t$n_checks[i])),
        tags$td(class = paste0("trap-cell trap-", t$status[i]), .iv(t$status[i], t$mean_interval_days[i], t$n_checks[i])),
        tags$td(if (is.finite(as.numeric(t$last_check[i]))) format(t$last_check[i], "%d %b %Y") else "—"),
        tags$td(.ov_num(t$captures[i]))))))

    observeEvent(input$line, {
      d   <- input$line
      per <- review()$per
      # NA-safe match: some traps carry no line (NA), and `per$line == d$line` is NA for those,
      # which would inject all-NA rows into `t` (→ NA status → a crash downstream in .iv).
      t   <- per[!is.na(per$reserve) & !is.na(per$line) & per$reserve == d$reserve & per$line == d$line, , drop = FALSE]
      t   <- t[order(-t$mean_interval_days), , drop = FALSE]
      sel_trap(NULL); rec_obs(NULL)
      showModal(modalDialog(
        title = sprintf("%s · Line %s — %d traps", d$reserve, d$line, nrow(t)),
        tabsetPanel(
          id = ns("line_tabs"),
          tabPanel("Traps",
            tags$p(class = "trap-lead", "Click a trap to see its individual checks this period."),
            .summary_table(t)),
          tabPanel("Trap detail", uiOutput(ns("trap_detail"))),
          tabPanel("Record",      uiOutput(ns("check_record")))),
        easyClose = TRUE, size = "l", footer = modalButton("Close")))
    })

    observeEvent(input$trap, {
      sel_trap(input$trap); rec_obs(NULL)
      updateTabsetPanel(session, "line_tabs", selected = "Trap detail")
    })

    # Check row → its full observation record (a trap check is exactly one observation).
    observeEvent(input$check, {
      rec_obs(input$check$obs)
      updateTabsetPanel(session, "line_tabs", selected = "Record")
    })

    observeEvent(input$tab_back, updateTabsetPanel(session, input$tab_back$tabset, selected = input$tab_back$to))

    output$trap_detail <- renderUI({
      tr <- sel_trap()
      if (is.null(tr)) return(tags$p(class = "trap-lead", "Select a trap from the Traps tab."))
      ch   <- ik_trap_checks(ik_data, tr$location, .ik_nz(selection()$season))
      dash <- function(x) if (length(x) == 0 || is.na(x) || !nzchar(x)) "—" else x
      tagList(
        .ik_tab_back(ns("tab_back"), "line_tabs", "Traps", "Back to traps"),
        tags$p(class = "trap-lead", sprintf("Trap %s — %d checks this period (newest first). Click a check for its full record.",
                                            tr$name, if (is.null(ch)) 0 else nrow(ch))),
        if (is.null(ch)) tags$p("No checks this period.") else tags$table(class = "trap-detail",
          tags$thead(tags$tr(tags$th("Check date"), tags$th("Interval"), tags$th("Outcome"),
                             tags$th("Bait"), tags$th("Volunteer"))),
          tags$tbody(lapply(seq_len(nrow(ch)), function(i) tags$tr(
            class = "trap-click",
            onclick = sprintf("Shiny.setInputValue('%s',{obs:'%s'},{priority:'event'})",
                              ns("check"), ch$observationID[i]),
            tags$td(format(ch$check_date[i], "%d %b %Y")),
            tags$td(if (isTRUE(ch$is_first[i])) tags$em("first record") else sprintf("%d d", ch$interval_days[i])),
            tags$td(ch$outcome[i]),
            tags$td(dash(ch$bait[i])),
            tags$td(dash(ch$volunteer[i])))))))
    })

    # Record tab — the full observation viewer for the clicked check, with a back link to the checks.
    output$check_record <- renderUI({
      if (is.null(rec_obs())) return(tags$p(class = "trap-lead", "Select a check from the Trap detail tab."))
      ob <- ik_observation(ik_data, rec_obs()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(
        .ik_tab_back(ns("tab_back"), "line_tabs", "Trap detail", "Back to checks"),
        .ovw_title(ik_data, ob, prefer()),
        .ovw_tabs(ik_data, ob, prefer(), tabset_id = ns("trap_subtabs")))
    })
  })
}
