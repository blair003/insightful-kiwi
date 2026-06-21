# trapping.R — the Trapping tab of Data → Quality: trap check-frequency / effort review for a
# chosen period. A per-LINE summary (the natural review unit), each line coloured by how often
# its traps are checked (mean interval); click a line for its per-trap breakdown (the same
# per-trap data that will feed the map). Driven by ik_trap_review() / ik_trap_review_lines().

#' Trapping review tab content (lives inside the Quality nav_panel). @param id Module id.
trapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/trapping.css"),
    div(class = "ik-trapping",
        tags$h5("Trap check-frequency review", class = "ik-review-head"),
        div(class = "trap-controls", selectInput(ns("period"), "Period", choices = NULL, width = "260px")),
        uiOutput(ns("intro")),
        div(class = "ik-trapping-scroll", uiOutput(ns("table"))))
  )
}

#' Trapping review server. @param id Module id. @param ik_data The ik_data container.
trapping_server <- function(id, ik_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr_dp <- ik_deployment_period(ik_data)
    tr_dp <- tr_dp[tr_dp$source_type == "trap", , drop = FALSE]
    tr_seasons <- ik_season_levels(tr_dp)
    # Default to the latest NEAR-FULL trap season, not the latest — a current season with only
    # a couple of weeks of data makes every line look neglected. "Full" = the season's data
    # envelope spans most of a season (≥ 75 of ~90 days), from app$period$monitoring_season.
    ms   <- ik_data$app$period$monitoring_season
    ms   <- ms[ms$source_type == "trap", , drop = FALSE]
    span <- tapply(seq_len(nrow(ms)), ms$calendar_season, function(ix)
      as.numeric(difftime(max(ms$end[ix]), min(ms$start[ix]), units = "days")))
    full <- intersect(tr_seasons, names(span)[span >= 75])
    default_season <- if (length(full)) full[length(full)]
                      else if (length(tr_seasons)) tr_seasons[length(tr_seasons)] else NULL
    updateSelectInput(session, "period", choices = ik_period_choices(ik_data),
                      selected = if (!is.null(default_season)) paste0("season:", default_season) else "all")

    review <- reactive({
      per <- ik_trap_review(ik_data, ik_expand_period(input$period, ik_data))
      list(per = per, lines = ik_trap_review_lines(per, ik_data))
    })

    output$intro <- renderUI({
      per <- review()$per
      if (is.null(per)) return(tags$p("No trap checks in this period."))
      st <- table(factor(per$status, c("good", "watch", "neglected")))
      h  <- ik_data$meta$trapping$health        # effective cutoffs = percentiles clamped by guardrails
      gm <- round(max(h$good_max  %||% TRAP_GOOD_INTERVAL_DAYS, h$floor   %||% 0))
      wm <- round(min(h$watch_max %||% TRAP_WATCH_INTERVAL_DAYS, h$ceiling %||% Inf))
      leg <- function(cls, lab, n) tags$span(class = "trap-legend-item",
        tags$span(class = paste0("trap-swatch trap-", cls)), sprintf("%s (%d)", lab, n))
      tagList(
        tags$p(class = "trap-lead", sprintf(
          "Check frequency by trapline — %s traps, %s checks, %s captures this period. ",
          .ov_num(nrow(per)), .ov_num(sum(per$n_checks)), .ov_num(sum(per$captures))),
          "Mean interval counts the gap since each trap's last check, so traps that stopped being ",
          "checked show as neglected; click a line for its traps."),
        tags$div(class = "trap-legend",
          leg("good", sprintf("Checked ≤%dd", gm), st[["good"]] %||% 0),
          leg("watch", sprintf("%d–%dd", gm, wm), st[["watch"]] %||% 0),
          leg("neglected", sprintf("Neglected >%dd", wm), st[["neglected"]] %||% 0)),
        tags$p(class = "trap-thresholds", sprintf(
          "Buckets are this project's own check-rate spread (good ≤ the median, %d d), capped by absolute guardrails so any gap over %d d is always neglected. They recalibrate only when data is re-imported.",
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
            onclick = sprintf("Shiny.setInputValue('%s',{line:'%s',reserve:'%s'},{priority:'event'})",
                              ns("line"), lr$line[i], lr$reserve[i]),
            tags$td(class = "trap-loc", lr$line[i]),
            tags$td(.ov_num(lr$n_traps[i])),
            tags$td(.ov_num(lr$checks[i])),
            tags$td(class = paste0("trap-cell trap-", lr$status[i]), sprintf("%.0f d", lr$mean_interval_days[i])),
            tags$td(.ov_num(round(lr$trap_days[i]))),
            tags$td(.ov_num(lr$captures[i])))
        })
        tagList(res_row, rows)
      })
      tags$table(class = "ik-trapping-table", tags$thead(hdr), tags$tbody(body))
    })

    # Line → a two-tab modal: a per-trap summary ("Traps"), and the individual checks of a
    # clicked trap ("Trap detail"). Tabs mean you can go back to the summary without reopening.
    sel_trap <- reactiveVal(NULL)

    .summary_table <- function(t) tags$table(class = "trap-detail",
      tags$thead(tags$tr(tags$th("Trap"), tags$th("Checks"), tags$th("Mean interval"),
                         tags$th("Last check"), tags$th("Captures"))),
      tags$tbody(lapply(seq_len(nrow(t)), function(i) tags$tr(
        class = "trap-click",
        onclick = sprintf("Shiny.setInputValue('%s',{location:'%s',name:'%s'},{priority:'event'})",
                          ns("trap"), t$location[i], t$name[i]),
        tags$td(t$name[i]),
        tags$td(.ov_num(t$n_checks[i])),
        tags$td(class = paste0("trap-cell trap-", t$status[i]), sprintf("%.0f d", t$mean_interval_days[i])),
        tags$td(if (is.finite(as.numeric(t$last_check[i]))) format(t$last_check[i], "%d %b %Y") else "—"),
        tags$td(.ov_num(t$captures[i]))))))

    observeEvent(input$line, {
      d   <- input$line
      per <- review()$per
      t   <- per[per$reserve == d$reserve & per$line == d$line, , drop = FALSE]
      t   <- t[order(-t$mean_interval_days), , drop = FALSE]
      sel_trap(NULL)
      showModal(modalDialog(
        title = sprintf("%s · Line %s — %d traps", d$reserve, d$line, nrow(t)),
        tabsetPanel(
          id = ns("line_tabs"),
          tabPanel("Traps",
            tags$p(class = "trap-lead", "Click a trap to see its individual checks this period."),
            .summary_table(t)),
          tabPanel("Trap detail", uiOutput(ns("trap_detail")))),
        easyClose = TRUE, size = "l", footer = modalButton("Close")))
    })

    observeEvent(input$trap, {
      sel_trap(input$trap)
      updateTabsetPanel(session, "line_tabs", selected = "Trap detail")
    })

    output$trap_detail <- renderUI({
      tr <- sel_trap()
      if (is.null(tr)) return(tags$p(class = "trap-lead", "Select a trap from the Traps tab."))
      ch   <- ik_trap_checks(ik_data, tr$location, ik_expand_period(input$period, ik_data))
      dash <- function(x) if (length(x) == 0 || is.na(x) || !nzchar(x)) "—" else x
      tagList(
        tags$p(class = "trap-lead", sprintf("Trap %s — %d checks this period (newest first).",
                                            tr$name, if (is.null(ch)) 0 else nrow(ch))),
        if (is.null(ch)) tags$p("No checks this period.") else tags$table(class = "trap-detail",
          tags$thead(tags$tr(tags$th("Check date"), tags$th("Interval"), tags$th("Outcome"),
                             tags$th("Bait"), tags$th("Volunteer"))),
          tags$tbody(lapply(seq_len(nrow(ch)), function(i) tags$tr(
            tags$td(format(ch$check_date[i], "%d %b %Y")),
            tags$td(if (isTRUE(ch$is_first[i])) tags$em("first record") else sprintf("%d d", ch$interval_days[i])),
            tags$td(ch$outcome[i]),
            tags$td(dash(ch$bait[i])),
            tags$td(dash(ch$volunteer[i])))))))
    })
  })
}
