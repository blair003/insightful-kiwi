# trapping.R — the Trapping tab of Data → Quality: trap check-frequency / effort review for a
# chosen period. A per-LINE summary (the natural review unit), each line coloured by how often
# its traps are checked (mean interval); click a line for its per-trap breakdown (the same
# per-trap data that will feed the map). Driven by ik_trap_review() / ik_trap_review_lines().

#' A compact left→right graphic of the servicing cadence path (Good → Watch → Neglected → Inactive),
#' so it reads as an ORDER a trap slips down over time as it's checked less often. @keywords internal
.trap_status_progression <- function() {
  step <- function(cls, lab) tags$div(class = paste0("trap-prog-step trap-", cls), lab)
  arr  <- tags$span(class = "trap-prog-arrow", "›")
  tags$div(class = "trap-progression",
    step("good", "Good"), arr, step("watch", "Watch"), arr,
    step("neglected", "Neglected"), arr, step("inactive", "Inactive"))
}

#' Per-TAB "how to read this" help body for Trap review (the (?) now lives in each tab, not the page
#' heading, so a tab's help is only reachable when that tab is open). `tr_meta` = ik_data$meta$trapping
#' (servicing-health + rate config), so cutoffs/norm match what's used. `which` selects the tab:
#' "byline" (the grading), "map" (the servicing map), "overtime" (the trend). @keywords internal
trapping_help_body <- function(tr_meta = NULL, which = c("byline", "map", "overtime")) {
  which <- match.arg(which)
  P  <- function(...) tags$p(...)
  h  <- tr_meta$health %||% list()
  pc <- h$percentiles %||% c(good = 0.5, watch = 0.9)
  pg <- round(100 * (pc[["good"]]  %||% 0.5))
  pw <- round(100 * (pc[["watch"]] %||% 0.9))
  fl <- h$floor %||% 14; ce <- h$ceiling %||% 60
  dorm <- round((tr_meta$dormant_after_days  %||% 182) / 30.4)
  hist <- round((tr_meta$historic_after_days %||% 365) / 30.4)
  norm <- (tr_meta$rate %||% list())$norm_trap_days %||% 100
  ntn  <- paste0(format(norm, big.mark = ","), " trap-nights")
  # The servicing grades — shared by the By-trapline and Map help (both colour by status), led by the
  # cadence PROGRESSION graphic.
  grades <- tagList(
    .trap_status_progression(),
    P("That's an ", tags$b("order, over time"), ": as a trap is checked less often it slips ",
      tags$b("Good → Watch → Neglected"), ", then to ", tags$b("Inactive"), " once long-untouched (",
      tags$b("dormant"), " ~", dorm, " mo+ / ", tags$b("historic"), " ~", hist, " mo+ — likely paused or retired)."),
    tags$ul(
      tags$li(tags$b("Good"), " — checked at least as often as the reserve's typical (≤ its ", pg, "th percentile)."),
      tags$li(tags$b("Watch"), " — slower than typical (≤ the ", pw, "th percentile)."),
      tags$li(tags$b("Neglected"), " — slower than the ", pw, "th percentile, or unchecked all period (the worst-serviced tail).")),
    P("A trap checked just ", tags$b("once"), " is graded the same way — by its gap to the period end — so a ",
      "lone stale check is ", tags$b("Neglected"), " and a recent one reads ", tags$b("Good"),
      " (we just can't judge a cadence from one check yet)."),
    P("Grades are ", tags$b("relative to this project's own"), " checking pattern, calibrated ", tags$b("per reserve"),
      " (a remote block and a roadside block have different normal cadences). Two ", tags$b("guardrails"), ": a trap ",
      "checked every ", tags$b(paste0(fl, " d")), " or better is always good; a gap over ", tags$b(paste0(ce, " d")),
      " is always neglected."))
  if (which == "byline")
    tagList(
      P("Each trap graded on ", tags$b("how often it's been checked"), " this period (its mean gap between checks), ",
        "judged ", tags$b("as of the period end"), " — the day-to-day management view. ", tags$b("Click a line"),
        " for the traps behind it."),
      grades,
      P(tags$em("Cutoffs are percentiles (", pg, "th / ", pw, "th) of each reserve's own mean-interval spread, frozen ",
        "on the cache — they recalibrate on re-import, not as you change the Period. The resolved day values show in ",
        "the legend above. A trap's gap is measured to the period end (no peek at later checks).")))
  else if (which == "map")
    tagList(
      P("Where the traps are, coloured by servicing status — ", tags$b(tags$span(style = "color:#2e7d32", "Good")),
        " green, ", tags$b(tags$span(style = "color:#f9a825", "Watch")), " amber, ",
        tags$b(tags$span(style = "color:#c62828", "Neglected")), " red, ",
        tags$b(tags$span(style = "color:#e09595", "Inactive")), " pale red (dormant/historic). Each status ",
        "is a ", tags$b("selectable layer"), " (top-right) — turn off the ones you don't need."),
      P(tags$b("High pressure"), " — a bold ", tags$b("black ring"), " marks a ", tags$b("productive but under-checked"),
        " trap: catching on at least half its checks, yet on a watch/neglected cadence, so you're likely missing ",
        "catches between visits. The table below lists them worst-first."),
      grades,
      P(tags$em("Click any trap for its full check history.")))
  else
    tagList(
      P("Is servicing improving over time — and is catching following it? Three stacked panels across ",
        tags$b("seasons or years"), ": the ", tags$b("mix"), " of good/watch/neglected, the mean ",
        tags$b("check interval"), ", and the ", tags$b("catch rate"), " (per ", ntn, ")."),
      P("This trend deliberately ", tags$b("ignores the sidebar Period"), " — it spans all of time. Completed ",
        "periods only; an in-progress current period is omitted."),
      P(tags$em("Catch rate = catches ÷ trap-nights × ", format(norm, big.mark = ","), ", so it's comparable across ",
        "periods regardless of how much trapping was done.")))
}

#' Trapping review tab content (lives inside the Quality nav_panel). @param id Module id.
#'   @param ik_data The container (servicing-health + rate config for the help modal).
trapping_ui <- function(id, ik_data = NULL) {
  ns <- NS(id)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/trapping.css")),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),
    tags$script(src = .ik_asset("js/maps.js")),                            # leaflet resize-on-tab-show
    div(class = "ik-trapping",
        # Period banner (subtitle under the title) tracks the active tab: the By-trapline table + Map
        # honour the window, the Over-time trend spans everything (reads "All data").
        .ik_page_header("Trap check-frequency review",
            banner = div(class = "ik-page-period", uiOutput(ns("period_banner")))),
        # Two tabs keep the current-period management DETAIL apart from the cross-period TREND — the
        # table is Period-driven, the trend ignores Period, so they don't belong on one page together.
        # "By trapline" leads (default): it's the day-to-day management view and the quicker to load;
        # "Over time" is the longer-horizon trend.
        tabsetPanel(
          id = ns("trap_view"),
          tabPanel(
            "By trapline", icon = icon("table-list"),
            div(class = "trap-tab-help",
                tags$span(class = "trap-tab-help-label", "Each trap graded by how often it's checked."),
                .ik_info(ns("byline_help"), "Trap grading — how to read this",
                         trapping_help_body(ik_data$meta$trapping, "byline"))),
            div(class = "trap-controls",
                checkboxGroupInput(ns("show_extra"), "Also show (otherwise active traps only)", inline = TRUE,
                  choices = c("Dormant (9 mo+)" = "dormant", "Historic (18 mo+)" = "historic"),
                  selected = "dormant")),
            uiOutput(ns("intro")),
            div(class = "ik-trapping-scroll", uiOutput(ns("table")))),
          tabPanel(
            "Map", icon = icon("map-location-dot"),
            div(class = "trap-tab-help",
              tags$span(class = "trap-lead",
                "Traps coloured by servicing status; a ", tags$b("black ring"), " flags high pressure."),
              .ik_info(ns("map_help"), "The servicing map — how to read this",
                       trapping_help_body(ik_data$meta$trapping, "map"))),
            leaflet::leafletOutput(ns("map"), height = "55vh"),
            uiOutput(ns("pressure_note")),
            tags$h6(class = "trap-pressure-title", "Trap pressure — productive traps checked too rarely"),
            tags$p(class = "trap-lead",
              "The ", tags$b("black-ringed"), " traps above: catching on ", tags$b("at least half their checks"),
              " yet on a ", tags$b("watch / neglected"), " cadence — you're likely missing catches between ",
              "visits, so these ", tags$b("would reward more frequent checks"), ". Worst first; hover a row to ",
              "find it on the map, click for its history."),
            DT::DTOutput(ns("pressure_table"))),
          tabPanel(
            "Over time", icon = icon("chart-line"),
            div(class = "trap-timeline",
                div(class = "trap-timeline-head",
                    tags$span(class = "trap-timeline-title", "Servicing over time"),
                    radioButtons(ns("grain"), NULL, inline = TRUE,
                                 choices = c("By season" = "season", "By year" = "year"), selected = "season"),
                    .ik_info(ns("overtime_help"), "Servicing over time — how to read this",
                             trapping_help_body(ik_data$meta$trapping, "overtime"))),
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
    # Tab-aware period banner: the Over-time trend ignores the window, so it reads "All data".
    output$period_banner <- renderUI(
      .ik_period_banner(ik_data, selection(), all_data = identical(input$trap_view, "Over time")))

    # Status cell text: cadence "X d" for good/watch/neglected; sparse → check count; else the tier name.
    .iv <- function(status, mean, n) {
      if (is.na(status))                        "—"
      else if (status %in% c("dormant", "historic")) tools::toTitleCase(status)
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
      st <- table(factor(per$status, c("good", "watch", "neglected", "dormant", "historic")))
      # Legend cutoffs scoped to the RESERVE(s) in view — each reserve is calibrated against its own
      # cadence spread, so filtering to a reserve shows that reserve's exact cutoff (falls back to the
      # in-view dataset blend when a reserve has no own calibration).
      h  <- ik_trap_health_cutoffs(ik_data, reserves = unique(per$reserve), datasets = unique(per$dataset))
      gm <- round(max(h$good_max  %||% TRAP_GOOD_INTERVAL_DAYS, h$floor   %||% 0))
      wm <- round(min(h$watch_max %||% TRAP_WATCH_INTERVAL_DAYS, h$ceiling %||% Inf))
      dm <- round((ik_data$meta$trapping$dormant_after_days  %||% 182) / 30.4)   # dormant/historic windows
      hm <- round((ik_data$meta$trapping$historic_after_days %||% 365) / 30.4)   # (months), from config
      leg <- function(cls, lab, n) tags$span(class = "trap-legend-item",
        tags$span(class = paste0("trap-swatch trap-", cls)), sprintf("%s (%d)", lab, n))
      legs <- list(
        leg("good", sprintf("Checked ≤%dd", gm), st[["good"]]),
        leg("watch", sprintf("%d–%dd", gm, wm), st[["watch"]]),
        leg("neglected", sprintf("Neglected >%dd", wm), st[["neglected"]]))
      if (st[["dormant"]]  > 0)          legs <- c(legs, list(leg("dormant",  sprintf("Dormant %d mo+",  dm), st[["dormant"]])))
      if (st[["historic"]] > 0)          legs <- c(legs, list(leg("historic", sprintf("Historic %d mo+", hm), st[["historic"]])))
      # Just the key now — a one-line count + the colour codes. The grading explanation moved to the
      # tab's (?) help so the top of the table stays lean.
      tagList(
        tags$p(class = "trap-lead", sprintf(
          "%s traps · %s checks · %s captures this period — graded as of the period end. Click a line for its traps.",
          .ov_num(nrow(per)), .ov_num(sum(per$n_checks)), .ov_num(sum(per$captures)))),
        tags$div(class = "trap-legend", legs)
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
            tags$td(if (isTRUE(ch$is_first[i])) tags$em("—") else sprintf("%d d", ch$interval_days[i])),
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

    # ============================ Map tab: spatial servicing + "trap pressure" ============================
    # Most traps stay grey; only Watch + Neglected get colour, and each status is its own selectable
    # layer. A HIGH-PRESSURE trap — productive (catches on ≥ PRESSURE_PCT % of checks) yet under-checked
    # (watch/neglected cadence) — carries a purple ring: you're likely missing catches, so check it more.
    PRESSURE_PCT <- 50
    .PRESS_RING  <- "#000000"                                  # black ring — reads clearly over the status colours
    TRAP_RAD     <- 6                                          # trap dot radius (bigger than the old 4, easier to see/click)
    INACTIVE_COL <- "#e09595"                                  # pale red: dormant/historic = faded from neglected, still flagged (not benign grey)
    map_traps <- reactive({
      per <- review()$all; if (is.null(per) || !nrow(per)) return(NULL)   # ALL traps — the map has its own layer toggles
      locs <- ik_data$app$geography$locations; gi <- match(per$location, locs$location_id)
      per$latitude  <- locs$latitude[gi]; per$longitude <- locs$longitude[gi]
      per$catch_pct <- ifelse(is.na(per$n_checks) | per$n_checks == 0, 0, 100 * per$captures / per$n_checks)
      per$high_pressure <- per$status %in% c("watch", "neglected") & per$catch_pct >= PRESSURE_PCT & per$n_checks >= 2
      per$pressure_score <- ifelse(per$high_pressure, (per$catch_pct / 100) *
                                   ifelse(is.na(per$mean_interval_days), 0, per$mean_interval_days), 0)
      # one selectable layer per status — the cadence progression: Good (green) · Watch (amber) ·
      # Neglected (red) · Inactive (pale red: dormant/historic). Colours = central .MAPS_STATUS.
      per$grp <- ifelse(per$status == "neglected", "Neglected",
                 ifelse(per$status == "watch",     "Watch",
                 ifelse(per$status == "good",      "Good", "Inactive")))
      per
    })

    mproxy <- function() leaflet::leafletProxy("map", session)
    output$map <- leaflet::renderLeaflet({
      locs <- ik_data$app$geography$locations
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
      m <- leaflet::addProviderTiles(m, canvas, group = "Map")
      m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
      m <- leaflet::addMapPane(m, "traps", zIndex = 410)
      m <- leaflet::addMapPane(m, "highlight", zIndex = 450)
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
             overlayGroups = c("Good", "Watch", "Neglected", "Inactive"),  # status = selectable layer
             options = leaflet::layersControlOptions(collapsed = FALSE))
      if (nrow(locs)) m <- leaflet::fitBounds(m, min(locs$longitude), min(locs$latitude), max(locs$longitude), max(locs$latitude))
      m
    })
    outputOptions(output, "map", suspendWhenHidden = FALSE)   # render from load so proxy layers land
    observeEvent(color_mode(), {
      leaflet::addProviderTiles(leaflet::clearGroup(mproxy(), "Map"),
        if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron, group = "Map")
    }, ignoreInit = TRUE)

    observe({                                                  # draw the selectable status layers
      req(identical(input$trap_view, "Map"))                   # only when the Map tab is visible, so the
      p <- mproxy()                                            # fit/redraw happen at real size (not 0×0 while
      for (g in c("Good", "Watch", "Neglected", "Inactive", "PressHighlight"))
        leaflet::clearGroup(p, g)                              # on another tab — which left it stale + wrong-zoomed)
      leaflet::clearControls(p)
      d <- map_traps(); req(!is.null(d))
      d <- d[is.finite(d$latitude) & is.finite(d$longitude), , drop = FALSE]; if (!nrow(d)) return()
      scol <- if (is_dark()) .MAPS_STATUS$dark else .MAPS_STATUS$light   # central servicing palette (maps.R)
      grp_col <- c("Good" = unname(scol["good"]), "Watch" = unname(scol["watch"]),
                   "Neglected" = unname(scol["neglected"]), "Inactive" = INACTIVE_COL)
      for (g in names(grp_col)) {
        dd <- d[d$grp == g, , drop = FALSE]; if (!nrow(dd)) next
        leaflet::addCircleMarkers(p, data = dd, lng = ~longitude, lat = ~latitude, group = g,
          layerId = paste0("T|", dd$location), radius = TRAP_RAD, fillColor = grp_col[[g]],
          fillOpacity = if (g == "Good") 0.6 else if (g == "Inactive") 0.8 else 0.9,
          stroke = TRUE, color = "#ffffff", weight = 1,
          label = lapply(sprintf("<b>%s</b><br>%s &middot; %s checks &middot; %s caught<br>%s%s · click for history",
            dd$name, tools::toTitleCase(gsub("_", " ", ifelse(is.na(dd$status), "—", dd$status))),
            as.integer(ifelse(is.na(dd$n_checks), 0L, dd$n_checks)), as.integer(dd$captures),
            ifelse(is.finite(dd$mean_interval_days), sprintf("~%.0f d gap", dd$mean_interval_days), ""),
            ifelse(dd$high_pressure, sprintf(" &middot; %d%% catch — high pressure", round(dd$catch_pct)), "")), htmltools::HTML),
          options = leaflet::pathOptions(pane = "traps"))
        # high-pressure traps (productive but under-checked): a thin black ring just outside the dot
        # (small padding), non-interactive so the dot beneath stays clickable. Same group → toggles with it.
        hp <- dd[dd$high_pressure, , drop = FALSE]
        if (nrow(hp)) leaflet::addCircleMarkers(p, data = hp, lng = ~longitude, lat = ~latitude, group = g,
          radius = TRAP_RAD + 2, fill = FALSE, stroke = TRUE, color = .PRESS_RING, weight = 2.5, opacity = 0.95,
          options = leaflet::pathOptions(pane = "traps", interactive = FALSE))
      }
      leaflet::addLegend(p, "bottomright",
        colors = c(unname(scol["good"]), unname(scol["watch"]), unname(scol["neglected"]), INACTIVE_COL,
                   .PRESS_RING),
        labels = c("Good", "Watch", "Neglected", "Inactive (dormant/historic)", "High pressure (ring)"),
        title = "Servicing", opacity = 0.9)
      leaflet::fitBounds(p, min(d$longitude), min(d$latitude), max(d$longitude), max(d$latitude), options = list(padding = c(25, 25)))
    })

    # ---- "trap pressure" flag table — hover to locate, click for history ----
    pressure_shown <- reactiveVal(NULL)
    output$pressure_note <- renderUI({
      d <- map_traps(); if (is.null(d)) return(NULL)
      wn <- sum(d$status %in% c("watch", "neglected")); hp <- sum(d$high_pressure)
      nomap <- sum(d$high_pressure & (!is.finite(d$latitude) | !is.finite(d$longitude)))
      tags$p(class = "trap-pressure-note",
        if (hp == 0) sprintf("Of %d watch/neglected trap%s, none are high-pressure — the productive traps are being checked often enough.",
                             wn, if (wn == 1) "" else "s")
        else sprintf("Of %d watch/neglected trap%s, %s high-pressure (productive — catching on ≥%d%% of checks): the priority for more frequent checks%s.",
                     wn, if (wn == 1) "" else "s", if (hp == 1) "1 is" else paste(hp, "are"), PRESSURE_PCT,
                     if (nomap > 0) sprintf("; %d have no coordinates (table only)", nomap) else ""))
    })
    output$pressure_table <- DT::renderDT({
      d <- map_traps(); validate(need(!is.null(d), "No traps in this period."))
      t <- d[d$high_pressure, , drop = FALSE]
      validate(need(nrow(t), "No high-pressure traps — productive traps are being checked often enough."))
      t <- t[order(-t$pressure_score), , drop = FALSE]; pressure_shown(t)
      st <- c(watch = "Watch", neglected = "Neglected")
      df <- data.frame(
        Trap = t$name, Reserve = t$reserve, Line = ifelse(is.na(t$line), "—", t$line),
        `Catch %` = round(t$catch_pct), `Mean gap (d)` = round(t$mean_interval_days),
        Captures = t$captures, Checks = as.integer(t$n_checks),
        Status = unname(ifelse(t$status %in% names(st), st[t$status], t$status)),
        .loc = t$location, check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(visible = FALSE, targets = 8)),       # hide .loc (index 8)
          rowCallback = DT::JS(sprintf(
            "function(r,d){r.addEventListener('mouseenter',function(){Shiny.setInputValue('%s',d[8],{priority:'event'});});r.addEventListener('mouseleave',function(){Shiny.setInputValue('%s','',{priority:'event'});});}",
            ns("pressure_hover"), ns("pressure_hover")))))
    })
    observeEvent(input$pressure_hover, {                        # hover a row → ring that trap (by id)
      p <- mproxy(); leaflet::clearGroup(p, "PressHighlight")
      loc <- input$pressure_hover; t <- pressure_shown()
      if (is.null(loc) || !nzchar(loc) || is.null(t)) return()
      r <- t[t$location == loc, , drop = FALSE]
      if (!nrow(r) || !is.finite(r$latitude[1]) || !is.finite(r$longitude[1])) return()
      leaflet::addCircleMarkers(p, data = r[1, , drop = FALSE], lng = ~longitude, lat = ~latitude, group = "PressHighlight",
        radius = TRAP_RAD + 10, fill = FALSE, stroke = TRUE, color = "#1565c0", weight = 3,
        options = leaflet::pathOptions(pane = "highlight"))
    })
    observeEvent(input$pressure_table_rows_selected, {
      i <- input$pressure_table_rows_selected; t <- pressure_shown()
      if (length(i) && !is.null(t) && i <= nrow(t)) .open_history(t$location[i])
      DT::selectRows(DT::dataTableProxy("pressure_table"), NULL)
    })

    # ---- click a trap MARKER (or pressure row) → its full check history → Record Details ----
    th_loc <- reactiveVal(NULL); th_open <- reactiveVal(NULL)
    .open_history <- function(loc) {
      al <- ik_active_locations(ik_data); nm <- al$name[match(loc, al$location_id)]
      th_loc(loc); th_open(NULL)
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s — check history", nm %||% loc),
                                "Every check at this trap (all-time) — click one for its full record."),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = ns("th_tabs"),
          tabPanel("Trap history",   icon = icon("clock-rotate-left"), DT::dataTableOutput(ns("th_table"))),
          tabPanel("Record details", icon = icon("circle-info"),       uiOutput(ns("th_record"))))))
      hideTab(session = session, inputId = "th_tabs", target = "Record details")
    }
    observeEvent(input$map_marker_click, {
      cid <- input$map_marker_click$id
      if (!is.null(cid) && startsWith(cid, "T|")) .open_history(sub("^T\\|", "", cid))
    })
    output$th_table <- DT::renderDT({
      req(th_loc())
      ch <- ik_trap_checks(ik_data, th_loc(), NULL)             # full all-time history, newest first
      validate(need(!is.null(ch) && nrow(ch), "No checks recorded for this trap."))
      df <- data.frame(
        Date = format(ch$check_date, "%d %b %Y"),
        Interval = ifelse(ch$is_first, "—", paste0(ch$interval_days, " d")),
        Outcome = ch$outcome, Bait = ifelse(is.na(ch$bait), "—", ch$bait),
        Volunteer = ifelse(is.na(ch$volunteer), "—", ch$volunteer), ObsID = ch$observationID,
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip",
          columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))
    })
    observeEvent(input$th_table_rows_selected, {
      i <- input$th_table_rows_selected; ch <- ik_trap_checks(ik_data, th_loc(), NULL)
      if (length(i) && !is.null(ch) && i <= nrow(ch)) {
        th_open(ch$observationID[i]); showTab(session = session, inputId = "th_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("th_table"), NULL)
    })
    observeEvent(input$th_back, updateTabsetPanel(session, input$th_back$tabset, selected = input$th_back$to))
    output$th_record <- renderUI({
      if (is.null(th_open())) return(tags$p(class = "trap-lead", "Pick a check from the Trap history tab."))
      ob <- ik_observation(ik_data, th_open()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(ns("th_back"), "th_tabs", "Trap history", "Back to trap history"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = ns("th_subtabs")))
    })
  })
}
