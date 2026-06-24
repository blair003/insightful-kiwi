# monitoring.R — the Monitoring page: a camera deployment-review grid (location × season),
# each cell coloured by deployment health (ik_monitoring_review): green ok · red silent
# (dead camera) · amber noisy (high blanks) · grey gap (no deployment). The cell shows the
# animal-detection count (what feeds RAI); click a cell for the full deployment detail.

#' "How to read this" help body for Camera deployment review — tabbed. @keywords internal
monitoring_help_body <- function() {
  P <- function(...) tags$p(...)
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "A grid with one cell ", tags$b("per camera location × season"), " (most recent first). ",
        "The number in a cell is its ", tags$b("animal detections"), " — the same data that feeds RAI — and the ",
        tags$b("colour"), " is the deployment's ", tags$b("health"), ", so you can tell a genuine quiet patch ",
        "from a camera that wasn't really working."),
      P("It's a ", tags$b("data-quality"), " check: a dead or misfiring camera makes RAI lie (a real hotspot ",
        "reads as empty, or blanks swamp the rate). Spot those here before you trust the activity numbers. ",
        "Click a cell for the specific deployment's detail.")),
    tabPanel(
      "The grades", icon = icon("traffic-light"),
      tags$ul(
        tags$br(),
        tags$li(tags$b(tags$span(style = "color:#2e7d32", "OK")), " — healthy: a normal mix of triggers."),
        tags$li(tags$b("Watch"), " — few triggers / elevated blanks; keep an eye on it."),
        tags$li(tags$b("Concern"), " — mostly blanks; the camera may be misaimed, obscured or misfiring."),
        tags$li(tags$b("Problem"), " — almost no triggers for the effort: likely a ", tags$b("dead camera"),
                " (flat battery, failure) — treat its zero as unknown, not a true absence."),
        tags$li(tags$b("No deployment"), " — grey: no camera ran on that line that season (a coverage gap, ",
                "not a quality problem).")),
      P("Use the ", tags$b("Highlight"), " toggles to focus on just the grades you're chasing.")),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Per deployment"), " — each camera deployment in a season is graded from its balance of ",
                tags$b("animal triggers vs blanks"), " for the effort it ran: plenty of triggers → OK; almost ",
                "none → Problem; mostly blanks → Concern."),
        tags$li(tags$b("Cell value"), " — the animal-detection count (what RAI is built from), so the grid ",
                "doubles as a coverage map of where the detections came from."),
        tags$li(tags$b("Why it matters"), " — RAI divides detections by effort; a dead camera still accrues ",
                "effort but no detections, dragging the rate down, so flagging it keeps RAI honest.")),
      P(tags$em("A grade is about the camera working, not about the place being empty — a true quiet site reads ",
                "OK with a low count, a dead camera reads Problem.")))
  )
}

#' Camera deployment-review tab content (lives inside the Quality nav_panel).
#' @param id Module id.
monitoring_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/monitoring.css")),
    div(class = "ik-monitoring",
        div(class = "ik-review-headrow",
            tags$h5("Camera deployment review", class = "ik-review-head"),
            .ik_info(ns("mon_help"), "Camera review — how to read this", monitoring_help_body())),
        uiOutput(ns("intro")),
        div(class = "mon-range",
            radioButtons(ns("range"), "Seasons shown", inline = TRUE,
              choices = c("Last 1 year" = 4, "Last 2 years" = 8, "All" = 0), selected = 4)),
        div(class = "mon-highlight",
            checkboxGroupInput(ns("highlight"), "Highlight", inline = TRUE,
              choices  = c("OK" = "ok", "Watch" = "mild", "Concern" = "moderate",
                           "Problem" = "serious", "No deployment" = "none"),
              selected = c("ok", "mild", "moderate", "serious", "none"))),
        div(class = "ik-monitoring-scroll", uiOutput(ns("grid"))))
  )
}

#' One coloured grid cell for a (location, season) review row. @keywords internal
.mon_cell <- function(row, ns, dim = FALSE) {
  st  <- row$severity
  dimc <- if (dim) " mon-dim" else ""
  if (identical(st, "none")) return(tags$td(class = paste0("mon-cell mon-none", dimc), ""))
  tags$td(
    class = paste0("mon-cell mon-click mon-", st, dimc),
    title = sprintf("%s · %s — %s. %s animal / %s events.",
                    row$name, row$season, row$issue, .ov_num(row$n_animal), .ov_num(row$n_events)),
    onclick = sprintf("Shiny.setInputValue('%s',{location:'%s',season:'%s'},{priority:'event'})",
                      ns("cell"), row$location, row$season),
    .ov_num(row$n_animal))
}

#' Monitoring server.
#' @param id      Module id.
#' @param ik_data The ik_data container.
#' @param prefer_scientific Reactive returning TRUE to label species scientifically (record drill).
monitoring_server <- function(id, ik_data, prefer_scientific = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Precomputed at build for ALL datasets — scope to the ACTIVE datasets (by location) so the grid
    # hides cells from hidden datasets, reactive on the Settings toggle. (Was a static all-datasets snapshot.)
    g <- reactive({
      gg <- ik_data$app$monitoring_review
      if (is.null(gg) || !nrow(gg)) return(gg)
      gg[gg$location %in% ik_active_locations(ik_data)$location_id, , drop = FALSE]
    })
    # The view = active grid capped to the most-recent N seasons (1 yr = 4, 2 yr = 8, 0 = all). The
    # whole UI (legend, grid, clicks) reads gv() so the legend counts match the columns on show.
    gv <- reactive({
      gg <- g(); if (is.null(gg) || !nrow(gg)) return(gg)
      n  <- suppressWarnings(as.integer(input$range %||% 4L))
      if (is.na(n) || n <= 0) return(gg)                       # "All"
      keep <- utils::head(rev(unique(gg[order(gg$season_order), "season"])), n)  # most recent first
      gg[gg$season %in% keep, , drop = FALSE]
    })
    by_cell <- reactive(if (!is.null(gv()) && nrow(gv())) split(gv(), paste(gv()$location, gv()$season)) else list())
    prefer   <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    cell_obs <- reactiveVal(NULL)   # animal detections behind the clicked cell's count
    rec_obs  <- reactiveVal(NULL)   # observationID open in the Record details tab

    output$intro <- renderUI({
      counts <- if (is.null(gv())) integer(0) else
        table(factor(gv()$severity, c("ok", "mild", "moderate", "serious", "none")))
      legend <- function(cls, lab, n) tags$span(class = "mon-legend-item",
        tags$span(class = paste0("mon-swatch mon-", cls)), sprintf("%s (%d)", lab, n))
      tagList(
        tags$p(class = "mon-lead",
          "Camera deployment review — one cell per location and season (most recent first), ",
          "showing animal detections. Colour = severity; click a cell for the specific issue."),
        tags$div(class = "mon-legend",
          legend("ok", "OK", counts[["ok"]] %||% 0),
          legend("mild", "Watch (few triggers / elevated blanks)", counts[["mild"]] %||% 0),
          legend("moderate", "Concern (mostly blanks)", counts[["moderate"]] %||% 0),
          legend("serious", "Problem (almost no triggers)", counts[["serious"]] %||% 0),
          legend("none", "No deployment", counts[["none"]] %||% 0))
      )
    })

    output$grid <- renderUI({
      g <- gv()
      if (is.null(g) || !nrow(g)) return(tags$p("No camera deployments to review."))
      bc   <- by_cell()
      hl   <- input$highlight                                   # severities to keep bright
      seas <- rev(unique(g[order(g$season_order), "season"]))   # most recent first (left)
      loc  <- unique(g[, c("location", "name", "reserve", "line")])
      loc  <- loc[order(loc$reserve, suppressWarnings(as.numeric(loc$line)), loc$name), , drop = FALSE]

      header <- tags$tr(tags$th(""), lapply(seas, function(s) tags$th(s)))
      body <- lapply(split(loc, factor(loc$reserve, levels = unique(loc$reserve))), function(lr) {
        res_row <- tags$tr(class = "mon-reserve-row",
                           tags$td(lr$reserve[1], colspan = length(seas) + 1))
        loc_rows <- lapply(seq_len(nrow(lr)), function(i) {
          tags$tr(
            tags$td(class = "mon-loc", lr$name[i]),
            lapply(seas, function(s) {
              row <- bc[[paste(lr$location[i], s)]]
              sv  <- if (is.null(row)) "none" else row$severity
              dim <- !is.null(hl) && !(sv %in% hl)
              if (is.null(row)) tags$td(class = paste0("mon-cell mon-none", if (dim) " mon-dim"), "")
              else .mon_cell(row, ns, dim)
            }))
        })
        tagList(res_row, loc_rows)
      })
      tags$table(class = "ik-monitoring-table",
                 tags$thead(header), tags$tbody(body))
    })

    # Cell click → a tabbed modal: the deployment detail, the animal detections behind the cell's
    # count, and (on row click) the full record viewer — the standard count → Records → Record
    # details drill, each stage with a back link.
    observeEvent(input$cell, {
      d   <- input$cell
      row <- by_cell()[[paste(d$location, d$season)]]
      if (is.null(row)) return()
      kv  <- function(k, v) {                                  # drop rows with empty text values
        if (is.character(v) && (length(v) == 0 || is.na(v) || !nzchar(v))) return(NULL)
        tags$tr(tags$td(class = "mon-k", k), tags$td(v))
      }
      fdate <- function(x) if (is.finite(as.numeric(x))) format(x, "%d %b %Y") else "—"
      detail <- tags$table(class = "mon-detail",
        kv("Reserve", row$reserve),
        kv("Line", row$line),
        kv("Issue", tags$span(class = paste0("mon-badge mon-", row$severity), row$issue)),
        kv("Deployed", sprintf("%s – %s", fdate(row$dep_start), fdate(row$dep_end))),
        kv("Deployments", .ov_num(row$n_deployments)),
        kv("Effort", sprintf("%s camera-days (%s hrs)",
                             .ov_num(round(row$effort_hours / 24)), .ov_num(round(row$effort_hours)))),
        kv("Triggers (events)", .ov_num(row$n_events)),
        kv("Animal detections", .ov_num(row$n_animal)),
        kv("Blanks", sprintf("%s (%.0f%%)", .ov_num(row$n_blank), 100 * (row$blank_frac %||% 0))),
        kv("Triggers / day", sprintf("%.1f", row$events_per_day %||% 0)),
        kv("Last trigger", fdate(row$last_trigger)),
        kv("Set up by", row$setup_by),
        kv("Comments", row$comments))
      # the records behind the cell number = this location's animal detections that season
      o <- ik_select(ik_data, season = row$season, location = row$location, source_type = "camera")$observations
      o <- o[!is.na(o$observationType) & o$observationType == "animal", , drop = FALSE]
      o <- o[order(o$eventStart, decreasing = TRUE), , drop = FALSE]
      cell_obs(o); rec_obs(NULL)
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s · %s", row$name, row$season),
                                paste("Deployment", row$deployment_id)),
        tabsetPanel(id = ns("cell_tabs"),
          tabPanel("Deployment",     icon = icon("video"),       detail),
          tabPanel("Records",        icon = icon("list"),        DT::dataTableOutput(ns("cell_table"))),
          tabPanel("Record details", icon = icon("circle-info"), uiOutput(ns("cell_record")))),
        easyClose = TRUE, size = "l", footer = modalButton("Close")))
      hideTab(session = session, inputId = "cell_tabs", target = "Record details")   # appears on row click
    })

    output$cell_table <- DT::renderDT({
      o <- cell_obs(); validate(need(!is.null(o) && nrow(o), "No animal detections in this deployment."))
      p <- prefer()
      has_t    <- format(o$eventStart, "%H:%M:%S") != "00:00:00"
      when_lab <- ifelse(has_t, format(o$eventStart, "%Y-%m-%d %H:%M"), format(o$eventStart, "%Y-%m-%d"))
      df <- data.frame(When = when_lab, Species = ik_species_label(o$scientificName, ik_data, p),
        Count = o$count, .obs = o$observationID, check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1L))))
    })
    observeEvent(input$cell_table_rows_selected, {
      i <- input$cell_table_rows_selected; o <- cell_obs()
      if (length(i) && !is.null(o) && i <= nrow(o)) {
        rec_obs(o$observationID[i]); showTab(session = session, inputId = "cell_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("cell_table"), NULL)
    })
    observeEvent(input$tab_back, updateTabsetPanel(session, input$tab_back$tabset, selected = input$tab_back$to))
    output$cell_record <- renderUI({
      if (is.null(rec_obs())) return(tags$p(class = "mon-lead", "Pick a record from the Records tab."))
      ob <- ik_observation(ik_data, rec_obs()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(ns("tab_back"), "cell_tabs", "Records", "Back to records"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = ns("cell_subtabs")))
    })
  })
}
