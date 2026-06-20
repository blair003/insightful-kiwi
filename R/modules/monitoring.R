# monitoring.R — the Monitoring page: a camera deployment-review grid (location × season),
# each cell coloured by deployment health (ik_monitoring_review): green ok · red silent
# (dead camera) · amber noisy (high blanks) · grey gap (no deployment). The cell shows the
# animal-detection count (what feeds RAI); click a cell for the full deployment detail.

#' Camera deployment-review tab content (lives inside the Quality nav_panel).
#' @param id Module id.
monitoring_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/monitoring.css"),
    div(class = "ik-monitoring",
        uiOutput(ns("intro")),
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
monitoring_server <- function(id, ik_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    g  <- ik_monitoring_review(ik_data)            # static (from the cache); compute once
    by_cell <- if (!is.null(g)) split(g, paste(g$location, g$season)) else list()

    output$intro <- renderUI({
      counts <- if (is.null(g)) integer(0) else
        table(factor(g$severity, c("ok", "mild", "moderate", "serious", "none")))
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
      if (is.null(g)) return(tags$p("No camera deployments to review."))
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
              row <- by_cell[[paste(lr$location[i], s)]]
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

    # Cell click → the deployment detail for that location & season.
    observeEvent(input$cell, {
      d   <- input$cell
      row <- by_cell[[paste(d$location, d$season)]]
      if (is.null(row)) return()
      kv  <- function(k, v) {                                  # drop rows with empty text values
        if (is.character(v) && (length(v) == 0 || is.na(v) || !nzchar(v))) return(NULL)
        tags$tr(tags$td(class = "mon-k", k), tags$td(v))
      }
      fdate <- function(x) if (is.finite(as.numeric(x))) format(x, "%d %b %Y") else "—"
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s · %s", row$name, row$season),
                                paste("Deployment", row$deployment_id)),
        tags$table(class = "mon-detail",
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
          kv("Comments", row$comments)),
        easyClose = TRUE, size = "m", footer = modalButton("Close")))
    })
  })
}
