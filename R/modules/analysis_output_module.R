# analysis_output_module.R
#
# Non-geographic render layer for the Spatial Analysis workbench.
#
# This module consumes the SAME `dataset` reactive that spatial_map_module renders
# from (exposed via the map module's return value, or built once and shared by the
# workbench) and presents it in non-map form. Today that means the Records tables;
# it is the seam for future "Insights" / summaries / charts / diagnostics / CSV
# export that read the same memoised analysis_dataset().
#
# `dataset()` is expected to return the list produced by the map engine's
# analysis_dataset reactive, including `$observations_for_table` and
# `$cumulative_observations_for_table`.
#
# NOTE (future): CSV export is intentionally NOT implemented here yet. The export
# *data* helper `prepare_map_records_export()` is global, but the export *metadata*
# (selection / timeline context captured at download time) is currently a local
# closure inside spatial_map_module_server (`prepare_map_records_export_metadata`).
# To add download here, the engine should surface that selection context on the
# dataset (e.g. `dataset()$export_context`) so this module can assemble metadata
# without reaching into the engine's reactive scope.

analysis_output_module_ui <- function(id,
                                      current_records = TRUE,
                                      cumulative_records = FALSE,
                                      cumulative_heading = "Cumulative records") {
  ns <- NS(id)

  tagList(
    if (isTRUE(current_records)) {
      tagList(
        uiOutput(ns("records_heading")),
        DT::dataTableOutput(ns("records_table"))
      )
    },
    if (isTRUE(cumulative_records)) {
      div(
        id = ns("cumulative_section"),
        class = "sa-cumulative-records",
        br(),
        h3(cumulative_heading),
        DT::dataTableOutput(ns("cumulative_records_table"))
      )
    }
  )
}

# `timeline_active` (optional reactive) tailors the labels: in timeline mode the
# current table is the time-progression step ("Time-step records") and the
# cumulative table is shown; otherwise it is just "Records" and cumulative hides.
analysis_output_module_server <- function(id, dataset, timeline_active = NULL) {
  stopifnot(is.function(dataset))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_timeline <- function() !is.null(timeline_active) && isTRUE(timeline_active())

    output$records_heading <- renderUI({
      h3(if (is_timeline()) "Time-step records" else "Records")
    })

    # The cumulative table only makes sense while stepping through time.
    observe({
      shinyjs::toggle(
        selector = paste0("#", ns("cumulative_section")),
        condition = is_timeline()
      )
    })

    records_datatable <- function(rows) {
      DT::datatable(
        prepare_map_records_table(rows),
        escape = FALSE,
        options = list(
          pageLength = 10,
          searching = TRUE,
          lengthChange = TRUE,
          order = list(list(3, "asc"))
        ),
        class = "display",
        rownames = FALSE
      )
    }

    output$records_table <- DT::renderDataTable({
      processed_data <- req(dataset())
      records_datatable(processed_data$observations_for_table)
    })

    output$cumulative_records_table <- DT::renderDataTable({
      processed_data <- req(dataset())
      records_datatable(processed_data$cumulative_observations_for_table)
    })

    invisible(NULL)
  })
}
