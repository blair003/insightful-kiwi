# analysis_output_module.R
#
# Non-geographic render layer for the Spatial Analysis workbench.
#
# This module consumes the SAME `dataset` reactive that spatial_map_module renders
# from (exposed via the map module's return value, or built once and shared by the
# workbench) and presents it in non-map form: the Records tables and their CSV
# export. It is the seam for future "Insights" / summaries / charts / diagnostics
# that read the same memoised analysis dataset.
#
# `dataset()` is expected to return the list produced by the map engine's analysis
# dataset reactive, including `$records`, `$cumulative_records`, and `$export_context`
# (the resolved selection / timeline state used to build the export metadata block).
#
# CSV export lives HERE (not in the engine): `prepare_map_records_export()` shapes the
# rows and `build_records_export_metadata()` formats the metadata block — both global
# helpers in table_presentation_functions.R — and the engine merely surfaces the data.

analysis_output_module_ui <- function(id,
                                      current_records = TRUE,
                                      cumulative_records = FALSE,
                                      cumulative_heading = "Cumulative records") {
  ns <- NS(id)

  tagList(
    if (isTRUE(current_records)) {
      tagList(
        # Heading (with the side label, e.g. "Primary Records") on the left, the
        # Download CSV button on the right of the same row.
        div(
          class = "analysis-records-header",
          uiOutput(ns("records_heading")),
          downloadButton(ns("download_records_csv"), "Download CSV", class = "btn-sm")
        ),
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
# `export_label` seeds the download filename so each instance (single / per
# comparison side / trapping) yields a distinctly named CSV.
analysis_output_module_server <- function(id, dataset, timeline_active = NULL,
                                          export_label = "records", records_label = NULL) {
  stopifnot(is.function(dataset))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_timeline <- function() !is.null(timeline_active) && isTRUE(timeline_active())

    output$records_heading <- renderUI({
      base <- if (is_timeline()) "Time-step records" else "Records"
      # Prefix the side label (e.g. "Primary") so comparison sub-tabs are distinct.
      heading <- if (!is.null(records_label) && nzchar(records_label)) {
        paste(records_label, base)
      } else {
        base
      }
      h3(heading, class = "analysis-records-title")
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
      records_datatable(processed_data$records)
    })

    output$cumulative_records_table <- DT::renderDataTable({
      processed_data <- req(dataset())
      records_datatable(processed_data$cumulative_records)
    })

    # CSV export: a metadata block (selection / timeline context) followed by the
    # record rows, which carry location lat/lon and (for trapping) prior_check_date.
    output$download_records_csv <- downloadHandler(
      filename = function() {
        paste0(export_label, "-export-", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        processed_data <- req(dataset())
        export_data <- prepare_map_records_export(processed_data$records)
        metadata <- build_records_export_metadata(processed_data$export_context, nrow(export_data))
        connection <- file(file, open = "w")
        on.exit(close(connection), add = TRUE)
        utils::write.csv(metadata, connection, row.names = FALSE, na = "")
        writeLines("", connection)
        utils::write.csv(export_data, connection, row.names = FALSE, na = "")
      }
    )

    invisible(NULL)
  })
}
