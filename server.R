# server.R

server <- function(input, output, session) {

  # Name display preference (Settings). Held here so it survives modal re-opens:
  # the modal is rebuilt each time, so the switch is seeded from this value and
  # writes back to it. ignoreNULL (observeEvent default) keeps the value when the
  # modal closes and input$prefer_scientific goes NULL.
  prefer_scientific <- reactiveVal(FALSE)
  observeEvent(input$prefer_scientific, {
    prefer_scientific(isTRUE(input$prefer_scientific))
  })

  # Settings modal — opened from the navbar gear icon (see ui.R).
  # Dialog is built by settings_modal() in R/functions/settings_modal.R.
  observeEvent(input$settings_btn, {
    showModal(settings_modal(prefer_scientific = prefer_scientific()))
  })

  # Records view.
  records_server("records", ik_data, prefer_scientific)

}
