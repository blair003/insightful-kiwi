# settings_modal.R
#
# Builds the Settings dialog opened from the navbar gear icon (see ui.R/server.R).
# Keep all Settings UI here so server.R stays thin and settings options have a
# single home as they grow. Server owns only the open/close wiring.

#' Build the Settings modal dialog.
#'
#' Appearance (dark/light mode) lives in the navbar via input_dark_mode(), not
#' here — see ui.R for why. Add future settings sections to this dialog.
#'
#' @param prefer_scientific Current value of the name preference, used to seed the
#'   switch. The modal is rebuilt on each open, so the live value must be passed in
#'   (held server-side) or the switch resets — see server.R.
#' @return A [shiny::modalDialog()] for use with [shiny::showModal()].
settings_modal <- function(prefer_scientific = FALSE) {
  modalDialog(
    title = "Settings",

    # Species names — vernacular (common) names by default; switch on to show
    # scientific names instead. Read via input$prefer_scientific and passed to
    # ik_species_label(prefer = ...).
    input_switch("prefer_scientific", "Prefer scientific names", value = isTRUE(prefer_scientific)),
    tags$small(
      class = "text-muted",
      "Show scientific names instead of common (vernacular) names where available."
    ),

    easyClose = TRUE,
    size = "l",
    footer = modalButton("Close")
  )
}
