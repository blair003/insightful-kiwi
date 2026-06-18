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
#' @return A [shiny::modalDialog()] for use with [shiny::showModal()].
settings_modal <- function() {
  modalDialog(
    title = "Settings",
    "Settings will go here.",
    easyClose = TRUE,
    size = "l",
    footer = modalButton("Close")
  )
}
