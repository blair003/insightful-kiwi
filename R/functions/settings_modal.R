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
#' @param datasets Named id→label vector of datasets to offer as show/hide checkboxes. With several,
#'   the section shows tick-boxes + a Save button (applied only on Save — not on each toggle). With
#'   exactly one it's shown DISABLED (so it's visible but can't be turned off). NULL → section omitted.
#'   @param active Currently-shown dataset ids.
#' @return A [shiny::modalDialog()] for use with [shiny::showModal()].
settings_modal <- function(prefer_scientific = FALSE, datasets = NULL, active = NULL) {
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

    # Datasets — show/hide which data sources feed the app. With several, the choice is DEFERRED:
    # tick boxes then "Save & refresh" (so the heavy analysis views recompute once, on demand, not
    # on every toggle). With exactly one, it's shown DISABLED so it's visible but can't be turned off.
    # Records keeps its own dataset filter, so this is for the analysis views.
    if (!is.null(datasets) && length(datasets) >= 1) tagList(
      tags$hr(),
      tags$label("Datasets", class = "fw-semibold"),
      if (length(datasets) == 1)
        div(class = "form-check",
            tags$input(type = "checkbox", class = "form-check-input", id = "ds_only",
                       checked = NA, disabled = NA),
            tags$label(class = "form-check-label text-muted", `for` = "ds_only", unname(datasets)[1]),
            tags$small(class = "d-block text-muted", "The only dataset loaded — always on."))
      else tagList(
        checkboxGroupInput("active_datasets", NULL, choices = datasets,
                           selected = active %||% unname(datasets)),
        actionButton("apply_datasets", tagList(icon("rotate"), " Save & refresh"),
                     class = "btn-primary btn-sm"),
        tags$small(class = "d-block text-muted mt-2",
                   "Tick the data sources to include, then ", tags$b("Save & refresh"), " — the Overview ",
                   "and analysis views update only when you save. At least one must stay on. ",
                   "Records has its own filter.")
      )
    ),

    easyClose = TRUE,
    size = "l",
    footer = modalButton("Close")
  )
}
