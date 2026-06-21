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

  # Global dataset visibility (Settings show/hide). Held here (survives modal re-opens) and
  # published on session$userData so the data accessors can scope to it ambiently (ik_active_*),
  # without threading it through every module. Default: all datasets on. ignoreNULL keeps the
  # value when the modal closes (input → NULL); unchecking the LAST box is likewise ignored, so
  # at least one dataset always stays on.
  ds_ids    <- names(ik_data$datasets)
  ds_labels <- vapply(ik_data$datasets, function(d) d$meta$name %||% "", character(1))
  active_datasets <- reactiveVal(ds_ids)
  session$userData$active_datasets <- active_datasets
  observeEvent(input$active_datasets, active_datasets(input$active_datasets))

  # Settings modal — opened from the navbar gear icon (see ui.R).
  # Dialog is built by settings_modal() in R/functions/settings_modal.R.
  observeEvent(input$settings_btn, {
    showModal(settings_modal(
      prefer_scientific = prefer_scientific(),
      datasets = if (length(ds_ids) > 1) stats::setNames(ds_ids, ds_labels) else NULL,
      active   = active_datasets()))
  })

  # Overview view — its own selection instance (period/geography/device).
  overview_selection <- selection_server("overview_selection", ik_data, prefer_scientific)
  overview_server("overview", ik_data, prefer_scientific, overview_selection)

  # Maps view — its own selection instance (camera-only; species via the in-page group picker).
  # color_mode (navbar dark/light toggle) drives the themed basemap.
  maps_selection <- selection_server("maps_selection", ik_data, prefer_scientific)
  maps_server("maps", ik_data, prefer_scientific, maps_selection,
              color_mode = reactive(input$color_mode))

  # Shared data selection (sidebar) — resolves the deployment-first selection. Records gets a
  # Dataset axis (independent of the global toggle) when there's more than one dataset.
  selection <- selection_server("selection", ik_data, prefer_scientific,
    datasets = if (length(ds_ids) > 1) stats::setNames(ds_ids, ds_labels) else NULL)

  # Records view (consumes the selection).
  records_server("records", ik_data, prefer_scientific, selection)

  # Data → Quality — camera deployment review + possible-duplicate review + trapping check review.
  monitoring_server("monitoring", ik_data)
  duplicates_server("duplicates", ik_data)
  trapping_server("trapping", ik_data)

  # Outcomes — "are we winning?" seasonal trend + bait effectiveness.
  outcomes_server("outcomes", ik_data, prefer_scientific)
  bait_server("bait", ik_data, prefer_scientific)

  # Auto-hide the sidebar on views that have no sidebar controls — only Overview and Records
  # populate it; every other view keeps its controls in-page, so an empty rail is just clutter.
  # (The collapse toggle stays, so a curious user can still open it and see the note.)
  SIDEBAR_NAVS <- c("overview", "maps", "records")
  observeEvent(input$nav, {
    bslib::toggle_sidebar("global_sidebar", open = input$nav %in% SIDEBAR_NAVS)
  })

}
