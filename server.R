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
  overview_selection <- selection_server("overview_selection", ik_data, prefer_scientific,
    show = c("period", "compare", "reserve"), active = reactive(identical(input$nav, "overview")))
  overview_server("overview", ik_data, prefer_scientific, overview_selection)

  # Maps — one device-locked instance per menu: a camera "Monitoring map" and a trap "Trapping map"
  # (the device toggle is gone; each lives under its device menu). Period/geography from the sidebar,
  # Measure/Species in-panel. color_mode (navbar dark/light) drives the themed basemap.
  mon_map_selection <- selection_server("mon_map_selection", ik_data, prefer_scientific,
    show = c("period", "reserve", "line", "location"), active = reactive(identical(input$nav, "monitoring-map")))
  maps_server("monitoring_map", ik_data, prefer_scientific, mon_map_selection,
              color_mode = reactive(input$color_mode), device = "camera",
              active = reactive(identical(input$nav, "monitoring-map")))
  trap_map_selection <- selection_server("trap_map_selection", ik_data, prefer_scientific,
    show = c("period", "reserve", "line", "location"), active = reactive(identical(input$nav, "trapping-map")))
  maps_server("trapping_map", ik_data, prefer_scientific, trap_map_selection,
              color_mode = reactive(input$color_mode), device = "trap",
              active = reactive(identical(input$nav, "trapping-map")))

  # Records — one per device menu, each with the Device filter DEFAULTED to that device (still
  # changeable). Each gets a Dataset axis (independent of the global toggle) when >1 dataset.
  .rec_datasets <- if (length(ds_ids) > 1) stats::setNames(ds_ids, ds_labels) else NULL
  .rec_show <- c("dataset", "period", "reserve", "line", "location", "device", "species", "net")
  mon_records_selection     <- selection_server("mon_records_selection",     ik_data, prefer_scientific,
    datasets = .rec_datasets, show = .rec_show, active = reactive(identical(input$nav, "monitoring-records")))
  control_records_selection <- selection_server("control_records_selection", ik_data, prefer_scientific,
    datasets = .rec_datasets, show = .rec_show, active = reactive(identical(input$nav, "trapping-records")))
  records_server("mon_records",     ik_data, prefer_scientific, mon_records_selection)
  records_server("control_records", ik_data, prefer_scientific, control_records_selection)

  # Data → Quality — camera deployment review + possible-duplicate review + trapping check review.
  # Trap review takes Period + Reserve from the sidebar (its own selection instance).
  monitoring_server("monitoring", ik_data, prefer_scientific)
  duplicates_server("duplicates", ik_data, color_mode = reactive(input$color_mode))
  trap_selection <- selection_server("trap_selection", ik_data, prefer_scientific,
    show = c("period", "reserve"), active = reactive(identical(input$nav, "trap-review")))
  trapping_server("trapping", ik_data, trap_selection, color_mode = reactive(input$color_mode),
                  prefer_scientific = prefer_scientific)

  # Outcomes — "are we winning?" seasonal trend + bait effectiveness (bait Period from the sidebar).
  cm <- reactive(input$color_mode)                       # navbar dark/light → themed plots
  outcomes_server("outcomes", ik_data, prefer_scientific, color_mode = cm)
  cooccurrence_server("cooccurrence", ik_data, prefer_scientific, color_mode = cm)
  neighbourhood_server("neighbourhood", ik_data, prefer_scientific, color_mode = cm)
  coverage_selection <- selection_server("coverage_selection", ik_data, prefer_scientific,
    show = c("period", "reserve"), active = reactive(identical(input$nav, "coverage")))
  coverage_server("coverage", ik_data, prefer_scientific, coverage_selection, color_mode = cm,
                  active = reactive(identical(input$nav, "coverage")))
  bait_selection <- selection_server("bait_selection", ik_data, prefer_scientific,
    show = c("period"), active = reactive(identical(input$nav, "bait")))
  bait_server("bait", ik_data, prefer_scientific, color_mode = cm, selection = bait_selection)
  trapping_effectiveness_server("trapping_eff", ik_data, prefer_scientific, color_mode = cm)

  # Auto-hide the sidebar on views that have no sidebar controls — only Overview and Records
  # populate it; every other view keeps its controls in-page, so an empty rail is just clutter.
  # (The collapse toggle stays, so a curious user can still open it and see the note.)
  SIDEBAR_NAVS <- c("overview", "monitoring-map", "trapping-map", "monitoring-records", "trapping-records",
                    "trap-review", "bait", "coverage")
  observeEvent(input$nav, {
    bslib::toggle_sidebar("global_sidebar", open = input$nav %in% SIDEBAR_NAVS)
  })

}
