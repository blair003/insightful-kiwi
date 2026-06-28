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
  # DEFERRED: apply the dataset selection only on "Save & refresh", not on each checkbox toggle, so
  # the heavy analysis views recompute once on demand. Empty selection (all unticked) is ignored —
  # at least one dataset always stays on. Single-dataset projects have no toggle (shown disabled).
  observeEvent(input$apply_datasets, {
    sel <- input$active_datasets
    if (length(sel)) { active_datasets(sel); removeModal() }
  })

  # Settings modal — opened from the navbar gear icon (see ui.R).
  # Dialog is built by settings_modal() in R/functions/settings_modal.R.
  observeEvent(input$settings_btn, {
    showModal(settings_modal(
      prefer_scientific = prefer_scientific(),
      datasets = stats::setNames(ds_ids, ds_labels),   # always shown (single → disabled, can't unselect)
      active   = active_datasets()))
  })

  # Overview — the main page is the slim cross-device headline; the FULL per-device sections now live
  # on their own "Overview" page under the Monitoring / Trapping menus. Three instances of the same
  # module (own selection each); the metric reactives are bindCache'd, so identical selections share.
  overview_selection <- selection_server("overview_selection", ik_data, prefer_scientific,
    show = c("period", "compare", "reserve"), active = reactive(identical(input$nav, "overview")))
  overview_server("overview", ik_data, prefer_scientific, overview_selection,
                  sections = c("camera", "trap"), landing = TRUE,
                  network = TRUE, color_mode = reactive(input$color_mode))
  monitoring_overview_selection <- selection_server("monitoring_overview_selection", ik_data, prefer_scientific,
    show = c("period", "compare", "reserve"), active = reactive(identical(input$nav, "monitoring-overview")))
  overview_server("monitoring_overview", ik_data, prefer_scientific, monitoring_overview_selection,
                  sections = "camera")
  trapping_overview_selection <- selection_server("trapping_overview_selection", ik_data, prefer_scientific,
    show = c("period", "compare", "reserve"), active = reactive(identical(input$nav, "trapping-overview")))
  overview_server("trapping_overview", ik_data, prefer_scientific, trapping_overview_selection,
                  sections = "trap")

  # Start a feature module on its FIRST visit, not at session connect — so the landing page never waits
  # on another feature's base map/plot pre-rendering. The leaflet features use suspendWhenHidden = FALSE
  # (so proxy layers land), which would otherwise render every base map eagerly at startup; deferring
  # instantiation keeps them off the initial load. Mirrors the species pages' lazy maps.
  .lazy_once <- function(active, init) {
    started <- reactiveVal(FALSE)
    observeEvent(active(), if (isTRUE(active()) && !started()) { started(TRUE); init() }, ignoreNULL = FALSE)
  }

  # Maps — one device-locked instance per menu: a camera "Monitoring map" and a trap "Trapping map"
  # (the device toggle is gone; each lives under its device menu). Period/geography from the sidebar,
  # Measure/Species in-panel. color_mode (navbar dark/light) drives the themed basemap.
  mon_map_selection <- selection_server("mon_map_selection", ik_data, prefer_scientific,
    show = c("period", "reserve", "line"), source_type = "camera",
    active = reactive(identical(input$nav, "monitoring-map")))
  .lazy_once(reactive(identical(input$nav, "monitoring-map")), function()
    maps_server("monitoring_map", ik_data, prefer_scientific, mon_map_selection,
                color_mode = reactive(input$color_mode), device = "camera",
                active = reactive(identical(input$nav, "monitoring-map"))))
  trap_map_selection <- selection_server("trap_map_selection", ik_data, prefer_scientific,
    show = c("period", "reserve", "line"), source_type = "trap",
    active = reactive(identical(input$nav, "trapping-map")))
  .lazy_once(reactive(identical(input$nav, "trapping-map")), function()
    maps_server("trapping_map", ik_data, prefer_scientific, trap_map_selection,
                color_mode = reactive(input$color_mode), device = "trap",
                active = reactive(identical(input$nav, "trapping-map"))))

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
  highlights_server("highlights", ik_data, active = reactive(identical(input$nav, "highlights")))
  duplicates_server("duplicates", ik_data, color_mode = reactive(input$color_mode))
  trap_selection <- selection_server("trap_selection", ik_data, prefer_scientific,
    show = c("period", "reserve"), active = reactive(identical(input$nav, "trap-review")))
  .lazy_once(reactive(identical(input$nav, "trap-review")), function()
    trapping_server("trapping", ik_data, trap_selection, color_mode = reactive(input$color_mode),
                    prefer_scientific = prefer_scientific))

  # Outcomes — "are we winning?" seasonal trend + bait effectiveness (bait Period from the sidebar).
  cm <- reactive(input$color_mode)                       # navbar dark/light → themed plots
  outcomes_server("overview_trends", ik_data, prefer_scientific, color_mode = cm,   # Overview → Trends tab
                  selection = overview_selection)   # so the Trends tab honours the sidebar Reserve
  cooccurrence_selection <- selection_server("cooccurrence_selection", ik_data, prefer_scientific,
    show = c("period", "reserve"), active = reactive(identical(input$nav, "cooccurrence")))
  cooccurrence_server("cooccurrence", ik_data, prefer_scientific, color_mode = cm,
                      selection = cooccurrence_selection)
  .lazy_once(reactive(identical(input$nav, "neighbourhood")), function()
    neighbourhood_server("neighbourhood", ik_data, prefer_scientific, color_mode = cm))
  reserve_report_server("reserve_report", ik_data, prefer_scientific, color_mode = cm)
  coverage_selection <- selection_server("coverage_selection", ik_data, prefer_scientific,
    show = c("period", "reserve"), active = reactive(identical(input$nav, "coverage")))
  .lazy_once(reactive(identical(input$nav, "coverage")), function()
    coverage_server("coverage", ik_data, prefer_scientific, coverage_selection, color_mode = cm,
                    active = reactive(identical(input$nav, "coverage"))))
  predator_pressure_selection <- selection_server("predator_pressure_selection", ik_data, prefer_scientific,
    show = c("period", "reserve"), active = reactive(identical(input$nav, "predator-pressure")))
  .lazy_once(reactive(identical(input$nav, "predator-pressure")), function()
    predator_pressure_server("predator_pressure", ik_data, prefer_scientific, predator_pressure_selection,
                             color_mode = cm, active = reactive(identical(input$nav, "predator-pressure"))))
  bait_selection <- selection_server("bait_selection", ik_data, prefer_scientific,
    show = c("period"), active = reactive(identical(input$nav, "bait")))
  bait_server("bait", ik_data, prefer_scientific, color_mode = cm, selection = bait_selection)
  # Catch efficiency is now a TAB inside Check frequency (nav "trap-review"), so it shares that nav's
  # sidebar selection (period + reserve) — no separate selection of its own.
  trapping_effectiveness_server("trapping_eff", ik_data, prefer_scientific, color_mode = cm,
                                selection = trap_selection)
  trap_hero_selection <- selection_server("trap_hero_selection", ik_data, prefer_scientific,
    show = c("period", "reserve"), active = reactive(identical(input$nav, "trap-hero")))
  .lazy_once(reactive(identical(input$nav, "trap-hero")), function()
    trap_hero_server("trap_hero", ik_data, prefer_scientific, trap_hero_selection, color_mode = cm,
                     active = reactive(identical(input$nav, "trap-hero"))))
  top_trappers_server("top_trappers", ik_data, prefer_scientific,
                      active = reactive(identical(input$nav, "top-trappers")))

  # Species dashboards — one server per page (group or split sub-species), sharing one period+reserve
  # selection. Each gates on its own nav value so only the open page computes. (Module id underscores
  # the nav value, which keeps hyphens for input$nav matching.)
  species_specs <- ik_species_taxa(ik_data)
  is_species_nav <- function(nv) !is.null(nv) && (startsWith(nv, "grp-") || startsWith(nv, "sp-"))
  species_selection <- selection_server("species_selection", ik_data, prefer_scientific,
    show = c("period", "reserve"), active = reactive(is_species_nav(input$nav)))
  lapply(species_specs, function(spec) {
    species_dashboard_server(gsub("-", "_", spec$key), spec, ik_data, species_selection,
      prefer_scientific, color_mode = cm, active = reactive(identical(input$nav, spec$key)))
  })

  # Sidebar OPEN state per view. The light-control Overviews and the Species pages are deliberately
  # LEFT OUT, so the rail collapses for them by default — regular users aren't confronted with filters,
  # while their controls still live in the sidebar (power users open the rail to change
  # Period/Compare/Reserve, or click the in-page period banner's calendar), and the current window
  # stays visible via that banner. The heavy-filter pages (Maps, Records, Trap review, …) open it.
  # (The collapse toggle stays either way.)
  SIDEBAR_NAVS <- c("monitoring-map", "trapping-map", "monitoring-records", "trapping-records",
                    "trap-review", "bait", "coverage", "predator-pressure", "trap-hero", "cooccurrence",
                    "neighbourhood", "reserve-report")   # their selection/anchor lives in the rail
  # Auto-collapse the rail on the light pages / open it on the heavy ones — DESKTOP ONLY. On mobile the rail
  # is an overlay: auto-opening it on every navigation just covered the content (and you'd have to dismiss it
  # each time), so on a phone we leave it CLOSED and let you open it on demand via the period-banner date.
  # `ik_desktop` (a matchMedia flag from ui.R) gates it; re-apply on a change of the nav OR that flag, so the
  # initial state lands once the flag arrives and a resize across the breakpoint re-applies.
  observeEvent(list(input$nav, input$ik_desktop), {
    if (!isTRUE(input$ik_desktop)) return()                   # mobile / not-yet-known: leave the rail to the user
    bslib::toggle_sidebar("global_sidebar", open = input$nav %in% SIDEBAR_NAVS)
  })
  # The in-page period banner's calendar/date is a button (one global input, set from .ik_period_banner)
  # that toggles the filter sidebar — so on pages where the rail is collapsed by default, the banner is
  # the way in to change Period/Compare/Reserve.
  observeEvent(input$ik_toggle_sidebar, bslib::toggle_sidebar("global_sidebar"), ignoreInit = TRUE)
  # In-page navigation jumps (e.g. the main Overview's "All Monitoring detail →" link card) set this
  # global input with a target nav value; switch to it.
  observeEvent(input$ik_goto_nav, bslib::nav_select("nav", input$ik_goto_nav), ignoreInit = TRUE)

}
