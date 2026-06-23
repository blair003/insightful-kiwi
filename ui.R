# ui.R

# Per-view Period defaults for the standalone reviews (now sidebar-driven, like every other view):
# Trap review → the latest near-full TRAP season; Bait → the latest whole YEAR (more data, rarer baits).
.trap_period_def <- { s <- .default_trap_season(ik_data); if (!is.null(s)) paste0("season:", s) else "all" }
.bait_period_def <- { pc <- ik_period_choices(ik_data); if (length(pc) >= 2) unname(pc[[2]][1]) else "all" }

# Device-organised menus are shown only when the org actually has that kind of data, so a
# trapping-only (or monitoring-only) group sees just its menu (bslib drops the NULL nav items).
.has_camera <- any(vapply(ik_data$datasets, function(d) identical(d$meta$source_type, "camera"), logical(1)))
.has_trap   <- any(vapply(ik_data$datasets, function(d) identical(d$meta$source_type, "trap"),   logical(1)))

ui <- page_navbar(
  id = "nav",
  title = tags$img(
    src = "images/insightful-kiwi.logo.svg", height = "32",
    alt = "Insightful Kiwi", title = "Insightful Kiwi"
  ),
  window_title = "Insightful Kiwi",
  theme = bs_theme(version = 5, font_scale = 0.9),
  fillable = FALSE,

  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/base.css")),
    tags$link(rel = "icon", href = "images/insightful-kiwi.logo.svg", type = "image/svg+xml"),
    tags$link(rel = "icon", href = "images/icons/favicon.png", type = "image/png", sizes = "32x32"),
    # full-screen sequence viewer for the observation Photos burst (delegated clicks → works in modals)
    tags$script(src = .ik_asset("js/ovw-lightbox.js"))
  ),

  # Global sidebar — filters/controls for the active view.
  # Populated per-view as modules are built; conditional on the selected nav.
  sidebar = sidebar(
    id = "global_sidebar",
    title = "Data Selection",
    conditionalPanel("input.nav === 'overview'",
                     selection_ui("overview_selection",
                                  show = c("period", "compare", "reserve"), ik_data = ik_data)),
    conditionalPanel("input.nav === 'outcomes'", tags$small("All seasons · network mean across reserves.")),
    conditionalPanel("input.nav === 'bait'",
                     selection_ui("bait_selection", show = c("period"), ik_data = ik_data,
                                  period_default = .bait_period_def)),
    conditionalPanel("input.nav === 'trap-review'",
                     # Period drives only the "By trapline" tab; the "Over time" trend ignores it, so the
                     # Period control hides on that tab. Reserve stays on both (the trend is reserve-scoped).
                     conditionalPanel("input['trapping-trap_view'] !== 'Over time'",
                       selection_ui("trap_selection", show = c("period"), ik_data = ik_data,
                                    period_default = .trap_period_def)),
                     selection_ui("trap_selection", show = c("reserve"), ik_data = ik_data)),
    conditionalPanel("input.nav === 'coverage'",
                     selection_ui("coverage_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = "all")),
    conditionalPanel("input.nav === 'camera-review' || input.nav === 'duplicates'",
                     tags$small("Controls are within the view.")),
    conditionalPanel("input.nav === 'monitoring-map'",
                     selection_ui("mon_map_selection",
                                  show = c("period", "reserve", "line", "location"), ik_data = ik_data)),
    conditionalPanel("input.nav === 'trapping-map'",
                     selection_ui("trap_map_selection",
                                  show = c("period", "reserve", "line", "location"), ik_data = ik_data)),
    conditionalPanel("input.nav === 'monitoring-records'",
                     selection_ui("mon_records_selection",
                                  show = c("dataset", "period", "reserve", "line", "location",
                                           "device", "species", "net"), ik_data = ik_data, device_default = "camera")),
    conditionalPanel("input.nav === 'trapping-records'",
                     selection_ui("control_records_selection",
                                  show = c("dataset", "period", "reserve", "line", "location",
                                           "device", "species", "net"), ik_data = ik_data, device_default = "trap")),
    tags$div(class = "ik-sidebar-foot",
             tags$em("Insightful Kiwi"), tags$br(),
             tags$a(href = "mailto:blair@aketechnology.co.nz?subject=Insightful%20Kiwi%20Query",
                    "Blair George"))
  ),

  # Centre the menu: equal flexible space on both sides of the nav.
  nav_spacer(),

  overview_ui("overview"),

  # Menus are organised by DEVICE — Monitoring (camera/observation) and "Trapping" (predator CONTROL:
  # trapping today, poison/etc. later — friendly label now, conceptually Control). Each shown only when
  # the org has that data; Records sits in each, device-filtered (default to the device, still changeable).
  if (.has_camera) nav_menu(
    "Monitoring", icon = icon("binoculars"),
    maps_ui("monitoring_map", device = "camera", label = "Map", value = "monitoring-map", ik_data = ik_data),
    nav_panel("Camera review", value = "camera-review", icon = icon("camera"),
              monitoring_ui("monitoring")),
    cooccurrence_ui("cooccurrence"),                       # predator ↔ protected timing (camera)
    nav_panel("Duplicate window", value = "duplicates", icon = icon("clone"),
              duplicates_ui("duplicates")),
    records_ui("mon_records", label = "Records", value = "monitoring-records")
  ),
  if (.has_trap) nav_menu(
    "Trapping", icon = icon("location-crosshairs"),
    maps_ui("trapping_map", device = "trap", label = "Map", value = "trapping-map", ik_data = ik_data),
    nav_panel("Trap review", value = "trap-review", icon = icon("heart-pulse"),
              trapping_ui("trapping")),
    bait_ui("bait", ik_data),                              # bait effectiveness (trap)
    trapping_effectiveness_ui("trapping_eff", ik_data),    # catch rate vs cadence, by season
    records_ui("control_records", label = "Records", value = "trapping-records")
  ),

  # Insights — cross-device synthesis, or features that work on whichever data you have.
  nav_menu(
    "Insights", icon = icon("chart-line"),
    outcomes_ui("outcomes"),                              # "Are we winning?"
    "Deeper analysis",                                    # section header within the dropdown
    # Neighbourhood is camera-ANCHORED (pick a camera site/line/reserve) — useless without camera
    # data, so hide it then, the same way the Monitoring menu auto-hides. Coverage still degrades
    # gracefully on trap-only data, so it stays.
    if (.has_camera) neighbourhood_ui("neighbourhood", ik_data),
    coverage_ui("coverage")
  ),

  nav_spacer(),

  # Dark/light toggle. Lives in the navbar (not the Settings modal) so the bslib
  # web component mounts once at page load; inside a modal it re-runs its connect
  # logic on every open and mis-detects the theme. Sets data-bs-theme on <html>.
  # mode = "light" pins the start state — without it the component follows the
  # OS prefers-color-scheme (so it loads dark on dark-themed machines).
  nav_item(
    input_dark_mode(id = "color_mode", mode = "light")
  ),

  # Settings — opens a modal handled in server.R.
  nav_item(
    actionLink("settings_btn", label = NULL, icon = icon("gear"), title = "Settings")
  )
)
