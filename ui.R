# ui.R

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
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/base.css"),
    tags$link(rel = "icon", href = "images/insightful-kiwi.logo.svg", type = "image/svg+xml"),
    tags$link(rel = "icon", href = "images/icons/favicon.png", type = "image/png", sizes = "32x32"),
    # full-screen sequence viewer for the observation Photos burst (delegated clicks → works in modals)
    tags$script(src = "js/ovw-lightbox.js")
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
    conditionalPanel("input.nav === 'bait'", tags$small("Controls are above the chart.")),
    conditionalPanel("input.nav === 'camera-review' || input.nav === 'trap-review' || input.nav === 'duplicates'",
                     tags$small("Controls are within the view.")),
    conditionalPanel("input.nav === 'maps'",
                     selection_ui("maps_selection",
                                  show = c("period", "reserve", "line", "location"), ik_data = ik_data)),
    conditionalPanel("input.nav === 'species'",  tags$small("Species controls will appear here.")),
    conditionalPanel("input.nav === 'records'",
                     selection_ui("selection",
                                  show = c("dataset", "period", "reserve", "line", "location",
                                           "device", "species", "net"), ik_data = ik_data))
  ),

  # Centre the menu: equal flexible space on both sides of the nav.
  nav_spacer(),

  overview_ui("overview"),
  # "Outcomes" is a dropdown: the headline trend + the operational bait analysis.
  nav_menu(
    "Outcomes", icon = icon("chart-line"),
    outcomes_ui("outcomes"),
    bait_ui("bait")
  ),
  maps_ui("maps"),
  nav_panel("Species", value = "species", icon = icon("paw"),
    h2("Species")
  ),
  # "Data" groups the raw-data + data-quality views as direct pages (room to grow: exports, …).
  nav_menu(
    "Data", icon = icon("database"),
    records_ui("records"),
    "Quality",                                            # section header within the dropdown
    nav_panel("Camera review",    value = "camera-review", icon = icon("camera"),
              monitoring_ui("monitoring")),
    nav_panel("Trap review",      value = "trap-review",   icon = icon("heart-pulse"),
              trapping_ui("trapping")),
    nav_panel("Duplicate window", value = "duplicates",    icon = icon("clone"),
              duplicates_ui("duplicates"))
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
