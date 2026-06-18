# ui.R

ui <- page_navbar(
  id = "nav",
  title = tags$img(
    src = "images/insightful-kiwi.logo.svg", height = "32",
    alt = "Insightful Kiwi", title = "Insightful Kiwi"
  ),
  theme = bs_theme(version = 5, font_scale = 0.9),
  fillable = FALSE,

  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/base.css"),
    tags$link(rel = "icon", href = "images/insightful-kiwi.logo.svg", type = "image/svg+xml"),
    tags$link(rel = "icon", href = "images/icons/favicon.png", type = "image/png", sizes = "32x32")
  ),

  # Global sidebar — filters/controls for the active view.
  # Populated per-view as modules are built; conditional on the selected nav.
  sidebar = sidebar(
    id = "global_sidebar",
    title = "Filters",
    conditionalPanel("input.nav === 'overview'", tags$small("Overview controls will appear here.")),
    conditionalPanel("input.nav === 'maps'",     tags$small("Map controls will appear here.")),
    conditionalPanel("input.nav === 'species'",  tags$small("Species controls will appear here.")),
    conditionalPanel("input.nav === 'records'",  tags$small("Records span all periods."))
  ),

  # Centre the menu: equal flexible space on both sides of the nav.
  nav_spacer(),

  nav_panel("Overview", value = "overview", icon = icon("gauge"),
    h2("Overview")
  ),
  nav_panel("Maps", value = "maps", icon = icon("map"),
    h2("Maps")
  ),
  nav_panel("Species", value = "species", icon = icon("paw"),
    h2("Species")
  ),
  nav_panel("Records", value = "records", icon = icon("table"),
    h2("Records")
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
