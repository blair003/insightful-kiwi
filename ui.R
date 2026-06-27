# ui.R

# Trap review's sidebar Period default — the latest 12 months (recency over a single season). (Bait now
# defaults to "rolling12" inline in its panel; the other reviews use ik_default_period.)
.trap_period_def <- "rolling12"

# Device-organised menus are shown only when the org actually has that kind of data, so a
# trapping-only (or monitoring-only) group sees just its menu (bslib drops the NULL nav items).
.has_camera <- ik_has_source_type(ik_data, "camera")
.has_trap   <- ik_has_source_type(ik_data, "trap")
.has_favourites <- ik_has_favourites(ik_data)   # Highlights nav only when the data has favourite images
.has_trappers   <- ik_has_trappers(ik_data)     # Top trappers nav only when trap data carries volunteer tags
.species_specs <- ik_species_taxa(ik_data)   # per-species dashboard pages (Species menu); see modules/species.R

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
    tags$script(src = .ik_asset("js/ovw-lightbox.js")),
    # When a tab is shown — including programmatically via nav_select (the Overview "Monitoring Detail
    # →" link cards) — collapse any navbar dropdown left open, so jumping to a menu page doesn't leave
    # its dropdown hanging open.
    tags$script(HTML(paste(
      "document.addEventListener('shown.bs.tab', function(){",
      "document.querySelectorAll('.navbar .dropdown-menu.show, .navbar .nav-item.dropdown.show, .navbar .dropdown-toggle.show')",
      ".forEach(function(e){ e.classList.remove('show'); e.setAttribute('aria-expanded','false'); }); });",
      sep = "\n")))
  ),

  # Global sidebar — filters/controls for the active view.
  # Populated per-view as modules are built; conditional on the selected nav.
  sidebar = sidebar(
    id = "global_sidebar",
    title = NULL,                          # no "Data Selection" header + divider — the rail starts straight at View options / Filters
    # The Overview pages keep their controls HERE but the rail is COLLAPSED by default for them (they're
    # left out of SIDEBAR_NAVS in server.R) — regular users aren't confronted with filters, power users
    # open the rail to change them, and the current window stays visible via the in-page period banner.
    conditionalPanel("input.nav === 'overview'",
                     # Compare is a "view option" (Snapshot only); Predators/Protected are view options for
                     # the Trends tab; Period + Reserve are the shared Filters. Period & Compare span all data
                     # on the Trends / Network density tabs, so both hide there (generic note in Period's place).
                     selection_ui("overview_selection",
                                  show = c("period", "compare", "reserve"), ik_data = ik_data,
                                  heading = "Filters", view = "compare",
                                  view_js = list(compare = "input['overview-ov_tabs'] !== 'Trends' && input['overview-ov_tabs'] !== 'Network density'"),
                                  view_extra = conditionalPanel("input['overview-ov_tabs'] === 'Trends'",
                                                                outcomes_controls("overview_trends", ik_data)),
                                  view_show_js = "input['overview-ov_tabs'] !== 'Network density'",
                                  period_show_js = "input['overview-ov_tabs'] !== 'Trends' && input['overview-ov_tabs'] !== 'Network density'")),
    conditionalPanel("input.nav === 'monitoring-overview'",
                     selection_ui("monitoring_overview_selection",
                                  show = c("period", "compare", "reserve"), ik_data = ik_data)),
    conditionalPanel("input.nav === 'trapping-overview'",
                     selection_ui("trapping_overview_selection",
                                  show = c("period", "compare", "reserve"), ik_data = ik_data)),
    conditionalPanel("input.nav === 'bait'",
                     bait_controls("bait", ik_data),
                     selection_ui("bait_selection", show = c("period"), ik_data = ik_data,
                                  period_default = "rolling12", heading = "Filters")),
    conditionalPanel("input.nav === 'trap-review'",
                     # Period drives the "By trapline" + Map tabs; the "Trend" tab spans all data, so
                     # Period hides there (the generic note in its place). Reserve stays on all tabs.
                     selection_ui("trap_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = .trap_period_def, heading = "Filters",
                                  view_extra = tagList(   # per-tab view controls: By-trapline Dormant/Historic, Over-time grain
                                    conditionalPanel("input['trapping-trap_view'] === 'By trapline'", trapping_byline_controls("trapping")),
                                    conditionalPanel("input['trapping-trap_view'] === 'Trend'", trapping_overtime_controls("trapping"))),
                                  view_show_js = "input['trapping-trap_view'] !== 'Map'",
                                  period_show_js = "input['trapping-trap_view'] !== 'Trend'")),
    conditionalPanel("input.nav === 'coverage'",
                     coverage_controls("coverage", ik_data),
                     selection_ui("coverage_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = "rolling12", heading = "Filters")),   # spatial view: a full annual cycle
    conditionalPanel("input.nav === 'neighbourhood'",
                     # Neighbourhood's anchor IS its data selection (no shared Period/Reserve); all of its
                     # controls go in the tinted "View options" group — see neighbourhood_controls.
                     neighbourhood_controls("neighbourhood", ik_data)),
    conditionalPanel("input.nav === 'trap-hero'",
                     trap_hero_controls("trap_hero", ik_data),
                     selection_ui("trap_hero_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = "rolling12", heading = "Filters")),
    conditionalPanel("input.nav === 'cooccurrence'",
                     # Predator/Protected/Within/Predator-after are the co-occurrence-specific picks (read by
                     # the cooccurrence module's namespace) — the tinted "View options" group at the TOP.
                     # Period + Reserve are the shared "Filters" below. The Trend tab spans all seasons, so
                     # Period hides there (a note in its place).
                     cooccurrence_controls("cooccurrence"),
                     selection_ui("cooccurrence_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = "rolling12",
                                  period_show_js = "input['cooccurrence-cooc_view'] !== 'Trend'",
                                  heading = "Filters")),
    conditionalPanel("input.nav === 'camera-review'",
                     monitoring_controls("monitoring")),
    conditionalPanel("input.nav === 'duplicates'",
                     tags$small("Controls are within the view.")),
    conditionalPanel("input.nav === 'monitoring-map'",
                     maps_controls("monitoring_map", device = "camera", ik_data = ik_data),
                     selection_ui("mon_map_selection",
                                  show = c("period", "reserve", "line"), ik_data = ik_data,
                                  heading = "Filters")),
    conditionalPanel("input.nav === 'trapping-map'",
                     maps_controls("trapping_map", device = "trap", ik_data = ik_data),
                     selection_ui("trap_map_selection",
                                  show = c("period", "reserve", "line"), ik_data = ik_data,
                                  heading = "Filters")),
    conditionalPanel("input.nav === 'monitoring-records'",
                     selection_ui("mon_records_selection",
                                  show = c("dataset", "period", "reserve", "line", "location",
                                           "device", "species", "net"), ik_data = ik_data, device_default = "camera")),
    conditionalPanel("input.nav === 'trapping-records'",
                     selection_ui("control_records_selection",
                                  show = c("dataset", "period", "reserve", "line", "location",
                                           "device", "species", "net"), ik_data = ik_data, device_default = "trap")),
    # Species dashboards (one shared selection — every species page's value starts grp-/sp-). Period
    # defaults to All data so every tab starts on the same footing; Summary & Trend always span all
    # data (Period hidden, a note in its place), the rest honour whatever Period the user sets.
    # Per-species View options (the Bait + Co-occurrence tab controls), each conditional on its OWN nav
    # value; the Period/Reserve/Device "Filters" below are shared across every species page.
    lapply(.species_specs, function(s) {
      ctl <- species_controls(gsub("-", "_", s$key), s, ik_data)
      if (is.null(ctl)) NULL else conditionalPanel(sprintf("input.nav === '%s'", s$key), ctl)
    }),
    conditionalPanel("input.nav && (input.nav.indexOf('grp-') === 0 || input.nav.indexOf('sp-') === 0)",
                     selection_ui("species_selection", show = c("period", "reserve", "device"), ik_data = ik_data,
                                  period_default = "all",
                                  period_show_js = "input.ik_species_tab !== 'Trend' && input.ik_species_tab !== 'Summary'",
                                  # Device (camera/trap) split only matters for the Records list, so show it just there.
                                  device_show_js = "input.ik_species_tab === 'Records'",
                                  heading = "Filters")),
    tags$div(class = "ik-sidebar-foot",
             tags$em("Insightful Kiwi"), tags$br(),
             tags$a(href = "mailto:blair@aketechnology.co.nz?subject=Insightful%20Kiwi%20Query",
                    "Blair George"))
  ),

  # Centre the menu: equal flexible space on both sides of the nav.
  nav_spacer(),

  # Main page: slim cross-device headline (Snapshot) + the "are we winning?" seasonal graphs as a
  # second Trends tab (suspended until opened, so the heavy outcome-series never hits the landing).
  overview_ui("overview", compact = TRUE, trends = outcomes_panel_body("overview_trends"), network = TRUE),

  # Menus are organised by DEVICE — Monitoring (camera/observation) and "Trapping" (predator CONTROL:
  # trapping today, poison/etc. later — friendly label now, conceptually Control). Each shown only when
  # the org has that data; Records sits in each, device-filtered (default to the device, still changeable).
  if (.has_camera) nav_menu(
    "Monitoring", icon = icon("binoculars"),
    overview_ui("monitoring_overview", sections = "camera", label = "Overview", value = "monitoring-overview"),
    if (.has_favourites && ik_feature_enabled(ik_data, "highlights")) highlights_ui("highlights", ik_data),   # friendly visual entry point
    maps_ui("monitoring_map", device = "camera", label = "Map", value = "monitoring-map", ik_data = ik_data),
    nav_panel("Camera review", value = "camera-review", icon = icon("camera"),
              monitoring_ui("monitoring")),
    if (ik_feature_enabled(ik_data, "cooccurrence")) cooccurrence_ui("cooccurrence"),   # predator ↔ protected timing (camera)
    if (ik_feature_enabled(ik_data, "duplicates"))
      nav_panel("Duplicate window", value = "duplicates", icon = icon("clone"),
                duplicates_ui("duplicates")),
    records_ui("mon_records", label = "Records", value = "monitoring-records", heading = "Monitoring records")
  ),
  if (.has_trap) nav_menu(
    "Trapping", icon = icon("location-crosshairs"),
    overview_ui("trapping_overview", sections = "trap", label = "Overview", value = "trapping-overview"),
    maps_ui("trapping_map", device = "trap", label = "Map", value = "trapping-map", ik_data = ik_data),
    nav_panel("Trap review", value = "trap-review", icon = icon("heart-pulse"),
              trapping_ui("trapping", ik_data)),
    if (ik_feature_enabled(ik_data, "bait")) bait_ui("bait", ik_data),   # bait effectiveness (trap)
    if (ik_feature_enabled(ik_data, "trapping_effectiveness")) trapping_effectiveness_ui("trapping_eff", ik_data),   # catch rate vs cadence, by season
    if (ik_feature_enabled(ik_data, "top_traps")) trap_hero_ui("trap_hero", ik_data),   # best-performing traps on a map
    if (.has_trappers && ik_feature_enabled(ik_data, "top_trappers")) top_trappers_ui("top_trappers", ik_data),   # gamified per-season volunteer leaderboard
    records_ui("control_records", label = "Records", value = "trapping-records", heading = "Trapping records")
  ),

  # Species — one dashboard page per species GROUP (+ split sub-species), generated from the data.
  if (ik_feature_enabled(ik_data, "species_pages")) do.call(nav_menu, c(
    list("Species", icon = icon("paw")),
    lapply(.species_specs, function(s) species_dashboard_ui(gsub("-", "_", s$key), s, ik_data)))),

  # Insights — cross-device synthesis, or features that work on whichever data you have. ("Are we
  # winning?" moved to the Overview page's Trends tab; Reserve report is the deeper over-time chain.)
  nav_menu(
    "Insights", icon = icon("chart-line"),
    if (.has_camera && .has_trap && ik_feature_enabled(ik_data, "reserve_report"))
      reserve_report_ui("reserve_report", ik_data),       # the over-time chain (cross-device)
    "Deeper analysis",                                    # section header within the dropdown
    # Neighbourhood is camera-ANCHORED (pick a camera site/line/reserve) — useless without camera
    # data, so hide it then, the same way the Monitoring menu auto-hides. Coverage still degrades
    # gracefully on trap-only data, so it stays.
    if (.has_camera && ik_feature_enabled(ik_data, "neighbourhood")) neighbourhood_ui("neighbourhood", ik_data),
    if (ik_feature_enabled(ik_data, "coverage")) coverage_ui("coverage", ik_data)
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
