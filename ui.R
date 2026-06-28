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
  title = tags$span(class = "ik-brand",
    tags$img(src = "images/insightful-kiwi.logo.svg", height = "32",
             alt = "Insightful Kiwi", title = "Insightful Kiwi"),
    # Wordmark — two-tone: "Insightful" in the logo teal, "Kiwi" in the app accent. Beside the logo it
    # fills the space before the hamburger on mobile and reads as the brand left of the menu on desktop.
    tags$span(class = "ik-brand-text",
              tags$span(class = "ik-brand-1", "Insightful"), " ",
              tags$span(class = "ik-brand-2", "Kiwi"))),
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
    ,
    # On mobile, bslib reads its mobile-vs-desktop breakpoint (a CSS var) ONCE when it initialises the
    # sidebar; if that runs before the stylesheet/layout settles it caches the wrong size and the rail is
    # left in a broken/hidden state until the next resize (the old "rotate the phone to reveal it" symptom).
    # bslib re-evaluates on the window 'resize' event, so dispatch a few once connected — bound via jQuery
    # because shiny:connected is a jQuery-triggered event (a native addEventListener never fires it).
    tags$script(HTML(paste(
      "(function(){",
      "  function nudge(){ try { window.dispatchEvent(new Event('resize')); } catch(e){} }",
      "  var mq = window.matchMedia ? window.matchMedia('(min-width: 576px)') : null;",   # bslib sidebar breakpoint
      "  function rpt(){ try { if (window.Shiny && Shiny.setInputValue && mq) Shiny.setInputValue('ik_desktop', mq.matches); } catch(e){} }",
      "  function bind(){",
      "    if (window.jQuery) { window.jQuery(document).on('shiny:connected', function(){ [60, 350, 900].forEach(function(t){ setTimeout(nudge, t); }); rpt(); }); }",
      "    if (mq) { if (mq.addEventListener) mq.addEventListener('change', rpt); else if (mq.addListener) mq.addListener(rpt); }",
      "    [120, 500, 1200].forEach(function(t){ setTimeout(rpt, t); });",   # report desktop/mobile early + on breakpoint change
      "  }",
      "  if (document.readyState === 'loading') { document.addEventListener('DOMContentLoaded', bind); } else { bind(); }",
      "})();",
      sep = "\n")))
  ),

  # Global sidebar — filters/controls for the active view.
  # Populated per-view as modules are built; conditional on the selected nav.
  sidebar = sidebar(
    id = "global_sidebar",
    # Desktop: open by default — the HEAVY pages (Maps/Records/…, SIDEBAR_NAVS) want the rail open and as
    # the default that costs no toggle, so they show it instantly. Mobile: COLLAPSIBLE, closed by default —
    # on a phone an open rail overlays the content, so the content is visible on load and the rail is opened
    # on demand (the date/period banner toggles it, just like desktop). The server applies the same per-nav
    # open/close rules on both (server.R). (Earlier "always" forced it open + non-collapsible, which is why
    # the date toggle did nothing on mobile.)
    open = list(desktop = "open", mobile = "closed"),
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
                                  show = c("period", "compare", "reserve"), ik_data = ik_data,
                                  view = "compare", heading = "Filters")),
    conditionalPanel("input.nav === 'trapping-overview'",
                     selection_ui("trapping_overview_selection",
                                  show = c("period", "compare", "reserve"), ik_data = ik_data,
                                  view = "compare", heading = "Filters")),
    conditionalPanel("input.nav === 'bait'",
                     selection_ui("bait_selection", show = c("period"), ik_data = ik_data,
                                  period_default = "rolling12",
                                  controls = bait_controls("bait", ik_data), heading = "Filters")),
    conditionalPanel("input.nav === 'trap-review'",
                     # Period drives the "By trapline" + Map + Catch efficiency tabs; the "Trend" tab spans all
                     # data, so Period hides there (the generic note in its place). Reserve stays on all tabs.
                     selection_ui("trap_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = .trap_period_def, heading = "Filters",
                                  view_extra = tagList(   # per-tab view controls: By-trapline Dormant/Historic, Over-time grain
                                    conditionalPanel("input['trapping-trap_view'] === 'By trapline'", trapping_byline_controls("trapping")),
                                    conditionalPanel("input['trapping-trap_view'] === 'Trend'", trapping_overtime_controls("trapping"))),
                                  # View-options section hides on the tabs with no view controls (Map, Catch efficiency).
                                  view_show_js = "input['trapping-trap_view'] !== 'Map' && input['trapping-trap_view'] !== 'Catch efficiency'",
                                  period_show_js = "input['trapping-trap_view'] !== 'Trend'")),
    conditionalPanel("input.nav === 'coverage'",
                     selection_ui("coverage_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = "rolling12",
                                  controls = coverage_controls("coverage", ik_data), heading = "Filters")),   # spatial view: a full annual cycle
    conditionalPanel("input.nav === 'predator-pressure'",
                     selection_ui("predator_pressure_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = "rolling12",
                                  controls = predator_pressure_controls("predator_pressure", ik_data), heading = "Filters")),
    conditionalPanel("input.nav === 'neighbourhood'",
                     # All-time view; the anchor (Reserve/Line) is its data selection, in View options.
                     selection_all_data(),
                     neighbourhood_controls("neighbourhood", ik_data)),
    conditionalPanel("input.nav === 'reserve-report'",
                     reserve_report_controls("reserve_report", ik_data)),
    conditionalPanel("input.nav === 'trap-hero'",
                     selection_ui("trap_hero_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = "rolling12",
                                  controls = trap_hero_controls("trap_hero", ik_data), heading = "Filters")),
    conditionalPanel("input.nav === 'cooccurrence'",
                     # Predator/Protected/Within/Predator-after are the co-occurrence-specific picks (read by
                     # the cooccurrence module's namespace) — the tinted "View options" group at the TOP.
                     # Period + Reserve are the shared "Filters" below. The Trend tab spans all seasons, so
                     # Period hides there (a note in its place).
                     selection_ui("cooccurrence_selection", show = c("period", "reserve"), ik_data = ik_data,
                                  period_default = "rolling12",
                                  period_show_js = "input['cooccurrence-cooc_view'] !== 'Trend'",
                                  controls = cooccurrence_controls("cooccurrence"), heading = "Filters")),
    conditionalPanel("input.nav === 'camera-review'",
                     selection_all_data(),
                     monitoring_controls("monitoring")),
    conditionalPanel("input.nav === 'duplicates'",
                     selection_all_data(),
                     tags$small(class = "ik-period-note", "Camera & species selectors are in the view.")),
    conditionalPanel("input.nav === 'monitoring-map'",
                     selection_ui("mon_map_selection",
                                  show = c("period", "reserve", "line"), ik_data = ik_data,
                                  controls = maps_controls("monitoring_map", device = "camera", ik_data = ik_data),
                                  heading = "Filters")),
    conditionalPanel("input.nav === 'trapping-map'",
                     selection_ui("trap_map_selection",
                                  show = c("period", "reserve", "line"), ik_data = ik_data,
                                  controls = maps_controls("trapping_map", device = "trap", ik_data = ik_data),
                                  heading = "Filters")),
    conditionalPanel("input.nav === 'monitoring-records'",
                     selection_ui("mon_records_selection",
                                  show = c("dataset", "period", "reserve", "line", "location",
                                           "device", "species", "net"), ik_data = ik_data,
                                  device_default = "camera", heading = "Filters")),
    conditionalPanel("input.nav === 'trapping-records'",
                     selection_ui("control_records_selection",
                                  show = c("dataset", "period", "reserve", "line", "location",
                                           "device", "species", "net"), ik_data = ik_data,
                                  device_default = "trap", heading = "Filters")),
    # Species dashboards (one shared selection — every species page's value starts grp-/sp-). Period
    # defaults to All data so every tab starts on the same footing; Summary & Trend always span all
    # data (Period hidden, a note in its place), the rest honour whatever Period the user sets.
    conditionalPanel("input.nav && (input.nav.indexOf('grp-') === 0 || input.nav.indexOf('sp-') === 0)",
                     selection_ui("species_selection", show = c("period", "reserve", "device"), ik_data = ik_data,
                                  period_default = "all",
                                  period_show_js = "input.ik_species_tab !== 'Trend' && input.ik_species_tab !== 'Summary'",
                                  # Device (camera/trap) split only matters for the Records list, so show it just there.
                                  device_show_js = "input.ik_species_tab === 'Records'",
                                  # Per-species View options (the Bait + Co-occurrence tab controls), each gated on its
                                  # OWN nav value; slotted below Data period, above the shared Period/Reserve/Device "Filters".
                                  controls = lapply(.species_specs, function(s) {
                                    ctl <- species_controls(gsub("-", "_", s$key), s, ik_data)
                                    if (is.null(ctl)) NULL else conditionalPanel(sprintf("input.nav === '%s'", s$key), ctl)
                                  }),
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
  # Monitoring / Trapping menus grouped with section headers (the Insights pattern): the everyday views,
  # then "Analysis", then "Data quality" / "Health" / "Effectiveness". Empty groups drop out via .ik_nav_group.
  if (.has_camera) do.call(nav_menu, c(
    list("Monitoring", icon = icon("binoculars")),
    .ik_nav_group(NULL, list(
      overview_ui("monitoring_overview", sections = "camera", label = "Overview", value = "monitoring-overview"),
      if (.has_favourites && ik_feature_enabled(ik_data, "highlights")) highlights_ui("highlights", ik_data),
      maps_ui("monitoring_map", device = "camera", label = "Map", value = "monitoring-map", ik_data = ik_data),
      records_ui("mon_records", label = "Records", value = "monitoring-records", heading = "Monitoring records"))),
    .ik_nav_group("Analysis", list(
      if (ik_feature_enabled(ik_data, "cooccurrence")) cooccurrence_ui("cooccurrence"))),   # predator ↔ protected timing (camera)
    .ik_nav_group("Data quality", list(
      nav_panel("Deployment review", value = "camera-review", icon = icon("camera"), monitoring_ui("monitoring", ik_data)),
      if (ik_feature_enabled(ik_data, "duplicates"))
        nav_panel("Duplicate window", value = "duplicates", icon = icon("clone"), duplicates_ui("duplicates")))))),
  if (.has_trap) do.call(nav_menu, c(
    list("Trapping", icon = icon("location-crosshairs")),
    .ik_nav_group(NULL, list(
      overview_ui("trapping_overview", sections = "trap", label = "Overview", value = "trapping-overview"),
      maps_ui("trapping_map", device = "trap", label = "Map", value = "trapping-map", ik_data = ik_data),
      records_ui("control_records", label = "Records", value = "trapping-records", heading = "Trapping records"))),
    .ik_nav_group("Health", list(
      # Check frequency (was "Trap review") — grades each trap by check cadence; the Catch-efficiency
      # analysis (catch rate vs cadence) is now a TAB inside it, not a separate menu item.
      nav_panel("Check frequency", value = "trap-review", icon = icon("heart-pulse"), trapping_ui("trapping", ik_data)))),
    .ik_nav_group("Effectiveness", list(
      if (ik_feature_enabled(ik_data, "bait")) bait_ui("bait", ik_data),   # bait effectiveness (trap)
      if (ik_feature_enabled(ik_data, "top_traps")) trap_hero_ui("trap_hero", ik_data),   # best-performing traps on a map
      if (.has_trappers && ik_feature_enabled(ik_data, "top_trappers")) top_trappers_ui("top_trappers", ik_data))))),  # volunteer leaderboard

  # Species — one dashboard page per species GROUP (+ split sub-species), grouped by role (config) with a
  # section header per role, like the Insights menu.
  if (ik_feature_enabled(ik_data, "species_pages")) {
    .sp_page <- function(s) species_dashboard_ui(gsub("-", "_", s$key), s, ik_data)
    .sp_by   <- function(role) lapply(Filter(function(s) identical(s$role, role), .species_specs), .sp_page)
    do.call(nav_menu, c(
      list("Species", icon = icon("paw")),
      .ik_nav_group("Protected", .sp_by("protected")),
      .ik_nav_group("Predators", .sp_by("predator")),
      .ik_nav_group("Other", lapply(Filter(function(s) !s$role %in% c("protected", "predator"), .species_specs), .sp_page))))
  },

  # Insights — cross-device synthesis, or features that work on whichever data you have. ("Are we
  # winning?" moved to the Overview page's Trends tab; Reserve report is the deeper over-time chain.)
  nav_menu(
    "Insights", icon = icon("chart-line"),
    if (.has_camera && .has_trap && ik_feature_enabled(ik_data, "reserve_report"))
      reserve_report_ui("reserve_report", ik_data),       # the over-time chain (cross-device)
    "Combined analysis",                                  # section header: features that synthesise camera + trap data
    # Predator pressure is camera-DERIVED (the priority surface), so it needs camera data; hide it on
    # trap-only data like Neighbourhood. The trapping overlay is additive context on top.
    if (.has_camera && ik_feature_enabled(ik_data, "predator_pressure")) predator_pressure_ui("predator_pressure", ik_data),
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
