# spatial_analysis_module.R
#
# The "Spatial Analysis" workbench: a config-driven orchestrator that builds its
# own sidebar and main area from a `spec` and composes the lower-level pieces:
#   - spatial_map_module  (geographic render; owns the analysis_dataset reactive)
#   - analysis_output_module (non-geographic render: Records, future Insights)
#   - period_selection_module (per-side period picker)
#
# A `spec` (see spatial_analysis_default_spec()) fully describes a map instance.
# When a mode control is `editable` the workbench renders a control for it and
# switches behaviour AT RUNTIME; when locked it uses the spec value. Existing maps
# become locked presets (see spatial_analysis_presets()).
#
# Persistence across runtime mode switches: the left panel (panel_left) is always
# present and owns the shared selection/layer inputs, so switching Single<->Comparison
# keeps the user's choices. The comparative side reads panel_left via overrides.
#
# IMPLEMENTED:
#   - single  + static / timeline
#   - comparison + static + linked + comparison_type "period"  (Density Summary parity)
#   - runtime Single <-> Comparison switching (when analysis_mode is editable)
# GUARDED (explicit error) until later phases:
#   - comparison + timeline; comparison_source "independent"; comparison_type
#     "data_source" / "custom".

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

sa_or <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

sa_resolve_period_default <- function(period_key, core_data) {
  defaults <- core_data$app$period_defaults
  if (is.null(period_key) || identical(period_key, "primary")) {
    return(defaults$primary_period)
  }
  if (identical(period_key, "comparative")) {
    return(defaults$comparative_period)
  }
  period_key
}

sa_default_species <- function(spec_species, core_data) {
  if (!is.null(spec_species)) {
    return(spec_species)
  }
  first_class <- core_data$app$spp_classes[[1]]
  first_class[seq_len(min(3, length(first_class)))]
}

sa_default_localities <- function(spec_localities, core_data) {
  if (!is.null(spec_localities)) {
    return(spec_localities)
  }
  unique(core_data$deps$locality)
}

# "Timeframe: <start> to <end>" readout for a period. Intended for use inside renderUI.
sa_period_readout <- function(period) {
  req(period$start_date(), period$end_date())

  timezone <- if (exists("timeline_actual_timezone", mode = "function", inherits = TRUE)) {
    timeline_actual_timezone()
  } else if (!is.null(config$globals$actual_timezone) && nzchar(config$globals$actual_timezone)) {
    config$globals$actual_timezone
  } else {
    "Pacific/Auckland"
  }

  start_time <- as.POSIXct(
    paste(format(as.Date(period$start_date()), "%Y-%m-%d"), "00:00:00"), tz = timezone)
  end_time <- as.POSIXct(
    paste(format(as.Date(period$end_date()), "%Y-%m-%d"), "23:59:59"), tz = timezone)

  # "Full period:" = the overall selected range, distinct from the current step
  # (the "Now"/season block shown when timeline is on). Friendly d mmm yyyy dates.
  div(
    class = "timeline-window-readout",
    strong("Full period:"),
    paste(format(start_time, "%-d %b %Y", tz = timezone), "to",
          format(end_time, "%-d %b %Y", tz = timezone))
  )
}

sa_mode_editable <- function(spec, control) {
  isTRUE(spec$controls[[control]]$visible) && isTRUE(spec$controls[[control]]$editable)
}

# Fail fast for spec combinations / reachable runtime modes not yet implemented.
sa_assert_supported <- function(spec) {
  reachable_comparison <- sa_mode_editable(spec, "analysis_mode") ||
    identical(spec$analysis_mode, "comparison")

  if (reachable_comparison) {
    if (!identical(sa_or(spec$comparison_type, "period"), "period")) {
      stop("spatial_analysis_module: only 'period' comparison is implemented yet.")
    }
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Spec + presets
# ---------------------------------------------------------------------------

spatial_analysis_default_spec <- function() {
  list(
    analysis_mode     = "single",        # "single" | "comparison"
    time_mode         = "static",        # "static" | "timeline"
    comparison_type   = "period",        # "period" | "data_source" | "custom"
    comparison_source = "linked",        # "linked" | "independent"
    linked_controls   = c("species", "localities", "layers"),
    independent_timeline = FALSE,

    sides = list(
      # "period" terminology (not "season"): a selected period may be a season,
      # a year, or several seasons combined.
      left  = list(label = "Primary period", species = NULL, localities = NULL,
                   period = "primary", data_source = "monitoring"),
      right = list(label = "Comparison period", species = NULL, localities = NULL,
                   period = "primary", data_source = "monitoring")
    ),

    layers = list(
      available = c("observation_records", "capture_records", "surface",
                    "capture_density_surface", "trap_check_frequency", "unchecked_traps"),
      locked    = character(0)
    ),

    temporal = list(
      grouping = "day",
      window   = "cumulative",
      # Calendar-progression timeline only goes down to daily; sub-daily cyclic
      # groupings (hourly / diel / day-night) belong to the Temporal Analysis map.
      grouping_choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month", "Season" = "season"),
      activity_pattern = FALSE
    ),

    tabs = list(map = TRUE, records = TRUE, insights = FALSE),

    controls = list(
      analysis_mode    = list(visible = TRUE, editable = TRUE),
      time_mode        = list(visible = TRUE, editable = TRUE),
      comparison_type  = list(visible = TRUE, editable = TRUE),
      comparison_source = list(visible = TRUE, editable = TRUE),
      species          = list(visible = TRUE, editable = TRUE),
      localities       = list(visible = TRUE, editable = TRUE),
      period           = list(visible = TRUE, editable = TRUE),
      data_source      = list(visible = TRUE, editable = TRUE),
      layers           = list(visible = TRUE, editable = TRUE),
      temporal_controls = list(visible = TRUE, editable = TRUE)
    )
  )
}

# Lock mode controls (used by fixed-mode presets that mirror existing maps). By
# default locks all four; pass `controls` to lock only a subset (e.g. the Temporal
# map locks the timeline + comparison type but leaves Map mode / analysis_mode editable).
sa_lock_modes <- function(spec,
                          controls = c("analysis_mode", "time_mode",
                                       "comparison_type", "comparison_source")) {
  for (ctl in controls) {
    spec$controls[[ctl]]$editable <- FALSE
  }
  spec
}

spatial_analysis_presets <- function() {
  base <- spatial_analysis_default_spec()

  list(
    # Power user: mode controls remain editable (runtime switching).
    spatial_analysis = base,

    density_timeline = sa_lock_modes(modifyList(base, list(
      analysis_mode = "single",
      time_mode     = "timeline"
    ))),

    density_summary = sa_lock_modes(modifyList(base, list(
      analysis_mode = "comparison", time_mode = "static",
      comparison_type = "period", comparison_source = "linked",
      linked_controls = c("species", "localities", "layers"),
      sides = list(
        left  = list(label = "Primary period", period = "primary"),
        right = list(label = "Comparison period", period = "comparative")
      )
    ))),

    monitoring_trapping = sa_lock_modes(modifyList(base, list(
      analysis_mode = "comparison", time_mode = "static",
      comparison_type = "data_source", comparison_source = "linked",
      linked_controls = c("species", "localities"),
      sides = list(
        left  = list(label = "Monitoring data", period = "primary", data_source = "monitoring"),
        right = list(label = "Trapping data", period = "primary", data_source = "trapping")
      )
    ))),

    activity_pattern = sa_lock_modes(modifyList(base, list(
      analysis_mode = "single", time_mode = "timeline",
      temporal = list(
        grouping = "day_night", window = "cumulative", activity_pattern = TRUE,
        grouping_choices = c("Hourly" = "hour", "Diel activity" = "diel",
                             "Daily - Day/Night" = "day_night")
      )
    ))),

    # Temporal Analysis map: collapses the selected period(s) into a repeating
    # temporal unit (Hourly / Diel / Day-Night) instead of a calendar progression.
    # Monitoring-only (no fine-grained temporal trap data): omitting the trapping
    # layers from `available` makes the layer UI render the Monitoring group only.
    # Map mode (Single / Swipe / Side-by-side) stays editable so two activity
    # patterns can be compared; the timeline is intrinsic (time_mode locked on).
    temporal_analysis = sa_lock_modes(
      modifyList(base, list(
        analysis_mode = "single", time_mode = "timeline",
        comparison_type = "period", comparison_source = "linked",
        layers = list(
          available = c("observation_records", "surface"),
          locked = character(0)
        ),
        temporal = list(
          # Discrete (single step) is the sensible default for a cyclic/activity view.
          grouping = "day_night", window = "single", activity_pattern = TRUE,
          grouping_choices = c("Hourly" = "hour", "Diel activity" = "diel",
                               "Daily - Day/Night" = "day_night")
        ),
        sides = list(
          left  = list(label = "Activity pattern", period = "primary", data_source = "monitoring"),
          right = list(label = "Activity pattern", period = "comparative", data_source = "monitoring")
        )
      )),
      controls = c("time_mode", "comparison_type", "comparison_source")
    )
  )
}

spatial_analysis_preset <- function(name) {
  presets <- spatial_analysis_presets()
  if (!name %in% names(presets)) {
    stop(sprintf("Unknown spatial analysis preset: %s", name))
  }
  presets[[name]]
}

# ---------------------------------------------------------------------------
# UI building blocks
# ---------------------------------------------------------------------------

# Layer / option controls for one panel (static uses density_options; timeline
# adds the timeline option panel). Reused by single, locked, and per-item sidebars.
sa_layer_options_ui <- function(panel_id, timeline, layer_flag, temporal_spec, include_config = TRUE) {
  if (timeline) {
    tagList(
      spatial_map_module_ui(
        id = panel_id, view = "density_options",
        # In timeline mode the density_timeline_controls view (below) owns the
        # layer toggles incl. the prediction surface, so this density_options call
        # renders config only — otherwise the surface inputs are duplicated.
        include_prediction_option = FALSE,
        include_marker_options = FALSE,
        include_duplicates_option = include_config,
        include_trap_distance_option = include_config
      ),
      spatial_map_module_ui(
        id = panel_id, view = "density_timeline_controls",
        include_marker_options = layer_flag("observation_records"),
        include_observation_layer_options = layer_flag("capture_records"),
        include_prediction_option = layer_flag("surface"),
        # Cyclic/Temporal recalculates the surface per discrete bin, so allow it in
        # discrete steps (calendar timelines keep it cumulative-only).
        surface_in_discrete = isTRUE(temporal_spec$activity_pattern),
        timeline_step_size_choices = sa_or(temporal_spec$grouping_choices,
                                           timeline_step_size_choices_default()),
        timeline_step_size_selected = temporal_spec$grouping,
        timeline_view_mode_selected = temporal_spec$window
      )
    )
  } else {
    spatial_map_module_ui(
      id = panel_id, view = "density_options",
      include_prediction_option = layer_flag("surface"),
      include_marker_options = layer_flag("observation_records"),
      include_duplicates_option = include_config,
      include_trap_distance_option = include_config
    )
  }
}

# Species + localities + layer-option controls for one side (single & locked maps).
# Species-display, exclude-duplicates and trap-distance live in the "Map settings"
# gear (beside the page heading), so they're omitted here (include_*_option = FALSE).
sa_selectors_ui <- function(panel_id, side_spec, core_data, timeline, layer_flag, temporal_spec) {
  tagList(
    spatial_map_module_ui(
      id = panel_id, view = "select_species",
      choices = core_data$app$spp_classes,
      selected = sa_default_species(side_spec$species, core_data),
      include_species_display_mode = FALSE
    ),
    spatial_map_module_ui(
      id = panel_id, view = "select_localities",
      choices = unique(core_data$deps$locality),
      selected = sa_default_localities(side_spec$localities, core_data)
    ),
    sa_layer_options_ui(panel_id, timeline, layer_flag, temporal_spec, include_config = FALSE)
  )
}

# JS conditionalPanel predicates. The editable "View" radio (input$view) controls
# LAYOUT only: "single" | "comparison" (swipe with draggable slider) | "comparison_fixed"
# (two static side-by-side maps, no slider). Timeline is a separate checkbox
# (input$time_mode), available in every layout.
sa_js_view_is_comparison <- function(ns) {
  sprintf("(input['%s'] == 'comparison' || input['%s'] == 'comparison_fixed')",
          ns("view"), ns("view"))
}
sa_js_view_is_single <- function(ns) sprintf("input['%s'] == 'single'", ns("view"))
sa_js_timeline_on <- function(ns) sprintf("input['%s']", ns("time_mode"))

# Default View radio value from a spec's analysis_mode (timeline is its own toggle).
sa_default_view <- function(spec) {
  if (identical(spec$analysis_mode, "comparison")) "comparison" else "single"
}

# A linkable sidebar item. The left widget keeps its own native label (with its
# icon); a small "Linked" checkbox (default checked, comparison mode only) is
# floated to the item's top-right via CSS (.sa-linkable / .sa-link-toggle in
# custom.css). The right (comparison) widget appears below when UNlinked.
# `link_id` is the fully-namespaced checkbox id.
sa_linkable_item <- function(ns, link_id, left_widget, right_widget) {
  div(
    class = "sa-linkable",
    conditionalPanel(
      sa_js_view_is_comparison(ns),
      div(
        class = "sa-link-toggle",
        title = "Toggle to link or unlink this field across the two maps.",
        checkboxInput(
          link_id,
          label = tagList(
            tags$span(class = "sa-icon-linked", icon("link")),
            tags$span(class = "sa-icon-unlinked", icon("link-slash"))
          ),
          value = TRUE, width = "auto"
        )
      )
    ),
    left_widget,
    conditionalPanel(
      sprintf("%s && !input['%s']", sa_js_view_is_comparison(ns), link_id),
      div(
        class = "sa-comparison-field",
        div(class = "sa-comparison-tag", "Comparison map"),
        right_widget
      )
    )
  )
}

# A locked sidebar item: shows a padlock (in the chain toggle's place) in
# comparison mode to signal the field is always shared and can't be unlinked.
sa_locked_item <- function(ns, widget) {
  div(
    class = "sa-linkable",
    conditionalPanel(
      sa_js_view_is_comparison(ns),
      div(
        class = "sa-link-toggle sa-locked",
        title = "Localities are always shared across the two maps — a synced side-by-side view only makes sense over the same area.",
        icon("lock")
      )
    ),
    widget
  )
}

# Compact display labels for the current species / localities selection, used in
# the map selection headings. Mirror server.R's selected_*_heading closures but
# read from the module's core_data.
sa_species_heading <- function(species, core_data) {
  species_choices <- unlist(unname(core_data$app$spp_classes), use.names = TRUE)
  if (is.null(species) || length(species) == 0) species <- unname(species_choices)
  species <- as.character(species)
  labels <- names(species_choices)[match(tolower(species), tolower(unname(species_choices)))]
  missing <- is.na(labels) | !nzchar(labels)
  labels[missing] <- species[missing]
  paste(labels, collapse = ", ")
}

sa_species_line <- function(species, core_data) {
  tags$div(
    class = "sa-species-line",
    tags$strong("Species:"), " ", sa_species_heading(species, core_data)
  )
}

# Current-step season/window/monitoring block appended INSIDE the identity heading
# overlay when timeline is on (status_html comes from the engine's timeline_status
# reactive; NULL when timeline off). One growing box, no extra map corner.
sa_now_block <- function(status_html) {
  if (is.null(status_html) || !nzchar(status_html)) return(NULL)
  tags$div(class = "sa-now-block", HTML(status_html))
}

# Shared colour/size scale helpers for comparison normalisation. Computed from
# raw obs / trap data (NOT the panels' analysis_dataset reactives, which read the
# scale override and would cycle). Mirror server.R's density_max_* helpers.
sa_max_location_count <- function(observations, species, localities, exclude_dups) {
  if (is.null(observations) || nrow(observations) == 0 ||
      is.null(species) || length(species) == 0 ||
      is.null(localities) || length(localities) == 0) {
    return(0)
  }
  species <- tolower(unname(as.character(species)))
  counts <- observations %>%
    dplyr::filter(.data$scientificName_lower %in% species, .data$locality %in% localities)
  if (isTRUE(exclude_dups) && "possible_duplicate" %in% names(counts)) {
    counts <- counts %>%
      dplyr::filter(is.na(.data$possible_duplicate) | !.data$possible_duplicate)
  }
  counts <- counts %>%
    dplyr::group_by(.data$locationID) %>%
    dplyr::summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(.data$count)
  if (length(counts) == 0 || all(is.na(counts))) return(0)
  max(counts, na.rm = TRUE)
}

sa_max_trap_captures <- function(trap_data, period, species, localities, distance) {
  if (is.null(trap_data) || is.null(species) || length(species) == 0 ||
      is.null(localities) || length(localities) == 0) {
    return(0)
  }
  trap_rows <- prepare_trap_observations_for_map(
    trap_data, period$start_date(), period$end_date(),
    tolower(unname(as.character(species))), localities, distance,
    include_blank_checks = FALSE, include_unchecked_locations = FALSE,
    period_intervals = period$period_intervals()
  )
  capture_summary <- create_trap_capture_summary(trap_rows)
  if (nrow(capture_summary) == 0) return(0)
  counts <- capture_summary %>%
    dplyr::group_by(.data$locationID) %>%
    dplyr::summarise(count = sum(.data$captures, na.rm = TRUE), .groups = "drop") %>%
    dplyr::pull(.data$count)
  if (length(counts) == 0 || all(is.na(counts))) return(0)
  max(counts, na.rm = TRUE)
}

# Workbench single-map leaflet fills its wrapper; the wrapper's height is set in
# CSS (viewport-fitting, adapting to the timeline bar via the .has-timeline class).
sa_map_height <- function() "100%"

# Single-map main layout (Map + Records tabs).
sa_single_main_ui <- function(ns, spec, timeline, toggleable = FALSE) {
  panel_id <- ns("panel_left")
  # Selection heading as a leaflet overlay (top-left), matching the comparison
  # panels.
  heading_overlay <- div(
    class = "map-swipe-label map-swipe-label-primary sa-single-label",
    uiOutput(ns("single_selection_heading"))
  )
  timeline_bar_ui <- function() spatial_map_module_ui(
    id = panel_id, view = "timeline_bar",
    timeline_step_size_choices = sa_or(spec$temporal$grouping_choices,
                                       timeline_step_size_choices_default()),
    timeline_step_size_selected = spec$temporal$grouping,
    timeline_view_mode_selected = spec$temporal$window
  )
  # The timeline bar (when present) sits ABOVE the map wrapper so the identity
  # heading overlay — absolutely positioned inside the wrapper — sits over the
  # leaflet, not floating over the slider. Used for BOTH the toggleable single
  # (bar shown/hidden by the Timeline checkbox) and a locked-timeline preset like
  # Temporal Analysis (bar always shown). A locked-timeline wrapper takes the
  # shorter .has-timeline height up-front (no shinyjs toggle to do it).
  wrap_class <- if (timeline && !toggleable) "sa-single-map-wrap has-timeline" else "sa-single-map-wrap"
  map_wrap <- div(
    id = ns("single_map_wrap"),
    class = wrap_class,
    spatial_map_module_ui(id = panel_id, view = "map", map_height = sa_map_height()),
    heading_overlay
  )
  map_ui <- if (toggleable || timeline) {
    tagList(timeline_bar_ui(), map_wrap)
  } else {
    map_wrap
  }

  tabs <- list()
  if (isTRUE(spec$tabs$map)) {
    tabs <- c(tabs, list(nav_panel("Map", map_ui, value = "map")))
  }
  if (isTRUE(spec$tabs$records)) {
    tabs <- c(tabs, list(nav_panel(
      "Records",
      analysis_output_module_ui(ns("output_left"), current_records = TRUE,
                                cumulative_records = timeline || toggleable),
      value = "data"
    )))
  }
  do.call(navset_tab, c(list(id = ns("tabs")), tabs))
}

# Comparison main layout (two synced swipe panels + Records sub-tabs).
# left_map_id / right_map_id are the fully-namespaced engine ids whose maps fill
# the swipe (distinct instances so the single-map layout can keep panel_left).
sa_comparison_main_ui <- function(ns, spec,
                                  left_map_id = ns("panel_left"),
                                  right_map_id = ns("panel_right"),
                                  toggleable = FALSE) {
  swipe <- spatial_map_module_ui(
    id = ns("comparison"),
    view = "density_comparison_layout",
    map_height = config$globals$leaflet_height,
    primary_map_id = left_map_id,
    comparative_map_id = right_map_id,
    primary_title = spec$sides$left$label,
    comparative_title = spec$sides$right$label,
    primary_heading_output_id = ns("left_period_name"),
    comparative_heading_output_id = ns("right_period_name"),
    primary_meta_output_id = ns("left_period_readout"),
    comparative_meta_output_id = ns("right_period_readout"),
    primary_data_label = spec$sides$left$label,
    comparative_data_label = spec$sides$right$label,
    primary_data_title = paste(spec$sides$left$label, "records"),
    comparative_data_title = paste(spec$sides$right$label, "records")
  )
  if (!toggleable) {
    return(swipe)
  }
  # Comparison + Timeline (editable): timeline bar(s) above the swipe. The primary
  # bar drives both maps when Period is LINKED (shared clock; the comparison bar is
  # hidden but kept live so it can be synced). When Period is UNLINKED each map gets
  # its own bar (independent clocks). Both appear only while Timeline is on.
  temporal_bar <- function(panel) spatial_map_module_ui(
    id = panel, view = "timeline_bar",
    timeline_step_size_choices = sa_or(spec$temporal$grouping_choices,
                                       timeline_step_size_choices_default()),
    timeline_step_size_selected = spec$temporal$grouping,
    timeline_view_mode_selected = spec$temporal$window
  )
  bars <- div(
    class = "sa-comparison-timeline",
    div(class = "sa-cmp-timeline sa-cmp-timeline-primary", temporal_bar(ns("panel_cmp_left"))),
    # Comparison-side bar: shown beside the primary when Period is UNLINKED.
    # When linked the whole panel is display:none (so it takes no row space) but
    # its slider still renders (suspendWhenHidden) so the shared clock can sync it.
    conditionalPanel(
      sprintf("!input['%s']", ns("link_period")),
      div(class = "sa-cmp-timeline sa-cmp-timeline-comparison", temporal_bar(ns("panel_cmp_right")))
    )
  )
  # When the Timeline is an optional toggle, hide the bars while it is off. When
  # the timeline is intrinsic (locked time_mode, e.g. the cyclic Temporal map)
  # there is no checkbox, so the bars are always shown.
  timeline_section <- if (sa_mode_editable(spec, "time_mode")) {
    conditionalPanel(sa_js_timeline_on(ns), bars)
  } else {
    bars
  }
  tagList(timeline_section, swipe)
}

# Expert "Map settings" gear popover (editable workbench only), shown beside the
# page heading. Controls render with panel_left's input ids so the engine reads
# them (species display + trap distance also propagate to the comparison side via
# overrides; normalise applies in comparison mode).
sa_settings_gear <- function(ns, trap_data) {
  panel_left_input <- NS(ns("panel_left"))
  bslib::popover(
    actionLink(ns("map_settings_trigger"), icon("gear"),
               class = "sa-settings-link",
               title = "Map settings: species display, duplicates, trap distance"),
    radioButtons(
      panel_left_input("species_display_mode"), "Species display:",
      choices = c("Combined" = "combined", "Separate" = "separate"),
      selected = "combined", inline = TRUE
    ),
    checkboxInput(
      panel_left_input("exclude_possible_duplicates"),
      label = tags$small("Exclude monitoring 'possible duplicate observations'"),
      value = isTRUE(config$globals$use_net_data)
    ),
    if (!is.null(trap_data)) {
      sliderInput(
        panel_left_input("trap_locality_distance_km"),
        label = tags$small("Include traps up to this distance (km) from selected localities"),
        min = 0, max = 10, value = 2, step = 0.25
      )
    },
    conditionalPanel(
      sa_js_view_is_comparison(ns),
      checkboxInput(
        ns("normalise_scale"),
        label = tags$small("Normalise marker scale across maps"),
        value = TRUE
      )
    ),
    title = "Map settings", placement = "right"
  )
}

# A Map-mode row for the sidebar: the Map mode icons (editable only) and the Map
# settings gear sit side by side in adjoining bordered cells. For locked presets
# (no Map mode) it's just the gear cell, right-aligned.
sa_mode_settings_row <- function(ns, trap_data, mode_box = NULL) {
  gear_cell <- div(class = "sa-settings-cell", sa_settings_gear(ns, trap_data))
  if (is.null(mode_box)) {
    div(class = "sa-mode-settings-row sa-settings-only", gear_cell)
  } else {
    div(class = "sa-mode-settings-row", mode_box, gear_cell)
  }
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

spatial_analysis_module_ui <- function(id, view = c("sidebar", "main"),
                                       spec = spatial_analysis_default_spec(),
                                       core_data, trap_data = NULL,
                                       page_title = NULL) {
  view <- match.arg(view)
  sa_assert_supported(spec)
  ns <- NS(id)

  analysis_editable <- sa_mode_editable(spec, "analysis_mode")
  timeline <- identical(spec$time_mode, "timeline")
  # Locked time_mode (cyclic Temporal map) => the timeline is intrinsic, so we omit
  # the optional "Timeline" checkbox; it is always on.
  time_editable <- sa_mode_editable(spec, "time_mode")
  layer_flag <- function(name) name %in% spec$layers$available
  # A second period selector exists whenever comparison (period type) is reachable.
  show_right_period <- (analysis_editable || identical(spec$analysis_mode, "comparison")) &&
    identical(sa_or(spec$comparison_type, "period"), "period")

  if (view == "sidebar") {
    period_choices <- period_selection_choices(core_data$period_groups, config = config)

    if (analysis_editable) {
      # Per-item linking: each item has a small right-aligned "Linked" checkbox
      # (default on) in its heading; unchecking reveals the comparison-side widget
      # for that item only. Localities stay linked (a synced side-by-side swipe
      # only makes sense over the same geographic area).

      # Map mode: three centered icon buttons with tooltips, in a bordered cell with
      # the Map settings gear in an adjoining cell (2-column row).
      mode_box <- div(
        class = "sa-view-mode sa-view-icons",
        tags$div(class = "sa-view-mode-label", "Map mode"),
        radioButtons(
          ns("view"), label = NULL, inline = TRUE,
          choiceNames = list(
            tags$span(title = "Single map", icon("map")),
            tags$span(title = "Swipe compare — two maps with a draggable divider",
                      icon("arrows-left-right")),
            tags$span(title = "Side by side — two fixed maps", icon("table-columns"))
          ),
          choiceValues = c("single", "comparison", "comparison_fixed"),
          selected = sa_default_view(spec)
        )
      )
      analysis_box <- sa_mode_settings_row(ns, trap_data, mode_box = mode_box)

      # Timeline (progression slider) is its own toggle, immediately under Period,
      # available in every layout (single + both side-by-side).
      period_item <- div(
        class = "sa-period-item",
        sa_linkable_item(
          ns, ns("link_period"),
          left_widget = period_selection_module_ui(
            id = ns("period_left"), view = "select", choices = period_choices,
            selected = sa_resolve_period_default(spec$sides$left$period, core_data),
            label = "Period:", multiple = TRUE
          ),
          right_widget = period_selection_module_ui(
            id = ns("period_right"), view = "select", choices = period_choices,
            selected = sa_resolve_period_default(spec$sides$right$period, core_data),
            label = "Period:", multiple = TRUE
          )
        ),
        if (time_editable) {
          div(
            class = "sa-timeline-toggle",
            checkboxInput(
              ns("time_mode"),
              label = tags$small("Timeline (progression slider)"),
              value = identical(spec$time_mode, "timeline")
            )
          )
        }
      )

      species_item <- sa_linkable_item(
        ns, ns("link_species"),
        left_widget = tagList(
          spatial_map_module_ui(
            id = ns("panel_left"), view = "select_species",
            choices = core_data$app$spp_classes,
            selected = sa_default_species(spec$sides$left$species, core_data),
            include_species_display_mode = FALSE
          ),
          uiOutput(ns("species_display_note"))
        ),
        right_widget = spatial_map_module_ui(
          id = ns("panel_cmp_right"), view = "select_species",
          choices = core_data$app$spp_classes,
          selected = sa_default_species(spec$sides$right$species, core_data),
          include_species_display_mode = FALSE,
          label = "Species:"
        )
      )

      localities_widget <- sa_locked_item(
        ns,
        spatial_map_module_ui(
          id = ns("panel_left"), view = "select_localities",
          choices = unique(core_data$deps$locality),
          selected = sa_default_localities(spec$sides$left$localities, core_data)
        )
      )
      # Layer outputs are grouped into Monitoring / Trapping boxes, each with its
      # own link chain so the two maps can show different data sources. Timeline
      # presets keep the ungrouped controls (which also carry the step/window
      # options); the power entry is static, so it uses the grouped path.
      layer_widgets <- if (timeline) {
        list(sa_layer_options_ui(ns("panel_left"), timeline, layer_flag,
                                 spec$temporal, include_config = FALSE))
      } else {
        mon_group <- function(panel) spatial_map_module_ui(
          id = panel, view = "density_options", layer_group = "monitoring",
          include_prediction_option = layer_flag("surface"),
          include_marker_options = layer_flag("observation_records"),
          include_duplicates_option = FALSE, include_trap_distance_option = FALSE
        )
        trap_group <- function(panel) spatial_map_module_ui(
          id = panel, view = "density_options", layer_group = "trapping",
          include_marker_options = TRUE,
          include_duplicates_option = FALSE, include_trap_distance_option = FALSE
        )
        items <- list(sa_linkable_item(
          ns, ns("link_monitoring"),
          left_widget = mon_group(ns("panel_left")),
          right_widget = mon_group(ns("panel_cmp_right"))
        ))
        if (!is.null(trap_data)) {
          items <- c(items, list(sa_linkable_item(
            ns, ns("link_trapping"),
            left_widget = trap_group(ns("panel_left")),
            right_widget = trap_group(ns("panel_cmp_right"))
          )))
        }
        items
      }

      return(tagList(
        analysis_box,
        period_item, species_item, localities_widget, layer_widgets
      ))
    }

    # Locked presets: fixed single, or fixed (period) comparison sidebar.
    left_period <- period_selection_module_ui(
      id = ns("period_left"), view = "select", choices = period_choices,
      selected = sa_resolve_period_default(spec$sides$left$period, core_data),
      label = if (show_right_period) "Primary period:" else "Period:", multiple = TRUE
    )
    right_period <- if (show_right_period) {
      period_selection_module_ui(
        id = ns("period_right"), view = "select", choices = period_choices,
        selected = sa_resolve_period_default(spec$sides$right$period, core_data),
        label = "Comparison period:", multiple = TRUE
      )
    }
    left_selectors <- sa_selectors_ui(ns("panel_left"), spec$sides$left, core_data,
                                      timeline, layer_flag, spec$temporal)
    # No Map mode for locked presets, so the Map settings gear sits on its own at
    # the top of the sidebar.
    return(tagList(
      sa_mode_settings_row(ns, trap_data),
      left_period, right_period, left_selectors
    ))
  }

  # view == "main"
  # Page heading row: title on the left, the "Map settings" gear on the right
  # Just the page title; the Map settings gear lives in the sidebar (beside the Map
  # mode icons) — at the top-right it collided with the navbar's global settings.
  heading <- if (!is.null(page_title)) {
    div(class = "map-page-heading", h2(page_title))
  }
  if (analysis_editable) {
    # Both layouts are rendered statically and toggled with conditionalPanel
    # (CSS), NOT uiOutput, so the leaflet elements persist in the DOM and the
    # engine's leafletProxy marker draws land on real elements. The single
    # layout uses panel_left; the comparison swipe uses distinct instances
    # (panel_cmp_left / panel_cmp_right) so panel_left's map id is never duplicated.
    return(tagList(
      heading,
      conditionalPanel(
        sa_js_view_is_single(ns),
        sa_single_main_ui(ns, spec, timeline, toggleable = TRUE)
      ),
      conditionalPanel(
        sa_js_view_is_comparison(ns),
        sa_comparison_main_ui(ns, spec,
                              left_map_id = ns("panel_cmp_left"),
                              right_map_id = ns("panel_cmp_right"),
                              toggleable = TRUE)
      )
    ))
  }
  if (identical(spec$analysis_mode, "comparison")) {
    return(tagList(heading, sa_comparison_main_ui(ns, spec)))
  }
  tagList(heading, sa_single_main_ui(ns, spec, timeline))
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

spatial_analysis_module_server <- function(id,
                                           spec = spatial_analysis_default_spec(),
                                           core_data,
                                           trap_data = NULL,
                                           nav_value = NULL,
                                           use_net = reactive(config$globals$use_net_data)) {
  sa_assert_supported(spec)

  analysis_editable <- sa_mode_editable(spec, "analysis_mode")
  timeline <- identical(spec$time_mode, "timeline")
  # When time_mode is editable the Timeline checkbox drives it at runtime; when it
  # is locked (e.g. the cyclic Temporal map) the timeline is intrinsic and always on.
  time_editable <- sa_mode_editable(spec, "time_mode")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Editable entry drives layout from the single "View" radio; locked presets use
    # the spec. comparison/comparison_fixed both map to "comparison".
    effective_mode <- reactive({
      if (analysis_editable && !is.null(input$view)) {
        if (identical(input$view, "single")) "single" else "comparison"
      } else {
        spec$analysis_mode
      }
    })
    # Timeline is a single toggle applying to whichever layout is active. With a
    # locked time_mode (cyclic Temporal map) there is no checkbox and it is always on.
    timeline_on <- reactive(if (time_editable) isTRUE(input$time_mode) else timeline)

    # Visibility reactives drive each panel's map-draw observer (engine `active`
    # arg). A hidden panel's analysis_dataset() then stays lazy, so it does NOT
    # recompute on mode switches (only when its selection/period actually
    # changes). The data-filtering reactives themselves stay ungated/cached.
    vis_single <- reactive(identical(effective_mode(), "single"))
    vis_comparison <- reactive(identical(effective_mode(), "comparison"))

    filtered_deps_for <- function(period) {
      reactive({
        filter_deps_by_period_names(
          core_data$deps, period$period_names(), period$start_date(),
          period$end_date(), period$period_intervals()
        )
      })
    }
    filtered_obs_for <- function(period) {
      reactive({
        filter_detection_obs(filter_obs_by_period_names(
          core_data$obs, period$period_names(), period$start_date(),
          period$end_date(), period$period_intervals()
        ))
      })
    }

    # --- period servers (both always present so selections persist) ----------
    period_left <- period_selection_module_server(
      id = "period_left", period_groups = core_data$period_groups,
      summary_output_ids = character(0),
      selected = sa_resolve_period_default(spec$sides$left$period, core_data)
    )
    period_right <- period_selection_module_server(
      id = "period_right", period_groups = core_data$period_groups,
      summary_output_ids = character(0),
      selected = sa_resolve_period_default(spec$sides$right$period, core_data)
    )

    # --- left / primary panel (always present; owns shared controls) ---------
    # Editable power entry: panel_left is the single map, so gate its draw observer
    # to single mode (it stays cached when comparison is shown). Locked presets:
    # always active (it is the only / left visible map).
    panel_left <- spatial_map_module_server(
      id = "panel_left",
      obs = filtered_obs_for(period_left),
      deps = filtered_deps_for(period_left),
      period_names = period_left$period_names,
      period_start_date = period_left$start_date,
      period_end_date = period_left$end_date,
      period_intervals = period_left$period_intervals,
      # Editable entry: the Timeline checkbox drives the timeline at runtime.
      # Locked presets keep their fixed mode.
      timeline_mode = if (analysis_editable) "reactive" else if (timeline) "always" else "none",
      timeline_active_override = if (analysis_editable) timeline_on else NULL,
      manage_density_timeline_tabs = FALSE,
      activity_pattern_mode = isTRUE(spec$temporal$activity_pattern),
      use_net = use_net,
      trap_data = trap_data,
      prefer_canvas = TRUE,
      overlay_side = "primary",
      active = if (analysis_editable) vis_single else NULL
    )

    if (isTRUE(spec$tabs$records)) {
      output_timeline_active <- if (analysis_editable) {
        timeline_on
      } else if (timeline) {
        reactive(TRUE)
      } else {
        NULL
      }
      analysis_output_module_server(id = "output_left", dataset = panel_left$dataset,
                                    timeline_active = output_timeline_active)
    }

    # Single-map height adapts to the timeline bar: add .has-timeline to the wrapper
    # when the bar is shown (shorter map, no overflow) and remove it when hidden (no
    # white space). Refresh the leaflet after the height change so tiles re-render.
    if (analysis_editable) {
      observeEvent(timeline_on(), {
        sel <- paste0("#", ns("single_map_wrap"))
        if (isTRUE(timeline_on())) {
          shinyjs::addClass(selector = sel, class = "has-timeline")
        } else {
          shinyjs::removeClass(selector = sel, class = "has-timeline")
        }
        shinyjs::delay(250, panel_left$recenter_map())
        # Sliders rendered inside a hidden bar keep a stale width; nudge a resize so
        # ionRangeSlider recomputes to fill its (grid) track once the bar is shown.
        shinyjs::delay(350, shinyjs::runjs("window.dispatchEvent(new Event('resize'));"))
      })
    }

    # Small note under Species clarifying how multiple selected species are shown,
    # driven by the gear's Species-display setting (combined vs separate).
    output$species_display_note <- renderUI({
      mode <- input[["panel_left-species_display_mode"]]
      mode <- if (is.null(mode)) "combined" else mode
      msg <- if (identical(mode, "separate")) {
        "Selected species are shown separately at each location."
      } else {
        "Selected species are combined into one count at each location."
      }
      tags$small(class = "text-muted sa-species-note", msg)
    })

    # Single-map mode heading content: Period name + Timeframe + Species,
    # mirroring the labels the comparison swipe shows on each panel. The wrapper
    # (overlay label, or block in timeline mode) is supplied by sa_single_main_ui.
    output$single_selection_heading <- renderUI({
      tagList(
        tags$h3(period_left$period_name()),
        sa_period_readout(period_left),
        sa_species_line(panel_left$selected_species(), core_data),
        sa_now_block(panel_left$timeline_status())
      )
    })

    # --- linked comparison panels --------------------------------------------
    # Distinct instances that mirror panel_left's selection via overrides and
    # differ only by period. Built up-front (not lazily) so the engine's swipe
    # init script finds their map widgets and leafletProxy marker draws land on
    # real, persistent DOM elements. Trap distance / unchecked toggles have no
    # engine return field, so link them by reading panel_left's own inputs.
    left_trap_distance <- reactive({
      value <- suppressWarnings(as.numeric(input[["panel_left-trap_locality_distance_km"]]))
      if (length(value) == 0 || is.na(value) || value < 0) 5 else value
    })
    left_unchecked_traps <- reactive({
      isTRUE(input[["panel_left-show_trap_unchecked_locations"]])
    })

    build_linked_panel <- function(panel_id, active, obs, deps,
                                   period_names, period_start_date, period_end_date, period_intervals,
                                   species_override = panel_left$selected_species,
                                   localities_override = panel_left$selected_localities,
                                   overlay_side = "comparative",
                                   scale_override = NULL,
                                   location_markers_override = panel_left$show_observation_records,
                                   prediction_surface_override = panel_left$show_predicted_rai_surface,
                                   prediction_surface_basis_override = panel_left$predicted_rai_surface_basis,
                                   trap_capture_markers_override = panel_left$show_capture_records,
                                   trap_check_frequency_override = panel_left$show_trap_check_frequency,
                                   capture_density_surface_override = panel_left$show_capture_density_surface,
                                   unchecked_traps_override = left_unchecked_traps,
                                   timeline_mode = "none",
                                   timeline_active_override = NULL) {
      spatial_map_module_server(
        id = panel_id,
        obs = obs,
        deps = deps,
        period_names = period_names,
        period_start_date = period_start_date,
        period_end_date = period_end_date,
        period_intervals = period_intervals,
        timeline_mode = timeline_mode,
        timeline_active_override = timeline_active_override,
        manage_density_timeline_tabs = FALSE,
        activity_pattern_mode = isTRUE(spec$temporal$activity_pattern),
        use_net = use_net,
        trap_data = trap_data,
        species_override = species_override,
        localities_override = localities_override,
        density_scale_max_override = scale_override,
        prediction_surface_override = prediction_surface_override,
        prediction_surface_basis_override = prediction_surface_basis_override,
        capture_density_surface_override = capture_density_surface_override,
        trap_check_frequency_override = trap_check_frequency_override,
        species_display_mode_override = panel_left$species_display_mode,
        location_markers_override = location_markers_override,
        trap_capture_markers_override = trap_capture_markers_override,
        unchecked_traps_override = unchecked_traps_override,
        trap_distance_override = left_trap_distance,
        prefer_canvas = TRUE,
        overlay_side = overlay_side,
        active = active
      )
    }

    obs_left_r <- filtered_obs_for(period_left)
    deps_left_r <- filtered_deps_for(period_left)
    obs_right_r <- filtered_obs_for(period_right)
    deps_right_r <- filtered_deps_for(period_right)

    # Per-item linking (editable power entry): each "Linked" checkbox defaults TRUE
    # (linked); unchecked -> that item is independent on the comparison side.
    link_period_r <- reactive({ v <- input[["link_period"]]; is.null(v) || isTRUE(v) })
    link_species_r <- reactive({ v <- input[["link_species"]]; is.null(v) || isTRUE(v) })
    link_monitoring_r <- reactive({ v <- input[["link_monitoring"]]; is.null(v) || isTRUE(v) })
    link_trapping_r <- reactive({ v <- input[["link_trapping"]]; is.null(v) || isTRUE(v) })

    # Per-group layer overrides for the comparison-right panel: mirror panel_left
    # when that group is linked, else return NULL so the engine reads the right
    # panel's own group inputs (the revealed comparison-side controls).
    cmp_obs_markers <- reactive(if (link_monitoring_r()) panel_left$show_observation_records() else NULL)
    cmp_surface <- reactive(if (link_monitoring_r()) panel_left$show_predicted_rai_surface() else NULL)
    cmp_surface_basis <- reactive(if (link_monitoring_r()) panel_left$predicted_rai_surface_basis() else NULL)
    cmp_capture_markers <- reactive(if (link_trapping_r()) panel_left$show_capture_records() else NULL)
    cmp_check_freq <- reactive(if (link_trapping_r()) panel_left$show_trap_check_frequency() else NULL)
    cmp_capture_density <- reactive(if (link_trapping_r()) panel_left$show_capture_density_surface() else NULL)
    cmp_unchecked <- reactive(if (link_trapping_r()) left_unchecked_traps() else NULL)

    # Right panel: period mirrors the left when Period is linked, else uses its own
    # comparison period; species mirror the left when Species is linked, else NULL
    # so the engine reads the comparison-side species input. Localities always link.
    cmp_right_obs <- reactive(if (link_period_r()) obs_left_r() else obs_right_r())
    cmp_right_deps <- reactive(if (link_period_r()) deps_left_r() else deps_right_r())
    cmp_right_pnames <- reactive(if (link_period_r()) period_left$period_names() else period_right$period_names())
    cmp_right_pstart <- reactive(if (link_period_r()) period_left$start_date() else period_right$start_date())
    cmp_right_pend <- reactive(if (link_period_r()) period_left$end_date() else period_right$end_date())
    cmp_right_pint <- reactive(if (link_period_r()) period_left$period_intervals() else period_right$period_intervals())
    cmp_right_species <- reactive(if (link_species_r()) panel_left$selected_species() else NULL)

    # Normalisation: one shared colour/size scale across both panels so the swipe
    # is directly comparable. Default on (gear toggle); returns NULL when off so
    # each side self-scales. Computed from raw data (not the dataset reactives) to
    # avoid a cycle. Both sides currently share the same record type (layers are
    # linked), so this is always a like-for-like scale; revisit when data sources
    # can be unlinked (then auto-disable for incomparable metrics).
    comparison_scale_max <- reactive({
      normalise <- if (is.null(input$normalise_scale)) TRUE else isTRUE(input$normalise_scale)
      if (!normalise) return(NULL)
      species_l <- panel_left$selected_species()
      species_r <- if (link_species_r()) species_l else input[["panel_cmp_right-selected_species"]]
      localities <- panel_left$selected_localities()
      exclude_dups <- isTRUE(input[["panel_left-exclude_possible_duplicates"]])
      right_obs <- if (link_period_r()) obs_left_r() else obs_right_r()
      right_period <- if (link_period_r()) period_left else period_right
      vals <- c()
      if (isTRUE(panel_left$show_observation_records())) {
        vals <- c(vals,
                  sa_max_location_count(obs_left_r(), species_l, localities, exclude_dups),
                  sa_max_location_count(right_obs, species_r, localities, exclude_dups))
      }
      if (!is.null(trap_data) && isTRUE(panel_left$show_capture_records())) {
        vals <- c(vals,
                  sa_max_trap_captures(trap_data, period_left, species_l, localities, left_trap_distance()),
                  sa_max_trap_captures(trap_data, right_period, species_r, localities, left_trap_distance()))
      }
      if (length(vals) == 0) return(NULL)
      m <- suppressWarnings(max(vals, na.rm = TRUE))
      if (is.na(m) || !is.finite(m) || m <= 0) NULL else m
    })

    # On the comparison side, list which fields are unlinked (i.e. differ from
    # the primary map) so the swipe is self-explanatory. Localities always link.
    sa_differs_badge <- function() {
      differs <- c(
        if (!link_period_r()) "Period",
        if (!link_species_r()) "Species",
        if (!link_monitoring_r()) "Monitoring Data",
        if (!link_trapping_r()) "Trapping Data"
      )
      if (length(differs) == 0) return(NULL)
      div(
        class = "sa-differs-note",
        icon("link-slash"),
        " Differs from primary: ",
        tags$strong(paste(differs, collapse = ", "))
      )
    }

    render_comparison_headings <- function(right_linked = NULL) {
      # Right-side species resolves linked/unlinked via the comparison panel's own
      # selected_species (locked presets link species, so fall back to the left).
      right_species <- function() {
        if (analysis_editable && !is.null(comparison_panels$panel_cmp_right)) {
          comparison_panels$panel_cmp_right$selected_species()
        } else {
          panel_left$selected_species()
        }
      }
      # Per-side current-step season/monitoring readout (NULL when timeline off).
      left_status <- function() {
        if (analysis_editable && !is.null(comparison_panels$panel_cmp_left)) {
          comparison_panels$panel_cmp_left$timeline_status()
        } else {
          panel_left$timeline_status()
        }
      }
      right_status <- function() {
        if (analysis_editable && !is.null(comparison_panels$panel_cmp_right)) {
          comparison_panels$panel_cmp_right$timeline_status()
        } else {
          NULL
        }
      }
      output$left_period_name <- renderText(period_left$period_name())
      output$left_period_readout <- renderUI(tagList(
        sa_period_readout(period_left),
        sa_species_line(panel_left$selected_species(), core_data),
        sa_now_block(left_status())
      ))
      if (is.null(right_linked)) {
        output$right_period_name <- renderText(period_right$period_name())
        output$right_period_readout <- renderUI(tagList(
          sa_period_readout(period_right),
          sa_species_line(right_species(), core_data),
          sa_now_block(right_status())
        ))
      } else {
        output$right_period_name <- renderText(
          if (right_linked()) period_left$period_name() else period_right$period_name())
        output$right_period_readout <- renderUI(tagList(
          if (right_linked()) sa_period_readout(period_left) else sa_period_readout(period_right),
          sa_species_line(right_species(), core_data),
          sa_now_block(right_status()),
          sa_differs_badge()
        ))
      }
    }

    comparison_panels <- list()
    if (analysis_editable) {
      # Power entry: single layout uses panel_left; the swipe uses distinct
      # mirrors so panel_left's map id is never duplicated in the DOM. They
      # compute only while comparison is shown.
      comparison_panels <- list(
        panel_cmp_left = build_linked_panel(
          "panel_cmp_left", vis_comparison,
          obs = obs_left_r, deps = deps_left_r,
          period_names = period_left$period_names, period_start_date = period_left$start_date,
          period_end_date = period_left$end_date, period_intervals = period_left$period_intervals,
          overlay_side = "primary",
          scale_override = comparison_scale_max,
          timeline_mode = "reactive",
          timeline_active_override = timeline_on
        ),
        panel_cmp_right = build_linked_panel(
          "panel_cmp_right", vis_comparison,
          obs = cmp_right_obs, deps = cmp_right_deps,
          period_names = cmp_right_pnames, period_start_date = cmp_right_pstart,
          period_end_date = cmp_right_pend, period_intervals = cmp_right_pint,
          species_override = cmp_right_species,
          localities_override = panel_left$selected_localities,
          scale_override = comparison_scale_max,
          location_markers_override = cmp_obs_markers,
          prediction_surface_override = cmp_surface,
          prediction_surface_basis_override = cmp_surface_basis,
          trap_capture_markers_override = cmp_capture_markers,
          trap_check_frequency_override = cmp_check_freq,
          capture_density_surface_override = cmp_capture_density,
          unchecked_traps_override = cmp_unchecked,
          timeline_mode = "reactive",
          timeline_active_override = timeline_on
        )
      )
      render_comparison_headings(right_linked = link_period_r)

      # Layout wiring as the View changes. "comparison" = swipe (re-init the clip +
      # camera sync; its outputs are suspended while hidden so the load-time init
      # can't find the maps). "comparison_fixed" = two static side-by-side maps:
      # add .is-fixed (CSS un-clips via !important + lays them in two columns) and
      # refresh both maps' size. Either way, recenter so the leaflet re-renders at
      # its new size.
      swipe_container <- paste0(ns("comparison"), "-swipe_comparison")
      observeEvent(input$view, {
        if (!identical(input$view, "comparison") && !identical(input$view, "comparison_fixed")) return()
        fixed <- identical(input$view, "comparison_fixed")
        shinyjs::runjs(sprintf(
          "var el=document.getElementById(%s); if(el){el.classList.%s('is-fixed');}",
          jsonlite::toJSON(swipe_container, auto_unbox = TRUE),
          if (fixed) "add" else "remove"
        ))
        if (!fixed) {
          shinyjs::delay(250, shinyjs::runjs(sprintf(
            "if (window.initMapSwipeComparison) { window.initMapSwipeComparison({containerId:%s, primaryMapId:%s, comparativeMapId:%s}); }",
            jsonlite::toJSON(swipe_container, auto_unbox = TRUE),
            jsonlite::toJSON(paste0(ns("panel_cmp_left"), "-map_display"), auto_unbox = TRUE),
            jsonlite::toJSON(paste0(ns("panel_cmp_right"), "-map_display"), auto_unbox = TRUE)
          )))
        }
        shinyjs::delay(300, for (panel in comparison_panels) panel$recenter_map())
      })

      # Shared-clock sync: when Period is LINKED, mirror the primary panel's
      # timeline controls onto the (hidden) comparison panel so both maps step
      # together. When Period is UNLINKED each panel shows its own bar and runs an
      # independent clock, so we do not sync.
      observeEvent(input[["panel_cmp_left-time_slider"]], {
        if (timeline_on() && link_period_r()) {
          updateSliderInput(session, "panel_cmp_right-time_slider",
                            value = input[["panel_cmp_left-time_slider"]])
        }
      }, ignoreInit = TRUE)
      observeEvent(input[["panel_cmp_left-timeline_step_size"]], {
        if (link_period_r()) {
          updateSelectInput(session, "panel_cmp_right-timeline_step_size",
                            selected = input[["panel_cmp_left-timeline_step_size"]])
        }
      }, ignoreInit = TRUE)
      observeEvent(input[["panel_cmp_left-timeline_view_mode"]], {
        if (link_period_r()) {
          updateRadioButtons(session, "panel_cmp_right-timeline_view_mode",
                             selected = input[["panel_cmp_left-timeline_view_mode"]])
        }
      }, ignoreInit = TRUE)
    } else if (identical(spec$analysis_mode, "comparison")) {
      # Locked comparison preset: panel_left is the left side; add the right.
      comparison_panels <- list(build_linked_panel(
        "panel_right", NULL,
        obs = obs_right_r, deps = deps_right_r,
        period_names = period_right$period_names, period_start_date = period_right$start_date,
        period_end_date = period_right$end_date, period_intervals = period_right$period_intervals
      ))
      render_comparison_headings()
    }

    # --- recenter (invalidateSize + fitBounds) -------------------------------
    recenter_all <- function() {
      panel_left$recenter_map()
      for (panel in comparison_panels) panel$recenter_map()
    }
    if (!is.null(nav_value)) {
      observeEvent(session$rootScope()$input$nav, {
        if (identical(session$rootScope()$input$nav, nav_value)) recenter_all()
      }, ignoreInit = FALSE)
    }
    observeEvent(effective_mode(), recenter_all(), ignoreInit = TRUE)
    observeEvent(input[["tabs"]], {
      if (identical(input[["tabs"]], "map")) recenter_all()
    }, ignoreInit = TRUE)
    observeEvent(input[["comparison-density_comparison_tabs"]], {
      if (identical(input[["comparison-density_comparison_tabs"]], "map")) recenter_all()
    }, ignoreInit = TRUE)

    invisible(list(panel_left = panel_left, period_left = period_left))
  })
}
