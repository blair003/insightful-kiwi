# R/modules/mapping_module.R

playback_timeline_ui <- function(ns, min, max, value, step) {
  timezone <- playback_actual_timezone()

  div(
    class = "playback-timeline",
    div(
      class = "playback-transport",
      actionButton(ns("play_btn"), "Play", icon = icon("play"), class = "btn-success playback-transport-btn"),
      shinyjs::disabled(
        actionButton(ns("pause_btn"), "Pause", icon = icon("pause"), class = "btn-warning playback-transport-btn")
      )
    ),
    div(
      class = "playback-slider",
      sliderInput(
        inputId = ns("time_slider"),
        label = "Time progression",
        min = min,
        max = max,
        value = value,
        step = step,
        width = "100%",
        ticks = FALSE,
        timezone = timezone
      )
    )
  )
}

playback_actual_timezone <- function() {
  if (exists("weather_playback_timezone", mode = "function", inherits = TRUE)) {
    return(weather_playback_timezone())
  }

  if (exists("config", inherits = TRUE) &&
      !is.null(config$globals$actual_timezone) &&
      nzchar(config$globals$actual_timezone)) {
    return(config$globals$actual_timezone)
  }

  "Pacific/Auckland"
}

playback_as_posix <- function(value) {
  as.POSIXct(value, tz = playback_actual_timezone())
}

playback_window_midpoint <- function(start_time, current_time) {
  if (is.null(start_time) || is.null(current_time) || is.na(start_time) || is.na(current_time)) {
    return(current_time)
  }

  as.POSIXct(
    mean(c(as.numeric(start_time), as.numeric(current_time))),
    origin = "1970-01-01",
    tz = playback_actual_timezone()
  )
}

playback_window_reference_time <- function(start_time, current_time, step_size, view_mode = "single") {
  if (identical(view_mode, "single") && step_size %in% c("day_night", "diel")) {
    return(playback_window_midpoint(start_time, current_time))
  }

  current_time
}

playback_time_info_for_window <- function(weather_df, start_time, current_time, step_size, view_mode = "single") {
  reference_time <- playback_window_reference_time(start_time, current_time, step_size, view_mode)
  time_of_day_info(weather_df, reference_time, step_size)
}

playback_format_time <- function(value) {
  format(as.POSIXct(value, tz = playback_actual_timezone()), "%Y-%m-%d %H:%M:%S", tz = playback_actual_timezone())
}

playback_format_date <- function(value) {
  format(as.Date(value, tz = playback_actual_timezone()), "%d %b %Y")
}

playback_monitoring_periods <- function(period_groups) {
  if (is.null(period_groups) || length(period_groups) == 0) {
    return(data.frame())
  }

  period_names <- names(period_groups)
  period_names <- period_names[period_names != "ALL"]
  if (length(period_names) == 0) {
    return(data.frame())
  }

  periods <- lapply(period_names, function(period_name) {
    period <- period_groups[[period_name]]
    if (is.null(period$start_date) || is.null(period$end_date)) {
      return(NULL)
    }
    if (!is.null(period$assign_period) && !isTRUE(period$assign_period)) {
      return(NULL)
    }

    data.frame(
      name = period_name,
      start = as.POSIXct(as.Date(period$start_date), tz = playback_actual_timezone()),
      end = as.POSIXct(as.Date(period$end_date) + 1, tz = playback_actual_timezone()) - 1,
      stringsAsFactors = FALSE
    )
  })

  periods <- Filter(Negate(is.null), periods)
  if (length(periods) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(periods) %>%
    dplyr::filter(!is.na(start), !is.na(end)) %>%
    dplyr::arrange(start, end)
}

playback_period_status <- function(current_time, period_groups) {
  current_time <- as.POSIXct(current_time, tz = playback_actual_timezone())
  periods <- playback_monitoring_periods(period_groups)

  if (is.null(current_time) || is.na(current_time) || nrow(periods) == 0) {
    return(list(
      in_period = FALSE,
      label = "No cameras deployed",
      detail = "No monitoring seasons available",
      next_start = NULL,
      next_name = NULL
    ))
  }

  current_period <- periods %>%
    dplyr::filter(start <= current_time, end >= current_time) %>%
    dplyr::slice(1)

  next_period <- periods %>%
    dplyr::filter(start > current_time) %>%
    dplyr::slice(1)

  if (nrow(current_period) > 0) {
    return(list(
      in_period = TRUE,
      label = current_period$name[[1]],
      detail = sprintf(
        "%s to %s",
        playback_format_date(current_period$start[[1]]),
        playback_format_date(current_period$end[[1]])
      ),
      current_end = current_period$end[[1]],
      next_start = if (nrow(next_period) > 0) next_period$start[[1]] else NULL,
      next_name = if (nrow(next_period) > 0) next_period$name[[1]] else NULL
    ))
  }

  list(
    in_period = FALSE,
    label = "No cameras deployed",
    detail = if (nrow(next_period) > 0) {
      sprintf("Next: %s from %s", next_period$name[[1]], playback_format_date(next_period$start[[1]]))
    } else {
      "Outside monitoring seasons"
    },
    next_start = if (nrow(next_period) > 0) next_period$start[[1]] else NULL,
    next_name = if (nrow(next_period) > 0) next_period$name[[1]] else NULL
  )
}

render_playback_period_control <- function(period_status) {
  if (is.null(period_status)) {
    return(NULL)
  }

  state_class <- if (isTRUE(period_status$in_period)) "is-active" else "is-gap"
  sprintf(
    "<div class='map-season-badge %s'><span>%s</span><small>%s</small></div>",
    state_class,
    weather_html_escape(period_status$label),
    weather_html_escape(period_status$detail)
  )
}

render_playback_skip_notice <- function(skip_notice) {
  if (is.null(skip_notice) || is.null(skip_notice$message)) {
    return(NULL)
  }

  sprintf(
    "<div class='map-playback-skip-badge'><strong>%s</strong><small>%s</small></div>",
    weather_html_escape(skip_notice$message),
    weather_html_escape("Playback resumes in 3 seconds")
  )
}

playback_gap_skip_target <- function(current_time, next_time, playback_end, period_groups) {
  status <- playback_period_status(current_time, period_groups)
  if (is.null(status$next_start) || is.na(status$next_start)) {
    return(NULL)
  }

  next_start <- as.POSIXct(status$next_start, tz = playback_actual_timezone())
  if (next_start > playback_end) {
    return(NULL)
  }

  if (!isTRUE(status$in_period) && next_start > current_time) {
    return(list(
      target = next_start,
      next_name = status$next_name
    ))
  }

  if (isTRUE(status$in_period) &&
      !is.null(status$current_end) &&
      next_time > status$current_end &&
      next_start > status$current_end) {
    return(list(
      target = next_start,
      next_name = status$next_name
    ))
  }

  NULL
}

playback_next_season_time <- function(current_time, playback_end, period_groups) {
  current_time <- as.POSIXct(current_time, tz = playback_actual_timezone())
  playback_end <- as.POSIXct(playback_end, tz = playback_actual_timezone())
  periods <- playback_monitoring_periods(period_groups)

  if (is.null(current_time) || is.na(current_time) ||
      is.null(playback_end) || is.na(playback_end) ||
      nrow(periods) == 0) {
    return(playback_end)
  }

  candidate <- periods %>%
    dplyr::filter(.data$end > current_time, .data$start <= playback_end) %>%
    dplyr::slice(1)

  if (nrow(candidate) == 0) {
    return(playback_end)
  }

  min(candidate$end[[1]], playback_end)
}

playback_current_season_start <- function(current_time, period_groups, fallback_start) {
  current_time <- as.POSIXct(current_time, tz = playback_actual_timezone())
  fallback_start <- as.POSIXct(fallback_start, tz = playback_actual_timezone())
  periods <- playback_monitoring_periods(period_groups)

  if (is.null(current_time) || is.na(current_time) || nrow(periods) == 0) {
    return(fallback_start)
  }

  candidate <- periods %>%
    dplyr::filter(.data$start <= current_time, .data$end >= current_time) %>%
    dplyr::slice(1)

  if (nrow(candidate) == 0) {
    return(fallback_start)
  }

  max(candidate$start[[1]], fallback_start)
}

playback_window_readout <- function(start_time, current_time, step_size, weather_df, view_mode = "single") {
  if (is.null(start_time) || is.null(current_time) || is.na(start_time) || is.na(current_time)) {
    return(NULL)
  }

  time_info <- playback_time_info_for_window(weather_df, start_time, current_time, step_size, view_mode)
  period_text <- if (!identical(view_mode, "single")) {
    "Cumulative playback"
  } else if (!is.null(time_info) && !is.null(time_info$label)) {
    paste0(weather_html_escape(time_info$label), " window")
  } else {
    "Current window"
  }

  sprintf(
    "<div class='playback-window-readout'><strong>%s:</strong> %s to %s <span>%s</span></div>",
    period_text,
    weather_html_escape(playback_format_time(start_time)),
    weather_html_escape(playback_format_time(current_time)),
    weather_html_escape(playback_actual_timezone())
  )
}

mapping_module_ui <- function(id,
                              view = "map",
                              choices,
                              selected = NULL,
                              multiple = TRUE,
                              label = "Species selection:",
                              include_prediction_option = TRUE,
                              include_marker_options = include_prediction_option,
                              prediction_cumulative_only = FALSE,
                              map_height = config$globals$leaflet_height) { # Added map_height
  ns <- NS(id)
  
  if (view == "select_species") {
    return(
      tagList(
        selectizeInput(
          inputId = ns("selected_species"),
          label = tagList(icon("paw"), label),
          choices = choices,
          selected = selected,
          multiple = multiple,
          options = list(
            placeholder = "Select species...",
            closeAfterSelect = TRUE
          )
        ),
        # For density map, this note is relevant.
        # For observation map, species are shown separately.
       # if (type == "density") { # Assuming 'type' can be inferred or passed for UI context
          tags$small("Note: Selected species will be combined, not shown separately.")
      #  }
      )
    )
  } else if (view == "select_localities") {
    return(
      tagList(
        selectInput(
          inputId = ns("selected_localities"),
          label = tagList(icon("location-dot"), "Locality selection:"),
          choices = choices,
          selected = selected,
          multiple = multiple,
          selectize = TRUE
        )
      )
    )
  } else if (view == "density_options") {
    prediction_condition <- if (isTRUE(prediction_cumulative_only)) {
      "input.playback_view_mode === 'cumulative'"
    } else {
      "true"
    }
    prediction_basis_condition <- if (isTRUE(prediction_cumulative_only)) {
      "input.playback_view_mode === 'cumulative' && input.show_predicted_rai_surface"
    } else {
      "input.show_predicted_rai_surface"
    }
    prediction_help_text <- if (isTRUE(prediction_cumulative_only)) {
      "Predicted surface uses IDW interpolation within the monitored footprint and updates monthly during cumulative playback."
    } else {
      "Predicted surface uses IDW interpolation within the monitored footprint."
    }

    return(
      tagList(
        checkboxInput(
          inputId = ns("exclude_possible_duplicates"),
          label = "Exclude possible duplicates",
          value = isTRUE(config$globals$use_net_data)
        ),
        if (isTRUE(include_prediction_option) || isTRUE(include_marker_options)) {
          tags$hr()
        },
        if (isTRUE(include_marker_options)) {
          checkboxInput(
            inputId = ns("show_density_location_markers"),
            label = "Show camera markers",
            value = TRUE
          )
        },
        if (isTRUE(include_marker_options)) {
          conditionalPanel(
            condition = "input.show_density_location_markers",
            ns = ns,
            radioButtons(
              inputId = ns("density_marker_metric"),
              label = "Marker value:",
              choices = c("Counts" = "count", "Line RAI" = "rai"),
              selected = "count",
              inline = TRUE
            )
          )
        },
        if (isTRUE(include_prediction_option)) {
          conditionalPanel(
            condition = prediction_condition,
            ns = ns,
            checkboxInput(
              inputId = ns("show_predicted_rai_surface"),
              label = "Show predicted RAI surface",
              value = FALSE
            )
          )
        },
        if (isTRUE(include_prediction_option)) {
          conditionalPanel(
            condition = prediction_basis_condition,
            ns = ns,
            radioButtons(
              inputId = ns("predicted_rai_surface_basis"),
              label = "Surface basis:",
              choices = c(
                "Location-weighted line RAI" = "weighted_line_rai",
                "Line RAI" = "line_rai"
              ),
              selected = "weighted_line_rai"
            )
          )
        },
        if (isTRUE(include_prediction_option)) {
          conditionalPanel(
            condition = prediction_condition,
            ns = ns,
            tags$small(prediction_help_text)
          )
        }
      )
    )
  } else if (view == "select_observation_map_options") { # New view for observation map specific options
    trap_data_available <- exists("trap_data", inherits = TRUE) &&
      !is.null(get("trap_data", inherits = TRUE))

    return(
      tagList(
        checkboxInput(
          inputId = ns("exclude_possible_duplicates"),
          label = "Exclude possible duplicates",
          value = isTRUE(config$globals$use_net_data)
        ),
        checkboxInput(
          inputId = ns("enhance_map_details"),
          label = "Show monitoring area"
        ),
        if (isTRUE(trap_data_available)) {
          checkboxInput(
            inputId = ns("include_trap_data"),
            label = "Include trapping data",
            value = FALSE
          )
        }
      )
    )
  } else if (view == "density_playback_controls") {
    return(
      tagList(
        hr(),
        div(class = "sidebar_heading", "PLAYBACK SETTINGS"),
        div(
          class = "playback-reset-row",
          actionButton(ns("reset_btn"), "Reset progression", icon = icon("rotate-left"), class = "btn-outline-danger")
        ),
        div(
          class = "playback-speed-control",
          sliderInput(
            inputId = ns("playback_speed"),
            label = "Playback speed",
            min = 0, max = 4, value = 1, step = 0.25
          ),
          div(
            class = "playback-speed-labels",
            tags$span("Faster"), tags$span("Slower")
          )
        ),
        selectInput(
          inputId = ns("playback_step_size"),
          label = "Playback increments",
          choices = c(
            "Hourly" = "hour",
            "Daily - Day/Night" = "day_night",
            "Diel activity" = "diel",
            "Daily" = "day",
            "Weekly" = "week",
            "Monthly" = "month",
            "Season" = "season"
          ),
          selected = "day"
        ),
        checkboxInput(
          inputId = ns("skip_playback_gaps"),
          label = "Skip gaps in monitoring data",
          value = TRUE
        ),
        if (isTRUE(include_marker_options)) {
          checkboxInput(
            inputId = ns("show_density_location_markers"),
            label = "Show camera markers",
            value = TRUE
          )
        },
        if (isTRUE(include_marker_options)) {
          conditionalPanel(
            condition = "input.show_density_location_markers",
            ns = ns,
            radioButtons(
              inputId = ns("density_marker_metric"),
              label = "Marker value:",
              choices = c("Counts" = "count", "Line RAI" = "rai"),
              selected = "count",
              inline = TRUE
            )
          )
        },
        radioButtons(
          inputId = ns("playback_view_mode"),
          label = "View mode:",
          choices = c("Single period" = "single", "Cumulative" = "cumulative"),
          selected = "cumulative"
        ),
        if (isTRUE(include_prediction_option)) {
          conditionalPanel(
            condition = "input.playback_view_mode === 'cumulative'",
            ns = ns,
            checkboxInput(
              inputId = ns("show_predicted_rai_surface"),
              label = "Show predicted RAI surface",
              value = FALSE
            )
          )
        },
        if (isTRUE(include_prediction_option)) {
          conditionalPanel(
            condition = "input.playback_view_mode === 'cumulative' && input.show_predicted_rai_surface",
            ns = ns,
            radioButtons(
              inputId = ns("predicted_rai_surface_basis"),
              label = "Surface basis:",
              choices = c(
                "Location-weighted line RAI" = "weighted_line_rai",
                "Line RAI" = "line_rai"
              ),
              selected = "weighted_line_rai"
            ),
            tags$small("Predicted surface uses IDW interpolation within the monitored footprint and updates monthly during cumulative playback.")
          )
        }
      )
    )
  } else if (view == "map") { # This is for the density map
    return(
      tagList(
        leafletOutput(ns("map_display"), height = map_height)
      )
    )
  } else if (view == "density_playback_layout") {
    return(
      tagList(
        div(
          class = "playback-time-slider",
          uiOutput(ns("playback_slider_ui")),
          uiOutput(ns("playback_window_ui"))
        ),
        navset_tab(
          id = ns("density_playback_tabs"),
          nav_panel(
            "Map",
            leafletOutput(ns("map_display"), height = map_height),
            value = "map"
          ),
          nav_panel(
            "Data",
            h3("Observations data in current window"),
            DT::dataTableOutput(ns("playback_data_table")),
            value = "data"
          )
        )
      )
    )
  } else if (view == "observation_map_layout") { # New view for the observation map page layout
    return(
      tagList(
        div(
          class = "playback-time-slider",
          uiOutput(ns("playback_slider_ui")),
          uiOutput(ns("playback_window_ui"))
        ),
        navset_tab(
          id = ns("observation_map_tabs"),
          selected = ns("map_tab"),
          nav_panel(
            "Map",
            leafletOutput(ns("map_display"), height = map_height), # Distinct ID for this map
            uiOutput(ns("observation_map_textoverlay_warning")), # For warnings
            value = ns("map_tab")
          ),
          nav_panel(
            "Data",
            h3("Browse observations shown on the map"),
            p(
              "This table shows the observations in the current playback window."
            ),
            DT::dataTableOutput(ns("observation_data_table")),
            value = ns("data_tab")
          ),
          nav_panel(
            "Cumulative Data",
            h3("Browse cumulative observations"),
            p(
              "This table shows observations from the start of the selected season up to the current playback time."
            ),
            DT::dataTableOutput(ns("observation_cumulative_data_table")),
            value = ns("cumulative_data_tab")
          )
        )
      )
    )
  } else if (view == "summary") { # Generic summary output, used by density map
    return(
      tagList(
        uiOutput(ns("obs_summary"))
      )
    )
  } else if (view == "observation_map_sidebar_summary") { # Specific summary for observation map sidebar
    return(
      tagList(
        uiOutput(ns("observation_map_sidebar_summary_content"))
      )
    )
  }
}


mapping_module_server <- function(id,
                                  type = "density", # "density" or "observation"
                                  obs = NULL, # Reactive: obs filtered by period
                                  deps = NULL, # Reactive: deps filtered by period
                                  species_override = NULL, # Reactive: for comparative density map
                                  localities_override = NULL, # Reactive: for comparative density map
                                  prediction_surface_override = NULL, # Reactive: for comparative density map
                                  prediction_surface_basis_override = NULL, # Reactive: for comparative density map
                                  location_markers_override = NULL, # Reactive: for comparative density map
                                  marker_metric_override = NULL, # Reactive: for comparative density map
                                  period_start_date = NULL, # Reactive: e.g. primary_period$start_date
                                  period_end_date = NULL,    # Reactive: e.g. primary_period$end_date
                                  playback_mode = c("none", "always"),
                                  enable_map_outputs = TRUE,
                                  use_net = reactive(config$globals$use_net_data),
                                  trap_data = NULL
) {
  playback_mode <- match.arg(playback_mode, choices = c("none", "always"))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    logger::log_debug(sprintf("mapping_module_server, %s moduleServer() running for type: %s", id, type))
    
    MAP_ID <- ns("map_display") 
    current_bounds <- reactiveVal(NULL) # Unified reactiveVal for map bounds
    needs_fit_bounds <- reactiveVal(FALSE)
    predicted_rai_surface_cache <- reactiveVal(NULL)
    last_density_map_update_key <- reactiveVal(NULL)

    observeEvent(use_net(), {
      updateCheckboxInput(
        session,
        "exclude_possible_duplicates",
        value = isTRUE(use_net())
      )
    }, ignoreInit = FALSE)

    exclude_possible_duplicates_selected <- reactive({
      if (is.null(input$exclude_possible_duplicates)) {
        return(isTRUE(use_net()))
      }

      isTRUE(input$exclude_possible_duplicates)
    })

    show_predicted_rai_surface_selected <- reactive({
      if (playback_active() && !identical(input$playback_view_mode, "cumulative")) {
        return(FALSE)
      }

      if (type == "density" &&
          !is.null(prediction_surface_override) &&
          !is.null(prediction_surface_override())) {
        return(isTRUE(prediction_surface_override()))
      }

      isTRUE(input$show_predicted_rai_surface)
    })

    predicted_rai_surface_basis_selected <- reactive({
      if (type == "density" &&
          !is.null(prediction_surface_basis_override) &&
          !is.null(prediction_surface_basis_override())) {
        basis <- prediction_surface_basis_override()
      } else {
        basis <- input$predicted_rai_surface_basis
      }

      if (is.null(basis) || !basis %in% c("line_rai", "weighted_line_rai")) {
        return("line_rai")
      }

      basis
    })

    show_density_location_markers_selected <- reactive({
      if (type == "density" &&
          !is.null(location_markers_override) &&
          !is.null(location_markers_override())) {
        return(isTRUE(location_markers_override()))
      }

      if (is.null(input$show_density_location_markers)) {
        return(TRUE)
      }

      isTRUE(input$show_density_location_markers)
    })

    density_marker_metric_selected <- reactive({
      if (type == "density" &&
          !is.null(marker_metric_override) &&
          !is.null(marker_metric_override())) {
        metric <- marker_metric_override()
      } else {
        metric <- input$density_marker_metric
      }

      if (is.null(metric) || !metric %in% c("count", "rai")) {
        return("count")
      }

      metric
    })
    
    # --- Common Reactives ---
    current_selected_species <- reactive({
      # For density map, species_override can be used for the comparative map
      if (type == "density" && !is.null(species_override) && !is.null(species_override())) {
        logger::log_debug(sprintf("mapping_module_server [density], %s species_override provided.", id))
        species_override()
      } else {
        logger::log_debug(sprintf("mapping_module_server [%s], %s using input$selected_species.", type, id))
        req(input$selected_species)
        as.character(input$selected_species)
      }
    })
    
    current_selected_localities <- reactive({
      if (type == "density" && !is.null(localities_override) && !is.null(localities_override())) {
        logger::log_debug(sprintf("mapping_module_server [density], %s localities_override provided.", id))
        localities_override()
      } else if (type == "observation") {
        if (!is.null(input$selected_localities)) { # This is ns("selected_localities")
          logger::log_debug(sprintf("mapping_module_server [observation], %s using input$selected_localities: %s", id, paste(input$selected_localities, collapse=", ")))
          as.character(input$selected_localities)
        } #else {
        #  # Fallback to all unique localities from the period's deployments if no explicit selection
        #  req(deps())
         # unique(deps()$locality)
       # }
      } else { # density type, primary map
        logger::log_debug(sprintf("mapping_module_server [density], %s using input$selected_localities.", id))
        req(input$selected_localities)
        as.character(input$selected_localities)
      }
    })
    
    # --- Generalized Helper Functions ---
    
    #' Calculate Map Bounds from Active Locations
    #'
    #' @param active_locs_df Data frame of active locations, must contain 'longitude' and 'latitude'.
    #' @param context_id_log The module ID for logging.
    #' @param type_log The map type ("density" or "observation") for logging.
    #' @return A list with min_lng, max_lng, min_lat, max_lat, or NULL if bounds cannot be calculated.
    calculate_bounds_from_locations <- function(active_locs_df, context_id_log, type_log) {
      if (nrow(active_locs_df) > 0 &&
          all(c("longitude", "latitude") %in% names(active_locs_df)) &&
          !any(is.na(active_locs_df$longitude)) && !any(is.na(active_locs_df$latitude))) {
        min_lng <- min(active_locs_df$longitude, na.rm = TRUE)
        max_lng <- max(active_locs_df$longitude, na.rm = TRUE)
        min_lat <- min(active_locs_df$latitude, na.rm = TRUE)
        max_lat <- max(active_locs_df$latitude, na.rm = TRUE)
        if(all(!is.infinite(c(min_lng, max_lng, min_lat, max_lat)))){
          return(list(min_lng = min_lng, max_lng = max_lng, min_lat = min_lat, max_lat = max_lat))
        } else {
          logger::log_warn(sprintf("mapping_module_server [%s], ID: %s - Invalid (Infinite) bounds calculated.", type_log, context_id_log))
          return(NULL)
        }
      } else {
        logger::log_warn(sprintf("mapping_module_server [%s], ID: %s - Insufficient data for bounds calculation (active_locs_df empty or missing lat/lon).", type_log, context_id_log))
        return(NULL)
      }
    }
    
    #' Invalidate Map Size
    #' (Targets the unified MAP_ID)
    invalidate_map_size <- function() {
      logger::log_debug(sprintf("mapping_module_server [%s], ID: %s calling invalidateSize for map: %s", type, id, MAP_ID))
      map_id_selector_json <- jsonlite::toJSON(paste0("#", MAP_ID), auto_unbox = TRUE)
      map_id_json <- jsonlite::toJSON(MAP_ID, auto_unbox = TRUE)
      type_json <- jsonlite::toJSON(type, auto_unbox = TRUE)
      shinyjs::runjs(sprintf(
        'setTimeout(function() {
           var mapWidget = HTMLWidgets.find(%s);
           if (mapWidget) {
             var mapObj = mapWidget.getMap();
             if (mapObj) {
               mapObj.invalidateSize();
               console.log("invalidateSize() called on " + %s + " map: " + %s);
             } else { console.error("Leaflet map object not found for " + %s + " map: " + %s); }
           } else { console.error("Map widget not found for " + %s + " map: " + %s); }
         }, 100);', map_id_selector_json, type_json, map_id_json, type_json, map_id_json, type_json, map_id_json
      ))
    }
    
    #' Apply Fit Bounds to Map
    #' (Uses current_bounds() and targets MAP_ID)
    apply_map_fit_bounds <- function() {
      bounds_val <- current_bounds() # Uses the unified bounds reactiveVal
      logger::log_debug(sprintf("mapping_module_server [%s], ID: %s apply_map_fit_bounds. Bounds are: %s", type, id, paste(capture.output(str(bounds_val)), collapse=",")))
      if (!is.null(bounds_val) && !is.null(bounds_val$min_lng) && !is.na(bounds_val$min_lng)) { # Added NA check for robustness
        leafletProxy(MAP_ID) %>%
          fitBounds(
            lng1 = bounds_val$min_lng, lat1 = bounds_val$min_lat,
            lng2 = bounds_val$max_lng, lat2 = bounds_val$max_lat
          )
        logger::log_info(sprintf("mapping_module_server [%s], ID: %s fitBounds applied to %s map.", type, id, type))
      } else {
        logger::log_warn(sprintf("mapping_module_server [%s], ID: %s no valid bounds for fitBounds on %s map.", type, id, type))
      }
    }
    
    #' Recenter Map (Generic)
    #' (Calls invalidate_map_size and apply_map_fit_bounds)
    recenter_map_generic <- function() {
      logger::log_info(sprintf("mapping_module_server [%s], ID: %s recenter_map_generic called.", type, id))
      invalidate_map_size()
      shinyjs::delay(150, { # Delay as in original code
        apply_map_fit_bounds()
      })
    }

    register_map_resize_handler <- function() {
      map_id_selector_json <- jsonlite::toJSON(paste0("#", MAP_ID), auto_unbox = TRUE)
      resize_input_id_json <- jsonlite::toJSON(ns("map_resize"), auto_unbox = TRUE)
      handler_name_json <- jsonlite::toJSON(paste0("insightfulKiwiMapResize_", gsub("[^A-Za-z0-9_]", "_", MAP_ID)), auto_unbox = TRUE)
      shinyjs::runjs(sprintf(
        '(function() {
           var selector = %s;
           var inputId = %s;
           var handlerName = %s;
           if (window[handlerName]) {
             window.removeEventListener("resize", window[handlerName]);
           }
           var timer = null;
           window[handlerName] = function() {
             clearTimeout(timer);
             timer = setTimeout(function() {
               var element = document.querySelector(selector);
               if (window.Shiny && element && element.offsetParent !== null) {
                 Shiny.setInputValue(inputId, Date.now(), {priority: "event"});
               }
             }, 200);
           };
           window.addEventListener("resize", window[handlerName]);
         })();',
        map_id_selector_json,
        resize_input_id_json,
        handler_name_json
      ))
    }
    
    # --- Unified Map Output ---
    # This single output will be used for both density and observation maps.
    # The content will be updated by type-specific observers.
    output$map_display <- renderLeaflet({
      logger::log_debug(sprintf("mapping_module_server [%s], %s renderLeaflet for unified map_display: %s", type, id, MAP_ID))
      leaflet() %>% addTiles(options = tileOptions(crossOrigin = TRUE))
    })
    outputOptions(output, "map_display", suspendWhenHidden = FALSE)
    register_map_resize_handler()
    observeEvent(input$map_resize, {
      recenter_map_generic()
    }, ignoreInit = TRUE)
    
    
    # --- Density Map Specific Logic ---
    if (type == "density") {
      is_playing <- reactiveVal(FALSE)
      playback_gap_notice <- reactiveVal(NULL)
      playback_skip_resume_id <- reactiveVal(0L)

      playback_active <- reactive({
        identical(playback_mode, "always")
      })

      skip_playback_gaps <- reactive({
        isTRUE(input$skip_playback_gaps)
      })

      playback_period_groups <- reactive({
        if (exists("core_data", inherits = TRUE) && !is.null(core_data$period_groups)) {
          return(core_data$period_groups)
        }

        NULL
      })

      floor_posix_hour <- function(value) {
        value <- as.POSIXct(value, tz = playback_actual_timezone())
        as.POSIXct(
          floor(as.numeric(value) / 3600) * 3600,
          origin = "1970-01-01",
          tz = playback_actual_timezone()
        )
      }

      ceiling_posix_hour <- function(value) {
        value <- as.POSIXct(value, tz = playback_actual_timezone())
        as.POSIXct(
          ceiling(as.numeric(value) / 3600) * 3600,
          origin = "1970-01-01",
          tz = playback_actual_timezone()
        )
      }

      playback_bounds <- function(start, end) {
        start <- floor_posix_hour(start)
        end <- ceiling_posix_hour(end)

        if (is.na(start) || is.na(end)) {
          return(NULL)
        }

        if (end <= start) {
          end <- start + 3600
        }

        list(start = start, end = end)
      }

      playback_observation_bounds <- reactive({
        obs_data <- obs()

        fallback_start <- if (is.function(period_start_date)) {
          as.POSIXct(period_start_date(), tz = playback_actual_timezone())
        } else {
          suppressWarnings(as.POSIXct(min(obs_data$timestamp, na.rm = TRUE), tz = playback_actual_timezone()))
        }

        fallback_end <- if (is.function(period_end_date)) {
          as.POSIXct(as.Date(period_end_date()) + 1, tz = playback_actual_timezone()) - 1
        } else {
          suppressWarnings(as.POSIXct(max(obs_data$timestamp, na.rm = TRUE), tz = playback_actual_timezone()))
        }

        req(fallback_start, fallback_end)

        period_obs <- obs_data %>%
          dplyr::filter(
            as.Date(timestamp) >= as.Date(fallback_start),
            as.Date(timestamp) <= as.Date(fallback_end),
            !is.na(timestamp)
          )

        if (nrow(period_obs) > 0) {
          return(playback_bounds(
            min(period_obs$timestamp, na.rm = TRUE),
            max(period_obs$timestamp, na.rm = TRUE)
          ))
        }

        playback_bounds(fallback_start, fallback_end)
      })

      playback_period_start <- reactive({
        req(playback_observation_bounds())
        playback_observation_bounds()$start
      })

      playback_period_end <- reactive({
        req(playback_observation_bounds())
        playback_observation_bounds()$end
      })

      playback_weather_data <- reactive({
        req(playback_period_start(), playback_period_end(), current_selected_localities(), deps())
        active_locations <- deps() %>%
          dplyr::filter(locality %in% current_selected_localities()) %>%
          dplyr::distinct(locationID, locality, .keep_all = TRUE)

        playback_weather_for_deployments(
          active_locations,
          as.Date(playback_period_start()),
          as.Date(playback_period_end())
        )
      })

      playback_step_seconds <- reactive({
        step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
        switch(step_size,
          "hour" = 3600,
          "day_night" = 3600,
          "diel" = 3600,
          "day" = 86400,
          "week" = 604800,
          "month" = 2592000,
          "season" = 604800,
          86400
        )
      })

      playback_initial_value <- reactive({
        req(playback_period_start(), playback_period_end())
        step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
        if (identical(input$playback_view_mode, "single")) {
          if (step_size %in% c("day_night", "diel")) {
            return(min(
              next_weather_boundary(playback_period_start(), playback_period_end(), playback_weather_data(), step_size),
              playback_period_end()
            ))
          }
          if (identical(step_size, "season")) {
            return(playback_next_season_time(playback_period_start(), playback_period_end(), playback_period_groups()))
          }
          min(playback_period_start() + playback_step_seconds(), playback_period_end())
        } else {
          playback_period_start()
        }
      })

      reset_playback_slider <- function() {
        req(playback_period_start(), playback_period_end())
        updateSliderInput(session, "time_slider", value = playback_initial_value())
      }

      output$playback_slider_ui <- renderUI({
        req(playback_active(), playback_period_start(), playback_period_end())
        playback_timeline_ui(
          ns = ns,
          min = playback_period_start(),
          max = playback_period_end(),
          value = playback_initial_value(),
          step = playback_step_seconds()
        )
      })

      set_playback_button_state <- function(playing) {
        if (!playback_active()) {
          return()
        }

        if (isTRUE(playing)) {
          shinyjs::disable(selector = paste0("#", ns("play_btn")))
          shinyjs::enable(selector = paste0("#", ns("pause_btn")))
        } else {
          shinyjs::enable(selector = paste0("#", ns("play_btn")))
          shinyjs::disable(selector = paste0("#", ns("pause_btn")))
        }
      }

      send_playback_timer <- function() {
        if (!playback_active()) {
          return()
        }

        playback_speed <- if (is.null(input$playback_speed)) 1 else input$playback_speed
        interval_ms <- max(50, as.numeric(playback_speed) * 1000)
        session$sendCustomMessage(
          "densityPlaybackTimer",
          list(
            id = ns("playback_tick"),
            enabled = isTRUE(is_playing() && playback_active()),
            interval = interval_ms
          )
        )
      }

      observe({
        set_playback_button_state(is_playing())
        send_playback_timer()
      })

      observeEvent(input$play_btn, {
        req(playback_active())
        is_playing(TRUE)
      })

      observeEvent(input$pause_btn, {
        playback_skip_resume_id(playback_skip_resume_id() + 1L)
        playback_gap_notice(NULL)
        is_playing(FALSE)
      })

      observeEvent(input$show_predicted_rai_surface, {
        req(playback_active())
        if (isTRUE(input$show_predicted_rai_surface)) {
          current_speed <- if (is.null(input$playback_speed)) 1 else as.numeric(input$playback_speed)
          if (is.finite(current_speed) && current_speed < 2) {
            updateSliderInput(session, "playback_speed", value = 2)
          }
          current_step <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
          if (current_step %in% c("hour", "day_night", "diel", "day")) {
            updateSelectInput(session, "playback_step_size", selected = "week")
          }
        }
      }, ignoreInit = TRUE)

      observeEvent(input$show_density_location_markers, {
        req(playback_active())
        if (!isTRUE(input$show_density_location_markers)) {
          current_step <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
          if (current_step %in% c("hour", "day_night", "diel", "day")) {
            updateSelectInput(session, "playback_step_size", selected = "week")
          }
        }
      }, ignoreInit = TRUE)

      observeEvent(input$reset_btn, {
        playback_skip_resume_id(playback_skip_resume_id() + 1L)
        playback_gap_notice(NULL)
        is_playing(FALSE)
        reset_playback_slider()
      })

      observeEvent(list(input$playback_step_size, input$playback_view_mode), {
        if (playback_active()) {
          playback_skip_resume_id(playback_skip_resume_id() + 1L)
          is_playing(FALSE)
          playback_gap_notice(NULL)
          reset_playback_slider()
        }
      }, ignoreInit = TRUE)

      observeEvent(list(session$rootScope()$input$nav, session$rootScope()$input$density_map_tabs), {
        if (identical(playback_mode, "none") &&
            (!identical(session$rootScope()$input$nav, "density_map") ||
             !identical(session$rootScope()$input$density_map_tabs, sub("^.*_", "", id)))) {
          playback_skip_resume_id(playback_skip_resume_id() + 1L)
          playback_gap_notice(NULL)
          is_playing(FALSE)
        }
      }, ignoreInit = TRUE)

      observeEvent(list(session$rootScope()$input$nav, input$density_playback_tabs), {
        if (identical(playback_mode, "always") &&
            (!identical(session$rootScope()$input$nav, "density_playback_map") ||
             !is.null(input$density_playback_tabs) && !identical(input$density_playback_tabs, "map"))) {
          playback_skip_resume_id(playback_skip_resume_id() + 1L)
          playback_gap_notice(NULL)
          is_playing(FALSE)
        }
      }, ignoreInit = TRUE)

      observeEvent(input$playback_tick, {
        req(is_playing(), playback_active(), input$time_slider)
        step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
        current_val <- playback_as_posix(input$time_slider)
        next_val <- if (step_size %in% c("day_night", "diel")) {
          next_weather_boundary(current_val, playback_period_end(), playback_weather_data(), step_size)
        } else if (identical(step_size, "season")) {
          playback_next_season_time(current_val, playback_period_end(), playback_period_groups())
        } else {
          current_val + playback_step_seconds()
        }

        skip_target <- if (skip_playback_gaps()) {
          playback_gap_skip_target(current_val, next_val, playback_period_end(), playback_period_groups())
        } else {
          NULL
        }

        if (!is.null(skip_target)) {
          is_playing(FALSE)
          playback_gap_notice(list(
            message = sprintf(
              "Skipping to %s",
              if (!is.null(skip_target$next_name) && !is.na(skip_target$next_name)) {
                skip_target$next_name
              } else {
                "next monitoring season"
              }
            )
          ))
          resume_id <- playback_skip_resume_id() + 1L
          playback_skip_resume_id(resume_id)
          shinyjs::delay(3000, {
            req(playback_active())
            req(identical(playback_skip_resume_id(), resume_id))
            updateSliderInput(session, "time_slider", value = skip_target$target)
            playback_gap_notice(NULL)
            is_playing(TRUE)
          })
          return()
        }

        if (next_val <= playback_period_end()) {
          playback_gap_notice(NULL)
          updateSliderInput(session, "time_slider", value = next_val)
        } else {
          playback_gap_notice(NULL)
          if (current_val < playback_period_end()) {
            updateSliderInput(session, "time_slider", value = playback_period_end())
          }
          is_playing(FALSE)
        }
      }, ignoreInit = TRUE)
      
      mapping_data_density <- reactive({
        req(obs(), deps(), current_selected_species(), current_selected_localities())
        species_dens <- tolower(unname(current_selected_species()))
        localities_dens <- current_selected_localities()
        if (playback_active()) {
          req(input$time_slider)
        }
        use_playback <- playback_active()
        
        logger::log_debug(sprintf(
          "mapping_module_server [density], %s mapping_data_density() for species: %s, localities: %s",
          id, paste(species_dens, collapse=", "), paste(localities_dens, collapse=", ")
        ))
        
        active_locations_dens <- deps() %>%
          dplyr::filter(locality %in% localities_dens) %>%
          dplyr::distinct(locationID, locality, .keep_all = TRUE)
        
        new_map_bounds <- calculate_bounds_from_locations(active_locations_dens, id, "density")
        new_bounds_key <- if (nrow(active_locations_dens) > 0) {
          paste(sort(unique(active_locations_dens$locationID)), collapse = "|")
        } else {
          ""
        }
        old_bounds <- current_bounds()
        old_bounds_key <- if (!is.null(old_bounds$key)) old_bounds$key else NULL
        if (!identical(new_bounds_key, old_bounds_key)) {
          if (!is.null(new_map_bounds)) {
            new_map_bounds$key <- new_bounds_key
          }
          current_bounds(new_map_bounds)
          needs_fit_bounds(TRUE)
        }
        
        obs_filtered_dens <- obs() %>%
          dplyr::filter(
            scientificName_lower %in% species_dens,
            locality %in% localities_dens
          )

        obs_for_scale_dens <- obs_filtered_dens

        start_time_dens <- NULL
        current_time_dens <- NULL
        if (use_playback) {
          step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
          current_time_dens <- playback_as_posix(input$time_slider)
          start_time_dens <- if (identical(input$playback_view_mode, "single")) {
            if (step_size %in% c("day_night", "diel")) {
              previous_weather_boundary(current_time_dens, playback_period_start(), playback_weather_data(), step_size)
            } else if (identical(step_size, "season")) {
              playback_current_season_start(current_time_dens, playback_period_groups(), playback_period_start())
            } else {
              current_time_dens - playback_step_seconds()
            }
          } else {
            playback_period_start()
          }

          obs_filtered_dens <- obs_filtered_dens %>%
            dplyr::filter(
              timestamp >= start_time_dens,
              timestamp <= current_time_dens
            )

          obs_for_scale_dens <- obs_for_scale_dens %>%
            dplyr::filter(
              timestamp >= start_time_dens,
              timestamp <= current_time_dens
            )
        }

        if (isTRUE(exclude_possible_duplicates_selected()) &&
            "possible_duplicate" %in% names(obs_filtered_dens)) {
          obs_filtered_dens <- obs_filtered_dens %>%
            dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
          obs_for_scale_dens <- obs_for_scale_dens %>%
            dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
        }
        
        active_location_effort_dens <- deps() %>%
          dplyr::filter(locality %in% localities_dens) %>%
          dplyr::group_by(locationID) %>%
          dplyr::summarise(
            locationName = dplyr::first(locationName),
            locality = dplyr::first(locality),
            line = dplyr::first(line),
            longitude = dplyr::first(longitude),
            latitude = dplyr::first(latitude),
            camera_hours = sum(camera_hours, na.rm = TRUE),
            .groups = "drop"
          )

        obs_location_counts_dens <- obs_filtered_dens %>%
          dplyr::group_by(locationID) %>%
          dplyr::summarise(
            count = sum(count, na.rm = TRUE),
            observation_ids = list(unique(observationID)),
            .groups = "drop"
          )

        location_summary_dens <- active_location_effort_dens %>%
          dplyr::left_join(obs_location_counts_dens, by = "locationID") %>%
          dplyr::mutate(
            count = dplyr::coalesce(.data$count, 0)
          )

        line_rai_summary_dens <- location_summary_dens %>%
          dplyr::group_by(locality, line) %>%
          dplyr::summarise(
            line_count = sum(count, na.rm = TRUE),
            line_camera_hours = sum(camera_hours, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            line_rai = dplyr::if_else(
              is.finite(.data$line_camera_hours) & .data$line_camera_hours > 0,
              (.data$line_count / .data$line_camera_hours) * config$globals$rai_norm_hours,
              NA_real_
            )
          )

        rai_location_dens <- location_summary_dens %>%
          dplyr::left_join(line_rai_summary_dens, by = c("locality", "line")) %>%
          dplyr::mutate(
            location_line_count_share = dplyr::if_else(
              is.finite(.data$line_count) & .data$line_count > 0,
              .data$count / .data$line_count,
              0
            ),
            weighted_line_rai = dplyr::if_else(
              is.finite(.data$line_rai),
              .data$line_rai * .data$location_line_count_share,
              NA_real_
            ),
            rai = .data$line_rai
          )

        obs_summary_location_dens <- rai_location_dens

        predicted_rai_surface <- NULL
        predicted_rai_surface_message <- NULL
        predicted_rai_surface_cache_key <- "surface-off"
        if (isTRUE(show_predicted_rai_surface_selected())) {
          surface_basis <- predicted_rai_surface_basis_selected()
          surface_basis_label <- if (identical(surface_basis, "weighted_line_rai")) {
            "location-weighted line RAI"
          } else {
            "line RAI"
          }
          surface_grid_n <- if (use_playback) 38 else 45
          surface_time_dens <- current_time_dens

          if (use_playback) {
            req(current_time_dens, playback_period_start(), playback_period_end())
            if (current_time_dens >= playback_period_end()) {
              surface_time_dens <- playback_period_end()
            } else {
              surface_time_dens <- lubridate::floor_date(current_time_dens, unit = "month")
              if (surface_time_dens < playback_period_start()) {
                surface_time_dens <- playback_period_start()
              }
              surface_time_dens <- as.POSIXct(surface_time_dens, tz = playback_actual_timezone())
            }
          }

          surface_time_key <- if (use_playback && !is.null(surface_time_dens)) {
            format(surface_time_dens, "%Y-%m-%d %H:%M:%S", tz = playback_actual_timezone())
          } else {
            "current-selection"
          }

          surface_cache_key <- paste(
            id,
            if (use_playback) "playback-monthly" else "static",
            paste(sort(species_dens), collapse = ","),
            paste(sort(localities_dens), collapse = ","),
            new_bounds_key,
            surface_basis,
            surface_time_key,
            exclude_possible_duplicates_selected(),
            sep = "|"
          )
          predicted_rai_surface_cache_key <- surface_cache_key

          surface_cache <- predicted_rai_surface_cache()
          if (!is.null(surface_cache) && identical(surface_cache$key, surface_cache_key)) {
            predicted_rai_surface <- surface_cache$surface
            predicted_rai_surface_message <- surface_cache$message
          } else {
            surface_rai_locations <- rai_location_dens
            if (use_playback) {
              surface_obs_filtered_dens <- obs() %>%
                dplyr::filter(
                  scientificName_lower %in% species_dens,
                  locality %in% localities_dens,
                  timestamp >= playback_period_start(),
                  timestamp <= surface_time_dens
                )

              if (isTRUE(exclude_possible_duplicates_selected()) &&
                  "possible_duplicate" %in% names(surface_obs_filtered_dens)) {
                surface_obs_filtered_dens <- surface_obs_filtered_dens %>%
                  dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
              }

              surface_obs_location_counts_dens <- surface_obs_filtered_dens %>%
                dplyr::group_by(locationID) %>%
                dplyr::summarise(
                  count = sum(count, na.rm = TRUE),
                  observation_ids = list(unique(observationID)),
                  .groups = "drop"
                )

              surface_location_summary_dens <- active_location_effort_dens %>%
                dplyr::left_join(surface_obs_location_counts_dens, by = "locationID") %>%
                dplyr::mutate(count = dplyr::coalesce(.data$count, 0))

              surface_line_rai_summary_dens <- surface_location_summary_dens %>%
                dplyr::group_by(locality, line) %>%
                dplyr::summarise(
                  line_count = sum(count, na.rm = TRUE),
                  line_camera_hours = sum(camera_hours, na.rm = TRUE),
                  .groups = "drop"
                ) %>%
                dplyr::mutate(
                  line_rai = dplyr::if_else(
                    is.finite(.data$line_camera_hours) & .data$line_camera_hours > 0,
                    (.data$line_count / .data$line_camera_hours) * config$globals$rai_norm_hours,
                    NA_real_
                  )
                )

              surface_rai_locations <- surface_location_summary_dens %>%
                dplyr::left_join(surface_line_rai_summary_dens, by = c("locality", "line")) %>%
                dplyr::mutate(
                  location_line_count_share = dplyr::if_else(
                    is.finite(.data$line_count) & .data$line_count > 0,
                    .data$count / .data$line_count,
                    0
                  ),
                  weighted_line_rai = dplyr::if_else(
                    is.finite(.data$line_rai),
                    .data$line_rai * .data$location_line_count_share,
                    NA_real_
                  ),
                  rai = .data$line_rai
                )
            }

            usable_rai_locations <- surface_rai_locations %>%
              dplyr::filter(
                is.finite(.data$longitude),
                is.finite(.data$latitude),
                is.finite(.data[[surface_basis]])
              )

            if (nrow(usable_rai_locations) < 3) {
              predicted_rai_surface_message <- sprintf(
                "Predicted RAI surface needs at least three active camera locations with %s data.",
                surface_basis_label
              )
            } else {
              predicted_rai_surface <- create_idw_prediction_surface(
                usable_rai_locations,
                value_col = surface_basis,
                group_col = "locality",
                grid_n = surface_grid_n
              )

              if (is.null(predicted_rai_surface) || nrow(predicted_rai_surface) == 0) {
                predicted_rai_surface_message <- "Predicted RAI surface is not available for the current camera layout."
              }
            }

            predicted_rai_surface_cache(
              list(
                key = surface_cache_key,
                surface = predicted_rai_surface,
                message = predicted_rai_surface_message
              )
            )
          }
        }
        
        obs_summary_locality_dens <- active_locations_dens %>%
          dplyr::select(locality) %>%
          dplyr::distinct() %>%
          dplyr::left_join(
            obs_filtered_dens %>%
              dplyr::group_by(locality) %>%
              dplyr::summarise(count = sum(count), .groups = "drop"),
            by = "locality"
          ) %>%
          dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
          dplyr::arrange(locality)
        
        grand_total_dens <- obs_summary_locality_dens %>%
          dplyr::summarise(count = sum(count)) %>%
          dplyr::mutate(locality = "Grand Total")
        
        max_location_count <- function(observations) {
          if (is.null(observations) || nrow(observations) == 0) {
            return(0)
          }

          counts <- observations %>%
            dplyr::group_by(locationID) %>%
            dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
            dplyr::pull(count)

          if (length(counts) == 0 || all(is.na(counts))) {
            return(0)
          }

          max(counts, na.rm = TRUE)
        }

        absolute_max <- max_location_count(obs_for_scale_dens)

        if (use_playback) {
          playback_absolute_max <- obs() %>%
            dplyr::filter(
              scientificName_lower %in% species_dens,
              locality %in% localities_dens,
              timestamp >= playback_period_start(),
              timestamp <= playback_period_end()
            )

          if (isTRUE(exclude_possible_duplicates_selected()) &&
              "possible_duplicate" %in% names(playback_absolute_max)) {
            playback_absolute_max <- playback_absolute_max %>%
              dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
          }

          playback_absolute_max <- max_location_count(playback_absolute_max)

          if (!is.na(playback_absolute_max) && !is.infinite(playback_absolute_max)) {
            absolute_max <- playback_absolute_max
          }
        }

        if (is.na(absolute_max) || is.infinite(absolute_max)) {
          absolute_max <- 0
        }

        weather_control <- if (use_playback) {
          step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
          view_mode_dens <- if (is.null(input$playback_view_mode)) "cumulative" else input$playback_view_mode
          weather_time_dens <- playback_window_reference_time(start_time_dens, current_time_dens, step_size, view_mode_dens)
          render_weather_map_control(
            weather_row_for_time(playback_weather_data(), weather_time_dens),
            time_info = playback_time_info_for_window(playback_weather_data(), start_time_dens, current_time_dens, step_size, view_mode_dens)
          )
        } else {
          NULL
        }

        period_control <- if (use_playback) {
          render_playback_period_control(playback_period_status(current_time_dens, playback_period_groups()))
        } else {
          NULL
        }

        list(
          active_locations = active_locations_dens,
          obs_summary_location = obs_summary_location_dens,
          obs_summary_locality_with_total = dplyr::bind_rows(obs_summary_locality_dens, grand_total_dens),
          start_time = start_time_dens,
          current_time = current_time_dens,
          absolute_max = absolute_max,
          obs_filtered = obs_filtered_dens,
          predicted_rai_surface = predicted_rai_surface,
          predicted_rai_surface_message = predicted_rai_surface_message,
          predicted_rai_surface_basis = predicted_rai_surface_basis_selected(),
          map_update_key = paste(
            id,
            if (use_playback) "playback" else "static",
            paste(sort(species_dens), collapse = ","),
            paste(sort(localities_dens), collapse = ","),
            new_bounds_key,
            if (is.null(start_time_dens)) "no-start" else format(start_time_dens, "%Y-%m-%d %H:%M:%S", tz = playback_actual_timezone()),
            if (is.null(current_time_dens)) "no-current" else format(current_time_dens, "%Y-%m-%d %H:%M:%S", tz = playback_actual_timezone()),
            exclude_possible_duplicates_selected(),
            show_density_location_markers_selected(),
            density_marker_metric_selected(),
            show_predicted_rai_surface_selected(),
            predicted_rai_surface_basis_selected(),
            predicted_rai_surface_cache_key,
            sep = "|"
          ),
          show_location_markers = show_density_location_markers_selected(),
          marker_metric = density_marker_metric_selected(),
          weather_control = weather_control,
          period_control = period_control,
          skip_notice = render_playback_skip_notice(playback_gap_notice())
        )
      })
      
      # Observer to update the content of the density map on the unified map display
      observe({
        req(mapping_data_density())
        logger::log_debug(sprintf("mapping_module_server [density], %s observer updating density map content on %s", id, MAP_ID))
        data_for_map <- mapping_data_density()

        if (!is.null(data_for_map$map_update_key) &&
            identical(last_density_map_update_key(), data_for_map$map_update_key)) {
          if (isTRUE(needs_fit_bounds())) {
            apply_map_fit_bounds()
            needs_fit_bounds(FALSE)
          }
          return()
        }
        last_density_map_update_key(data_for_map$map_update_key)
        
        # update_density_map is a function specific to rendering density data.
        # It now targets the unified MAP_ID.
        update_density_map(
          map_id = MAP_ID, 
          active_locations = data_for_map$active_locations,
          obs_summary_location = data_for_map$obs_summary_location,
          show_zero = TRUE,
          absolute_max = data_for_map$absolute_max,
          predicted_rai_surface = data_for_map$predicted_rai_surface,
          predicted_rai_surface_message = data_for_map$predicted_rai_surface_message,
          show_location_markers = data_for_map$show_location_markers,
          marker_metric = data_for_map$marker_metric,
          weather_control_html = data_for_map$weather_control,
          period_control_html = data_for_map$period_control,
          skip_notice_html = data_for_map$skip_notice
        )
        if (isTRUE(needs_fit_bounds())) {
          apply_map_fit_bounds()
          needs_fit_bounds(FALSE)
        }
      })
      
      if (!playback_active()) {
        output$obs_summary <- renderUI({ # Density-specific summary table
          data_for_summary <- req(mapping_data_density())
          data_summary <- data_for_summary$obs_summary_locality_with_total
          total_row_index <- nrow(data_summary)
          kable_table <- knitr::kable(data_summary, format = "html", escape = FALSE, col.names = c("Locality", "Count")) %>%
            kableExtra::kable_styling(
              bootstrap_options = c("hover", "condensed", "bordered"),
              full_width = FALSE, position = "center", font_size = 12
            ) %>%
            kableExtra::row_spec(total_row_index, bold = TRUE)
          HTML(kable_table)
        })
        outputOptions(output, "obs_summary", suspendWhenHidden = FALSE)
      }

      output$playback_window_ui <- renderUI({
        req(playback_active(), input$time_slider)

        current_time <- playback_as_posix(input$time_slider)
        step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
        view_mode <- if (is.null(input$playback_view_mode)) "cumulative" else input$playback_view_mode
        start_time <- if (identical(view_mode, "single")) {
          if (step_size %in% c("day_night", "diel")) {
            previous_weather_boundary(current_time, playback_period_start(), playback_weather_data(), step_size)
          } else if (identical(step_size, "season")) {
            playback_current_season_start(current_time, playback_period_groups(), playback_period_start())
          } else {
            current_time - playback_step_seconds()
          }
        } else {
          playback_period_start()
        }

        HTML(playback_window_readout(
          start_time,
          current_time,
          step_size,
          playback_weather_data(),
          view_mode
        ))
      })

      output$playback_data_table <- DT::renderDataTable({
        req(playback_active(), mapping_data_density())
        table_data <- mapping_data_density()$obs_filtered

        table_data <- prepare_spec_table_data(
          table_data,
          table_id = "observationmap_observations_browse",
          column_help = FALSE
        )$table_data

        DT::datatable(
          table_data,
          escape = FALSE,
          options = list(pageLength = 10, searching = TRUE, lengthChange = TRUE, order = list(list(3, 'asc'))),
          class = 'display',
          rownames = FALSE
        )
      })

      root_session <- session$rootScope()
      if (is.null(root_session$userData$pdf_export_density_map_renderers)) {
        root_session$userData$pdf_export_density_map_renderers <- list()
      }

      root_session$userData$pdf_export_density_map_renderers[[MAP_ID]] <- function(width = NULL, height = NULL, export_dir = NULL) {
        data_for_map <- isolate(mapping_data_density())
        export_map <- create_pdf_export_density_map(
          active_locations = data_for_map$active_locations,
          obs_summary_location = data_for_map$obs_summary_location,
          show_zero = TRUE,
          predicted_rai_surface = data_for_map$predicted_rai_surface,
          show_location_markers = data_for_map$show_location_markers,
          marker_metric = data_for_map$marker_metric,
          width = width,
          height = height
        )

        render_pdf_export_leaflet_png(
          map = export_map,
          map_id = MAP_ID,
          export_dir = export_dir,
          width = width,
          height = height
        )
      }
      
      # Observer for auto-recentering density map on tab switch
      # This assumes 'density_map_tabs' is an input in the parent UI controlling visibility
      sub_tab_id <- sub("^.*_", "", id) # e.g., "primary" or "comparative"
      observe({
        main_nav <- session$rootScope()$input$nav
        sub_tab  <- session$rootScope()$input$density_map_tabs # Input from parent UI
        
        req(identical(playback_mode, "none"), main_nav == "density_map", sub_tab == sub_tab_id)
        
        shinyjs::runjs(sprintf( # GA event
          "gtag('event','tab_switch',{
             'event_category':'sub_tab_navigation',
             'event_label': %s,
             'value': %s
           });",
          jsonlite::toJSON(paste0("main_menu_", main_nav, "_tab_switch"), auto_unbox = TRUE),
          jsonlite::toJSON(sub_tab, auto_unbox = TRUE)
        ))
        
        logger::log_debug(sprintf(
          "mapping_module_server [density], %s auto-recenter due to tab switch to '%s' tab",
          id, sub_tab
        ))
        recenter_map_generic() # Use the generic recenter function
      })

      observe({
        main_nav <- session$rootScope()$input$nav
        playback_tab <- input$density_playback_tabs

        req(identical(playback_mode, "always"), main_nav == "density_playback_map")
        req(is.null(playback_tab) || identical(playback_tab, "map"))

        logger::log_debug(sprintf(
          "mapping_module_server [density playback], %s auto-recenter due to navigation/tab switch",
          id
        ))
        recenter_map_generic()
      })
      
      return(list(
        selected_species = current_selected_species,
        selected_localities = current_selected_localities,
        show_predicted_rai_surface = show_predicted_rai_surface_selected,
        predicted_rai_surface_basis = predicted_rai_surface_basis_selected,
        show_density_location_markers = show_density_location_markers_selected,
        density_marker_metric = density_marker_metric_selected,
        recenter_map = recenter_map_generic # Return the generic recenter function
      ))
      
      
      # --- Observation Map Specific Logic ---
    } else if (type == "observation") {
      if (!isTRUE(enable_map_outputs)) {
        return(list(
          selected_species = current_selected_species,
          selected_localities = current_selected_localities,
          recenter_map = recenter_map_generic
        ))
      }

      on.exit({ # Specific to observation type, as in original
        print(paste0("DEBUG: The fully namespaced ID for observation_map_tabs is: '", ns("observation_map_tabs"), "'"))
      })
      
      observation_map_warning_content <- reactiveVal(NULL) # Specific to observation map
      playback_gap_notice_obs <- reactiveVal(NULL)

      current_trap_data <- reactive({
        if (is.null(trap_data)) {
          return(NULL)
        }

        trap_data_value <- if (is.function(trap_data)) trap_data() else trap_data
        if (is.null(trap_data_value) ||
            is.null(trap_data_value$obs) ||
            is.null(trap_data_value$deps)) {
          return(NULL)
        }

        trap_data_value
      })

      include_trap_data_selected <- reactive({
        isTRUE(input$include_trap_data) && !is.null(current_trap_data())
      })

      floor_posix_hour_obs <- function(value) {
        value <- as.POSIXct(value, tz = playback_actual_timezone())
        as.POSIXct(
          floor(as.numeric(value) / 3600) * 3600,
          origin = "1970-01-01",
          tz = playback_actual_timezone()
        )
      }

      ceiling_posix_hour_obs <- function(value) {
        value <- as.POSIXct(value, tz = playback_actual_timezone())
        as.POSIXct(
          ceiling(as.numeric(value) / 3600) * 3600,
          origin = "1970-01-01",
          tz = playback_actual_timezone()
        )
      }

      playback_bounds_obs <- function(start, end) {
        start <- floor_posix_hour_obs(start)
        end <- ceiling_posix_hour_obs(end)

        if (is.na(start) || is.na(end)) {
          return(NULL)
        }

        if (end <= start) {
          end <- start + 3600
        }

        list(start = start, end = end)
      }

      playback_active_obs <- reactive({
        identical(playback_mode, "always")
      })

      skip_playback_gaps_obs <- reactive({
        isTRUE(input$skip_playback_gaps)
      })

      playback_period_groups_obs <- reactive({
        if (exists("core_data", inherits = TRUE) && !is.null(core_data$period_groups)) {
          return(core_data$period_groups)
        }

        NULL
      })

      playback_observation_bounds_obs <- reactive({
        obs_data <- obs()

        fallback_start <- if (is.function(period_start_date)) {
          as.POSIXct(period_start_date(), tz = playback_actual_timezone())
        } else {
          suppressWarnings(as.POSIXct(min(obs_data$timestamp, na.rm = TRUE), tz = playback_actual_timezone()))
        }

        fallback_end <- if (is.function(period_end_date)) {
          as.POSIXct(as.Date(period_end_date()) + 1, tz = playback_actual_timezone()) - 1
        } else {
          suppressWarnings(as.POSIXct(max(obs_data$timestamp, na.rm = TRUE), tz = playback_actual_timezone()))
        }

        req(fallback_start, fallback_end)

        period_obs <- obs_data %>% dplyr::filter(!is.na(timestamp))
        if (isTRUE(include_trap_data_selected())) {
          return(playback_bounds_obs(fallback_start, fallback_end))
        }

        if (nrow(period_obs) > 0) {
          return(playback_bounds_obs(
            min(period_obs$timestamp, na.rm = TRUE),
            max(period_obs$timestamp, na.rm = TRUE)
          ))
        }

        playback_bounds_obs(fallback_start, fallback_end)
      })

      playback_period_start_obs <- reactive({
        req(playback_observation_bounds_obs())
        playback_observation_bounds_obs()$start
      })

      playback_period_end_obs <- reactive({
        req(playback_observation_bounds_obs())
        playback_observation_bounds_obs()$end
      })

      playback_weather_data_obs <- reactive({
        req(playback_period_start_obs(), playback_period_end_obs(), current_selected_localities(), deps())
        active_locations <- deps() %>%
          dplyr::filter(locality %in% current_selected_localities()) %>%
          dplyr::distinct(locationID, .keep_all = TRUE)

        playback_weather_for_deployments(
          active_locations,
          as.Date(playback_period_start_obs()),
          as.Date(playback_period_end_obs())
        )
      })

      playback_step_seconds_obs <- reactive({
        step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
        switch(step_size,
          "hour" = 3600,
          "day_night" = 3600,
          "diel" = 3600,
          "day" = 86400,
          "week" = 604800,
          "month" = 2592000,
          "season" = 604800,
          86400
        )
      })

      playback_initial_value_obs <- reactive({
        req(playback_period_start_obs(), playback_period_end_obs())
        step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
        if (identical(input$playback_view_mode, "single")) {
          if (step_size %in% c("day_night", "diel")) {
            return(min(
              next_weather_boundary(playback_period_start_obs(), playback_period_end_obs(), playback_weather_data_obs(), step_size),
              playback_period_end_obs()
            ))
          }
          if (identical(step_size, "season")) {
            return(playback_next_season_time(
              playback_period_start_obs(),
              playback_period_end_obs(),
              playback_period_groups_obs()
            ))
          }
          min(playback_period_start_obs() + playback_step_seconds_obs(), playback_period_end_obs())
        } else {
          playback_period_start_obs()
        }
      })

      reset_playback_slider_obs <- function() {
        req(playback_period_start_obs(), playback_period_end_obs())
        updateSliderInput(session, "time_slider", value = playback_initial_value_obs())
      }

      output$playback_slider_ui <- renderUI({
        req(playback_active_obs(), playback_period_start_obs(), playback_period_end_obs())
        playback_timeline_ui(
          ns = ns,
          min = playback_period_start_obs(),
          max = playback_period_end_obs(),
          value = playback_initial_value_obs(),
          step = playback_step_seconds_obs()
        )
      })

      is_playing_obs <- reactiveVal(FALSE)
      playback_skip_resume_id_obs <- reactiveVal(0L)

      set_playback_button_state_obs <- function(playing) {
        if (!playback_active_obs()) {
          return()
        }

        if (isTRUE(playing)) {
          shinyjs::disable(selector = paste0("#", ns("play_btn")))
          shinyjs::enable(selector = paste0("#", ns("pause_btn")))
        } else {
          shinyjs::enable(selector = paste0("#", ns("play_btn")))
          shinyjs::disable(selector = paste0("#", ns("pause_btn")))
        }
      }

      send_playback_timer_obs <- function() {
        if (!playback_active_obs()) {
          return()
        }

        playback_speed <- if (is.null(input$playback_speed)) 1 else input$playback_speed
        interval_ms <- max(50, as.numeric(playback_speed) * 1000)
        session$sendCustomMessage(
          "densityPlaybackTimer",
          list(
            id = ns("playback_tick"),
            enabled = isTRUE(is_playing_obs() && playback_active_obs()),
            interval = interval_ms
          )
        )
      }

      observe({
        set_playback_button_state_obs(is_playing_obs())
        send_playback_timer_obs()
      })

      observeEvent(input$play_btn, {
        req(playback_active_obs())
        is_playing_obs(TRUE)
      })

      observeEvent(input$pause_btn, {
        playback_skip_resume_id_obs(playback_skip_resume_id_obs() + 1L)
        playback_gap_notice_obs(NULL)
        is_playing_obs(FALSE)
      })

      observeEvent(input$reset_btn, {
        playback_skip_resume_id_obs(playback_skip_resume_id_obs() + 1L)
        playback_gap_notice_obs(NULL)
        is_playing_obs(FALSE)
        reset_playback_slider_obs()
      })

      observeEvent(list(input$playback_step_size, input$playback_view_mode), {
        if (playback_active_obs()) {
          playback_skip_resume_id_obs(playback_skip_resume_id_obs() + 1L)
          is_playing_obs(FALSE)
          playback_gap_notice_obs(NULL)
          reset_playback_slider_obs()
        }
      }, ignoreInit = TRUE)

      observeEvent(list(session$rootScope()$input$nav, input$observation_map_tabs), {
        if (!identical(session$rootScope()$input$nav, "observation_map") ||
            !is.null(input$observation_map_tabs) && !identical(input$observation_map_tabs, ns("map_tab"))) {
          playback_skip_resume_id_obs(playback_skip_resume_id_obs() + 1L)
          playback_gap_notice_obs(NULL)
          is_playing_obs(FALSE)
        }
      }, ignoreInit = TRUE)

      observe({
        if (identical(input$playback_view_mode, "single")) {
          shiny::showTab(
            inputId = "observation_map_tabs",
            target = ns("cumulative_data_tab"),
            session = session
          )
        } else {
          if (identical(input$observation_map_tabs, ns("cumulative_data_tab"))) {
            updateTabsetPanel(
              session,
              "observation_map_tabs",
              selected = ns("data_tab")
            )
          }
          shiny::hideTab(
            inputId = "observation_map_tabs",
            target = ns("cumulative_data_tab"),
            session = session
          )
        }
      })

      observeEvent(input$playback_tick, {
        req(is_playing_obs(), playback_active_obs(), input$time_slider)
        step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
        current_val <- playback_as_posix(input$time_slider)
        next_val <- if (step_size %in% c("day_night", "diel")) {
          next_weather_boundary(current_val, playback_period_end_obs(), playback_weather_data_obs(), step_size)
        } else if (identical(step_size, "season")) {
          playback_next_season_time(current_val, playback_period_end_obs(), playback_period_groups_obs())
        } else {
          current_val + playback_step_seconds_obs()
        }

        skip_target <- if (skip_playback_gaps_obs()) {
          playback_gap_skip_target(current_val, next_val, playback_period_end_obs(), playback_period_groups_obs())
        } else {
          NULL
        }

        if (!is.null(skip_target)) {
          is_playing_obs(FALSE)
          playback_gap_notice_obs(list(
            message = sprintf(
              "Skipping to %s",
              if (!is.null(skip_target$next_name) && !is.na(skip_target$next_name)) {
                skip_target$next_name
              } else {
                "next monitoring season"
              }
            )
          ))
          resume_id <- playback_skip_resume_id_obs() + 1L
          playback_skip_resume_id_obs(resume_id)
          shinyjs::delay(3000, {
            req(playback_active_obs())
            req(identical(playback_skip_resume_id_obs(), resume_id))
            updateSliderInput(session, "time_slider", value = skip_target$target)
            playback_gap_notice_obs(NULL)
            is_playing_obs(TRUE)
          })
          return()
        }

        if (next_val <= playback_period_end_obs()) {
          playback_gap_notice_obs(NULL)
          updateSliderInput(session, "time_slider", value = next_val)
        } else {
          playback_gap_notice_obs(NULL)
          is_playing_obs(FALSE)
        }
      }, ignoreInit = TRUE)
      
      observation_map_processed_data <- reactive({
        req(obs(), deps(), current_selected_species(), current_selected_localities())
        if (playback_active_obs()) {
          req(input$time_slider)
        }

        start_date <- if (is.function(period_start_date)) {
          period_start_date()
        } else {
          suppressWarnings(min(as.Date(obs()$timestamp), na.rm = TRUE))
        }

        end_date <- if (is.function(period_end_date)) {
          period_end_date()
        } else {
          suppressWarnings(max(as.Date(obs()$timestamp), na.rm = TRUE))
        }

        req(start_date, end_date)
        
        species_to_map <- tolower(unname(current_selected_species()))
        localities_to_map <- current_selected_localities()
        trap_obs_filtered <- if (isTRUE(include_trap_data_selected())) {
          prepare_trap_observations_for_map(
            current_trap_data(),
            start_date,
            end_date,
            species_to_map
          )
        } else {
          dplyr::tibble()
        }
        
        logger::log_debug(sprintf(
          "mapping_module_server [observation] ID: %s - observation_map_processed_data: Processing for species: [%s], localities: [%s]",
          id, paste(species_to_map, collapse=", "), paste(localities_to_map, collapse=", ")
        ))
        
        # Note: distinct criteria for observation map active locations
        active_locations_obsmap <- deps() %>%
          dplyr::filter(locality %in% localities_to_map) %>%
          dplyr::distinct(locationID, .keep_all = TRUE) 

        bounds_locations_obsmap <- active_locations_obsmap
        if (nrow(trap_obs_filtered) > 0) {
          bounds_locations_obsmap <- dplyr::bind_rows(
            bounds_locations_obsmap %>%
              dplyr::select(locationID, locationName, latitude, longitude),
            trap_obs_filtered %>%
              dplyr::select(locationID, locationName, latitude, longitude) %>%
              dplyr::distinct(locationID, .keep_all = TRUE)
          )
        }
        
        new_map_bounds <- calculate_bounds_from_locations(bounds_locations_obsmap, id, "observation")
        new_bounds_key <- if (nrow(bounds_locations_obsmap) > 0) {
          paste(sort(unique(bounds_locations_obsmap$locationID)), collapse = "|")
        } else {
          ""
        }
        old_bounds <- current_bounds()
        old_bounds_key <- if (!is.null(old_bounds$key)) old_bounds$key else NULL
        if (!identical(new_bounds_key, old_bounds_key)) {
          if (!is.null(new_map_bounds)) {
            new_map_bounds$key <- new_bounds_key
          }
          current_bounds(new_map_bounds)
          needs_fit_bounds(TRUE)
        }
        
        obs_filtered_obsmap <- obs() %>%
          dplyr::filter(tolower(scientificName) %in% species_to_map,
                        locality %in% localities_to_map)

        if (isTRUE(exclude_possible_duplicates_selected()) &&
            "possible_duplicate" %in% names(obs_filtered_obsmap)) {
          obs_filtered_obsmap <- obs_filtered_obsmap %>%
            dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
        }

        trap_obs_filtered <- align_trap_observation_types(trap_obs_filtered, obs_filtered_obsmap)
        obs_cumulative_table <- obs_filtered_obsmap
        trap_cumulative_table <- trap_obs_filtered

        start_time_obsmap <- NULL
        current_time_obsmap <- NULL
        if (playback_active_obs()) {
          step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
          current_time_obsmap <- playback_as_posix(input$time_slider)
          start_time_obsmap <- if (identical(input$playback_view_mode, "single")) {
            if (step_size %in% c("day_night", "diel")) {
              previous_weather_boundary(current_time_obsmap, playback_period_start_obs(), playback_weather_data_obs(), step_size)
            } else if (identical(step_size, "season")) {
              playback_current_season_start(current_time_obsmap, playback_period_groups_obs(), playback_period_start_obs())
            } else {
              current_time_obsmap - playback_step_seconds_obs()
            }
          } else {
            playback_period_start_obs()
          }

          obs_cumulative_table <- obs_cumulative_table %>%
            dplyr::filter(
              timestamp >= playback_period_start_obs(),
              timestamp <= current_time_obsmap
            )

          if (nrow(trap_cumulative_table) > 0) {
            trap_cumulative_table <- trap_cumulative_table %>%
              dplyr::filter(
                display_start_time <= current_time_obsmap,
                display_end_time >= playback_period_start_obs()
              )
          }

          obs_filtered_obsmap <- obs_filtered_obsmap %>%
            dplyr::filter(
              timestamp >= start_time_obsmap,
              timestamp <= current_time_obsmap
            )

          if (nrow(trap_obs_filtered) > 0) {
            # Trap kills are date-interval observations: the animal may have been
            # killed any time after the prior check and up to the check date. For
            # now we display them on every playback window that overlaps that
            # uncertainty interval. This is deliberately isolated here so future
            # alternatives, such as midpoint display or a config option, can be
            # swapped in without changing the conversion or table code.
            trap_obs_filtered <- trap_obs_filtered %>%
              dplyr::filter(
                display_start_time <= current_time_obsmap,
                display_end_time >= start_time_obsmap
              )
          }
        }
        
        no_obs_locations_obsmap <- active_locations_obsmap %>%
          dplyr::filter(!locationName %in% obs_filtered_obsmap$locationName)
        
        marker_prep_input <- list(
          observations = obs_filtered_obsmap,
          no_obs_deployments = no_obs_locations_obsmap
        )
        
        # create_map_markers is assumed to be an external helper function
        prepared_markers_list <- create_map_markers(marker_prep_input) 

        if (nrow(trap_obs_filtered) > 0) {
          prepared_markers_list$markers[["trap_kills"]] <- list(
            species = "trap kills",
            markers = create_trap_map_markers(trap_obs_filtered),
            warning = NULL
          )
        }
        
        if (!is.null(prepared_markers_list$warnings) && length(prepared_markers_list$warnings) > 0) {
          concatenated_warnings <- paste("Warning: ", prepared_markers_list$warnings, collapse = "<br>")
          observation_map_warning_content(concatenated_warnings)
        } else {
          observation_map_warning_content(NULL)
        }
        
        # config$globals$species_name_type is assumed to be defined elsewhere
        observations_for_table <- dplyr::bind_rows(obs_filtered_obsmap, trap_obs_filtered)
        cumulative_observations_for_table <- dplyr::bind_rows(obs_cumulative_table, trap_cumulative_table)

        obs_summary_for_sidebar <- observations_for_table %>%
          dplyr::group_by(!!sym(config$globals$species_name_type), species_class, species_rank) %>%
          dplyr::summarise(Count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
          dplyr::rename(Species = !!sym(config$globals$species_name_type)) %>%
          dplyr::arrange(species_rank) %>%
          dplyr::select(Species, Count)
        
        weather_control <- if (playback_active_obs()) {
          step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
          view_mode_obsmap <- if (is.null(input$playback_view_mode)) "cumulative" else input$playback_view_mode
          weather_time_obsmap <- playback_window_reference_time(start_time_obsmap, current_time_obsmap, step_size, view_mode_obsmap)
          render_weather_map_control(
            weather_row_for_time(playback_weather_data_obs(), weather_time_obsmap),
            time_info = playback_time_info_for_window(playback_weather_data_obs(), start_time_obsmap, current_time_obsmap, step_size, view_mode_obsmap)
          )
        } else {
          NULL
        }

        period_control <- if (playback_active_obs()) {
          render_playback_period_control(playback_period_status(current_time_obsmap, playback_period_groups_obs()))
        } else {
          NULL
        }

        list(
          observations_for_table = observations_for_table,
          cumulative_observations_for_table = cumulative_observations_for_table,
          obs_summary_for_sidebar = obs_summary_for_sidebar,
          prepared_markers = prepared_markers_list,
          active_locations = active_locations_obsmap,
          no_obs_locations_count = nrow(no_obs_locations_obsmap),
          trap_observations_count = nrow(trap_obs_filtered),
          trap_legend = NULL,
          start_time = start_time_obsmap,
          current_time = current_time_obsmap,
          weather_control = weather_control,
          period_control = period_control,
          skip_notice = render_playback_skip_notice(playback_gap_notice_obs())
        )
      })

      playback_window_times_obs <- reactive({
        if (!playback_active_obs()) {
          return(NULL)
        }

        req(input$time_slider)
        step_size <- if (is.null(input$playback_step_size)) "day" else input$playback_step_size
        current_time_obsmap <- playback_as_posix(input$time_slider)
        start_time_obsmap <- if (identical(input$playback_view_mode, "single")) {
          if (step_size %in% c("day_night", "diel")) {
            previous_weather_boundary(
              current_time_obsmap,
              playback_period_start_obs(),
              playback_weather_data_obs(),
              step_size
            )
          } else if (identical(step_size, "season")) {
            playback_current_season_start(current_time_obsmap, playback_period_groups_obs(), playback_period_start_obs())
          } else {
            current_time_obsmap - playback_step_seconds_obs()
          }
        } else {
          playback_period_start_obs()
        }

        list(
          start_time = start_time_obsmap,
          current_time = current_time_obsmap,
          step_size = step_size
        )
      })
      
      # Reactive to check if the observation map's "map_tab" is active.
      # This assumes the observation map section in the UI might have its own sub-tabs
      # (e.g., one for map, one for data table), controlled by input$observation_map_tabs.
      is_observation_map_with_map_tab_active <- reactive({
        main_nav <- session$rootScope()$input$nav
        req(main_nav == "observation_map") # Ensure we are on the correct main page
        
        module_tab_value <- input$observation_map_tabs # Input from UI, specific to observation map section
        expected_tab_value <- ns("map_tab") # Assumes a tab named "map_tab" within the observation module's UI
        
        if (is.null(module_tab_value)) {
          logger::log_debug(sprintf("is_observation_map_with_map_tab_active (ID: %s): main_nav is 'observation_map', input$observation_map_tabs is NULL. Assuming default value '%s' is active.", id, expected_tab_value))
          return(TRUE) # Defaulting to TRUE if input is NULL, assuming map tab is default
        } else {
          logger::log_debug(sprintf("is_observation_map_with_map_tab_active (ID: %s): main_nav is 'observation_map', input$observation_map_tabs is '%s'. Checking if it's '%s'.", id, module_tab_value, expected_tab_value))
          return(module_tab_value == expected_tab_value)
        }
      })
      
      # Observer to update the content of the observation map on the unified map display
      observe({
        # logger::log_error(sprintf("VERY_BASIC_OBSERVER_ENTRY (ID: %s): This observer has been entered.", id)) # Original log
        
        if (!is_observation_map_with_map_tab_active()) {
          logger::log_debug(sprintf("OBSERVER_STATE (ID: %s): Not on observation_map page OR map_tab is not active. Skipping map update.", id))
          return()
        }
        logger::log_info(sprintf("OBSERVER_STATE (ID: %s): On observation_map page AND map_tab is active. Proceeding to check data.", id))
        
        processed_data_val <- req(observation_map_processed_data())
        req(current_selected_localities()) # As in original
        
        logger::log_info(sprintf("MAP_UPDATE_ACTION (ID: %s): Conditions met (page, tab, data). Updating map content on %s.", id, MAP_ID))
        
        all_markers_data <- processed_data_val$prepared_markers
        active_locs <- processed_data_val$active_locations
        
        # update_map is a function specific to rendering observation data.
        # It now targets the unified MAP_ID.
        update_map(
          all_markers_data,
          MAP_ID,
          active_locs,
          weather_control_html = processed_data_val$weather_control,
          period_control_html = processed_data_val$period_control,
          skip_notice_html = processed_data_val$skip_notice,
          trap_legend_html = processed_data_val$trap_legend
        ) 
        
        if (isTRUE(needs_fit_bounds())) {
          recenter_map_generic()
          needs_fit_bounds(FALSE)
        }
        
        if (!is.null(input$enhance_map_details) && input$enhance_map_details) {
          # update_map_area is specific to observation map type
          update_map_area(MAP_ID, active_locs) 
        }
      })
      
      # Observer for tab switches within the observation map's own UI (if applicable)
      observeEvent(input$observation_map_tabs, {
        logger::log_warn(sprintf(
          "TAB_SWITCH_EVENT (ID: %s): User switched tabs within the module. New active tab input: '%s'. Expected map tab ID: '%s'",
          id,
          if (is.null(input$observation_map_tabs)) "NULL" else input$observation_map_tabs,
          ns("map_tab")
        ))
        
        if (is_observation_map_with_map_tab_active()) { # If switched TO the map_tab
          logger::log_info(sprintf(
            "TAB_SWITCH_EVENT (ID: %s): Switched TO the 'map_tab'. Calling recenter_map_generic().",
            id
          ))
          recenter_map_generic() # Use the generic recenter function
        } else {
          logger::log_debug(sprintf(
            "TAB_SWITCH_EVENT (ID: %s): Switched to a tab other than 'map_tab', or main page context is no longer 'observation_map'. No map-specific action by this event observer.",
            id
          ))
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
      
      # --- Observation-specific outputs (UI elements other than the map) ---
      prepare_observation_map_table <- function(observation_rows) {
        table_data <- prepare_spec_table_data(
          observation_rows,
          table_id = "observationmap_observations_browse",
          column_help = FALSE
        )$table_data

        source_rows <- if ("observation_source" %in% names(observation_rows)) {
          !is.na(observation_rows$observation_source) &
            observation_rows$observation_source == "trapping"
        } else {
          rep(FALSE, nrow(observation_rows))
        }
        timestamp_column <- intersect(c("Timestamp", "timestamp"), names(table_data))
        if (length(timestamp_column) > 0 && any(source_rows, na.rm = TRUE)) {
          timestamp_column <- timestamp_column[[1]]
          table_data[[timestamp_column]][source_rows] <- format(
            as.Date(observation_rows$timestamp[source_rows]),
            "%Y-%m-%d"
          )
        }

        table_data
      }

      output$observation_data_table <- DT::renderDataTable({
        processed_data <- req(observation_map_processed_data())
        table_data <- prepare_observation_map_table(processed_data$observations_for_table)

        DT::datatable( table_data, escape = FALSE,
                       options = list( pageLength = 10, searching = TRUE, lengthChange = TRUE, order = list(list(3, 'asc')) ),
                       class = 'display', rownames = FALSE
        )
      })

      output$observation_cumulative_data_table <- DT::renderDataTable({
        processed_data <- req(observation_map_processed_data())
        table_data <- prepare_observation_map_table(processed_data$cumulative_observations_for_table)

        DT::datatable( table_data, escape = FALSE,
                       options = list( pageLength = 10, searching = TRUE, lengthChange = TRUE, order = list(list(3, 'asc')) ),
                       class = 'display', rownames = FALSE
        )
      })
      
      output$observation_map_sidebar_summary_content <- renderUI({
        req(observation_map_processed_data())
        data_summary <- observation_map_processed_data()
        title <- "<strong>OBSERVATIONS SUMMARY</strong>"
        start_d <- if (is.function(period_start_date)) {
          as.Date(period_start_date())
        } else {
          suppressWarnings(min(as.Date(obs()$timestamp), na.rm = TRUE))
        }
        end_d <- if (is.function(period_end_date)) {
          as.Date(period_end_date())
        } else {
          suppressWarnings(max(as.Date(obs()$timestamp), na.rm = TRUE))
        }
        req(start_d, end_d)
        formatted_start_date <- paste0("<strong>", format(start_d, "%d %b %Y (%a)"), "</strong>")
        formatted_end_date <- paste0("<strong>", format(end_d, "%d %b %Y (%a)"), "</strong>")
        date_table_df <- data.frame(Label = c("Start:", "End:"), Date = c(formatted_start_date, formatted_end_date))
        header <- knitr::kable(date_table_df, format = "html", col.names = NULL, escape = FALSE) %>%
          kableExtra::kable_styling(bootstrap_options = c("condensed"), full_width = FALSE, font_size=11)
        
        
        kable_summary_table <- knitr::kable(data_summary$obs_summary_for_sidebar, format = "html", col.names = c("Species", "Count")) %>%
          kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("hover", "bordered", "responsive", "condensed"), font_size = 12)
        trap_footer <- if (isTRUE(include_trap_data_selected())) {
          paste0("<br><small>Trap kills shown: <strong>", data_summary$trap_observations_count, "</strong></small>")
        } else {
          ""
        }
        footer <- paste0(
          "<div><small>Locations with no selected species: <strong>",
          data_summary$no_obs_locations_count,
          "</strong></small>",
          trap_footer,
          "</div>"
        )
        HTML(paste0(title, header, kable_summary_table, footer))
      })

      output$playback_window_ui <- renderUI({
        if (!playback_active_obs()) {
          return(NULL)
        }

        window_times <- req(playback_window_times_obs())
        HTML(playback_window_readout(
          window_times$start_time,
          window_times$current_time,
          window_times$step_size,
          playback_weather_data_obs(),
          if (is.null(input$playback_view_mode)) "cumulative" else input$playback_view_mode
        ))
      })
      outputOptions(output, "playback_window_ui", suspendWhenHidden = FALSE)
      
      output$observation_map_textoverlay_warning <- renderUI({
        if (!is.null(observation_map_warning_content())) {
          div(class = "map-overlay-warning", HTML(observation_map_warning_content()))
        }
      })
      
      return(list(
        selected_species = current_selected_species,
        selected_localities = current_selected_localities,
        recenter_map = recenter_map_generic # Return the generic recenter function
      ))
    }
  })
}


update_density_map <- function(map_id = NULL, 
                               active_locations = NULL, 
                               obs_summary_location = NULL, 
                               show_zero = TRUE,
                               absolute_max = NULL,
                               predicted_rai_surface = NULL,
                               predicted_rai_surface_message = NULL,
                               show_location_markers = TRUE,
                               marker_metric = "count",
                               weather_control_html = NULL,
                               period_control_html = NULL,
                               skip_notice_html = NULL) {
  #browser()
  max_scale <- 1
  radius_range <- c(10, 50)
  marker_metric <- if (!is.null(marker_metric) && marker_metric %in% c("count", "rai")) marker_metric else "count"
  marker_value_label <- if (identical(marker_metric, "rai")) "Line RAI" else "Number of individuals"

  # Clear the map
  proxy <- leafletProxy(map_id) %>%
    clearMarkers() %>%
    clearShapes() %>%
    clearControls()

  if (!is.null(weather_control_html)) {
    proxy <- proxy %>% addControl(
      html = weather_control_html,
      position = "topright",
      className = "map-weather-control"
    )
  }

  if (!is.null(period_control_html)) {
    proxy <- proxy %>% addControl(
      html = period_control_html,
      position = "topright",
      className = "map-season-control"
    )
  }

  if (!is.null(skip_notice_html)) {
    proxy <- proxy %>% addControl(
      html = skip_notice_html,
      position = "topleft",
      className = "map-playback-skip-control"
    )
  }
  
  # Check if obs_summary_location has any rows
  if (nrow(obs_summary_location) > 0) {
    max_count <- max(obs_summary_location$count, na.rm = TRUE)
    if (!is.null(absolute_max) && absolute_max > 0) {
      max_count <- max(max_count, absolute_max)
    }
  } else {
    logger::log_debug(sprintf("mapping_module_server, %s update_density_map() has no results available", map_id))
    max_count <- if (!is.null(absolute_max)) absolute_max else NA
    
    # Show "No results!" message 
    if (!show_zero || nrow(active_locations) == 0) {
      logger::log_debug(sprintf("mapping_module_server, %s update_density_map() creating popup saying no results", map_id))
      proxy %>% addControl(
        html = "<strong>No markers to show! Check your selection filter criteria.</strong>", 
        position = "topright", 
        className = "no-results-message"
      )
      return()
    }
  }
  
  if (show_zero) {
    zero_icon <- get_species_icon(species = "none")
    
    zero_entries <- active_locations %>%
      anti_join(obs_summary_location, by = "locationID") %>%
      mutate(count = 0)
    
    obs_summary_location <- bind_rows(obs_summary_location, zero_entries)
  }
  
  if (identical(marker_metric, "rai") && "line_rai" %in% names(obs_summary_location)) {
    obs_summary_location$marker_value <- ifelse(is.finite(obs_summary_location$line_rai), obs_summary_location$line_rai, 0)
  } else {
    obs_summary_location$marker_value <- ifelse(is.finite(obs_summary_location$count), obs_summary_location$count, 0)
  }

  max_marker_value <- if (nrow(obs_summary_location) > 0) {
    max(obs_summary_location$marker_value, na.rm = TRUE)
  } else {
    NA_real_
  }
  if (identical(marker_metric, "count") && !is.null(absolute_max) && absolute_max > 0) {
    max_marker_value <- max(max_marker_value, absolute_max)
  }

  can_scale_marker_radius <- !is.na(max_marker_value) && max_marker_value > 0
  obs_summary_location <- obs_summary_location %>%
    mutate(radius = ifelse(marker_value > 0 & can_scale_marker_radius,
                           scales::rescale(marker_value, to = radius_range, from = c(0, max_marker_value)),
                           radius_range[1]))

  pal_domain <- if (!is.na(max_marker_value) && max_marker_value > 0) c(0, max_marker_value) else obs_summary_location$marker_value
  pal <- colorNumeric(palette = "inferno", domain = pal_domain)

  if (!is.null(predicted_rai_surface) && nrow(predicted_rai_surface) > 0) {
    surface_max <- max(predicted_rai_surface$predicted_rai, na.rm = TRUE)
    surface_domain <- if (is.finite(surface_max) && surface_max > 0) {
      c(0, surface_max)
    } else {
      c(0, 1)
    }
    surface_pal <- colorNumeric(
      palette = "YlOrRd",
      domain = surface_domain
    )

    proxy %>%
      addPolygons(
        data = predicted_rai_surface,
        fillColor = ~surface_pal(predicted_rai),
        fillOpacity = 0.42,
        stroke = FALSE,
        smoothFactor = 0,
        label = ~sprintf("%s predicted RAI: %0.2f", locality, predicted_rai),
        group = "Predicted RAI surface"
      ) %>%
      addLegend(
        "bottomleft",
        pal = surface_pal,
        values = predicted_rai_surface$predicted_rai,
        title = "Predicted RAI",
        labFormat = labelFormat(),
        opacity = 0.8
      )
  } else if (!is.null(predicted_rai_surface_message) && nzchar(predicted_rai_surface_message)) {
    proxy %>%
      addControl(
        html = sprintf("<strong>Predicted RAI surface unavailable</strong><br><small>%s</small>", predicted_rai_surface_message),
        position = "topleft",
        className = "map-prediction-message-control"
      )
  }

  format_marker_value <- function(value, digits = 1) {
    if (is.null(value) || length(value) == 0 || !is.finite(value)) {
      return("N/A")
    }

    format(round(as.numeric(value), digits), big.mark = ",", nsmall = digits, trim = TRUE)
  }

  rai_norm_hours <- if (exists("config", inherits = TRUE) &&
                        !is.null(config$globals$rai_norm_hours)) {
    config$globals$rai_norm_hours
  } else {
    NA_real_
  }

  marker_popup_html <- function(marker_data, include_review_link = TRUE) {
    marker_field <- function(field, i, default = NA) {
      if (!field %in% names(marker_data)) {
        return(default)
      }

      marker_data[[field]][[i]]
    }

    vapply(seq_len(nrow(marker_data)), function(i) {
      observation_ids <- if ("observation_ids" %in% names(marker_data) &&
                            is.list(marker_data$observation_ids)) {
        unlist(marker_data$observation_ids[[i]], use.names = FALSE)
      } else {
        character(0)
      }
      observation_ids <- as.character(observation_ids)
      observation_ids <- observation_ids[!is.na(observation_ids) & nzchar(observation_ids)]

      payload <- list(
        location_name = marker_field("locationName", i, ""),
        locality = marker_field("locality", i, ""),
        observation_ids = observation_ids
      )

      safe_marker_text <- function(value) {
        if (is.null(value) || length(value) == 0 || is.na(value)) {
          return("")
        }

        htmltools::htmlEscape(as.character(value))
      }

      line_value <- marker_field("line", i, "")
      line_text <- if (!is.na(line_value) && nzchar(as.character(line_value))) {
        paste0("Line ", safe_marker_text(line_value))
      } else {
        ""
      }
      location_text <- safe_marker_text(marker_field("locationName", i, ""))
      locality_text <- safe_marker_text(marker_field("locality", i, ""))
      locality_line_text <- paste(c(locality_text, line_text)[nzchar(c(locality_text, line_text))], collapse = ", ")

      review_link <- ""
      if (isTRUE(include_review_link) && length(observation_ids) > 0) {
        payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
        encoded_payload <- utils::URLencode(payload_json, reserved = TRUE)
        onclick_js <- sprintf(
          "Shiny.setInputValue('density_map_review_sequences_click', JSON.parse(decodeURIComponent('%s')), {priority: 'event'}); return false;",
          encoded_payload
        )
        review_link <- sprintf("<br><a href='#' onclick=\"%s\" title='Review Sequences'>Review Sequences</a>", onclick_js)
      }

      paste0(
        "<strong>Location: ", location_text, "</strong>",
        if (nzchar(locality_line_text)) paste0("<br>", locality_line_text) else "",
        "<br><br><strong>Observations:</strong>",
        "<br>Location: ", format_marker_value(marker_field("count", i), digits = 0),
        " (effort: ", format_marker_value(marker_field("camera_hours", i), digits = 1), " hours)",
        "<br>Line: ", format_marker_value(marker_field("line_count", i), digits = 0),
        " (effort: ", format_marker_value(marker_field("line_camera_hours", i), digits = 1), " hours)",
        "<br>Location share of line count: ", format_marker_value(marker_field("location_line_count_share", i) * 100, digits = 1), "%",
        "<br><br><strong>RAI:</strong>",
        "<br>Line RAI: ", format_marker_value(marker_field("line_rai", i), digits = 2),
        " (per ", format_marker_value(rai_norm_hours, digits = 0), " hours)",
        "<br>Weighted Location-Line RAI: ", format_marker_value(marker_field("weighted_line_rai", i), digits = 2),
        review_link
      )
    }, character(1))
  }

  if (isTRUE(show_location_markers)) {
    obs_summary_location$popup_content <- marker_popup_html(obs_summary_location, include_review_link = TRUE)
    obs_summary_location$label_content <- marker_popup_html(obs_summary_location, include_review_link = FALSE)

    circle_locations <- if (identical(marker_metric, "rai")) {
      obs_summary_location %>% filter(is.finite(marker_value))
    } else {
      obs_summary_location %>% filter(count > 0)
    }

    if (nrow(circle_locations) > 0) {
      proxy %>%
        addCircleMarkers(
          data = circle_locations,
          lng = ~longitude, lat = ~latitude,
          radius = ~radius * max_scale,
          fillColor = ~pal(marker_value),
          fillOpacity = 0.8,
          stroke = FALSE,
          popup = ~popup_content,
          label = lapply(circle_locations$label_content, htmltools::HTML),
          labelOptions = labelOptions(direction = "auto", opacity = 0.95)
        )
    }

    if (show_zero && identical(marker_metric, "count")) {
      zero_locations <- obs_summary_location %>% filter(count == 0)
      if (nrow(zero_locations) > 0) {
        proxy %>%
          addMarkers(
            data = zero_locations,
            lng = ~longitude, lat = ~latitude,
            icon = zero_icon,
            popup = ~popup_content,
            label = lapply(zero_locations$label_content, htmltools::HTML),
            labelOptions = labelOptions(direction = "auto", opacity = 0.95)
          )
      }
    }

    if (!is.na(max_marker_value) && max_marker_value > 0) {
      proxy %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = obs_summary_location$marker_value,
          title = marker_value_label,
          labFormat = labelFormat(),
          opacity = 1
        )
    }
  }
}




# interactive_map_functions.R
# Contains the functions that are ONLY used by the interactive maps
# Other functions not contained here are still required as well

# Function to create markers for given data, including the markers for other and no observations 
# deployments. The data  comes from reactive function relating to the map
# 
create_map_markers <- function(data) {
  #  browser()
  max_markers <- config$globals$max_markers_per_species_per_location
  max_markers_threshold <- config$globals$max_markers_apply_for_total_counts_over
  
  all_warnings <- list()
  
  logger::log_debug("create_map_markers() checking and creating markers for all species")
  
  # Group data by scientificName_lower and locationName
  grouped_data <- data$observations %>%
    group_by(scientificName_lower, locationName) %>%
    arrange(scientificName_lower, locationName)
  
  all_markers_data <- list()
  
  # Process each species group
  for(species in unique(grouped_data$scientificName_lower)) {
    species_data <- grouped_data %>%
      dplyr::filter(scientificName_lower == species)
    
    warning_message <- NULL
    
    if (nrow(species_data) > max_markers_threshold) {
      warning_message <- sprintf("Markers for %s truncated. Total observations %s, showing max %s per location. 
                              Check 'Data' view for full %s results.", species, nrow(species_data), max_markers, species)
      all_warnings[[species]] <- warning_message
      
      # Limit the number of markers per location
      markers <- species_data %>%
        group_by(locationName) %>%
        slice_head(n = max_markers) %>%
        ungroup() %>%  # Ensure we return to a flat structure
        rowwise() %>%  # Ensure row-wise operations
        mutate(marker = list(create_marker_from_record(as.list(pick(everything())), species))) %>%  # Create marker for each row
        pull(marker)  # Extract the list of markers directly without flattening
      
    } else {
      # If total count does not exceed the threshold
      markers <- lapply(1:nrow(species_data), function(i) {
        obs_record <- species_data[i, ]
        create_marker_from_record(obs_record, species)
      })
    }
    
    
    all_markers_data[[species]] <- list(species = species, markers = markers, warning = warning_message)
  }
  
  logger::log_debug("create_map_markers() created markers for %s species", 
                    length(all_markers_data))
  
  if (nrow(data$no_obs_deployments) > 0) {
    markers <- lapply(1:nrow(data$no_obs_deployments), function(i) {
      obs_record <- data$no_obs_deployments[i, ]
      create_marker_from_record(obs_record, "no_obs_deployments")
    })
    
    all_markers_data[["no_obs_deployments"]] <- list(species = "no_obs_deployments", markers = markers)
  }
  
  logger::log_debug("create_map_markers() created no observation markers for %s unique Locations", 
                    nrow(data$no_obs_deployments))
  
  # Extract warnings from each species' data
  all_warnings <- lapply(all_markers_data, function(x) x$warning)
  all_warnings <- Filter(Negate(is.null), all_warnings)  # Remove NULL entries
  
  # Combine all markers and warnings data
  all_markers_data_with_warnings <- list(markers = all_markers_data, warnings = all_warnings)
  
  
  return(all_markers_data_with_warnings)
}



create_marker_from_record <- function(obs_record, scientific_name_lower) {
  #  browser()
  # Determines how far from the locationName lat/lng a species marker will be
  offset_value <- config$globals$marker_offset_value
  
  if (scientific_name_lower == "no_obs_deployments") {
    # Popup content based on the deployment details not observation details
    popup_content <- sprintf("Location: %s (%s)<br>No relevant observations at this location for the selected daterange.",
                             obs_record$locationName, obs_record$locality)
  } else {
    # Create random offsets for latitude and longitude to avoid marker overlap
    lat_offset <- runif(1, -offset_value, offset_value)
    long_offset <- runif(1, -offset_value, offset_value)
    
    # Apply the offsets to the coordinates
    obs_record$longitude <- obs_record$longitude + long_offset
    obs_record$latitude <- obs_record$latitude + lat_offset
    
    confidence_text <- get_confidence_text(obs_record$classificationConfidence)
    
    classified_by_text <- if (obs_record$classificationMethod == "machine") {
      "machine"
    } else {
      obs_record$classifiedBy
    }
    
    popup_content <- sprintf(
      "
        <div>
          Sighting of %s (%s) <strong>%s</strong> at %s (%s) on %s. Classified by %s, with %s confidence.
          <a href='javascript:void(0);' class='observation-link' data-observationid='%s' data-action-type='view_sequence|modal'>View Images</a>
        </div>
      ",
      int_to_text(obs_record$count),
      obs_record$count,
      str_to_title(obs_record[[config$globals$species_name_type]]),
      obs_record$locationName,
      obs_record$locality,
      obs_record$timestamp,
      classified_by_text,
      confidence_text,
      obs_record$observationID
    )
  }
  
  # Determine the icon to use based on the scientific name
  species_icon <- get_species_icon(scientific_name_lower)
  
  # Determine the zIndexOffset based on the scientific name
  zIndexOffset <- dplyr::coalesce(get_icon_importance(scientific_name_lower), 100)
  
  # Return a list containing all marker properties
  return(list(
    lat = obs_record$latitude, 
    lng = obs_record$longitude, 
    icon = species_icon, 
    popup_content = popup_content, 
    zIndexOffset = zIndexOffset
  ))
}


extract_trap_tag_value <- function(tags, key) {
  prefix <- paste0(key, ":")

  vapply(tags, function(tag_value) {
    if (is.na(tag_value) || !nzchar(tag_value)) {
      return(NA_character_)
    }

    parts <- strsplit(as.character(tag_value), "\\s*\\|\\s*")[[1]]
    match <- parts[startsWith(parts, prefix)]
    if (length(match) == 0) {
      return(NA_character_)
    }

    trimws(sub(prefix, "", match[[1]], fixed = TRUE))
  }, character(1), USE.NAMES = FALSE)
}

trap_marker_group_one <- function(description) {
  description <- tolower(ifelse(is.na(description), "", description))

  if (grepl("stoat", description)) return("stoat")
  if (grepl("weasel", description)) return("weasel")
  if (grepl("ferret", description)) return("ferret")
  if (grepl("rat", description)) return("rat")
  if (grepl("mouse", description)) return("mouse")
  if (grepl("cat", description)) return("cat")
  if (grepl("rabbit", description)) return("rabbit")
  if (grepl("hedgehog", description)) return("hedgehog")
  if (grepl("possum", description)) return("possum")
  if (grepl("weka", description)) return("weka")
  if (grepl("bird", description)) return("bird")
  if (grepl("mustelid", description)) return("mustelid")

  "other"
}

trap_marker_group <- function(description) {
  vapply(description, trap_marker_group_one, character(1), USE.NAMES = FALSE)
}

trap_marker_label <- function(description) {
  labels <- c(
    stoat = "Stoat",
    weasel = "Weasel",
    ferret = "Ferret",
    rat = "Rat",
    mouse = "Mouse",
    cat = "Cat",
    rabbit = "Rabbit",
    hedgehog = "Hedgehog",
    possum = "Possum",
    weka = "Weka",
    bird = "Bird",
    mustelid = "Mustelid",
    other = "Other"
  )

  group <- trap_marker_group(description)
  unname(labels[group])
}

trap_marker_color <- function(description) {
  colors <- c(
    stoat = "darkred",
    weasel = "red",
    ferret = "orange",
    rat = "black",
    mouse = "gray",
    cat = "purple",
    rabbit = "green",
    hedgehog = "cadetblue",
    possum = "darkblue",
    weka = "darkgreen",
    bird = "blue",
    mustelid = "lightred",
    other = "darkpurple"
  )

  group <- trap_marker_group(description)
  unname(colors[group])
}

get_trap_kill_icon <- function(description) {
  makeIcon(
    iconUrl = "www/icons/map_icons/trap-kill.svg",
    iconWidth = 30,
    iconHeight = 30,
    iconAnchorX = 15,
    iconAnchorY = 15
  )
}

prepare_trap_observations_for_map <- function(trap_data_value,
                                              start_date,
                                              end_date,
                                              selected_species) {
  if (is.null(trap_data_value) ||
      is.null(trap_data_value$obs) ||
      is.null(trap_data_value$deps) ||
      nrow(trap_data_value$obs) == 0) {
    return(dplyr::tibble())
  }

  trap_obs <- trap_data_value$obs
  trap_deps <- trap_data_value$deps

  if (!all(c("deploymentID", "latitude", "longitude", "locationName") %in% names(trap_deps))) {
    return(dplyr::tibble())
  }

  spp_classes <- lapply(config$globals$spp_classes, function(x) tolower(unlist(x)))
  if (!"prior_check_date" %in% names(trap_obs)) {
    trap_obs$prior_check_date <- as.Date(trap_obs$eventStart)
  }

  trap_obs <- trap_obs %>%
    dplyr::mutate(
      timestamp = as.POSIXct(as.Date(eventStart), tz = playback_actual_timezone()),
      check_date = as.Date(eventStart),
      prior_check_date = as.Date(prior_check_date),
      display_start_time = as.POSIXct(prior_check_date, tz = playback_actual_timezone()),
      display_end_time = as.POSIXct(check_date + 1, tz = playback_actual_timezone()) - 1,
      observation_source = "trapping",
      outcome = extract_trap_tag_value(observationTags, "outcome"),
      outcome_id = extract_trap_tag_value(observationTags, "outcome_id"),
      scientificName_lower = tolower(scientificName),
      `vernacularNames.eng` = trap_marker_label(outcome),
      species_class = determine_species_class(scientificName_lower, spp_classes),
      species_rank = create_species_rank(scientificName_lower, spp_classes),
      possible_duplicate = FALSE,
      period = NA_character_,
      day_night_class = NA_character_,
      diel_class = NA_character_,
      classificationConfidence = NA_real_,
      trap_kill_type = outcome
    ) %>%
    dplyr::filter(
      observationType == "animal",
      !is.na(scientificName_lower),
      scientificName_lower %in% selected_species,
      prior_check_date <= as.Date(end_date),
      check_date >= as.Date(start_date)
    ) %>%
    dplyr::left_join(
      trap_deps %>%
        dplyr::transmute(
          deploymentID,
          locationID,
          locationName = trimws(locationName),
          latitude,
          longitude,
          trap_line = deploymentGroups,
          line = suppressWarnings(as.integer(gsub("[^0-9]+", "", deploymentGroups))),
          locality = "Trap network"
        ),
      by = "deploymentID"
    )

  trap_obs
}

align_trap_observation_types <- function(trap_observations, observation_template) {
  if (is.null(trap_observations) || nrow(trap_observations) == 0 ||
      is.null(observation_template) || nrow(observation_template) == 0) {
    return(trap_observations)
  }

  common_columns <- intersect(names(trap_observations), names(observation_template))
  for (column_name in common_columns) {
    template_column <- observation_template[[column_name]]

    if (inherits(template_column, "POSIXt")) {
      trap_observations[[column_name]] <- as.POSIXct(
        trap_observations[[column_name]],
        tz = playback_actual_timezone()
      )
    } else if (inherits(template_column, "Date")) {
      trap_observations[[column_name]] <- as.Date(trap_observations[[column_name]])
    } else if (is.integer(template_column)) {
      trap_observations[[column_name]] <- suppressWarnings(as.integer(trap_observations[[column_name]]))
    } else if (is.numeric(template_column)) {
      trap_observations[[column_name]] <- suppressWarnings(as.numeric(trap_observations[[column_name]]))
    } else if (is.character(template_column)) {
      trap_observations[[column_name]] <- as.character(trap_observations[[column_name]])
    } else if (is.logical(template_column)) {
      trap_observations[[column_name]] <- as.logical(trap_observations[[column_name]])
    } else if (is.factor(template_column)) {
      trap_observations[[column_name]] <- factor(
        trap_observations[[column_name]],
        levels = levels(template_column)
      )
    }
  }

  trap_observations
}

create_trap_marker_from_record <- function(trap_record) {
  offset_value <- config$globals$marker_offset_value
  lat_offset <- runif(1, -offset_value, offset_value)
  long_offset <- runif(1, -offset_value, offset_value)

  trap_record$longitude <- trap_record$longitude + long_offset
  trap_record$latitude <- trap_record$latitude + lat_offset

  popup_content <- sprintf(
    paste0(
      "<div>",
      "Trap kill: <strong>%s</strong><br>",
      "Trap: %s<br>",
      "Line: %s<br>",
      "Kill window: %s to %s (%s days)<br>",
      "Observation ID: <a href='javascript:void(0);' class='trap-observation-link' data-observationid='%s'>%s</a>",
      "</div>"
    ),
    trap_record$trap_kill_type,
    trap_record$locationName,
    trap_record$trap_line,
    format(as.Date(trap_record$prior_check_date), "%Y-%m-%d"),
    format(as.Date(trap_record$timestamp), "%Y-%m-%d"),
    trap_record$check_interval,
    trap_record$observationID,
    trap_record$observationID
  )

  list(
    lat = trap_record$latitude,
    lng = trap_record$longitude,
    icon = get_trap_kill_icon(trap_record$trap_kill_type),
    popup_content = popup_content,
    zIndexOffset = 1200
  )
}

create_trap_map_markers <- function(trap_observations) {
  if (is.null(trap_observations) || nrow(trap_observations) == 0) {
    return(list())
  }

  lapply(seq_len(nrow(trap_observations)), function(i) {
    create_trap_marker_from_record(trap_observations[i, ])
  })
}

render_trap_marker_legend <- function(trap_observations) {
  if (is.null(trap_observations) || nrow(trap_observations) == 0) {
    return(NULL)
  }

  outcomes <- trap_observations %>%
    dplyr::distinct(trap_kill_type) %>%
    dplyr::arrange(trap_kill_type) %>%
    dplyr::pull(trap_kill_type)

  rows <- vapply(outcomes, function(outcome) {
    sprintf(
      "<div class='trap-marker-legend-row'><span class='trap-marker-legend-swatch trap-marker-legend-swatch-%s'></span>%s</div>",
      trap_marker_group(outcome),
      htmltools::htmlEscape(outcome)
    )
  }, character(1), USE.NAMES = FALSE)

  paste0(
    "<div class='trap-marker-legend'><strong>Trap kills</strong>",
    paste(rows, collapse = ""),
    "</div>"
  )
}



get_confidence_text <- function(confidence) {
  if (is.na(confidence)) return("full")
  if (confidence == 1) return("full (validated)")
  if (confidence > 0.8) return("high")
  return("uncertain")
}



update_map <- function(all_marker_data_with_warnings,
                       map_id,
                       active_locations,
                       weather_control_html = NULL,
                       period_control_html = NULL,
                       skip_notice_html = NULL,
                       trap_legend_html = NULL) {
  
  proxy <- leafletProxy(map_id) %>% 
    clearMarkers() %>% 
    clearShapes() %>% # Also clear shapes if update_map_area adds them
    clearControls()

  if (!is.null(weather_control_html)) {
    proxy <- proxy %>% addControl(
      html = weather_control_html,
      position = "topright",
      className = "map-weather-control"
    )
  }

  if (!is.null(period_control_html)) {
    proxy <- proxy %>% addControl(
      html = period_control_html,
      position = "topright",
      className = "map-season-control"
    )
  }

  if (!is.null(skip_notice_html)) {
    proxy <- proxy %>% addControl(
      html = skip_notice_html,
      position = "topleft",
      className = "map-playback-skip-control"
    )
  }

  if (!is.null(trap_legend_html)) {
    proxy <- proxy %>% addControl(
      html = trap_legend_html,
      position = "bottomleft",
      className = "trap-marker-legend-control"
    )
  }
  
  all_marker_data <- all_marker_data_with_warnings$markers
  
  # Iterate over all species
  if (length(all_marker_data) > 0) {
    for (species_data_key in names(all_marker_data)) { # Iterate by keys/names
      species_data <- all_marker_data[[species_data_key]]
      species <- species_data$species 
      markers <- species_data$markers  
      
      if (!is.null(markers) && length(markers) > 0) {
        logger::log_debug(sprintf("update_map() for map_id: %s - Adding %d markers for species: %s", 
                                  map_id, length(markers), species))
        for (marker_data in markers) {
          proxy %>% addMarkers(
            lng = marker_data$lng,
            lat = marker_data$lat,
            icon = marker_data$icon,
            popup = marker_data$popup_content,
            options = markerOptions(zIndexOffset = marker_data$zIndexOffset)
          )
        }
      } else {
        logger::log_debug(sprintf("update_map() for map_id: %s - No markers to add for species: %s", map_id, species))
      }
    }
  } else {
    logger::log_debug(sprintf("update_map() for map_id: %s - No species data in all_marker_data.", map_id))
  }
  
  # Return the summary of markers added (from your original server.R logic, adapted)
  output_summary <- list()
  if (length(all_marker_data) > 0) {
    for (species_data_key in names(all_marker_data)) {
      species_data <- all_marker_data[[species_data_key]]
      if (!is.null(species_data$markers) && species_data$species != "no_obs_deployments") {
        output_summary[[species_data$species]] <- length(species_data$markers)
      }
    }
  }
  return(output_summary)
}

update_map_area <- function(map_id, deployments) {
  # Area calculation
  #browser()
  area_data <- calculate_area_by_locality(deployments)
  #  area_data_df <- st_drop_geometry(area_data) %>%
  #    mutate(area_km2 = round(area_km2, 2))  # Round area_km2 for display
  
  visible_localities <- unique(deployments$locality)
  filtered_area_data <- area_data[area_data$locality %in% visible_localities, ]
  # Extract attributes from the sf object to a regular dataframe for popup content
  
  
  # Define the color palette
  #  locality_colors <- colorFactor(palette = c("red", "blue", "green"), 
  #                                 domain = unique(area_data$locality))
  
  leafletProxy(map_id) %>%
    #  addMarkers(data = deployments, ~longitude, ~latitude, popup = ~locationName) %>%
    addPolygons(
      data = filtered_area_data, 
      fillColor = "red",
      fillOpacity = 0.1, 
      color = "white", 
      weight = 1, 
      smoothFactor = 0.5,
      popup = ~paste(locality, ": ", round(area_ha, 1), " ha")
    )
  
}



# Used forzIndexOffset which determines which icons are on top of other icons on a leaflet
get_icon_importance <- function(species) {
  
  importance_level <- list(
    "apteryx mantelli" = 1000,   # Highest priority
    "mustela erminea" = 900,
    "mustela nivalis" = 890,
    "prosthemadera novaeseelandiae" = 810,
    "felis catus" = 800,
    "erinaceus europaeus" = 800,
    "sus scrofa" = 800,
    "rattus" = 700,
    
    "gallirallus australis" = 300
  )
  
  if (species %in% tolower(names(importance_level))) {
    return(importance_level[[tolower(species)]])
  } else {
    return(NULL)
  }
}

# Function to get the icon for a given species
get_species_icon <- function(species) {
  
  # Assuming makeIcon is available in your environment
  # Define a named list of icons
  species_icons <- list(
    "apteryx mantelli" = makeIcon(
      iconUrl = "www/icons/map_icons/kiwi.png", # https://www.cleanpng.com/
      iconWidth = 49,
      iconHeight = 29
    ),
    "mustela erminea" = makeIcon(
      iconUrl = "www/icons/map_icons/stoat.png",
      iconWidth = 60,
      iconHeight = 23
    ),
    "mustela nivalis" = makeIcon(
      iconUrl = "www/icons/map_icons/weasel.png",
      iconWidth = 50,
      iconHeight = 36
    ),
    "felis catus" = makeIcon(
      iconUrl = "www/icons/map_icons/cat.png",
      iconWidth = 37,
      iconHeight = 28
    ),
    "Rattus" = makeIcon(
      iconUrl = "www/icons/map_icons/rat.png",
      iconWidth = 30,
      iconHeight = 26
    ),
    
    "Gallirallus australis" = makeIcon(
      iconUrl = "www/icons/map_icons/weka.png",
      iconWidth = 40,
      iconHeight = 35
    ),
    "Prosthemadera novaeseelandiae" = makeIcon(
      iconUrl = "www/icons/map_icons/tui.png",
      iconWidth = 30,
      iconHeight = 27
    ),
    "Erinaceus europaeus" = makeIcon(
      iconUrl = "www/icons/map_icons/hedgehog.png",
      iconWidth = 30, 
      iconHeight = 22   
    ),
    "Sus scrofa" = makeIcon(
      iconUrl = "www/icons/map_icons/wildboar.png",
      iconWidth = 40,
      iconHeight = 28
    ),
    
    "no_obs_deployments" = makeIcon(
      iconUrl = "www/icons/map_icons/cross.png",
      iconWidth = 10, 
      iconHeight = 10
    ),
    
    "none" = makeIcon(
      iconUrl = "www/icons/map_icons/cross.png",
      iconWidth = 10, 
      iconHeight = 10
    )
  )
  
  "default_icon" <- makeIcon(
    iconUrl = "www/icons/map_icons/other1.png",
    iconWidth = 10,
    iconHeight = 10
  )
  # https://icons8.com/icon/set/popular/ios-glyphs
  
  species_icons <- setNames(species_icons, tolower(names(species_icons)))
  
  if (tolower(species) %in% names(species_icons)) {
    return(species_icons[[tolower(species)]])
  } else {
    # message(sprintf("No icon found in list for %s, using default icon", species))
    return(default_icon)
  }
}
