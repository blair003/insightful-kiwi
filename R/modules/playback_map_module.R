playback_map_module_ui <- function(id, view = "map", species_choices = NULL, species_selected = NULL, locality_choices = NULL, locality_selected = NULL, map_height = config$globals$leaflet_height) {
  ns <- NS(id)

  if (view == "sidebar") {
    return(
      tagList(
        selectizeInput(
          inputId = ns("selected_species"),
          label = tagList(icon("paw"), "Species selection:"),
          choices = species_choices,
          selected = species_selected,
          multiple = TRUE,
          options = list(placeholder = "Select species...", closeAfterSelect = TRUE)
        ),
        selectInput(
          inputId = ns("selected_localities"),
          label = tagList(icon("location-dot"), "Locality selection:"),
          choices = locality_choices,
          selected = locality_selected,
          multiple = TRUE,
          selectize = TRUE
        ),
        hr(),
        selectInput(
          inputId = ns("step_size"),
          label = "Playback Step Size:",
          choices = c("Hourly" = "hour", "Daily" = "day", "Weekly" = "week", "Monthly" = "month"),
          selected = "day"
        ),
        radioButtons(
          inputId = ns("view_mode"),
          label = "View Mode:",
          choices = c("Single Period" = "single", "Cumulative" = "cumulative"),
          selected = "cumulative"
        ),
        checkboxInput(
          inputId = ns("limit_season"),
          label = "Limit to starting season",
          value = TRUE
        ),
        sliderInput(
          inputId = ns("playback_speed"),
          label = "Playback speed",
          min = 0.5, max = 5, value = 1.5, step = 0.5
        ),
        div(style = "display: flex; justify-content: space-between; font-size: 0.8em; color: #888; margin-top: -15px; margin-bottom: 15px;",
          tags$span("Faster"), tags$span("Slower")
        ),
        div(
          class = "d-grid gap-2",
          actionButton(ns("play_btn"), "Play", icon = icon("play"), class = "btn-success"),
          actionButton(ns("pause_btn"), "Pause", icon = icon("pause")),
          actionButton(ns("reset_btn"), "Reset", icon = icon("rotate-left"), class = "btn-danger")
        ),
        hr(),
        uiOutput(ns("playback_summary"))
      )
    )
  } else if (view == "map") {
    return(
      tagList(
        div(style = "margin-bottom: 20px;", uiOutput(ns("slider_ui"))),
        navset_tab(
          id = ns("playback_map_tabs"),
          nav_panel(
            "Map",
            leafletOutput(ns("map_display"), height = map_height)
          ),
          nav_panel(
            "Data",
            h3("Observations Data in Current Slice"),
            p("This table shows the raw observations for the time slice currently shown on the map."),
            DT::dataTableOutput(ns("playback_data_table"))
          )
        )
      )
    )
  }
}

playback_map_module_server <- function(id, core_data, playback_period) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    MAP_ID <- "map_display"
    current_bounds <- reactiveVal(NULL)

    current_overall_end <- reactive({
      req(playback_period$end_date())
      if (isTRUE(input$limit_season)) {
          as.POSIXct(playback_period$end_date())
      } else {
          as.POSIXct(max(core_data$obs$timestamp, na.rm = TRUE))
      }
    })

    # Render Slider based on step size
    output$slider_ui <- renderUI({
      req(playback_period$start_date(), current_overall_end())
      start_date <- as.POSIXct(playback_period$start_date())
      overall_end <- current_overall_end()

      step_val <- switch(input$step_size,
        "hour" = 3600,       # 1 hour in seconds
        "day" = 86400,       # 1 day in seconds
        "week" = 604800,     # 1 week in seconds
        "month" = 2592000    # 30 days in seconds (approx)
      )

      # Default to a small window ahead if in single mode, else just start
      init_value <- if (!is.null(input$view_mode) && input$view_mode == "single") {
        min(start_date + step_val, overall_end)
      } else {
        start_date
      }

      sliderInput(
        inputId = ns("time_slider"),
        label = "Time Progression",
        min = start_date,
        max = overall_end,
        value = init_value,
        step = step_val,
        width = "100%",
        timezone = config$globals$actual_timezone
      )
    })

    is_playing <- reactiveVal(FALSE)

    observeEvent(input$play_btn, {
      is_playing(TRUE)
    })

    observeEvent(input$pause_btn, {
      is_playing(FALSE)
    })

    observeEvent(input$reset_btn, {
      is_playing(FALSE)
      req(playback_period$start_date(), current_overall_end())

      start_date <- as.POSIXct(playback_period$start_date())
      step_val <- switch(input$step_size, hour = 3600,
          day = 86400, week = 604800, month = 2592000)

      overall_end <- current_overall_end()
      init_value <- if (!is.null(input$view_mode) && input$view_mode == "single") {
          min(start_date + step_val, overall_end)
      } else {
          start_date
      }

      updateSliderInput(session, "time_slider", value = init_value)
    })

    observe({
      if (is_playing()) {
        req(input$playback_speed)
        invalidateLater(input$playback_speed * 1000, session)

        step_val <- switch(input$step_size,
          "hour" = 3600,
          "day" = 86400,
          "week" = 604800,
          "month" = 2592000
        )

        current_val <- as.POSIXct(input$time_slider)
        next_val <- current_val + step_val
        overall_end <- current_overall_end()

        if (next_val <= overall_end) {
          updateSliderInput(session, "time_slider", value = next_val)
        } else {
          is_playing(FALSE)
        }
      }
    })

    playback_data <- reactive({
      req(input$selected_species, input$selected_localities, input$time_slider, input$step_size, input$view_mode, playback_period$start_date())

      species_to_map <- tolower(unname(input$selected_species))
      localities_to_map <- input$selected_localities
      current_time <- as.POSIXct(input$time_slider)

      # Determine start time based on view mode
      start_time <- if (input$view_mode == "cumulative") {
        as.POSIXct(playback_period$start_date())
      } else {
        step_val <- switch(input$step_size,
          "hour" = lubridate::hours(1),
          "day" = lubridate::days(1),
          "week" = lubridate::weeks(1),
          "month" = lubridate::months(1)
        )
        current_time - step_val
      }

      # Filter active deployments for bounds calculation
      active_locations <- core_data$deps %>%
        dplyr::filter(locality %in% localities_to_map) %>%
        dplyr::distinct(locationID, locality, .keep_all = TRUE)

      # Filter observations based on selected time window and filters
      obs_filtered <- core_data$obs %>%
        dplyr::filter(
          scientificName_lower %in% species_to_map,
          locality %in% localities_to_map,
          timestamp >= start_time,
          timestamp <= current_time
        )

      # Summarize counts by location
      obs_summary_location <- obs_filtered %>%
        dplyr::group_by(locationID, locationName, locality, longitude, latitude) %>%
        dplyr::summarise(
          count = sum(count, na.rm = TRUE),
          observation_ids = list(unique(observationID)),
          .groups = "drop"
        )

      # Summarize overall counts for sidebar
      obs_summary_locality <- active_locations %>%
        dplyr::select(locality) %>%
        dplyr::distinct() %>%
        dplyr::left_join(
          obs_filtered %>%
            dplyr::group_by(locality) %>%
            dplyr::summarise(count = sum(count), .groups = "drop"),
          by = "locality"
        ) %>%
        dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
        dplyr::arrange(locality)

      grand_total <- obs_summary_locality %>%
        dplyr::summarise(count = sum(count)) %>%
        dplyr::mutate(locality = "Grand Total")

      absolute_max <- core_data$obs %>%
        dplyr::filter(
          scientificName_lower %in% species_to_map,
          locality %in% localities_to_map,
          timestamp >= as.POSIXct(playback_period$start_date()),
          timestamp <= current_overall_end()
        ) %>%
        dplyr::group_by(locationID) %>%
        dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(max_count = max(count, na.rm = TRUE)) %>%
        dplyr::pull(max_count)

      if (is.na(absolute_max) || is.infinite(absolute_max)) absolute_max <- 0

      list(
        active_locations = active_locations,
        obs_summary_location = obs_summary_location,
        obs_summary_locality_with_total = dplyr::bind_rows(obs_summary_locality, grand_total),
        current_time = current_time,
        start_time = start_time,
        absolute_max = absolute_max,
        obs_filtered = obs_filtered
      )
    })

    # Map Output Initialization
    output$map_display <- renderLeaflet({
      leaflet() %>% addTiles(options = tileOptions(crossOrigin = TRUE))
    })

    # Reset bounds when locality selection changes to refit the map
    observeEvent(input$selected_localities, {
      current_bounds(NULL)
    }, ignoreNULL = FALSE)

    # Invalidate size when navigating to the tab or sub-tabs to ensure proper rendering
    observeEvent(list(session$rootScope()$input$nav, input$playback_map_tabs), {
      req(session$rootScope()$input$nav == "playback_map")
      shinyjs::runjs(sprintf(
        'setTimeout(function() {
           var mapWidget = HTMLWidgets.find(%s);
           if (mapWidget) {
             var mapObj = mapWidget.getMap();
             if (mapObj) {
               mapObj.invalidateSize();
             }
           }
         }, 100);', jsonlite::toJSON(paste0("#", ns("map_display")), auto_unbox = TRUE)
      ))

      # Re-apply bounds after sizing
      bounds <- current_bounds()
      if (!is.null(bounds)) {
        shinyjs::delay(150, {
          leafletProxy(MAP_ID) %>%
            fitBounds(
              lng1 = bounds$min_lng, lat1 = bounds$min_lat,
              lng2 = bounds$max_lng, lat2 = bounds$max_lat
            )
        })
      }
    })

    # Update Map Output
    observe({
      req(playback_data())
      data_for_map <- playback_data()

      update_density_map(
        map_id = MAP_ID,
        active_locations = data_for_map$active_locations,
        obs_summary_location = data_for_map$obs_summary_location,
        show_zero = TRUE,
        absolute_max = data_for_map$absolute_max
      )

      # Fit bounds
      if (is.null(current_bounds())) {
        bounds <- calculate_bounds_from_locations(data_for_map$active_locations, id, "playback")
        if (!is.null(bounds)) {
          current_bounds(bounds)
          leafletProxy(MAP_ID) %>%
            fitBounds(
              lng1 = bounds$min_lng, lat1 = bounds$min_lat,
              lng2 = bounds$max_lng, lat2 = bounds$max_lat
            )
        }
      }
    })

    output$playback_data_table <- DT::renderDataTable({
      req(playback_data())
      table_data <- playback_data()$obs_filtered %>%
        dplyr::select(locality, line, locationName, timestamp, count, `vernacularNames.eng`, scientificName, possible_duplicate, observationID)

      table_data <- prepare_spec_table_data(
          table_data,
          table_id = "playback_observations_browse",
          column_help = FALSE
        )$table_data

      DT::datatable(table_data, escape = FALSE,
                    options = list(pageLength = 10, searching = TRUE, lengthChange = TRUE, order = list(list(3, 'asc'))),
                    class = 'display', rownames = FALSE)
    })

    output$playback_summary <- renderUI({
      req(playback_data())
      data_summary <- playback_data()$obs_summary_locality_with_total

      start_str <- format(playback_data()$start_time, "%Y-%m-%d %H:%M")
      end_str <- format(playback_data()$current_time, "%Y-%m-%d %H:%M")

      date_html <- HTML(paste0("<strong>Showing window:</strong><br>", start_str, " to ", end_str, "<br><br>"))

      total_row_index <- nrow(data_summary)
      kable_table <- knitr::kable(data_summary, format = "html", escape = FALSE, col.names = c("Locality", "Count")) %>%
        kableExtra::kable_styling(
          bootstrap_options = c("hover", "condensed", "bordered"),
          full_width = FALSE, position = "center", font_size = 12
        ) %>%
        kableExtra::row_spec(total_row_index, bold = TRUE)

      HTML(paste0(date_html, kable_table))
    })

    # Provide the bounds calculation helper logic
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
        }
      }
      return(NULL)
    }
  })
}
