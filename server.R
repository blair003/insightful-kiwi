# server.R
# (c) Copyright 2024 Blair George

# Shiny App web to analyse and report on data in camtrap DP format
# Read about the camptrap DP format at https://github.com/tdwg/camtrap-dp

# PREREQUISITES
# The locationName needs to be in a specific format containing 6 digits, including a space.
# It is transformed into locality, line and location.

# Examples:
# locationName  locality_code   Line  location on the line
# KP 1_3        KP              1     3
# OH 4_2        OH              4     2
# OH 1_3        OH              1     3

# We later convert the locality_code to Location e.g. KP to Kohi Point

source("includes/metrics_functions.R")
source("includes/dashboard_functions.R")
source("modules/reporting_data_module.R")
source("modules/reporting_visualisations_module.R")
source("modules/reporting_rendering_module.R")

source("includes/data_preparation_functions.R")
source("includes/data_presentation_functions.R")
source("includes/media_functions.R")
source("includes/spatial_functions.R")
source("includes/visualisation_functions.R")
source("includes/utility_functions.R")

generate_multi_species_activity_plot <- function(sobs_data) {
  if(nrow(sobs_data) == 0) return(plot(1, type="n", axes=F, xlab="", ylab="", main="No Data"))
  sobs_data$scientificName <- as.character(sobs_data$scientificName)
  sobs_data$hour <- as.numeric(format(sobs_data$timestamp, "%H"))

  # We want a full grid of hour (0-23) and species
  all_hours <- 0:23
  species_list <- sort(unique(as.character(sobs_data$scientificName)))
  if(length(species_list) == 0) return(plot(1, type="n", axes=F, xlab="", ylab="", main="No Data"))

  # Count occurrences, assuming each row in sobs_data is an observation
  # or if count is a column use it, but typically it is count column. Let's use count if it exists, otherwise n()
  if ("count" %in% names(sobs_data)) {
    hourly_counts <- sobs_data %>%
      dplyr::group_by(scientificName, hour) %>%
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups="drop")
  } else {
    hourly_counts <- sobs_data %>%
      dplyr::group_by(scientificName, hour) %>%
      dplyr::summarise(count = dplyr::n(), .groups="drop")
  }

  all_combinations <- expand.grid(hour = all_hours, scientificName = species_list, stringsAsFactors = FALSE)
  plot_data <- dplyr::left_join(all_combinations, hourly_counts, by = c("hour", "scientificName")) %>%
    dplyr::mutate(
      count = dplyr::coalesce(.data$count, 0),
      hour_midpoint = .data$hour + 0.5
    )

  # Use config$globals$species_name_type if we want nice names
  name_type <- config$globals$species_name_type

  if (name_type %in% names(sobs_data)) {
    # Find one stable display name per species.
    nice_names <- sobs_data %>%
      dplyr::mutate(display_name = as.character(.data[[name_type]])) %>%
      dplyr::arrange(is.na(.data$display_name) | .data$display_name == "") %>%
      dplyr::group_by(.data$scientificName) %>%
      dplyr::summarise(display_name = dplyr::first(.data$display_name), .groups = "drop")

    plot_data <- plot_data %>% dplyr::left_join(nice_names, by = "scientificName")
    plot_data$display_name[is.na(plot_data$display_name)] <- plot_data$scientificName[is.na(plot_data$display_name)]
  } else {
    plot_data$display_name <- plot_data$scientificName
  }

  # Capitalize first letter
  plot_data$display_name <- stringr::str_to_title(plot_data$display_name)
  plot_data <- plot_data %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::mutate(
      species_total = sum(.data$count, na.rm = TRUE),
      activity_percent = dplyr::if_else(
        .data$species_total > 0,
        (.data$count / .data$species_total) * 100,
        0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      legend_label = sprintf(
        "%s (n=%s)",
        .data$display_name,
        format(.data$species_total, big.mark = ",", scientific = FALSE, trim = TRUE)
      )
    )

  display_levels <- plot_data %>%
    dplyr::distinct(.data$scientificName, .data$display_name, .data$legend_label) %>%
    dplyr::arrange(.data$display_name) %>%
    dplyr::pull(legend_label) %>%
    unique()
  plot_data$legend_label <- factor(plot_data$legend_label, levels = display_levels)

  activity_palette <- c(
    "#0072B2", "#D55E00", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#000000", "#8B5CF6",
    "#6B7280", "#A6761D"
  )
  species_count <- length(display_levels)
  has_low_sample_species <- any(plot_data$species_total > 0 & plot_data$species_total < 10)

  library(ggplot2)
  ggplot(
    plot_data,
    aes(
      x = hour_midpoint,
      y = activity_percent,
      fill = legend_label
    )
  ) +
    geom_col(width = 0.92, colour = "black", linewidth = 0.25, alpha = 0.82) +
    facet_wrap(~ legend_label) +
    coord_polar(start = 0) +
    scale_x_continuous(breaks = 0:23 + 0.5, limits = c(0, 24), labels = paste0(0:23, ":00")) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    scale_fill_manual(values = rep(activity_palette, length.out = species_count)) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      panel.grid.major.x = element_line(color = "grey80"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "none",
      strip.text = element_text(face = "bold")
    ) +
    labs(
      title = "Share of Detections by Hour of Day",
      caption = if (has_low_sample_species) {
        "Species with fewer than 10 detections are shown for completeness, but their activity pattern is uncertain."
      } else {
        NULL
      }
    )
}


server <- function(input, output, session) {

  source("includes/server_observation_handlers.R", local = TRUE)


  logger::log_debug("server.R, starting server() function")

  primary_period <- period_selection_module_server(
    id = "primary_period",
    period_groups = core_data$period_groups,
    summary_output_ids = c("summary_output_reporting", 
                           "summary_output_density_map", 
                           "summary_output_explorer_map"),
    selected = core_data$period_defaults$primary_period
  )

  comparative_period <- period_selection_module_server(
    id = "comparative_period", 
    period_groups = core_data$period_groups,
    selected = core_data$period_defaults$comparative_period
  )

  # Reactive filtering of deps and obs for primary and comparative periods
  filtered_deps_primary <- reactive({
    filter_deps(core_data$deps, primary_period$start_date(), primary_period$end_date())
  })
  
  filtered_deps_comparative <- reactive({
    filter_deps(core_data$deps, comparative_period$start_date(), comparative_period$end_date())
  })
  
  filtered_obs_primary <- reactive({
    filter_obs(core_data$obs, primary_period$start_date(), primary_period$end_date())
  })
  
  filtered_obs_comparative <- reactive({
    filter_obs(core_data$obs, comparative_period$start_date(), comparative_period$end_date())
  })
  
  
  # Dashboard widgets
  dashboard_plot_periods <- period_names_without_all(core_data$period_groups)
  dashboard_plot_periods <- dashboard_plot_periods[
    seq(core_data$period_defaults$primary_period_index, length(dashboard_plot_periods))
  ]
  dashboard_plot_deps <- core_data$deps %>%
    dplyr::filter(as.character(period) %in% dashboard_plot_periods)

  # Initialize the period selection servers for dashboard
  main_dashboard_current_period <- period_selection_module_server("main_dashboard_current_period", period_groups = core_data$period_groups, selected = species_dashboard_period_defaults(core_data)$current_period)
  main_dashboard_prior_period <- period_selection_module_server("main_dashboard_prior_period", period_groups = core_data$period_groups, selected = species_dashboard_period_defaults(core_data)$prior_period)
  main_dashboard_last_year_period <- period_selection_module_server("main_dashboard_last_year_period", period_groups = core_data$period_groups, selected = species_dashboard_period_defaults(core_data)$last_year_period)

  output$main_dashboard_current_period_name <- renderText({ main_dashboard_current_period$period_name() })
  output$main_dashboard_prior_period_name <- renderText({ main_dashboard_prior_period$period_name() })
  output$main_dashboard_last_year_period_name <- renderText({ main_dashboard_last_year_period$period_name() })

  render_tab_cards <- function(period_name) {
    combine_localities <- input[["dashboard_rai_plot-combine_localities"]]
    if (is.null(combine_localities)) {
      combine_localities <- TRUE
    }

    selected_localities <- input[["dashboard_rai_plot-selected_localities"]]
    if (is.null(selected_localities) || length(selected_localities) == 0) {
      selected_localities <- unique(core_data$deps$locality)
    }

    if (isTRUE(combine_localities)) {
      return(render_dashboard_metric_cards(selected_localities, period_name))
    }

    tagList(lapply(selected_localities, function(locality) {
      tagList(
        div(class = "dashboard-locality-heading", locality_display_name(locality)),
        render_dashboard_metric_cards(locality, period_name)
      )
    }))
  }

  render_tab_weather <- function(period_name) {
    combine_localities <- input[["dashboard_rai_plot-combine_localities"]]
    if (is.null(combine_localities)) {
      combine_localities <- TRUE
    }

    selected_localities <- input[["dashboard_rai_plot-selected_localities"]]
    if (is.null(selected_localities) || length(selected_localities) == 0) {
      selected_localities <- unique(core_data$deps$locality)
    }

    period_info <- core_data$period_groups[[period_name]]
    if (is.null(period_info)) {
      return(NULL)
    }
    start_date <- period_info$start_date
    end_date <- period_info$end_date

    if (isTRUE(combine_localities)) {
      return(render_weather_cards(selected_localities, start_date, end_date))
    }

    tagList(lapply(selected_localities, function(locality) {
      tagList(
        div(class = "dashboard-locality-heading", locality_display_name(locality)),
        render_weather_cards(locality, start_date, end_date)
      )
    }))
  }

  output$main_dashboard_current_period_cards <- renderUI({ render_tab_cards(main_dashboard_current_period$period_name()) })
  output$main_dashboard_prior_period_cards <- renderUI({ render_tab_cards(main_dashboard_prior_period$period_name()) })
  output$main_dashboard_last_year_period_cards <- renderUI({ render_tab_cards(main_dashboard_last_year_period$period_name()) })

  output$main_dashboard_current_period_weather_cards <- renderUI({ render_tab_weather(main_dashboard_current_period$period_name()) })
  output$main_dashboard_prior_period_weather_cards <- renderUI({ render_tab_weather(main_dashboard_prior_period$period_name()) })
  output$main_dashboard_last_year_period_weather_cards <- renderUI({ render_tab_weather(main_dashboard_last_year_period$period_name()) })

  observeEvent(input$dashboard_rai_details_clicked, {
    detail_parts <- strsplit(input$dashboard_rai_details_clicked, "\\|", fixed = FALSE)[[1]]
    rai_group <- detail_parts[[1]]
    locality <- if (length(detail_parts) > 1 && detail_parts[[2]] != "ALL") {
      strsplit(detail_parts[[2]], ",", fixed = TRUE)[[1]]
    } else {
      NULL
    }
    period_name <- if (length(detail_parts) > 2 && detail_parts[[3]] != "ALL") {
      detail_parts[[3]]
    } else {
      NULL
    }

    lower_is_better <- rai_group %in% c("Mustelids", "Rats")
    show_rai_metric_modal(dashboard_rai_metric(rai_group, lower_is_better, locality, period_name))
  })

  observeEvent(input$dashboard_weather_details_clicked, {
    token <- input$dashboard_weather_details_clicked
    show_weather_modal(token$lat, token$lng, token$start_date, token$end_date)
  })
  
  # Camera Hours Logged: Sum of camera hours
  output$dashcard_camera_hours <- renderText({
    format(round(sum(core_data$deps$camera_hours, na.rm = TRUE)), big.mark = ",")
  })

  output$dashcard_camera_days <- renderText({
    paste(format_dash_number(sum(core_data$deps$camera_hours, na.rm = TRUE) / 24), "camera days")
  })
  
  output$dashcard_data_updated <- renderText({
    format(as.POSIXct(core_data$created, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"), 
           "%d/%m/%Y")
  })

  output$dashcard_data_package_name<- renderText({
    core_data$name
  })

  plotting_module_server(
    id = "dashboard_rai_plot",
    type = NULL,
    obs = core_data$obs,
    deps = dashboard_plot_deps,
    species_override = NULL,
    rai_groups = config$globals$rai_groups,
    rai_norm_hours = config$globals$rai_norm_hours,
    use_net = config$globals$rai_net_count
  )

  plotting_module_server(
    id = "spp_obs_plot_visualisations",
    type = NULL,
    obs = core_data$obs,
    deps = dashboard_plot_deps,
    species_override = NULL
  )
  # Initialize species dashboards dynamically (Lazy Loading)
  loaded_species_dashboards <- reactiveVal(character())

  observeEvent(input$nav, {
    nav_item <- input$nav

    if (startsWith(nav_item, "species_dashboard_")) {
      sci_name_make_names <- sub("^species_dashboard_", "", nav_item)

      # Check if already loaded
      if (!(sci_name_make_names %in% loaded_species_dashboards())) {

        # Find the full scientific and vernacular names
        found <- FALSE
        for (group_name in names(core_data$spp_classes)) {
          species_in_group <- core_data$spp_classes[[group_name]]
          for (s_name in names(species_in_group)) {
            s_sci_name <- species_in_group[[s_name]]
            if (make.names(s_sci_name) == sci_name_make_names) {

              # Initialize module
              species_dashboard_module_server(
                id = paste0("species_dashboard_", make.names(s_sci_name)),
                species_name = s_sci_name,
                vernacular_name = s_name,
                obs = filtered_obs_primary,
                deps = filtered_deps_primary,
                core_data = core_data
              )

              # Add to loaded list
              loaded_species_dashboards(c(loaded_species_dashboards(), sci_name_make_names))
              logger::log_debug(sprintf("server.R, lazily loaded species dashboard for %s", s_sci_name))

              found <- TRUE
              break
            }
          }
          if (found) break
        }
      }
    }
  })
  
  
  
  setup_table_output(output, 
                     table_id = "rawdata_deployments_browse", 
                     data = core_data$deps,
                     table_type = "paged",
                     table_order = list(list(4, 'asc'))) 
  
  
  setup_table_output(output, 
                     table_id = "rawdata_observations_browse", 
                     data = core_data$obs,
                     table_type = "paged",
                     table_order = list(list(4, 'asc'))) 
  
  
    
    # Render UI for the dashboard plot
    output$dash_spp_obs_plot <- renderUI({
#      if (isTRUE(input$is_fullscreen)) {
#        # Full-screen content: 2-tab layout with data tables and plot
#        navset_card_tab(
#          nav_panel("Tab 1", plotOutput("spp_obs_plot")), # Plot goes here
#          nav_panel("Tab 2", DT::dataTableOutput("obs_data_table")) # Placeholder for datatable
#        )
#      } else {
#        # Normal content: Just the plot
        plotOutput("spp_obs_plot")
#      }
    })
    
    # Optionally render a datatable for full-screen view
    output$obs_data_table <- DT::renderDataTable({
      DT::datatable(filtered_data())
    })

    # Observe changes in input$nav and toggle sidebar accordingly
    # Not on mobile in portrait mode
    observe({
      nav_value <- input$nav
      logger::log_debug(sprintf("server.R, observer triggered, input$nav is %s", nav_value))
      
      # Debugging: Log the current value of input$nav to the console
      runjs(sprintf("console.log(%s);", jsonlite::toJSON(paste0("Nav changed to: ", nav_value), auto_unbox = TRUE)))
      
      # JavaScript code to handle sidebar toggling. Only applies on non-mobile
      # devices or likely mobile devices in portrait mode (768)
      nav_json <- jsonlite::toJSON(nav_value, auto_unbox = TRUE)
      runjs(sprintf("toggleSidebar(%s, defaultSidebarState);", nav_json))
    })

  # Listens for info_field_clicked returning from JS customInfoButtonClickHandler in ui.R head tags
  # Generates an information modal with information relevant to the field clicked
  observeEvent(input$info_field_clicked, {

    field_name <- input$info_field_clicked
    column_description <- get_column_description(field_name)
    
    if (is.null(column_description)) {
      # Check for the specific edge case where field_name includes "RAI ± SE"
      if (grepl("RAI ± SE", field_name)) {
        # If the edge case is matched, modify field_name accordingly
        field_name <- "RAI ± SE"
      } else if (grepl("Count", field_name)) {
        # If the edge case is matched, modify field_name accordingly
        field_name <- "Count"
      }
      
      column_description <- get_column_description(field_name)
    }
    
    if (!is.null(column_description)) {
      # Generate and display the modal with the field information
      generate_info_modal(field_name, column_description)
    } else {
      # Handle cases where field_name is not found in column_descriptions
      logger::log_warn(paste("Clicked field not recognised:", field_name))
    }
    
    runjs(sprintf("gtag('event', 'click', {'event_category': 'info_button', 'event_label': %s});", jsonlite::toJSON(field_name, auto_unbox = TRUE)))
    
  })
 

  


  current_reporting <- reporting_data_module_server("current_reporting", filtered_obs_primary, filtered_deps_primary)
  current_visualisations <- reporting_visualisations_module_server("current_visualisations", filtered_obs_primary)
  reporting_rendering_module_server("current_tables", current_reporting$reporting_data)
  
  # Convert ggplot object to plotly object
  
  # Optionally, you can customize the tooltip content
  # interactive_plot <- interactive_plot %>% layout(tooltip = c("x", "y", "fill"))
  
  output$locality_ggplot_daily_species_count <- renderPlot({
    current_visualisations$visualisations()$locality_ggplot_daily_species_count
    
  })
  
  output$locality_ggplotly_daily_species_count <- renderPlotly({
    ggplotly(current_visualisations$visualisations()$locality_ggplot_daily_species_count)
    
  })
  
  output$reporting_executive_summary <- renderUI({
    reporting_rendering_module_ui("current_tables", "reporting_executive_summary")
  })
  
  output$reporting_species_summary <- renderUI({
    reporting_rendering_module_ui("current_tables", "reporting_species_summary")
  })
  
  output$reporting_results_summary <- renderUI({
    reporting_rendering_module_ui("current_tables", "reporting_results_summary")
  })
  
#  output$reporting_camera_network_overview <- renderUI({
#    reporting_rendering_module_ui("current_tables", "reporting_camera_network_overview")
#  })
#  
#  output$reporting_raw_data_browse <- renderUI({
#    reporting_rendering_module_ui("current_tables", "reporting_raw_data_browse")
#  })
  

  
  # Updates visibility reactiveVal's based on which which menu is showing
  # Covers standard map features and autoplay features
  observeEvent(input$nav, {
    nav_item <- input$nav

    runjs(sprintf("gtag('config', %s, {'page_path': %s});",
                  jsonlite::toJSON(config$globals$ga_tag, auto_unbox = TRUE),
                  jsonlite::toJSON(paste0("/", nav_item), auto_unbox = TRUE)))

    
    if (nav_item  == "dashboard") {
      # Run latest images
      #message("observeEvent(input$nav, viewing dashboard page -- triggered image update")
      # latest_images(get_latest_images())
    }

    
    if (nav_item  == "reporting") {
      #print("Reporting menu")
      # Generate reporting data for the current period

      
      
      # Generate reporting datatables, all the action happens in reporting_rendering_module_ui, server returns nothing
      # Uses prepared_data for creation of observations and deployments browse, maybe I should do in reporting_data_module_server
      #reporting_rendering_module_server("current_tables", current$reporting_data, prepared_data)
      
      output$season_selection_text <- renderUI({
        package_date_text <- generate_package_date_text(core_data$created)
        season_selection_text <- generate_season_selection_text(
          primary_period$start_date(),
          primary_period$end_date(),
          primary_period$selected()$season
        )
        
        combined_text <- paste(package_date_text, "<br><br>", season_selection_text)
        HTML(combined_text)
      })
    }


  })

  
  
  observeEvent(input$observation_click, {
    # Parse the input
   # browser()
    parts <- strsplit(input$observation_click, "\\|")[[1]]
    
    # Extract components
    observation_id <- parts[1]   # Extract observationID
    sequence_id <- parts[2]      # Extract sequenceID
    action_type <- paste(parts[3:(length(parts) - 1)], collapse = "|")  # Handle multi-part action_type
    timestamp <- parts[length(parts)]  # Extract timestamp
    
    # Remove any leading or trailing | from action_type
    action_type <- gsub("^\\|+|\\|+$", "", action_type)
    
    # Further split action_type to determine main action and mode
    action_parts <- strsplit(action_type, "\\|")[[1]]
    main_action <- action_parts[1]  # Extract main action (e.g., "view_sequence")
    view_mode <- if (length(action_parts) > 1) action_parts[2] else "modal"  # Default to modal if missing

    # Backwards compatibility: if main_action is "modal", it likely meant view_sequence in modal mode
    if (main_action == "modal") {
      main_action <- "view_sequence"
      view_mode <- "modal"
    }
    
    # Validate that at least one ID is present and valid
    if ((!nchar(observation_id) > 0 || !is_valid_UUID(observation_id)) &&
        (!nchar(sequence_id) > 0 || !is_valid_UUID(sequence_id))) {
      stop("Invalid input: either observation_id or sequence_id must be provided.")
    }
    
    # Handle view_sequence (requires observation_id)
    if (main_action == "view_sequence") {
      handle_view_sequence(observation_id, view_mode)
    } 
    # Handle edit_sequence (requires sequence_id)
    else if (main_action == "edit_sequence") {
     # handle_edit_sequence(sequence_id)
    } else {
      stop("Unknown action_type.")
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$observationID_click, {
    parts <- strsplit(input$observationID_click, "\\|")[[1]]
    observation_id <- parts[1]
    action_type <- parts[2] 

    if (nchar(observation_id) > 0) {
      if (is_valid_UUID(observation_id)) {

        observation_details <- create_observation_viewer_output(observation_id, action_type)
        
        if (!is.null(observation_details)) {

          if (action_type == "modal") {
            image_output <- create_observation_images_ui(observation_details$sequence_media_info, 
                                                         observation_id,
                                                         context = "modal", review_nav = list(current_index = current_index, total_sequences = total_sequences))
            
            show_image_modal(observation_id, image_output$ui_elements)
            session$sendCustomMessage(type = "refreshCarousel", message = list(carouselId = image_output$carousel_id))
          } else if (action_type == "pageview") {
            image_output <- create_observation_images_ui(observation_details$sequence_media_info, 
                                                         observation_id,
                                                         context = "pageview")
            
            output$observation_images_pageview <- renderUI({
              image_output$ui_elements
            })
            
            updateNavbarPage(session, "main_menu", selected = "observations")
            # Navigate to the Viewer tab if not already there
            updateTabsetPanel(session, "observations_tabsetpanel", selected = "Viewer")
 
          }
          
          # If there was no cache hit, update the image cache so we can display from cache next time. The UIx is too 
          # slow to download the images then always display from cache. Better to have the progressive loading of the 
          # jpeg and display the version from Agouti if we don't have them
          if (!image_output$cache_hit) {
            update_image_cache(observation_details$sequence_media_info)
          }
          # Reset so that future clicks to same link will work
          shinyjs::click("reset_button")
        }
      }
    }

  })
  
  
  
  observe({
    # Use URL parameters on initial load

    query <- parseQueryString(session$clientData$url_search)
    print(sprintf("Query string is %s", query))
    if (!is.null(query$observation_id)) {
      # Extracted observation ID from URL
      observation_id <- query$observation_id
      
      if (nchar(observation_id) > 0) {
        if (is_valid_UUID(observation_id)) {
          observation_details <- create_observation_viewer_output(observation_id, "pageview")
          
          if (!is.null(observation_details)) {
            # Set the value in the text input (optional)
            updateTextInput(session, "observationID_input", value = observation_id)
            
            image_output <- create_observation_images_ui(observation_details$sequence_media_info, 
                                                         observation_id,
                                                         context = "pageview")
            
            
            output$observation_images_pageview <- renderUI({
              image_output$ui_elements
            })
            
            updateNavbarPage(session, "main_menu", selected = "observations")
            # Navigate to the Viewer tab if not already there
            updateTabsetPanel(session, "observations_tabsetpanel", selected = "Viewer")
            
            # Check and download for next time
            if (!image_output$cache_hit) {
              update_image_cache(observation_details$sequence_media_info)
            }
          }
        } else {
          output$observation_images_pageview_error <- renderUI({ "ObservationID not found" })
        }
      }
    }
  })
  
  
  # This listens to the actionButton on the Observation viewer page
  observeEvent(input$view_observation, {
    # This always implies a page view action since it's triggered from within the Viewer tab
    observation_id <- trimws(input$observationID_input)
    output$observation_images_pageview_error <- renderUI({ NULL })
    
    if (nchar(observation_id) > 0) {
      if (is_valid_UUID(observation_id)) {
        observation_details <- create_observation_viewer_output(observation_id, "pageview")
    
        if (!is.null(observation_details)) {
          image_output <- create_observation_images_ui(observation_details$sequence_media_info, 
                                                       observation_id,
                                                       context = "pageview")
          
          output$observation_images_pageview <- renderUI({
            image_output$ui_elements
          })
          # Check and download for next time
          if (!image_output$cache_hit) {
            update_image_cache(observation_details$sequence_media_info)
          }
        }
      } else {
        output$observation_images_pageview_error <- renderUI({ "ObservationID not found" })
      }
    }
  })
  
  
  
  observeEvent(input$reset_button, {
    # This is just to trigger a change in input values; no action needed here but leave it
    # Related to shinyjs::click("reset_button")
  }, ignoreNULL = FALSE)
  
  
  
  # Initialize a reactive value to store the latest images
  latest_images <- reactiveVal()
  
  # Initially populate latest_images
  latest_images(get_latest_images())
  
  output$imageSlider <- renderUI({
    
    image_paths <- latest_images() 
    img_tags <- lapply(image_paths, function(path) {
      tags$img(src = path, style = "width: 100%;") # Ensure images fill their container
    })
    
    tagList(
      div(id = "slickSlider", class = "slider", img_tags),
      tags$script(HTML("initImageSlider('slickSlider');"))
    )
  })
  

  

  
  ########### DENSITY MAP FEATURE ###########
  
  density_map_primary <- NULL
  density_map_comparative <- NULL
  loaded_density_tabs <- reactiveVal(character())
  
  # Used for the tab names
  output$primary_season_name <- renderText({
    primary_period$period_name()
  })
  
  output$comparative_season_name <- renderText({
    comparative_period$period_name()
  })
  
  observeEvent(list(input$nav, input$density_map_tabs), {
    req(input$nav == "density_map")

    current_tab <- input$density_map_tabs
    if (!is.null(current_tab) && !(current_tab %in% loaded_density_tabs())) {

      if (current_tab == "primary") {
        logger::log_debug("server.R, lazily calling mapping_module_server() for density_map_primary")
        density_map_primary <<- mapping_module_server(
          id = "density_map_primary",
          type = "density",
          obs = filtered_obs_primary,
          deps = filtered_deps_primary
        )
      } else if (current_tab == "comparative") {
        logger::log_debug("server.R, lazily calling mapping_module_server() for density_map_comparative")
        # Ensure primary is initialized first if we need its reactive properties, or fallback
        if (is.null(density_map_primary)) {
          density_map_primary <<- mapping_module_server(
            id = "density_map_primary",
            type = "density",
            obs = filtered_obs_primary,
            deps = filtered_deps_primary
          )
          loaded_density_tabs(c(loaded_density_tabs(), "primary"))
        }

        density_map_comparative <<- mapping_module_server(
          id = "density_map_comparative",
          type = "density",
          obs = filtered_obs_comparative,
          deps = filtered_deps_comparative,
          species_override = density_map_primary$selected_species,
          localities_override = density_map_primary$selected_localities
        )
      }

      loaded_density_tabs(c(loaded_density_tabs(), current_tab))
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  
  
  ########### ACTIVITY PATTERNS FEATURE ###########
  activity_patterns_map <- NULL
  activity_patterns_loaded <- reactiveVal(FALSE)

  observeEvent(input$nav, {
    if (input$nav == "activity_patterns" && !activity_patterns_loaded()) {
      logger::log_debug("server.R, lazily calling mapping_module_server() for activity_patterns_map")
      activity_patterns_map <<- mapping_module_server(
        id = "activity_patterns_map",
        type = "observation", # Just to get selected species and localities
        obs = reactive({core_data$obs}),
        deps = reactive({core_data$deps})
      )
      activity_patterns_loaded(TRUE)
    }
  })

  # Reactive to filter obs for activity pattern plots
  activity_patterns_obs <- reactive({
    req(activity_patterns_loaded(), activity_patterns_map$selected_species(), activity_patterns_map$selected_localities())
    species <- tolower(activity_patterns_map$selected_species())
    localities <- activity_patterns_map$selected_localities()

    filtered_obs <- core_data$obs %>%
      dplyr::filter(tolower(scientificName) %in% species, locality %in% localities)

    return(filtered_obs)
  })

  # Overall Activity Pattern
  output$activity_patterns_overall <- renderPlot({
    req(activity_patterns_obs())
    generate_multi_species_activity_plot(activity_patterns_obs())
  })

  # Current Period Activity Pattern
  output$activity_patterns_current <- renderPlot({
    req(activity_patterns_obs(), main_dashboard_current_period$start_date(), main_dashboard_current_period$end_date())
    period_obs <- filter_obs(activity_patterns_obs(), main_dashboard_current_period$start_date(), main_dashboard_current_period$end_date())
    generate_multi_species_activity_plot(period_obs)
  })
  output$activity_patterns_current_period_name <- renderText({
    main_dashboard_current_period$period_name()
  })

  # Prior Period Activity Pattern
  output$activity_patterns_prior <- renderPlot({
    req(activity_patterns_obs(), main_dashboard_prior_period$start_date(), main_dashboard_prior_period$end_date())
    period_obs <- filter_obs(activity_patterns_obs(), main_dashboard_prior_period$start_date(), main_dashboard_prior_period$end_date())
    generate_multi_species_activity_plot(period_obs)
  })
  output$activity_patterns_prior_period_name <- renderText({
    main_dashboard_prior_period$period_name()
  })

  # Last Year Period Activity Pattern
  output$activity_patterns_last_year <- renderPlot({
    req(activity_patterns_obs(), main_dashboard_last_year_period$start_date(), main_dashboard_last_year_period$end_date())
    period_obs <- filter_obs(activity_patterns_obs(), main_dashboard_last_year_period$start_date(), main_dashboard_last_year_period$end_date())
    generate_multi_species_activity_plot(period_obs)
  })
  output$activity_patterns_last_year_period_name <- renderText({
    main_dashboard_last_year_period$period_name()
  })

  ########### OBSERVATION MAP FEATURE ###########
  
  observation_map <- NULL
  observation_map_loaded <- reactiveVal(FALSE)
  
  observeEvent(input$nav, {
    if (input$nav == "observation_map" && !observation_map_loaded()) {
      logger::log_debug("server.R, lazily calling mapping_module_server() for observation_map")
      observation_map <<- mapping_module_server(
        id = "observation_map",
        type = "observation",
        obs = filtered_obs_primary,
        deps = filtered_deps_primary,
        period_start_date = primary_period$start_date, # Pass reactive from period_selection_module
        period_end_date = primary_period$end_date     # Pass reactive from period_selection_module
        # The module will use its own internal input$selected_species and input$enhance_map_details
        # from the UI elements defined by mapping_module_ui in the sidebar.
      )
      observation_map_loaded(TRUE)
    }
  })
  

  ########### REPORT DOWNLOAD ###########
  
  #message(sprintf("Report download requested in %s format", input$report_format)),
  # Create a downloadable report
  # Creates an html file then converts that to PDF for better layout

  output$download_report <- downloadHandler(
    filename = function() {
      generate_report_filename(primary_period$period_name(), core_data$created, input$report_format)
    },
    
    content = function(file) {

      reports_cache_dir <- "cache/reports"
      density_maps_dir <- paste0(reports_cache_dir, "/density_maps")
      plots_dir <- paste0(reports_cache_dir, "/plots")
      
      period_name <- primary_period$period_name()
      package_created_date <- core_data$created
      package_date_string <- format(as.POSIXct(package_created_date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"), format = "%Y%m%d%H%M", tz = "Pacific/Auckland")
      
      ensure_directories_exist(reports_cache_dir, density_maps_dir, plots_dir)
      
      report_html <- file.path(reports_cache_dir, gsub(" ", "_", paste0(period_name, "_deployment_report_", package_date_string, ".html")))
      
      if (!file.exists(report_html)) {
        
        start_date <- primary_period$start_date()
        end_date <- primary_period$end_date()
        reporting_data <- current_reporting$reporting_data()
        #browser()
        data_to_export <- collate_reporting_data(
          start_date, 
          end_date, 
          period_name, 
          reporting_data, 
          filtered_deps_primary(),
          config
        )
      #  browser()
        # Maintain ordering per config$globals$spp_classes
        named_class_species <- reporting_data$spp_summary$locality %>% 
          dplyr::filter(species_class != config$globals$spp_class_unclassified) %>% 
          mutate(scientificName = factor(scientificName, levels = unname(unlist(config$globals$spp_classes)), ordered = TRUE)) %>% 
          arrange(scientificName) %>% 
          distinct(scientificName, vernacularNames.eng, .keep_all = TRUE) %>% 
          select(scientificName, vernacularNames.eng)
        
        data_to_export$data$density_maps <- generate_density_maps(
          named_class_species,
          period_name,
          reports_cache_dir,
          package_date_string,
          filtered_obs_primary(),
          filtered_deps_primary(),
          config$globals$species_name_type
        )
        
        
        unique_localities <- filtered_deps_primary() %>% 
          dplyr::distinct(locality) %>% 
          dplyr::pull(locality)
        
        data_to_export$data$plots <- generate_locality_plots(
          filtered_obs_primary(), 
          unique_localities, 
          period_name,
          reports_cache_dir,
          plots_dir, 
          package_date_string
        )
       # browser()
        render_report(period_name, 
                      package_date_string, 
                      reports_cache_dir, 
                      data_to_export
                      )
      }
      
      if (input$report_format == "pdf") {
      #  browser()
        report_pdf <- file.path(reports_cache_dir, gsub(" ", "-", paste0(period_name, "_deployment_report_", package_date_string, ".pdf")))
        convert_to_pdf(report_html, report_pdf)
        file.copy(report_pdf, file, overwrite = TRUE)
      } else {
        file.copy(report_html, file, overwrite = TRUE)
      }
    }
  )
  
  

  

  # Observe review sequences click from dashboard
  observeEvent(input$review_sequences_click, {
    action_data <- input$review_sequences_click

    # Extract details
    period_name <- action_data$period_name
    rai_group <- action_data$rai_group
    species_name <- action_data$species_name
    locality_filter <- action_data$locality
    supplied_observation_ids <- action_data$observation_ids

    if (!is.null(supplied_observation_ids) && length(supplied_observation_ids) > 0) {
      observation_ids <- unlist(supplied_observation_ids, use.names = FALSE)
      observation_ids <- observation_ids[nzchar(observation_ids)]

      if (length(observation_ids) > 0) {
        show_review_sequences_modal(observation_ids, 1)
      } else {
        showModal(modalDialog(
          title = "No Sequences Found",
          "There are no sequences to review for this selection.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }

      return()
    }

    if (!is.null(period_name) && period_name == "ALL") {
      filtered_obs <- core_data$obs
    } else if (!is.null(period_name) && period_name %in% names(core_data$period_groups)) {
      period <- core_data$period_groups[[period_name]]

      # Filter obs
      filtered_obs <- filter_obs(core_data$obs, period$start_date, period$end_date)
    } else {
      filtered_obs <- NULL
    }

    if (!is.null(filtered_obs)) {

      # Apply locality filter
      if (!is.null(locality_filter) && locality_filter != "ALL") {
        localities <- strsplit(locality_filter, ",")[[1]]
        filtered_obs <- filtered_obs %>% filter(locality %in% localities)
      }

      # Apply species filter based on either a species dashboard click or an RAI group click.
      if (!is.null(species_name) && nzchar(species_name)) {
        filtered_obs <- filtered_obs %>% filter(tolower(scientificName) == tolower(species_name))
      } else {
        species_list <- config$globals$rai_groups[[rai_group]]
        if (!is.null(species_list)) {
          filtered_obs <- filtered_obs %>% filter(tolower(scientificName) %in% tolower(species_list))
        }
      }

      # Extract observation IDs
      observation_ids <- filtered_obs$observationID

      if (length(observation_ids) > 0) {
        show_review_sequences_modal(observation_ids, 1)
      } else {
        showModal(modalDialog(
          title = "No Sequences Found",
          "There are no sequences to review for this selection.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    }
  })


  observeEvent(input$review_nav_click, {
    nav_click <- input$review_nav_click
    if (is.list(nav_click)) {
      new_index <- nav_click$index
      initial_slide <- if (!is.null(nav_click$initial_slide)) nav_click$initial_slide else 0
    } else {
      new_index <- nav_click
      initial_slide <- 0
    }
    state <- session$userData$review_sequences_state

    if (!is.null(state)) {
      show_review_sequences_modal(state$observation_ids, new_index, initial_slide)
    }
  })


  # Observe review sequences click from density map
  observeEvent(input$density_map_review_sequences_click, {
    action_data <- input$density_map_review_sequences_click

    location_name <- action_data$location_name
    locality <- action_data$locality
    supplied_observation_ids <- action_data$observation_ids

    if (!is.null(supplied_observation_ids) && length(supplied_observation_ids) > 0) {
      observation_ids <- unlist(supplied_observation_ids, use.names = FALSE)
      observation_ids <- observation_ids[nzchar(observation_ids)]

      if (length(observation_ids) > 0) {
        show_review_sequences_modal(observation_ids, 1)
      } else {
        showModal(modalDialog(
          title = "No Sequences Found",
          "There are no sequences to review for this selection.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }

      return()
    }

    # We need to know which period we are in. Check if we are on primary or comparative tab.
    active_tab <- input$density_map_tabs

    if (is.null(active_tab) || active_tab == "primary") {
      period_obs <- filtered_obs_primary()
      selected_species <- density_map_primary$selected_species()
    } else {
      period_obs <- filtered_obs_comparative()
      selected_species <- density_map_comparative$selected_species()
    }

    if (!is.null(period_obs) && !is.null(selected_species)) {
      # Filter to the specific location, locality and selected species
      species_to_map <- tolower(unname(selected_species))

      location_obs <- period_obs %>%
        filter(
          locationName == location_name,
          locality == locality,
          tolower(scientificName) %in% species_to_map
        )

      observation_ids <- location_obs$observationID

      if (length(observation_ids) > 0) {
        show_review_sequences_modal(observation_ids, 1)
      } else {
        showModal(modalDialog(
          title = "No Sequences Found",
          "There are no sequences to review for this selection.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    }
  })

} # server

