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

source("modules/reporting_data_module.R")
source("modules/reporting_visualisations_module.R")
source("modules/reporting_rendering_module.R")

source("includes/data_preparation_functions.R")
source("includes/data_presentation_functions.R")
source("includes/get_column_descriptions.R")
source("includes/mapping_functions.R")
source("includes/other_functions.R")


server <- function(input, output, session) {
  logger::log_debug("server.R, starting server() function")

  primary_period <- period_selection_module_server(
    id = "primary_period",
    period_groups = core_data$period_groups,
    summary_output_ids = c("summary_output_reporting", 
                           "summary_output_density_map", 
                           "summary_output_explorer_map")
  )

  comparative_period <- period_selection_module_server(
    id = "comparative_period", 
    period_groups = core_data$period_groups
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
  # Kiwi Detections: Sum the count for "Apteryx mantelli"
  output$dashcard_kiwi_observations <- renderText({
    sum(core_data$obs %>% 
          filter(scientificName_lower == "apteryx mantelli") %>% 
          pull(count), na.rm = TRUE)
  })
  
  # Total Animal Detections: Sum of animal detections
  output$dashcard_animal_detections <- renderText({
    format(round(sum(core_data$deps$animal_detections_count, na.rm = TRUE)), big.mark = ",")
  })
  
  # Camera Hours Logged: Sum of camera hours
  output$dashcard_camera_hours <- renderText({
    format(round(sum(core_data$deps$camera_hours, na.rm = TRUE)), big.mark = ",")
  })
  
  output$dashcard_data_updated <- renderText({
    format(as.POSIXct(core_data$created, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"), 
           "%d/%m/%Y")
  })

  plotting_module_server(
    id = "spp_obs_plot_dashboard",
    type = NULL,
    obs = core_data$obs,
    deps = core_data$deps,
    species_override = NULL
  )
  
  
  
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
      runjs(sprintf("console.log('Nav changed to: %s');", nav_value))
      
      # JavaScript code to handle sidebar toggling. Only applies on non-mobile
      # devices or likely mobile devices in portrait mode (768)
      runjs(sprintf("
      if (window.innerWidth > 768) {
        if (defaultSidebarState['%s'] !== undefined) {
          console.log('Default sidebar state for %s is: ' + defaultSidebarState['%s']);
          if (defaultSidebarState['%s']) {
            if (!$('.collapse-toggle').attr('aria-expanded') || $('.collapse-toggle').attr('aria-expanded') === 'false') {
              console.log('Opening the sidebar by clicking the toggle button.');
              $('.collapse-toggle').click();  // Click the toggle button to open the sidebar
            }
          } else {
            if ($('.collapse-toggle').attr('aria-expanded') === 'true') {
              console.log('Collapsing the sidebar by clicking the toggle button.');
              $('.collapse-toggle').click();  // Click the toggle button to close the sidebar
            }
          }
        }
      } else {
        console.log('Skipping sidebar toggle logic on mobile devices.');
      }
    ", nav_value, nav_value, nav_value, nav_value))
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
    
    runjs(sprintf("gtag('event', 'click', {'event_category': 'info_button', 'event_label': '%s'});", field_name))
    
  })
 

  
  # This reactive function filters obs_filtered <- core_data$obs based on selected dates and species 
  explorer_map_data <- reactive({
    #browser()
    req(explorer_map_selected())
    
    start_date <- as.Date(primary_period$start_date())  
    end_date <- as.Date(primary_period$end_date()) 
    
    logger::log_info(paste("explorer_map_data() reactive triggered, filtering data based on ", start_date, end_date))

    species_classes_selected <- unlist(
      lapply(input$explorer_map_selected_species, tolower)
    )

    #str(species_classes_all)
    obs_filtered <- core_data$obs %>%
      dplyr::filter(timestamp <= end_date & timestamp >= start_date) %>%
      dplyr::filter(tolower(scientificName) %in% species_classes_selected)

    # Create a summary of observations, ordered by species_rank
    obs_summary <- obs_filtered %>%
      group_by(!!sym(config$globals$species_name_type), species_class, species_rank) %>%  # Include species_rank in group_by
      summarise(Count = sum(count, na.rm = TRUE)) %>%
      ungroup() %>%  # Remove grouping
      rename(Species = !!sym(config$globals$species_name_type)) %>%
      arrange(species_rank) %>%  # Now arrange based on species_rank
      select(Species, Count)  # Keep only Species and Count for display
    
    
    # Iniitalise a list to hold mapping data
    mapping_data <- list()
    mapping_data$observations <- obs_filtered
   # browser()
    active_locations <- core_data$deps %>%
      dplyr::filter(core_data$deps$start <= as.Date(end_date) & core_data$deps$end >= as.Date(start_date)) %>%
      distinct(locationID, .keep_all = TRUE)  # Keep only unique locationIDs and retain other columns
    

    # Determine which active_locations had no observations
    no_obs_locations <- active_locations %>% 
      dplyr::filter(!locationName %in% obs_filtered$locationName) 
    
    mapping_data$no_obs_deployments <- no_obs_locations
      
    # Clear warning
    explorer_map_warning_content(NULL)
    logger::log_info("server.R, explorer_map_data() calling create_map_markers()")
    prepared_markers <- create_map_markers(mapping_data)
    
    # Check for warnings and output them if present
    if (!is.null(prepared_markers$warnings) && length(prepared_markers$warnings) > 0) {
      logger::log_info("Warnings detected after create_map_markers() run from explorer_map_data()")
      # Concatenate all warnings into a single string
      concatenated_warnings <- paste("Warning: ", prepared_markers$warnings, collapse = "<br>")
      
      explorer_map_warning_content(concatenated_warnings)
    }
   # browser()
    # Finally, prepare obs_filtered  for the browse by filtering fields. Doing this last
    # because we needed lat/lng etc in mapping_data and no_obs_deployments needed deploymentID
    obs_filtered <- obs_filtered %>%
      select(locality, line, locationName, timestamp, count, `vernacularNames.eng`, scientificName, possible_duplicate, observationID) 
    
    
    return(list(
      observations = obs_filtered,       # For Observations by day data table browse
      obs_summary = obs_summary,
      prepared_markers = prepared_markers,
      active_locations = active_locations,
      no_obs_locations = no_obs_locations
      )
    )
  })
  
  
  observe({
    req(input$nav == "explorer_map")
    logger::log_info("server.R, observer req input$nav explorer_map, updating the explorer_map data table...")
    # Ensure explorer_map_data() is updated reactively
    species_data <- explorer_map_data()$observations
    
    # Setup the table output to react to changes in explorer_map_data()
    setup_table_output(output, 
                       table_id = "network_observations_browse", 
                       data = species_data,
                       table_type = "paged",
                       table_order = list(list(3, 'asc')))
  })
  
  output$explorer_map_obs_summary <- renderUI({
    
    title <- "<strong>OBSERVATIONS SUMMARY</strong>"
    
    # Header: formatted start and end dates
    start_date <- as.Date(primary_period$start_date())  
    end_date <- as.Date(primary_period$end_date())
    
    formatted_start_date <- paste0("<strong>", format(start_date, "%d %b %Y"), " (", format(start_date, "%a"), ")</strong>")
    formatted_end_date <- paste0("<strong>", format(end_date, "%d %b %Y"), " (", format(end_date, "%a"), ")</strong>")
    
    # Create a table for the date display using kable, with no border or extra styling
    date_table <- data.frame(
      Label = c("Start:", "End:"),
      Date = c(formatted_start_date, formatted_end_date)
    )
    
    header <- knitr::kable(date_table, format = "html", col.names = NULL, escape = FALSE)
    
    # Kable summary table
    data <- explorer_map_data()$obs_summary
    
    kable_table <- knitr::kable(data, format = "html", col.names = c("Species", "Count")) %>%
      kableExtra::kable_styling(
        full_width = FALSE,
        bootstrap_options = c("hover", "bordered", "responsive"),
        font_size = 12
      )
    
    # Footer: No observations count
    no_obs_locations_count <- nrow(explorer_map_data()$no_obs_locations)
    #footer <- paste0("<small>There were <strong>", no_obs_locations_count, "</strong> locations that contained no observations of any selected species.</small>")
   
    footer <- paste0("<div><small>Locations containing no observations of any selected species: <strong>", no_obs_locations_count, "</strong></small></div>")
    
    
    # Combine all parts into a single HTML output
    HTML(paste0(title, header, kable_table, footer))
  })
  

  
  
  explorer_map_markers_summary <- reactiveVal(NULL)
  
#  output$explorer_map_textoverlay <- renderUI({
#    message("output$playback_map_textoverlay rendered")
#    
#    start_date <- as.Date(primary_period$start_date())  
#    end_date <- as.Date(primary_period$end_date()) 
#    
#    # Format the dates
#    formatted_start_date <- format(start_date, "%A, %d %b %Y")
#    formatted_end_date <- format(end_date, "%A, %d %b %Y")
#    
#    # Extract the summary data
#    summary_data <- explorer_map_markers_summary()
#
#    # Convert the summary data to a formatted text
#    summary_text <- sapply(names(summary_data), function(species) {
#      paste(species, ": ", summary_data[[species]])
#    })
#    
#    # Combine the date and summary text
#    text_content <- paste0(
#      strong(formatted_start_date), " to ", strong(formatted_end_date),
#      "<br>", 
#      strong("Markers showing "),
#      paste(summary_text, collapse = " | ")
#    )
#    
#    tags$div(class = "map-overlay", HTML(text_content))
#  })
  
  # This is not being used currently.
  explorer_map_warning_content <- reactiveVal(NULL)
  
  output$explorer_map_textoverlay_warning <- renderUI({
    # Check if there is any content to display
    if (!is.null(explorer_map_warning_content())) {
      div(
        class = "map-overlay-warning",  # Check custom.css for this class
        HTML(explorer_map_warning_content())
      )
    }
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
  
  
  # Reactive values to track if a map is visible. 
  explorer_map_selected <- reactiveVal(FALSE)

  logger::log_debug("server.R, explorer_map_selected set to FALSE in main server function")
  
  
  # Updates visibility reactiveVal's based on which which menu is showing
  # Covers standard map features and autoplay features
  observeEvent(input$nav, {
    nav_item <- input$nav

    runjs(sprintf("gtag('config', '%s', {'page_path': '/%s'});", config$globals$ga_tag, nav_item))
    
    
    explorer_map_selected(FALSE)

    #message("observeEvent triggered on input$nav, resetting explorer_map_selected and density_map_selected to FALSE")

    
    if (nav_item  == "dashboard") {
      # Run latest images
      #message("observeEvent(input$nav, viewing dashboard page -- triggered image update")
      latest_images(get_latest_images())
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

    # Check if 'explorer_map' is the current main menu item
    if (nav_item == "explorer_map") {
        explorer_map_selected(TRUE)
        logger::log_info("observeEvent(input$nav, setting explorer_map_selected to TRUE")
        
        
        output$explorer_map <- renderLeaflet({
          logger::log_debug("server.R, output$explorer_map renderLeaflet")
          
          leaflet() %>%
            # addProviderTiles(providers$Esri.WorldImagery)
            addTiles()
        })
        
      } 

  })
  
  
  # The req() statement checks to ensure the reactiveVal explorer_map_selected() is true,but it also triggers reactivity 
  # by referencing the reactive function map_params(). This causes the map to redraw if the window resizes
  # The check on if (input$explorer_map_tabsetpanel == "Map") { before running update_map() allows markers to be updated on 
  # data selection changes from the Browse observations tab, but does not run update_map() unless you are viewing it
  # This observer still triggers update_map even if data has not changed, but I think that is fine and better than not triggering
  # at all even if data has changed while on a different tabsetpanel.
  observe({
    req(explorer_map_selected())
    
    if (input$nav == "explorer_map") {
      logger::log_debug("Main observer for explorer_map triggering")

      
      # Only actually update the map if we are on the Map tabsetPanel
      
      if (input$explorer_map_tabs == "explorer_map_map") {
        all_markers_data <- explorer_map_data()$prepared_markers
        active_locations <- explorer_map_data()$active_locations
        
        logger::log_debug("Viewing the Map tab, updating map..")
        res <- update_map(all_markers_data, "explorer_map", active_locations)
        explorer_map_markers_summary(res)
        
        # Check if 'enhance_map_details' switch is on
        if (!is.null(input$enhance_map_details) && !is.na(input$enhance_map_details) && input$enhance_map_details) {
          update_map_area("explorer_map", active_locations)
          # Optionally calculate distances or other spatial data
          # result <- calculate_distance_between_line_locations(active_locations, config$globals$min_distance_threshold)
        }
      }
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
    view_mode <- if (length(action_parts) > 1) action_parts[2] else NULL  # Extract mode if present
    
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
  
  
  # Handle viewing a sequence
  handle_view_sequence <- function(observation_id, view_mode) {
    #browser()
    observation_details <- create_observation_viewer_output(observation_id, "view_sequence|modal")

    if (!is.null(observation_details)) {
      image_output <- create_observation_images_ui(observation_details$sequence_media_info, 
                                                   observation_id,
                                                   context = view_mode)
      if (view_mode == "modal") {
        show_image_modal(observation_id, image_output$ui_elements)
        session$sendCustomMessage(type = "refreshCarousel", message = list(carouselId = image_output$carousel_id))
      } else if (view_mode == "pageview") {
        output$observation_images_pageview <- renderUI({
          image_output$ui_elements
        })
        updateNavbarPage(session, "main_menu", selected = "observations")
        updateTabsetPanel(session, "observations_tabsetpanel", selected = "Viewer")
      }
      
      if (!image_output$cache_hit) {
        update_image_cache(observation_details$sequence_media_info)
      }
      shinyjs::click("reset_button")
    }
  }
  
  
  
  
  # Handle editing a sequence
  handle_edit_sequence <- function(sequence_id) {
    sequence_details <- create_sequence_editor_output(sequence_id)
    
    if (!is.null(sequence_details)) {
      sequence_ui <- create_sequence_editor_ui(sequence_details, context = "pageview")
      output$sequence_editor_pageview <- renderUI({
        sequence_ui
      })
      updateNavbarPage(session, "main_menu", selected = "sequences")
      updateTabsetPanel(session, "sequences_tabsetpanel", selected = "Editor")
      shinyjs::click("reset_button")
    }
  }
  
  
  # Updated create_observation_viewer_output function
  create_observation_viewer_output <- function(observation_id = NULL, action_type) {
    browser()
    # Parse the action_type to extract the action and view_mode
    action_parts <- strsplit(action_type, "\\|")[[1]]
    action <- action_parts[1]      # "view_sequence" or "edit_sequence"
    view_mode <- action_parts[2]   # "modal" or "pageview" (only relevant for view_sequence)
    
    # Fetch the observation row
    obs_row <- core_data$obs %>%
      dplyr::filter(observationID == observation_id)
    
    if (!is.null(obs_row)) {
      media <- core_data$media
      
      # Filter media for the specified sequenceID
      media_filtered <- media %>% 
        dplyr::filter(sequenceID == obs_row$sequenceID)
      
      sequence_media_info <- get_sequence_media_urls(media_filtered)
      
      if (action == "view_sequence") {
        # Handle view_sequence actions
        if (view_mode == "modal") {
          # Render for modal
          output$observation_record_table_modal <- renderUI({
            setup_kable_output(
              table_id = "special_observation_viewer",
              data = obs_row,
              table_type = "short",
              column_spec = NULL,
              heading_level = NULL
            )
          })
        } else if (view_mode == "pageview") {
          # Render for pageview
          fields <- c("locality", "line", "locationName", "timestamp", "count", config$globals$species_name_type, "comments")
          selected_data <- filter_fields(obs_row, fields, species_group_definitions = NULL, species_fields = NULL)
          output_data <- format_fieldnames(selected_data)
          
          output$observation_record_table_pageview <- renderUI({
            HTML(
              kable(output_data, format = "html") %>%
                kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover", "condensed", "bordered"))
            )
          })
        }
      } else if (action == "edit_sequence") {
        # Handle edit_sequence actions (implement as needed)
        output$sequence_editor_table <- renderUI({
          # Example: Render a sequence editor UI (logic needs to be defined)
          setup_kable_output(
            table_id = "sequence_editor_viewer",
            data = media_filtered,
            table_type = "full",
            column_spec = NULL,
            heading_level = NULL
          )
        })
      }
      
      return(list(
        sequence_id = obs_row$sequenceID,
        sequence_media_info = sequence_media_info
      ))
    }
  }
  
  
  
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
                                                         context = "modal")
            
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
      tags$script(HTML('
      $(document).ready(function() {
        $("#slickSlider").slick({
          lazyLoad: "progressive", // or ondemand
          slidesToShow: 2,  // Display 3 images at a time
          slidesToScroll: 2,  // Scroll one image at a time
          infinite: true,  // Enable infinite looping
          dots: true,  // Show navigation dots
          arrows: false, // navigation arrows
          autoplay: true,  // Optional: Enable autoplay
          autoplaySpeed: 3000,  // Optional: Autoplay speed in milliseconds

          speed: 1000,  // Transition speed
          responsive: [  // Adjust settings for different screen sizes
            {
              breakpoint: 1024,  // Devices less than 1024px wide
              settings: {
                slidesToShow: 2,
                slidesToScroll: 1
              }
            },
            {
              breakpoint: 600,  // Devices less than 600px wide
              settings: {
                slidesToShow: 1,
                slidesToScroll: 1
              }
            }
          ]
        });
      });
    '))
    )
  })
  

  

  
  ########### DENSITY MAP FEATURE ###########
  
  logger::log_debug("server.R, calling mapping_module_server() for density_map_primary")
  
  density_map_primary <- NULL
  
  density_map_primary <- mapping_module_server(
    id = "density_map_primary",
    type = "density",
    obs = filtered_obs_primary,
    deps = filtered_deps_primary
  )
  
  logger::log_debug("server.R, calling mapping_module_server() for density_map_comparative")
  
  density_map_comparative <- NULL
  
  density_map_comparative <- mapping_module_server(
    id = "density_map_comparative",
    type = "density",
    obs = filtered_obs_comparative,
    deps = filtered_deps_comparative,
    species_override = density_map_primary$selected_species,
    localities_override = density_map_primary$selected_localities
    
  )
  
  # Tab switch observer
  observe({
    req(input$nav == "density_map", input$density_map_tabs == "primary")
    
    runjs(sprintf("gtag('event', 'tab_switch', {
      'event_category': 'sub_tab_navigation',
      'event_label': 'main_menu_%s_tab_switch',
      'value': '%s'
    });", input$nav, input$density_map_tabs))
    
    isolate({
      logger::log_debug("server.R, observer triggered density_map_primary$recenter_map()")
      density_map_primary$recenter_map()
    })
  })
  
  # Tab switch observer for comparative map
  observe({
    req(input$nav == "density_map", input$density_map_tabs == "comparative")
    
    runjs(sprintf("gtag('event', 'tab_switch', {
      'event_category': 'sub_tab_navigation',
      'event_label': 'main_menu_%s_tab_switch',
      'value': '%s'
    });", input$nav, input$density_map_tabs))
    
    isolate({
      logger::log_debug("server.R, observer triggered density_map_comparative$recenter_map()")
      density_map_comparative$recenter_map()
    })
  })
  

  # Used for the tab names
  output$primary_season_name <- renderText({
    primary_period$period_name()
  })
  
  output$comparative_season_name <- renderText({
    comparative_period$period_name()
  })
  
  
#  output$selected_dates_summary_reporting <- renderUI({
#    output_text <- primary_period$selected()$html_string
#    HTML(output_text)
#  })
  


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
    #    browser()
        report_pdf <- file.path(reports_cache_dir, gsub(" ", "-", paste0(period_name, "_deployment_report_", package_date_string, ".pdf")))
        convert_to_pdf(report_html, report_pdf)
        file.copy(report_pdf, file, overwrite = TRUE)
      } else {
        file.copy(report_html, file, overwrite = TRUE)
      }
    }
  )
  
  

  
} # server

