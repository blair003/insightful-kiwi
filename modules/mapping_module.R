# modules/mapping_module.R

mapping_module_ui <- function(id,
                              view = "map",
                              choices,
                              selected = NULL,
                              multiple = TRUE,
                              label = "Species selection:",
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
  } else if (view == "select_observation_map_options") { # New view for observation map specific options
    return(
      tagList(
        checkboxInput(
          inputId = ns("enhance_map_details"),
          label = "Show area calc"
        )
      )
    )
  } else if (view == "map") { # This is for the density map
    return(
      tagList(
        leafletOutput(ns("map_display"), height = map_height)
      )
    )
  } else if (view == "observation_map_layout") { # New view for the observation map page layout
    return(
      tagList(
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
              "The map is showing graphically the (unfiltered) results in this table,
              subject to limits imposed for species with high counts (see notes tab)."
            ),
            DT::dataTableOutput(ns("observation_data_table")),
            value = ns("data_tab")
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
                                  period_start_date = NULL, # Reactive: e.g. primary_period$start_date
                                  period_end_date = NULL    # Reactive: e.g. primary_period$end_date
) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    logger::log_debug(sprintf("mapping_module_server, %s moduleServer() running for type: %s", id, type))
    
    MAP_ID <- ns("map_display") 
    current_bounds <- reactiveVal(NULL) # Unified reactiveVal for map bounds
    
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
      shinyjs::runjs(sprintf(
        'setTimeout(function() {
           var mapWidget = HTMLWidgets.find("#%s");
           if (mapWidget) {
             var mapObj = mapWidget.getMap();
             if (mapObj) {
               mapObj.invalidateSize();
               console.log("invalidateSize() called on %s map: %s");
             } else { console.error("Leaflet map object not found for %s map: %s"); }
           } else { console.error("Map widget not found for %s map: %s"); }
         }, 100);', MAP_ID, type, MAP_ID, type, MAP_ID, type, MAP_ID
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
    
    # --- Unified Map Output ---
    # This single output will be used for both density and observation maps.
    # The content will be updated by type-specific observers.
    output$map_display <- renderLeaflet({
      logger::log_debug(sprintf("mapping_module_server [%s], %s renderLeaflet for unified map_display: %s", type, id, MAP_ID))
      leaflet() %>% addTiles()
    })
    outputOptions(output, "map_display", suspendWhenHidden = FALSE)
    
    
    # --- Density Map Specific Logic ---
    if (type == "density") {
      
      mapping_data_density <- reactive({
        req(obs(), deps(), current_selected_species(), current_selected_localities())
        species_dens <- tolower(unname(current_selected_species()))
        localities_dens <- current_selected_localities()
        
        logger::log_debug(sprintf(
          "mapping_module_server [density], %s mapping_data_density() for species: %s, localities: %s",
          id, paste(species_dens, collapse=", "), paste(localities_dens, collapse=", ")
        ))
        
        active_locations_dens <- deps() %>%
          dplyr::filter(locality %in% localities_dens) %>%
          dplyr::distinct(locationID, locality, .keep_all = TRUE)
        
        # Calculate and set the unified bounds
        new_map_bounds <- calculate_bounds_from_locations(active_locations_dens, id, "density")
        current_bounds(new_map_bounds) # Update the single reactiveVal
        
        obs_filtered_dens <- obs() %>%
          dplyr::filter(
            scientificName_lower %in% species_dens,
            locality %in% localities_dens
          )
        
        obs_summary_location_dens <- obs_filtered_dens %>%
          dplyr::group_by(locationID, locationName, longitude, latitude) %>%
          dplyr::summarise(count = sum(count), .groups = "drop")
        
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
        
        list(
          active_locations = active_locations_dens,
          obs_summary_location = obs_summary_location_dens,
          obs_summary_locality_with_total = dplyr::bind_rows(obs_summary_locality_dens, grand_total_dens)
        )
      })
      
      # Observer to update the content of the density map on the unified map display
      observe({
        req(mapping_data_density())
        logger::log_debug(sprintf("mapping_module_server [density], %s observer updating density map content on %s", id, MAP_ID))
        data_for_map <- mapping_data_density()
        
        # update_density_map is a function specific to rendering density data.
        # It now targets the unified MAP_ID.
        update_density_map(
          map_id = MAP_ID, 
          active_locations = data_for_map$active_locations,
          obs_summary_location = data_for_map$obs_summary_location,
          show_zero = TRUE # Assuming this is a param for update_density_map
        )
        # Apply fit bounds after updating map content, as in original logic for density
        apply_map_fit_bounds() 
      })
      
      output$obs_summary <- renderUI({ # Density-specific summary table
        req(mapping_data_density())
        data_summary <- mapping_data_density()$obs_summary_locality_with_total
        total_row_index <- nrow(data_summary)
        kable_table <- knitr::kable(data_summary, format = "html", escape = FALSE, col.names = c("Locality", "Count")) %>%
          kableExtra::kable_styling(
            bootstrap_options = c("hover", "condensed", "bordered"),
            full_width = FALSE, position = "center", font_size = 12
          ) %>%
          kableExtra::row_spec(total_row_index, bold = TRUE)
        HTML(kable_table)
      })
      
      # Observer for auto-recentering density map on tab switch
      # This assumes 'density_map_tabs' is an input in the parent UI controlling visibility
      sub_tab_id <- sub("^.*_", "", id) # e.g., "primary" or "comparative"
      observe({
        main_nav <- session$rootScope()$input$nav
        sub_tab  <- session$rootScope()$input$density_map_tabs # Input from parent UI
        
        req(main_nav == "density_map", sub_tab == sub_tab_id)
        
        shinyjs::runjs(sprintf( # GA event
          "gtag('event','tab_switch',{
             'event_category':'sub_tab_navigation',
             'event_label':'main_menu_%s_tab_switch',
             'value':'%s'
           });",
          main_nav, sub_tab
        ))
        
        logger::log_debug(sprintf(
          "mapping_module_server [density], %s auto-recenter due to tab switch to '%s' tab",
          id, sub_tab
        ))
        recenter_map_generic() # Use the generic recenter function
      })
      
      return(list(
        selected_species = current_selected_species,
        selected_localities = current_selected_localities,
        recenter_map = recenter_map_generic # Return the generic recenter function
      ))
      
      
      # --- Observation Map Specific Logic ---
    } else if (type == "observation") {
      on.exit({ # Specific to observation type, as in original
        print(paste0("DEBUG: The fully namespaced ID for observation_map_tabs is: '", ns("observation_map_tabs"), "'"))
      })
      
      observation_map_warning_content <- reactiveVal(NULL) # Specific to observation map
      
      observation_map_processed_data <- reactive({
        req(obs(), deps(), current_selected_species(), current_selected_localities(), period_start_date(), period_end_date())
        
        species_to_map <- tolower(unname(current_selected_species()))
        localities_to_map <- current_selected_localities()
        
        logger::log_debug(sprintf(
          "mapping_module_server [observation] ID: %s - observation_map_processed_data: Processing for species: [%s], localities: [%s]",
          id, paste(species_to_map, collapse=", "), paste(localities_to_map, collapse=", ")
        ))
        
        # Note: distinct criteria for observation map active locations
        active_locations_obsmap <- deps() %>%
          dplyr::filter(locality %in% localities_to_map) %>%
          dplyr::distinct(locationID, .keep_all = TRUE) 
        
        # Calculate and set the unified bounds
        new_map_bounds <- calculate_bounds_from_locations(active_locations_obsmap, id, "observation")
        current_bounds(new_map_bounds) # Update the single reactiveVal
        
        obs_filtered_obsmap <- obs() %>%
          dplyr::filter(tolower(scientificName) %in% species_to_map,
                        locality %in% localities_to_map)
        
        no_obs_locations_obsmap <- active_locations_obsmap %>%
          dplyr::filter(!locationName %in% obs_filtered_obsmap$locationName)
        
        marker_prep_input <- list(
          observations = obs_filtered_obsmap,
          no_obs_deployments = no_obs_locations_obsmap
        )
        
        # create_map_markers is assumed to be an external helper function
        prepared_markers_list <- create_map_markers(marker_prep_input) 
        
        if (!is.null(prepared_markers_list$warnings) && length(prepared_markers_list$warnings) > 0) {
          concatenated_warnings <- paste("Warning: ", prepared_markers_list$warnings, collapse = "<br>")
          observation_map_warning_content(concatenated_warnings)
        } else {
          observation_map_warning_content(NULL)
        }
        
        # config$globals$species_name_type is assumed to be defined elsewhere
        obs_summary_for_sidebar <- obs_filtered_obsmap %>%
          dplyr::group_by(!!sym(config$globals$species_name_type), species_class, species_rank) %>%
          dplyr::summarise(Count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
          dplyr::rename(Species = !!sym(config$globals$species_name_type)) %>%
          dplyr::arrange(species_rank) %>%
          dplyr::select(Species, Count)
        
        list(
          observations_for_table = obs_filtered_obsmap %>%
            dplyr::select(locality, line, locationName, timestamp, count, `vernacularNames.eng`, scientificName, possible_duplicate, observationID),
          obs_summary_for_sidebar = obs_summary_for_sidebar,
          prepared_markers = prepared_markers_list,
          active_locations = active_locations_obsmap,
          no_obs_locations_count = nrow(no_obs_locations_obsmap)
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
        logger::log_error(sprintf("VERY_BASIC_OBSERVER_ENTRY (ID: %s): This observer has been entered.", id)) # Original log
        
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
        update_map(all_markers_data, MAP_ID, active_locs) 
        
        # Recenter map after updating content, as in original logic for observation
        recenter_map_generic() 
        
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
      output$observation_data_table <- DT::renderDataTable({
        req(observation_map_processed_data())
        table_data <- observation_map_processed_data()$observations_for_table
        DT::datatable( table_data, escape = FALSE,
                       options = list( pageLength = 10, searching = TRUE, lengthChange = TRUE, order = list(list(3, 'asc')) ),
                       class = 'display', rownames = FALSE
        )
      })
      
      output$observation_map_sidebar_summary_content <- renderUI({
        req(observation_map_processed_data(), period_start_date(), period_end_date())
        data_summary <- observation_map_processed_data()
        title <- "<strong>OBSERVATIONS SUMMARY</strong>"
        start_d <- as.Date(period_start_date()); end_d <- as.Date(period_end_date())
        formatted_start_date <- paste0("<strong>", format(start_d, "%d %b %Y (%a)"), "</strong>")
        formatted_end_date <- paste0("<strong>", format(end_d, "%d %b %Y (%a)"), "</strong>")
        date_table_df <- data.frame(Label = c("Start:", "End:"), Date = c(formatted_start_date, formatted_end_date))
        header <- knitr::kable(date_table_df, format = "html", col.names = NULL, escape = FALSE) %>%
          kableExtra::kable_styling(bootstrap_options = c("condensed"), full_width = FALSE, font_size=11)
        
        
        kable_summary_table <- knitr::kable(data_summary$obs_summary_for_sidebar, format = "html", col.names = c("Species", "Count")) %>%
          kableExtra::kable_styling(full_width = FALSE, bootstrap_options = c("hover", "bordered", "responsive", "condensed"), font_size = 12)
        footer <- paste0("<div><small>Locations with no selected species: <strong>", data_summary$no_obs_locations_count, "</strong></small></div>")
        HTML(paste0(title, header, kable_summary_table, footer))
      })
      
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
                               show_zero = TRUE) {
  #browser()
  max_scale <- 1
  radius_range <- c(10, 50)

  # Clear the map
  proxy <- leafletProxy(map_id) %>%
    clearMarkers() %>%
    clearShapes() %>%
    clearControls()
  
  # Check if obs_summary_location has any rows
  if (nrow(obs_summary_location) > 0) {
    max_count <- max(obs_summary_location$count, na.rm = TRUE)
  } else {
    logger::log_debug(sprintf("mapping_module_server, %s update_density_map() has no results available", map_id))
    max_count <- NA
    
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
  
  obs_summary_location <- obs_summary_location %>%
    mutate(radius = ifelse(count > 0,
                           scales::rescale(count, to = radius_range, from = c(0, max_count)),
                           radius_range[1]))
  
  pal <- colorNumeric(palette = "inferno", domain = obs_summary_location$count)

  
  # Add markers for non-zero observations
  proxy %>% 
    addCircleMarkers(
      data = obs_summary_location %>% filter(count > 0),
      lng = ~longitude, lat = ~latitude,
      radius = ~radius * max_scale,
      fillColor = ~pal(count),
      fillOpacity = 0.8,
      stroke = FALSE,
      popup = ~paste(locationName, "<br>Count:", count)
    )
  
  # Add markers for zero observations if show_zero is TRUE
  if (show_zero) {
    proxy %>% 
      addMarkers(
        data = obs_summary_location %>% filter(count == 0),
        lng = ~longitude, lat = ~latitude,
        icon = zero_icon,
        popup = ~paste(locationName, "<br>Count: 0")
      )
  }
  
  # Add a legend if there are any non-zero counts
  if (!is.na(max_count) && max_count > 0) {
    proxy %>% 
      addLegend(
        "bottomright", 
        pal = pal, 
        values = obs_summary_location$count, 
        title = "Number of individuals", 
        labFormat = labelFormat(), 
        opacity = 1
      )
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
          <a href='javascript:void(0);' class='observation-link' data-observationid='%s' data-action-type='modal'>View Images</a>
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




get_confidence_text <- function(confidence) {
  if (is.na(confidence)) return("full")
  if (confidence == 1) return("full (validated)")
  if (confidence > 0.8) return("high")
  return("uncertain")
}



update_map <- function(all_marker_data_with_warnings, map_id, active_locations) {
  
  proxy <- leafletProxy(map_id) %>% 
    clearMarkers() %>% 
    clearShapes() # Also clear shapes if update_map_area adds them
  
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
  
  # Fit Bounds Logic
  logger::log_debug(sprintf("update_map() for map_id: %s - About to call fitBounds. Number of active_locations: %d", 
                            map_id, if(!is.null(active_locations)) nrow(active_locations) else 0))
  
  if (!is.null(active_locations) && nrow(active_locations) > 0 && 
      all(c("longitude", "latitude") %in% names(active_locations)) &&
      !any(is.na(active_locations$longitude)) && !any(is.na(active_locations$latitude))) {
    
    min_lng_val <- min(active_locations$longitude, na.rm = TRUE)
    min_lat_val <- min(active_locations$latitude, na.rm = TRUE)
    max_lng_val <- max(active_locations$longitude, na.rm = TRUE)
    max_lat_val <- max(active_locations$latitude, na.rm = TRUE)
    
    logger::log_debug(sprintf("update_map() for map_id: %s - Valid active_locations for fitBounds. MinLng: %f, MinLat: %f, MaxLng: %f, MaxLat: %f",
                              map_id, min_lng_val, min_lat_val, max_lng_val, max_lat_val))
    proxy %>%
      fitBounds(
        lng1 = min_lng_val, lat1 = min_lat_val,
        lng2 = max_lng_val, lat2 = max_lat_val
      )
  } else {
    logger::log_warn(sprintf("update_map() for map_id: %s - Skipping fitBounds due to empty or invalid active_locations.", map_id))
    # Optionally, you could set a default wide view if active_locations is bad,
    # but world view is the typical leaflet default.
    # proxy %>% setView(lng = 0, lat = 0, zoom = 2) 
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



