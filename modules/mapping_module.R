mapping_module_ui <- function(id, 
                                  view = "map",
                                  choices, 
                                  selected = NULL,
                                  multiple = TRUE,
                                  label = "Species selection:") {
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
        
        tags$small("Note: Selected species will be combined, not shown separately."),
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
  } else if (view == "map") {
    return(
      tagList(
        # Output placeholder for the map
        leafletOutput(ns("leaflet_map"), height = config$globals$leaflet_height)
      )
    )
  } else if (view == "summary") {
    return(
      tagList(
        # Output placeholder for the kableExtra styled table
        uiOutput(ns("obs_summary"))
      )
    )
  }
}


mapping_module_server <- function(id,
                                  type = "density",
                                  obs,
                                  deps, 
                                  species_override = NULL,
                                  localities_override = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  #  browser()
    logger::log_debug(sprintf("mapping_module_server, %s moduleServer() running", id))

    # Use the passed-in species input or get it from the picker
    selected_species <- reactive({
      if (is.null(species_override)) {
        logger::log_debug(sprintf("mapping_module_server, %s species_override not provided, using value from input$selected_species", id))
        #req(input$selected_species)  # Ensure species is selected
        as.character(input$selected_species)
      } else {
        logger::log_debug(sprintf("mapping_module_server, %s species_override provided directly", id))
        species_override()  # Use the reactive value correctly
      }
    })
    
    selected_localities <- reactive({
      if (is.null(localities_override)) {
        logger::log_debug(sprintf("mapping_module_server, %s localities_override not provided, using value from input$selected_localities", id))
      #  req(input$selected_localities)  # Ensure localities are selected
        as.character(input$selected_localities)
      } else {
        logger::log_debug(sprintf("mapping_module_server, %s localities_override provided directly", id))
        localities_override()  # Use the reactive value correctly
      }
    })
    
    
    # Google Analytics Tracking
    observeEvent(input$selected_species, {
      selected_items <- selected_species()
      num_selected <- length(selected_items)
      selected_items_str <- paste(selected_items, collapse = ", ")
      
      runjs(sprintf("gtag('event', 'select', {
        'event_category': 'species_selection',
        'event_label': 'mapping_species_selection',
        'value': %d,
        'items_selected': '%s'
      });", num_selected, selected_items_str))
    })
    
    # Reactive to store the bounds (min/max lat/lng)
    bounds <- reactiveVal(NULL)
    
    mapping_data <- reactive({
      #browser()

      req(obs(), deps())
      
      species <- tolower(unname(selected_species()))
      localities <- selected_localities()
      
      logger::log_debug(sprintf(
        "mapping_module_server, %s mapping_data() reactive triggered for species: %s and localities: %s.", 
        id, paste(species, collapse = ", "), paste(localities, collapse = ", ")
      ))
    
      # Ensure species is treated as a vector even if a single species is selected
      species <- if (is.null(species)) character(0) else as.character(species)
      localities <- if (is.null(localities)) character(0) else as.character(localities)

      
      # Filter for deployments that were active at any point during the provided start and end dates
      active_locations <- deps() %>%
        dplyr::filter(locality %in% localities)  %>% # Dont report on unselected localities
        distinct(locationID, locality, .keep_all = TRUE)  # Keep only unique locationIDs and retain other columns

      # Calculate bounds (min/max lat/lng)
      if (nrow(active_locations) > 0) {
        min_lng <- min(active_locations$longitude)
        max_lng <- max(active_locations$longitude)
        min_lat <- min(active_locations$latitude)
        max_lat <- max(active_locations$latitude)
        bounds(list(min_lng = min_lng, max_lng = max_lng, min_lat = min_lat, max_lat = max_lat))
      }


      # Filter observations by selected species and localities
      obs_filtered <- obs() %>%
        filter(
          scientificName_lower %in% species,
          locality %in% localities
        )
      
      if (nrow(obs_filtered) == 0) {
        logger::log_warn(sprintf("mapping_module_server, %s obs_filtered yields no rows.", id))
      }
      
      obs_summary_location <- obs_filtered %>%
        group_by(locationID, locationName, longitude, latitude) %>%
        summarise(count = sum(count), .groups = "drop")

      zero_locations <- active_locations %>%
        anti_join(obs_summary_location, by = "locationID") %>%
        mutate(count = 0)
      
      # Summary by locality, even if there are no observations (using left join)
      obs_summary_locality <- active_locations %>%
        dplyr::select(locality) %>%  # Only keep locality for the join
        dplyr::distinct() %>%
        left_join(
          obs_filtered %>%
            group_by(locality) %>%
            summarise(count = sum(count), .groups = "drop"),
          by = "locality"
        ) %>%
        mutate(count = ifelse(is.na(count), 0, count)) %>%  # Replace NA counts with 0
        arrange(locality)  # Sort alphabetically by locality

      grand_total <- obs_summary_locality %>%
        summarise(count = sum(count)) %>%
        mutate(locality = "Grand Total")

      obs_summary_locality_with_total <- bind_rows(obs_summary_locality, grand_total)
      #browser()
      return(list(
        active_locations = active_locations,
        zero_locations = zero_locations,
        obs_filtered = obs_filtered,
        obs_summary_location = obs_summary_location,
        obs_summary_locality_with_total = obs_summary_locality_with_total
      ))
      
    })
    
    output$leaflet_map <- renderLeaflet({
      logger::log_debug(sprintf("mapping_module_server, %s renderLeaflet output$leaflet_map", id))
        leaflet() %>%
        addTiles()
    })
    
    # Important! So that the hidden tab updates as well
    outputOptions(output, "leaflet_map", suspendWhenHidden = FALSE)
    
    # Use leafletProxy to update the map dynamically whenever data changes
    # I find leafletProxy less harsh than re-rendering the entire map.
    observe({
      req(mapping_data())
      logger::log_debug(sprintf("mapping_module_server, %s observe triggered update_density_map()", id))
      
      mapping_data <- mapping_data()
      #browser()
      # Call the update_density_map function with leafletProxy
      update_density_map(
        map_id = ns("leaflet_map"),
        active_locations = mapping_data$active_locations,
        obs_summary_location = mapping_data$obs_summary_location,
        show_zero = TRUE
      )
      
      # Separated as we need to call it in multiple circumstances, not just with update_leaflet_map
      apply_fit_bounds()
      
    })

    # Render the obs_summary using tagList and tags functions
    output$obs_summary <- renderUI({
      req(mapping_data())

      data <- mapping_data()$obs_summary_locality_with_total
      
      total_row_index <- nrow(data)
      
      kable_table <- knitr::kable(data, format = "html", escape = FALSE, col.names = c("Locality", "Count")) %>%
        kableExtra::kable_styling(
          bootstrap_options = c("hover", "condensed", "bordered"), 
          full_width = FALSE, 
          position = "center",
          font_size = 12) %>%
        kableExtra::row_spec(total_row_index, bold = TRUE)  # Make the last row bold
    
      HTML(kable_table)

    })
    
    # Delay could be 50-150ms? Maybe depends on device, 100ms is common
    invalidate_size <- function() {
      logger::log_debug(sprintf("mapping_module_server, %s calling invalidateSize", id))
      shinyjs::runjs(sprintf(
        'setTimeout(function() { 
          var mapObj = HTMLWidgets.find("#%s").getMap(); 
          if (mapObj) { 
            mapObj.invalidateSize(); 
            console.log("invalidateSize() called on %s"); 
          } else { 
            console.error("Map not found for #%s"); 
          }
        }, 90);', ns("leaflet_map"), ns("leaflet_map"), ns("leaflet_map"))
      )
    }
    
    
    # Separated this out from update_density_map because in some fringe cases where map was
    # in a tabset, fitBounds was being applied too early, resulting in worldmap view
    
    apply_fit_bounds <- function() {
      logger::log_debug(sprintf("mapping_module_server, %s calling fitBounds()", id))
      bounds_val <- bounds()
      if (!is.null(bounds_val)) {
        leafletProxy(ns("leaflet_map")) %>%
          fitBounds(
            lng1 = bounds_val$min_lng, lat1 = bounds_val$min_lat,
            lng2 = bounds_val$max_lng, lat2 = bounds_val$max_lat
          )
      } else {
        logger::log_debug(sprintf("mapping_module_server, %s no bounds available for fitBounds", id))
      }
    }
    
    
    # Combined function to invalidateSize and then apply fitBounds after a delay
    recenter_map <- function() {
      invalidate_size()  # First, invalidate the map size
      
      # Use future to introduce a non-blocking delay before applying fitBounds
      future({
        Sys.sleep(0.1)  # 100ms delay (can adjust as needed)
      }) %...>% { 
        # Apply fitBounds after the delay
        apply_fit_bounds()
      } %...!% { err ->
          # Error handling
          logger::log_error(sprintf("mapping_module_server, %s error during fitBounds: %s", id, err$message))
          
      }
    }
    
    
    # Return both the selected_species and recenter_map func
    return(list(
      selected_species = selected_species,
      selected_localities = selected_localities,
      recenter_map = recenter_map
    ))
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



