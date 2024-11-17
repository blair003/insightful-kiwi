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

  logger::log_debug("update_map() running for %s, markers cleared via leafletProxy with clearMarkers()", map_id)
  
  proxy <- leafletProxy(map_id) %>% 
    clearMarkers() %>% 
    clearShapes()
  
  all_marker_data <- all_marker_data_with_warnings$markers
  
  output <- list() # Start a blank list for the map we are working with
  # Iterate over all species
  for (species_data in all_marker_data) {
    species <- species_data$species  # Assuming 'species_group' key exists in each sub-list
    markers <- species_data$markers  # Extract list of markers for the species
    
    # Check if markers are not NULL and there are markers to add
    if (!is.null(markers) && length(markers) > 0) {
      logger::log_info(sprintf("update_map() is using leafletProxy() to add %s markers to %s for %s", 
                               length(markers), map_id, species))
      
      # Create a list so we can return it and use the data
      output[[species]] <- length(markers)
      # Iterate over markers for the current species
      for (marker_data in markers) {
        # Add each marker to the map
        proxy %>% addMarkers(
          lng = marker_data$lng,
          lat = marker_data$lat,
          icon = marker_data$icon,
          popup = marker_data$popup_content,
          options = markerOptions(zIndexOffset = marker_data$zIndexOffset)
        )
      }
    }
  }
  
  proxy %>%
    fitBounds(
      lng1 = min(active_locations$longitude), lat1 = min(active_locations$latitude),
      lng2 = max(active_locations$longitude), lat2 = max(active_locations$latitude)
    )
  
  return (output)
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


# Calculates midpoint in the extremes of lat and lng
# Calculates an approximation of the area and estimated zoom level based on 
# the area, window height and window width

calc_map_params <- function(deployments, window_width, window_height) {
 
  # Check for NULL or empty dataframe
  if (is.null(deployments) || nrow(deployments) == 0) {
    return(NULL)
  }
  
  # Calculate midpoints for longitude and latitude, used to center the map
  midpoint_longitude <- (max(deployments$longitude, na.rm = TRUE) + min(deployments$longitude, na.rm = TRUE)) / 2
  midpoint_latitude <- (max(deployments$latitude, na.rm = TRUE) + min(deployments$latitude, na.rm = TRUE)) / 2
  
  # Estimate the range (difference between max and min)
  longitude_range <- max(deployments$longitude, na.rm = TRUE) - min(deployments$longitude, na.rm = TRUE)
  latitude_range <- max(deployments$latitude, na.rm = TRUE) - min(deployments$latitude, na.rm = TRUE)
  
  # Heuristic to estimate zoom level based on the range
  # Adjust this formula as needed for your specific map behavior
  range_factor <- max(longitude_range, latitude_range)
  # Range factor examples
  # Ohope 0.024357
  # KP only 0.016013
  # MK+KP 0.03811999
  # ALL 0.046571000

  
  # Calculate zoom level
  # Based on testing limited range factors (from WKT data) and screen heights up to 1117 pixels (simulating a 16:10 screen)
  # Base zoom levels for different range factors based on window height of 1100 (since it stabilizes at and beyond this point)
  # Probably need to refactor when we have more data for different range_factors
  
  # Adjust for tablet mode by checking if height is greater than width
  effective_height <- ifelse(window_height > window_width, window_width, window_height)
  
  base_zoom_levels <- c(16, 15, 14, 14) # Corresponding to the provided range factor thresholds
  
  # Thresholds for range factors
  range_thresholds <- c(0.02, 0.025, 0.05, Inf)
  
  # Find the index for the appropriate base zoom level based on range factor
  zoom_index <- which.max(range_factor < range_thresholds) 
  
  # This adjustment can be fine-tuned as per more detailed analysis or requirements
  height_adjustment <- min(1, (1300 - effective_height) / 300) # Simplified adjustment: Decreases as height increases
  
  # Calculate final estimated zoom level
  estimated_zoom_level <- base_zoom_levels[zoom_index] - height_adjustment
  
  # Can we only return updated avg_longitude an avg_latitude if its changed? Why  not. Fix no observations first
  message(sprintf("calc_map_params() lrange factor: %s, screen widthxheight: %s x %s, base zoom: %s, height adj: %s, estimated zoom: %s", round(range_factor,4), window_width, window_height, base_zoom_levels[zoom_index], round(height_adjustment,2), round(estimated_zoom_level,2)))
  return(list(
    avg_latitude = midpoint_latitude,
    avg_longitude = midpoint_longitude,
    estimated_zoom = estimated_zoom_level
  ))
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

