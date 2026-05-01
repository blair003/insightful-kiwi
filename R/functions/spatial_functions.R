# Used by generate_reporting_data_for_period to calculate network footprint
# Uses the 'chull' function on point coordinates to find the convex hull, identifying the outermost points
# within each locality. It determines the smallest convex shape that encompasses all points, providing a more
# precise representation of the area, particularly for irregularly spread points. By accounting for
# Earth's curvature, it ensures accuracy for expansive or polar regions. This method offers a clearer insight into
# the spatial distribution or extent of points across a locality, without limiting the area to a rectangular shape.

calculate_area_by_locality <- function(deployments) {
  # Check for NULL or empty dataframe
  if (is.null(deployments) || nrow(deployments) == 0) {
    return(NULL)
  }

  # Convert data to sf object
  deployments_sf <- deployments %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Calculate area for each locality without converting to km^2
  area_data <- deployments_sf %>%
    group_by(locality) %>%
    summarise(geometry = st_combine(geometry)) %>%
    mutate(geometry = st_convex_hull(geometry)) %>%
#    mutate(area_m2 = st_area(geometry)) %>% # Keep area in square meters
#    select(locality, area_m2) # Rename column to reflect unit
    mutate(area_ha = as.numeric(st_area(geometry) / 10000)) %>% # Convert m² to hectares
    select(locality, area_ha) # Rename column to reflect unit

  return(area_data)
}






# Returns a tibbble of locality, locationName1, locationName2, and distance where distance is
# the distance between the two locations. Output includes results where the distance is
# less than min_distance_threshold, which can be 0 if you want all marker distances
calculate_distance_between_line_locations <- function(deployments, min_distance_threshold) {

  distances <- deployments %>%
    group_by(locality) %>%
    do({
      data <- .
      pairs <- expand.grid(index1 = 1:nrow(data), index2 = 1:nrow(data))
      pairs <- subset(pairs, index1 < index2 & data$line[index1] != data$line[index2])

      distances <- mapply(function(x, y) distm(c(data$longitude[x], data$latitude[x]),
                                               c(data$longitude[y], data$latitude[y]),
                                               fun = distHaversine),
                          pairs$index1, pairs$index2)

      pairs$distance <- distances
      # Filter within the threshold
      pairs <- pairs[which(pairs$distance <= min_distance_threshold),]

      # Proceed only if pairs exist after filtering
      if(nrow(pairs) > 0) {
        pairs$locality <- rep(data$locality[1], nrow(pairs))
        pairs$locationName1 <- data$locationName[pairs$index1]
        pairs$locationName2 <- data$locationName[pairs$index2]

        # Select and rename columns as necessary, avoiding explicit return
        pairs <- pairs %>%
          select(locality, locationName1, locationName2, distance)
      } else {
        # Create an empty dataframe with the same structure
        pairs <- tibble(locality = character(), locationName1 = character(), locationName2 = character(), distance = numeric())
      }

      pairs # Implicitly return the resulting dataframe/tibble
    }) %>%
    ungroup()

  return(distances)
}







create_density_map <- function(obs, deps, species, show_zero = TRUE) {

  max_scale <- 1
  radius_range <- c(10, 50)

  # Filter deployments and observations based on the period and species
  active_locations <- deps %>%
    distinct(locationID, locality, .keep_all = TRUE)

  obs_filtered <- obs %>%
    dplyr::filter(
      scientificName_lower == tolower(species)
    )

  obs_summary_location <- obs_filtered %>%
    group_by(locationID, locationName, longitude, latitude) %>%
    summarise(count = sum(count), .groups = "drop")

  if (nrow(obs_summary_location) > 0) {
    max_count <- max(obs_summary_location$count, na.rm = TRUE)
  } else {
    #logger::log_info("density_map_module_server, update_density_map() has no results of selected species and daterange")
    max_count <- NA
  }

  if (show_zero) {
    zero_entries <- active_locations %>%
      anti_join(obs_summary_location, by = "locationID") %>%
      mutate(count = 0)

    obs_summary_location <- bind_rows(obs_summary_location, zero_entries)
  }

  zero_icon <- get_species_icon(species = "none")

  obs_summary_location <- obs_summary_location %>%
    mutate(radius = ifelse(count > 0,
                           scales::rescale(count, to = radius_range, from = c(0, max_count)),
                           radius_range[1]))

  pal <- colorNumeric(palette = "inferno", domain = obs_summary_location$count)

  # Start with the base Leaflet map
  leaflet_map <- leaflet() %>%
    addTiles(options = tileOptions(crossOrigin = TRUE)) %>%
    #addProviderTiles(providers$Esri.WorldImagery) %>%
    addCircleMarkers(
      data = obs_summary_location %>% filter(count > 0),
      lng = ~longitude, lat = ~latitude,
      radius = ~radius * max_scale,
      fillColor = ~pal(count),
      fillOpacity = 0.8,
      stroke = FALSE
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = obs_summary_location$count,
      title = "Number of individuals",
      labFormat = labelFormat(),
      opacity = 1
    )

  # Conditionally add markers for count == 0 if show_zero is TRUE
  if (show_zero) {
    leaflet_map <- leaflet_map %>%
      addMarkers(
        data = obs_summary_location %>% filter(count == 0),
        lng = ~longitude, lat = ~latitude,
        icon = zero_icon,
        popup = ~paste(locationName, "<br>Count: 0")
      )
  }

  # Return the final Leaflet map
  leaflet_map

}
