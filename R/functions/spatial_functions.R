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






# Takes known RAI values at camera/site coordinates, draws a buffered boundary
# around those sites, lays a grid over that boundary, estimates RAI at each grid
# cell using inverse-distance weighting, and returns polygons ready to plot on a Shiny map.
create_idw_prediction_surface <- function(location_values,
                                          value_col = "rai",
                                          group_col = "locality",
                                          min_points = 3,
                                          idp = 2,
                                          grid_n = 45,
                                          max_cells = 1800,
                                          buffer_m = NULL,
                                          output_col = "predicted_rai") {
  required_cols <- c("longitude", "latitude", value_col)
  if (is.null(location_values) ||
      nrow(location_values) == 0 ||
      !all(required_cols %in% names(location_values))) {
    return(NULL)
  }

  create_surface_for_group <- function(group_data, group_name) {
    values <- suppressWarnings(as.numeric(group_data[[value_col]]))
    usable_locations <- group_data %>%
      dplyr::mutate(.surface_value = values) %>%
      dplyr::filter(
        is.finite(.data$longitude),
        is.finite(.data$latitude),
        is.finite(.data$.surface_value)
      )

    if (nrow(usable_locations) < min_points) {
      return(NULL)
    }

    points_sf <- sf::st_as_sf(
      usable_locations,
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    ) %>%
      sf::st_transform(3857)

    point_coords <- sf::st_coordinates(points_sf)
    distinct_point_count <- nrow(unique(round(point_coords, digits = 2)))
    if (distinct_point_count < min_points) {
      return(NULL)
    }

    distance_matrix <- as.matrix(stats::dist(point_coords))
    distance_matrix[distance_matrix == 0] <- NA_real_
    nearest_distance <- suppressWarnings(apply(distance_matrix, 1, min, na.rm = TRUE))
    nearest_distance <- nearest_distance[is.finite(nearest_distance)]
    inferred_buffer_m <- if (length(nearest_distance) > 0) {
      max(stats::median(nearest_distance, na.rm = TRUE) * 0.75, 50)
    } else {
      100
    }
    footprint_buffer_m <- if (is.null(buffer_m)) inferred_buffer_m else buffer_m

    footprint <- points_sf %>%
      sf::st_geometry() %>%
      sf::st_union() %>%
      sf::st_convex_hull() %>%
      sf::st_buffer(dist = footprint_buffer_m)

    if (length(footprint) == 0 || any(sf::st_is_empty(footprint))) {
      return(NULL)
    }

    footprint_bbox <- sf::st_bbox(footprint)
    width_m <- as.numeric(footprint_bbox[["xmax"]] - footprint_bbox[["xmin"]])
    height_m <- as.numeric(footprint_bbox[["ymax"]] - footprint_bbox[["ymin"]])
    extent_m <- max(width_m, height_m)
    if (!is.finite(extent_m) || extent_m <= 0) {
      return(NULL)
    }

    cell_size_m <- max(extent_m / grid_n, 25)
    estimated_cells <- (width_m / cell_size_m) * (height_m / cell_size_m)
    if (is.finite(estimated_cells) && estimated_cells > max_cells) {
      cell_size_m <- cell_size_m * sqrt(estimated_cells / max_cells)
    }

    grid <- sf::st_make_grid(
      footprint,
      cellsize = cell_size_m,
      what = "polygons",
      square = TRUE
    )
    if (length(grid) == 0) {
      return(NULL)
    }

    grid_sf <- sf::st_sf(
      locality = as.character(group_name),
      geometry = grid
    )
    grid_centres <- suppressWarnings(sf::st_centroid(grid_sf))
    inside_footprint <- lengths(sf::st_within(grid_centres, footprint)) > 0
    grid_sf <- grid_sf[inside_footprint, ]
    grid_centres <- grid_centres[inside_footprint, ]

    if (nrow(grid_sf) == 0) {
      return(NULL)
    }

    centre_coords <- sf::st_coordinates(grid_centres)
    point_values <- points_sf$.surface_value
    predictions <- vapply(seq_len(nrow(centre_coords)), function(i) {
      distances <- sqrt((point_coords[, 1] - centre_coords[i, 1])^2 +
                          (point_coords[, 2] - centre_coords[i, 2])^2)
      coincident <- distances == 0
      if (any(coincident)) {
        return(mean(point_values[coincident], na.rm = TRUE))
      }

      weights <- 1 / (distances ^ idp)
      sum(weights * point_values, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    }, numeric(1))

    grid_sf[[output_col]] <- predictions
    grid_sf <- grid_sf %>%
      dplyr::filter(is.finite(.data[[output_col]])) %>%
      sf::st_transform(4326)

    if (nrow(grid_sf) == 0) {
      return(NULL)
    }

    grid_sf[, c("locality", output_col, "geometry")]
  }

  grouped_locations <- if (!is.null(group_col) && group_col %in% names(location_values)) {
    split(location_values, location_values[[group_col]], drop = TRUE)
  } else {
    list("Selected area" = location_values)
  }

  surfaces <- Filter(
    Negate(is.null),
    Map(create_surface_for_group, grouped_locations, names(grouped_locations))
  )

  if (length(surfaces) == 0) {
    return(NULL)
  }

  do.call(rbind, surfaces)
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
