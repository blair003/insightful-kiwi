reporting_data_module_ui <- function(id, label) {
  ns <- NS(id)
  
}

reporting_data_module_server <- function(id, obs, deps) {
  moduleServer(id, function(input, output, session) {
   logger::log_debug(sprintf("reporting_data_module_server() moduleServer running for %s", id))

    data_error <- reactiveVal(NULL) 
      
    reporting_data <- reactive({
      req(obs(), deps())

      logger::log_info(sprintf("reporting_data_module_server, reporting_data() reactive triggered, %s obs, %s deps.",
                      nrow(obs), nrow(deps)))

      tryCatch({
        result <- generate_summary_reporting(obs(), deps())

        if (is.null(result)) {
          data_error("No data available for the selected date range.")
          return(NULL)
        } else {
          data_error(NULL)  # Reset error message if data is available
          result
        }
      }, error = function(e) {
        # Handle any errors that occur during data preparation
        custom_message <- paste("An error occurred while preparing reporting_data:", e$message)
        data_error(custom_message)  # Set the custom error message
        NULL
      })
    })

    # Return the reactive expression
    return(list(
      reporting_data = reporting_data
    ))
  })
}



# Generates all reporting based on data in refined_data()
generate_summary_reporting <- function (obs, deps) {
  #message("reporting_data_module_server, generate_summary_reporting() function called..")
  
  spp_summary <- generate_spp_summary(obs, deps, config$globals$rai_norm_hours)

  summary_data <- generate_summary_data(obs, deps)

  camera_network_overview <- generate_camera_network_overview(deps)
 # browser()
  return(list(
    camera_network_overview = camera_network_overview,
    summary_data = summary_data,
    spp_summary = spp_summary
    )
  )
}



  
generate_summary_data <- function(obs = NULL, deps = NULL) {
 # browser()
  # Get details once at locationName level, we can then aggregate down
  deployment_data <- deps %>%
    group_by(locationName) %>%
    summarise(
      camera_hours = sum(camera_hours, na.rm = TRUE), 
      blank_detections_count = sum(blank_detections_count, na.rm = TRUE),
      unknown_detections_count = sum(unknown_detections_count, na.rm = TRUE),
      camera_rest_interval = first(cameraInterval), 
      .groups = "drop")

  # Select only fields we need from obs for this function. My understanding is doing it here is optimal
  obs_details <- obs %>%
    select(locality, line, locationName, deploymentID, scientificName, count, possible_duplicate, species_class)
  
  # Helper function to calculate the counts by species class, and unique_species. We want to recalculate these for 
  # each view rather than aggregate to avoid hard coding so that changes to species class in config don't break the code
  calculate_species_class_counts <- function(obs_details, group_vars = NULL) {

    # Ensure group_vars is either NULL or a character vector
    if (!is.null(group_vars) && !is.character(group_vars)) {
      stop("group_vars must be NULL or a character vector.")
    }
    
    # We want the total for the entire dataset (aggregate row)
    if (is.null(group_vars)) {
      # Mutate locality so that all obs_details have the same locality
      obs_details <- obs_details %>%
        mutate(locality = "Aggregate")
      
      group_vars = c("locality")
    }

    obs_grouped <- obs_details %>% 
      group_by(across(all_of(group_vars)))
    
    # Calculate unique species within each grouping
    unique_species_counts <- obs_grouped %>%
      summarise(unique_species_count = n_distinct(scientificName), .groups = "drop")
    
    # Group for pivot_wider, include species_class in grouping
    obs_details <- obs_details %>% 
      group_by(across(all_of(group_vars)), species_class)

    # Sum counts and pivot for species_class counts
    species_class_counts <- obs_details %>%
      summarise(total_individuals_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = species_class,
        values_from = total_individuals_count,
        values_fill = list(total_individuals_count = 0),
        names_prefix = "individuals_count_"
      )
    
    # Combine unique_species_counts with species_class_counts
    # Both tibbles have the same grouping keys so they can be joined correctly.
    final_result <- left_join(unique_species_counts, species_class_counts, by = group_vars)
    
    return(final_result)
  } # end of helper function
  
  
  ### LOCATION VIEW ###
  
  # Caluculate species_class counts for the location view
  species_class_counts <- calculate_species_class_counts(obs_details, c("locationName"))
  
  location_summary <- obs_details %>%
    group_by(locationName) %>%
    summarise(
      locality = first(locality), 
      line = first(line), 
      deployments = n_distinct(deploymentID),
      animal_detections = n(), # Total number of records in obs_details
      individuals_count = sum(count, na.rm = TRUE),
      possible_duplicates_count = sum(ifelse(possible_duplicate, count, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
      # Join first to get camera_hours from deployment_data
      left_join(deployment_data, by = "locationName") %>%
      left_join(species_class_counts, by = "locationName") %>%
      mutate(
        mean_detection_interval = ifelse(animal_detections > 0, camera_hours / animal_detections, NA),
        possible_duplicates_percentage = (possible_duplicates_count / individuals_count) * 100,
        blank_detections_percentage = (blank_detections_count / (animal_detections + blank_detections_count)) * 100
      )

  ### LINE VIEW ###
  species_class_counts <- calculate_species_class_counts(obs_details, c("locality", "line"))

  line_summary <- location_summary %>%
    group_by(locality, line) %>%
    summarise(
      locations = n(),
      deployments = sum(deployments, na.rm = TRUE),
      animal_detections = sum(animal_detections, na.rm = TRUE),
      individuals_count = sum(individuals_count, na.rm = TRUE),
      blank_detections_count = sum(blank_detections_count, na.rm = TRUE),
      unknown_detections_count = sum(unknown_detections_count, na.rm = TRUE),
      possible_duplicates_count = sum(possible_duplicates_count, na.rm = TRUE),
      camera_hours = sum(camera_hours, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      mean_detection_interval = ifelse(animal_detections > 0, camera_hours / animal_detections, NA),
      possible_duplicates_percentage = (possible_duplicates_count / individuals_count) * 100,
      blank_detections_percentage = (blank_detections_count / (animal_detections + blank_detections_count)) * 100
    ) %>%
      left_join(species_class_counts, by = c("locality", "line"))

 
  ### LOCALITY VIEW ### 
  species_class_counts <- calculate_species_class_counts(obs_details, c("locality"))
  
  locality_summary <- line_summary %>%
    group_by(locality) %>%
    summarise(
      lines = n_distinct(line, na.rm = TRUE),
      locations = sum(locations, na.rm = TRUE),
      deployments = sum(deployments, na.rm = TRUE),
      animal_detections = sum(animal_detections, na.rm = TRUE),
      individuals_count = sum(individuals_count, na.rm = TRUE),
      blank_detections_count = sum(blank_detections_count, na.rm = TRUE),
      unknown_detections_count = sum(unknown_detections_count, na.rm = TRUE),
      possible_duplicates_count = sum(possible_duplicates_count, na.rm = TRUE),
      camera_hours = sum(camera_hours, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      mean_detection_interval = ifelse(animal_detections > 0, camera_hours / animal_detections, NA),
      possible_duplicates_percentage = (possible_duplicates_count / individuals_count) * 100,
      blank_detections_percentage = (blank_detections_count / (animal_detections + blank_detections_count)) * 100
    ) %>%
    left_join(species_class_counts, by = c("locality"))

  ### LOCALITY AGGREGATE ROW ###
  # Doing again I can get the species_class counts without specifying specific species_class's, which may change in config
  species_class_counts <- calculate_species_class_counts(obs_details)
  
  # We can't get the lines total with aggregation as they are only unique with the locality
  lines_total <- locality_summary %>%
    summarise(
      lines = sum(lines, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      locality = "Aggregate"
    )
  
  # The the rest of the data we can get from location_summary, without applying grouping
  # We need the lowest level of summary to calculate accurate means and percentages
  locality_summary_totals <- location_summary %>%
    # Need to count the locations before summarising, n_distinct doesn't work like numerical aggregation functions (sum etc)
    mutate(locations_count = n_distinct(locationName, na.rm = TRUE)) %>%
    summarise(
      locations = first(locations_count),
      across(
        c(deployments, animal_detections, individuals_count, possible_duplicates_count, camera_hours, blank_detections_count, unknown_detections_count),
        \(x) sum(x, na.rm = TRUE)
      )
    ) %>%
    mutate(
      locality = "Aggregate",
      mean_detection_interval = ifelse(animal_detections > 0, camera_hours / animal_detections, NA),
      possible_duplicates_percentage = (possible_duplicates_count / individuals_count) * 100,
      blank_detections_percentage = (blank_detections_count / (animal_detections + blank_detections_count)) * 100
    ) %>%
    left_join(species_class_counts, by = "locality") %>%
    left_join(lines_total, by = "locality")
    
  locality_summary_with_totals <- bind_rows(locality_summary, locality_summary_totals)
  
  return(list(
    location = location_summary,
    line = line_summary,
    locality = locality_summary_with_totals
  ))
}

generate_camera_network_overview <- function(deps) {

  # Filter by unique locationName, for views that include more than one deployment period e.g. ALL
  result <- deps %>%
    distinct(locationName, .keep_all = TRUE) %>%
    group_by(locality) %>%
    summarise(
      lines = n_distinct(line, na.rm = TRUE),
      locations = n_distinct(locationName, na.rm = TRUE),
      camera_rest_interval = first(cameraInterval)
    )

  area_data <- calculate_area_by_locality(deps)
  
  # Calculate all distances between markers once
  all_locations_distances_data <- calculate_distance_between_line_locations(deps, 1000000)
  
  # Group area_data by locality and calculate area estimate for each locality
  area_data <- area_data %>%
    group_by(locality) %>%
    summarise(encompassed_area_ha = sum(area_ha)) %>%
    ungroup()
  
  # Group all_locations_distances_data by locality and calculate average distance and count within threshold for each locality
  all_locations_distances_data <- all_locations_distances_data %>%
    group_by(locality) %>%
    summarise(mean_location_pair_spacing = mean(distance),
              location_pair_exceptions = sum(distance < config$globals$min_distance_threshold)) %>%
    ungroup()
  
  # Update result with new metrics and join with grouped area data
  result <- result %>%
    left_join(area_data, by = "locality") %>%
    left_join(all_locations_distances_data, by = "locality")
  
  # Calculate camera density, returning formatted column name with area in km2 instead of m2
  result <- result %>%
    #mutate(encompassed_area_ha = as.numeric(gsub("\\s*\\[.*\\]", "", encompassed_area_ha)) / 1e6) %>%
    mutate(camera_density_ha = locations / encompassed_area_ha)
  
    #mutate(encompassed_area_km2 = as.numeric(gsub("\\s*\\[.*\\]", "", encompassed_area_m2)) / 1e6) %>%
    #mutate(camera_density_km2 = locations / encompassed_area_km2)
  
  return(result)
}




