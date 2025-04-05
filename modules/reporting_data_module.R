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
  browser()
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

# This function generates a tibble summarising key data for every species in obs, with a view by
# location, line,locality, and for the entire network
generate_spp_summary <- function(obs, deps, rai_norm_hours) {
  #browser()
  deployment_data <- deps %>%
    group_by(locality, line) %>%
    summarise(
      camera_hours = sum(camera_hours, na.rm = TRUE),
      .groups = "drop")
  

  spp_summary_location <- obs %>%
    group_by(locationName, scientificName) %>%
    summarise(
      locality = first(locality),
      line = first(line),
      locationName = first(locationName),
      `vernacularNames.eng` = first(`vernacularNames.eng`),
      species_class = first(species_class),
      animal_detections = n(),
      individuals_count = sum(count, na.rm = TRUE),
      possible_duplicates_count = sum(ifelse(possible_duplicate, count, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      possible_duplicates_percentage = (possible_duplicates_count / individuals_count) * 100,
      net_individuals_count = individuals_count - possible_duplicates_count
    )

  spp_summary_line <- obs %>%
    group_by(locality, line, scientificName) %>%
    summarise(
      `vernacularNames.eng` = first(`vernacularNames.eng`),
      species_class = first(species_class),
      animal_detections = n(),
      individuals_count = sum(count, na.rm = TRUE),
      possible_duplicates_count = sum(ifelse(possible_duplicate, count, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      possible_duplicates_percentage = (possible_duplicates_count / individuals_count) * 100,
      net_individuals_count = individuals_count - possible_duplicates_count
    )

  # We want to have an entry for every species for every locality-line view to get correct RAI and SE calculations
  # I may be overlooking an easier way, but this works.
  sci_name_vernacular_class <- obs %>%
    select(scientificName, `vernacularNames.eng`, species_class) %>%
    distinct()
  
  complete_grid <- tidyr::expand_grid(
    deployment_data %>% select(locality, line) %>% distinct(),
    sci_name_vernacular_class
  )
  
  # Ensure spp_summary_line includes all required combinations
  spp_summary_line <- complete_grid %>%
    left_join(spp_summary_line, by = c("locality", "line", "scientificName", "vernacularNames.eng", "species_class"))
  
 # Replace NA values with 0
  spp_summary_line <- spp_summary_line %>%
    replace_na(list(
      animal_detections = 0,
      individuals_count = 0,
      possible_duplicates_count = 0,
      possible_duplicates_percentage = 0,
      net_individuals_count = 0
    ))
  
  # Join with deployment_data to include camera_hours and blank_detections_count for all
  spp_summary_line <- spp_summary_line %>%
    left_join(deployment_data, by = c("locality", "line"))
  
  spp_summary_line <- spp_summary_line %>%
    mutate(
      RAI = (individuals_count / camera_hours) * rai_norm_hours ,
      RAI_net = (net_individuals_count / camera_hours) * rai_norm_hours 
    )

  spp_summary_locality <- spp_summary_line %>%
    group_by(locality, scientificName) %>%
    summarise(
      `vernacularNames.eng` = first(`vernacularNames.eng`),
      species_class = first(species_class),
      animal_detections = sum(animal_detections, na.rm = TRUE),
      individuals_count = sum(individuals_count, na.rm = TRUE),
      possible_duplicates_count = sum(possible_duplicates_count, na.rm = TRUE),
      net_individuals_count = sum(net_individuals_count, na.rm = TRUE),
      camera_hours = sum(camera_hours, na.rm = TRUE),
      mRAI = mean(RAI, na.rm = TRUE),
      sd_RAI = sd(RAI, na.rm = TRUE), # Calculate standard deviation for RAI within each group
      mRAI_net = mean(RAI_net, na.rm = TRUE),
      sd_RAI_net = sd(RAI_net, na.rm = TRUE), 
      rai_count = n(), # Same RAI counts for all species as we created 0 entries for each locality-line for each species
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      possible_duplicates_percentage = ifelse(possible_duplicates_count > 0, 
                                              (possible_duplicates_count / individuals_count) * 100, 
                                              0),
      SE = sd_RAI / sqrt(rai_count), 
      SE_filtered = sd_RAI_net / sqrt(rai_count), 
      mRAI_SE = sprintf("%0.1f ± %0.2f", mRAI, SE),
      mRAI_SE_net = sprintf("%0.1f ± %0.2f", mRAI_net, SE_filtered)
    )


  # Remember this will generate invalid SE if there is only one locality in the deployment selection!!
  spp_summary_network <- spp_summary_locality %>%
    group_by(scientificName) %>%
    summarise(
      `vernacularNames.eng` = first(`vernacularNames.eng`),
      species_class = first(species_class),
      animal_detections = sum(animal_detections, na.rm = TRUE),
      individuals_count = sum(individuals_count, na.rm = TRUE),
      possible_duplicates_count = sum(possible_duplicates_count, na.rm = TRUE),
      net_individuals_count = sum(net_individuals_count, na.rm = TRUE),
      camera_hours = sum(camera_hours, na.rm = TRUE),
      mmRAI = mean(mRAI, na.rm = TRUE),
      sd_mRAI = sd(mRAI, na.rm = TRUE), 
      mmRAI_net = mean(mRAI_net, na.rm = TRUE),
      sd_mRAI_net = sd(mRAI_net, na.rm = TRUE), 
      mrai_count = n(), 
    ) %>%
    mutate(
      possible_duplicates_percentage = ifelse(possible_duplicates_count > 0, 
                                              (possible_duplicates_count / individuals_count) * 100, 
                                              0),
      SE = sd_mRAI / sqrt(mrai_count), 
      SE_filtered = sd_mRAI_net / sqrt(mrai_count), 
      mmRAI_SE = sprintf("%0.1f ± %0.2f", mmRAI, SE),
      mmRAI_SE_net = sprintf("%0.1f ± %0.2f", mmRAI_net, SE_filtered)
    )
  
  
  # Create an aggregate row for locality 
  spp_summary_locality_totals <- spp_summary_locality %>%
    summarise(
      scientificName = paste(as.character(n_distinct(scientificName)), "(unique)"),
      `vernacularNames.eng` = paste(as.character(n_distinct(`vernacularNames.eng`)), "(unique)"),
      across(
        c(animal_detections, individuals_count, possible_duplicates_count, net_individuals_count, camera_hours),
        \(x) sum(x, na.rm = TRUE)
      )
    ) %>%
    mutate(
      locality = "Aggregate",
      possible_duplicates_percentage = ifelse(possible_duplicates_count > 0, 
                                              (possible_duplicates_count / individuals_count) * 100, 
                                              0)
    )
  
  spp_summary_locality_with_totals <- bind_rows(spp_summary_locality, spp_summary_locality_totals)
  
  
  
  # Combine species from the first two species groups ("protected" and "managed" by default) into one vector
#  species_classes_important <- c(config$globals$spp_classes[[1]], config$globals$spp_classes[[2]])
#  species_classes_less_important <- c(config$globals$spp_classes[[3]], config$globals$spp_classes[[4]])
  species_classes_all <- unlist(lapply(config$globals$spp_classes, function(x) x), recursive = TRUE, use.names = FALSE)
  
#  locality_species_rai_important_classes <- generate_rai_species_comparison_table_wide(spp_summary_locality, species_classes_important)
#  locality_species_rai_less_important_classes <- generate_rai_species_comparison_table_wide(spp_summary_locality, species_classes_less_important)

  locality_species_mean_rai_showing_class <- generate_rai_species_comparison_table(spp_summary_locality, species_classes_all)
  
  # We needed to have these for sd/RAI/SE calculations, not needed now
  if (config$globals$spp_summary_rm_zeros$line) {
    spp_summary_line <- spp_summary_line %>%
      dplyr::filter(individuals_count != 0)
  }
  
  if (config$globals$spp_summary_rm_zeros$locality) {
 # if (config$globals$spp_summary_locality_remove_zero_records) {
    spp_summary_locality <- spp_summary_locality %>%
      dplyr::filter(individuals_count != 0)

    spp_summary_locality_with_totals <- spp_summary_locality_with_totals %>%
      dplyr::filter(individuals_count != 0)
  }
  
  return(list(
    location = spp_summary_location,
    line = spp_summary_line,
    locality = spp_summary_locality_with_totals,
    network = spp_summary_network,
    locality_species_mean_rai_showing_class = locality_species_mean_rai_showing_class
    #,
    #locality_species_rai_important_classes = locality_species_rai_important_classes,
    #locality_species_rai_less_important_classes = locality_species_rai_less_important_classes
  )
    
  )
}

generate_rai_species_comparison_table <- function(spp_summary, species) {

  # Determine which column to use based on config
  mRAI_column <- ifelse(config$globals$rai_net_count, "mRAI_SE_net", "mRAI_SE")
  logger::log_debug(sprintf("reporting_data_module_server: RAI data using values from: %s", mRAI_column))
  
  # Convert species list to lowercase for case-insensitive comparison
  species_lower <- tolower(species)
  
  # Filter data to include only species of interest, ensure case-insensitive match
  filtered_data <- spp_summary %>%
    select(scientificName, vernacularNames.eng, species_class, mRAI_SE, mRAI_SE_net, locality) %>%
    mutate(scientificName_lower = tolower(scientificName),
           species_order = match(scientificName_lower, species_lower)) %>%
    dplyr::filter(!is.na(species_order)) %>%
    arrange(species_order) %>%
    select(-scientificName_lower, -species_order)
  
  # Issues trying to select the column dynamically, so lets mutate based on the mRAI_column
  filtered_data <- filtered_data %>%
    mutate(RAI_value = .data[[mRAI_column]]) %>%
    select(-mRAI_SE_net, -mRAI_SE)
  
  # Pivot data to wide format
  wide_data <- filtered_data %>%
    pivot_wider(
      names_from = locality, 
      values_from = RAI_value,
      names_glue = "RAI ± SE: {locality}",  # Prefix each locality with "RAI ± SE:"
      values_fill = list(RAI_value = NA)
    )
  
  # Dynamically determine columns to keep and remove, setting up for supporting vernacularNames in different languages later
  # Identify columns that are not the specified output name type and match vernacularNames pattern or are scientificName
  columns_to_remove <- names(wide_data) %>%
    # We want to remove columns that are NOT the specified nametype and
    # are either any vernacularNames (not matching the specified one) or the scientificName (if not specified)
    .[!grepl(paste0("^", config$globals$species_name_type, "$"), .) & 
        (grepl("^vernacularNames\\.", .) | . == "scientificName")]
  
  # Now, dynamically select columns in wide_data, removing unwanted ones
  wide_data <- wide_data %>%
    select(-all_of(columns_to_remove))
  
  return(wide_data)
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




