# This function generates a tibble summarising key data for every species in obs, with a view by
# location, line, locality, and for the entire network.
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

  rai_groups <- generate_rai_group_summary(
    obs,
    deployment_data,
    config$globals$rai_groups,
    rai_norm_hours
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
    rai_groups = rai_groups,
    locality_species_mean_rai_showing_class = locality_species_mean_rai_showing_class
    #,
    #locality_species_rai_important_classes = locality_species_rai_important_classes,
    #locality_species_rai_less_important_classes = locality_species_rai_less_important_classes
  )

  )
}

generate_rai_group_summary <- function(obs, deployment_data, rai_groups, rai_norm_hours) {
  if (is.null(rai_groups) || length(rai_groups) == 0) {
    return(list(
      line = tibble::tibble(),
      locality = tibble::tibble(),
      network = tibble::tibble()
    ))
  }

  rai_group_lookup <- tibble::tibble(
    rai_group = rep(names(rai_groups), lengths(rai_groups)),
    scientificName_lower = tolower(unlist(rai_groups, use.names = FALSE))
  )

  group_grid <- tidyr::expand_grid(
    deployment_data %>% select(locality, line, camera_hours) %>% distinct(),
    rai_group = names(rai_groups)
  )

  group_counts <- obs %>%
    mutate(scientificName_lower = tolower(scientificName)) %>%
    inner_join(rai_group_lookup, by = "scientificName_lower") %>%
    group_by(locality, line, rai_group) %>%
    summarise(
      animal_detections = n(),
      individuals_count = sum(count, na.rm = TRUE),
      possible_duplicates_count = sum(ifelse(possible_duplicate, count, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      net_individuals_count = individuals_count - possible_duplicates_count
    )

  rai_group_line <- group_grid %>%
    left_join(group_counts, by = c("locality", "line", "rai_group")) %>%
    replace_na(list(
      animal_detections = 0,
      individuals_count = 0,
      possible_duplicates_count = 0,
      net_individuals_count = 0
    )) %>%
    mutate(
      RAI = (individuals_count / camera_hours) * rai_norm_hours,
      RAI_net = (net_individuals_count / camera_hours) * rai_norm_hours,
      possible_duplicates_percentage = ifelse(
        individuals_count > 0,
        (possible_duplicates_count / individuals_count) * 100,
        0
      )
    )

  rai_group_locality <- rai_group_line %>%
    group_by(locality, rai_group) %>%
    summarise(
      animal_detections = sum(animal_detections, na.rm = TRUE),
      individuals_count = sum(individuals_count, na.rm = TRUE),
      possible_duplicates_count = sum(possible_duplicates_count, na.rm = TRUE),
      net_individuals_count = sum(net_individuals_count, na.rm = TRUE),
      camera_hours = sum(camera_hours, na.rm = TRUE),
      mRAI = mean(RAI, na.rm = TRUE),
      sd_RAI = sd(RAI, na.rm = TRUE),
      mRAI_net = mean(RAI_net, na.rm = TRUE),
      sd_RAI_net = sd(RAI_net, na.rm = TRUE),
      rai_count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      possible_duplicates_percentage = ifelse(
        individuals_count > 0,
        (possible_duplicates_count / individuals_count) * 100,
        0
      ),
      SE = sd_RAI / sqrt(rai_count),
      SE_filtered = sd_RAI_net / sqrt(rai_count),
      mRAI_SE = sprintf("%0.1f ± %0.2f", mRAI, SE),
      mRAI_SE_net = sprintf("%0.1f ± %0.2f", mRAI_net, SE_filtered)
    )

  rai_group_network <- rai_group_locality %>%
    group_by(rai_group) %>%
    summarise(
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
      .groups = "drop"
    ) %>%
    mutate(
      possible_duplicates_percentage = ifelse(
        individuals_count > 0,
        (possible_duplicates_count / individuals_count) * 100,
        0
      ),
      SE = sd_mRAI / sqrt(mrai_count),
      SE_filtered = sd_mRAI_net / sqrt(mrai_count),
      mmRAI_SE = sprintf("%0.1f ± %0.2f", mmRAI, SE),
      mmRAI_SE_net = sprintf("%0.1f ± %0.2f", mmRAI_net, SE_filtered)
    )

  list(
    line = rai_group_line,
    locality = rai_group_locality,
    network = rai_group_network
  )
}

generate_rai_group_network_metric <- function(obs, deps, rai_groups, rai_group, rai_norm_hours, use_net = TRUE) {
  empty_metric <- list(
    value = NA_real_,
    se = NA_real_,
    formatted_value = "N/A",
    animal_detections = NA_real_,
    individuals_count = NA_real_,
    possible_duplicates_count = NA_real_,
    net_individuals_count = NA_real_,
    camera_hours = NA_real_,
    locality_count = NA_integer_,
    line_count = NA_integer_,
    rai_norm_hours = rai_norm_hours,
    use_net = use_net
  )

  if (is.null(rai_groups) || is.null(rai_groups[[rai_group]]) || nrow(deps) == 0) {
    return(empty_metric)
  }

  deployment_data <- deps %>%
    group_by(locality, line) %>%
    summarise(
      camera_hours = sum(camera_hours, na.rm = TRUE),
      .groups = "drop"
    )

  rai_group_summary <- generate_rai_group_summary(
    obs,
    deployment_data,
    rai_groups,
    rai_norm_hours
  )

  locality_summary <- rai_group_summary$locality
  network_summary <- rai_group_summary$network
  if (is.null(locality_summary) || nrow(locality_summary) == 0 || is.null(network_summary) || nrow(network_summary) == 0) {
    return(empty_metric)
  }

  group_locality_rows <- locality_summary[locality_summary$rai_group == rai_group, , drop = FALSE]
  if (nrow(group_locality_rows) == 0) {
    return(empty_metric)
  }

  if (nrow(group_locality_rows) == 1) {
    group_row <- group_locality_rows
    value_column <- if (isTRUE(use_net)) "mRAI_net" else "mRAI"
    se_column <- if (isTRUE(use_net)) "SE_filtered" else "SE"
    formatted_column <- if (isTRUE(use_net)) "mRAI_SE_net" else "mRAI_SE"
    locality_count <- 1L
  } else {
    value_column <- if (isTRUE(use_net)) "mmRAI_net" else "mmRAI"
    se_column <- if (isTRUE(use_net)) "SE_filtered" else "SE"
    formatted_column <- if (isTRUE(use_net)) "mmRAI_SE_net" else "mmRAI_SE"
    if (!all(c(value_column, se_column, formatted_column) %in% names(network_summary))) {
      return(empty_metric)
    }

    group_row <- network_summary[network_summary$rai_group == rai_group, , drop = FALSE]
    if (nrow(group_row) == 0) {
      return(empty_metric)
    }
    locality_count <- as.integer(group_row$mrai_count[1])
  }

  if (!all(c(value_column, se_column, formatted_column) %in% names(group_row))) {
    return(empty_metric)
  }

  group_line_summary <- rai_group_summary$line[rai_group_summary$line$rai_group == rai_group, , drop = FALSE]

  list(
    value = as.numeric(group_row[[value_column]][1]),
    se = as.numeric(group_row[[se_column]][1]),
    formatted_value = as.character(group_row[[formatted_column]][1]),
    animal_detections = as.numeric(group_row$animal_detections[1]),
    individuals_count = as.numeric(group_row$individuals_count[1]),
    possible_duplicates_count = as.numeric(group_row$possible_duplicates_count[1]),
    net_individuals_count = as.numeric(group_row$net_individuals_count[1]),
    camera_hours = as.numeric(group_row$camera_hours[1]),
    locality_count = locality_count,
    line_count = nrow(group_line_summary),
    rai_norm_hours = rai_norm_hours,
    use_net = use_net
  )
}

generate_rai_group_network_value <- function(obs, deps, rai_groups, rai_group, rai_norm_hours, use_net = TRUE) {
  metric <- generate_rai_group_network_metric(obs, deps, rai_groups, rai_group, rai_norm_hours, use_net)
  metric$value
}

generate_rai_group_period_comparison <- function(obs,
                                                 deps,
                                                 period_groups,
                                                 rai_groups,
                                                 rai_group,
                                                 rai_norm_hours,
                                                 use_net = TRUE,
                                                 current_period_index = 1,
                                                 lower_is_better = TRUE) {
  period_names <- names(period_groups)
  period_names <- period_names[period_names != "ALL"]

  empty_period_metric <- function(period_name = NA_character_) {
    list(
      period = period_name,
      start_date = NA,
      end_date = NA,
      value = NA_real_,
      se = NA_real_,
      formatted_value = "N/A",
      animal_detections = NA_real_,
      individuals_count = NA_real_,
      possible_duplicates_count = NA_real_,
      net_individuals_count = NA_real_,
      camera_hours = NA_real_,
      locality_count = NA_integer_,
      line_count = NA_integer_,
      rai_norm_hours = rai_norm_hours,
      use_net = use_net
    )
  }

  compare_periods <- function(current_metric, comparison_metric) {
    comparison_period <- comparison_metric$period
    delta <- current_metric$value - comparison_metric$value
    display_delta <- round(delta, 1)
    direction <- dplyr::case_when(
      is.na(current_metric$value) || is.na(comparison_metric$value) ~ "unavailable",
      display_delta < 0 ~ "down",
      display_delta > 0 ~ "up",
      TRUE ~ "unchanged"
    )
    state <- dplyr::case_when(
      direction == "unavailable" ~ "unavailable",
      direction == "unchanged" ~ "unchanged",
      lower_is_better && direction == "down" ~ "improved",
      lower_is_better && direction == "up" ~ "worse",
      !lower_is_better && direction == "up" ~ "improved",
      !lower_is_better && direction == "down" ~ "worse"
    )
    message <- dplyr::case_when(
      state == "unavailable" ~ "No comparison period",
      direction == "down" ~ sprintf("Down %.1f vs %s", abs(display_delta), comparison_period),
      direction == "up" ~ sprintf("Up %.1f vs %s", abs(display_delta), comparison_period),
      TRUE ~ sprintf("No change vs %s", comparison_period)
    )

    list(
      period = comparison_period,
      value = comparison_metric$value,
      formatted_value = comparison_metric$formatted_value,
      delta = delta,
      direction = direction,
      state = state,
      message = message
    )
  }

  overall_state <- function(comparisons) {
    states <- vapply(comparisons, function(x) x$state, character(1))
    states <- states[states != "unavailable"]

    if (length(states) == 0) {
      return("unavailable")
    }
    if (all(states == "improved")) {
      return("improved")
    }
    if (all(states == "worse")) {
      return("worse")
    }
    if (all(states == "unchanged")) {
      return("unchanged")
    }

    "mixed"
  }

  find_matching_prior_season <- function(current_period_name) {
    current_match <- regexec("^(.+)\\s+(\\d{4})$", current_period_name)
    current_parts <- regmatches(current_period_name, current_match)[[1]]
    if (length(current_parts) != 3) {
      return(NA_character_)
    }

    target_period_name <- paste(trimws(current_parts[[2]]), as.integer(current_parts[[3]]) - 1)
    if (target_period_name %in% period_names) {
      return(target_period_name)
    }

    NA_character_
  }

  empty_result <- function(message) {
    list(
      rai_group = rai_group,
      current_period = NA_character_,
      prior_period = NA_character_,
      matching_prior_season_period = NA_character_,
      current_value = NA_real_,
      current_se = NA_real_,
      current_formatted_value = "N/A",
      current_metric = empty_period_metric(),
      prior_value = NA_real_,
      prior_se = NA_real_,
      prior_formatted_value = "N/A",
      prior_metric = empty_period_metric(),
      matching_prior_season_value = NA_real_,
      matching_prior_season_se = NA_real_,
      matching_prior_season_formatted_value = "N/A",
      matching_prior_season_metric = empty_period_metric(),
      comparisons = list(),
      comparison_state = "unavailable",
      comparison_message = message
    )
  }

  if (length(period_names) == 0) {
    return(empty_result("No period data"))
  }

  current_period_index <- suppressWarnings(as.integer(current_period_index[1]))
  if (is.na(current_period_index) || current_period_index < 1) {
    current_period_index <- 1
  }

  current_period_index <- min(current_period_index, length(period_names))
  prior_period_index <- current_period_index + 1

  current_period_name <- period_names[[current_period_index]]
  prior_period_name <- if (prior_period_index <= length(period_names)) {
    period_names[[prior_period_index]]
  } else {
    NA_character_
  }
  matching_prior_season_period_name <- find_matching_prior_season(current_period_name)
  if (!is.na(matching_prior_season_period_name) &&
      !is.na(prior_period_name) &&
      matching_prior_season_period_name == prior_period_name) {
    matching_prior_season_period_name <- NA_character_
  }

  calculate_period_metric <- function(period_name) {
    if (is.na(period_name) || !period_name %in% names(period_groups)) {
      return(empty_period_metric(period_name))
    }

    period <- period_groups[[period_name]]
    period_obs <- filter_obs(obs, period$start_date, period$end_date)
    period_deps <- filter_deps(deps, period$start_date, period$end_date)

    metric <- generate_rai_group_network_metric(
      period_obs,
      period_deps,
      rai_groups,
      rai_group,
      rai_norm_hours,
      use_net
    )

    c(
      list(
        period = period_name,
        start_date = period$start_date,
        end_date = period$end_date
      ),
      metric
    )
  }

  current_metric <- calculate_period_metric(current_period_name)
  prior_metric <- calculate_period_metric(prior_period_name)
  matching_prior_season_metric <- calculate_period_metric(matching_prior_season_period_name)
  current_value <- current_metric$value
  prior_value <- prior_metric$value
  matching_prior_season_value <- matching_prior_season_metric$value

  comparisons <- list(
    prior_period = compare_periods(current_metric, prior_metric),
    matching_prior_season = compare_periods(current_metric, matching_prior_season_metric)
  )
  comparison_state <- overall_state(comparisons)
  comparison_message <- paste(
    vapply(comparisons, function(x) x$message, character(1)),
    collapse = "\n"
  )

  list(
    rai_group = rai_group,
    current_period = current_period_name,
    prior_period = prior_period_name,
    matching_prior_season_period = matching_prior_season_period_name,
    current_value = current_value,
    current_se = current_metric$se,
    current_formatted_value = current_metric$formatted_value,
    current_metric = current_metric,
    prior_value = prior_value,
    prior_se = prior_metric$se,
    prior_formatted_value = prior_metric$formatted_value,
    prior_metric = prior_metric,
    matching_prior_season_value = matching_prior_season_value,
    matching_prior_season_se = matching_prior_season_metric$se,
    matching_prior_season_formatted_value = matching_prior_season_metric$formatted_value,
    matching_prior_season_metric = matching_prior_season_metric,
    comparisons = comparisons,
    comparison_state = comparison_state,
    comparison_message = comparison_message
  )
}

generate_rai_species_comparison_table <- function(spp_summary, species) {

  # Determine which column to use based on config
  mRAI_column <- ifelse(config$globals$rai_net_count, "mRAI_SE_net", "mRAI_SE")
  logger::log_debug(sprintf("metrics_functions: RAI data using values from: %s", mRAI_column))

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
