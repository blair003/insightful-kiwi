generate_select_input <- function(id, choices, selected, multiple = TRUE, label = "Species:") {
  selectizeInput(
    inputId = id,
    label = tagList(icon("paw"), label),  # Label with icon
    choices = choices,  # Choices passed as a parameter
    selected = selected,  # Dynamically selected species
    multiple = multiple,  # Allow multiple selections if TRUE
    options = list(
      placeholder = "Select species...",  # Placeholder text
      closeAfterSelect = TRUE  # Close dropdown after each selection
    )
  )
}



create_species_observation_density_grid <- function(obs, locality = NULL) {
  #browser()
  spp_class_names <- names(config$globals$spp_classes)
  species_name_type <- config$globals$species_name_type

  # Filter data if a specific locality is provided
  filtered_obs <- obs %>%
    mutate(
      date = as.Date(timestamp),
      species_name = .data[[species_name_type]]
    )

  if (!is.null(locality)) {
    filtered_obs <- filtered_obs %>% filter(locality == !!locality)
  }

  # Group and summarise the data by date, species, and locality
  filtered_obs <- filtered_obs %>%
    group_by(date, species_name, locality) %>%
    summarise(count = sum(count), .groups = 'drop')

  # Proceed with your data transformation
  obs_wide <- filtered_obs %>%
    pivot_wider(names_from = species_name, values_from = count,
                values_fill = list(count = 0), id_cols = c(date, locality)) %>%
    as.data.frame()

  # Transforming the data back to long format for ggplot
  obs_long <- pivot_longer(
    obs_wide,
    cols = -c(date, locality), # Ensure locality is also excluded from pivoting
    names_to = "species",
    values_to = "count"
  )


  # Summarize total counts by species
  species_counts <- obs_long %>%
    group_by(species) %>%
    summarise(total_count = sum(count)) %>%
    arrange(total_count)

  # Reorder species factor levels based on total count
  obs_long$species <- factor(obs_long$species,
                             levels = species_counts$species)


  # Determine the earliest start date for each locality
  earliest_dates <- filtered_obs %>%
    group_by(locality) %>%
    summarise(earliest_start_date = min(date)) %>%
    arrange(earliest_start_date) %>%
    ungroup()

  obs_long$locality <- factor(obs_long$locality,
                              levels = earliest_dates$locality)

  breaks <- c(0, 1, 2, 3, 4, 5, 10, 20, 50, 100) # Example breakpoints
  colors <- c("#F8F8F8", "#67a9cf", "#2166ac", "red")
  positions <- breaks / max(breaks) # Calculate positions for each break

  spp_obs_density_grid <- ggplot(obs_long, aes(x = date, y = species, fill = count)) +
    geom_tile(color = "white", linewidth = 0.2) +
    facet_wrap(~locality, scales = "free_x") +
    scale_fill_gradientn(colors = colorRampPalette(colors)(length(breaks)),
                         values = rescale(positions), # Ensure positions are correctly scaled
                         guide = guide_colourbar(barwidth = 0.5, barheight = 15,
                                                 title.position = "top", title.hjust = 0.5,
                                                 label.position = "right", label.hjust = 1)) +
    scale_x_date(date_breaks = "3 days", date_labels = "%d-%b") +
    theme_insightful_report() +
    labs(title = "Daily Species Counts",
         subtitle = "Colours scaled to make subtle variation in lower counts more prominent.",
         caption = sprintf("Data source: %s deployments | InsightfulKiwi", config$globals$organisation_abbrev),
         x = "Date",
         y = "Species",
         fill = "Count")



  return(spp_obs_density_grid)

}


theme_insightful_report <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Set the overall plot background
      plot.background = element_rect(fill = "white", color = NA),  # Ensure full plot background is white

      # Title and padding adjustments
      plot.title = element_text(size = rel(1.2), face = "bold", margin = margin(10, 0, 10, 0), hjust = 0),
      plot.subtitle = element_text(size = rel(1), margin = margin(5, 0, 10, 0)),  # Adjust subtitle margin
      plot.caption = element_text(size = rel(0.85), margin = margin(10, 0, 0, 0), hjust = 1),  # Caption at the bottom with margin

      # Panel and Grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),  # Ensure panel background is white

      # Axis
      axis.title = element_text(size = rel(0.95), face = "bold"),
      axis.text = element_text(size = rel(0.95)),
      axis.text.x = element_text(angle = 45),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),

      # Legend
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),

      # Facet Labels (strip)
      strip.background = element_rect(fill = "white", color = "white"),  # Ensure facet labels background is white
      strip.text = element_text(size = rel(0.85), face = "bold", color = "black", margin = margin(5,0,5,0)),

      # Adjust the plot margins to add more space around the entire plot
      plot.margin = margin(t = 20, r = 10, b = 20, l = 10)  # Adjust as needed for more space
    )
}
generate_multi_species_activity_plot <- function(sobs_data) {
  if (nrow(sobs_data) == 0) {
    return(plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No Data"))
  }

  sobs_data$scientificName <- as.character(sobs_data$scientificName)
  sobs_data$hour <- as.numeric(format(sobs_data$timestamp, "%H"))

  all_hours <- 0:23
  species_list <- sort(unique(as.character(sobs_data$scientificName)))
  if (length(species_list) == 0) {
    return(plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No Data"))
  }

  if ("count" %in% names(sobs_data)) {
    hourly_counts <- sobs_data %>%
      dplyr::group_by(scientificName, hour) %>%
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  } else {
    hourly_counts <- sobs_data %>%
      dplyr::group_by(scientificName, hour) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
  }

  all_combinations <- expand.grid(hour = all_hours, scientificName = species_list, stringsAsFactors = FALSE)
  plot_data <- dplyr::left_join(all_combinations, hourly_counts, by = c("hour", "scientificName")) %>%
    dplyr::mutate(
      count = dplyr::coalesce(.data$count, 0),
      hour_midpoint = .data$hour + 0.5
    )

  name_type <- config$globals$species_name_type

  if (name_type %in% names(sobs_data)) {
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

  plot_data$display_name <- stringr::str_to_title(plot_data$display_name)
  plot_data <- plot_data %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::mutate(species_total = sum(.data$count, na.rm = TRUE)) %>%
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

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = hour_midpoint,
      y = count,
      fill = legend_label
    )
  ) +
    ggplot2::geom_col(width = 0.92, colour = "black", linewidth = 0.25, alpha = 0.82) +
    ggplot2::facet_wrap(~ legend_label, scales = "free_y") +
    ggplot2::coord_polar(start = 0) +
    ggplot2::scale_x_continuous(breaks = 0:23 + 0.5, limits = c(0, 24), labels = paste0(0:23, ":00")) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.06))) +
    ggplot2::scale_fill_manual(values = rep(activity_palette, length.out = species_count)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey80"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0.5),
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::labs(
      title = "Detections by Hour of Day",
      caption = if (has_low_sample_species) {
        "Species with fewer than 10 detections are shown for completeness, but their activity pattern is uncertain."
      } else {
        NULL
      }
    )
}
