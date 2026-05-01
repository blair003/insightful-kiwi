generate_select_input <- function(id, choices, selected, multiple = TRUE, label = "Species selection:") {
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
