reporting_visulisations_module_ui <- function(id, label) {
  ns <- NS(id)
  
}

reporting_visualisations_module_server <- function(id, obs) {
  moduleServer(id, function(input, output, session) {
    logger::log_debug("reporting_visualisations_module.R, reporting_visualisations_module_server() moduleServer running")
    # Store errors for use with tryCatch
    data_error <- reactiveVal(NULL) 

    reporting_visualisations <- reactive({
      logger::log_debug("reporting_data_module_server, reporting_data() reactive triggered...\n")
      
      req(obs())
      
      tryCatch({
        result <- generate_reporting_visualisations(obs())
        
        # Check if the result is NULL (no data)
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
      visualisations = reporting_visualisations
    ))
    
  })
}


generate_reporting_visualisations <- function(obs, locality = NULL) {

    spp_class_names <- names(config$globals$spp_classes)
    species_name_type <- config$globals$species_name_type
    
    # Filter data if a specific locality is provided
    filtered_obs <- obs %>%
      mutate(
        locality = locality,  # If locality column exists already, remove this line
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
    
    breaks <- c(0, 1, 2, 3, 4, 5, 10, 20, 50, 100) # breakpoints
    colors <- c("#F8F8F8", "#67a9cf", "#2166ac", "red") 
    positions <- breaks / max(breaks) # Calculate positions for each break
    
    locality_ggplot_daily_species_count <- ggplot(obs_long, aes(x = date, y = species, fill = count)) +
      geom_tile(color = "white", linewidth = 0.2) + 
      facet_wrap(~locality, scales = "free_x") +
      scale_fill_gradientn(colors = colorRampPalette(colors)(length(breaks)), 
                           values = rescale(positions), # Ensure positions are correctly scaled
                           guide = guide_colourbar(barwidth = 0.5, barheight = 15, 
                                                   title.position = "top", title.hjust = 0.5,
                                                   label.position = "right", label.hjust = 1)) +
      scale_x_date(date_breaks = "3 days", date_labels = "%d-%b") +
      theme_insightful() +
      labs(title = "Daily Species Counts by Locality",
           subtitle = "Colours scaled to make subtle variation in lower counts more prominant.",
           caption = sprintf("Data source: %s deployments | InsightfulKiwi", config$globals$organisation_abbrev),
           x = "Date",
           y = "Species",
           fill = "Count")
    
    
  return(list(
    locality_ggplot_daily_species_count = locality_ggplot_daily_species_count
    )
  )
  
}


theme_insightful <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      
      plot.title = element_text(size = rel(1.2), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.75)),
      axis.text.x = element_text(angle = 45),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)), # Adjust axis title margin
      #axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      
      # For horizontal facet labels
      #strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "black", margin = margin(5,0,5,0))
    )
}