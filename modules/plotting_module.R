plotting_module_ui <- function(id, 
                               view = "plot",
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
        )
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
        ),
        checkboxInput(ns("combine_localities"), "Combine selected localities", value = TRUE)

        
      )
    )
  } else if (view == "select_plot_options") {
    return(
      tagList(
        div(
          class = "form-group",  # Ensures consistent styling
          tags$label(
            class = "control-label plot-options-label",
            tagList(icon("sliders-h"), "Toggle options:")
          ),
          checkboxInput(ns("stacked"), "Stacked", value = TRUE),
          checkboxInput(ns("data_labels"), "Data labels", value = TRUE)
        )
        
        
      )
    )
  } else if (view == "plot") {
    return(
      tagList(
        plotOutput(ns("obs_plot"))
      )
    )
  } else if (view == "summary") {
    return(
      tagList(
        uiOutput(ns("obs_summary"))
      )
    )
  }
}




plotting_module_server <- function(id,
                                   type = "density",
                                   obs,
                                   deps,
                                   species_override = NULL) {

  moduleServer(id, function(input, output, session) {
    logger::log_debug(sprintf("plotting_module_server, %s moduleServer() running", id))

    get_plot_width <- function() {
      plot_width <- session$clientData[[paste0("output_", session$ns("obs_plot"), "_width")]]

      if (is.null(plot_width)) {
        plot_width <- session$clientData[["output_obs_plot_width"]]
      }

      if (is.null(plot_width) || is.na(plot_width) || plot_width <= 0) {
        return(0)
      }

      plot_width
    }

    x_axis_text_theme <- function(periods, plot_width, facet_count = 1) {
      period_labels <- unique(as.character(periods))
      period_labels <- period_labels[!is.na(period_labels)]
      label_count <- length(period_labels)

      if (label_count == 0) {
        return(element_text())
      }

      longest_label <- max(nchar(period_labels))
      facet_columns <- if (facet_count <= 1) 1 else ceiling(sqrt(facet_count))
      panel_width <- plot_width / max(1, facet_columns)
      estimated_required_width <- label_count * longest_label * 7
      should_rotate <- plot_width > 0 && (
        estimated_required_width > panel_width * 0.85
      )

      if (should_rotate) {
        element_text(angle = 90, hjust = 1, vjust = 0.5)
      } else {
        element_text(angle = 0, hjust = 0.5, vjust = 1)
      }
    }

    selected_species <- reactive({
     # req(input$selected_species)  # Ensure species is selected
      logger::log_debug(sprintf("plotting_module_server, %s input$selected_species changed", id))
      # Ensure selected species is always treated as a vector
      as.character(input$selected_species)
    })
    
    selected_localities <- reactive({
     # req(input$selected_localities)  # Uncomment to ensure localities are selected
      as.character(input$selected_localities)
    })
    
    plotting_data <- reactive({
      #browser()
      req(obs, deps)
      species <- selected_species()
      localities <- selected_localities()
      
      logger::log_debug(sprintf(
        "plotting_module_server, %s plotting_data() reactive triggered for species: %s and localities: %s.", 
        id, paste(species, collapse = ", "), paste(localities, collapse = ", ")
      ))
      
      # Exclude 'ALL' group while preserving the order
      periods_without_all <- names(core_data$period_groups)
      periods_without_all <- periods_without_all[periods_without_all != "ALL"]
      
      # Select the species name type (either scientific or vernacular)
      species_name_column <- sym(config$globals$species_name_type)  # Dynamically select the species name column
     ## browser()
      # Filter observations by selected species and localities
      obs_filtered <- obs %>%
        filter(scientificName_lower %in% tolower(species),
               locality %in% localities)
      
      # Create a species lookup table with the necessary name columns
      species_lookup <- obs_filtered %>%
        select(scientificName_lower, scientificName, `vernacularNames.eng`) %>%
        distinct()
      
      # Filter the localities and periods to include only those with deployments
      valid_localities_periods <- deps %>%
        filter(period %in% periods_without_all, locality %in% selected_localities()) %>%  # Filter by valid periods and selected localities
        distinct(locality, period)
      
      # Create a grid of all species and valid combinations of periods and localities
      all_combinations <- expand.grid(
        scientificName_lower = species_lookup$scientificName_lower,
        period = unique(valid_localities_periods$period),  
        locality = unique(valid_localities_periods$locality),
        stringsAsFactors = FALSE
      ) %>%
        # Perform an inner join to keep only valid locality-period combinations
        inner_join(valid_localities_periods, by = c("locality", "period")) %>%
        left_join(species_lookup, by = "scientificName_lower")
      
      # Summarize counts in the filtered data
      data_summarized <- obs_filtered %>%
        group_by(scientificName_lower, period, locality) %>%
        summarize(count = sum(count), .groups = 'drop')
      
      # Merge the observation data with the complete grid, filling missing values with 0
      complete_data <- all_combinations %>%
        left_join(data_summarized, by = c("scientificName_lower", "period", "locality")) %>%
        replace_na(list(count = 0))
      
      # Dynamically select the species name column based on config
      complete_data <- complete_data %>%
        mutate(species_name = !!species_name_column)
      
      # Filter out rows where the species name is NA (handles edge cases)
      complete_data <- complete_data %>%
        filter(!is.na(species_name))
      
      # Group by species, period, and locality, then sum the counts
      aggregated_data <- complete_data %>%
        group_by(species_name, period, locality) %>%
        summarize(count = sum(count), .groups = 'drop')
      
      # Check if 'Combine into single graph' is selected
      if (isTRUE(input$combine_localities)) {
        # Combine data for all localities into a single line
        aggregated_data <- aggregated_data %>%
          group_by(species_name, period) %>%
          summarize(count = sum(count), .groups = 'drop')  # Combine counts across localities
      }
      
      # Set the period factor levels in chronological order (oldest to newest)
      aggregated_data$period <- factor(aggregated_data$period, levels = rev(periods_without_all))
      #browser()
      return(list(
        aggregated_data = aggregated_data,
        localities = localities
      ))
      
    })
    
    
  
    output$obs_plot <- renderPlot({
      req(plotting_data())
      data <- plotting_data()
      plot_width <- get_plot_width()
      facet_count <- if (isTRUE(input$combine_localities)) {
        1
      } else {
        length(unique(data$aggregated_data$locality))
      }
      axis_text_x <- x_axis_text_theme(
        data$aggregated_data$period,
        plot_width,
        facet_count
      )

      if (nrow(data$aggregated_data) == 0) {
        plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
        title("No data available")
        return(NULL)
      }
      
      # Convert period to a factor so that ggplot treats the x-axis as discrete
      data$aggregated_data$period <- factor(data$aggregated_data$period)
      
      # Determine bar position based on stack checkbox
      bar_position <- if (input$stacked) "stack" else position_dodge(width = 0.9)
      
      # Create the base plot depending on whether localities are combined or not
      if (!isTRUE(input$combine_localities)) {
        p <- ggplot(data$aggregated_data, aes(x = period, y = count, fill = species_name)) +
          geom_bar(stat = "identity", position = bar_position) +
          labs(x = "Period", y = "Individuals Count", fill = "Species") +
          facet_wrap(~ locality) +
          theme_minimal(base_size = 14) +
          theme(
            strip.text = element_text(size = rel(1.3), face = "bold"),
            axis.text = element_text(size = rel(1)),
            legend.position = "bottom",
            legend.text = element_text(size = rel(1)),
            legend.title = element_text(size = rel(1)),
            legend.key.size = unit(1.5, "lines"),
            axis.text.x = axis_text_x
          ) +
          scale_fill_brewer(palette = "Set1")

      } else {
        title <- sprintf("Combined observations across: %s", 
                         paste(data$localities, collapse = ", "))
        
        p <- ggplot(data$aggregated_data, aes(x = period, y = count, fill = species_name)) +
          geom_bar(stat = "identity", position = bar_position) +
          labs(title = title, x = "Period", y = "Individuals Count", fill = "Species") +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(size = rel(1.3), face = "bold"),
            axis.text = element_text(size = rel(1)),
            legend.position = "bottom",
            legend.text = element_text(size = rel(1)),
            legend.title = element_text(size = rel(1)),
            legend.key.size = unit(1.5, "lines"),
            axis.text.x = axis_text_x
          ) +
          scale_fill_brewer(palette = "Set1")
      }
      
      # Conditionally add data labels
      if (input$data_labels) {
        if (input$stacked) {
          p <- p + geom_text(aes(label = ifelse(count == 0, "", count)), 
                             position = position_stack(vjust = 0.5), 
                             size = 5, 
                             color = "white")
        } else {
          p <- p + geom_text(aes(y = count/2, label = ifelse(count == 0, "", count)), 
                             position = position_dodge(width = 0.9), 
                             size = 5, 
                             color = "white")
        }
      }
      
      print(p)
    })
    
    
    
    
    
  })
}
