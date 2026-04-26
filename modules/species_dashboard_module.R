species_dashboard_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dashboard_header")),
    layout_column_wrap(
      width = "250px",
      uiOutput(ns("total_detections_card")),
      uiOutput(ns("unique_locations_card")),
      uiOutput(ns("rai_card"))
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Activity Pattern (Time of Day)"),
        plotOutput(ns("activity_plot"), height = "400px")
      ),
      card(
        card_header("Co-occurrence with Kiwi"),
        uiOutput(ns("cooccurrence_ui"))
      )
    )
  )
}

species_dashboard_module_server <- function(id, species_name, obs, deps, rai_norm_hours = 2000) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filter observations for the target species
    species_obs <- reactive({
      req(obs())
      obs() %>% dplyr::filter(tolower(scientificName) == tolower(species_name))
    })

    # Calculate metrics
    output$dashboard_header <- renderUI({
      h3(sprintf("%s Dashboard", str_to_title(species_name)))
    })

    output$total_detections_card <- renderUI({
      req(species_obs())
      total_count <- sum(species_obs()$count, na.rm = TRUE)
      card(
        card_header("Total Detections"),
        card_body(h2(total_count))
      )
    })

    output$unique_locations_card <- renderUI({
      req(species_obs())
      unique_locs <- length(unique(species_obs()$locationName))
      card(
        card_header("Unique Locations"),
        card_body(h2(unique_locs))
      )
    })

    output$rai_card <- renderUI({
      req(species_obs(), deps())
      # Simplified RAI calculation: total count / total hours * norm_hours
      total_count <- sum(species_obs()$count, na.rm = TRUE)

      # Calculate total hours for deployments that overlap with these obs dates, or just all deps.
      # Using all active deps in period
      deps_data <- deps()
      if (nrow(deps_data) > 0 && !is.null(deps_data$camera_hours)) {
         total_hours <- sum(deps_data$camera_hours, na.rm = TRUE)
         if(total_hours > 0) {
           rai <- (total_count / total_hours) * rai_norm_hours
         } else {
           rai <- 0
         }
      } else {
        rai <- 0
      }

      card(
        card_header("Network RAI"),
        card_body(h2(round(rai, 2)))
      )
    })

    # Activity Plot (Circular/Polar)
    output$activity_plot <- renderPlot({
      req(species_obs())
      sobs <- species_obs()
      if(nrow(sobs) == 0) return(plot(1, type="n", axes=F, xlab="", ylab="", main="No Data"))

      # Extract hour from timestamp
      # assuming timestamp is POSIXct
      sobs$hour <- as.numeric(format(sobs$timestamp, "%H"))

      # Aggregate by hour
      hourly_counts <- sobs %>%
        dplyr::group_by(hour) %>%
        dplyr::summarise(count = sum(count, na.rm = TRUE), .groups="drop")

      # Fill missing hours with 0
      all_hours <- data.frame(hour = 0:23)
      plot_data <- merge(all_hours, hourly_counts, by="hour", all.x=TRUE)
      plot_data$count[is.na(plot_data$count)] <- 0

      # ggplot polar bar chart
      library(ggplot2)
      ggplot(plot_data, aes(x = hour, y = count)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black") +
        coord_polar(start = 0) +
        scale_x_continuous(breaks = 0:23, limits = c(0, 24), labels = paste0(0:23, ":00")) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_line(color = "grey80"),
          plot.title = element_text(hjust = 0.5, face = "bold")
        ) +
        labs(title = "Detections by Hour of Day")
    })

    # Co-occurrence with Kiwi
    output$cooccurrence_ui <- renderUI({
      req(obs(), species_obs())

      kiwi_obs <- obs() %>% dplyr::filter(tolower(scientificName) == "apteryx mantelli")

      if(nrow(kiwi_obs) == 0) {
        return(p("No Kiwi observations in this period."))
      }
      if(nrow(species_obs()) == 0) {
         return(p("No target species observations to compare."))
      }

      # Find deployments where BOTH were seen
      s_deps <- unique(species_obs()$deploymentID)
      k_deps <- unique(kiwi_obs$deploymentID)

      shared_deps <- intersect(s_deps, k_deps)

      tagList(
        p(sprintf("This species was detected at %d unique deployments.", length(s_deps))),
        p(sprintf("Kiwi were detected at %d unique deployments.", length(k_deps))),
        h4(sprintf("Shared Deployments: %d", length(shared_deps))),
        p("Number of deployments where both this species and Kiwi were detected in the same period.")
      )
    })

  })
}
