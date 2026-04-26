species_dashboard_period_defaults <- function(core_data) {
  period_names <- period_names_without_all(core_data$period_groups)
  current_period <- core_data$period_defaults$primary_period
  current_period_index <- match(current_period, period_names)

  prior_period <- if (!is.na(current_period_index) && current_period_index < length(period_names)) {
    period_names[[current_period_index + 1]]
  } else {
    current_period
  }

  last_year_period <- find_matching_prior_year_period(current_period, core_data$period_groups)
  if (is.na(last_year_period)) {
    last_year_period <- core_data$period_defaults$comparative_period
  }

  list(
    current_period = current_period,
    prior_period = prior_period,
    last_year_period = last_year_period
  )
}

species_dashboard_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dashboard_header")),

    # 3 Tabs
    navset_tab(
      id = ns("dashboard_tabs"),

      # 1. Overall / Combined Tab
      nav_panel("Overall",
        value = "overall",
        br(),
        layout_column_wrap(
          width = "250px",
          uiOutput(ns("overall_total_detections_card")),
          uiOutput(ns("overall_unique_locations_card")),
          uiOutput(ns("overall_rai_card"))
        ),
        br(),
        # Add RAI plot specific to this species
        card(
          class = "dashboard-plot-card",
          card_header(tagList(icon("chart-line"), "RAI for selected species")),
          plotting_module_ui(id = ns("overall_rai_plot"), view = "rai_plot"),
          full_screen = FALSE
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Activity Pattern (Time of Day)"),
            plotOutput(ns("overall_activity_plot"), height = "400px")
          ),
          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("overall_cooccurrence_ui"))
          )
        )
      ),

      # 2. Prior Period Tab
      nav_panel(
        title = textOutput(ns("current_period_name"), inline = TRUE),
        value = "current_period",
        br(),
        layout_column_wrap(
          width = "250px",
          uiOutput(ns("current_total_detections_card")),
          uiOutput(ns("current_unique_locations_card")),
          uiOutput(ns("current_rai_card"))
        ),
        br(),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Activity Pattern (Time of Day)"),
            plotOutput(ns("current_activity_plot"), height = "400px")
          ),
          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("current_cooccurrence_ui"))
          )
        )
      ),

      # 3. Prior Period Tab
      nav_panel(
        title = textOutput(ns("prior_period_name"), inline = TRUE),
        value = "prior_period",
        br(),
        layout_column_wrap(
          width = "250px",
          uiOutput(ns("prior_total_detections_card")),
          uiOutput(ns("prior_unique_locations_card")),
          uiOutput(ns("prior_rai_card"))
        ),
        br(),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Activity Pattern (Time of Day)"),
            plotOutput(ns("prior_activity_plot"), height = "400px")
          ),
          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("prior_cooccurrence_ui"))
          )
        )
      ),

      # 4. Last Year Tab
      nav_panel(
        title = textOutput(ns("last_year_period_name"), inline = TRUE),
        value = "last_year_period",
        br(),
        layout_column_wrap(
          width = "250px",
          uiOutput(ns("last_year_total_detections_card")),
          uiOutput(ns("last_year_unique_locations_card")),
          uiOutput(ns("last_year_rai_card"))
        ),
        br(),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Activity Pattern (Time of Day)"),
            plotOutput(ns("last_year_activity_plot"), height = "400px")
          ),
          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("last_year_cooccurrence_ui"))
          )
        )
      )
    )
  )
}

species_dashboard_module_server <- function(id, species_name, vernacular_name, obs, deps, core_data, rai_norm_hours = 2000) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_localities <- reactive({
      localities <- input[["overall_rai_plot-selected_localities"]]
      if (is.null(localities) || length(localities) == 0) {
        return(unique(as.character(core_data$deps$locality)))
      }

      as.character(localities)
    })

    filter_dashboard_obs <- function(obs_data) {
      obs_data %>% dplyr::filter(.data$locality %in% selected_localities())
    }

    filter_dashboard_deps <- function(deps_data) {
      deps_data %>% dplyr::filter(.data$locality %in% selected_localities())
    }

    locality_filter_token <- function() {
      paste(selected_localities(), collapse = ",")
    }

    # Dashboard Header
    output$dashboard_header <- renderUI({
      tagList(
        h2(sprintf("%s Dashboard", str_to_title(vernacular_name))),
        h4(em(species_name))
      )
    })

    # Helper to generate the cards
    generate_cards <- function(species_obs, deps_data, period_name_label) {
      total_count <- sum(species_obs$count, na.rm = TRUE)
      unique_locs <- length(unique(species_obs$locationName))

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

      review_action <- if (total_count > 0) {
        onclick_payload <- jsonlite::toJSON(
          list(
            period_name = period_name_label,
            species_name = species_name,
            locality = locality_filter_token()
          ),
          auto_unbox = TRUE
        )
        onclick_js <- sprintf(
          "Shiny.setInputValue('review_sequences_click', %s, {priority: 'event'}); return false;",
          onclick_payload
        )

        div(
          class = "dashcard-card-action",
          tags$a(
            href = "#",
            title = "Review sequences",
            onclick = onclick_js,
            icon("images")
          )
        )
      } else {
        NULL
      }

      list(
        total = card(card_header("Total Detections"), card_body(h2(total_count), review_action)),
        unique = card(card_header("Unique Locations"), card_body(h2(unique_locs))),
        rai = card(card_header("Network RAI"), card_body(h2(sprintf("%.2f", rai))))
      )
    }

    # Helper for activity plot
    generate_activity_plot <- function(sobs) {
      if(nrow(sobs) == 0) return(plot(1, type="n", axes=F, xlab="", ylab="", main="No Data"))
      sobs$hour <- as.numeric(format(sobs$timestamp, "%H"))
      hourly_counts <- sobs %>% dplyr::group_by(hour) %>% dplyr::summarise(count = sum(count, na.rm = TRUE), .groups="drop")
      all_hours <- data.frame(hour = 0:23)
      plot_data <- merge(all_hours, hourly_counts, by="hour", all.x=TRUE)
      plot_data$count[is.na(plot_data$count)] <- 0
      library(ggplot2)
      ggplot(plot_data, aes(x = hour, y = count)) + geom_bar(stat = "identity", fill = "steelblue", color = "black") + coord_polar(start = 0) + scale_x_continuous(breaks = 0:23, limits = c(0, 24), labels = paste0(0:23, ":00")) + theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank(), panel.grid.major.x = element_line(color = "grey80"), plot.title = element_text(hjust = 0.5, face = "bold")) + labs(title = "Detections by Hour of Day")
    }

    # Helper for co-occurrence
    generate_cooccurrence <- function(sobs, full_obs) {
      kiwi_obs <- full_obs %>% dplyr::filter(tolower(scientificName) == "apteryx mantelli")
      if(nrow(kiwi_obs) == 0) return(p("No Kiwi observations in this period."))
      if(nrow(sobs) == 0) return(p("No target species observations to compare."))
      s_deps <- unique(sobs$deploymentID)
      k_deps <- unique(kiwi_obs$deploymentID)
      shared_deps <- intersect(s_deps, k_deps)
      tagList(
        p(sprintf("This species was detected at %d unique deployments.", length(s_deps))),
        p(sprintf("Kiwi were detected at %d unique deployments.", length(k_deps))),
        h4(sprintf("Shared Deployments: %d", length(shared_deps))),
        p("Number of deployments where both this species and Kiwi were detected in the same period.")
      )
    }

    # 1. OVERALL
    overall_obs <- reactive({
      filter_dashboard_obs(core_data$obs)
    })
    overall_deps <- reactive({
      filter_dashboard_deps(core_data$deps)
    })
    overall_sobs <- reactive({
      overall_obs() %>% dplyr::filter(tolower(scientificName) == tolower(species_name))
    })

    output$overall_total_detections_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$total })
    output$overall_unique_locations_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$unique })
    output$overall_rai_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$rai })
    output$overall_activity_plot <- renderPlot({ generate_activity_plot(overall_sobs()) })
    output$overall_cooccurrence_ui <- renderUI({ generate_cooccurrence(overall_sobs(), overall_obs()) })

    period_defaults <- species_dashboard_period_defaults(core_data)

    # Overall RAI plot using plotting_module_server (we map this to overall)
    plotting_module_server(
      id = "overall_rai_plot",
      type = NULL,
      obs = core_data$obs,
      deps = core_data$deps,
      species_override = species_name,
      rai_groups = stats::setNames(list(species_name), vernacular_name),
      rai_norm_hours = config$globals$rai_norm_hours,
      use_net = config$globals$rai_net_count
    )

    # 2. CURRENT PERIOD
    current_period_data <- period_selection_module_server("current_period", period_groups = core_data$period_groups, selected = period_defaults$current_period)
    current_sobs <- reactive({
      req(current_period_data$start_date(), current_period_data$end_date())
      filter_obs(overall_sobs(), current_period_data$start_date(), current_period_data$end_date())
    })
    current_deps <- reactive({
      req(current_period_data$start_date(), current_period_data$end_date())
      filter_deps(overall_deps(), current_period_data$start_date(), current_period_data$end_date())
    })
    current_obs <- reactive({
      filter_obs(overall_obs(), current_period_data$start_date(), current_period_data$end_date())
    })

    output$current_total_detections_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$total })
    output$current_unique_locations_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$unique })
    output$current_rai_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$rai })
    output$current_activity_plot <- renderPlot({ generate_activity_plot(current_sobs()) })
    output$current_cooccurrence_ui <- renderUI({ generate_cooccurrence(current_sobs(), current_obs()) })
    output$current_period_name <- renderText({ current_period_data$period_name() })

    # 3. PRIOR PERIOD
    prior_period_data <- period_selection_module_server("prior_period", period_groups = core_data$period_groups, selected = period_defaults$prior_period)
    prior_sobs <- reactive({
      req(prior_period_data$start_date(), prior_period_data$end_date())
      filter_obs(overall_sobs(), prior_period_data$start_date(), prior_period_data$end_date())
    })
    prior_deps <- reactive({
      req(prior_period_data$start_date(), prior_period_data$end_date())
      filter_deps(overall_deps(), prior_period_data$start_date(), prior_period_data$end_date())
    })
    prior_obs <- reactive({
      filter_obs(overall_obs(), prior_period_data$start_date(), prior_period_data$end_date())
    })

    output$prior_total_detections_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$total })
    output$prior_unique_locations_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$unique })
    output$prior_rai_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$rai })
    output$prior_activity_plot <- renderPlot({ generate_activity_plot(prior_sobs()) })
    output$prior_cooccurrence_ui <- renderUI({ generate_cooccurrence(prior_sobs(), prior_obs()) })
    output$prior_period_name <- renderText({ prior_period_data$period_name() })

    # 4. LAST YEAR
    last_year_period_data <- period_selection_module_server("last_year_period", period_groups = core_data$period_groups, selected = period_defaults$last_year_period)
    ly_sobs <- reactive({
      filter_obs(overall_sobs(), last_year_period_data$start_date(), last_year_period_data$end_date())
    })
    ly_deps <- reactive({
      filter_deps(overall_deps(), last_year_period_data$start_date(), last_year_period_data$end_date())
    })
    ly_obs <- reactive({
      filter_obs(overall_obs(), last_year_period_data$start_date(), last_year_period_data$end_date())
    })

    output$last_year_total_detections_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$total })
    output$last_year_unique_locations_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$unique })
    output$last_year_rai_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$rai })
    output$last_year_activity_plot <- renderPlot({ generate_activity_plot(ly_sobs()) })
    output$last_year_cooccurrence_ui <- renderUI({ generate_cooccurrence(ly_sobs(), ly_obs()) })
    output$last_year_period_name <- renderText({ last_year_period_data$period_name() })

  })
}
