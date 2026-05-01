activity_patterns_module_ui <- function(id,
                                        view = "main",
                                        species_choices = NULL,
                                        selected_species = NULL,
                                        locality_choices = NULL,
                                        selected_localities = NULL) {
  ns <- NS(id)

  if (view == "sidebar") {
    return(
      conditionalPanel(
        condition = "input.nav === 'activity_patterns'",
        mapping_module_ui(
          id = ns("activity_patterns_map"),
          view = "select_species",
          choices = species_choices,
          selected = selected_species,
          multiple = TRUE
        ),
        mapping_module_ui(
          id = ns("activity_patterns_map"),
          view = "select_localities",
          choices = locality_choices,
          selected = selected_localities
        )
      )
    )
  }

  if (view == "main") {
    return(
      nav_panel(
        title = "Activity Patterns",
        icon = icon("clock"),
        value = "activity_patterns",
        h2("Activity Patterns"),
        uiOutput(ns("activity_patterns_locality_heading")),
        navset_tab(
          id = ns("activity_patterns_tabs"),
          selected = "overall",
          nav_panel("Overall", value = "overall", plotOutput(ns("activity_patterns_overall"), height = "560px")),
          nav_panel(title = textOutput(ns("activity_patterns_current_period_name"), inline = TRUE), value = "current_period", plotOutput(ns("activity_patterns_current"), height = "560px")),
          nav_panel(title = textOutput(ns("activity_patterns_prior_period_name"), inline = TRUE), value = "prior_period", plotOutput(ns("activity_patterns_prior"), height = "560px")),
          nav_panel(title = textOutput(ns("activity_patterns_last_year_period_name"), inline = TRUE), value = "last_year_period", plotOutput(ns("activity_patterns_last_year"), height = "560px"))
        )
      )
    )
  }
}


activity_patterns_module_server <- function(id,
                                            core_data,
                                            nav,
                                            current_period,
                                            prior_period,
                                            last_year_period) {
  moduleServer(id, function(input, output, session) {
    activity_patterns_map <- NULL
    activity_patterns_loaded <- reactiveVal(FALSE)

    observeEvent(nav(), {
      if (nav() == "activity_patterns" && !activity_patterns_loaded()) {
        logger::log_debug("activity_patterns_module_server, lazily calling mapping_module_server()")
        activity_patterns_map <<- mapping_module_server(
          id = "activity_patterns_map",
          type = "observation",
          obs = reactive({ core_data$obs }),
          deps = reactive({ core_data$deps }),
          enable_map_outputs = FALSE
        )
        activity_patterns_loaded(TRUE)
      }
    })

    activity_patterns_obs <- reactive({
      req(activity_patterns_loaded(), activity_patterns_map$selected_species(), activity_patterns_map$selected_localities())
      species <- tolower(activity_patterns_map$selected_species())
      localities <- activity_patterns_map$selected_localities()

      core_data$obs %>%
        dplyr::filter(tolower(scientificName) %in% species, locality %in% localities)
    })

    output$activity_patterns_locality_heading <- renderUI({
      req(activity_patterns_loaded(), activity_patterns_map$selected_localities())
      localities <- activity_patterns_map$selected_localities()
      div(
        class = "dashboard-locality-heading",
        paste("Locality selection:", paste(vapply(as.character(localities), locality_display_name, character(1)), collapse = ", "))
      )
    })

    output$activity_patterns_overall <- renderPlot({
      req(activity_patterns_obs())
      generate_multi_species_activity_plot(activity_patterns_obs())
    })

    output$activity_patterns_current <- renderPlot({
      req(activity_patterns_obs(), current_period$start_date(), current_period$end_date())
      period_obs <- filter_obs(activity_patterns_obs(), current_period$start_date(), current_period$end_date())
      generate_multi_species_activity_plot(period_obs)
    })
    output$activity_patterns_current_period_name <- renderText({
      current_period$period_name()
    })

    output$activity_patterns_prior <- renderPlot({
      req(activity_patterns_obs(), prior_period$start_date(), prior_period$end_date())
      period_obs <- filter_obs(activity_patterns_obs(), prior_period$start_date(), prior_period$end_date())
      generate_multi_species_activity_plot(period_obs)
    })
    output$activity_patterns_prior_period_name <- renderText({
      prior_period$period_name()
    })

    output$activity_patterns_last_year <- renderPlot({
      req(activity_patterns_obs(), last_year_period$start_date(), last_year_period$end_date())
      period_obs <- filter_obs(activity_patterns_obs(), last_year_period$start_date(), last_year_period$end_date())
      generate_multi_species_activity_plot(period_obs)
    })
    output$activity_patterns_last_year_period_name <- renderText({
      last_year_period$period_name()
    })
  })
}
