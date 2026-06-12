dashboard_module_ui <- function(id, view = "main", core_data, config) {
  ns <- NS(id)
  dashboard_tabs_id <- ns("main_dashboard_tabs")

  if (view == "sidebar") {
    return(
      tagList(
        conditionalPanel(
          condition = sprintf("input.nav === 'dashboard' && input['%s'] === 'current_period'", dashboard_tabs_id),
          period_selection_module_ui(
            id = ns("main_dashboard_current_period"),
            view = "select",
            choices = period_selection_choices(core_data$period_groups, config = config),
            selected = core_data$app$period_defaults$primary_period,
            label = "Current period:"
          )
        ),
        conditionalPanel(
          condition = sprintf("input.nav === 'dashboard' && input['%s'] === 'prior_period'", dashboard_tabs_id),
          period_selection_module_ui(
            id = ns("main_dashboard_prior_period"),
            view = "select",
            choices = period_selection_choices(core_data$period_groups, config = config),
            selected = species_dashboard_period_defaults(core_data)$prior_period,
            label = "Prior period:"
          )
        ),
        conditionalPanel(
          condition = sprintf("input.nav === 'dashboard' && input['%s'] === 'last_year_period'", dashboard_tabs_id),
          period_selection_module_ui(
            id = ns("main_dashboard_last_year_period"),
            view = "select",
            choices = period_selection_choices(core_data$period_groups, config = config),
            selected = species_dashboard_period_defaults(core_data)$last_year_period,
            label = "Same period last year:"
          )
        ),
        conditionalPanel(
          condition = "input.nav === 'dashboard'",
          plotting_module_ui(
            id = ns("dashboard_rai_plot"),
            view = "select_localities",
            choices = unique(core_data$deps$locality),
            selected = unique(core_data$deps$locality)
          )
        )
      )
    )
  }

  if (view == "main") {
    return(
      nav_panel(
        "Dashboard",
        value = "dashboard",
        icon = icon("dashboard"),
        h2("Results Dashboard"),
        uiOutput(ns("main_dashboard_locality_heading")),
        navset_tab(
          id = ns("main_dashboard_tabs"),
          selected = "current_period",

          nav_panel("Overall",
            value = "overall",
            div(
              class = "dashboard-page",
              card(
                class = "dashboard-plot-card",
                card_header(
                  div(
                    class = "dashboard-card-header-with-controls",
                    div(
                      class = "dashboard-card-header-title",
                      icon("chart-line"),
                      "RAI history"
                    ),
                    div(
                      class = "dashboard-card-header-controls",
                      plotting_module_ui(
                        id = ns("dashboard_rai_plot"),
                        view = "select_rai_group_inline",
                        choices = names(config$globals$rai_groups),
                        selected = intersect(
                          config$globals$dashboard_rai_history_default_groups,
                          names(config$globals$rai_groups)
                        )
                      ),
                      plotting_module_ui(id = ns("dashboard_rai_plot"), view = "rai_plot_inline_options")
                    )
                  )
                ),
                div(
                  class = "rai-plot-area-with-info",
                  uiOutput(ns("dashboard_rai_plot_basis_link"), inline = TRUE),
                  plotting_module_ui(id = ns("dashboard_rai_plot"), view = "rai_plot")
                ),
                uiOutput(ns("dashboard_rai_plot_count_basis_footer")),
                full_screen = FALSE
              ),
              div(class = "dashboard-section-heading", "LATEST IMAGES"),
              uiOutput(ns("dashboard_favourites_hero")),
              div(class = "dashboard-section-heading", "WHOLE PROJECT"),
              uiOutput(ns("whole_project_sections"))
            )
          ),

          nav_panel(
            title = textOutput(ns("main_dashboard_current_period_name"), inline = TRUE),
            value = "current_period",
            div(
              class = "dashboard-page",
              div(class = "dashboard-section-heading dashboard-current-period-heading", "RAI Snapshot"),
              uiOutput(ns("main_dashboard_current_period_cards")),
              uiOutput(ns("main_dashboard_current_period_favourite_images")),
              uiOutput(ns("main_dashboard_current_period_project_work_sections")),
              div(class = "dashboard-section-heading dashboard-current-period-heading", "Weather"),
              uiOutput(ns("main_dashboard_current_period_weather_cards"))
            )
          ),

          nav_panel(
            title = textOutput(ns("main_dashboard_prior_period_name"), inline = TRUE),
            value = "prior_period",
            div(
              class = "dashboard-page",
              div(class = "dashboard-section-heading dashboard-current-period-heading", "RAI Snapshot"),
              uiOutput(ns("main_dashboard_prior_period_cards")),
              uiOutput(ns("main_dashboard_prior_period_favourite_images")),
              uiOutput(ns("main_dashboard_prior_period_project_work_sections")),
              div(class = "dashboard-section-heading dashboard-current-period-heading", "Weather"),
              uiOutput(ns("main_dashboard_prior_period_weather_cards"))
            )
          ),

          nav_panel(
            title = textOutput(ns("main_dashboard_last_year_period_name"), inline = TRUE),
            value = "last_year_period",
            div(
              class = "dashboard-page",
              div(class = "dashboard-section-heading dashboard-current-period-heading", "RAI Snapshot"),
              uiOutput(ns("main_dashboard_last_year_period_cards")),
              uiOutput(ns("main_dashboard_last_year_period_favourite_images")),
              uiOutput(ns("main_dashboard_last_year_period_project_work_sections")),
              div(class = "dashboard-section-heading dashboard-current-period-heading", "Weather"),
              uiOutput(ns("main_dashboard_last_year_period_weather_cards"))
            )
          )
        )
      )
    )
  }
}


dashboard_module_server <- function(id, core_data, config, use_net = reactive(config$globals$use_net_data)) {
  moduleServer(id, function(input, output, session) {
    period_defaults <- species_dashboard_period_defaults(core_data)
    dashboard_plot_periods <- period_names_from_index(
      core_data$period_groups,
      period_index = core_data$app$period_defaults$primary_period_index,
      period_name = period_defaults$current_period
    )
    dashboard_plot_deps <- core_data$deps %>%
      dplyr::filter(as.character(period) %in% dashboard_plot_periods)

    main_dashboard_current_period <- period_selection_module_server("main_dashboard_current_period", period_groups = core_data$period_groups, selected = period_defaults$current_period)
    main_dashboard_prior_period <- period_selection_module_server("main_dashboard_prior_period", period_groups = core_data$period_groups, selected = period_defaults$prior_period)
    main_dashboard_last_year_period <- period_selection_module_server("main_dashboard_last_year_period", period_groups = core_data$period_groups, selected = period_defaults$last_year_period)

    output$main_dashboard_current_period_name <- renderText({ main_dashboard_current_period$period_name() })
    output$main_dashboard_prior_period_name <- renderText({ main_dashboard_prior_period$period_name() })
    output$main_dashboard_last_year_period_name <- renderText({ main_dashboard_last_year_period$period_name() })

    dashboard_selected_localities <- reactive({
      selected_localities <- input[["dashboard_rai_plot-selected_localities"]]
      if (is.null(selected_localities) || length(selected_localities) == 0) {
        selected_localities <- unique(core_data$deps$locality)
      }

      as.character(selected_localities)
    })

    dashboard_combine_localities <- reactive({
      combine_localities <- input[["dashboard_rai_plot-combine_localities"]]
      if (is.null(combine_localities)) {
        return(TRUE)
      }

      isTRUE(combine_localities)
    })

    dashboard_locality_heading <- reactive({
      selected_localities <- dashboard_selected_localities()
      if (dashboard_combine_localities()) {
        locality_scope_label(selected_localities)
      } else {
        paste("Localities:", paste(vapply(selected_localities, locality_display_name, character(1)), collapse = ", "))
      }
    })

    output$main_dashboard_locality_heading <- renderUI({
      div(class = "dashboard-locality-heading", dashboard_locality_heading())
    })

    output$dashboard_rai_plot_basis_link <- renderUI({
      rai_groups <- input[["dashboard_rai_plot-selected_rai_group"]]
      if (is.null(rai_groups) || length(rai_groups) == 0) {
        rai_groups <- intersect(
          config$globals$dashboard_rai_history_default_groups,
          names(config$globals$rai_groups)
        )
        if (length(rai_groups) == 0) {
          rai_groups <- names(config$globals$rai_groups)
        }
      }
      rai_groups <- rai_groups[rai_groups %in% names(config$globals$rai_groups)]

      locality_token <- paste(dashboard_selected_localities(), collapse = ",")
      tagList(lapply(rai_groups, function(rai_group) {
        tags$span(
          class = "dashboard-rai-basis-link",
          title = paste(rai_group, "RAI calculation basis"),
          render_dashboard_info_link(
            paste(rai_group, locality_token, "ALL", sep = "|"),
            session$ns("dashboard_rai_details_clicked")
          )
        )
      }))
    })

    output$dashboard_rai_plot_count_basis_footer <- renderUI({
      render_count_basis_footer(use_net())
    })

    output$dashboard_favourites_hero <- renderUI({
      render_dashboard_favourites_hero()
    })

    output$whole_project_sections <- renderUI({
      render_dashboard_whole_project_sections(
        dashboard_selected_localities(),
        volunteer_detail_input_id = session$ns("dashboard_volunteer_hours_details_clicked"),
        classifier_info_input_id = session$ns("dashboard_classifier_info_clicked")
      )
    }) %>%
      bindCache(paste(dashboard_selected_localities(), collapse = "|"), use_net())

    render_tab_cards <- function(period_name) {
      combine_localities <- dashboard_combine_localities()
      selected_localities <- dashboard_selected_localities()

      if (isTRUE(combine_localities)) {
        return(render_dashboard_rai_cards(
          selected_localities,
          period_name,
          detail_input_id = session$ns("dashboard_rai_details_clicked")
        ))
      }

      tagList(lapply(selected_localities, function(locality) {
        tagList(
          div(class = "dashboard-locality-heading", locality_display_name(locality)),
          render_dashboard_rai_cards(
            locality,
            period_name,
            detail_input_id = session$ns("dashboard_rai_details_clicked")
          )
        )
      }))
    }

    render_tab_favourite_images <- function(period_name, slider_id) {
      hero <- render_dashboard_favourites_hero(
        period_name = period_name,
        slider_id = session$ns(slider_id)
      )

      if (is.null(hero)) {
        return(NULL)
      }

      tagList(
        div(class = "dashboard-section-heading dashboard-current-period-heading", "FAVOURITE IMAGES"),
        hero
      )
    }

    render_tab_project_work_sections <- function(period_name) {
      combine_localities <- dashboard_combine_localities()
      selected_localities <- dashboard_selected_localities()

      if (isTRUE(combine_localities)) {
        return(render_dashboard_period_project_work_sections(
          selected_localities,
          period_name,
          volunteer_detail_input_id = session$ns("dashboard_volunteer_hours_details_clicked"),
          classifier_info_input_id = session$ns("dashboard_classifier_info_clicked")
        ))
      }

      tagList(lapply(selected_localities, function(locality) {
        tagList(
          div(class = "dashboard-locality-heading", locality_display_name(locality)),
          render_dashboard_period_project_work_sections(
            locality,
            period_name,
            volunteer_detail_input_id = session$ns("dashboard_volunteer_hours_details_clicked"),
            classifier_info_input_id = session$ns("dashboard_classifier_info_clicked")
          )
        )
      }))
    }

    render_tab_weather <- function(period_name) {
      combine_localities <- dashboard_combine_localities()
      selected_localities <- dashboard_selected_localities()

      period_info <- period_group_by_name(core_data$period_groups, period_name)
      if (is.null(period_info)) {
        return(NULL)
      }
      start_date <- period_info$start_date
      end_date <- period_info$end_date

      weather_by_locality <- isTRUE(config$globals$dashboard_weather_by_locality)

      if (isTRUE(combine_localities) || !isTRUE(weather_by_locality)) {
        return(render_weather_cards(
          selected_localities,
          start_date,
          end_date,
          info_input_id = session$ns("dashboard_weather_details_clicked")
        ))
      }

      tagList(lapply(selected_localities, function(locality) {
        tagList(
          div(class = "dashboard-locality-heading", locality_display_name(locality)),
          render_weather_cards(
            locality,
            start_date,
            end_date,
            info_input_id = session$ns("dashboard_weather_details_clicked")
          )
        )
      }))
    }

    output$main_dashboard_current_period_cards <- renderUI({ render_tab_cards(main_dashboard_current_period$period_name()) }) %>%
      bindCache(paste(dashboard_selected_localities(), collapse = "|"), dashboard_combine_localities(), main_dashboard_current_period$period_name(), config$globals$rai_norm_hours, use_net())
    output$main_dashboard_prior_period_cards <- renderUI({ render_tab_cards(main_dashboard_prior_period$period_name()) }) %>%
      bindCache(paste(dashboard_selected_localities(), collapse = "|"), dashboard_combine_localities(), main_dashboard_prior_period$period_name(), config$globals$rai_norm_hours, use_net())
    output$main_dashboard_last_year_period_cards <- renderUI({ render_tab_cards(main_dashboard_last_year_period$period_name()) }) %>%
      bindCache(paste(dashboard_selected_localities(), collapse = "|"), dashboard_combine_localities(), main_dashboard_last_year_period$period_name(), config$globals$rai_norm_hours, use_net())

    output$main_dashboard_current_period_favourite_images <- renderUI({
      render_tab_favourite_images(main_dashboard_current_period$period_name(), "main_dashboard_current_period_favourites_slider")
    })
    output$main_dashboard_prior_period_favourite_images <- renderUI({
      render_tab_favourite_images(main_dashboard_prior_period$period_name(), "main_dashboard_prior_period_favourites_slider")
    })
    output$main_dashboard_last_year_period_favourite_images <- renderUI({
      render_tab_favourite_images(main_dashboard_last_year_period$period_name(), "main_dashboard_last_year_period_favourites_slider")
    })

    output$main_dashboard_current_period_project_work_sections <- renderUI({ render_tab_project_work_sections(main_dashboard_current_period$period_name()) }) %>%
      bindCache(paste(dashboard_selected_localities(), collapse = "|"), dashboard_combine_localities(), main_dashboard_current_period$period_name(), use_net())
    output$main_dashboard_prior_period_project_work_sections <- renderUI({ render_tab_project_work_sections(main_dashboard_prior_period$period_name()) }) %>%
      bindCache(paste(dashboard_selected_localities(), collapse = "|"), dashboard_combine_localities(), main_dashboard_prior_period$period_name(), use_net())
    output$main_dashboard_last_year_period_project_work_sections <- renderUI({ render_tab_project_work_sections(main_dashboard_last_year_period$period_name()) }) %>%
      bindCache(paste(dashboard_selected_localities(), collapse = "|"), dashboard_combine_localities(), main_dashboard_last_year_period$period_name(), use_net())

    output$main_dashboard_current_period_weather_cards <- renderUI({ render_tab_weather(main_dashboard_current_period$period_name()) })
    output$main_dashboard_prior_period_weather_cards <- renderUI({ render_tab_weather(main_dashboard_prior_period$period_name()) })
    output$main_dashboard_last_year_period_weather_cards <- renderUI({ render_tab_weather(main_dashboard_last_year_period$period_name()) })

    show_dashboard_rai_detail_modal <- function(detail_token) {
      detail_parts <- strsplit(detail_token, "\\|", fixed = FALSE)[[1]]
      rai_group <- detail_parts[[1]]
      locality <- if (length(detail_parts) > 1 && detail_parts[[2]] != "ALL") {
        strsplit(detail_parts[[2]], ",", fixed = TRUE)[[1]]
      } else {
        NULL
      }
      period_name <- if (length(detail_parts) > 2 && detail_parts[[3]] != "ALL") {
        detail_parts[[3]]
      } else {
        NULL
      }

      lower_is_better <- rai_group %in% c(
        "Mustelids", "Cats", "Rats", "Pigs", "Dogs", "Possums", "Hedgehogs", "Mice"
      )
      show_rai_metric_modal(dashboard_rai_metric(rai_group, lower_is_better, locality, period_name))
    }

    observeEvent(input$dashboard_rai_details_clicked, {
      show_dashboard_rai_detail_modal(input$dashboard_rai_details_clicked)
    })

    observeEvent(input$dashboard_weather_details_clicked, {
      token <- input$dashboard_weather_details_clicked
      show_weather_modal(token$lat, token$lng, token$start_date, token$end_date, token$locality)
    })

    observeEvent(input$dashboard_volunteer_hours_details_clicked, {
      token <- parse_dashboard_selection_detail_token(input$dashboard_volunteer_hours_details_clicked)
      show_dashboard_volunteer_hours_modal(token$locality, token$period_name)
    })

    observeEvent(input$dashboard_classifier_info_clicked, {
      show_dashboard_classifier_info_modal()
    })

    plotting_module_server(
      id = "dashboard_rai_plot",
      type = NULL,
      obs = filter_detection_obs(core_data$obs),
      deps = dashboard_plot_deps,
      species_override = NULL,
      rai_groups = config$globals$rai_groups,
      rai_norm_hours = config$globals$rai_norm_hours,
      use_net = use_net
    )

    list(
      current_period = main_dashboard_current_period,
      prior_period = main_dashboard_prior_period,
      last_year_period = main_dashboard_last_year_period,
      show_rai_detail_modal = show_dashboard_rai_detail_modal
    )
  })
}
