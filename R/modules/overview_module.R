overview_module_ui <- function(id, view = "main", core_data, config) {
  ns <- NS(id)
  overview_tabs_id <- ns("main_overview_tabs")

  if (view == "sidebar") {
    return(
      tagList(
        conditionalPanel(
          condition = sprintf("input.nav === 'overview' && input['%s'] === 'current_period'", overview_tabs_id),
          period_selection_module_ui(
            id = ns("main_overview_current_period"),
            view = "select",
            choices = period_selection_choices(core_data$period_groups, config = config),
            selected = core_data$app$period_defaults$primary_period,
            label = "Current period:"
          )
        ),
        conditionalPanel(
          condition = sprintf("input.nav === 'overview' && input['%s'] === 'prior_period'", overview_tabs_id),
          period_selection_module_ui(
            id = ns("main_overview_prior_period"),
            view = "select",
            choices = period_selection_choices(core_data$period_groups, config = config),
            selected = species_overview_period_defaults(core_data)$prior_period,
            label = "Prior period:"
          )
        ),
        conditionalPanel(
          condition = sprintf("input.nav === 'overview' && input['%s'] === 'last_year_period'", overview_tabs_id),
          period_selection_module_ui(
            id = ns("main_overview_last_year_period"),
            view = "select",
            choices = period_selection_choices(core_data$period_groups, config = config),
            selected = species_overview_period_defaults(core_data)$last_year_period,
            label = "Same period last year:"
          )
        ),
        conditionalPanel(
          condition = "input.nav === 'overview'",
          plotting_module_ui(
            id = ns("overview_rai_plot"),
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
        "Overview",
        value = "overview",
        icon = icon("clipboard-list"),
        h2("Overview"),
        uiOutput(ns("main_overview_locality_heading")),
        navset_tab(
          id = ns("main_overview_tabs"),
          selected = "current_period",

          nav_panel("All Time",
            value = "alltime",
            div(
              class = "overview-page",
              card(
                class = "overview-plot-card",
                card_header(
                  div(
                    class = "overview-card-header-with-controls",
                    div(
                      class = "overview-card-header-title",
                      icon("chart-line"),
                      "RAI history"
                    ),
                    div(
                      class = "overview-card-header-controls",
                      plotting_module_ui(
                        id = ns("overview_rai_plot"),
                        view = "select_rai_group_inline",
                        choices = names(config$globals$rai_groups),
                        selected = intersect(
                          config$globals$overview_rai_history_default_groups,
                          names(config$globals$rai_groups)
                        )
                      ),
                      plotting_module_ui(id = ns("overview_rai_plot"), view = "rai_plot_inline_options")
                    )
                  )
                ),
                div(
                  class = "rai-plot-area-with-info",
                  uiOutput(ns("overview_rai_plot_basis_link"), inline = TRUE),
                  plotting_module_ui(id = ns("overview_rai_plot"), view = "rai_plot")
                ),
                uiOutput(ns("overview_rai_plot_count_basis_footer")),
                full_screen = FALSE
              ),
              div(class = "overview-section-heading", "LATEST IMAGES"),
              uiOutput(ns("overview_favourites_hero")),
              div(class = "overview-section-heading", "WHOLE PROJECT"),
              uiOutput(ns("whole_project_sections"))
            )
          ),

          nav_panel(
            title = textOutput(ns("main_overview_current_period_name"), inline = TRUE),
            value = "current_period",
            div(
              class = "overview-page",
              div(class = "overview-section-heading overview-current-period-heading", "RAI Snapshot"),
              uiOutput(ns("main_overview_current_period_cards")),
              uiOutput(ns("main_overview_current_period_favourite_images")),
              uiOutput(ns("main_overview_current_period_project_work_sections")),
              div(class = "overview-section-heading overview-current-period-heading", "Weather"),
              uiOutput(ns("main_overview_current_period_weather_cards"))
            )
          ),

          nav_panel(
            title = textOutput(ns("main_overview_prior_period_name"), inline = TRUE),
            value = "prior_period",
            div(
              class = "overview-page",
              div(class = "overview-section-heading overview-current-period-heading", "RAI Snapshot"),
              uiOutput(ns("main_overview_prior_period_cards")),
              uiOutput(ns("main_overview_prior_period_favourite_images")),
              uiOutput(ns("main_overview_prior_period_project_work_sections")),
              div(class = "overview-section-heading overview-current-period-heading", "Weather"),
              uiOutput(ns("main_overview_prior_period_weather_cards"))
            )
          ),

          nav_panel(
            title = textOutput(ns("main_overview_last_year_period_name"), inline = TRUE),
            value = "last_year_period",
            div(
              class = "overview-page",
              div(class = "overview-section-heading overview-current-period-heading", "RAI Snapshot"),
              uiOutput(ns("main_overview_last_year_period_cards")),
              uiOutput(ns("main_overview_last_year_period_favourite_images")),
              uiOutput(ns("main_overview_last_year_period_project_work_sections")),
              div(class = "overview-section-heading overview-current-period-heading", "Weather"),
              uiOutput(ns("main_overview_last_year_period_weather_cards"))
            )
          )
        )
      )
    )
  }
}


overview_module_server <- function(id, core_data, config, use_net = reactive(config$globals$use_net_data)) {
  moduleServer(id, function(input, output, session) {
    period_defaults <- species_overview_period_defaults(core_data)
    overview_plot_periods <- period_names_from_index(
      core_data$period_groups,
      period_index = core_data$app$period_defaults$primary_period_index,
      period_name = period_defaults$current_period
    )
    overview_plot_deps <- filter_deps_by_period_names(
      core_data$deps,
      overview_plot_periods,
      NULL,
      NULL,
      period_intervals_for_names(core_data$period_groups, overview_plot_periods)
    )

    main_overview_current_period <- period_selection_module_server("main_overview_current_period", period_groups = core_data$period_groups, selected = period_defaults$current_period)
    main_overview_prior_period <- period_selection_module_server("main_overview_prior_period", period_groups = core_data$period_groups, selected = period_defaults$prior_period)
    main_overview_last_year_period <- period_selection_module_server("main_overview_last_year_period", period_groups = core_data$period_groups, selected = period_defaults$last_year_period)

    output$main_overview_current_period_name <- renderText({ main_overview_current_period$period_name() })
    output$main_overview_prior_period_name <- renderText({ main_overview_prior_period$period_name() })
    output$main_overview_last_year_period_name <- renderText({ main_overview_last_year_period$period_name() })

    overview_selected_localities <- reactive({
      selected_localities <- input[["overview_rai_plot-selected_localities"]]
      if (is.null(selected_localities) || length(selected_localities) == 0) {
        selected_localities <- unique(core_data$deps$locality)
      }

      as.character(selected_localities)
    })

    overview_combine_localities <- reactive({
      combine_localities <- input[["overview_rai_plot-combine_localities"]]
      if (is.null(combine_localities)) {
        return(TRUE)
      }

      isTRUE(combine_localities)
    })

    overview_locality_heading <- reactive({
      selected_localities <- overview_selected_localities()
      if (overview_combine_localities()) {
        locality_scope_label(selected_localities)
      } else {
        paste("Localities:", paste(vapply(selected_localities, locality_display_name, character(1)), collapse = ", "))
      }
    })

    output$main_overview_locality_heading <- renderUI({
      div(class = "overview-locality-heading", overview_locality_heading())
    })

    output$overview_rai_plot_basis_link <- renderUI({
      rai_groups <- input[["overview_rai_plot-selected_rai_group"]]
      if (is.null(rai_groups) || length(rai_groups) == 0) {
        rai_groups <- intersect(
          config$globals$overview_rai_history_default_groups,
          names(config$globals$rai_groups)
        )
        if (length(rai_groups) == 0) {
          rai_groups <- names(config$globals$rai_groups)
        }
      }
      rai_groups <- rai_groups[rai_groups %in% names(config$globals$rai_groups)]

      locality_token <- paste(overview_selected_localities(), collapse = ",")
      tagList(lapply(rai_groups, function(rai_group) {
        tags$span(
          class = "overview-rai-basis-link",
          title = paste(rai_group, "RAI calculation basis"),
          render_overview_info_link(
            paste(rai_group, locality_token, "ALL", sep = "|"),
            session$ns("overview_rai_details_clicked")
          )
        )
      }))
    })

    output$overview_rai_plot_count_basis_footer <- renderUI({
      render_count_basis_footer(use_net())
    })

    output$overview_favourites_hero <- renderUI({
      render_overview_favourites_hero()
    })

    output$whole_project_sections <- renderUI({
      render_overview_whole_project_sections(
        overview_selected_localities(),
        volunteer_detail_input_id = session$ns("overview_volunteer_hours_details_clicked"),
        classifier_info_input_id = session$ns("overview_classifier_info_clicked")
      )
    }) %>%
      bindCache(paste(overview_selected_localities(), collapse = "|"), use_net())

    render_tab_cards <- function(period_name) {
      combine_localities <- overview_combine_localities()
      selected_localities <- overview_selected_localities()

      if (isTRUE(combine_localities)) {
        return(render_overview_rai_cards(
          selected_localities,
          period_name,
          detail_input_id = session$ns("overview_rai_details_clicked")
        ))
      }

      tagList(lapply(selected_localities, function(locality) {
        tagList(
          div(class = "overview-locality-heading", locality_display_name(locality)),
          render_overview_rai_cards(
            locality,
            period_name,
            detail_input_id = session$ns("overview_rai_details_clicked")
          )
        )
      }))
    }

    render_tab_favourite_images <- function(period_name, slider_id) {
      hero <- render_overview_favourites_hero(
        period_name = period_name,
        slider_id = session$ns(slider_id)
      )

      if (is.null(hero)) {
        return(NULL)
      }

      tagList(
        div(class = "overview-section-heading overview-current-period-heading", "FAVOURITE IMAGES"),
        hero
      )
    }

    render_tab_project_work_sections <- function(period_name) {
      combine_localities <- overview_combine_localities()
      selected_localities <- overview_selected_localities()

      if (isTRUE(combine_localities)) {
        return(render_overview_period_project_work_sections(
          selected_localities,
          period_name,
          volunteer_detail_input_id = session$ns("overview_volunteer_hours_details_clicked"),
          classifier_info_input_id = session$ns("overview_classifier_info_clicked")
        ))
      }

      tagList(lapply(selected_localities, function(locality) {
        tagList(
          div(class = "overview-locality-heading", locality_display_name(locality)),
          render_overview_period_project_work_sections(
            locality,
            period_name,
            volunteer_detail_input_id = session$ns("overview_volunteer_hours_details_clicked"),
            classifier_info_input_id = session$ns("overview_classifier_info_clicked")
          )
        )
      }))
    }

    render_tab_weather <- function(period_name) {
      combine_localities <- overview_combine_localities()
      selected_localities <- overview_selected_localities()

      period_info <- period_group_by_name(core_data$period_groups, period_name)
      if (is.null(period_info)) {
        return(NULL)
      }
      start_date <- period_info$start_date
      end_date <- period_info$end_date

      weather_by_locality <- isTRUE(config$globals$overview_weather_by_locality)

      if (isTRUE(combine_localities) || !isTRUE(weather_by_locality)) {
        return(render_weather_cards(
          selected_localities,
          start_date,
          end_date,
          info_input_id = session$ns("overview_weather_details_clicked")
        ))
      }

      tagList(lapply(selected_localities, function(locality) {
        tagList(
          div(class = "overview-locality-heading", locality_display_name(locality)),
          render_weather_cards(
            locality,
            start_date,
            end_date,
            info_input_id = session$ns("overview_weather_details_clicked")
          )
        )
      }))
    }

    output$main_overview_current_period_cards <- renderUI({ render_tab_cards(main_overview_current_period$period_name()) }) %>%
      bindCache(paste(overview_selected_localities(), collapse = "|"), overview_combine_localities(), main_overview_current_period$period_name(), config$globals$rai_norm_hours, use_net())
    output$main_overview_prior_period_cards <- renderUI({ render_tab_cards(main_overview_prior_period$period_name()) }) %>%
      bindCache(paste(overview_selected_localities(), collapse = "|"), overview_combine_localities(), main_overview_prior_period$period_name(), config$globals$rai_norm_hours, use_net())
    output$main_overview_last_year_period_cards <- renderUI({ render_tab_cards(main_overview_last_year_period$period_name()) }) %>%
      bindCache(paste(overview_selected_localities(), collapse = "|"), overview_combine_localities(), main_overview_last_year_period$period_name(), config$globals$rai_norm_hours, use_net())

    output$main_overview_current_period_favourite_images <- renderUI({
      render_tab_favourite_images(main_overview_current_period$period_name(), "main_overview_current_period_favourites_slider")
    })
    output$main_overview_prior_period_favourite_images <- renderUI({
      render_tab_favourite_images(main_overview_prior_period$period_name(), "main_overview_prior_period_favourites_slider")
    })
    output$main_overview_last_year_period_favourite_images <- renderUI({
      render_tab_favourite_images(main_overview_last_year_period$period_name(), "main_overview_last_year_period_favourites_slider")
    })

    output$main_overview_current_period_project_work_sections <- renderUI({ render_tab_project_work_sections(main_overview_current_period$period_name()) }) %>%
      bindCache(paste(overview_selected_localities(), collapse = "|"), overview_combine_localities(), main_overview_current_period$period_name(), use_net())
    output$main_overview_prior_period_project_work_sections <- renderUI({ render_tab_project_work_sections(main_overview_prior_period$period_name()) }) %>%
      bindCache(paste(overview_selected_localities(), collapse = "|"), overview_combine_localities(), main_overview_prior_period$period_name(), use_net())
    output$main_overview_last_year_period_project_work_sections <- renderUI({ render_tab_project_work_sections(main_overview_last_year_period$period_name()) }) %>%
      bindCache(paste(overview_selected_localities(), collapse = "|"), overview_combine_localities(), main_overview_last_year_period$period_name(), use_net())

    output$main_overview_current_period_weather_cards <- renderUI({ render_tab_weather(main_overview_current_period$period_name()) })
    output$main_overview_prior_period_weather_cards <- renderUI({ render_tab_weather(main_overview_prior_period$period_name()) })
    output$main_overview_last_year_period_weather_cards <- renderUI({ render_tab_weather(main_overview_last_year_period$period_name()) })

    show_overview_rai_detail_modal <- function(detail_token) {
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
      show_rai_metric_modal(overview_rai_metric(rai_group, lower_is_better, locality, period_name))
    }

    observeEvent(input$overview_rai_details_clicked, {
      show_overview_rai_detail_modal(input$overview_rai_details_clicked)
    })

    observeEvent(input$overview_weather_details_clicked, {
      token <- input$overview_weather_details_clicked
      show_weather_modal(token$lat, token$lng, token$start_date, token$end_date, token$locality)
    })

    observeEvent(input$overview_volunteer_hours_details_clicked, {
      token <- parse_overview_selection_detail_token(input$overview_volunteer_hours_details_clicked)
      show_overview_volunteer_hours_modal(token$locality, token$period_name)
    })

    observeEvent(input$overview_classifier_info_clicked, {
      show_overview_classifier_info_modal()
    })

    plotting_module_server(
      id = "overview_rai_plot",
      type = NULL,
      obs = filter_detection_obs(core_data$obs),
      deps = overview_plot_deps,
      species_override = NULL,
      rai_groups = config$globals$rai_groups,
      rai_norm_hours = config$globals$rai_norm_hours,
      use_net = use_net
    )

    list(
      current_period = main_overview_current_period,
      prior_period = main_overview_prior_period,
      last_year_period = main_overview_last_year_period,
      show_rai_detail_modal = show_overview_rai_detail_modal
    )
  })
}
