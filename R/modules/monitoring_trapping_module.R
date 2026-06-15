monitoring_trapping_help_link <- function(ns, field) {
  tags$a(
    href = "#",
    class = "dashcard-count-basis-info",
    title = "Information",
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
      ns("help_clicked"),
      field
    ),
    icon("circle-info")
  )
}

monitoring_trapping_label <- function(ns, icon_name, label, help_id) {
  tagList(icon(icon_name), label, monitoring_trapping_help_link(ns, help_id))
}

monitoring_trapping_format_number <- function(value, digits = 0, suffix = "") {
  if (is.null(value) || length(value) == 0 || is.na(value) || !is.finite(value)) {
    return("N/A")
  }

  formatted <- format(round(as.numeric(value), digits), big.mark = ",", nsmall = digits, trim = TRUE)
  paste0(formatted, suffix)
}

monitoring_trapping_metric_card <- function(label, value, detail = NULL) {
  div(
    class = "monitoring-trapping-metric-card",
    div(label, class = "monitoring-trapping-metric-label"),
    div(value, class = "monitoring-trapping-metric-value"),
    if (!is.null(detail) && nzchar(as.character(detail))) {
      div(detail, class = "monitoring-trapping-metric-detail")
    }
  )
}

monitoring_trapping_metric_grid <- function(...) {
  div(class = "monitoring-trapping-metric-grid", ...)
}

trapping_outcomes_module_ui <- function(id,
                                           core_data,
                                           config,
                                           trap_data = NULL,
                                           view = "main") {
  ns <- NS(id)
  locality_choices <- sort(unique(as.character(core_data$deps$locality)))
  period_choices <- period_selection_choices(core_data$period_groups, config = config)
  default_period <- core_data$app$period_defaults$primary_period

  if (identical(view, "sidebar")) {
    return(tagList(
      period_selection_module_ui(
        ns("period"),
        view = "select",
        choices = period_choices,
        selected = default_period,
        label = "Period:",
        multiple = TRUE
      ),
      mapping_module_ui(
        id = ns("outcomes_map"),
        view = "select_species",
        choices = core_data$app$spp_classes,
        selected = c(
          core_data$app$spp_classes[[1]][1],
          core_data$app$spp_classes[[1]][2],
          core_data$app$spp_classes[[1]][3]
        ),
        label = "Species selection:",
        show_combined_species_note = FALSE
      ),
      mapping_module_ui(
        id = ns("outcomes_map"),
        view = "select_localities",
        choices = locality_choices,
        selected = locality_choices,
        label = "Localities:"
      ),
      mapping_module_ui(
        id = ns("outcomes_map"),
        view = "select_map_record_options",
        include_monitoring_records_default = TRUE,
        include_trap_data_default = TRUE,
      ),
      conditionalPanel(
        condition = "input.monitoring_trapping_tabs === 'map' || (input.monitoring_trapping_tabs === 'records' && input.records_tabs === 'map_window_records')",
        ns = ns,
        mapping_module_ui(
          id = ns("outcomes_map"),
          view = "density_timeline_controls",
          include_prediction_option = FALSE,
          include_monitoring_area_option = TRUE,
          include_observation_layer_options = TRUE
        )
      )
    ))
  }

  tagList(
    div(
      class = "map-page-heading",
      h2("Monitoring & Trapping"),
      uiOutput(ns("outcomes_heading"))
    ),
    navset_tab(
      id = ns("monitoring_trapping_tabs"),
      nav_panel(
        "Map",
        value = "map",
        mapping_module_ui(ns("outcomes_map"), view = "map_only")
      ),
      nav_panel(
        "Effort",
        value = "effort",
        uiOutput(ns("period_scope_note_effort")),
        h3("Monitoring effort"),
        uiOutput(ns("monitoring_effort_cards")),
        navset_tab(
          id = ns("monitoring_effort_tabs"),
          nav_panel(
            "Locality",
            value = "locality",
            DT::dataTableOutput(ns("monitoring_effort_table"))
          ),
          nav_panel(
            "Line",
            value = "line",
            DT::dataTableOutput(ns("monitoring_effort_line_table"))
          ),
          nav_panel(
            "Location",
            value = "location",
            DT::dataTableOutput(ns("monitoring_effort_location_table"))
          )
        ),
        br(),
        h3("Trapping effort"),
        uiOutput(ns("trapping_effort_cards")),
        navset_tab(
          id = ns("trapping_effort_tabs"),
          nav_panel(
            "Locality",
            value = "locality",
            DT::dataTableOutput(ns("trapping_effort_locality_table"))
          ),
          nav_panel(
            "Line",
            value = "line",
            DT::dataTableOutput(ns("trapping_effort_table"))
          ),
          nav_panel(
            "Trap",
            value = "trap",
            DT::dataTableOutput(ns("trapping_effort_by_trap_table"))
          )
        )
      ),
      nav_panel(
        "Outcomes",
        value = "outcomes",
        uiOutput(ns("period_scope_note_outcomes")),
        h3("Monitoring outcomes"),
        uiOutput(ns("monitoring_outcomes_cards")),
        br(),
        h4("Monitoring observations by species"),
        DT::dataTableOutput(ns("monitoring_outcomes_species_table")),
        br(),
        h4("Monitoring observations by locality"),
        DT::dataTableOutput(ns("monitoring_outcomes_locality_table")),
        br(),
        h3("Trapping outcomes"),
        uiOutput(ns("trapping_outcomes_cards")),
        br(),
        h4("Trap kills by trapping line"),
        DT::dataTableOutput(ns("trapping_outcomes_table"))
      ),
      nav_panel(
        "Records",
        value = "records",
        navset_tab(
          id = ns("records_tabs"),
          nav_panel(
            "Period Records",
            value = "period_records",
            uiOutput(ns("period_scope_note_records")),
            h3("Monitoring records"),
            DT::dataTableOutput(ns("period_monitoring_records_table")),
            br(),
            h3("Trapping records"),
            DT::dataTableOutput(ns("period_trapping_records_table"))
          ),
          nav_panel(
            "Map Window Records",
            value = "map_window_records",
            uiOutput(ns("map_window_scope_note")),
            mapping_module_ui(ns("outcomes_map"), view = "map_record_data_panel")
          )
        )
      )
    )
  )
}

trapping_outcomes_module_server <- function(id,
                                               core_data,
                                               trap_data,
                                               config,
                                               use_net = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    default_period <- core_data$app$period_defaults$primary_period
    period <- period_selection_module_server(
      id = "period",
      period_groups = core_data$period_groups,
      selected = default_period
    )

    map_start_date <- reactive({
      req(period$start_date())
      period$start_date()
    })

    map_end_date <- reactive({
      req(period$end_date())
      period$end_date()
    })

    map_obs <- reactive({
      start_date <- map_start_date()
      end_date <- map_end_date()
      filter_detection_obs(filter_obs(core_data$obs, start_date, end_date))
    })

    map_deps <- reactive({
      filter_deps(core_data$deps, map_start_date(), map_end_date())
    })

    outcomes_map_state <- mapping_module_server(
      id = "outcomes_map",
      obs = map_obs,
      deps = map_deps,
      period_start_date = map_start_date,
      period_end_date = map_end_date,
      period_intervals = period$period_intervals,
      species_display_mode_override = reactive("separate"),
      timeline_mode = "always",
      use_net = use_net,
      trap_data = reactive(trap_data)
    )

    monitoring_records_enabled <- reactive({
      if (is.null(input[["outcomes_map-include_monitoring_records"]])) {
        TRUE
      } else {
        isTRUE(input[["outcomes_map-include_monitoring_records"]])
      }
    })

    trapping_records_enabled <- reactive({
      if (is.null(input[["outcomes_map-include_trap_data"]])) {
        TRUE
      } else {
        isTRUE(input[["outcomes_map-include_trap_data"]])
      }
    })

    observe({
      if (!isTRUE(monitoring_records_enabled()) &&
          !isTRUE(trapping_records_enabled()) &&
          input$monitoring_trapping_tabs %in% c("effort", "outcomes", "records")) {
        updateTabsetPanel(session, "monitoring_trapping_tabs", selected = "map")
      }
    })


    selected_localities <- reactive({
      values <- input[["outcomes_map-selected_localities"]]
      values <- as.character(values)
      values[!is.na(values) & nzchar(values)]
    })

    selected_period_windows <- reactive({
      intervals <- period$period_intervals()
      if (is.null(intervals) || nrow(intervals) == 0) {
        return(dplyr::tibble(window_start = as.Date(map_start_date()), window_end = as.Date(map_end_date())))
      }

      start_column <- if ("start_date" %in% names(intervals)) "start_date" else "start"
      end_column <- if ("end_date" %in% names(intervals)) "end_date" else "end"
      if (!all(c(start_column, end_column) %in% names(intervals))) {
        return(dplyr::tibble(window_start = as.Date(map_start_date()), window_end = as.Date(map_end_date())))
      }

      intervals %>%
        dplyr::transmute(window_start = as.Date(.data[[start_column]]), window_end = as.Date(.data[[end_column]])) %>%
        dplyr::filter(!is.na(.data$window_start), !is.na(.data$window_end)) %>%
        dplyr::distinct()
    })

    monitoring_rows <- reactive({
      req(monitoring_records_enabled())
      rows <- filter_detection_obs(filter_obs(core_data$obs, map_start_date(), map_end_date()))
      localities <- selected_localities()
      if (length(localities) > 0) {
        rows <- rows %>% dplyr::filter(.data$locality %in% localities)
      }

      selected_species <- tolower(as.character(outcomes_map_state$selected_species()))
      selected_species <- selected_species[!is.na(selected_species) & nzchar(selected_species)]
      if (length(selected_species) > 0) {
        species_values <- if ("scientificName_lower" %in% names(rows)) {
          tolower(as.character(rows$scientificName_lower))
        } else if ("scientificName" %in% names(rows)) {
          tolower(as.character(rows$scientificName))
        } else {
          rep(NA_character_, nrow(rows))
        }
        rows <- rows[species_values %in% selected_species, , drop = FALSE]
      }
      rows
    })

    monitoring_deployments <- reactive({
      req(monitoring_records_enabled())
      deps <- filter_deps_by_period_names(
        core_data$deps,
        period$period_names(),
        map_start_date(),
        map_end_date(),
        period$period_intervals()
      )
      localities <- selected_localities()
      if (length(localities) > 0) {
        deps <- deps %>% dplyr::filter(.data$locality %in% localities)
      }
      deps
    })

    trap_detail_rows <- reactive({
      req(trapping_records_enabled(), trap_data)
      monitoring_trapping_trap_details(
        trap_data = trap_data,
        core_data = core_data,
        start_date = map_start_date(),
        end_date = map_end_date(),
        scientific_names = outcomes_map_state$selected_species(),
        selected_localities = selected_localities(),
        max_locality_distance_km = 1,
        windows = selected_period_windows()
      )
    })

    trap_check_detail_rows <- reactive({
      req(trapping_records_enabled(), trap_data)
      monitoring_trapping_filtered_deployments(
        trap_data = trap_data,
        core_data = core_data,
        start_date = map_start_date(),
        end_date = map_end_date(),
        selected_localities = selected_localities(),
        max_locality_distance_km = 1,
        windows = selected_period_windows()
      )
    })

    trap_inventory_rows <- reactive({
      req(trapping_records_enabled(), trap_data)
      if (is.null(trap_data$deps) || nrow(trap_data$deps) == 0) {
        return(dplyr::tibble())
      }

      rows <- trap_data$deps %>%
        dplyr::mutate(
          trap_code = if ("locationName" %in% names(.)) trimws(.data$locationName) else NA_character_,
          trap_line = if ("deploymentGroups" %in% names(.)) as.character(.data$deploymentGroups) else NA_character_,
          trap_locality = if ("locality" %in% names(.)) as.character(.data$locality) else NA_character_,
          locality_match_type = if ("locality_match_type" %in% names(.)) as.character(.data$locality_match_type) else NA_character_,
          locality_distance_km = suppressWarnings(as.numeric(if ("locality_distance_km" %in% names(.)) .data$locality_distance_km else NA_real_))
        )

      localities <- selected_localities()
      if (length(localities) > 0) {
        rows <- rows %>%
          dplyr::filter(
            .data$trap_locality %in% localities,
            .data$locality_match_type == "within" |
              suppressWarnings(as.numeric(.data$locality_distance_km)) <= 1
          )
      }

      rows %>%
        dplyr::filter(!is.na(.data$locationID)) %>%
        dplyr::distinct(.data$trap_locality, .data$trap_line, .data$trap_code, .data$locationID, .keep_all = TRUE)
    })

    period_scope_ui <- function() {
      req(map_start_date(), map_end_date())
      div(
        class = "timeline-window-readout",
        strong("Selected period:"),
        sprintf(
          "%s to %s. Effort and Outcomes use the full selected period; the timeline controls only affect the Map and Map Window Records.",
          format(map_start_date(), "%d %b %Y"),
          format(map_end_date(), "%d %b %Y")
        )
      )
    }

    output$period_scope_note_effort <- renderUI(period_scope_ui())
    output$period_scope_note_outcomes <- renderUI(period_scope_ui())
    output$period_scope_note_records <- renderUI(period_scope_ui())

    output$map_window_scope_note <- renderUI({
      div(
        class = "timeline-window-readout",
        strong("Map window records:"),
        "These records follow the Map tab timeline slider, time step, and view mode. Use Period Records for the full selected period."
      )
    })

    trapping_line_value <- function(rows) {
      trap_line <- if ("trap_line" %in% names(rows)) as.character(rows$trap_line) else rep(NA_character_, nrow(rows))
      monitoring_line <- if ("monitoring_line" %in% names(rows)) as.character(rows$monitoring_line) else rep(NA_character_, nrow(rows))
      trap_line[is.na(trap_line) | !nzchar(trap_line)] <- monitoring_line[is.na(trap_line) | !nzchar(trap_line)]
      trap_line[is.na(trap_line) | !nzchar(trap_line)] <- "Unspecified trapping line"
      trap_line
    }

    trapping_locality_value <- function(rows) {
      trap_locality <- if ("trap_locality" %in% names(rows)) as.character(rows$trap_locality) else rep(NA_character_, nrow(rows))
      monitoring_locality <- if ("monitoring_locality" %in% names(rows)) as.character(rows$monitoring_locality) else rep(NA_character_, nrow(rows))
      trap_locality[is.na(trap_locality) | !nzchar(trap_locality)] <- monitoring_locality[is.na(trap_locality) | !nzchar(trap_locality)]
      trap_locality[is.na(trap_locality) | !nzchar(trap_locality)] <- "Unknown locality"
      trap_locality
    }

    empty_message_table <- function(message) {
      data.frame(Message = message, check.names = FALSE)
    }

    add_trap_inventory_count <- function(rows, inventory, by) {
      if (nrow(inventory) == 0) {
        rows$Traps <- 0L
        return(rows)
      }

      inventory_counts <- inventory %>%
        dplyr::mutate(
          .trap_locality = trapping_locality_value(.),
          .trap_line = trapping_line_value(.)
        ) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
        dplyr::summarise(Traps = dplyr::n_distinct(.data$locationID), .groups = "drop")

      rows %>%
        dplyr::left_join(inventory_counts, by = by) %>%
        dplyr::mutate(Traps = dplyr::coalesce(.data$Traps, 0L))
    }

    output$outcomes_heading <- renderUI({
      req(map_start_date(), map_end_date())
      latest_trap_date <- monitoring_trapping_latest_trap_date(trap_data)
      tagList(
        tags$p(sprintf(
          "Monitoring and trapping records for the selected monitoring season(s), from %s to %s.",
          format(map_start_date(), "%d %b %Y"),
          format(map_end_date(), "%d %b %Y")
        )),
        tags$small(
          class = "text-muted",
          sprintf(
            "Trap data available through %s.",
            ifelse(is.na(latest_trap_date), "unknown date", format(latest_trap_date, "%d %b %Y"))
          )
        )
      )
    })

    output$monitoring_effort_cards <- renderUI({
      deps <- monitoring_deployments()
      camera_hours <- if ("camera_hours" %in% names(deps)) sum(deps$camera_hours, na.rm = TRUE) else NA_real_
      monitoring_lines <- deps %>%
        dplyr::filter(!is.na(.data$locality), !is.na(.data$line)) %>%
        dplyr::distinct(.data$locality, .data$line) %>%
        nrow()
      monitoring_trapping_metric_grid(
        monitoring_trapping_metric_card("Monitoring lines", monitoring_trapping_format_number(monitoring_lines)),
        monitoring_trapping_metric_card("Camera locations", monitoring_trapping_format_number(dplyr::n_distinct(deps$locationID))),
        monitoring_trapping_metric_card("Camera deployments", monitoring_trapping_format_number(dplyr::n_distinct(deps$deploymentID))),
        monitoring_trapping_metric_card("Camera hours", monitoring_trapping_format_number(camera_hours), paste(monitoring_trapping_format_number(camera_hours / 24, 1), "camera days"))
      )
    })

    output$monitoring_effort_table <- DT::renderDataTable({
      deps <- monitoring_deployments()
      camera_hours <- if ("camera_hours" %in% names(deps)) deps$camera_hours else rep(NA_real_, nrow(deps))
      rows <- deps %>%
        dplyr::mutate(.camera_hours = suppressWarnings(as.numeric(camera_hours))) %>%
        dplyr::group_by(.data$locality) %>%
        dplyr::summarise(
          `Monitoring lines` = dplyr::n_distinct(.data$line),
          `Camera locations` = dplyr::n_distinct(.data$locationID),
          `Camera deployments` = dplyr::n_distinct(.data$deploymentID),
          `Camera hours` = round(sum(.data$.camera_hours, na.rm = TRUE), 1),
          `Camera days` = round(sum(.data$.camera_hours, na.rm = TRUE) / 24, 1),
          .groups = "drop"
        ) %>%
        dplyr::rename(Locality = locality) %>%
        dplyr::arrange(.data$Locality)

      DT::datatable(
        rows,
        rownames = FALSE,
        options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, dom = "tip")
      )
    })

    output$monitoring_effort_line_table <- DT::renderDataTable({
      deps <- monitoring_deployments()
      camera_hours <- if ("camera_hours" %in% names(deps)) deps$camera_hours else rep(NA_real_, nrow(deps))
      rows <- deps %>%
        dplyr::mutate(.camera_hours = suppressWarnings(as.numeric(camera_hours))) %>%
        dplyr::group_by(.data$locality, .data$line) %>%
        dplyr::summarise(
          `Camera locations` = dplyr::n_distinct(.data$locationID),
          `Camera deployments` = dplyr::n_distinct(.data$deploymentID),
          `Camera hours` = round(sum(.data$.camera_hours, na.rm = TRUE), 1),
          `Camera days` = round(sum(.data$.camera_hours, na.rm = TRUE) / 24, 1),
          .groups = "drop"
        ) %>%
        dplyr::rename(Locality = locality, `Monitoring line` = line) %>%
        dplyr::arrange(.data$Locality, .data$`Monitoring line`)

      DT::datatable(
        rows,
        rownames = FALSE,
        options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, dom = "tip")
      )
    })

    output$monitoring_effort_location_table <- DT::renderDataTable({
      deps <- monitoring_deployments()
      camera_hours <- if ("camera_hours" %in% names(deps)) deps$camera_hours else rep(NA_real_, nrow(deps))
      location_names <- if ("locationName" %in% names(deps)) as.character(deps$locationName) else as.character(deps$locationID)
      rows <- deps %>%
        dplyr::mutate(
          .camera_hours = suppressWarnings(as.numeric(camera_hours)),
          .location_name = location_names
        ) %>%
        dplyr::group_by(.data$locality, .data$line, .data$.location_name) %>%
        dplyr::summarise(
          `Camera deployments` = dplyr::n_distinct(.data$deploymentID),
          `Camera hours` = round(sum(.data$.camera_hours, na.rm = TRUE), 1),
          `Camera days` = round(sum(.data$.camera_hours, na.rm = TRUE) / 24, 1),
          .groups = "drop"
        ) %>%
        dplyr::rename(
          Locality = locality,
          `Monitoring line` = line,
          `Camera location` = .location_name
        ) %>%
        dplyr::arrange(.data$Locality, .data$`Monitoring line`, .data$`Camera location`)

      DT::datatable(
        rows,
        rownames = FALSE,
        options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, scrollX = TRUE, dom = "tip")
      )
    })

    output$monitoring_outcomes_cards <- renderUI({
      rows <- monitoring_rows()
      counts <- if ("count" %in% names(rows)) suppressWarnings(as.numeric(rows$count)) else rep(1, nrow(rows))
      counts[is.na(counts) | !is.finite(counts)] <- 1
      species_values <- if ("scientificName" %in% names(rows)) rows$scientificName else rep(NA_character_, nrow(rows))
      monitoring_trapping_metric_grid(
        monitoring_trapping_metric_card("Observation records", monitoring_trapping_format_number(nrow(rows))),
        monitoring_trapping_metric_card("Individuals counted", monitoring_trapping_format_number(sum(counts, na.rm = TRUE))),
        monitoring_trapping_metric_card("Species observed", monitoring_trapping_format_number(dplyr::n_distinct(species_values[!is.na(species_values) & nzchar(as.character(species_values))]))),
        monitoring_trapping_metric_card("Camera locations with observations", monitoring_trapping_format_number(dplyr::n_distinct(rows$locationID)))
      )
    })

    output$monitoring_outcomes_species_table <- DT::renderDataTable({
      rows <- monitoring_rows()
      if (nrow(rows) == 0) {
        return(DT::datatable(empty_message_table("No monitoring observations match the selected period and filters."), rownames = FALSE, options = list(dom = "t")))
      }

      species_column <- config$globals$species_name_type
      if (!species_column %in% names(rows)) {
        species_column <- if ("scientificName" %in% names(rows)) "scientificName" else "scientificName_lower"
      }
      counts <- if ("count" %in% names(rows)) suppressWarnings(as.numeric(rows$count)) else rep(1, nrow(rows))
      counts[is.na(counts) | !is.finite(counts)] <- 1

      table_rows <- rows %>%
        dplyr::mutate(
          .species = as.character(.data[[species_column]]),
          .species = dplyr::if_else(!is.na(.data$.species) & nzchar(.data$.species), .data$.species, "Unknown species"),
          .count = counts
        ) %>%
        dplyr::group_by(.data$.species) %>%
        dplyr::summarise(
          `Observation records` = dplyr::n(),
          `Individuals counted` = sum(.data$.count, na.rm = TRUE),
          Localities = dplyr::n_distinct(.data$locality),
          `Camera locations` = dplyr::n_distinct(.data$locationID),
          .groups = "drop"
        ) %>%
        dplyr::rename(Species = .species) %>%
        dplyr::arrange(dplyr::desc(.data$`Individuals counted`), .data$Species)

      DT::datatable(
        table_rows,
        rownames = FALSE,
        options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, dom = "tip")
      )
    })

    output$monitoring_outcomes_locality_table <- DT::renderDataTable({
      rows <- monitoring_rows()
      if (nrow(rows) == 0) {
        return(DT::datatable(empty_message_table("No monitoring observations match the selected period and filters."), rownames = FALSE, options = list(dom = "t")))
      }

      counts <- if ("count" %in% names(rows)) suppressWarnings(as.numeric(rows$count)) else rep(1, nrow(rows))
      counts[is.na(counts) | !is.finite(counts)] <- 1
      species_values <- if ("scientificName" %in% names(rows)) as.character(rows$scientificName) else as.character(rows$scientificName_lower)

      table_rows <- rows %>%
        dplyr::mutate(.count = counts, .species = species_values) %>%
        dplyr::group_by(.data$locality) %>%
        dplyr::summarise(
          `Observation records` = dplyr::n(),
          `Individuals counted` = sum(.data$.count, na.rm = TRUE),
          `Species observed` = dplyr::n_distinct(.data$.species[!is.na(.data$.species) & nzchar(.data$.species)]),
          `Camera locations` = dplyr::n_distinct(.data$locationID),
          .groups = "drop"
        ) %>%
        dplyr::rename(Locality = locality) %>%
        dplyr::arrange(.data$Locality)

      DT::datatable(
        table_rows,
        rownames = FALSE,
        options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, dom = "tip")
      )
    })

    output$trapping_effort_cards <- renderUI({
      rows <- trap_detail_rows()
      inventory <- trap_inventory_rows()
      monitoring_trapping_metric_grid(
        monitoring_trapping_metric_card("Traps", monitoring_trapping_format_number(dplyr::n_distinct(inventory$locationID))),
        monitoring_trapping_metric_card("Traps checked", monitoring_trapping_format_number(dplyr::n_distinct(rows$locationID))),
        monitoring_trapping_metric_card("Trap checks", monitoring_trapping_format_number(sum(rows$trap_checks, na.rm = TRUE))),
        monitoring_trapping_metric_card("Trap-days", monitoring_trapping_format_number(sum(rows$trap_days, na.rm = TRUE), 1)),
        monitoring_trapping_metric_card("Checks / 100 trap-days", monitoring_trapping_format_number(mean(rows$trap_checks_per_100_trap_days, na.rm = TRUE), 1))
      )
    })

    output$trapping_effort_locality_table <- DT::renderDataTable({
      detail_rows <- trap_detail_rows()
      rows <- detail_rows %>%
        dplyr::mutate(
          .trap_locality = trapping_locality_value(.),
          .trap_line = trapping_line_value(.)
        ) %>%
        dplyr::group_by(.data$.trap_locality) %>%
        dplyr::summarise(
          `Trapping lines` = dplyr::n_distinct(.data$.trap_line),
          `Traps checked` = dplyr::n_distinct(.data$locationID),
          `Trap checks` = sum(.data$trap_checks, na.rm = TRUE),
          `Trap-days` = round(sum(.data$trap_days, na.rm = TRUE), 1),
          `Checks / 100 trap-days` = round(100 * sum(.data$trap_checks, na.rm = TRUE) / sum(.data$trap_days, na.rm = TRUE), 1),
          `Mean days between checks` = round(sum(.data$trap_days, na.rm = TRUE) / sum(.data$trap_checks, na.rm = TRUE), 1),
          .groups = "drop"
        ) %>%
        add_trap_inventory_count(
          trap_inventory_rows() %>% dplyr::mutate(.trap_locality = trapping_locality_value(.)),
          by = ".trap_locality"
        ) %>%
        dplyr::rename(Locality = .trap_locality) %>%
        dplyr::select("Locality", "Trapping lines", "Traps", "Traps checked", dplyr::everything()) %>%
        dplyr::arrange(.data$Locality)

      rows$`Checks / 100 trap-days`[!is.finite(rows$`Checks / 100 trap-days`)] <- NA_real_
      rows$`Mean days between checks`[!is.finite(rows$`Mean days between checks`)] <- NA_real_

      DT::datatable(
        rows,
        rownames = FALSE,
        options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, dom = "tip")
      )
    })

    output$trapping_effort_table <- DT::renderDataTable({
      detail_rows <- trap_detail_rows()
      rows <- detail_rows %>%
        dplyr::mutate(
          .trap_locality = trapping_locality_value(.),
          .trap_line = trapping_line_value(.)
        ) %>%
        dplyr::group_by(.data$.trap_locality, .data$.trap_line) %>%
        dplyr::summarise(
          `Traps checked` = dplyr::n_distinct(.data$locationID),
          `Trap checks` = sum(.data$trap_checks, na.rm = TRUE),
          `Trap-days` = round(sum(.data$trap_days, na.rm = TRUE), 1),
          `Checks / 100 trap-days` = round(100 * sum(.data$trap_checks, na.rm = TRUE) / sum(.data$trap_days, na.rm = TRUE), 1),
          `Mean days between checks` = round(sum(.data$trap_days, na.rm = TRUE) / sum(.data$trap_checks, na.rm = TRUE), 1),
          .groups = "drop"
        ) %>%
        add_trap_inventory_count(
          trap_inventory_rows() %>%
            dplyr::mutate(
              .trap_locality = trapping_locality_value(.),
              .trap_line = trapping_line_value(.)
            ),
          by = c(".trap_locality", ".trap_line")
        ) %>%
        dplyr::rename(Locality = .trap_locality, `Trapping line` = .trap_line) %>%
        dplyr::select("Locality", "Trapping line", "Traps", "Traps checked", dplyr::everything()) %>%
        dplyr::arrange(.data$Locality, .data$`Trapping line`)

      rows$`Checks / 100 trap-days`[!is.finite(rows$`Checks / 100 trap-days`)] <- NA_real_
      rows$`Mean days between checks`[!is.finite(rows$`Mean days between checks`)] <- NA_real_

      DT::datatable(
        rows,
        rownames = FALSE,
        options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, dom = "tip")
      )
    })

    trapping_effort_by_trap_rows <- reactive({
      detail_rows <- trap_detail_rows()
      detail_rows %>%
        dplyr::mutate(
          Locality = trapping_locality_value(.),
          `Trapping line` = trapping_line_value(.),
          Trap = dplyr::if_else(!is.na(.data$trap_code) & nzchar(.data$trap_code), .data$trap_code, as.character(.data$locationID)),
          .locationID = as.character(.data$locationID),
          `First check` = as.character(.data$first_check),
          `Last check` = as.character(.data$last_check),
          `Trap-days` = round(.data$trap_days, 1),
          `Checks / 100 trap-days` = round(.data$trap_checks_per_100_trap_days, 1),
          `Mean days between checks` = round(.data$mean_days_between_checks, 1)
        ) %>%
        dplyr::select(
          "Locality",
          "Trapping line",
          "Trap",
          ".locationID",
          `Trap checks` = "trap_checks",
          "Trap-days",
          "Checks / 100 trap-days",
          "Mean days between checks",
          "First check",
          "Last check"
        ) %>%
        dplyr::arrange(.data$Locality, .data$`Trapping line`, .data$Trap)
    })

    output$trapping_effort_by_trap_table <- DT::renderDataTable({
      rows <- trapping_effort_by_trap_rows() %>%
        dplyr::select(-".locationID")

      DT::datatable(
        rows,
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, scrollX = TRUE, dom = "tip")
      )
    })

    observeEvent(input$trapping_effort_by_trap_table_rows_selected, {
      selected_index <- input$trapping_effort_by_trap_table_rows_selected
      rows <- trapping_effort_by_trap_rows()
      req(length(selected_index) == 1, nrow(rows) >= selected_index)
      selected <- rows[selected_index, , drop = FALSE]

      check_rows <- trap_check_detail_rows() %>%
        dplyr::filter(as.character(.data$locationID) == selected$.locationID[[1]]) %>%
        dplyr::mutate(
          `Prior check` = as.character(.data$prior_check_date),
          `Check date` = as.character(.data$check_date),
          `Covered from` = as.character(.data$overlap_start),
          `Covered to` = as.character(.data$overlap_end),
          `Covered days` = round(.data$overlap_days, 1),
          `Check interval days` = round(suppressWarnings(as.numeric(.data$interval_days)), 1)
        ) %>%
        dplyr::transmute(
          `Check date`,
          `Prior check`,
          `Covered from`,
          `Covered to`,
          `Covered days`,
          `Check interval days`,
          `Check ID` = .data$deploymentID
        ) %>%
        dplyr::arrange(.data$`Check date`)

      if (nrow(check_rows) == 0) {
        check_rows <- empty_message_table("No check records are available for this trap in the selected period.")
      }

      showModal(modalDialog(
        title = sprintf("Trap check records: %s", selected$Trap[[1]]),
        tags$p(sprintf(
          "%s / %s",
          selected$Locality[[1]],
          selected$`Trapping line`[[1]]
        )),
        DT::datatable(
          check_rows,
          rownames = FALSE,
          options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, scrollX = TRUE, dom = "tip")
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    }, ignoreInit = TRUE)

    output$trapping_outcomes_cards <- renderUI({
      rows <- trap_detail_rows()
      total_kills <- sum(rows$any_species_kill_count, na.rm = TRUE)
      selected_kills <- sum(rows$selected_species_kill_count, na.rm = TRUE)
      trap_days <- sum(rows$trap_days, na.rm = TRUE)
      monitoring_trapping_metric_grid(
        monitoring_trapping_metric_card("Total trap kills", monitoring_trapping_format_number(total_kills)),
        monitoring_trapping_metric_card("Selected species kills", monitoring_trapping_format_number(selected_kills)),
        monitoring_trapping_metric_card("Kills / 100 trap-days", monitoring_trapping_format_number(if (trap_days > 0) 100 * total_kills / trap_days else NA_real_, 2)),
        monitoring_trapping_metric_card("Traps with kills", monitoring_trapping_format_number(dplyr::n_distinct(rows$locationID[rows$any_species_kill_count > 0])))
      )
    })

    output$trapping_outcomes_table <- DT::renderDataTable({
      detail_rows <- trap_detail_rows()
      rows <- detail_rows %>%
        dplyr::mutate(
          .trap_locality = trapping_locality_value(.),
          .trap_line = trapping_line_value(.)
        ) %>%
        dplyr::group_by(.data$.trap_locality, .data$.trap_line) %>%
        dplyr::summarise(
          `Traps with kills` = dplyr::n_distinct(.data$locationID[.data$any_species_kill_count > 0]),
          `Total trap kills` = sum(.data$any_species_kill_count, na.rm = TRUE),
          `Selected species kills` = sum(.data$selected_species_kill_count, na.rm = TRUE),
          `Trap-days` = sum(.data$trap_days, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          `Total kills / 100 trap-days` = dplyr::if_else(.data$`Trap-days` > 0, 100 * .data$`Total trap kills` / .data$`Trap-days`, NA_real_),
          `Selected kills / 100 trap-days` = dplyr::if_else(.data$`Trap-days` > 0, 100 * .data$`Selected species kills` / .data$`Trap-days`, NA_real_),
          `Trap-days` = round(.data$`Trap-days`, 1),
          `Total kills / 100 trap-days` = round(.data$`Total kills / 100 trap-days`, 2),
          `Selected kills / 100 trap-days` = round(.data$`Selected kills / 100 trap-days`, 2)
        ) %>%
        dplyr::rename(Locality = .trap_locality, `Trapping line` = .trap_line) %>%
        dplyr::arrange(.data$Locality, .data$`Trapping line`)

      DT::datatable(
        rows,
        rownames = FALSE,
        options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, scrollX = TRUE, dom = "tip")
      )
    })


    output$period_monitoring_records_table <- DT::renderDataTable({
      rows <- monitoring_rows()
      if (nrow(rows) == 0) {
        return(DT::datatable(empty_message_table("No monitoring records match the selected period and filters."), rownames = FALSE, options = list(dom = "t")))
      }

      table_rows <- tryCatch(
        prepare_spec_table_data(
          rows,
          table_id = "period_observations_browse",
          column_help = FALSE
        )$table_data,
        error = function(e) rows
      )

      DT::datatable(
        table_rows,
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 10, searching = TRUE, lengthChange = TRUE, scrollX = TRUE)
      )
    })

    output$period_trapping_records_table <- DT::renderDataTable({
      detail_rows <- trap_detail_rows()
      if (nrow(detail_rows) == 0) {
        return(DT::datatable(empty_message_table("No trapping records match the selected period and filters."), rownames = FALSE, options = list(dom = "t")))
      }

      rows <- detail_rows %>%
        dplyr::mutate(
          Locality = trapping_locality_value(.),
          `Trapping line` = trapping_line_value(.),
          Trap = dplyr::if_else(!is.na(.data$trap_code) & nzchar(.data$trap_code), .data$trap_code, as.character(.data$locationID)),
          `Nearest monitoring line` = as.character(.data$monitoring_line),
          `First check` = as.character(.data$first_check),
          `Last check` = as.character(.data$last_check),
          `Trap-days` = round(.data$trap_days, 1),
          `Checks / 100 trap-days` = round(.data$trap_checks_per_100_trap_days, 1),
          `Mean days between checks` = round(.data$mean_days_between_checks, 1),
          `Selected species kills` = round(.data$selected_species_kill_count, 0),
          `Total trap kills` = round(.data$any_species_kill_count, 0),
          `Selected kills / 100 trap-days` = round(.data$kills_per_100_trap_days_selected_species, 2),
          `Total kills / 100 trap-days` = round(.data$kills_per_100_trap_days_any_species, 2)
        ) %>%
        dplyr::select(
          "Locality",
          "Trapping line",
          "Trap",
          "Nearest monitoring line",
          `Trap checks` = "trap_checks",
          "Trap-days",
          "Checks / 100 trap-days",
          "Mean days between checks",
          "Selected species kills",
          "Total trap kills",
          "Selected kills / 100 trap-days",
          "Total kills / 100 trap-days",
          "First check",
          "Last check"
        ) %>%
        dplyr::arrange(.data$Locality, .data$`Trapping line`, .data$Trap)

      DT::datatable(
        rows,
        rownames = FALSE,
        options = list(pageLength = 10, searching = TRUE, lengthChange = TRUE, scrollX = TRUE)
      )
    })
  })
}


monitoring_trapping_module_ui <- function(id,
                                          core_data,
                                          config,
                                          trap_data = NULL,
                                          view = "main") {
  ns <- NS(id)
  group_choices <- names(config$globals$rai_groups)
  locality_choices <- sort(unique(as.character(core_data$deps$locality)))
  season_choices <- period_names_without_all(core_data$monitoring_period_groups, assignable_only = FALSE)

  if (identical(view, "sidebar")) {
    return(tagList(
      selectInput(
        ns("rai_group"),
        label = tagList(icon("paw"), "Species or group"),
        choices = group_choices,
        selected = if ("Mustelids" %in% group_choices) "Mustelids" else group_choices[[1]]
      ),
      period_selection_module_ui(
        ns("period"),
        view = "select",
        choices = season_choices,
        selected = monitoring_trapping_default_supported_period(core_data, trap_data, after_days = 21),
        label = "Monitoring season(s)",
        multiple = TRUE
      ),
      selectInput(
        ns("localities"),
        label = tagList(icon("location-dot"), "Locality"),
        choices = locality_choices,
        selected = locality_choices,
        multiple = TRUE
      ),
      selectInput(
        ns("lag_window"),
        label = monitoring_trapping_label(ns, "clock", "Trap comparison window", "trap_comparison_window"),
        choices = monitoring_trapping_lag_windows(),
        selected = "next_28"
      ),
      sliderInput(
        ns("trap_distance_km"),
        label = monitoring_trapping_label(ns, "route", "Include traps within km of reserve", "trap_distance"),
        min = 0,
        max = 5,
        value = 1,
        step = 0.25
      ),
      sliderInput(
        ns("high_percentile"),
        label = monitoring_trapping_label(ns, "fire", "Hotspot threshold", "hotspot_threshold"),
        min = 0.5,
        max = 0.9,
        value = 0.6,
        step = 0.05
      ),
      numericInput(
        ns("min_trap_days"),
        label = monitoring_trapping_label(ns, "tachometer-alt", "Minimum trap-days", "minimum_trap_days"),
        value = 25,
        min = 0,
        step = 5
      )
    ))
  }

  tagList(
    navset_tab(
      id = ns("monitoring_trapping_tabs"),
      nav_panel(
        "Mismatch Map",
        value = "mismatch_map",
        div(
          class = "map-page-heading",
          h2("Trapping Analysis"),
          uiOutput(ns("summary_heading"))
        ),
        leaflet::leafletOutput(ns("mismatch_map"), height = "650px"),
        br(),
        h3("Monitoring line summary"),
        DT::dataTableOutput(ns("mismatch_table")),
        br(),
        h3("Trap detail behind the selected window"),
        DT::dataTableOutput(ns("trap_detail_table"))
      ),
      nav_panel(
        "Lag Analysis",
        value = "lag_analysis",
        div(
          class = "map-page-heading",
          h2("Lag Analysis"),
          uiOutput(ns("lag_heading"))
        ),
        uiOutput(ns("lag_message")),
        plotOutput(ns("lag_correlation_plot"), height = "300px"),
        plotOutput(ns("lag_scatter_plot"), height = "420px"),
        DT::dataTableOutput(ns("lag_table"))
      )
    )
  )
}

monitoring_trapping_module_server <- function(id,
                                              core_data,
                                              trap_data,
                                              config,
                                              use_net = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    logger::log_debug(sprintf("monitoring_trapping_module_server, %s moduleServer() running", id))

    period <- period_selection_module_server(
      id = "period",
      period_groups = core_data$monitoring_period_groups,
      selected = monitoring_trapping_default_supported_period(core_data, trap_data, after_days = 21)
    )

    selected_localities <- reactive({
      localities <- as.character(input$localities)
      localities[nzchar(localities)]
    })

    selected_group <- reactive({
      req(input$rai_group)
      as.character(input$rai_group)
    })

    trap_distance_km <- reactive({
      value <- suppressWarnings(as.numeric(input$trap_distance_km))
      if (is.na(value) || value < 0) 1 else value
    })

    mismatch_result <- reactive({
      req(trap_data)
      req(selected_group())
      req(period$start_date(), period$end_date())

      monitoring_trapping_mismatch_summary(
        core_data = core_data,
        trap_data = trap_data,
        period_names = period$period_names(),
        start_date = period$start_date(),
        end_date = period$end_date(),
        rai_groups = config$globals$rai_groups,
        rai_group = selected_group(),
        lag_window = input$lag_window,
        rai_norm_hours = config$globals$rai_norm_hours,
        use_net = isTRUE(use_net()),
        selected_localities = selected_localities(),
        max_locality_distance_km = trap_distance_km(),
        min_trap_days = input$min_trap_days,
        high_percentile = input$high_percentile
      )
    })

    lag_result <- reactive({
      req(trap_data)
      req(selected_group())

      monitoring_trapping_lag_summary(
        core_data = core_data,
        trap_data = trap_data,
        period_groups = core_data$monitoring_period_groups[period$period_names()],
        rai_groups = config$globals$rai_groups,
        rai_group = selected_group(),
        rai_norm_hours = config$globals$rai_norm_hours,
        use_net = isTRUE(use_net()),
        selected_localities = selected_localities(),
        max_locality_distance_km = trap_distance_km()
      )
    })

    show_help_modal <- function(help_id) {
      help <- switch(
        help_id,
        trap_comparison_window = list(
          title = "Trap comparison window",
          body = tagList(
            tags$p("This controls which trap check intervals or trap kills are compared with the selected monitoring season."),
            tags$ul(
              tags$li(tags$strong("Trapping Analysis: "), "same-period or next-window trap checks are compared with monitoring RAI."),
              tags$li(tags$strong("Before/after analysis: "), "trap kills are split into equal before, during, and after monitoring windows. The before/after duration is set in days.")
            )
          )
        ),
        trap_distance = list(
          title = "Trap selection distance",
          body = tagList(
            tags$p("Trap data is already assigned to monitoring reserves during import using the monitoring camera locations."),
            tags$p("This control includes traps assigned within the selected reserve plus nearby traps whose stored locality distance is less than or equal to the selected kilometres."),
            tags$p("The trap detail table shows the traps that make up each line total, including their locality match and distance.")
          )
        ),
        hotspot_threshold = list(
          title = "Hotspot threshold",
          body = tags$p("Relative categories use percentile ranks across the visible monitoring lines. At 0.60, the top 40% of monitoring RAI values are treated as relatively high and the bottom 40% as relatively low. The same threshold is applied to selected-group trap kill-rate ranks.")
        ),
        minimum_trap_days = list(
          title = "Minimum trap-days",
          body = tags$p("Lines with less trapping effort than this are labelled insufficient data. This avoids interpreting low kill rates from tiny amounts of trapping effort as meaningful low trapping response.")
        ),
        trap_days = list(
          title = "Trap-days",
          body = tagList(
            tags$p("Trap-days are the sum of the covered days for trap check intervals in the selected comparison window."),
            tags$p("For example, 10 traps each covering 14 days contributes 140 trap-days. Clicking a trap-days value opens the per-trap breakdown behind that total.")
          )
        ),
        trap_checks_per_100 = list(
          title = "Trap checks per 100 trap-days",
          body = tagList(
            tags$p("This is a trapping effort regularity metric: trap checks divided by trap-days, multiplied by 100."),
            tags$p("A trap checked every 14 days is about 7.1 checks per 100 trap-days. A lower value means traps are being checked less frequently across the selected window.")
          )
        ),
        NULL
      )

      if (is.null(help)) {
        return(NULL)
      }

      showModal(modalDialog(
        title = help$title,
        help$body,
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    }

    observeEvent(input$help_clicked, {
      show_help_modal(input$help_clicked)
    }, ignoreInit = TRUE)

    output$summary_heading <- renderUI({
      result <- mismatch_result()
      latest_trap_date <- monitoring_trapping_latest_trap_date(trap_data)
      tagList(
        tags$p(sprintf(
          "%s monitoring RAI from %s to %s compared with selected-group trap kills per 100 trap-days for %s (%s to %s).",
          selected_group(),
          format(as.Date(period$start_date()), "%d %b %Y"),
          format(as.Date(period$end_date()), "%d %b %Y"),
          result$trap_window$label,
          format(result$trap_window$start_date, "%d %b %Y"),
          format(result$trap_window$end_date, "%d %b %Y")
        )),
        tags$small(
          class = "text-muted",
          sprintf(
            "Trap data available through %s. Categories are relative ranks across the visible monitoring lines, not absolute effectiveness grades.",
            ifelse(is.na(latest_trap_date), "unknown date", format(latest_trap_date, "%d %b %Y"))
          )
        )
      )
    })

    output$lag_heading <- renderUI({
      tags$p(sprintf(
        "%s monitoring RAI by line compared with selected-group trap kills per 100 trap-days for the selected monitoring season(s).",
        selected_group()
      ))
    })

    trap_days_link <- function(value, reserve, line) {
      if (is.na(value) || !is.finite(value) || value <= 0) {
        return("0.0")
      }

      key <- paste(reserve, line, sep = "|")
      sprintf(
        "<a href=\"#\" class=\"trap-days-drilldown\" onclick=\"Shiny.setInputValue('%s', '%s|' + Math.random(), {priority: 'event'}); return false;\">%0.1f</a>",
        session$ns("trap_days_drilldown"),
        htmltools::htmlEscape(key),
        value
      )
    }

    show_trap_days_modal <- function(drilldown_value) {
      parts <- strsplit(as.character(drilldown_value), "\\|", fixed = FALSE)[[1]]
      if (length(parts) < 2) {
        return(NULL)
      }

      reserve <- parts[[1]]
      line <- suppressWarnings(as.integer(parts[[2]]))
      details <- mismatch_result()$trap_details %>%
        dplyr::filter(.data$monitoring_locality == reserve, .data$monitoring_line == line) %>%
        dplyr::transmute(
          Trap = .data$trap_code,
          `Trap line` = .data$trap_line,
          `Trap-days` = round(.data$trap_days, 1),
          Checks = .data$trap_checks,
          `Checks / 100 trap-days` = round(.data$trap_checks_per_100_trap_days, 2),
          `Mean days between checks` = round(.data$mean_days_between_checks, 1),
          `Selected group kills` = round(.data$selected_species_kill_count, 0),
          `All species kills` = round(.data$any_species_kill_count, 0),
          `First covered check` = .data$first_check,
          `Last covered check` = .data$last_check,
          `Locality match` = .data$locality_match_type,
          `Distance km` = round(.data$locality_distance_km, 2)
        ) %>%
        dplyr::arrange(.data$Trap)

      if (nrow(details) == 0) {
        return(showModal(modalDialog(
          title = sprintf("Trap-days breakdown: %s Line %s", reserve, line),
          tags$p("No trap details are available for this line and window."),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l"
        )))
      }

      total_row <- dplyr::tibble(
        Trap = "TOTAL",
        `Trap line` = "",
        `Trap-days` = round(sum(details$`Trap-days`, na.rm = TRUE), 1),
        Checks = sum(details$Checks, na.rm = TRUE),
        `Checks / 100 trap-days` = round(ifelse(sum(details$`Trap-days`, na.rm = TRUE) > 0, 100 * sum(details$Checks, na.rm = TRUE) / sum(details$`Trap-days`, na.rm = TRUE), NA_real_), 2),
        `Mean days between checks` = round(ifelse(sum(details$Checks, na.rm = TRUE) > 0, sum(details$`Trap-days`, na.rm = TRUE) / sum(details$Checks, na.rm = TRUE), NA_real_), 1),
        `Selected group kills` = sum(details$`Selected group kills`, na.rm = TRUE),
        `All species kills` = sum(details$`All species kills`, na.rm = TRUE),
        `First covered check` = as.Date(NA),
        `Last covered check` = as.Date(NA),
        `Locality match` = "",
        `Distance km` = NA_real_
      )

      modal_table <- dplyr::bind_rows(details, total_row)

      showModal(modalDialog(
        title = sprintf("Trap-days breakdown: %s Line %s", reserve, line),
        tags$p("Each row shows the trap contribution to the clicked trap-days total for the selected comparison window."),
        tags$div(
          style = "max-height: 60vh; overflow-y: auto;",
          tags$table(
            class = "table table-sm table-striped",
            tags$thead(tags$tr(lapply(names(modal_table), tags$th))),
            tags$tbody(lapply(seq_len(nrow(modal_table)), function(i) {
              tags$tr(lapply(modal_table[i, ], function(value) tags$td(as.character(value))))
            }))
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "xl"
      ))
    }

    observeEvent(input$trap_days_drilldown, {
      show_trap_days_modal(input$trap_days_drilldown)
    }, ignoreInit = TRUE)

    output$mismatch_map <- leaflet::renderLeaflet({
      result <- mismatch_result()
      map_data <- result$summary %>%
        dplyr::filter(is.finite(.data$latitude), is.finite(.data$longitude))

      pal <- leaflet::colorFactor(
        palette = c(
          "Relatively high monitoring / relatively low trapping" = "#d73027",
          "Relatively high monitoring / relatively high trapping" = "#1a9850",
          "Relatively low monitoring / relatively high trapping" = "#4575b4",
          "Relatively low monitoring / relatively low trapping" = "#91cf60",
          "Mixed signal" = "#fdae61",
          "Insufficient data" = "#969696"
        ),
        domain = c(
          "Relatively high monitoring / relatively low trapping",
          "Relatively high monitoring / relatively high trapping",
          "Relatively low monitoring / relatively high trapping",
          "Relatively low monitoring / relatively low trapping",
          "Mixed signal",
          "Insufficient data"
        )
      )

      map <- leaflet::leaflet(map_data) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

      if (nrow(map_data) == 0) {
        return(map)
      }

      max_rai <- max(map_data$selected_RAI, na.rm = TRUE)
      if (!is.finite(max_rai) || max_rai <= 0) {
        max_rai <- 1
      }

      popup <- sprintf(
        "<strong>%s</strong><br>%s<br>RAI per %s camera-hours: <strong>%0.2f</strong><br>Camera hours: <strong>%0.1f</strong><br>Selected group kills / 100 trap-days: <strong>%s</strong><br>All species kills / 100 trap-days: <strong>%s</strong><br>Trap-days: <strong>%0.1f</strong><br>Trap checks / 100 trap-days: <strong>%s</strong><br>Mean days between checks: <strong>%s</strong><br>Selected group kills: <strong>%0.0f</strong><br>All species kills: <strong>%0.0f</strong><br>Traps included: <strong>%0.0f</strong>",
        htmltools::htmlEscape(map_data$monitoring_label),
        htmltools::htmlEscape(map_data$mismatch_category),
        htmltools::htmlEscape(as.character(config$globals$rai_norm_hours)),
        map_data$selected_RAI,
        map_data$camera_hours,
        ifelse(is.na(map_data$kills_per_100_trap_days_selected_species), "NA", sprintf("%0.2f", map_data$kills_per_100_trap_days_selected_species)),
        ifelse(is.na(map_data$kills_per_100_trap_days_any_species), "NA", sprintf("%0.2f", map_data$kills_per_100_trap_days_any_species)),
        map_data$trap_days,
        ifelse(is.na(map_data$trap_checks_per_100_trap_days), "NA", sprintf("%0.2f", map_data$trap_checks_per_100_trap_days)),
        ifelse(is.na(map_data$mean_days_between_checks), "NA", sprintf("%0.1f", map_data$mean_days_between_checks)),
        map_data$selected_species_kill_count,
        map_data$any_species_kill_count,
        map_data$trap_count
      )

      map %>%
        leaflet::addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = ~pmax(8, 8 + 18 * sqrt(pmax(selected_RAI, 0) / max_rai)),
          fillColor = ~pal(mismatch_category),
          fillOpacity = 0.78,
          stroke = TRUE,
          color = "#222222",
          weight = 1,
          label = ~monitoring_label,
          popup = popup,
          group = "Monitoring lines"
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = ~mismatch_category,
          title = "Relative category",
          opacity = 0.8
        ) %>%
        leaflet::fitBounds(
          lng1 = min(map_data$longitude, na.rm = TRUE),
          lat1 = min(map_data$latitude, na.rm = TRUE),
          lng2 = max(map_data$longitude, na.rm = TRUE),
          lat2 = max(map_data$latitude, na.rm = TRUE)
        )
    })

    mismatch_table_data <- reactive({
      mismatch_result()$summary %>%
        dplyr::mutate(
          trap_days_link = mapply(trap_days_link, .data$trap_days, .data$locality, .data$line, USE.NAMES = FALSE)
        ) %>%
        dplyr::transmute(
          Locality = .data$locality,
          Line = .data$line,
          `Relative category` = .data$mismatch_category,
          `Monitoring RAI` = round(.data$selected_RAI, 2),
          `Camera hours` = round(.data$camera_hours, 1),
          `Selected group kills / 100 trap-days` = round(.data$kills_per_100_trap_days_selected_species, 2),
          `All species kills / 100 trap-days` = round(.data$kills_per_100_trap_days_any_species, 2),
          `Trap-days` = .data$trap_days_link,
          `Trap checks / 100 trap-days` = round(.data$trap_checks_per_100_trap_days, 2),
          `Mean days between checks` = round(.data$mean_days_between_checks, 1),
          `Selected group kills` = round(.data$selected_species_kill_count, 0),
          `All species kills` = round(.data$any_species_kill_count, 0),
          `Trap checks` = .data$trap_checks,
          `Trap count` = .data$trap_count,
          `Relative score` = round(.data$mismatch_score, 2)
        ) %>%
        dplyr::arrange(dplyr::desc(.data$`Relative score`))
    })

    output$mismatch_table <- DT::renderDataTable({
      DT::datatable(
        mismatch_table_data(),
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          order = list(list(14, "desc")),
          dom = "lfrtip"
        )
      )
    })

    output$trap_detail_table <- DT::renderDataTable({
      details <- mismatch_result()$trap_details %>%
        dplyr::transmute(
          Locality = .data$monitoring_locality,
          Line = .data$monitoring_line,
          Trap = .data$trap_code,
          `Trap line` = .data$trap_line,
          `Trap-days` = round(.data$trap_days, 1),
          Checks = .data$trap_checks,
          `Checks / 100 trap-days` = round(.data$trap_checks_per_100_trap_days, 2),
          `Mean days between checks` = round(.data$mean_days_between_checks, 1),
          `Selected group kills` = round(.data$selected_species_kill_count, 0),
          `All species kills` = round(.data$any_species_kill_count, 0),
          `Selected group kills / 100 trap-days` = round(.data$kills_per_100_trap_days_selected_species, 2),
          `First covered check` = .data$first_check,
          `Last covered check` = .data$last_check,
          `Locality match` = .data$locality_match_type,
          `Distance km` = round(.data$locality_distance_km, 2)
        )

      DT::datatable(
        details,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          order = list(list(0, "asc"), list(1, "asc"), list(2, "asc")),
          dom = "lfrtip"
        )
      )
    })

    output$lag_message <- renderUI({
      correlations <- lag_result()$correlations
      usable <- correlations %>%
        dplyr::filter(is.finite(.data$correlation))

      if (nrow(usable) == 0) {
        return(tags$p(
          class = "text-muted",
          "No finite lag correlation is available for the current selection. This usually means there are too few monitoring lines with both RAI and selected-group trap kills, or the values do not vary enough for a correlation."
        ))
      }

      NULL
    })

    output$lag_correlation_plot <- renderPlot({
      correlations <- lag_result()$correlations %>%
        dplyr::filter(is.finite(.data$correlation))
      if (nrow(correlations) == 0) {
        return(NULL)
      }

      ggplot2::ggplot(
        correlations,
        ggplot2::aes(x = stats::reorder(.data$lag_label, .data$lag_days), y = .data$correlation)
      ) +
        ggplot2::geom_col(fill = "#2f6f73", width = 0.65) +
        ggplot2::geom_hline(yintercept = 0, colour = "#555555", linewidth = 0.3) +
        ggplot2::coord_cartesian(ylim = c(-1, 1)) +
        ggplot2::labs(
          x = NULL,
          y = "Correlation",
          title = "RAI vs selected-group trap kill-rate correlation by lag",
          subtitle = "Pearson correlation across monitoring line and selected season combinations"
        ) +
        theme_insightful()
    })

    output$lag_scatter_plot <- renderPlot({
      rows <- lag_result()$rows %>%
        dplyr::filter(.data$lag_key == input$lag_window)
      if (nrow(rows) == 0) {
        return(NULL)
      }

      plot <- ggplot2::ggplot(
        rows,
        ggplot2::aes(
          x = .data$selected_RAI,
          y = .data$kills_per_100_trap_days_selected_species,
          colour = .data$locality
        )
      ) +
        ggplot2::geom_point(size = 2.5, alpha = 0.85) +
        ggplot2::labs(
          x = sprintf("%s RAI per %s camera-hours", selected_group(), config$globals$rai_norm_hours),
          y = "Selected-group trap kills / 100 trap-days",
          colour = "Locality",
          title = sprintf("Selected lag window: %s", names(monitoring_trapping_lag_windows())[monitoring_trapping_lag_windows() == input$lag_window])
        ) +
        theme_insightful()

      finite_rows <- rows %>%
        dplyr::filter(
          is.finite(.data$selected_RAI),
          is.finite(.data$kills_per_100_trap_days_selected_species)
        )
      if (nrow(finite_rows) >= 3 &&
          length(unique(finite_rows$selected_RAI)) > 1 &&
          length(unique(finite_rows$kills_per_100_trap_days_selected_species)) > 1) {
        plot <- plot + ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "#222222", linewidth = 0.6)
      }

      plot
    })

    output$lag_table <- DT::renderDataTable({
      rows <- lag_result()$rows %>%
        dplyr::filter(.data$lag_key == input$lag_window) %>%
        dplyr::transmute(
          Season = .data$period_name,
          Locality = .data$locality,
          Line = .data$line,
          `Monitoring RAI` = round(.data$selected_RAI, 2),
          `Selected group trap kills / 100 trap-days` = round(.data$kills_per_100_trap_days_selected_species, 2),
          `Trap-days` = round(.data$trap_days, 1),
          `Trap checks / 100 trap-days` = round(.data$trap_checks_per_100_trap_days, 2),
          `Mean days between checks` = round(.data$mean_days_between_checks, 1),
          `Trap checks` = .data$trap_checks,
          `Selected group kills` = round(.data$selected_species_kill_count, 0),
          `Trap window start` = .data$trap_window_start,
          `Trap window end` = .data$trap_window_end
        )

      DT::datatable(
        rows,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          order = list(list(0, "desc")),
          dom = "lfrtip"
        )
      )
    })
  })
}
