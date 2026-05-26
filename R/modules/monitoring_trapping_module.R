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

monitoring_trapping_module_ui <- function(id,
                                          core_data,
                                          config,
                                          trap_data = NULL,
                                          view = "main") {
  ns <- NS(id)
  group_choices <- names(config$globals$rai_groups)
  locality_choices <- sort(unique(as.character(core_data$deps$locality)))
  season_choices <- period_names_without_all(core_data$period_groups, assignable_only = FALSE)

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
        selected = core_data$app$period_defaults$primary_period,
        label = "Monitoring season(s)",
        multiple = TRUE
      ),
      selectInput(
        ns("localities"),
        label = tagList(icon("location-dot"), "Reserve"),
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
          h2("Monitoring vs Trapping"),
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
      period_groups = core_data$period_groups,
      selected = core_data$app$period_defaults$primary_period
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
        period_groups = core_data$period_groups[period$period_names()],
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
            tags$p("This controls which trap check intervals are compared with the selected monitoring season."),
            tags$ul(
              tags$li(tags$strong("Same monitoring period: "), "trap checks whose covered interval overlaps the monitoring dates."),
              tags$li(tags$strong("Next windows: "), "trap checks after the monitoring period, ending 2, 4, 8, or 12 weeks after the monitoring period ends.")
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
          body = tags$p("Mismatch categories use percentile ranks across the visible monitoring lines. At 0.60, the top 40% of monitoring RAI values are high and the bottom 40% are low. The same threshold is applied to selected-group trap kill-rate ranks.")
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
      tags$p(sprintf(
        "%s monitoring RAI from %s to %s compared with selected-group trap kills per 100 trap-days for %s (%s to %s).",
        selected_group(),
        format(as.Date(period$start_date()), "%d %b %Y"),
        format(as.Date(period$end_date()), "%d %b %Y"),
        result$trap_window$label,
        format(result$trap_window$start_date, "%d %b %Y"),
        format(result$trap_window$end_date, "%d %b %Y")
      ))
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
          "High monitoring / low trapping" = "#d73027",
          "High monitoring / high trapping" = "#1a9850",
          "Low monitoring / high trapping" = "#4575b4",
          "Low monitoring / low trapping" = "#91cf60",
          "Mixed signal" = "#fdae61",
          "Insufficient data" = "#969696"
        ),
        domain = c(
          "High monitoring / low trapping",
          "High monitoring / high trapping",
          "Low monitoring / high trapping",
          "Low monitoring / low trapping",
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
          title = "Mismatch category",
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
          Reserve = .data$locality,
          Line = .data$line,
          Category = .data$mismatch_category,
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
          `Mismatch score` = round(.data$mismatch_score, 2)
        ) %>%
        dplyr::arrange(dplyr::desc(.data$`Mismatch score`))
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
          Reserve = .data$monitoring_locality,
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
          colour = "Reserve",
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
          Reserve = .data$locality,
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
