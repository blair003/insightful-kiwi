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
        uiOutput(ns("overall_metric_cards")),
        br(),
        # Add RAI plot specific to this species
        card(
          class = "dashboard-plot-card",
          card_header(tagList(
            div(
              class = "dashboard-card-header-with-controls",
              div(
                class = "dashboard-card-header-title",
                icon("chart-line"),
                "RAI history"
              ),
              div(
                class = "dashboard-card-header-controls",
                plotting_module_ui(id = ns("overall_rai_plot"), view = "rai_plot_inline_options")
              )
            )
          )),
          div(
            class = "rai-plot-area-with-info",
            uiOutput(ns("overall_rai_plot_basis_link"), inline = TRUE),
            plotting_module_ui(id = ns("overall_rai_plot"), view = "rai_plot")
          ),
          full_screen = FALSE
        ),
        uiOutput(ns("overall_favourite_images")),
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
        ),
        br(),
        card(
          class = "dashboard-plot-card",
          card_header(tagList(icon("map"), "Species Density Map")),
          mapping_module_ui(id = ns("species_density_map_overall"), view = "map"),
          full_screen = FALSE
        )
      ),

      # 2. Prior Period Tab
      nav_panel(
        title = textOutput(ns("current_period_name"), inline = TRUE),
        value = "current_period",
        br(),
        uiOutput(ns("current_metric_cards")),
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
        ),
        br(),
        card(
          class = "dashboard-plot-card",
          card_header(tagList(icon("map"), "Species Density Map")),
          mapping_module_ui(id = ns("species_density_map_current"), view = "map"),
          full_screen = FALSE
        )
      ),

      # 3. Prior Period Tab
      nav_panel(
        title = textOutput(ns("prior_period_name"), inline = TRUE),
        value = "prior_period",
        br(),
        uiOutput(ns("prior_metric_cards")),
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
        ),
        br(),
        card(
          class = "dashboard-plot-card",
          card_header(tagList(icon("map"), "Species Density Map")),
          mapping_module_ui(id = ns("species_density_map_prior"), view = "map"),
          full_screen = FALSE
        )
      ),

      # 4. Last Year Tab
      nav_panel(
        title = textOutput(ns("last_year_period_name"), inline = TRUE),
        value = "last_year_period",
        br(),
        uiOutput(ns("last_year_metric_cards")),
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
        ),
        br(),
        card(
          class = "dashboard-plot-card",
          card_header(tagList(icon("map"), "Species Density Map")),
          mapping_module_ui(id = ns("species_density_map_last_year"), view = "map"),
          full_screen = FALSE
        )
      )
    )
  )
}

species_dashboard_module_server <- function(id,
                                            species_name,
                                            vernacular_name,
                                            obs,
                                            deps,
                                            core_data,
                                            rai_norm_hours = 2000,
                                            initial_rai_detail = NULL) {
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

    rai_groups_for_species <- stats::setNames(list(species_name), vernacular_name)

    selected_locality_label <- function() {
      paste("Locality selection:", paste(vapply(selected_localities(), locality_display_name, character(1)), collapse = ", "))
    }

    combine_localities_selected <- reactive({
      combine_localities <- input[["overall_rai_plot-combine_localities"]]
      is.null(combine_localities) || isTRUE(combine_localities)
    })

    rai_calculation_basis_link <- function(period_name_label, locality_token = locality_filter_token()) {
      onclick_payload <- jsonlite::toJSON(
        list(
          period_name = period_name_label,
          locality = locality_token
        ),
        auto_unbox = TRUE
      )
      onclick_js <- sprintf(
        "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
        ns("species_rai_details_clicked"),
        onclick_payload
      )

      tags$a(
        href = "#",
        class = "dashcard-info-link",
        title = "Show RAI calculation basis",
        onclick = onclick_js,
        icon("circle-info")
      )
    }

    output$overall_rai_plot_basis_link <- renderUI({
      rai_calculation_basis_link("ALL")
    })

    output$overall_favourite_images <- renderUI({
      hero <- render_dashboard_favourites_hero(
        context = "species",
        species = species_name,
        slider_id = ns("overall_favourites_slider")
      )

      if (is.null(hero)) {
        return(NULL)
      }

      tagList(
        div(class = "dashboard-section-heading", "FAVOURITE IMAGES"),
        hero
      )
    })

    period_metric_for_basis <- function(period_name_label, locality_filter) {
      period_obs <- core_data$obs
      period_deps <- core_data$deps
      start_date <- NA
      end_date <- NA

      if (!is.null(period_name_label) &&
          period_name_label %in% names(core_data$period_groups)) {
        period <- core_data$period_groups[[period_name_label]]
        start_date <- period$start_date
        end_date <- period$end_date

        if (period_name_label != "ALL") {
          period_obs <- filter_obs(period_obs, start_date, end_date)
          period_deps <- filter_deps(period_deps, start_date, end_date)
        }
      }

      if (!is.null(locality_filter) && length(locality_filter) > 0) {
        period_obs <- period_obs %>% dplyr::filter(.data$locality %in% !!locality_filter)
        period_deps <- period_deps %>% dplyr::filter(.data$locality %in% !!locality_filter)
      }

      metric <- generate_rai_group_network_metric(
        period_obs,
        period_deps,
        rai_groups_for_species,
        vernacular_name,
        config$globals$rai_norm_hours,
        config$globals$rai_net_count
      )

      c(
        list(
          period = period_name_label,
          start_date = start_date,
          end_date = end_date
        ),
        metric
      )
    }

    show_species_rai_metric_modal <- function(period_name_label, locality_filter) {
      metric <- period_metric_for_basis(period_name_label, locality_filter)

      formula <- if (isTRUE(metric$use_net)) {
        "line RAI = net individuals / camera hours x RAI normalisation"
      } else {
        "line RAI = individuals counted / camera hours x RAI normalisation"
      }

      constant_rows <- list(
        c("Species included", species_name),
        c("RAI normalisation", paste(format_dash_number(metric$rai_norm_hours), "camera hours")),
        c("Duplicate handling", if (isTRUE(metric$use_net)) "RAI uses Net count, excluding possible duplicates" else "RAI uses Total individuals counted, including possible duplicates"),
        c("Locality scope", locality_scope_label(locality_filter))
      )

      result_rows <- list(
        c("RAI", metric$formatted_value),
        c("Period", metric$period),
        c("Start Date", if (is.na(metric$start_date)) "All data" else as.character(metric$start_date)),
        c("End Date", if (is.na(metric$end_date)) "All data" else as.character(metric$end_date)),
        c("Detections", format_dash_number(metric$animal_detections)),
        c("Individuals counted", format_dash_number(metric$individuals_count)),
        c("Possible duplicate individuals", format_dash_number(metric$possible_duplicates_count)),
        c("Net individuals used", format_dash_number(metric$net_individuals_count)),
        c("Camera hours", format_dash_number(metric$camera_hours, 1)),
        c("Localities averaged", format_dash_number(metric$locality_count)),
        c("Locality-line records", format_dash_number(metric$line_count))
      )

      line_values <- metric$line_rai_values
      locality_values <- metric$locality_rai_values
      line_table <- if (!is.null(line_values) && nrow(line_values) > 0) {
        tbody_rows <- list()

        localities <- unique(line_values$locality)
        for (locality in localities) {
          locality_lines <- line_values[line_values$locality == locality, , drop = FALSE]

          # Add line rows
          for (i in seq_len(nrow(locality_lines))) {
            row <- locality_lines[i, ]
            tbody_rows <- append(tbody_rows, list(tags$tr(
              tags$td(locality_display_name(row$locality)),
              tags$td(row$line),
              tags$td(row$formatted_value),
              tags$td(format_dash_number(row$individuals_count)),
              tags$td(format_dash_number(row$possible_duplicates_count)),
              tags$td(format_dash_number(row$net_individuals_count)),
              tags$td(format_dash_number(row$camera_hours, 1))
            )))
          }

          # Add locality subtotal row
          locality_row <- if (!is.null(locality_values) && nrow(locality_values) > 0) {
            locality_values[locality_values$locality == locality, , drop = FALSE]
          } else {
            data.frame()
          }

          locality_rai <- if (nrow(locality_row) > 0 && !is.na(locality_row$formatted_value[[1]])) {
            locality_row$formatted_value[[1]]
          } else {
            "N/A"
          }

          tbody_rows <- append(tbody_rows, list(tags$tr(
            class = "fw-bold", style = "border-top: 2px solid #dee2e6; border-bottom: 2px solid #dee2e6;",
            tags$td(locality_display_name(locality)),
            tags$td("Locality RAI"),
            tags$td(locality_rai),
            tags$td(""),
            tags$td(""),
            tags$td(""),
            tags$td("")
          )))
        }

        tags$table(
          class = "table table-sm table-striped rai-detail-table",
          tags$thead(tags$tr(
            tags$th("Locality"),
            tags$th("Line"),
            tags$th("Line/Locality RAI"),
            tags$th("Individuals"),
            tags$th("Duplicates"),
            tags$th("Net individuals"),
            tags$th("Camera hours")
          )),
          tags$tbody(tbody_rows)
        )
      } else {
        tags$p("Line RAI values are not available.", class = "rai-line-empty")
      }
      render_key_value_table <- function(rows) {
        tags$table(
          class = "table table-sm table-striped rai-detail-table",
          tags$tbody(lapply(rows, function(row) {
            tags$tr(tags$th(row[[1]]), tags$td(row[[2]]))
          }))
        )
      }
      render_rai_proof_actions <- function() {
        tags$div(
          class = "rai-proof-actions",
          tags$span("Hint: copy or send the proof text below to verify the calculations.", class = "rai-proof-hint"),
          tags$div(
            class = "rai-proof-action-buttons",
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-secondary",
              onclick = "copyRaiProof(this)",
              title = "Copy RAI proof text",
              tags$i(class = "fa fa-copy"),
              " Copy proof"
            ),
            tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-secondary",
              onclick = "verifyRaiProof('chatgpt', this)",
              title = "Open ChatGPT with this proof text",
              tags$i(class = "fa fa-arrow-up-right-from-square"),
              " Verify with ChatGPT"
            )
          )
        )
      }

      detail_token <- paste(period_name_label, paste(locality_filter, collapse = ","), sep = "|")
      share_btn <- tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-secondary",
        onclick = sprintf("copySpeciesRaiBasisUrl(%s, this)", jsonlite::toJSON(detail_token, auto_unbox = TRUE)),
        title = "Share this view",
        icon("share-nodes"),
        " Share"
      )

      modal_title_text <- paste(str_to_title(vernacular_name), "RAI calculation basis")
      if (!is.null(locality_filter) && length(locality_filter) > 0) {
        modal_title_text <- paste(modal_title_text, "-", locality_scope_label(locality_filter))
      }

      showModal(modalDialog(
        title = tagList(
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: flex-start; width: 100%; padding-right: 20px; gap: 12px;",
            tags$div(modal_title_text),
            share_btn
          )
        ),
        tags$p(
          "Each locality-line first gets a line RAI: ",
          tags$code(formula),
          ". Line values are then averaged to locality RAI (mRAI), then to network RAI (mmRAI) when multiple localities are selected."
        ),
        tags$h5("Constant Settings"),
        tags$div(class = "rai-table-scroll", render_key_value_table(constant_rows)),
        tags$h5("Results and Inputs"),
        tags$div(class = "rai-table-scroll", render_key_value_table(result_rows)),
        tags$details(
          class = "rai-detail-section",
          open = "open",
          tags$summary("Line and Locality RAIs"),
          tags$div(class = "rai-table-scroll", line_table)
        ),
        tags$h5("RAI Proof"),
        render_rai_proof_actions(),

        tags$pre(
          class = "rai-calculation-trace",
          if (!is.null(metric$calculation_trace) && !is.na(metric$calculation_trace)) {
            metric$calculation_trace
          } else {
            "Calculation trace is not available."
          }
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    }

    show_species_rai_detail_token <- function(detail_token) {
      detail_parts <- strsplit(detail_token, "\\|", fixed = FALSE)[[1]]
      period_name_label <- if (length(detail_parts) > 0 && nzchar(detail_parts[[1]])) {
        detail_parts[[1]]
      } else {
        "ALL"
      }
      locality_token <- if (length(detail_parts) > 1) {
        detail_parts[[2]]
      } else {
        ""
      }
      localities <- if (nzchar(locality_token)) {
        strsplit(locality_token, ",", fixed = TRUE)[[1]]
      } else {
        selected_localities()
      }

      show_species_rai_metric_modal(period_name_label, localities)
    }

    observeEvent(input$species_rai_details_clicked, {
      action_data <- input$species_rai_details_clicked
      locality_filter <- action_data$locality
      localities <- if (!is.null(locality_filter) && nzchar(locality_filter)) {
        strsplit(locality_filter, ",", fixed = TRUE)[[1]]
      } else {
        selected_localities()
      }

      show_species_rai_metric_modal(action_data$period_name, localities)
    })

    if (!is.null(initial_rai_detail) && nzchar(initial_rai_detail)) {
      session$onFlushed(function() {
        show_species_rai_detail_token(initial_rai_detail)
      }, once = TRUE)
    }

    # Dashboard Header
    output$dashboard_header <- renderUI({
      tagList(
        h2(sprintf("%s Dashboard", str_to_title(vernacular_name))),
        h4(em(species_name)),
        div(class = "dashboard-locality-heading", selected_locality_label())
      )
    })

    generate_rai_card <- function(species_obs, deps_data, period_name_label, title, subtitle, locality_token = locality_filter_token()) {
      metric <- generate_rai_group_network_metric(
        species_obs,
        deps_data,
        rai_groups_for_species,
        vernacular_name,
        rai_norm_hours,
        config$globals$rai_net_count
      )
      formatted_rai <- ifelse(is.na(metric$value), "N/A", metric$formatted_value)

      card(
        card_header(title),
        card_body(
          div(
            class = "dashcard-metric-state dashcard-metric-state-plain",
            div(class = "dashcard-card-action", rai_calculation_basis_link(period_name_label, locality_token)),
            div(formatted_rai, class = "dashcard-output"),
            div(subtitle, class = "dashcard-period")
          )
        )
      )
    }

    generate_rai_cards <- function(species_obs, deps_data, period_name_label) {
      if (combine_localities_selected()) {
        return(list(generate_rai_card(
          species_obs,
          deps_data,
          period_name_label,
          "RAI ± SE",
          "combined for selected localities"
        )))
      }

      lapply(selected_localities(), function(locality) {
        locality_obs <- species_obs %>% dplyr::filter(.data$locality == !!locality)
        locality_deps <- deps_data %>% dplyr::filter(.data$locality == !!locality)
        generate_rai_card(
          locality_obs,
          locality_deps,
          period_name_label,
          paste("RAI ± SE", locality_display_name(locality)),
          "locality only",
          locality
        )
      })
    }

    # Helper to generate the cards
    render_metric_cards <- function(species_obs, deps_data, period_name_label) {
      total_count <- sum(species_obs$count, na.rm = TRUE)
      unique_locs <- length(unique(species_obs$locationName))
      total_deployments <- length(unique(deps_data$locationName))
      pct_locations <- if (total_deployments > 0) (unique_locs / total_deployments) * 100 else 0

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

      total_detections_count <- nrow(species_obs)

      total_card <- card(
          card_header("Total Detections"),
          card_body(
            render_dashcard_metric_body(
              total_detections_count,
              div(sprintf("for %d individuals", total_count), class = "dashcard-period"),
              review_action
            )
          )
        )
      unique_card <- card(
          card_header("Unique Locations"),
          card_body(
            render_dashcard_metric_body(
              unique_locs,
              div(sprintf("%.1f%% of locations", pct_locations), class = "dashcard-period")
            )
          )
      )

      do.call(
        layout_column_wrap,
        c(
          list(width = "250px", total_card, unique_card),
          generate_rai_cards(species_obs, deps_data, period_name_label)
        )
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
      plot_data$hour_midpoint <- plot_data$hour + 0.5
      library(ggplot2)
      ggplot(plot_data, aes(x = hour_midpoint, y = count)) + geom_bar(stat = "identity", fill = "steelblue", color = "black") + coord_polar(start = 0) + scale_x_continuous(breaks = 0:23 + 0.5, limits = c(0, 24), labels = paste0(0:23, ":00")) + theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank(), panel.grid.major.x = element_line(color = "grey80"), plot.title = element_text(hjust = 0.5, face = "bold")) + labs(title = "Detections by Hour of Day")
    }

    # Helper for co-occurrence
    generate_cooccurrence <- function(sobs, full_obs) {
      kiwi_obs <- full_obs %>% dplyr::filter(tolower(scientificName) == "apteryx mantelli")
      if(nrow(kiwi_obs) == 0) return(p("No Kiwi observations in this period."))
      if(nrow(sobs) == 0) return(p("No target species observations to compare."))
      s_deps <- unique(sobs$deploymentID)
      k_deps <- unique(kiwi_obs$deploymentID)
      shared_deps <- intersect(s_deps, k_deps)

      near_miss_minutes <- config$globals$kiwi_near_miss_minutes
      if (is.null(near_miss_minutes) || is.na(near_miss_minutes)) {
        near_miss_minutes <- 60
      }

      near_miss_ui <- NULL
      if (tolower(species_name) != "apteryx mantelli" &&
          length(shared_deps) > 0 &&
          all(c("deploymentID", "observationID", "sequenceID", "timestamp") %in% names(sobs)) &&
          all(c("deploymentID", "observationID", "sequenceID", "timestamp") %in% names(kiwi_obs))) {

        target_pairs <- sobs %>%
          dplyr::filter(.data$deploymentID %in% shared_deps) %>%
          dplyr::select(
            deploymentID,
            target_observationID = observationID,
            target_sequenceID = sequenceID,
            target_timestamp = timestamp,
            target_locationName = locationName
          )

        kiwi_pairs <- kiwi_obs %>%
          dplyr::filter(.data$deploymentID %in% shared_deps) %>%
          dplyr::select(
            deploymentID,
            kiwi_observationID = observationID,
            kiwi_sequenceID = sequenceID,
            kiwi_timestamp = timestamp
          )

        joined_pairs <- suppressWarnings(
          dplyr::inner_join(target_pairs, kiwi_pairs, by = "deploymentID")
        )

        near_misses <- joined_pairs %>%
          dplyr::filter(.data$target_observationID != .data$kiwi_observationID) %>%
          dplyr::mutate(
            minutes_apart = abs(as.numeric(difftime(.data$target_timestamp, .data$kiwi_timestamp, units = "mins"))),
            first_observationID = ifelse(.data$target_timestamp <= .data$kiwi_timestamp, .data$target_observationID, .data$kiwi_observationID),
            second_observationID = ifelse(.data$target_timestamp <= .data$kiwi_timestamp, .data$kiwi_observationID, .data$target_observationID)
          ) %>%
          dplyr::filter(!is.na(.data$minutes_apart), .data$minutes_apart <= near_miss_minutes) %>%
          dplyr::arrange(.data$minutes_apart, .data$target_timestamp, .data$kiwi_timestamp) %>%
          dplyr::distinct(.data$target_sequenceID, .data$kiwi_sequenceID, .keep_all = TRUE)

        if (nrow(near_misses) > 0) {
          near_miss_rows <- near_misses %>%
            dplyr::slice_head(n = 10)

          near_miss_ui <- tagList(
            h4(sprintf("Near Misses Within %s Minutes: %d", format_dash_number(near_miss_minutes), nrow(near_misses))),
            tags$table(
              class = "table table-sm table-striped",
              tags$thead(tags$tr(
                tags$th("Location"),
                tags$th("Target Time"),
                tags$th("Kiwi Time"),
                tags$th("Minutes Apart"),
                tags$th("")
              )),
              tags$tbody(lapply(seq_len(nrow(near_miss_rows)), function(i) {
                row <- near_miss_rows[i, ]
                observation_ids <- unname(c(row$first_observationID, row$second_observationID))
                onclick_payload <- jsonlite::toJSON(
                  list(observation_ids = observation_ids),
                  auto_unbox = TRUE
                )
                onclick_js <- sprintf(
                  "Shiny.setInputValue('review_sequences_click', %s, {priority: 'event'}); return false;",
                  onclick_payload
                )

                tags$tr(
                  tags$td(row$target_locationName),
                  tags$td(format(row$target_timestamp, "%d %b %Y %H:%M")),
                  tags$td(format(row$kiwi_timestamp, "%d %b %Y %H:%M")),
                  tags$td(sprintf("%0.1f", row$minutes_apart)),
                  tags$td(tags$a(href = "#", onclick = onclick_js, icon("images"), " View"))
                )
              }))
            ),
            if (nrow(near_misses) > nrow(near_miss_rows)) {
              p(sprintf("Showing the 10 closest near misses of %d total.", nrow(near_misses)))
            }
          )
        } else {
          near_miss_ui <- p(sprintf("No Kiwi detections were within %s minutes of this species at the same deployment.", format_dash_number(near_miss_minutes)))
        }
      }

      tagList(
        p(sprintf("This species was detected at %d unique deployments.", length(s_deps))),
        p(sprintf("Kiwi were detected at %d unique deployments.", length(k_deps))),
        h4(sprintf("Shared Deployments: %d", length(shared_deps))),
        p("Number of deployments where both this species and Kiwi were detected in the same period."),
        near_miss_ui
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

    output$overall_metric_cards <- renderUI({ render_metric_cards(overall_sobs(), overall_deps(), "ALL") })
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
      rai_groups = rai_groups_for_species,
      rai_norm_hours = config$globals$rai_norm_hours,
      use_net = config$globals$rai_net_count
    )

    # Track loaded map tabs for lazy loading
    loaded_dashboard_tabs <- reactiveVal(character())

    observeEvent(input$dashboard_tabs, {
      req(input$dashboard_tabs)
      current_tab <- input$dashboard_tabs

      if (!(current_tab %in% loaded_dashboard_tabs())) {

        if (current_tab == "overall") {
          # Overall Density Map
          mapping_module_server(
            id = "species_density_map_overall",
            type = "density",
            obs = overall_obs,
            deps = overall_deps,
            species_override = reactive(species_name),
            localities_override = selected_localities
          )
        } else if (current_tab == "current_period") {
          # Current Period Density Map
          mapping_module_server(
            id = "species_density_map_current",
            type = "density",
            obs = current_obs,
            deps = current_deps,
            species_override = reactive(species_name),
            localities_override = selected_localities
          )
        } else if (current_tab == "prior_period") {
          # Prior Period Density Map
          mapping_module_server(
            id = "species_density_map_prior",
            type = "density",
            obs = prior_obs,
            deps = prior_deps,
            species_override = reactive(species_name),
            localities_override = selected_localities
          )
        } else if (current_tab == "last_year_period") {
          # Last Year Period Density Map
          mapping_module_server(
            id = "species_density_map_last_year",
            type = "density",
            obs = ly_obs,
            deps = ly_deps,
            species_override = reactive(species_name),
            localities_override = selected_localities
          )
        }

        # Add to loaded list
        loaded_dashboard_tabs(c(loaded_dashboard_tabs(), current_tab))
        logger::log_debug(sprintf("species_dashboard_module_server, lazily loaded map for tab %s", current_tab))
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

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

    output$current_metric_cards <- renderUI({ render_metric_cards(current_sobs(), current_deps(), current_period_data$period_name()) })
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

    output$prior_metric_cards <- renderUI({ render_metric_cards(prior_sobs(), prior_deps(), prior_period_data$period_name()) })
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

    output$last_year_metric_cards <- renderUI({ render_metric_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name()) })
    output$last_year_activity_plot <- renderPlot({ generate_activity_plot(ly_sobs()) })
    output$last_year_cooccurrence_ui <- renderUI({ generate_cooccurrence(ly_sobs(), ly_obs()) })
    output$last_year_period_name <- renderText({ last_year_period_data$period_name() })

  })
}
