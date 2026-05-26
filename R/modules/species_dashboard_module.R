species_dashboard_period_defaults <- function(core_data) {
  period_names <- period_names_without_all(core_data$period_groups)
  fallback_period <- if (length(period_names) > 0) {
    period_names[[1]]
  } else if (length(names(core_data$period_groups)) > 0) {
    names(core_data$period_groups)[[1]]
  } else {
    NA_character_
  }

  current_period <- core_data$app$period_defaults$primary_period
  current_period <- if (length(current_period) > 0 && !is.na(current_period[[1]]) && nzchar(as.character(current_period[[1]]))) {
    as.character(current_period[[1]])
  } else {
    fallback_period
  }
  current_period_index <- match(current_period, period_names)

  prior_period <- if (length(current_period_index) == 1 && !is.na(current_period_index) && current_period_index < length(period_names)) {
    period_names[[current_period_index + 1]]
  } else {
    current_period
  }

  last_year_period <- find_matching_prior_year_period(current_period, core_data$period_groups)
  if (length(last_year_period) != 1 || is.na(last_year_period)) {
    comparative_period <- core_data$app$period_defaults$comparative_period
    last_year_period <- if (length(comparative_period) > 0 && !is.na(comparative_period[[1]]) && nzchar(as.character(comparative_period[[1]]))) {
      as.character(comparative_period[[1]])
    } else {
      prior_period
    }
  }

  list(
    current_period = current_period,
    prior_period = prior_period,
    last_year_period = last_year_period
  )
}

species_dashboard_diel_levels <- c("Matutinal", "Diurnal", "Vespertine", "Nocturnal")

species_dashboard_diel_colours <- c(
  Matutinal = "#f0ad4e",
  Diurnal = "#2b8cbe",
  Vespertine = "#d95f0e",
  Nocturnal = "#4b5563"
)

species_dashboard_diel_thresholds <- function(core_data = NULL) {
  thresholds <- core_data$app$diel_thresholds
  if (is.null(thresholds) && exists("config", inherits = TRUE)) {
    thresholds <- core_data_diel_thresholds(get("config", inherits = TRUE))
  }

  normalise_species_dashboard_diel_thresholds(thresholds)
}

species_dashboard_empty_diel_summary <- function(reason = "No data") {
  data.frame(
    diel_class = species_dashboard_diel_levels,
    observations = 0,
    effort_hours = 0,
    rate = 0,
    rate_share = 0,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      classification = "Insufficient data",
      meaning = reason,
      sample_size = 0,
      confidence = "insufficient",
      note = reason
    )
}

species_dashboard_diel_period_intervals <- function(daylight_row) {
  timezone <- if (exists("daylight_timezone", mode = "function", inherits = TRUE)) daylight_timezone() else weather_playback_timezone()
  day_start <- as.POSIXct(as.Date(daylight_row$date), tz = timezone)
  day_end <- day_start + 24 * 60 * 60

  sunrise <- daylight_row$sunrise[[1]]
  sunset <- daylight_row$sunset[[1]]
  civil_dawn <- daylight_row$civil_dawn[[1]]
  civil_dusk <- daylight_row$civil_dusk[[1]]

  if (is.na(sunrise) || is.na(sunset) || sunset <= sunrise) {
    return(NULL)
  }
  if (is.na(civil_dawn) || civil_dawn > sunrise) {
    civil_dawn <- sunrise
  }
  if (is.na(civil_dusk) || civil_dusk < sunset) {
    civil_dusk <- sunset
  }

  data.frame(
    diel_class = c("Nocturnal", "Matutinal", "Diurnal", "Vespertine", "Nocturnal"),
    interval_start = as.POSIXct(c(day_start, civil_dawn, sunrise, sunset, civil_dusk), origin = "1970-01-01", tz = timezone),
    interval_end = as.POSIXct(c(civil_dawn, sunrise, sunset, civil_dusk, day_end), origin = "1970-01-01", tz = timezone),
    stringsAsFactors = FALSE
  )
}

species_dashboard_diel_effort_hours <- function(deps_data, environment_daily, period_start = NULL, period_end = NULL) {
  empty_effort <- stats::setNames(rep(0, length(species_dashboard_diel_levels)), species_dashboard_diel_levels)
  if (is.null(deps_data) || nrow(deps_data) == 0 ||
      is.null(environment_daily) || nrow(environment_daily) == 0 ||
      !all(c("locationID", "start", "end") %in% names(deps_data)) ||
      !all(c("locationID", "date", "sunrise", "sunset", "civil_dawn", "civil_dusk") %in% names(environment_daily))) {
    return(empty_effort)
  }

  timezone <- if (exists("daylight_timezone", mode = "function", inherits = TRUE)) daylight_timezone() else weather_playback_timezone()
  period_start <- if (is.null(period_start) || is.na(period_start)) {
    as.POSIXct(NA, tz = timezone)
  } else {
    as.POSIXct(period_start, tz = timezone)
  }
  period_end <- if (is.null(period_end) || is.na(period_end)) {
    as.POSIXct(NA, tz = timezone)
  } else {
    as.POSIXct(period_end, tz = timezone)
  }

  effort_hours <- empty_effort
  environment_daily <- environment_daily %>%
    dplyr::filter(.data$locationID %in% unique(deps_data$locationID))

  for (dep_index in seq_len(nrow(deps_data))) {
    deployment <- deps_data[dep_index, ]
    dep_start <- as.POSIXct(deployment$start[[1]], tz = timezone)
    dep_end <- as.POSIXct(deployment$end[[1]], tz = timezone)

    if (!is.na(period_start)) {
      dep_start <- max(dep_start, period_start, na.rm = TRUE)
    }
    if (!is.na(period_end)) {
      dep_end <- min(dep_end, period_end, na.rm = TRUE)
    }
    if (is.na(dep_start) || is.na(dep_end) || dep_end <= dep_start) {
      next
    }

    deployment_daylight <- environment_daily %>%
      dplyr::filter(
        .data$locationID == deployment$locationID[[1]],
        .data$date >= as.Date(dep_start, tz = timezone),
        .data$date <= as.Date(dep_end, tz = timezone)
      )

    if (nrow(deployment_daylight) == 0) {
      next
    }

    for (daylight_index in seq_len(nrow(deployment_daylight))) {
      intervals <- species_dashboard_diel_period_intervals(deployment_daylight[daylight_index, ])
      if (is.null(intervals) || nrow(intervals) == 0) {
        next
      }

      intervals$overlap_start <- pmax(intervals$interval_start, dep_start)
      intervals$overlap_end <- pmin(intervals$interval_end, dep_end)
      intervals$hours <- pmax(
        0,
        as.numeric(difftime(intervals$overlap_end, intervals$overlap_start, units = "hours"))
      )

      interval_hours <- tapply(intervals$hours, intervals$diel_class, sum, na.rm = TRUE)
      effort_hours[names(interval_hours)] <- effort_hours[names(interval_hours)] + interval_hours
    }
  }

  effort_hours[species_dashboard_diel_levels]
}

species_dashboard_classify_diel_activity <- function(rate_share, sample_size, thresholds = species_dashboard_diel_thresholds()) {

  if (sample_size < thresholds$insufficient_n) {
    return(list(
      classification = "Insufficient data",
      meaning = "Too few observations",
      confidence = "insufficient"
    ))
  }

  day_share <- rate_share[["Diurnal"]]
  night_share <- rate_share[["Nocturnal"]]
  dawn_share <- rate_share[["Matutinal"]]
  dusk_share <- rate_share[["Vespertine"]]
  crepuscular_share <- dawn_share + dusk_share
  confidence <- if (sample_size < thresholds$normal_confidence_n) "low" else "normal"

  if (!is.na(day_share) && day_share >= thresholds$dominant_share) {
    classification <- "Diurnal"
    meaning <- "Day-active"
  } else if (!is.na(night_share) && night_share >= thresholds$dominant_share) {
    classification <- "Nocturnal"
    meaning <- "Night-active"
  } else if (!is.na(crepuscular_share) &&
             crepuscular_share >= thresholds$crepuscular_share &&
             dawn_share >= thresholds$crepuscular_component_share &&
             dusk_share >= thresholds$crepuscular_component_share) {
    classification <- "Crepuscular"
    meaning <- "Dawn/dusk-active"
  } else if (!is.na(day_share) && !is.na(night_share) &&
             day_share >= thresholds$cathemeral_day_night_share &&
             night_share >= thresholds$cathemeral_day_night_share) {
    classification <- "Cathemeral"
    meaning <- "Active intermittently day and night"
  } else {
    classification <- "Arrhythmic"
    meaning <- "No clear diel rhythm"
  }

  list(
    classification = classification,
    meaning = meaning,
    confidence = confidence
  )
}

species_dashboard_diel_summary <- function(species_obs, deps_data, environment_daily, period_start = NULL, period_end = NULL, thresholds = species_dashboard_diel_thresholds()) {
  if (is.null(species_obs) || nrow(species_obs) == 0 || !"diel_class" %in% names(species_obs)) {
    return(species_dashboard_empty_diel_summary("No observations in this selection"))
  }

  valid_obs <- species_obs %>%
    dplyr::filter(.data$diel_class %in% species_dashboard_diel_levels)
  sample_size <- nrow(valid_obs)
  if (sample_size == 0) {
    return(species_dashboard_empty_diel_summary("No classified diel observations in this selection"))
  }

  observation_counts <- table(factor(valid_obs$diel_class, levels = species_dashboard_diel_levels))
  effort_hours <- species_dashboard_diel_effort_hours(deps_data, environment_daily, period_start, period_end)
  has_effort <- any(effort_hours > 0, na.rm = TRUE)

  rates <- if (has_effort) {
    ifelse(effort_hours > 0, as.numeric(observation_counts) / effort_hours, 0)
  } else {
    as.numeric(observation_counts)
  }
  names(rates) <- species_dashboard_diel_levels

  rate_total <- sum(rates, na.rm = TRUE)
  rate_share <- if (rate_total > 0) rates / rate_total else stats::setNames(rep(0, length(rates)), names(rates))
  classification_info <- species_dashboard_classify_diel_activity(rate_share, sample_size, thresholds)
  confidence_note <- dplyr::case_when(
    identical(classification_info$confidence, "insufficient") ~ sprintf(
      "Fewer than %d observations in this selection.",
      thresholds$insufficient_n
    ),
    identical(classification_info$confidence, "low") ~ sprintf(
      "Low confidence: %d observations. Treat this seasonal classification as tentative.",
      sample_size
    ),
    TRUE ~ sprintf(
      "Based on %d observations and effort-normalised diel rates.",
      sample_size
    )
  )

  if (!has_effort) {
    confidence_note <- paste(confidence_note, "Deployment-hour normalization was unavailable, so observation share was used.")
  }

  data.frame(
    diel_class = species_dashboard_diel_levels,
    observations = as.integer(observation_counts),
    effort_hours = as.numeric(effort_hours),
    rate = as.numeric(rates),
    rate_share = as.numeric(rate_share),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      classification = classification_info$classification,
      meaning = classification_info$meaning,
      sample_size = sample_size,
      confidence = classification_info$confidence,
      note = confidence_note
    )
}

species_dashboard_possible_duplicate_queue <- function(species_obs) {
  if (is.null(species_obs) || nrow(species_obs) == 0 ||
      !"possible_duplicate" %in% names(species_obs) ||
      !"observationID" %in% names(species_obs) ||
      !"timestamp" %in% names(species_obs)) {
    return(character())
  }

  location_field <- if ("locationID" %in% names(species_obs)) "locationID" else "locationName"
  if (!location_field %in% names(species_obs)) {
    return(character())
  }

  queue_rows <- list()
  obs_data <- species_obs %>%
    dplyr::filter(!is.na(.data$timestamp), nzchar(.data$observationID)) %>%
    dplyr::mutate(.location_key = as.character(.data[[location_field]])) %>%
    dplyr::arrange(.data$.location_key, .data$timestamp, .data$observationID)

  if (nrow(obs_data) == 0) {
    return(character())
  }

  grouped_obs <- split(obs_data, obs_data$.location_key)
  for (location_obs in grouped_obs) {
    original_observation_id <- NA_character_

    for (row_index in seq_len(nrow(location_obs))) {
      row <- location_obs[row_index, ]
      is_duplicate <- isTRUE(row$possible_duplicate[[1]])

      if (!is_duplicate) {
        original_observation_id <- as.character(row$observationID[[1]])
        next
      }

      duplicate_time <- row$timestamp[[1]]
      duplicate_id <- as.character(row$observationID[[1]])

      if (!is.na(original_observation_id) && nzchar(original_observation_id)) {
        queue_rows <- c(queue_rows, list(data.frame(
          observationID = original_observation_id,
          queue_time = duplicate_time,
          role_order = 1,
          stringsAsFactors = FALSE
        )))
      }

      queue_rows <- c(queue_rows, list(data.frame(
        observationID = duplicate_id,
        queue_time = duplicate_time,
        role_order = 2,
        stringsAsFactors = FALSE
      )))
    }
  }

  if (length(queue_rows) == 0) {
    return(character())
  }

  dplyr::bind_rows(queue_rows) %>%
    dplyr::arrange(.data$queue_time, .data$role_order, .data$observationID) %>%
    dplyr::distinct(.data$observationID, .keep_all = TRUE) %>%
    dplyr::pull(.data$observationID)
}

species_dashboard_observations_table_data <- function(species_obs) {
  table_fields <- c(
    "locality",
    "line",
    "locationName",
    "period",
    "timestamp",
    "diel_class",
    config$globals$species_name_type,
    "count",
    "possible_duplicate",
    "observationID"
  )

  if (is.null(species_obs) || nrow(species_obs) == 0) {
    return(data.frame())
  }

  display_fields <- intersect(table_fields, names(species_obs))
  if (length(display_fields) == 0) {
    return(data.frame())
  }

  table_data <- species_obs %>%
    dplyr::arrange(dplyr::desc(.data$timestamp)) %>%
    dplyr::select(dplyr::all_of(display_fields))

  prepare_table_data(
    table_data,
    table_id = "observations_browse",
    fields = names(table_data),
    column_help = FALSE
  )$table_data
}

species_dashboard_module_ui <- function(id) {
  ns <- NS(id)

  species_dashboard_period_ui <- function(prefix, title, value, include_rai = FALSE, include_favourites = FALSE) {
    nav_panel(
      title = title,
      value = value,
      br(),
      navset_tab(
        id = ns(paste0(prefix, "_section_tabs")),
        nav_panel(
          "Summary",
          value = "summary",
          br(),
          uiOutput(ns(paste0(prefix, "_metric_cards"))),
          br(),
          if (isTRUE(include_rai)) {
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
              uiOutput(ns("overall_rai_plot_count_basis_footer")),
              full_screen = FALSE
            )
          },
          if (isTRUE(include_favourites)) {
            uiOutput(ns("overall_favourite_images"))
          }
        ),
        nav_panel(
          "Behaviour",
          value = "behaviour",
          br(),
          div(class = "dashboard-section-heading", "BEHAVIOURAL ANALYSIS"),
          layout_column_wrap(
            width = 1/3,
            card(
              card_header("Activity Pattern (Time of Day)"),
              plotOutput(ns(paste0(prefix, "_activity_plot")), height = "400px"),
              uiOutput(ns(paste0(prefix, "_activity_plot_count_basis_footer")))
            ),
            uiOutput(ns(paste0(prefix, "_diel_activity_card"))),
            card(
              card_header("Co-occurrence with Kiwi"),
              uiOutput(ns(paste0(prefix, "_cooccurrence_ui")))
            )
          )
        ),
        nav_panel(
          "Observations",
          value = "observations",
          br(),
          card(
            class = "species-observations-card",
            card_header(
              div(
                class = "dashboard-card-header-with-controls",
                div(
                  class = "dashboard-card-header-title",
                  icon("table"),
                  "Animal Observations"
                ),
                div(
                  class = "dashboard-card-header-controls",
                  uiOutput(ns(paste0(prefix, "_duplicate_review_control")))
                )
              )
            ),
            DT::DTOutput(ns(paste0(prefix, "_observations_table"))),
            uiOutput(ns(paste0(prefix, "_observations_table_count_basis_footer")))
          )
        ),
        nav_panel(
          "Map",
          value = "map",
          br(),
          card(
            class = "dashboard-plot-card",
            card_header(uiOutput(ns(paste0("species_density_map_", prefix, "_header")))),
            mapping_module_ui(id = ns(paste0("species_density_map_", prefix)), view = "map"),
            full_screen = FALSE
          )
        )
      )
    )
  }

  tagList(
    uiOutput(ns("dashboard_header")),

    navset_tab(
      id = ns("dashboard_tabs"),
      species_dashboard_period_ui("overall", "Overall", "overall", include_rai = TRUE, include_favourites = TRUE),
      species_dashboard_period_ui("current", textOutput(ns("current_period_name"), inline = TRUE), "current_period"),
      species_dashboard_period_ui("prior", textOutput(ns("prior_period_name"), inline = TRUE), "prior_period"),
      species_dashboard_period_ui("last_year", textOutput(ns("last_year_period_name"), inline = TRUE), "last_year_period")
    )
  )
}

species_dashboard_module_server <- function(id,
                                            species_name,
                                            vernacular_name,
                                            obs,
                                            deps,
                                            core_data,
                                            rai_norm_hours = config$globals$rai_norm_hours,
                                            use_net = reactive(config$globals$use_net_data),
                                            initial_rai_detail = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    diel_threshold_config <- species_dashboard_diel_thresholds(core_data)

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

    render_species_count_basis_footer <- function() {
      render_count_basis_footer(use_net())
    }

    render_species_density_map_header <- function() {
      title <- if (isTRUE(use_net())) {
        "Species Density Map (net data used)"
      } else {
        "Species Density Map"
      }

      tagList(
        icon("map"),
        title,
        if (isTRUE(use_net())) {
          tags$a(
            href = "#",
            class = "dashcard-count-basis-info",
            title = "Count basis",
            onclick = "Shiny.setInputValue('info_field_clicked', 'Net Data Basis', {priority: 'event'}); return false;",
            icon("circle-info")
          )
        }
      )
    }

    output$overall_rai_plot_count_basis_footer <- renderUI({ render_species_count_basis_footer() })
    output$overall_activity_plot_count_basis_footer <- renderUI({ render_species_count_basis_footer() })
    output$current_activity_plot_count_basis_footer <- renderUI({ render_species_count_basis_footer() })
    output$prior_activity_plot_count_basis_footer <- renderUI({ render_species_count_basis_footer() })
    output$last_year_activity_plot_count_basis_footer <- renderUI({ render_species_count_basis_footer() })
    output$overall_observations_table_count_basis_footer <- renderUI({ render_species_observations_table_note() })
    output$current_observations_table_count_basis_footer <- renderUI({ render_species_observations_table_note() })
    output$prior_observations_table_count_basis_footer <- renderUI({ render_species_observations_table_note() })
    output$last_year_observations_table_count_basis_footer <- renderUI({ render_species_observations_table_note() })
    output$species_density_map_overall_header <- renderUI({ render_species_density_map_header() })
    output$species_density_map_current_header <- renderUI({ render_species_density_map_header() })
    output$species_density_map_prior_header <- renderUI({ render_species_density_map_header() })
    output$species_density_map_last_year_header <- renderUI({ render_species_density_map_header() })

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
      period_obs <- filter_detection_obs(core_data$obs)
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

      metric <- generate_rai_group_network_metric_cached(
        period_obs,
        period_deps,
        rai_groups_for_species,
        vernacular_name,
        rai_norm_hours,
        use_net(),
        cache_context = paste("species_basis", species_name, period_name_label, paste(locality_filter, collapse = ","), sep = "|")
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
        c("End Date", if (is.na(metric$end_date)) "All data" else as.character(metric$end_date))
      )

      input_rows <- list(
        c("Detections", format_dash_number(metric$animal_detections)),
        c("Individuals counted", format_dash_number(metric$individuals_count)),
        c("Possible duplicate individuals", format_dash_number(metric$possible_duplicates_count))
      )

      if (isTRUE(metric$use_net)) {
        input_rows <- c(input_rows, list(c("Net individuals used", format_dash_number(metric$net_individuals_count))))
      }

      input_rows <- c(input_rows, list(
        c("Camera hours", format_dash_number(metric$camera_hours, 1)),
        c("Localities averaged", format_dash_number(metric$locality_count)),
        c("Locality-line records", format_dash_number(metric$line_count))
      ))

      render_key_value_table <- function(rows) {
        tags$table(
          class = "table table-sm table-striped rai-detail-table",
          tags$tbody(lapply(rows, function(row) {
            tags$tr(tags$th(row[[1]]), tags$td(row[[2]]))
          }))
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

      source_rows <- rai_metric_source_rows(
        period_metric = metric,
        period_label = "Selected",
        rai_group = vernacular_name,
        species_included = species_name,
        scope_label = locality_scope_label(locality_filter)
      )
      csv_link <- rai_basis_csv_download_link(
        source_rows,
        rai_basis_filename(paste(vernacular_name, metric$period, "rai source data"))
      )

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
        tags$h5("Period Results"),
        tags$div(class = "rai-table-scroll", render_key_value_table(result_rows)),
        tags$details(
          class = "rai-detail-section",
          tags$summary("Period Inputs"),
          tags$div(class = "rai-table-scroll", render_key_value_table(input_rows))
        ),
        tags$div(class = "rai-source-download-row", csv_link),
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

    diel_info_link <- function() {
      tags$a(
        href = "#",
        class = "dashcard-header-info-link",
        title = "Diel activity classification",
        onclick = sprintf("Shiny.setInputValue('%s', true, {priority: 'event'}); return false;", ns("species_diel_info_clicked")),
        icon("circle-info")
      )
    }

    render_diel_card_header <- function() {
      div(
        class = "dashcard-header-row",
        tags$span(tagList(icon("clock"), "Diel Activity"), class = "dashcard-header-title"),
        diel_info_link()
      )
    }

    render_diel_distribution_bar <- function(summary_data) {
      segments <- lapply(seq_len(nrow(summary_data)), function(i) {
        row <- summary_data[i, ]
        share <- row$rate_share
        width <- if (is.na(share) || share <= 0) 0 else share * 100
        tags$div(
          class = "diel-activity-bar-segment",
          style = sprintf(
            "width: %.4f%%; background-color: %s;",
            width,
            species_dashboard_diel_colours[[row$diel_class]]
          ),
          title = sprintf(
            "%s: %.1f%% of effort-normalised rate",
            row$diel_class,
            share * 100
          )
        )
      })

      div(class = "diel-activity-bar", segments)
    }

    render_diel_legend <- function(summary_data) {
      div(
        class = "diel-activity-legend",
        lapply(seq_len(nrow(summary_data)), function(i) {
          row <- summary_data[i, ]
          div(
            class = "diel-activity-legend-item",
            tags$span(
              class = "diel-activity-legend-swatch",
              style = sprintf("background-color: %s;", species_dashboard_diel_colours[[row$diel_class]])
            ),
            tags$span(row$diel_class),
            tags$span(sprintf("%.0f%%", row$rate_share * 100), class = "diel-activity-legend-value")
          )
        })
      )
    }

    render_diel_activity_card <- function(summary_data) {
      if (is.null(summary_data) || nrow(summary_data) == 0) {
        summary_data <- species_dashboard_empty_diel_summary()
      }

      classification <- summary_data$classification[[1]]
      meaning <- summary_data$meaning[[1]]
      sample_size <- summary_data$sample_size[[1]]
      confidence <- summary_data$confidence[[1]]
      note <- summary_data$note[[1]]
      count_label <- if (isTRUE(use_net())) "net observations" else "observations"
      state_class <- dplyr::case_when(
        identical(confidence, "insufficient") ~ "diel-activity-state-insufficient",
        identical(confidence, "low") ~ "diel-activity-state-low",
        TRUE ~ "diel-activity-state-normal"
      )

      card(
        card_header(render_diel_card_header()),
        card_body(
          div(
            class = paste("diel-activity-card", state_class),
            div(classification, class = "dashcard-output diel-activity-classification"),
            div(meaning, class = "dashcard-period"),
            div(sprintf("%s %s", format_dash_number(sample_size), count_label), class = "dashcard-period"),
            render_diel_distribution_bar(summary_data),
            render_diel_legend(summary_data),
            div(note, class = "diel-activity-note")
          )
        ),
        render_count_basis_footer(use_net())
      )
    }

    observeEvent(input$species_diel_info_clicked, {
      thresholds <- diel_threshold_config
      showModal(modalDialog(
        title = "Diel activity classification",
        tags$p(
          "This card classifies the selected species for the current dashboard selection. Observation counts use the same count basis as the dashboard: net observations when net data is enabled."
        ),
        tags$h5("Diel Periods"),
        tags$table(
          class = "table table-sm table-striped",
          tags$tbody(
            tags$tr(tags$th("Matutinal"), tags$td("Civil dawn to sunrise.")),
            tags$tr(tags$th("Diurnal"), tags$td("Sunrise to sunset.")),
            tags$tr(tags$th("Vespertine"), tags$td("Sunset to civil dusk.")),
            tags$tr(tags$th("Nocturnal"), tags$td("Civil dusk to civil dawn."))
          )
        ),
        tags$h5("Overall Classes"),
        tags$table(
          class = "table table-sm table-striped",
          tags$tbody(
            tags$tr(tags$th("Diurnal"), tags$td("Day-active.")),
            tags$tr(tags$th("Nocturnal"), tags$td("Night-active.")),
            tags$tr(tags$th("Crepuscular"), tags$td("Dawn/dusk-active.")),
            tags$tr(tags$th("Cathemeral"), tags$td("Active intermittently day and night.")),
            tags$tr(tags$th("Arrhythmic"), tags$td("Enough observations, but no clear diel rhythm."))
          )
        ),
        tags$h5("Classification Rules"),
        tags$p(
          "The card estimates deployed camera hours in each diel period from deployment start/end times and per-location sunrise, sunset, civil dawn, and civil dusk. It then compares observations per deployed hour across the four diel periods."
        ),
        tags$ul(
          tags$li(sprintf("Fewer than %d observations: Insufficient data.", thresholds$insufficient_n)),
          tags$li(sprintf("%d to %d observations: classification is shown with low confidence.", thresholds$insufficient_n, thresholds$normal_confidence_n - 1)),
          tags$li(sprintf("Diurnal or Nocturnal: that rate share is at least %.0f%%.", thresholds$dominant_share * 100)),
          tags$li(sprintf("Crepuscular: dawn plus dusk rate share is at least %.0f%%, with both dawn and dusk represented.", thresholds$crepuscular_share * 100)),
          tags$li(sprintf("Cathemeral: both day and night rate shares are at least %.0f%%.", thresholds$cathemeral_day_night_share * 100)),
          tags$li("Arrhythmic: the sample is sufficient, but none of the above patterns is dominant.")
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    })

    render_species_observations_table_note <- function() {
      if (isTRUE(use_net())) {
        return(card_footer(
          class = "dashcard-count-basis-footer species-observations-note",
          tags$span("All observations shown, including possible duplicates excluded from net metrics."),
          tags$a(
            href = "#",
            class = "dashcard-count-basis-info",
            title = "Count basis",
            onclick = "Shiny.setInputValue('info_field_clicked', 'Net Data Basis', {priority: 'event'}); return false;",
            icon("circle-info")
          )
        ))
      }

      card_footer(
        class = "species-observations-note",
        tags$span("All observations shown, including possible duplicates.")
      )
    }

    render_duplicate_review_control <- function(species_obs) {
      duplicate_count <- if (!is.null(species_obs) && nrow(species_obs) > 0 && "possible_duplicate" %in% names(species_obs)) {
        sum(!is.na(species_obs$possible_duplicate) & species_obs$possible_duplicate, na.rm = TRUE)
      } else {
        0
      }
      review_ids <- species_dashboard_possible_duplicate_queue(species_obs)

      if (length(review_ids) == 0) {
        return(tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-secondary",
          disabled = "disabled",
          title = "No possible duplicates in this selection",
          icon("clone"),
          " Review possible duplicates"
        ))
      }

      onclick_payload <- jsonlite::toJSON(
        list(observation_ids = review_ids),
        auto_unbox = TRUE
      )
      onclick_js <- sprintf(
        "Shiny.setInputValue('review_sequences_click', %s, {priority: 'event'}); return false;",
        onclick_payload
      )

      tags$div(
        class = "species-duplicate-review-control",
        tags$a(
          href = "#",
          class = "btn btn-sm btn-outline-secondary",
          title = "Review possible duplicate sequences",
          onclick = onclick_js,
          icon("clone"),
          " Review possible duplicates"
        ),
        tags$span(
          sprintf("%s possible duplicate observations", format_dash_number(duplicate_count)),
          class = "species-duplicate-review-count"
        )
      )
    }

    render_species_observations_table <- function(species_obs) {
      table_data <- species_dashboard_observations_table_data(species_obs)
      table_options <- list(
        pageLength = 25,
        searching = TRUE,
        lengthChange = TRUE,
        scrollX = TRUE
      )
      if ("Timestamp" %in% names(table_data)) {
        table_options$order <- list(list(match("Timestamp", names(table_data)) - 1, "desc"))
      }

      DT::datatable(
        table_data,
        escape = FALSE,
        filter = "top",
        options = table_options,
        class = "display",
        rownames = FALSE
      )
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
      metric <- generate_rai_group_network_metric_cached(
        species_obs,
        deps_data,
        rai_groups_for_species,
        vernacular_name,
        rai_norm_hours,
        use_net(),
        cache_context = paste("species_card", species_name, period_name_label, locality_token, sep = "|")
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
        ),
        render_count_basis_footer(use_net())
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
      species_obs_for_counts <- filter_possible_duplicates_for_use_net(species_obs, use_net())

      total_count <- sum(species_obs_for_counts$count, na.rm = TRUE)
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

      total_detections_count <- nrow(species_obs_for_counts)
      count_label <- if (isTRUE(use_net())) "net individuals" else "individuals"
      season_dates_card <- if (!is.null(period_name_label) &&
                               length(period_name_label) == 1 &&
                               !is.na(period_name_label) &&
                               period_name_label != "ALL" &&
                               period_name_label %in% names(core_data$period_groups)) {
        render_dashboard_period_dates_card(
          summarise_dashboard_effort(locality = selected_localities(), period_name = period_name_label)
        )
      } else {
        NULL
      }

      total_card <- card(
          card_header("Animal Observations"),
          card_body(
            render_dashcard_metric_body(
              total_detections_count,
              div(sprintf("for %d %s", total_count, count_label), class = "dashcard-period"),
              review_action
            )
          ),
          render_count_basis_footer(use_net())
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

      metric_cards <- list(width = "250px")
      if (!is.null(season_dates_card)) {
        metric_cards <- c(metric_cards, list(season_dates_card))
      }
      metric_cards <- c(metric_cards, list(total_card, unique_card), generate_rai_cards(species_obs, deps_data, period_name_label))

      do.call(
        layout_column_wrap,
        metric_cards
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
      has_near_miss_window <- !is.null(near_miss_minutes) && !is.na(near_miss_minutes)

      near_miss_ui <- NULL
      if (tolower(species_name) != "apteryx mantelli" &&
          has_near_miss_window &&
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
      filter_dashboard_obs(filter_detection_obs(core_data$obs))
    })
    overall_deps <- reactive({
      filter_dashboard_deps(core_data$deps)
    })
    overall_sobs <- reactive({
      overall_obs() %>% dplyr::filter(tolower(scientificName) == tolower(species_name))
    })

    output$overall_metric_cards <- renderUI({ render_metric_cards(overall_sobs(), overall_deps(), "ALL") }) %>%
      bindCache(species_name, locality_filter_token(), combine_localities_selected(), rai_norm_hours, use_net(), "ALL")
    output$overall_activity_plot <- renderPlot({ generate_activity_plot(filter_possible_duplicates_for_use_net(overall_sobs(), use_net())) })
    output$overall_diel_activity_card <- renderUI({
      render_diel_activity_card(
        species_dashboard_diel_summary(
          filter_possible_duplicates_for_use_net(overall_sobs(), use_net()),
          overall_deps(),
          core_data$environment_daily,
          thresholds = diel_threshold_config
        )
      )
    }) %>%
      bindCache(species_name, locality_filter_token(), use_net(), "ALL", "diel_activity_card")
    output$overall_cooccurrence_ui <- renderUI({ generate_cooccurrence(overall_sobs(), overall_obs()) })
    output$overall_duplicate_review_control <- renderUI({ render_duplicate_review_control(overall_sobs()) })
    output$overall_observations_table <- DT::renderDT({ render_species_observations_table(overall_sobs()) })

    period_defaults <- species_dashboard_period_defaults(core_data)

    # Overall RAI plot using plotting_module_server (we map this to overall)
    plotting_module_server(
      id = "overall_rai_plot",
      type = NULL,
      obs = filter_detection_obs(core_data$obs),
      deps = core_data$deps,
      species_override = species_name,
      rai_groups = rai_groups_for_species,
      rai_norm_hours = rai_norm_hours,
      use_net = use_net
    )

    # Track loaded map tabs for lazy loading
    loaded_dashboard_tabs <- reactiveVal(character())

    observe({
      req(input$dashboard_tabs)
      current_tab <- input$dashboard_tabs
      section_tab <- switch(
        current_tab,
        overall = input$overall_section_tabs,
        current_period = input$current_section_tabs,
        prior_period = input$prior_section_tabs,
        last_year_period = input$last_year_section_tabs,
        NULL
      )
      req(identical(section_tab, "map"))

      if (!(current_tab %in% loaded_dashboard_tabs())) {

        if (current_tab == "overall") {
          # Overall Density Map
          mapping_module_server(
            id = "species_density_map_overall",
            type = "density",
            obs = overall_obs,
            deps = overall_deps,
            species_override = reactive(species_name),
            localities_override = selected_localities,
            use_net = use_net
          )
        } else if (current_tab == "current_period") {
          # Current Period Density Map
          mapping_module_server(
            id = "species_density_map_current",
            type = "density",
            obs = current_obs,
            deps = current_deps,
            species_override = reactive(species_name),
            localities_override = selected_localities,
            use_net = use_net
          )
        } else if (current_tab == "prior_period") {
          # Prior Period Density Map
          mapping_module_server(
            id = "species_density_map_prior",
            type = "density",
            obs = prior_obs,
            deps = prior_deps,
            species_override = reactive(species_name),
            localities_override = selected_localities,
            use_net = use_net
          )
        } else if (current_tab == "last_year_period") {
          # Last Year Period Density Map
          mapping_module_server(
            id = "species_density_map_last_year",
            type = "density",
            obs = ly_obs,
            deps = ly_deps,
            species_override = reactive(species_name),
            localities_override = selected_localities,
            use_net = use_net
          )
        }

        # Add to loaded list
        loaded_dashboard_tabs(c(loaded_dashboard_tabs(), current_tab))
        logger::log_debug(sprintf("species_dashboard_module_server, lazily loaded map for tab %s", current_tab))
      }
    })

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

    output$current_metric_cards <- renderUI({ render_metric_cards(current_sobs(), current_deps(), current_period_data$period_name()) }) %>%
      bindCache(species_name, locality_filter_token(), combine_localities_selected(), rai_norm_hours, use_net(), current_period_data$period_name())
    output$current_activity_plot <- renderPlot({ generate_activity_plot(filter_possible_duplicates_for_use_net(current_sobs(), use_net())) })
    output$current_diel_activity_card <- renderUI({
      render_diel_activity_card(
        species_dashboard_diel_summary(
          filter_possible_duplicates_for_use_net(current_sobs(), use_net()),
          current_deps(),
          core_data$environment_daily,
          current_period_data$start_date(),
          current_period_data$end_date(),
          thresholds = diel_threshold_config
        )
      )
    }) %>%
      bindCache(species_name, locality_filter_token(), use_net(), current_period_data$period_name(), "diel_activity_card")
    output$current_cooccurrence_ui <- renderUI({ generate_cooccurrence(current_sobs(), current_obs()) })
    output$current_duplicate_review_control <- renderUI({ render_duplicate_review_control(current_sobs()) })
    output$current_observations_table <- DT::renderDT({ render_species_observations_table(current_sobs()) })
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

    output$prior_metric_cards <- renderUI({ render_metric_cards(prior_sobs(), prior_deps(), prior_period_data$period_name()) }) %>%
      bindCache(species_name, locality_filter_token(), combine_localities_selected(), rai_norm_hours, use_net(), prior_period_data$period_name())
    output$prior_activity_plot <- renderPlot({ generate_activity_plot(filter_possible_duplicates_for_use_net(prior_sobs(), use_net())) })
    output$prior_diel_activity_card <- renderUI({
      render_diel_activity_card(
        species_dashboard_diel_summary(
          filter_possible_duplicates_for_use_net(prior_sobs(), use_net()),
          prior_deps(),
          core_data$environment_daily,
          prior_period_data$start_date(),
          prior_period_data$end_date(),
          thresholds = diel_threshold_config
        )
      )
    }) %>%
      bindCache(species_name, locality_filter_token(), use_net(), prior_period_data$period_name(), "diel_activity_card")
    output$prior_cooccurrence_ui <- renderUI({ generate_cooccurrence(prior_sobs(), prior_obs()) })
    output$prior_duplicate_review_control <- renderUI({ render_duplicate_review_control(prior_sobs()) })
    output$prior_observations_table <- DT::renderDT({ render_species_observations_table(prior_sobs()) })
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

    output$last_year_metric_cards <- renderUI({ render_metric_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name()) }) %>%
      bindCache(species_name, locality_filter_token(), combine_localities_selected(), rai_norm_hours, use_net(), last_year_period_data$period_name())
    output$last_year_activity_plot <- renderPlot({ generate_activity_plot(filter_possible_duplicates_for_use_net(ly_sobs(), use_net())) })
    output$last_year_diel_activity_card <- renderUI({
      render_diel_activity_card(
        species_dashboard_diel_summary(
          filter_possible_duplicates_for_use_net(ly_sobs(), use_net()),
          ly_deps(),
          core_data$environment_daily,
          last_year_period_data$start_date(),
          last_year_period_data$end_date(),
          thresholds = diel_threshold_config
        )
      )
    }) %>%
      bindCache(species_name, locality_filter_token(), use_net(), last_year_period_data$period_name(), "diel_activity_card")
    output$last_year_cooccurrence_ui <- renderUI({ generate_cooccurrence(ly_sobs(), ly_obs()) })
    output$last_year_duplicate_review_control <- renderUI({ render_duplicate_review_control(ly_sobs()) })
    output$last_year_observations_table <- DT::renderDT({ render_species_observations_table(ly_sobs()) })
    output$last_year_period_name <- renderText({ last_year_period_data$period_name() })

  })
}
