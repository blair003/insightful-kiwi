# Helpers for calculating and rendering dashboard summary cards.

dashboard_rai_metric <- function(rai_group, lower_is_better, locality = NULL) {
  metric_obs <- core_data$obs
  metric_deps <- core_data$deps

  if (!is.null(locality)) {
    metric_obs <- metric_obs %>% dplyr::filter(.data$locality %in% !!locality)
    metric_deps <- metric_deps %>% dplyr::filter(.data$locality %in% !!locality)
  }

  metric <- generate_rai_group_period_comparison(
    obs = metric_obs,
    deps = metric_deps,
    period_groups = core_data$period_groups,
    rai_groups = config$globals$rai_groups,
    rai_group = rai_group,
    rai_norm_hours = config$globals$rai_norm_hours,
    use_net = config$globals$rai_net_count,
    current_period_index = core_data$period_defaults$primary_period_index,
    lower_is_better = lower_is_better
  )

  metric$scope_label <- locality_scope_label(locality)
  metric$locality_filter <- if (is.null(locality)) "ALL" else paste(locality, collapse = ",")
  metric
}

locality_scope_label <- function(locality = NULL) {
  if (is.null(locality)) {
    return("Combined localities")
  }

  locality <- as.character(locality)
  if (length(locality) == 0) {
    return("Combined localities")
  }

  if (length(locality) == 1) {
    return(locality_display_name(locality))
  }

  paste("Combined selected localities:", paste(vapply(locality, locality_display_name, character(1)), collapse = ", "))
}

locality_display_name <- function(locality) {
  locality_name <- config$meta$localities_list[[locality]]
  if (is.null(locality_name)) {
    return(locality)
  }

  as.character(locality_name)
}

format_dash_number <- function(value, digits = 0) {
  if (is.na(value)) {
    return("N/A")
  }

  format(round(value, digits), big.mark = ",", nsmall = digits)
}

format_percent_change <- function(delta, comparison_value, digits = 0) {
  if (is.na(delta) || is.na(comparison_value) || comparison_value == 0) {
    return("")
  }

  paste0(" (", format_dash_number(abs(delta / comparison_value) * 100, digits), "%)")
}

compare_dashboard_values <- function(current_value,
                                     comparison_value,
                                     comparison_period,
                                     lower_is_better = FALSE,
                                     include_percent = FALSE) {
  delta <- current_value - comparison_value
  direction <- dplyr::case_when(
    is.na(current_value) || is.na(comparison_value) ~ "unavailable",
    delta < 0 ~ "down",
    delta > 0 ~ "up",
    TRUE ~ "unchanged"
  )
  state <- dplyr::case_when(
    direction == "unavailable" ~ "unavailable",
    direction == "unchanged" ~ "unchanged",
    lower_is_better && direction == "down" ~ "improved",
    lower_is_better && direction == "up" ~ "worse",
    !lower_is_better && direction == "up" ~ "improved",
    !lower_is_better && direction == "down" ~ "worse"
  )
  percent_text <- if (isTRUE(include_percent)) {
    format_percent_change(delta, comparison_value)
  } else {
    ""
  }
  message <- dplyr::case_when(
    state == "unavailable" ~ "No comparison period",
    direction == "down" ~ sprintf("Down %s%s vs %s", format_dash_number(abs(delta)), percent_text, comparison_period),
    direction == "up" ~ sprintf("Up %s%s vs %s", format_dash_number(abs(delta)), percent_text, comparison_period),
    TRUE ~ sprintf("No change vs %s", comparison_period)
  )

  list(
    period = comparison_period,
    value = comparison_value,
    delta = delta,
    direction = direction,
    state = state,
    message = message
  )
}

get_dashboard_comparison_state <- function(comparisons) {
  states <- vapply(comparisons, function(x) x$state, character(1))
  states <- states[states != "unavailable"]

  if (length(states) == 0) {
    return("unavailable")
  }
  if (all(states == "improved")) {
    return("improved")
  }
  if (all(states == "worse")) {
    return("worse")
  }
  if (all(states == "unchanged")) {
    return("unchanged")
  }

  "mixed"
}

dashboard_animal_detections_metric <- function(locality = NULL) {
  period_names <- period_names_without_all(core_data$period_groups)
  current_period <- core_data$period_defaults$primary_period
  current_period_index <- core_data$period_defaults$primary_period_index
  prior_period <- if (current_period_index < length(period_names)) {
    period_names[[current_period_index + 1]]
  } else {
    NA_character_
  }
  last_year_period <- find_matching_prior_year_period(current_period, core_data$period_groups)

  if (!is.na(last_year_period) && !is.na(prior_period) && last_year_period == prior_period) {
    last_year_period <- NA_character_
  }

  get_period_value <- function(period_name) {
    if (is.na(period_name) || !period_name %in% names(core_data$period_groups)) {
      return(NA_real_)
    }

    period <- core_data$period_groups[[period_name]]
    period_deps <- filter_deps(core_data$deps, period$start_date, period$end_date)
    if (!is.null(locality)) {
      period_deps <- period_deps %>% dplyr::filter(.data$locality %in% !!locality)
    }
    sum(period_deps$animal_detections_count, na.rm = TRUE)
  }

  current_value <- get_period_value(current_period)
  comparisons <- list(
    prior_period = compare_dashboard_values(
      current_value,
      get_period_value(prior_period),
      prior_period,
      lower_is_better = FALSE,
      include_percent = TRUE
    ),
    matching_prior_season = compare_dashboard_values(
      current_value,
      get_period_value(last_year_period),
      last_year_period,
      lower_is_better = FALSE,
      include_percent = TRUE
    )
  )

  list(
    current_period = current_period,
    current_formatted_value = format_dash_number(current_value),
    comparisons = comparisons,
    comparison_state = get_dashboard_comparison_state(comparisons),
    scope_label = locality_scope_label(locality)
  )
}

render_dashboard_info_link <- function(detail_token) {
  if (is.null(detail_token)) {
    return(NULL)
  }

  tags$a(
    href = "#",
    class = "dashcard-info-link",
    title = "Show RAI calculation basis",
    onclick = sprintf(
      "Shiny.setInputValue('dashboard_rai_details_clicked', %s, {priority: 'event'}); return false;",
      jsonlite::toJSON(detail_token, auto_unbox = TRUE)
    ),
    icon("circle-info")
  )
}

dashboard_comparison_state_class <- function(state) {
  if (is.null(state) || length(state) == 0 || is.na(state[[1]])) {
    state <- "unavailable"
  } else {
    state <- as.character(state[[1]])
  }

  state_class <- paste("dashcard-state", state, sep = "-")
  if (!state %in% c("improved", "mixed", "worse", "unchanged", "unavailable")) {
    state_class <- "dashcard-state-unavailable"
  }

  state_class
}

render_dashboard_trend_icon <- function(comparison) {
  if (comparison$state == "unavailable") {
    return(NULL)
  }

  icon_name <- dplyr::case_when(
    comparison$direction == "up" ~ "arrow-up",
    comparison$direction == "down" ~ "arrow-down",
    TRUE ~ "minus"
  )

  tags$span(icon(icon_name), class = "dashcard-trend-icon", `aria-hidden` = "true")
}

render_dashboard_comparison_body <- function(metric, detail_token = NULL) {
  state_class <- dashboard_comparison_state_class(metric$comparison_state)

  comparison_tags <- lapply(metric$comparisons, function(comparison) {
    if (comparison$state == "unavailable") {
      return(NULL)
    }

    comparison_state_class <- dashboard_comparison_state_class(comparison$state)
    div(
      render_dashboard_trend_icon(comparison),
      tags$span(comparison$message),
      class = paste("dashcard-comparison", comparison_state_class)
    )
  })
  comparison_tags <- Filter(Negate(is.null), comparison_tags)

  if (length(comparison_tags) == 0) {
    comparison_tags <- list(div("No comparison period", class = paste("dashcard-comparison", state_class)))
  }
  details_action <- if (!is.null(detail_token)) {
    div(class = "dashcard-card-action", render_dashboard_info_link(detail_token))
  } else {
    NULL
  }

  div(
    class = paste("dashcard-metric-state", state_class),
    details_action,
    div(metric$current_formatted_value, class = "dashcard-output dashcard-output-rai"),
    div(metric$current_period, class = "dashcard-period"),
    tagList(comparison_tags)
  )
}

dashboard_rat_icon <- function() {
  HTML(
    '<i class="fas fa-rat" style="display: inline-block; width: 1.2em; height: 1em; vertical-align: -0.125em;">
      <svg viewBox="0 0 512 512" fill="currentColor" style="width: 1.2em; height: 1.2em;">
      <path d="M512 352c0-26.5-21.5-48-48-48c-8.7 0-16.8 2.3-23.8 6.4L393.1 264c11.9-18.4 18.9-40.4 18.9-64c0-66.3-53.7-120-120-120H192c-15.6 0-30.5 3-44.1 8.5C125.4 51.5 86.1 24 40 24C17.9 24 0 41.9 0 64s17.9 40 40 40c1.5 0 3-.1 4.4-.3C69.3 162.1 127.3 208 195.4 208H400c8.8 0 16 7.2 16 16s-7.2 16-16 16s-16-7.2-16-16c0-4.4-3.6-8-8-8s-8 3.6-8 8c0 22.1 17.9 40 40 40s40-17.9 40-40c0-1.8-.1-3.5-.4-5.2c16.3-4.5 28.4-19.4 28.4-37.1c0-21.3-17.2-38.6-38.6-38.6c-4.1 0-8 1.1-11.8 1.8c-23.5-65.5-86.4-113.8-160.2-113.8H192c-10.7 0-21.1 1.5-31 4.3C143.5 110.1 113.1 80 80 80c-44.2 0-80 35.8-80 80s35.8 80 80 80c4.1 0 8-1.1 11.8-1.8c23.5 65.5 86.4 113.8 160.2 113.8H400c14.1 0 25.6 11.5 25.6 25.6s-11.5 25.6-25.6 25.6-25.6-11.5-25.6-25.6c0-8.8-7.2-16-16-16s-16 7.2-16 16c0 31.8 25.8 57.6 57.6 57.6s57.6-25.8 57.6-57.6z"/>
      </svg>
      </i>'
  )
}

render_dashboard_card_header <- function(card_icon, card_title, custom_icon = NULL) {
  header_icon <- if (!is.null(custom_icon)) {
    custom_icon
  } else {
    icon(card_icon)
  }

  div(
    class = "dashcard-header-row",
    tags$span(tagList(header_icon, card_title), class = "dashcard-header-title")
  )
}

render_dashboard_metric_cards <- function(locality = NULL) {
  locality_token <- if (is.null(locality)) "ALL" else paste(locality, collapse = ",")
  mustelid_metric <- dashboard_rai_metric("Mustelids", lower_is_better = TRUE, locality = locality)
  rat_metric <- if ("Rats" %in% names(config$globals$rai_groups)) {
    dashboard_rai_metric("Rats", lower_is_better = TRUE, locality = locality)
  } else {
    NULL
  }
  kiwi_metric <- dashboard_rai_metric("Kiwi", lower_is_better = FALSE, locality = locality)
  animal_metric <- dashboard_animal_detections_metric(locality = locality)

  layout_column_wrap(
    width = "180px",
    card(
      card_header(render_dashboard_card_header("otter", "Mustelid RAI")),
      card_body(render_dashboard_comparison_body(mustelid_metric, paste("Mustelids", locality_token, sep = "|"))),
      full_screen = FALSE
    ),
    if (!is.null(rat_metric)) {
      card(
        card_header(render_dashboard_card_header(NULL, "Rat RAI", custom_icon = dashboard_rat_icon())),
        card_body(render_dashboard_comparison_body(rat_metric, paste("Rats", locality_token, sep = "|"))),
        full_screen = FALSE
      )
    },
    card(
      card_header(render_dashboard_card_header("kiwi-bird", "Kiwi RAI")),
      card_body(render_dashboard_comparison_body(kiwi_metric, paste("Kiwi", locality_token, sep = "|"))),
      full_screen = FALSE
    ),
    card(
      card_header(render_dashboard_card_header("paw", "Animal Detections")),
      card_body(render_dashboard_comparison_body(animal_metric)),
      full_screen = FALSE
    )
  )
}

show_rai_metric_modal <- function(metric) {
  format_period_date <- function(period_metric, field) {
    if (is.na(period_metric[[field]])) {
      return("N/A")
    }

    format(as.Date(period_metric[[field]]), "%d %b %Y")
  }

  format_period_value <- function(period_metric, value_name, comparison = NULL) {
    if (is.null(period_metric) || is.na(period_metric$period)) {
      return("N/A")
    }

    value <- switch(
      value_name,
      rai = period_metric$formatted_value,
      change = if (!is.null(comparison) && comparison$state != "unavailable") comparison$message else "Current",
      period = period_metric$period,
      start_date = format_period_date(period_metric, "start_date"),
      end_date = format_period_date(period_metric, "end_date"),
      detections = format_dash_number(period_metric$animal_detections),
      individuals = {
        val <- format_dash_number(period_metric$individuals_count)
        if (!is.na(period_metric$individuals_count) && period_metric$individuals_count > 0) {
                  # Manually construct JS object to avoid double quotes in the HTML attribute
                  onclick_js <- sprintf("Shiny.setInputValue('review_sequences_click', {period_name: '%s', rai_group: '%s', locality: '%s'}, {priority: 'event'}); return false;",
                    period_metric$period, metric$rai_group, metric$locality_filter)
                  HTML(sprintf('<a href="#" onclick="%s" title="Review Sequences">%s</a>',
                    onclick_js, val))
        } else {
          val
        }
      },
      duplicates = format_dash_number(period_metric$possible_duplicates_count),
      net_individuals = format_dash_number(period_metric$net_individuals_count),
      camera_hours = format_dash_number(period_metric$camera_hours, 1),
      localities = format_dash_number(period_metric$locality_count),
      lines = format_dash_number(period_metric$line_count),
      "N/A"
    )

    value
  }

  render_period_table <- function(rows) {
    tags$table(
      class = "table table-sm table-striped rai-detail-table rai-period-table",
      tags$thead(tags$tr(
        tags$th(""),
        lapply(period_columns, function(column) tags$th(column$label))
      )),
      tags$tbody(lapply(rows, function(row) {
        tags$tr(
          tags$th(row$label),
          lapply(period_columns, function(column) {
            tags$td(format_period_value(column$metric, row$key, column$comparison))
          })
        )
      }))
    )
  }

  get_line_rai_localities <- function() {
    localities <- unlist(lapply(period_columns, function(column) {
      line_values <- column$metric$line_rai_values
      if (is.null(line_values) || nrow(line_values) == 0) {
        return(character())
      }

      as.character(line_values$locality)
    }), use.names = FALSE)

    unique(localities)
  }

  get_locality_period_rai <- function(period_metric, locality) {
    locality_values <- period_metric$locality_rai_values
    if (is.null(locality_values) || nrow(locality_values) == 0) {
      return("N/A")
    }

    matching_locality <- locality_values[locality_values$locality == locality, , drop = FALSE]
    if (nrow(matching_locality) == 0 || is.na(matching_locality$formatted_value[[1]])) {
      return("N/A")
    }

    matching_locality$formatted_value[[1]]
  }

  render_line_rai_period_summary <- function(period_metric, locality, period_label) {
    if (is.null(period_metric) || is.na(period_metric$period)) {
      return(tags$div(
        class = "rai-line-period-summary rai-line-period-summary-empty",
        tags$div(class = "rai-line-period-label", period_label),
        tags$div("N/A", class = "rai-line-empty")
      ))
    }

    line_values <- period_metric$line_rai_values
    if (is.null(line_values) || nrow(line_values) == 0) {
      return(tags$div(
        class = "rai-line-period-summary rai-line-period-summary-empty",
        tags$div(class = "rai-line-period-label", period_label),
        tags$div("N/A", class = "rai-line-empty")
      ))
    }

    locality_lines <- line_values[line_values$locality == locality, , drop = FALSE]
    if (nrow(locality_lines) == 0) {
      return(tags$div(
        class = "rai-line-period-summary rai-line-period-summary-empty",
        tags$div(class = "rai-line-period-label", period_label),
        tags$div("N/A", class = "rai-line-empty")
      ))
    }

    tags$div(
      class = "rai-line-period-summary",
      tags$div(class = "rai-line-period-label", period_label),
      tags$div(
        tags$span("Combined RAI ± SE:", class = "rai-line-summary-label"),
        tags$span(get_locality_period_rai(period_metric, locality), class = "rai-line-summary-value")
      ),
      tags$div(
        tags$span("Line RAIs:", class = "rai-line-summary-label"),
        tags$span(paste(locality_lines$formatted_value, collapse = ", "), class = "rai-line-summary-value")
      )
    )
  }

  render_line_rai_section <- function() {
    localities <- get_line_rai_localities()
    if (length(localities) == 0) {
      return(tags$p("Line RAI values are not available.", class = "rai-line-empty"))
    }

    tags$div(
      class = "rai-line-grid",
      lapply(localities, function(locality) {
        tags$div(
          class = "rai-line-locality-summary",
          tags$div(class = "rai-line-locality-heading", locality_display_name(locality)),
          tags$div(
            class = "rai-line-period-grid",
            lapply(period_columns, function(column) {
              render_line_rai_period_summary(column$metric, locality, column$label)
            })
          )
        )
      })
    )
  }

  constant_rows <- list(
    c("Species included", paste(config$globals$rai_groups[[metric$rai_group]], collapse = ", ")),
    c("RAI normalisation", paste(format_dash_number(metric$current_metric$rai_norm_hours), "camera hours")),
    c("Duplicate handling", if (isTRUE(metric$current_metric$use_net)) "RAI uses Net count, excluding possible duplicates" else "RAI uses Total individuals counted, including possible duplicates")
  )

  constant_table <- tags$table(
    class = "table table-sm table-striped rai-detail-table",
    tags$tbody(lapply(constant_rows, function(row) {
      tags$tr(tags$th(row[[1]]), tags$td(row[[2]]))
    }))
  )

  period_columns <- list(
    current = list(label = "Current", metric = metric$current_metric, comparison = NULL),
    prior = list(label = "Prior", metric = metric$prior_metric, comparison = metric$comparisons$prior_period),
    last_year = list(label = "Last Year", metric = metric$matching_prior_season_metric, comparison = metric$comparisons$matching_prior_season)
  )

  period_result_rows <- list(
    list(label = "RAI", key = "rai"),
    list(label = "Change", key = "change"),
    list(label = "Period", key = "period"),
    list(label = "Start Date", key = "start_date"),
    list(label = "End Date", key = "end_date")
  )

  period_input_rows <- list(
    list(label = "Detections", key = "detections"),
    list(label = "Individuals counted", key = "individuals"),
    list(label = "Possible duplicate individuals", key = "duplicates")
  )

  if (isTRUE(metric$current_metric$use_net)) {
    period_input_rows <- c(period_input_rows, list(list(label = "Net individuals used", key = "net_individuals")))
  }

  is_single_locality_metric <- !is.na(metric$current_metric$locality_count) &&
    metric$current_metric$locality_count == 1

  period_input_rows <- c(period_input_rows, list(
    list(label = "Camera hours", key = "camera_hours"),
    list(label = "Localities averaged", key = "localities"),
    list(label = "Locality-line records", key = "lines")
  ))

  period_formula <- if (isTRUE(metric$current_metric$use_net)) {
    "line RAI = net individuals / camera hours x RAI normalisation"
  } else {
    "line RAI = individuals counted / camera hours x RAI normalisation"
  }

  modal_title <- if (!is.null(metric$scope_label) && metric$scope_label != "Combined localities") {
    paste(metric$rai_group, "RAI calculation basis -", metric$scope_label)
  } else {
    paste(metric$rai_group, "RAI calculation basis")
  }

  showModal(modalDialog(
    title = modal_title,
    tags$p(
      "Each locality-line first gets a line RAI: ",
      tags$code(period_formula),
      ". Line values are then averaged to locality RAI (mRAI)."
    ),
    if (is_single_locality_metric) {
      tags$p("The displayed uncertainty is the standard error across line RAI values.")
    } else {
      tags$p(
        "Locality values are averaged to network RAI (mmRAI). ",
        "The displayed uncertainty is the network standard error across locality RAI values."
      )
    },
    tags$h5("Constant Settings"),
    tags$div(class = "rai-table-scroll", constant_table),
    tags$h5("Period Results"),
    tags$div(class = "rai-table-scroll", render_period_table(period_result_rows)),
    tags$h5("Period Inputs"),
    tags$div(class = "rai-table-scroll", render_period_table(period_input_rows)),
    tags$h5("Line RAIs"),
    render_line_rai_section(),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l"
  ))
}
