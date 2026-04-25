# Helpers for calculating and rendering dashboard summary cards.

dashboard_rai_metric <- function(rai_group, lower_is_better, locality = NULL) {
  metric_obs <- core_data$obs
  metric_deps <- core_data$deps

  if (!is.null(locality)) {
    metric_obs <- metric_obs %>% dplyr::filter(.data$locality == !!locality)
    metric_deps <- metric_deps %>% dplyr::filter(.data$locality == !!locality)
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

  metric$scope_label <- if (is.null(locality)) "Combined localities" else locality_display_name(locality)
  metric
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
      period_deps <- period_deps %>% dplyr::filter(.data$locality == !!locality)
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
    scope_label = if (is.null(locality)) "Combined localities" else locality_display_name(locality)
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

render_dashboard_comparison_body <- function(metric, detail_token = NULL) {
  state_class <- paste("dashcard-state", metric$comparison_state, sep = "-")

  if (!metric$comparison_state %in% c("improved", "mixed", "worse", "unchanged", "unavailable")) {
    state_class <- "dashcard-state-unavailable"
  }

  comparison_tags <- lapply(metric$comparisons, function(comparison) {
    if (comparison$state == "unavailable") {
      return(NULL)
    }

    div(comparison$message, class = paste("dashcard-comparison", state_class))
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

render_dashboard_card_header <- function(card_icon, card_title) {
  div(
    class = "dashcard-header-row",
    tags$span(tagList(icon(card_icon), card_title), class = "dashcard-header-title")
  )
}

render_dashboard_metric_cards <- function(locality = NULL) {
  locality_token <- if (is.null(locality)) "ALL" else locality
  mustelid_metric <- dashboard_rai_metric("Mustelids", lower_is_better = TRUE, locality = locality)
  kiwi_metric <- dashboard_rai_metric("Kiwi", lower_is_better = FALSE, locality = locality)
  animal_metric <- dashboard_animal_detections_metric(locality = locality)

  layout_column_wrap(
    width = "180px",
    card(
      card_header(render_dashboard_card_header("otter", "Mustelid RAI")),
      card_body(render_dashboard_comparison_body(mustelid_metric, paste("Mustelids", locality_token, sep = "|"))),
      full_screen = FALSE
    ),
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
      individuals = format_dash_number(period_metric$individuals_count),
      duplicates = format_dash_number(period_metric$possible_duplicates_count),
      net_individuals = format_dash_number(period_metric$net_individuals_count),
      camera_hours = format_dash_number(period_metric$camera_hours, 1),
      localities = format_dash_number(period_metric$locality_count),
      lines = format_dash_number(period_metric$line_count),
      "N/A"
    )

    value
  }

  constant_rows <- list(
    c("Species included", paste(config$globals$rai_groups[[metric$rai_group]], collapse = ", ")),
    c("RAI normalisation", paste(format_dash_number(metric$current_metric$rai_norm_hours), "camera hours")),
    c("Duplicate handling", if (isTRUE(metric$current_metric$use_net)) "Net count excludes possible duplicates" else "Total individuals counted includes possible duplicates")
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

  period_rows <- list(
    list(label = "RAI", key = "rai"),
    list(label = "Change", key = "change"),
    list(label = "Period", key = "period"),
    list(label = "Start Date", key = "start_date"),
    list(label = "End Date", key = "end_date"),
    list(label = "Detections", key = "detections"),
    list(label = "Individuals counted", key = "individuals"),
    list(label = "Possible duplicate individuals", key = "duplicates")
  )

  if (isTRUE(metric$current_metric$use_net)) {
    period_rows <- c(period_rows, list(list(label = "Net individuals used", key = "net_individuals")))
  }

  period_rows <- c(period_rows, list(
    list(label = "Camera hours", key = "camera_hours"),
    list(label = "Localities averaged", key = "localities"),
    list(label = "Locality-line records", key = "lines")
  ))

  period_table <- tags$table(
    class = "table table-sm table-striped rai-detail-table rai-period-table",
    tags$thead(tags$tr(
      tags$th(""),
      lapply(period_columns, function(column) tags$th(column$label))
    )),
    tags$tbody(lapply(period_rows, function(row) {
      tags$tr(
        tags$th(row$label),
        lapply(period_columns, function(column) {
          tags$td(format_period_value(column$metric, row$key, column$comparison))
        })
      )
    }))
  )

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
    if (!is.null(metric$scope_label) && metric$scope_label != "Combined localities") {
      tags$p("The displayed uncertainty is the standard error across line RAI values.")
    } else {
      tags$p(
        "Locality values are averaged to network RAI (mmRAI). ",
        "The displayed uncertainty is the network standard error across locality RAI values."
      )
    },
    tags$h5("Constant Settings"),
    constant_table,
    tags$h5("Period Inputs and Results"),
    period_table,
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l"
  ))
}
