# Helpers for calculating and rendering dashboard summary cards.

dashboard_rai_metric <- function(rai_group, lower_is_better, locality = NULL, period_name = NULL) {
  metric_obs <- filter_detection_obs(core_data$obs)
  metric_deps <- core_data$deps

  if (!is.null(locality)) {
    metric_obs <- metric_obs %>% dplyr::filter(.data$locality %in% !!locality)
    metric_deps <- metric_deps %>% dplyr::filter(.data$locality %in% !!locality)
  }

  current_period_index <- core_data$app$period_defaults$primary_period_index
  if (!is.null(period_name)) {
    period_names <- names(core_data$period_groups)
    period_names <- period_names[period_names != "ALL"]
    idx <- match(period_name, period_names)
    if (!is.na(idx)) {
      current_period_index <- idx
    }
  }
  if (length(current_period_index) != 1 || is.na(current_period_index)) {
    current_period_index <- get_period_index(core_data$period_groups, core_data$app$period_defaults$primary_period)
  }
  current_period_index <- suppressWarnings(as.integer(current_period_index[[1]]))
  if (is.na(current_period_index)) {
    current_period_index <- get_period_index(core_data$period_groups, core_data$app$period_defaults$primary_period)
  }

  metric <- generate_rai_group_period_comparison(
    obs = metric_obs,
    deps = metric_deps,
    period_groups = core_data$period_groups,
    rai_groups = config$globals$rai_groups,
    rai_group = rai_group,
    rai_norm_hours = config$globals$rai_norm_hours,
    use_net = get_use_net_data_setting(),
    current_period_index = current_period_index,
    lower_is_better = lower_is_better
  )

  metric$scope_label <- locality_scope_label(locality)
  metric$locality_filter <- if (is.null(locality)) "ALL" else paste(locality, collapse = ",")
  metric$period_filter <- if (is.null(period_name)) "ALL" else period_name
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

  paste("Combined:", paste(vapply(locality, locality_display_name, character(1)), collapse = ", "))
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

dashboard_animal_detections_metric <- function(locality = NULL, period_name = NULL) {
  period_names <- period_names_without_all(core_data$period_groups)

  if (!is.null(period_name) && period_name %in% period_names) {
    current_period <- period_name
    current_period_index <- match(period_name, period_names)
  } else {
    current_period <- if (length(core_data$app$period_defaults$primary_period) > 0 && !is.na(core_data$app$period_defaults$primary_period[[1]])) {
      as.character(core_data$app$period_defaults$primary_period[[1]])
    } else if (length(period_names) > 0) {
      period_names[[1]]
    } else {
      NA_character_
    }
    current_period_index <- core_data$app$period_defaults$primary_period_index
  }
  current_period_index <- get_period_index(core_data$period_groups, current_period)

  prior_period <- if (length(current_period_index) == 1 && !is.na(current_period_index) && current_period_index < length(period_names)) {
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
    period_obs <- filter_detection_obs(filter_obs(core_data$obs, period$start_date, period$end_date))
    if (!is.null(locality)) {
      period_obs <- period_obs %>% dplyr::filter(.data$locality %in% !!locality)
    }

    period_obs <- filter_possible_duplicates_for_use_net(period_obs)

    sum(period_obs$count, na.rm = TRUE)
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

render_dashboard_info_link <- function(detail_token,
                                       input_id = "dashboard_rai_details_clicked",
                                       title = "Show RAI calculation basis") {
  if (is.null(detail_token)) {
    return(NULL)
  }

  tags$a(
    href = "#",
    class = "dashcard-info-link",
    title = title,
    onclick = sprintf(
      "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
      input_id,
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

render_dashboard_comparison_body <- function(metric,
                                             detail_token = NULL,
                                             use_state_background = TRUE,
                                             detail_input_id = "dashboard_rai_details_clicked") {
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
    div(class = "dashcard-card-action", render_dashboard_info_link(detail_token, detail_input_id))
  } else {
    NULL
  }

  div(
    class = paste(c("dashcard-metric-state", if (isTRUE(use_state_background)) state_class else NULL), collapse = " "),
    details_action,
    div(metric$current_formatted_value, class = "dashcard-output dashcard-output-rai"),
    div(metric$current_period, class = "dashcard-period"),
    tagList(comparison_tags)
  )
}

render_dashcard_metric_body <- function(value, ...) {
  div(
    class = "dashcard-metric-state dashcard-metric-state-plain",
    div(value, class = "dashcard-output"),
    ...
  )
}

render_count_basis_footer <- function(use_net = get_use_net_data_setting()) {
  if (!isTRUE(use_net)) {
    return(NULL)
  }

  label <- "Net data used"
  info_js <- "Shiny.setInputValue('info_field_clicked', 'Net Data Basis', {priority: 'event'}); return false;"

  card_footer(
    class = "dashcard-count-basis-footer",
    tags$span(label),
    tags$a(
      href = "#",
      class = "dashcard-count-basis-info",
      title = "Count basis",
      onclick = info_js,
      icon("circle-info")
    )
  )
}

format_dashboard_date_range <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date)) {
    return("Dates unavailable")
  }

  if (format(start_date, "%Y") == format(end_date, "%Y")) {
    return(paste(format(start_date, "%d %b"), "-", format(end_date, "%d %b %Y")))
  }

  paste(format(start_date, "%d %b %Y"), "-", format(end_date, "%d %b %Y"))
}

classifiable_observation_count <- function(obs) {
  if (is.null(obs) || nrow(obs) == 0) {
    return(integer(0))
  }

  if (!("count" %in% names(obs))) {
    return(rep(1L, nrow(obs)))
  }

  count <- suppressWarnings(as.integer(obs$count))
  count[is.na(count) | count < 1L] <- 1L
  count
}

sum_classifiable_observations <- function(obs) {
  sum(classifiable_observation_count(obs), na.rm = TRUE)
}

summarise_dashboard_effort <- function(locality = NULL, period_name = NULL) {
  period_deps <- if (!is.null(period_name) && period_name %in% names(core_data$period_groups)) {
    period <- core_data$period_groups[[period_name]]
    filter_deps(core_data$deps, period$start_date, period$end_date)
  } else {
    core_data$deps
  }
  period_obs <- if (!is.null(period_name) && period_name %in% names(core_data$period_groups)) {
    period <- core_data$period_groups[[period_name]]
    filter_obs(core_data$obs, period$start_date, period$end_date)
  } else {
    core_data$obs
  }
  if (!is.null(locality)) {
    period_deps <- period_deps %>% dplyr::filter(.data$locality %in% !!locality)
    period_obs <- period_obs %>% dplyr::filter(.data$locality %in% !!locality)
  }

  detection_obs <- filter_detection_obs(period_obs)
  detection_counts <- classifiable_observation_count(detection_obs)
  total_animal_observations_count <- sum(detection_counts, na.rm = TRUE)
  possible_duplicate_animals_count <- if ("possible_duplicate" %in% names(detection_obs)) {
    sum(ifelse(is.na(detection_obs$possible_duplicate) | !detection_obs$possible_duplicate, 0, detection_counts), na.rm = TRUE)
  } else {
    0
  }
  net_animal_observations_count <- total_animal_observations_count - possible_duplicate_animals_count
  animal_observations_count <- if (isTRUE(get_use_net_data_setting())) {
    net_animal_observations_count
  } else {
    total_animal_observations_count
  }

  blank_obs <- period_obs %>% dplyr::filter(.data$observationType == "blank")
  unclassified_unknown_obs <- period_obs %>%
    dplyr::filter(!(.data$observationType %in% c(core_data_detection_observation_types(), "blank")))
  blanks_count <- sum_classifiable_observations(blank_obs)
  unclassified_unknown_count <- sum_classifiable_observations(unclassified_unknown_obs)
  observations_count <- sum_classifiable_observations(period_obs)

  deployment_start_dates <- as.Date(period_deps$start)
  deployment_end_dates <- as.Date(period_deps$end)
  period_start_date <- if (nrow(period_deps) > 0 && any(!is.na(deployment_start_dates))) {
    min(deployment_start_dates, na.rm = TRUE)
  } else {
    NA
  }
  period_end_date <- if (nrow(period_deps) > 0 && any(!is.na(deployment_end_dates))) {
    max(deployment_end_dates, na.rm = TRUE)
  } else {
    NA
  }

  list(
    period_name = period_name,
    period_label = if (is.null(period_name)) "All Data" else period_name,
    period_date_text = format_dashboard_date_range(period_start_date, period_end_date),
    camera_hours_total = sum(period_deps$camera_hours, na.rm = TRUE),
    deployments_count = nrow(period_deps),
    animal_observations_count = animal_observations_count,
    total_animal_observations_count = total_animal_observations_count,
    possible_duplicate_animals_count = possible_duplicate_animals_count,
    net_animal_observations_count = net_animal_observations_count,
    blanks_count = blanks_count,
    unclassified_unknown_count = unclassified_unknown_count,
    observations_count = observations_count
  )
}


dashboard_config_global <- function(name, default = NULL) {
  if (exists("config", inherits = TRUE) && !is.null(config$globals[[name]])) {
    return(config$globals[[name]])
  }

  default
}

dashboard_numeric_assumption <- function(value, default) {
  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  numeric_value <- suppressWarnings(as.numeric(value[[1]]))
  if (is.na(numeric_value) || numeric_value < 0) {
    return(default)
  }

  numeric_value
}

dashboard_volunteer_hour_assumptions <- function() {
  settings <- dashboard_config_global("dashboard_volunteer_hours", list())

  list(
    deployment_setup_minutes = dashboard_numeric_assumption(
      settings$deployment_setup_minutes,
      30
    ),
    deployment_retrieval_minutes = dashboard_numeric_assumption(
      settings$deployment_retrieval_minutes,
      20
    ),
    data_check_upload_minutes_per_deployment = dashboard_numeric_assumption(
      settings$data_check_upload_minutes_per_deployment,
      20
    ),
    annotation_seconds_per_observation = dashboard_numeric_assumption(
      settings$annotation_seconds_per_observation,
      15
    )
  )
}

estimate_dashboard_volunteer_hours <- function(effort_summary,
                                               assumptions = dashboard_volunteer_hour_assumptions()) {
  deployments_count <- if (is.null(effort_summary$deployments_count)) {
    0
  } else {
    effort_summary$deployments_count
  }
  observations_count <- if (is.null(effort_summary$observations_count)) {
    0
  } else {
    effort_summary$observations_count
  }

  setup_hours <- deployments_count * assumptions$deployment_setup_minutes / 60
  retrieval_hours <- deployments_count * assumptions$deployment_retrieval_minutes / 60
  data_check_upload_hours <- deployments_count * assumptions$data_check_upload_minutes_per_deployment / 60
  annotation_hours <- observations_count * assumptions$annotation_seconds_per_observation / 3600
  field_work_hours <- setup_hours + retrieval_hours
  data_annotation_hours <- data_check_upload_hours + annotation_hours

  list(
    assumptions = assumptions,
    deployments_count = deployments_count,
    observations_count = observations_count,
    setup_hours = setup_hours,
    retrieval_hours = retrieval_hours,
    field_work_hours = field_work_hours,
    data_check_upload_hours = data_check_upload_hours,
    annotation_hours = annotation_hours,
    data_annotation_hours = data_annotation_hours,
    total_hours = field_work_hours + data_annotation_hours
  )
}

dashboard_selection_detail_token <- function(locality = NULL, period_name = NULL) {
  locality_token <- if (is.null(locality) || length(locality) == 0) {
    "ALL"
  } else {
    paste(as.character(locality), collapse = ",")
  }
  period_token <- if (is.null(period_name) || length(period_name) == 0 || is.na(period_name[[1]])) {
    "ALL"
  } else {
    as.character(period_name[[1]])
  }

  paste(locality_token, period_token, sep = "|")
}

parse_dashboard_selection_detail_token <- function(detail_token) {
  if (is.null(detail_token) || length(detail_token) == 0) {
    return(list(locality = NULL, period_name = NULL))
  }

  detail_token <- as.character(detail_token[[1]])
  detail_parts <- strsplit(detail_token, "\\|", fixed = FALSE)[[1]]
  locality <- if (length(detail_parts) > 0 && detail_parts[[1]] != "ALL") {
    strsplit(detail_parts[[1]], ",", fixed = TRUE)[[1]]
  } else {
    NULL
  }
  period_name <- if (length(detail_parts) > 1 && detail_parts[[2]] != "ALL") {
    detail_parts[[2]]
  } else {
    NULL
  }

  list(locality = locality, period_name = period_name)
}

format_dashboard_hours <- function(hours, digits = 1) {
  paste0(format_dash_number(hours, digits), "h")
}

render_dashboard_volunteer_hours_card <- function(effort_summary,
                                                  detail_token = NULL,
                                                  detail_input_id = "dashboard_volunteer_hours_details_clicked") {
  volunteer_hours <- estimate_dashboard_volunteer_hours(effort_summary)

  card(
    card_header(render_dashboard_card_header("hands-helping", "Volunteer Hours")),
    card_body(
      div(
        class = "dashcard-metric-state dashcard-metric-state-plain",
        div(
          class = "dashcard-card-action",
          render_dashboard_info_link(
            detail_token,
            detail_input_id,
            title = "Show volunteer hours estimate"
          )
        ),
        div(format_dash_number(volunteer_hours$total_hours, 1), class = "dashcard-output"),
        div("estimated volunteer hours", class = "dashcard-period"),
        div(
          sprintf(
            "%s field + %s data",
            format_dashboard_hours(volunteer_hours$field_work_hours),
            format_dashboard_hours(volunteer_hours$data_annotation_hours)
          ),
          class = "dashcard-period"
        )
      )
    ),
    full_screen = FALSE
  )
}

dashboard_classifier_display_mapping <- function() {
  mapping <- dashboard_config_global("dashboard_classifier_display_names", list())
  if (is.null(mapping) || length(mapping) == 0) {
    return(character())
  }

  unlist(mapping, use.names = TRUE)
}

dashboard_title_word <- function(value) {
  value <- as.character(value)
  if (!nzchar(value)) {
    return(value)
  }

  paste0(toupper(substr(value, 1, 1)), tolower(substr(value, 2, nchar(value))))
}

dashboard_classifier_looks_like_model <- function(value) {
  grepl(
    "\\b(v[0-9]+|model|detector|classifier|agouti|mega|yolo|ai|ml)\\b|[0-9]",
    value,
    ignore.case = TRUE
  )
}

format_dashboard_classifier_label <- function(classified_by) {
  raw_value <- trimws(as.character(classified_by))
  if (is.na(raw_value) || !nzchar(raw_value)) {
    return("Unrecorded")
  }

  display_mapping <- dashboard_classifier_display_mapping()
  if (raw_value %in% names(display_mapping)) {
    return(as.character(display_mapping[[raw_value]]))
  }

  if (dashboard_classifier_looks_like_model(raw_value)) {
    return(raw_value)
  }

  login_value <- sub("@.*$", "", raw_value)
  name_parts <- unlist(strsplit(login_value, "[[:space:]_.-]+", perl = TRUE))
  name_parts <- name_parts[nzchar(name_parts)]

  if (length(name_parts) >= 2) {
    first_name <- dashboard_title_word(name_parts[[1]])
    last_initial <- toupper(substr(name_parts[[length(name_parts)]], 1, 1))
    return(paste0(first_name, " ", last_initial, "."))
  }

  raw_value
}

summarise_dashboard_classifiers <- function(locality = NULL, period_name = NULL) {
  period_obs <- if (!is.null(period_name) && period_name %in% names(core_data$period_groups)) {
    period <- core_data$period_groups[[period_name]]
    filter_obs(core_data$obs, period$start_date, period$end_date)
  } else {
    core_data$obs
  }
  if (!is.null(locality)) {
    period_obs <- period_obs %>% dplyr::filter(.data$locality %in% !!locality)
  }

  if (nrow(period_obs) == 0 || !"classifiedBy" %in% names(period_obs)) {
    return(list(total_observations = 0, classifiers = data.frame()))
  }

  period_obs$classification_count <- classifiable_observation_count(period_obs)

  classifier_rows <- period_obs %>%
    dplyr::mutate(
      classifier_raw = dplyr::if_else(
        is.na(.data$classifiedBy) | !nzchar(trimws(as.character(.data$classifiedBy))),
        "Unrecorded",
        trimws(as.character(.data$classifiedBy))
      ),
      classifier_label = vapply(
        .data$classifier_raw,
        format_dashboard_classifier_label,
        character(1)
      )
    ) %>%
    dplyr::group_by(.data$classifier_label) %>%
    dplyr::summarise(observations = sum(.data$classification_count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      percent = (.data$observations / sum(.data$observations)) * 100
    ) %>%
    dplyr::arrange(dplyr::desc(.data$observations), .data$classifier_label)

  list(
    total_observations = sum(classifier_rows$observations),
    classifiers = as.data.frame(classifier_rows, stringsAsFactors = FALSE)
  )
}

format_dashboard_percent <- function(value, digits = 1) {
  if (is.na(value)) {
    return("N/A")
  }

  paste0(format_dash_number(value, digits), "%")
}

format_dashboard_classifier_count <- function(value) {
  label <- if (identical(as.integer(value), 1L)) "classification" else "classifications"
  paste(format(value, big.mark = ","), label)
}

format_dashboard_classifier_share <- function(percent, observations, include_label = FALSE) {
  count_text <- if (isTRUE(include_label)) {
    format_dashboard_classifier_count(observations)
  } else {
    format(observations, big.mark = ",")
  }

  sprintf("%s (%s)", format_dashboard_percent(percent), count_text)
}

render_dashboard_classifier_card <- function(classifier_summary,
                                             detail_input_id = "dashboard_classifier_info_clicked") {
  classifier_rows <- classifier_summary$classifiers
  info_link <- render_dashboard_info_link(
    "classified_by_info",
    detail_input_id,
    title = "About classifiedBy"
  )

  if (is.null(classifier_rows) || nrow(classifier_rows) == 0) {
    return(card(
      card_header(render_dashboard_card_header("user-check", "Classified By")),
      card_body(
        div(
          class = "dashcard-metric-state dashcard-metric-state-plain",
          div(class = "dashcard-card-action", info_link),
          div("N/A", class = "dashcard-output"),
          div("No classified observations", class = "dashcard-period")
        )
      ),
      full_screen = FALSE
    ))
  }

  top_classifier <- classifier_rows[1, , drop = FALSE]
  contributor_rows <- classifier_rows[-1, , drop = FALSE]
  contributor_limit <- dashboard_numeric_assumption(
    dashboard_config_global("dashboard_classifier_contributor_limit", 4),
    4
  )
  contributor_limit <- max(0, as.integer(contributor_limit))
  if (nrow(contributor_rows) > contributor_limit) {
    remaining_rows <- contributor_rows[(contributor_limit + 1):nrow(contributor_rows), , drop = FALSE]
    contributor_rows <- contributor_rows[seq_len(contributor_limit), , drop = FALSE]
    contributor_rows <- rbind(
      contributor_rows,
      data.frame(
        classifier_label = sprintf("%d others", nrow(remaining_rows)),
        observations = sum(remaining_rows$observations),
        percent = sum(remaining_rows$percent),
        stringsAsFactors = FALSE
      )
    )
  }

  card(
    card_header(render_dashboard_card_header("user-check", "Classified By")),
    card_body(
      div(
        class = "dashcard-metric-state dashcard-metric-state-plain dashboard-classifier-card",
        div(class = "dashcard-card-action", info_link),
        div(top_classifier$classifier_label[[1]], class = "dashcard-output dashboard-classifier-primary-name"),
        div(
          format_dashboard_classifier_share(
            top_classifier$percent[[1]],
            top_classifier$observations[[1]],
            include_label = TRUE
          ),
          class = "dashcard-period dashboard-classifier-top-label"
        ),
        div(
          sprintf("%s total", format_dashboard_classifier_count(classifier_summary$total_observations)),
          class = "dashcard-period"
        ),
        if (nrow(contributor_rows) > 0) {
          div(
            class = "dashboard-classifier-contributors",
            lapply(seq_len(nrow(contributor_rows)), function(row_index) {
              row <- contributor_rows[row_index, , drop = FALSE]
              div(
                class = "dashboard-classifier-contributor",
                tags$span(row$classifier_label[[1]], class = "dashboard-classifier-name"),
                tags$span(
                  format_dashboard_classifier_share(row$percent[[1]], row$observations[[1]]),
                  class = "dashboard-classifier-percent"
                )
              )
            })
          )
        }
      )
    ),
    full_screen = FALSE
  )
}

dashboard_detail_table_row <- function(label, value) {
  tags$tr(
    tags$th(scope = "row", label),
    tags$td(value)
  )
}

show_dashboard_volunteer_hours_modal <- function(locality = NULL, period_name = NULL) {
  effort_summary <- summarise_dashboard_effort(locality = locality, period_name = period_name)
  volunteer_hours <- estimate_dashboard_volunteer_hours(effort_summary)
  assumptions <- volunteer_hours$assumptions

  showModal(modalDialog(
    title = "Estimated Volunteer Hours",
    tags$p(
      sprintf(
        "%s, %s.",
        effort_summary$period_label,
        locality_scope_label(locality)
      )
    ),
    tags$table(
      class = "table table-sm dashboard-detail-table",
      tags$tbody(
        dashboard_detail_table_row("Total", format_dashboard_hours(volunteer_hours$total_hours)),
        dashboard_detail_table_row("Field work", format_dashboard_hours(volunteer_hours$field_work_hours)),
        dashboard_detail_table_row("Deployment setup", sprintf(
          "%s deployments x %s min = %s",
          format(volunteer_hours$deployments_count, big.mark = ","),
          format_dash_number(assumptions$deployment_setup_minutes),
          format_dashboard_hours(volunteer_hours$setup_hours)
        )),
        dashboard_detail_table_row("Deployment retrieval", sprintf(
          "%s deployments x %s min = %s",
          format(volunteer_hours$deployments_count, big.mark = ","),
          format_dash_number(assumptions$deployment_retrieval_minutes),
          format_dashboard_hours(volunteer_hours$retrieval_hours)
        )),
        dashboard_detail_table_row("Data and annotation", format_dashboard_hours(volunteer_hours$data_annotation_hours)),
        dashboard_detail_table_row("Camera data check and upload", sprintf(
          "%s deployments x %s min = %s",
          format(volunteer_hours$deployments_count, big.mark = ","),
          format_dash_number(assumptions$data_check_upload_minutes_per_deployment),
          format_dashboard_hours(volunteer_hours$data_check_upload_hours)
        )),
        dashboard_detail_table_row("Annotations", sprintf(
          "%s observations x %s sec = %s",
          format(volunteer_hours$observations_count, big.mark = ","),
          format_dash_number(assumptions$annotation_seconds_per_observation),
          format_dashboard_hours(volunteer_hours$annotation_hours)
        ))
      )
    ),
    tags$p(
      class = "text-muted",
        "This is a calculated estimate of the hours required for the ongoing core 
        operation of the camera monitoring network. 
        
        It excludes the initial planning, development, and setup phases, as well as 
        ongoing reporting and analysis of observations and related activities such as 
        community engagement."
     
    ),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
}

show_dashboard_classifier_info_modal <- function() {
  showModal(modalDialog(
    title = "Classified By",
    tags$p(
      "Classified by is the primary classifier recorded on each observation in the selected dashboard period and locality."
    ),
    tags$p(
      "Percentages do not factor in validation checks or confirmations."
    ),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
}

render_dashboard_period_dates_card <- function(effort_summary, title = "Season Dates") {
  card(
    card_header(render_dashboard_card_header("calendar-days", title)),
    card_body(
      render_dashcard_metric_body(
        effort_summary$period_date_text,
        div(effort_summary$period_label, class = "dashcard-period"),
        div("actual deployment span", class = "dashcard-period")
      )
    ),
    full_screen = FALSE
  )
}

render_dashboard_camera_hours_card <- function(effort_summary) {
  card(
    card_header(render_dashboard_card_header("camera", "Camera Hours")),
    card_body(
      render_dashcard_metric_body(
        format(round(effort_summary$camera_hours_total), big.mark = ","),
        div(paste(format_dash_number(effort_summary$camera_hours_total / 24), "camera days"), class = "dashcard-period"),
        div(paste(effort_summary$deployments_count, "camera deployments"), class = "dashcard-period")
      )
    ),
    full_screen = FALSE
  )
}

render_dashboard_observations_card <- function(effort_summary) {
  card(
    card_header(render_dashboard_card_header("clipboard-list", "Observations")),
    card_body(
      render_dashcard_metric_body(
        format(effort_summary$observations_count, big.mark = ","),
        div(paste(format(effort_summary$possible_duplicate_animals_count, big.mark = ","), "possible duplicate animals"), class = "dashcard-period"),
        div(paste(format(effort_summary$blanks_count, big.mark = ","), "blanks"), class = "dashcard-period"),
        div(paste(format(effort_summary$unclassified_unknown_count, big.mark = ","), "unclassified or unknown"), class = "dashcard-period")
      )
    ),
    full_screen = FALSE
  )
}

render_dashboard_animal_observations_total_card <- function(effort_summary) {
  animal_label <- if (isTRUE(get_use_net_data_setting())) {
    "net animal observations"
  } else {
    "total animal observations"
  }

  card(
    card_header(render_dashboard_card_header("paw", "Animal Observations")),
    card_body(
      render_dashcard_metric_body(
        format(effort_summary$animal_observations_count, big.mark = ","),
        div(animal_label, class = "dashcard-period")
      )
    ),
    render_count_basis_footer(),
    full_screen = FALSE
  )
}

render_dashboard_monitoring_data_card <- function() {
  card(
    card_header(render_dashboard_card_header("rotate", "Data Package")),
    card_body(
      render_dashcard_metric_body(
        format(as.POSIXct(core_data$created, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "%d/%m/%Y"),
        div(core_data$name, class = "dashcard-period")
      )
    ),
    full_screen = FALSE
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

render_dashboard_rai_cards <- function(locality = NULL,
                                       period_name = NULL,
                                       detail_input_id = "dashboard_rai_details_clicked") {
  locality_token <- if (is.null(locality)) "ALL" else paste(locality, collapse = ",")
  period_token <- if (is.null(period_name)) "ALL" else period_name
  detail_token <- function(group) paste(group, locality_token, period_token, sep = "|")

  mustelid_metric <- dashboard_rai_metric("Mustelids", lower_is_better = TRUE, locality = locality, period_name = period_name)
  rat_metric <- if ("Rats" %in% names(config$globals$rai_groups)) {
    dashboard_rai_metric("Rats", lower_is_better = TRUE, locality = locality, period_name = period_name)
  } else {
    NULL
  }
  kiwi_metric <- dashboard_rai_metric("Kiwi", lower_is_better = FALSE, locality = locality, period_name = period_name)

  layout_column_wrap(
    width = "180px",
    card(
      card_header(render_dashboard_card_header("otter", "Mustelid RAI")),
      card_body(render_dashboard_comparison_body(mustelid_metric, detail_token("Mustelids"), detail_input_id = detail_input_id)),
      render_count_basis_footer(),
      full_screen = FALSE
    ),
    if (!is.null(rat_metric)) {
      card(
        card_header(render_dashboard_card_header(NULL, "Rat RAI", custom_icon = dashboard_rat_icon())),
        card_body(render_dashboard_comparison_body(rat_metric, detail_token("Rats"), detail_input_id = detail_input_id)),
        render_count_basis_footer(),
        full_screen = FALSE
      )
    },
    card(
      card_header(render_dashboard_card_header("kiwi-bird", "Kiwi RAI")),
      card_body(render_dashboard_comparison_body(kiwi_metric, detail_token("Kiwi"), detail_input_id = detail_input_id)),
      render_count_basis_footer(),
      full_screen = FALSE
    )
  )
}

dashboard_favourite_image_records <- function(max_images = 30,
                                             context = "period",
                                             period_name = NULL,
                                             species = NULL) {
  manifest_path <- file.path("www", "cache", "images", "favourites", "_manifest.csv")

  if (!file.exists(manifest_path)) {
    return(data.frame())
  }

  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  if (!all(c("web_path", "context", "observationID") %in% names(manifest))) {
    return(data.frame())
  }

  manifest <- manifest[manifest$context == context, , drop = FALSE]
  manifest <- manifest[!is.na(manifest$observationID) & nzchar(manifest$observationID), , drop = FALSE]
  if (!is.null(period_name) && "period" %in% names(manifest)) {
    manifest <- manifest[manifest$period == period_name, , drop = FALSE]
  }
  if (!is.null(species) && "scientificName" %in% names(manifest)) {
    manifest <- manifest[tolower(manifest$scientificName) == tolower(species), , drop = FALSE]
  }

  if (nrow(manifest) == 0) {
    return(manifest)
  }

  file_paths <- file.path("www", manifest$web_path)
  manifest$file_mtime <- as.POSIXct(file.info(file_paths)$mtime)
  manifest <- manifest[order(manifest$file_mtime, decreasing = TRUE), , drop = FALSE]
  manifest <- head(manifest, max_images)
  manifest$src <- vapply(
    strsplit(manifest$web_path, "/", fixed = TRUE),
    function(path_parts) paste(utils::URLencode(path_parts, reserved = TRUE), collapse = "/"),
    character(1)
  )

  manifest
}

render_dashboard_favourites_hero <- function(max_images = 30,
                                             context = "period",
                                             period_name = NULL,
                                             species = NULL,
                                             slider_id = "dashboard_favourites_hero_slider") {
  image_records <- dashboard_favourite_image_records(
    max_images = max_images,
    context = context,
    period_name = period_name,
    species = species
  )

  if (nrow(image_records) == 0) {
    return(NULL)
  }

  div(
    class = "dashboard-favourites-hero",
    div(
      id = slider_id,
      class = "dashboard-favourites-slider",
      lapply(seq_len(nrow(image_records)), function(image_index) {
        observation_id <- image_records$observationID[[image_index]]
        click_payload <- jsonlite::toJSON(list(observation_ids = c(observation_id)), auto_unbox = TRUE)
        div(
          class = "dashboard-favourites-slide",
          tags$button(
            type = "button",
            class = "dashboard-favourites-image-button",
            title = "Review sequence",
            onclick = sprintf(
              "pauseDashboardHeroSlider('%s'); Shiny.setInputValue('review_sequences_click', %s, {priority: 'event'});",
              slider_id,
              click_payload
            ),
            tags$img(
              src = image_records$src[[image_index]],
              alt = sprintf("Favourite observation image %d", image_index),
              loading = "lazy"
            )
          )
        )
      })
    ),
    tags$script(HTML(sprintf("initDashboardHeroSlider('%s');", slider_id)))
  )
}

render_dashboard_monitoring_effort_cards <- function(effort_summary,
                                                     classifier_summary,
                                                     detail_token,
                                                     volunteer_detail_input_id = "dashboard_volunteer_hours_details_clicked",
                                                     classifier_info_input_id = "dashboard_classifier_info_clicked",
                                                     dates_title = "Season Dates") {
  layout_column_wrap(
    width = "180px",
    render_dashboard_period_dates_card(effort_summary, title = dates_title),
    render_dashboard_camera_hours_card(effort_summary),
    render_dashboard_volunteer_hours_card(
      effort_summary,
      detail_token = detail_token,
      detail_input_id = volunteer_detail_input_id
    ),
    render_dashboard_classifier_card(
      classifier_summary,
      detail_input_id = classifier_info_input_id
    )
  )
}

render_dashboard_period_core_output_cards <- function(effort_summary, animal_metric) {
  layout_column_wrap(
    width = "180px",
    render_dashboard_observations_card(effort_summary),
    card(
      card_header(render_dashboard_card_header("paw", "Animal Observations")),
      card_body(render_dashboard_comparison_body(animal_metric, use_state_background = FALSE)),
      render_count_basis_footer(),
      full_screen = FALSE
    )
  )
}

render_dashboard_whole_project_core_output_cards <- function(effort_summary) {
  layout_column_wrap(
    width = "180px",
    render_dashboard_observations_card(effort_summary),
    render_dashboard_animal_observations_total_card(effort_summary)
  )
}

render_dashboard_project_section_heading <- function(label) {
  div(class = "dashboard-section-heading dashboard-current-period-heading", label)
}

render_dashboard_period_project_work_sections <- function(locality = NULL,
                                                         period_name = NULL,
                                                         volunteer_detail_input_id = "dashboard_volunteer_hours_details_clicked",
                                                         classifier_info_input_id = "dashboard_classifier_info_clicked") {
  animal_metric <- dashboard_animal_detections_metric(locality = locality, period_name = period_name)
  effort_summary <- summarise_dashboard_effort(locality = locality, period_name = period_name)
  classifier_summary <- summarise_dashboard_classifiers(locality = locality, period_name = period_name)
  detail_token <- dashboard_selection_detail_token(locality = locality, period_name = period_name)

  tagList(
    render_dashboard_project_section_heading("MONITORING EFFORT"),
    render_dashboard_monitoring_effort_cards(
      effort_summary,
      classifier_summary,
      detail_token = detail_token,
      volunteer_detail_input_id = volunteer_detail_input_id,
      classifier_info_input_id = classifier_info_input_id
    ),
    render_dashboard_project_section_heading("CORE OUTPUTS"),
    render_dashboard_period_core_output_cards(effort_summary, animal_metric)
  )
}

render_dashboard_whole_project_sections <- function(locality = NULL,
                                                    volunteer_detail_input_id = "dashboard_volunteer_hours_details_clicked",
                                                    classifier_info_input_id = "dashboard_classifier_info_clicked") {
  effort_summary <- summarise_dashboard_effort(locality = locality)
  classifier_summary <- summarise_dashboard_classifiers(locality = locality)
  detail_token <- dashboard_selection_detail_token(locality = locality)

  tagList(
    render_dashboard_project_section_heading("MONITORING EFFORT"),
    render_dashboard_monitoring_effort_cards(
      effort_summary,
      classifier_summary,
      detail_token = detail_token,
      volunteer_detail_input_id = volunteer_detail_input_id,
      classifier_info_input_id = classifier_info_input_id,
      dates_title = "Project Dates"
    ),
    render_dashboard_project_section_heading("CORE OUTPUTS"),
    render_dashboard_whole_project_core_output_cards(effort_summary)
  )
}

format_dashboard_data_package_created <- function(data) {
  created <- data$created
  if (is.null(created) || length(created) == 0 || is.na(created[[1]]) || !nzchar(created[[1]])) {
    return("Not available")
  }

  created <- created[[1]]
  created_date <- as.POSIXct(created, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.na(created_date)) {
    return(created)
  }

  format(created_date, "%d/%m/%Y")
}

format_dashboard_data_package_name <- function(data) {
  package_name <- data$name
  if (is.null(package_name) || length(package_name) == 0 || is.na(package_name[[1]]) || !nzchar(package_name[[1]])) {
    return("Not available")
  }

  package_name[[1]]
}

render_dashboard_data_package_settings_rows <- function(data) {
  tagList(
    tags$tr(
      tags$th(scope = "row", "Data package"),
      tags$td(format_dashboard_data_package_name(data))
    ),
    tags$tr(
      tags$th(scope = "row", "Data package created"),
      tags$td(format_dashboard_data_package_created(data))
    )
  )
}

rai_basis_filename <- function(label) {
  safe_label <- gsub("[^A-Za-z0-9]+", "-", label)
  safe_label <- gsub("(^-+|-+$)", "", safe_label)
  paste0(tolower(safe_label), "-rai-source-data.csv")
}

rai_metric_source_rows <- function(period_metric,
                                   period_label,
                                   rai_group,
                                   species_included,
                                   scope_label = NULL) {
  metric_value <- function(name, default = NA) {
    value <- period_metric[[name]]
    if (is.null(value) || length(value) == 0) {
      return(default)
    }
    if (is.data.frame(value)) {
      return(value)
    }

    value[[1]]
  }

  row_from_values <- function(row_type,
                              locality = NA_character_,
                              line = NA_character_,
                              rai = NA_real_,
                              se = NA_real_,
                              formatted_value = NA_character_,
                              animal_detections = NA_real_,
                              individuals_count = NA_real_,
                              possible_duplicates_count = NA_real_,
                              net_individuals_count = NA_real_,
                              camera_hours = NA_real_,
                              line_count = NA_integer_,
                              locality_count = NA_integer_,
                              calculation_trace = NA_character_) {
    data.frame(
      period_label = period_label,
      period = metric_value("period", NA_character_),
      start_date = as.character(metric_value("start_date", NA)),
      end_date = as.character(metric_value("end_date", NA)),
      rai_group = rai_group,
      species_included = species_included,
      locality_scope = if (is.null(scope_label)) NA_character_ else scope_label,
      count_basis = if (isTRUE(metric_value("use_net", TRUE))) "net_individuals_count" else "individuals_count",
      rai_norm_hours = metric_value("rai_norm_hours", NA_real_),
      row_type = row_type,
      locality = locality,
      line = line,
      rai = rai,
      se = se,
      formatted_value = formatted_value,
      animal_detections = animal_detections,
      individuals_count = individuals_count,
      possible_duplicates_count = possible_duplicates_count,
      net_individuals_count = net_individuals_count,
      camera_hours = camera_hours,
      line_count = line_count,
      locality_count = locality_count,
      calculation_trace = calculation_trace,
      stringsAsFactors = FALSE
    )
  }

  rows <- list(row_from_values(
    row_type = "period_result",
    rai = metric_value("value", NA_real_),
    se = metric_value("se", NA_real_),
    formatted_value = metric_value("formatted_value", NA_character_),
    animal_detections = metric_value("animal_detections", NA_real_),
    individuals_count = metric_value("individuals_count", NA_real_),
    possible_duplicates_count = metric_value("possible_duplicates_count", NA_real_),
    net_individuals_count = metric_value("net_individuals_count", NA_real_),
    camera_hours = metric_value("camera_hours", NA_real_),
    line_count = metric_value("line_count", NA_integer_),
    locality_count = metric_value("locality_count", NA_integer_),
    calculation_trace = metric_value("calculation_trace", NA_character_)
  ))

  locality_values <- metric_value("locality_rai_values", NULL)
  if (!is.null(locality_values) && nrow(locality_values) > 0) {
    rows <- c(rows, lapply(seq_len(nrow(locality_values)), function(i) {
      locality_row <- locality_values[i, , drop = FALSE]
      row_from_values(
        row_type = "locality_summary",
        locality = as.character(locality_row$locality[[1]]),
        rai = if ("locality_rai" %in% names(locality_row)) as.numeric(locality_row$locality_rai[[1]]) else NA_real_,
        se = if ("se" %in% names(locality_row)) as.numeric(locality_row$se[[1]]) else NA_real_,
        formatted_value = as.character(locality_row$formatted_value[[1]]),
        animal_detections = if ("animal_detections" %in% names(locality_row)) as.numeric(locality_row$animal_detections[[1]]) else NA_real_,
        individuals_count = if ("individuals_count" %in% names(locality_row)) as.numeric(locality_row$individuals_count[[1]]) else NA_real_,
        possible_duplicates_count = if ("possible_duplicates_count" %in% names(locality_row)) as.numeric(locality_row$possible_duplicates_count[[1]]) else NA_real_,
        net_individuals_count = if ("net_individuals_count" %in% names(locality_row)) as.numeric(locality_row$net_individuals_count[[1]]) else NA_real_,
        camera_hours = if ("camera_hours" %in% names(locality_row)) as.numeric(locality_row$camera_hours[[1]]) else NA_real_,
        line_count = if ("line_count" %in% names(locality_row)) as.integer(locality_row$line_count[[1]]) else NA_integer_,
        calculation_trace = if ("calculation_trace" %in% names(locality_row)) as.character(locality_row$calculation_trace[[1]]) else NA_character_
      )
    }))
  }

  line_values <- metric_value("line_rai_values", NULL)
  if (!is.null(line_values) && nrow(line_values) > 0) {
    rows <- c(rows, lapply(seq_len(nrow(line_values)), function(i) {
      line_row <- line_values[i, , drop = FALSE]
      row_from_values(
        row_type = "line_input",
        locality = as.character(line_row$locality[[1]]),
        line = as.character(line_row$line[[1]]),
        rai = as.numeric(line_row$line_rai[[1]]),
        formatted_value = as.character(line_row$formatted_value[[1]]),
        animal_detections = as.numeric(line_row$animal_detections[[1]]),
        individuals_count = as.numeric(line_row$individuals_count[[1]]),
        possible_duplicates_count = as.numeric(line_row$possible_duplicates_count[[1]]),
        net_individuals_count = as.numeric(line_row$net_individuals_count[[1]]),
        camera_hours = as.numeric(line_row$camera_hours[[1]]),
        calculation_trace = as.character(line_row$calculation_trace[[1]])
      )
    }))
  }

  do.call(rbind, rows)
}

rai_basis_csv_text <- function(rows) {
  character_columns <- vapply(rows, is.character, logical(1))
  rows[character_columns] <- lapply(rows[character_columns], function(column) {
    gsub("\u00b1", "+/-", column, fixed = TRUE)
  })

  csv_lines <- character()
  csv_connection <- textConnection("csv_lines", "w", local = TRUE)
  on.exit(close(csv_connection), add = TRUE)
  utils::write.csv(rows, csv_connection, row.names = FALSE, na = "")
  paste(csv_lines, collapse = "\n")
}

rai_basis_csv_download_link <- function(rows, filename) {
  csv <- rai_basis_csv_text(rows)

  tags$a(
    href = paste0("data:text/csv;charset=utf-8,", utils::URLencode(csv, reserved = TRUE)),
    download = filename,
    class = "btn btn-sm btn-outline-secondary rai-source-download",
    title = "Download source data for checking this RAI calculation",
    icon("download"),
    " Download source CSV"
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

    tbody_rows <- list()

    for (locality in localities) {
      # Get unique lines for this locality across all periods
      locality_lines <- unique(unlist(lapply(period_columns, function(column) {
        line_values <- column$metric$line_rai_values
        if (is.null(line_values) || nrow(line_values) == 0) return(character())
        as.character(line_values$line[line_values$locality == locality])
      })))

      # Add line rows
      for (line in sort(locality_lines)) {
        row_cells <- list(
          tags$td(locality_display_name(locality)),
          tags$td(line)
        )

        for (column in period_columns) {
          line_values <- column$metric$line_rai_values
          val <- "N/A"
          if (!is.null(line_values) && nrow(line_values) > 0) {
            matching_line <- line_values[line_values$locality == locality & line_values$line == line, , drop = FALSE]
            if (nrow(matching_line) > 0) {
              val <- matching_line$formatted_value[[1]]
            }
          }
          row_cells <- append(row_cells, list(tags$td(val)))
        }

        tbody_rows <- append(tbody_rows, list(tags$tr(row_cells)))
      }

      # Add locality subtotal row
      subtotal_cells <- list(
        tags$td(locality_display_name(locality)),
        tags$td("Locality RAI")
      )

      for (column in period_columns) {
        val <- get_locality_period_rai(column$metric, locality)
        subtotal_cells <- append(subtotal_cells, list(tags$td(val)))
      }

      tbody_rows <- append(tbody_rows, list(tags$tr(class = "fw-bold", style = "border-top: 2px solid #dee2e6; border-bottom: 2px solid #dee2e6;", subtotal_cells)))
    }

    tags$table(
      class = "table table-sm table-striped rai-detail-table",
      tags$thead(tags$tr(
        tags$th("Locality", style = "width: 25%;"),
        tags$th("Line", style = "width: 15%;"),
        lapply(period_columns, function(column) tags$th(column$label, style = "width: 20%;"))
      )),
      tags$tbody(tbody_rows)
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

  modal_subtitle <- if (!is.null(metric$scope_label) && metric$scope_label != "Combined localities") {
    metric$scope_label
  } else {
    NULL
  }

  detail_token <- paste(metric$rai_group, metric$locality_filter, metric$period_filter, sep = "|")
  share_btn <- tags$button(
    type = "button",
    class = "btn btn-sm btn-outline-secondary",
    onclick = sprintf("copyRaiBasisUrl(%s, this)", jsonlite::toJSON(detail_token, auto_unbox = TRUE)),
    title = "Share this view",
    icon("share-nodes"),
    " Share"
  )

  source_rows <- do.call(rbind, lapply(period_columns, function(column) {
    rai_metric_source_rows(
      period_metric = column$metric,
      period_label = column$label,
      rai_group = metric$rai_group,
      species_included = paste(config$globals$rai_groups[[metric$rai_group]], collapse = ", "),
      scope_label = metric$scope_label
    )
  }))
  csv_link <- rai_basis_csv_download_link(
    source_rows,
    rai_basis_filename(paste(metric$rai_group, "rai source data"))
  )

  showModal(modalDialog(
    title = tagList(
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: flex-start; width: 100%; padding-right: 20px; gap: 12px;",
        tags$div(
          tags$div(paste(metric$rai_group, "RAI calculation basis")),
          if (!is.null(modal_subtitle)) {
            tags$div(modal_subtitle, style = "font-size: 0.9rem; font-weight: 400; margin-top: 2px;")
          }
        ),
        share_btn
      )
    ),
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
    tags$details(
      class = "rai-detail-section",
      tags$summary("Period Inputs"),
      tags$div(class = "rai-table-scroll", render_period_table(period_input_rows))
    ),
    tags$div(class = "rai-source-download-row", csv_link),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l"
  ))
}
