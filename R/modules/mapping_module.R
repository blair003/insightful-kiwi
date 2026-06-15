# R/modules/mapping_module.R

timeline_ui <- function(ns, points, value, step_size = "day") {
  point_count <- length(points)
  selected_index <- timeline_index_for_time(points, value)
  slider_id <- ns("time_slider")
  slider_labels <- timeline_slider_labels(points, step_size)
  transport <- div(
    class = "playback-transport",
    actionButton(ns("play_btn"), "Play", icon = icon("play"), class = "btn-success playback-transport-btn"),
    shinyjs::disabled(
      actionButton(ns("pause_btn"), "Pause", icon = icon("pause"), class = "btn-warning playback-transport-btn")
    ),
    actionButton(
      ns("reset_btn"),
      label = NULL,
      icon = icon("rotate-left"),
      class = "btn-danger playback-transport-btn playback-reset-btn",
      title = "Reset progression"
    )
  )

  if (point_count <= 1) {
    return(div(
      class = "timeline-controls",
      transport,
      div(
        class = "timeline-slider timeline-single-point-slider",
        sliderInput(
          inputId = slider_id,
          label = "Time progression",
          min = 1,
          max = 2,
          value = 1,
          step = 1,
          width = "100%",
          ticks = FALSE,
          sep = ""
        ),
        tags$script(HTML(sprintf(
          "(function() {
            var sliderId = %s;
            var label = %s;
            var attempts = 0;
            function applySinglePointSlider() {
              var el = document.getElementById(sliderId);
              var slider = el && window.jQuery ? window.jQuery(el).data('ionRangeSlider') : null;
              if (!slider) {
                if (attempts++ < 40) window.setTimeout(applySinglePointSlider, 50);
                return;
              }
              slider.update({
                from: 1,
                disable: true,
                prettify: function() { return label; }
              });
              window.jQuery(el).closest('.shiny-input-container').find('.irs-single').text(label);
              Shiny.setInputValue(sliderId, 1, {priority: 'event'});
            }
            applySinglePointSlider();
          })();",
          jsonlite::toJSON(slider_id, auto_unbox = TRUE),
          jsonlite::toJSON(slider_labels[[1]], auto_unbox = TRUE)
        )))
      )
    ))
  }

  div(
    class = "timeline-controls",
    transport,
    div(
      class = "timeline-slider",
      sliderInput(
        inputId = slider_id,
        label = "Time progression",
        min = 1,
        max = point_count,
        value = selected_index,
        step = 1,
        width = "100%",
        ticks = FALSE,
        sep = ""
      ),
      tags$script(HTML(sprintf(
        "(function() {
          var sliderId = %s;
          var labels = %s;
          var attempts = 0;
          var bound = false;
          function labelFor(value) {
            var index = Math.round(Number(value)) - 1;
            return labels[index] || String(value);
          }
          function updateVisibleLabel(el) {
            if (!el || !window.jQuery) return;
            var $el = window.jQuery(el);
            var slider = $el.data('ionRangeSlider');
            var value = slider && slider.result ? slider.result.from : el.value;
            $el.closest('.shiny-input-container').find('.irs-single').text(labelFor(value));
          }
          function applyTimelineLabels() {
            var el = document.getElementById(sliderId);
            var slider = el && window.jQuery ? window.jQuery(el).data('ionRangeSlider') : null;
            if (!slider) {
              if (attempts++ < 40) window.setTimeout(applyTimelineLabels, 50);
              return;
            }
            slider.update({
              prettify: function(value) {
                return labelFor(value);
              }
            });
            if (!bound) {
              bound = true;
              window.jQuery(el).on('input change', function() { updateVisibleLabel(el); });
            }
            updateVisibleLabel(el);
            window.setTimeout(function() { updateVisibleLabel(el); }, 0);
            window.setTimeout(function() { updateVisibleLabel(el); }, 100);
          }
          applyTimelineLabels();
        })();",
        jsonlite::toJSON(slider_id, auto_unbox = TRUE),
        jsonlite::toJSON(slider_labels, auto_unbox = TRUE)
      )))
    )
  )
}

surface_basis_label <- function(ns) {
  tags$span(
    class = "surface-basis-label",
    "Surface basis:",
    actionLink(
      ns("predicted_rai_surface_basis_info"),
      label = NULL,
      icon = icon("circle-info"),
      class = "surface-basis-info-link",
      title = "About surface basis"
    )
  )
}

map_option_info_label <- function(ns, label, input_id, title) {
  tags$span(
    class = "surface-basis-label",
    label,
    actionLink(
      ns(input_id),
      label = NULL,
      icon = icon("circle-info"),
      class = "surface-basis-info-link",
      title = title
    )
  )
}

trapping_records_label <- function(ns) {
  map_option_info_label(
    ns,
    "Trapping records",
    "include_trap_data_info",
    "About trapping records"
  )
}

trap_check_counters_label <- function(ns) {
  map_option_info_label(
    ns,
    "Trap check counters",
    "trap_check_counters_info",
    "About trap check counters"
  )
}

unchecked_traps_label <- function(ns) {
  map_option_info_label(
    ns,
    "Unchecked traps",
    "unchecked_traps_info",
    "About unchecked traps"
  )
}

timeline_actual_timezone <- function() {
  if (exists("weather_timeline_timezone", mode = "function", inherits = TRUE)) {
    return(weather_timeline_timezone())
  }

  if (exists("config", inherits = TRUE) &&
      !is.null(config$globals$actual_timezone) &&
      nzchar(config$globals$actual_timezone)) {
    return(config$globals$actual_timezone)
  }

  "Pacific/Auckland"
}

timeline_as_posix <- function(value, points = NULL) {
  if (!is.null(points)) {
    return(timeline_selected_time(value, points))
  }

  as.POSIXct(value, tz = timeline_actual_timezone())
}

timeline_selected_index <- function(value, point_count) {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(1L)
  }

  max(1L, min(point_count, as.integer(round(as.numeric(value)))))
}

timeline_selected_time <- function(value, points) {
  if (is.null(points) || length(points) == 0) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = timeline_actual_timezone()))
  }

  points[[timeline_selected_index(value, length(points))]]
}

timeline_index_for_time <- function(points, value) {
  if (is.null(points) || length(points) == 0) {
    return(1L)
  }

  value <- as.POSIXct(value, tz = timeline_actual_timezone())
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(1L)
  }

  which.min(abs(as.numeric(points) - as.numeric(value)))
}

timeline_slider_labels <- function(points, step_size) {
  timezone <- timeline_actual_timezone()
  format_string <- if (step_size %in% c("day", "week", "month", "season")) {
    "%Y-%m-%d"
  } else {
    "%Y-%m-%d %H:%M"
  }

  format(as.POSIXct(points, tz = timezone), format_string, tz = timezone)
}

timeline_update_slider <- function(session, input_id, points, value) {
  updateSliderInput(session, input_id, value = timeline_index_for_time(points, value))
}

timeline_index_at_or_after_time <- function(points, value) {
  if (is.null(points) || length(points) == 0) {
    return(1L)
  }

  value <- as.POSIXct(value, tz = timeline_actual_timezone())
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(1L)
  }

  candidates <- which(as.numeric(points) >= as.numeric(value))
  if (length(candidates) == 0) {
    return(length(points))
  }

  candidates[[1]]
}

timeline_skip_slider_to_time <- function(session, input_id, points, value) {
  updateSliderInput(session, input_id, value = timeline_index_at_or_after_time(points, value))
}

timeline_as_date <- function(value) {
  if (inherits(value, "POSIXt")) {
    return(as.Date(value, tz = timeline_actual_timezone()))
  }

  as.Date(value)
}

timeline_date_midnight <- function(date_value) {
  timezone <- timeline_actual_timezone()
  as.POSIXct(
    paste(format(timeline_as_date(date_value), "%Y-%m-%d"), "00:00:00"),
    tz = timezone
  )
}

timeline_date_end <- function(date_value) {
  timezone <- timeline_actual_timezone()
  as.POSIXct(
    paste(format(timeline_as_date(date_value), "%Y-%m-%d"), "23:59:59"),
    tz = timezone
  )
}

timeline_day_start <- function(value, fallback_start = NULL) {
  timezone <- timeline_actual_timezone()
  value <- as.POSIXct(value, tz = timeline_actual_timezone())
  if (is.null(value) || is.na(value)) {
    return(fallback_start)
  }

  day_start <- as.POSIXct(
    paste(format(value, "%Y-%m-%d", tz = timezone), "00:00:00"),
    tz = timezone
  )

  day_start
}

timeline_day_end <- function(value, timeline_end = NULL) {
  timezone <- timeline_actual_timezone()
  value <- as.POSIXct(value, tz = timeline_actual_timezone())
  if (is.null(value) || is.na(value)) {
    return(timeline_end)
  }

  day_end <- as.POSIXct(
    paste(format(value, "%Y-%m-%d", tz = timezone), "23:59:59"),
    tz = timezone
  )
  if (!is.null(timeline_end) && !is.na(timeline_end)) {
    day_end <- min(day_end, as.POSIXct(timeline_end, tz = timeline_actual_timezone()))
  }

  day_end
}

timeline_next_day_end <- function(current_time, timeline_end) {
  timezone <- timeline_actual_timezone()
  current_time <- as.POSIXct(current_time, tz = timeline_actual_timezone())
  current_day_end <- timeline_day_end(current_time, timeline_end)
  if (!is.null(current_day_end) && !is.na(current_day_end) && current_time < current_day_end) {
    return(current_day_end)
  }

  current_date <- as.Date(format(current_time, "%Y-%m-%d", tz = timezone))
  timeline_day_end(timeline_date_midnight(current_date + 1), timeline_end)
}

timeline_next_step_time <- function(current_time, timeline_end, step_size, weather_df = NULL, period_groups = NULL) {
  current_time <- as.POSIXct(current_time, tz = timeline_actual_timezone())
  timeline_end <- as.POSIXct(timeline_end, tz = timeline_actual_timezone())

  if (step_size %in% c("day_night", "diel")) {
    return(next_weather_boundary(current_time, timeline_end, weather_df, step_size))
  }

  if (identical(step_size, "season")) {
    return(timeline_next_season_time(current_time, timeline_end, period_groups))
  }

  if (identical(step_size, "day")) {
    return(timeline_next_day_end(current_time, timeline_end))
  }

  current_time + switch(step_size,
    "hour" = 3600,
    "week" = 604800,
    "month" = 2592000,
    86400
  )
}

timeline_time_points <- function(start_time, end_time, step_size, view_mode = "cumulative",
                                 weather_df = NULL, period_groups = NULL) {
  start_time <- as.POSIXct(start_time, tz = timeline_actual_timezone())
  end_time <- as.POSIXct(end_time, tz = timeline_actual_timezone())

  if (is.null(start_time) || is.null(end_time) || is.na(start_time) || is.na(end_time) || end_time < start_time) {
    return(start_time)
  }

  if (step_size %in% c("day_night", "diel")) {
    boundaries <- timeline_weather_boundaries(step_size, weather_df, start_time, end_time)
    points <- boundaries[boundaries > start_time & boundaries <= end_time]
    if (length(points) == 0) {
      points <- end_time
    }
    return(sort(unique(as.POSIXct(points, origin = "1970-01-01", tz = timeline_actual_timezone()))))
  }

  if (identical(step_size, "season")) {
    season_start <- timeline_date_midnight(start_time)
    season_end <- timeline_date_end(end_time)
    periods <- timeline_monitoring_periods(period_groups)
    if (nrow(periods) > 0) {
      season_points <- periods %>%
        dplyr::filter(.data$start <= season_end, .data$end >= season_start) %>%
        dplyr::mutate(point = pmin(.data$end, season_end)) %>%
        dplyr::filter(.data$point >= season_start, .data$point <= season_end) %>%
        dplyr::pull("point")

      if (length(season_points) > 0) {
        return(sort(unique(as.POSIXct(season_points, origin = "1970-01-01", tz = timeline_actual_timezone()))))
      }
    }

    return(as.POSIXct(season_end, origin = "1970-01-01", tz = timeline_actual_timezone()))
  }

  if (step_size %in% c("week", "month")) {
    step_seconds <- if (identical(step_size, "week")) 604800 else 2592000
    first_end <- min(start_time + step_seconds - 1, end_time)
    points <- first_end

    repeat {
      next_time <- tail(points, 1) + step_seconds
      if (is.null(next_time) || is.na(next_time) || next_time > end_time || next_time <= tail(points, 1)) {
        break
      }
      points <- c(points, next_time)
      if (length(points) > 20000) {
        break
      }
    }

    points <- points[!is.na(points) & points <= end_time]
    return(sort(unique(as.POSIXct(points, origin = "1970-01-01", tz = timeline_actual_timezone()))))
  }

  points <- timeline_next_step_time(start_time, end_time, step_size, weather_df, period_groups)

  repeat {
    next_time <- timeline_next_step_time(tail(points, 1), end_time, step_size, weather_df, period_groups)
    if (is.null(next_time) || is.na(next_time) || next_time > end_time || next_time <= tail(points, 1)) {
      break
    }
    points <- c(points, next_time)
    if (length(points) > 20000) {
      break
    }
  }

  points <- points[!is.na(points) & points <= end_time]
  if (length(points) == 0) {
    points <- end_time
  }

  sort(unique(as.POSIXct(points, origin = "1970-01-01", tz = timeline_actual_timezone())))
}

timeline_effective_current_time <- function(value, step_size, timeline_end = NULL, points = NULL) {
  value <- timeline_as_posix(value, points)
  if (identical(step_size, "day")) {
    return(timeline_day_end(value, timeline_end))
  }

  value
}

timeline_single_window_start <- function(current_time, timeline_start, step_size, step_seconds) {
  if (identical(step_size, "day")) {
    return(timeline_day_start(current_time, timeline_start))
  }

  if (step_size %in% c("week", "month")) {
    return(max(current_time - step_seconds + 1, timeline_start))
  }

  current_time - step_seconds
}

timeline_window_midpoint <- function(start_time, current_time) {
  if (is.null(start_time) || is.null(current_time) || is.na(start_time) || is.na(current_time)) {
    return(current_time)
  }

  as.POSIXct(
    mean(c(as.numeric(start_time), as.numeric(current_time))),
    origin = "1970-01-01",
    tz = timeline_actual_timezone()
  )
}

timeline_window_reference_time <- function(start_time, current_time, step_size, view_mode = "single") {
  if (identical(view_mode, "single") && step_size %in% c("day_night", "diel")) {
    return(timeline_window_midpoint(start_time, current_time))
  }

  current_time
}

timeline_time_info_for_window <- function(weather_df, start_time, current_time, step_size, view_mode = "single") {
  reference_time <- timeline_window_reference_time(start_time, current_time, step_size, view_mode)
  time_of_day_info(weather_df, reference_time, step_size)
}

timeline_format_time <- function(value) {
  format(as.POSIXct(value, tz = timeline_actual_timezone()), "%Y-%m-%d %H:%M:%S", tz = timeline_actual_timezone())
}

timeline_format_timeframe_value <- function(value, step_size) {
  if (step_size %in% c("day", "week", "month", "season")) {
    return(format(as.Date(value, tz = timeline_actual_timezone()), "%Y-%m-%d"))
  }

  timeline_format_time(value)
}

timeline_format_date <- function(value, include_year = TRUE) {
  fmt <- if (include_year) "%d %b %Y" else "%d %b"
  format(as.Date(value, tz = timeline_actual_timezone()), fmt)
}

timeline_monitoring_periods <- function(period_groups) {
  if (is.null(period_groups) || length(period_groups) == 0) {
    return(data.frame())
  }

  flat_period_groups <- flatten_period_groups(period_groups)
  period_names <- names(flat_period_groups)
  period_names <- period_names[period_names != "ALL"]
  if (length(period_names) == 0) {
    return(data.frame())
  }

  periods <- lapply(period_names, function(period_name) {
    period <- flat_period_groups[[period_name]]
    if (is.null(period$start_date) || is.null(period$end_date)) {
      return(NULL)
    }
    if (!is.null(period$assign_period) && !isTRUE(period$assign_period)) {
      return(NULL)
    }

    data.frame(
      name = period_name,
      start = timeline_date_midnight(period$start_date),
      end = timeline_date_end(period$end_date),
      stringsAsFactors = FALSE
    )
  })

  periods <- Filter(Negate(is.null), periods)
  if (length(periods) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(periods) %>%
    dplyr::filter(!is.na(start), !is.na(end)) %>%
    dplyr::arrange(start, end)
}

timeline_period_status <- function(current_time, period_groups) {
  current_time <- as.POSIXct(current_time, tz = timeline_actual_timezone())
  periods <- timeline_monitoring_periods(period_groups)

  if (is.null(current_time) || is.na(current_time) || nrow(periods) == 0) {
    return(list(
      in_period = FALSE,
      label = "No cameras deployed",
      detail = "No monitoring seasons available",
      next_start = NULL,
      next_name = NULL
    ))
  }

  current_period <- periods %>%
    dplyr::filter(start <= current_time, end >= current_time) %>%
    dplyr::slice(1)

  next_period <- periods %>%
    dplyr::filter(start > current_time) %>%
    dplyr::slice(1)

  if (nrow(current_period) > 0) {
    return(list(
      in_period = TRUE,
      label = current_period$name[[1]],
      detail = sprintf(
        "%s to %s",
        timeline_format_date(current_period$start[[1]]),
        timeline_format_date(current_period$end[[1]])
      ),
      current_end = current_period$end[[1]],
      next_start = if (nrow(next_period) > 0) next_period$start[[1]] else NULL,
      next_name = if (nrow(next_period) > 0) next_period$name[[1]] else NULL
    ))
  }

  list(
    in_period = FALSE,
    label = "No cameras deployed",
    detail = if (nrow(next_period) > 0) {
      sprintf("Next: %s from %s", next_period$name[[1]], timeline_format_date(next_period$start[[1]]))
    } else {
      "Outside monitoring seasons"
    },
    next_start = if (nrow(next_period) > 0) next_period$start[[1]] else NULL,
    next_name = if (nrow(next_period) > 0) next_period$name[[1]] else NULL
  )
}

render_timeline_period_control <- function(period_status, monitoring_window = NULL) {
  if (is.null(period_status)) {
    return(NULL)
  }

  state_class <- if (isTRUE(period_status$in_period)) "is-active" else "is-gap"
  monitoring_line <- if (!is.null(monitoring_window) &&
                          !is.na(monitoring_window$start) &&
                          !is.na(monitoring_window$end)) {
    sprintf(
      "<small>Monitoring: %s to %s</small>",
      weather_html_escape(timeline_format_date(monitoring_window$start, include_year = FALSE)),
      weather_html_escape(timeline_format_date(monitoring_window$end, include_year = FALSE))
    )
  } else {
    ""
  }

  sprintf(
    "<div class='map-season-badge %s'><span>%s</span><small>%s</small>%s</div>",
    state_class,
    weather_html_escape(period_status$label),
    weather_html_escape(period_status$detail),
    monitoring_line
  )
}

render_playback_skip_notice <- function(skip_notice) {
  if (is.null(skip_notice) || is.null(skip_notice$message)) {
    return(NULL)
  }

  sprintf(
    "<div class='map-playback-skip-badge'><strong>%s</strong><small>%s</small></div>",
    weather_html_escape(skip_notice$message),
    weather_html_escape("Playback resumes in 3 seconds")
  )
}

playback_gap_skip_target <- function(current_time, next_time, timeline_end, period_groups) {
  status <- timeline_period_status(current_time, period_groups)
  if (is.null(status$next_start) || is.na(status$next_start)) {
    return(NULL)
  }

  next_start <- as.POSIXct(status$next_start, tz = timeline_actual_timezone())
  if (next_start > timeline_end) {
    return(NULL)
  }

  if (!isTRUE(status$in_period) && next_start > current_time) {
    return(list(
      target = next_start,
      next_name = status$next_name
    ))
  }

  if (isTRUE(status$in_period) &&
      !is.null(status$current_end) &&
      next_time > status$current_end &&
      next_start > status$current_end) {
    return(list(
      target = next_start,
      next_name = status$next_name
    ))
  }

  NULL
}

timeline_next_season_time <- function(current_time, timeline_end, period_groups) {
  current_time <- as.POSIXct(current_time, tz = timeline_actual_timezone())
  timeline_end <- as.POSIXct(timeline_end, tz = timeline_actual_timezone())
  periods <- timeline_monitoring_periods(period_groups)

  if (is.null(current_time) || is.na(current_time) ||
      is.null(timeline_end) || is.na(timeline_end) ||
      nrow(periods) == 0) {
    return(timeline_end)
  }

  candidate <- periods %>%
    dplyr::filter(.data$end > current_time, .data$start <= timeline_end) %>%
    dplyr::slice(1)

  if (nrow(candidate) == 0) {
    return(timeline_end)
  }

  min(candidate$end[[1]], timeline_end)
}

timeline_current_season_start <- function(current_time, period_groups, fallback_start) {
  current_time <- as.POSIXct(current_time, tz = timeline_actual_timezone())
  fallback_start <- as.POSIXct(fallback_start, tz = timeline_actual_timezone())
  periods <- timeline_monitoring_periods(period_groups)

  if (is.null(current_time) || is.na(current_time) || nrow(periods) == 0) {
    return(fallback_start)
  }

  candidate <- periods %>%
    dplyr::filter(.data$start <= current_time, .data$end >= current_time) %>%
    dplyr::slice(1)

  if (nrow(candidate) == 0) {
    return(fallback_start)
  }

  max(candidate$start[[1]], fallback_start)
}

timeline_window_readout <- function(start_time, current_time, step_size, weather_df, view_mode = "single") {
  if (is.null(start_time) || is.null(current_time) || is.na(start_time) || is.na(current_time)) {
    return(NULL)
  }

  sprintf(
    "<div class='timeline-window-readout'><strong>Timeframe:</strong> %s to %s</div>",
    weather_html_escape(timeline_format_timeframe_value(start_time, step_size)),
    weather_html_escape(timeline_format_timeframe_value(current_time, step_size))
  )
}

mapping_module_ui <- function(id,
                              view = "map",
                              choices,
                              selected = NULL,
                              multiple = TRUE,
                              label = "Species:",
                              include_prediction_option = TRUE,
                              include_marker_options = include_prediction_option,
                              include_monitoring_area_option = FALSE,
                              include_observation_layer_options = FALSE,
                              prediction_cumulative_only = FALSE,
                              show_combined_species_note = TRUE,
                              include_species_display_mode = FALSE,
                              include_monitoring_records_default = TRUE,
                              include_trap_data_default = FALSE,
                              include_density_trap_option = TRUE,
                              exclude_untrapped_species_default = TRUE,
                              timeline_view_mode_selected = "cumulative",
                              timeline_step_size_selected = "day",
                              timeline_step_size_choices = c(
                                "Hourly" = "hour",
                                "Diel activity" = "diel",
                                "Daily - Day/Night" = "day_night",
                                "Daily" = "day",
                                "Weekly" = "week",
                                "Monthly" = "month",
                                "Season" = "season"
                              ),
                              primary_map_id = NULL,
                              comparative_map_id = NULL,
                              primary_heading_output_id = NULL,
                              comparative_heading_output_id = NULL,
                              primary_meta_output_id = NULL,
                              comparative_meta_output_id = NULL,
                              primary_title = "Selected season",
                              comparative_title = "Comparison season",
                              primary_data_label = "Primary",
                              comparative_data_label = "Comparison",
                              primary_data_title = "Primary period records",
                              comparative_data_title = "Comparison season records",
                              map_height = config$globals$leaflet_height) { # Added map_height
  ns <- NS(id)

  map_height_css <- function(value) {
    if (is.numeric(value)) {
      return(paste0(value, "px"))
    }

    as.character(value)
  }

  comparison_map_label <- function(heading_output_id, meta_output_id, title, side) {
    div(
      class = paste("map-swipe-label", paste0("map-swipe-label-", side)),
      h3(
        if (!is.null(heading_output_id)) {
          textOutput(heading_output_id, inline = TRUE)
        } else {
          title
        }
      ),
      if (!is.null(meta_output_id)) {
        uiOutput(meta_output_id)
      }
    )
  }

  comparison_data_panel <- function(map_id, title) {
    div(
      class = "map-comparison-data-panel",
      div(
        class = "map-comparison-data-heading",
        h3(title)
      ),
      DT::dataTableOutput(NS(map_id)("map_data_table"))
    )
  }
  
  if (view == "select_species") {
    return(
      tagList(
        selectizeInput(
          inputId = ns("selected_species"),
          label = tagList(icon("paw"), label),
          choices = choices,
          selected = selected,
          multiple = multiple,
          options = list(
            placeholder = "Select species...",
            closeAfterSelect = TRUE
          )
        ),
        if (isTRUE(include_species_display_mode)) {
          radioButtons(
            inputId = ns("species_display_mode"),
            label = "Species display:",
            choices = c("Combined" = "combined", "Separate" = "separate"),
            selected = "combined",
            inline = TRUE
          )
        } else if (isTRUE(show_combined_species_note)) {
          tags$small("Note: Selected species will be combined, not shown separately.")
        }
      )
    )
  } else if (view == "select_localities") {
    return(
      tagList(
        selectInput(
          inputId = ns("selected_localities"),
          label = tagList(icon("location-dot"), "Localities:"),
          choices = choices,
          selected = selected,
          multiple = multiple,
          selectize = TRUE
        )
      )
    )
  } else if (view == "density_options") {
    trap_data_available <- isTRUE(include_density_trap_option) &&
      exists("trap_data", inherits = TRUE) &&
      !is.null(get("trap_data", inherits = TRUE))
    species_combined_condition <- "(!input.species_display_mode || input.species_display_mode === 'combined')"
    prediction_condition <- if (isTRUE(prediction_cumulative_only)) {
      paste("input.timeline_view_mode === 'cumulative'", species_combined_condition, sep = " && ")
    } else {
      species_combined_condition
    }
    prediction_basis_condition <- paste(prediction_condition, "input.show_predicted_rai_surface", sep = " && ")
    return(
      tagList(
        checkboxInput(
          inputId = ns("exclude_possible_duplicates"),
          label = tags$small("Exclude monitoring 'possible duplicate observations'"),
          value = isTRUE(config$globals$use_net_data)
        ),
        if (isTRUE(trap_data_available)) {
          sliderInput(
            inputId = ns("trap_locality_distance_km"),
            label = tags$small("Include traps up to this distance (km) from selected localities"),
            min = 0,
            max = 10,
            value = 2,
            step = 0.25
          )
        },

        if (isTRUE(include_prediction_option) || isTRUE(include_marker_options)) {
          tags$hr()
        },
        tags$div(
          class = "map-display-layer-options",
          if (isTRUE(include_marker_options)) {
            tagList(
              tags$strong("Monitoring records:"),
              checkboxInput(
                inputId = ns("show_density_location_markers"),
                label = "Observation counts",
                value = isTRUE(include_monitoring_records_default)
              )
            )
          },
          if (isTRUE(include_prediction_option)) {
            conditionalPanel(
              condition = prediction_condition,
              ns = ns,
              checkboxInput(
                inputId = ns("show_predicted_rai_surface"),
                label = "Predicted RAI surface",
                value = FALSE
              )
            )
          },
          if (isTRUE(include_prediction_option)) {
            conditionalPanel(
              condition = prediction_basis_condition,
              ns = ns,
              tags$div(
                class = "prediction-surface-options",
                radioButtons(
                  inputId = ns("predicted_rai_surface_basis"),
                  label = surface_basis_label(ns),
                  choices = c(
                    "Location-weighted RAI" = "weighted_line_rai",
                    "Line RAI" = "line_rai"
                  ),
                  selected = "weighted_line_rai"
                )
              )
            )
          },
          if (isTRUE(include_marker_options) && isTRUE(trap_data_available)) {
            tagList(
              tags$strong("Trapping records:"),
              checkboxInput(
                inputId = ns("show_trap_kill_markers"),
                label = "Trap captures",
                value = isTRUE(include_trap_data_default)
              ),
              conditionalPanel(
                condition = "input.show_trap_kill_markers",
                ns = ns,
                tags$div(
                  class = "trap-capture-sub-options",
                  checkboxInput(
                    inputId = ns("show_trap_blank_checks"),
                    label = trap_check_counters_label(ns),
                    value = FALSE
                  )
                )
              ),
              checkboxInput(
                inputId = ns("show_trap_unchecked_locations"),
                label = unchecked_traps_label(ns),
                value = FALSE
              )
            )
          }
        )
      )
    )
  } else if (view == "monitoring_trapping_density_options") {
    trap_data_available <- exists("trap_data", inherits = TRUE) &&
      !is.null(get("trap_data", inherits = TRUE))
    trap_distance_max <- 10
    if (isTRUE(trap_data_available)) {
      trap_data_value <- get("trap_data", inherits = TRUE)
      if (!is.null(trap_data_value$deps) && "locality_distance_km" %in% names(trap_data_value$deps)) {
        distances <- suppressWarnings(as.numeric(trap_data_value$deps$locality_distance_km))
        distances <- distances[is.finite(distances)]
        if (length(distances) > 0) {
          trap_distance_max <- max(10, ceiling(max(distances, na.rm = TRUE)))
        }
      }
    }

    return(
      tagList(
        checkboxInput(
          inputId = ns("exclude_possible_duplicates"),
          label = tags$small("Exclude monitoring 'possible duplicate observations'"),
          value = isTRUE(config$globals$use_net_data)
        ),
        if (isTRUE(trap_data_available)) {
          sliderInput(
            inputId = ns("trap_locality_distance_km"),
            label = tags$small("Include traps up to this distance (km) from selected localities"),
            min = 0,
            max = trap_distance_max,
            value = min(2, trap_distance_max),
            step = 0.25
          )
        },
        if (isTRUE(trap_data_available)) {
          tags$div(
            class = "map-display-layer-options",
            tags$hr(),
            tags$strong("Trapping records:"),
            checkboxInput(
              inputId = ns("show_trap_blank_checks"),
              label = trap_check_counters_label(ns),
              value = FALSE
            ),
            checkboxInput(
              inputId = ns("show_trap_unchecked_locations"),
              label = unchecked_traps_label(ns),
              value = FALSE
            )
          )
        }
      )
    )
  } else if (view == "select_map_record_options") {
    trap_data_available <- exists("trap_data", inherits = TRUE) &&
      !is.null(get("trap_data", inherits = TRUE))
    trap_distance_max <- 5
    if (isTRUE(trap_data_available)) {
      trap_data_value <- get("trap_data", inherits = TRUE)
      if (!is.null(trap_data_value$deps) && "locality_distance_km" %in% names(trap_data_value$deps)) {
        distances <- suppressWarnings(as.numeric(trap_data_value$deps$locality_distance_km))
        distances <- distances[is.finite(distances)]
        if (length(distances) > 0) {
          trap_distance_max <- max(5, ceiling(max(distances, na.rm = TRUE)))
        }
      }
    }

    return(
      tagList(
        checkboxInput(
          inputId = ns("include_monitoring_records"),
          label = "Monitoring records",
          value = isTRUE(include_monitoring_records_default)
        ),
        conditionalPanel(
          condition = "input.include_monitoring_records",
          ns = ns,
          tags$div(
            class = "monitoring-record-options",
            checkboxInput(
              inputId = ns("exclude_possible_duplicates"),
              label = "Exclude possible duplicates",
              value = isTRUE(config$globals$use_net_data)
            ),
            if (isTRUE(trap_data_available)) {
              conditionalPanel(
                condition = "input.include_trap_data",
                ns = ns,
                checkboxInput(
                  inputId = ns("exclude_untrapped_species"),
                  label = "Exclude untrapped species",
                  value = isTRUE(exclude_untrapped_species_default)
                )
              )
            }
          )
        ),
        if (isTRUE(trap_data_available)) {
          checkboxInput(
            inputId = ns("include_trap_data"),
            label = trapping_records_label(ns),
            value = isTRUE(include_trap_data_default)
          )
        },
        if (isTRUE(trap_data_available)) {
          conditionalPanel(
            condition = "input.include_trap_data",
            ns = ns,
            tags$div(
              class = "trap-data-options",
              sliderInput(
                inputId = ns("trap_locality_distance_km"),
                label = tags$small("Maximum distance (km) from selected localities"),
                min = 0,
                max = trap_distance_max,
                value = min(2.0, trap_distance_max),
                step = 0.25
              )
            )
          )
        }
      )
    )
  } else if (view == "density_timeline_controls") {
    return(
      tagList(
        hr(),
        tags$div(
          class = "map-display-layer-options",
          if (isTRUE(include_marker_options)) {
            tagList(
              tags$strong("Monitoring records:"),
              checkboxInput(
                inputId = ns("show_density_location_markers"),
                label = "Observation counts",
                value = TRUE
              )
            )
          },
          if (isTRUE(include_prediction_option)) {
            conditionalPanel(
              condition = "input.timeline_view_mode === 'cumulative' && (!input.species_display_mode || input.species_display_mode === 'combined')",
              ns = ns,
              checkboxInput(
                inputId = ns("show_predicted_rai_surface"),
                label = "Predicted RAI surface",
                value = FALSE
              )
            )
          },
          if (isTRUE(include_prediction_option)) {
            conditionalPanel(
              condition = "input.timeline_view_mode === 'cumulative' && (!input.species_display_mode || input.species_display_mode === 'combined') && input.show_predicted_rai_surface",
              ns = ns,
              tags$div(
                class = "prediction-surface-options",
                radioButtons(
                  inputId = ns("predicted_rai_surface_basis"),
                  label = surface_basis_label(ns),
                  choices = c(
                    "Location-weighted RAI" = "weighted_line_rai",
                    "Line RAI" = "line_rai"
                  ),
                  selected = "weighted_line_rai"
                ),
              )
            )
          },
          if (isTRUE(include_observation_layer_options)) {
            tagList(
              tags$strong("Trapping records:"),
              checkboxInput(
                inputId = ns("show_trap_kill_markers"),
                label = "Trap captures",
                value = isTRUE(include_trap_data_default)
              ),
              conditionalPanel(
                condition = "input.show_trap_kill_markers",
                ns = ns,
                tags$div(
                  class = "trap-capture-sub-options",
                  checkboxInput(
                    inputId = ns("show_trap_blank_checks"),
                    label = trap_check_counters_label(ns),
                    value = FALSE
                  )
                )
              ),
              checkboxInput(
                inputId = ns("show_trap_unchecked_locations"),
                label = unchecked_traps_label(ns),
                value = FALSE
              )
            )
          }
        ),
        if (isTRUE(include_monitoring_area_option)) {
          checkboxInput(
            inputId = ns("enhance_map_details"),
            label = "Monitoring area boundaries"
          )
        },
        radioButtons(
          inputId = ns("timeline_view_mode"),
          label = "Time progression mode:",
          choices = c("Discrete (single step)" = "single", "Cumulative total to date" = "cumulative"),
          selected = timeline_view_mode_selected
        ),
        div(
          class = "playback-speed-control",
          sliderInput(
            inputId = ns("playback_speed"),
            label = "Playback speed",
            min = 0, max = 4, value = 1, step = 0.25
          ),
          div(
            class = "playback-speed-labels",
            tags$span("Faster"), tags$span("Slower")
          )
        )
      )
    )
  } else if (view == "map") { # This is for the density map
    return(
      tagList(
        leafletOutput(ns("map_display"), height = map_height)
      )
    )
  } else if (view == "density_comparison_layout") {
    if (is.null(primary_map_id) || is.null(comparative_map_id)) {
      stop("primary_map_id and comparative_map_id must be provided when view is 'density_comparison_layout'.")
    }

    comparison_id <- ns("swipe_comparison")
    primary_output_id <- paste0(primary_map_id, "-map_display")
    comparative_output_id <- paste0(comparative_map_id, "-map_display")

    map_panel <- div(
      id = comparison_id,
      class = "map-swipe-comparison",
      style = paste0("height: ", map_height_css(map_height), ";"),
      div(
        class = "map-swipe-map map-swipe-primary-map",
        mapping_module_ui(primary_map_id, view = "map", map_height = "100%"),
        comparison_map_label(
          heading_output_id = primary_heading_output_id,
          meta_output_id = primary_meta_output_id,
          title = primary_title,
          side = "primary"
        )
      ),
      div(
        class = "map-swipe-map map-swipe-comparative-map",
        mapping_module_ui(comparative_map_id, view = "map", map_height = "100%"),
        comparison_map_label(
          heading_output_id = comparative_heading_output_id,
          meta_output_id = comparative_meta_output_id,
          title = comparative_title,
          side = "comparative"
        )
      ),
      tags$button(
        type = "button",
        class = "map-swipe-divider",
        `aria-label` = "Adjust season comparison",
        `aria-valuemin` = "0",
        `aria-valuemax` = "100",
        `aria-valuenow` = "50",
        tags$span(class = "map-swipe-divider-grip")
      ),
      tags$script(HTML(sprintf(
        "(function initSwipe(attemptsLeft) { var config = {containerId:%s, primaryMapId:%s, comparativeMapId:%s}; if (window.initMapSwipeComparison) { window.initMapSwipeComparison(config); } else if (attemptsLeft > 0) { window.setTimeout(function() { initSwipe(attemptsLeft - 1); }, 100); } })(30);",
        jsonlite::toJSON(comparison_id, auto_unbox = TRUE),
        jsonlite::toJSON(primary_output_id, auto_unbox = TRUE),
        jsonlite::toJSON(comparative_output_id, auto_unbox = TRUE)
      )))
    )

    return(
      navset_tab(
        id = ns("density_comparison_tabs"),
        nav_panel(
          "Map",
          map_panel,
          value = "map"
        ),
        nav_panel(
          "Records",
          navset_tab(
            id = ns("density_comparison_data_tabs"),
            nav_panel(
              primary_data_label,
              comparison_data_panel(
                map_id = primary_map_id,
                title = primary_data_title
              ),
              value = "primary"
            ),
            nav_panel(
              comparative_data_label,
              comparison_data_panel(
                map_id = comparative_map_id,
                title = comparative_data_title
              ),
              value = "comparative"
            )
          ),
          value = "data"
        )
      )
    )
  } else if (view == "density_timeline_layout") {
    return(
      tagList(
        div(
          class = "timeline-bar",
          div(
            class = "timeline-step-row",
            selectInput(
              inputId = ns("timeline_step_size"),
              label = "Time step",
              choices = timeline_step_size_choices,
              selected = timeline_step_size_selected
            )
          ),
          uiOutput(ns("timeline_slider_ui")),
          uiOutput(ns("timeline_window_ui"))
        ),
        navset_tab(
          id = ns("density_timeline_tabs"),
          nav_panel(
            "Map",
            leafletOutput(ns("map_display"), height = map_height),
            value = "map"
          ),
          nav_panel(
            "Records",
            h3("Current window records"),
            DT::dataTableOutput(ns("map_data_table")),
            value = "data"
          ),
          nav_panel(
            "Cumulative Records",
            h3("Cumulative records"),
            DT::dataTableOutput(ns("map_cumulative_data_table")),
            value = "cumulative_data"
          )
        )
      )
    )
  } else if (view == "map_only") {
    return(
      tagList(
        div(
          class = "timeline-bar",
          div(
            class = "timeline-step-row",
            selectInput(
              inputId = ns("timeline_step_size"),
              label = "Time step",
              choices = timeline_step_size_choices,
              selected = timeline_step_size_selected
            )
          ),
          uiOutput(ns("timeline_slider_ui")),
          uiOutput(ns("timeline_window_ui"))
        ),
        leafletOutput(ns("map_display"), height = map_height),
        uiOutput(ns("map_textoverlay_warning"))
      )
    )
  } else if (view == "map_record_data_panel") {
    return(
      tagList(
        div(
          class = "mapping-data-actions",
          downloadButton(ns("download_map_records_export"), "Download CSV")
        ),
        h3("Current window records"),
        DT::dataTableOutput(ns("map_data_table")),
        conditionalPanel(
          condition = "input.timeline_view_mode == 'single'",
          ns = ns,
          br(),
          h3("Cumulative records"),
          DT::dataTableOutput(ns("map_cumulative_data_table"))
        )
      )
    )
  }
}


mapping_module_server <- function(id,
                                  obs = NULL, # Reactive: obs filtered by period
                                  deps = NULL, # Reactive: deps filtered by period
                                  species_override = NULL, # Reactive: for comparative density map
                                  localities_override = NULL, # Reactive: for comparative density map
                                  prediction_surface_override = NULL, # Reactive: for comparative density map
                                  prediction_surface_basis_override = NULL, # Reactive: for comparative density map
                                  species_display_mode_override = NULL, # Reactive: for comparative density map
                                  location_markers_override = NULL, # Reactive: for comparative density map
                                  density_data_source_override = NULL, # Reactive: for comparative density map
                                  density_scale_max_override = NULL, # Reactive: shared density comparison max
                                  trap_distance_override = NULL, # Reactive: for comparative density map
                                  trap_kill_markers_override = NULL, # Reactive: for comparative density map
                                  trap_check_counters_override = NULL, # Reactive: for comparative density map
                                  unchecked_traps_override = NULL, # Reactive: for comparative density map
                                  period_names = NULL,      # Reactive: selected period names
                                  period_start_date = NULL, # Reactive: e.g. primary_period$start_date
                                  period_end_date = NULL,    # Reactive: e.g. primary_period$end_date
                                  period_intervals = NULL,   # Reactive: selected period intervals
                                  timeline_mode = c("none", "always"),
                                  enable_map_outputs = TRUE,
                                  use_net = reactive(config$globals$use_net_data),
                                  trap_data = NULL,
                                  period_groups_override = NULL
) {
  timeline_mode <- match.arg(timeline_mode, choices = c("none", "always"))
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    logger::log_debug(sprintf("mapping_module_server, %s moduleServer() running", id))
    
    MAP_ID <- ns("map_display") 
    current_bounds <- reactiveVal(NULL) # Unified reactiveVal for map bounds
    needs_fit_bounds <- reactiveVal(FALSE)
    predicted_rai_surface_cache <- reactiveVal(NULL)
    last_density_map_update_key <- reactiveVal(NULL)
    density_marker_scale <- reactiveVal(0)

    current_trap_data <- reactive({
      if (is.null(trap_data)) {
        return(NULL)
      }

      trap_data_value <- if (is.function(trap_data)) trap_data() else trap_data
      if (is.null(trap_data_value) ||
          is.null(trap_data_value$obs) ||
          is.null(trap_data_value$deps)) {
        return(NULL)
      }

      trap_data_value
    })

    observeEvent(use_net(), {
      updateCheckboxInput(
        session,
        "exclude_possible_duplicates",
        value = isTRUE(use_net())
      )
    }, ignoreInit = FALSE)

    exclude_possible_duplicates_selected <- reactive({
      if (is.null(input$exclude_possible_duplicates)) {
        return(isTRUE(use_net()))
      }

      isTRUE(input$exclude_possible_duplicates)
    })

    observeEvent(input$predicted_rai_surface_basis_info, {
      showModal(modalDialog(
        title = "Surface basis",
        tags$p("The predicted RAI surface estimates RAI between camera locations within each selected locality."),
        tags$p("It uses IDW interpolation, which gives nearby camera locations more influence than locations farther away."),
        tags$p(tags$strong("Location-weighted RAI"), " starts with the RAI for each line, then weights it by the share of observations recorded at each camera location on that line."),
        tags$p(tags$strong("Line RAI"), " gives each camera location the RAI calculated for its line."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$include_trap_data_info, {
      showModal(modalDialog(
        title = "Include trapping records",
        tags$p("Includes trap records for the current map timeframe and locality selection. 
        When checked, trap captures of the selected species will be shown (one icon per species per location)."),

        tags$p("A record is included when the interval from the previous check to this check overlaps the 
        selected timeframe, even if the check date itself falls outside it."),
        
        tags$p("You can widen the trap catchment area to include any trap within the specified distance 
        of any selected locality. When checked, additional options are available to display trap check 
        counters and unchecked traps."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$trap_check_counters_info, {
      showModal(modalDialog(
        title = "Trap check counters",
        tags$p("Trap check counters show how many checks were recorded for each matching trap, and appear when that count is greater than one."),
        tags$p("The count is based on your season and locality selection. It ignores the species filter, so checks are counted whether or not anything was caught."),
        tags$p("A check is counted when the interval from the previous check to this check overlaps the selected timeframe, even if the check date itself falls outside it."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$unchecked_traps_info, {
      showModal(modalDialog(
        title = "Unchecked traps",
        tags$p("Unchecked traps use the selected season scope rather than the moving timeline window. They mark selected-locality traps with no overlapping check anywhere in the selected seasons, and show a count of 0."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)

    include_monitoring_records_selected <- reactive({
      if (is.null(input$include_monitoring_records)) {
        return(TRUE)
      }

      isTRUE(input$include_monitoring_records)
    })

    include_trap_data_selected <- reactive({
      if (is.null(current_trap_data())) {
        return(FALSE)
      }
      if (is.null(input$include_trap_data)) {
        return(TRUE)
      }

      isTRUE(input$include_trap_data)
    })

    exclude_untrapped_species_selected <- reactive({
      isTRUE(input$exclude_untrapped_species) &&
        isTRUE(include_trap_data_selected()) &&
        !is.null(current_trap_data())
    })

    density_data_source_selected <- reactive({
      if (!is.null(density_data_source_override) &&
          !is.null(density_data_source_override())) {
        source <- density_data_source_override()
        if (!source %in% c("monitoring", "trapping", "both", "none")) {
          return("monitoring")
        }
        if (identical(source, "trapping") && is.null(current_trap_data())) {
          return("none")
        }
        if (identical(source, "both") && is.null(current_trap_data())) {
          return("monitoring")
        }
        return(source)
      }

      include_monitoring_layer <- if (is.null(input$show_density_location_markers) && is.null(input$show_predicted_rai_surface)) {
        TRUE
      } else {
        isTRUE(input$show_density_location_markers) || isTRUE(input$show_predicted_rai_surface)
      }
      include_monitoring <- isTRUE(include_monitoring_records_selected()) && include_monitoring_layer

      unchecked_requested <- if (!is.null(unchecked_traps_override) && !is.null(unchecked_traps_override())) {
        isTRUE(unchecked_traps_override())
      } else {
        isTRUE(input$show_trap_unchecked_locations)
      }
      include_trapping <- isTRUE(include_trap_data_selected()) && (
        isTRUE(show_trap_kill_markers_selected()) || unchecked_requested
      )

      if (include_monitoring && include_trapping) {
        return("both")
      }
      if (include_trapping) {
        return("trapping")
      }
      if (include_monitoring) {
        return("monitoring")
      }
      "none"
    })

    trap_locality_distance_km_selected <- reactive({
      if (!is.null(trap_distance_override) && !is.null(trap_distance_override())) {
        value <- suppressWarnings(as.numeric(trap_distance_override()))
      } else {
        value <- suppressWarnings(as.numeric(input$trap_locality_distance_km))
      }
      if (length(value) == 0 || is.na(value) || value < 0) {
          return(2)
      }
      value
    })

    show_trap_kill_markers_selected <- reactive({
      if (!is.null(trap_kill_markers_override) && !is.null(trap_kill_markers_override())) {
        return(isTRUE(trap_kill_markers_override()))
      }
      if (!is.null(input$show_trap_kill_markers)) {
        return(isTRUE(input$show_trap_kill_markers))
      }
      TRUE
    })

    show_trap_blank_checks_selected <- reactive({
      if (!isTRUE(include_trap_data_selected()) ||
          !isTRUE(show_trap_kill_markers_selected())) {
        return(FALSE)
      }
      if (!is.null(trap_check_counters_override) && !is.null(trap_check_counters_override())) {
        return(isTRUE(trap_check_counters_override()))
      }
      isTRUE(input$show_trap_blank_checks)
    })

    show_trap_unchecked_locations_selected <- reactive({
      if (!isTRUE(include_trap_data_selected())) {
        return(FALSE)
      }
      if (!is.null(unchecked_traps_override) && !is.null(unchecked_traps_override())) {
        return(isTRUE(unchecked_traps_override()))
      }
      isTRUE(input$show_trap_unchecked_locations)
    })

    show_predicted_rai_surface_selected <- reactive({
      if (!density_data_source_selected() %in% c("monitoring", "both")) {
        return(FALSE)
      }

      if (identical(species_display_mode_selected(), "separate")) {
        return(FALSE)
      }

      if (timeline_active() && !identical(input$timeline_view_mode, "cumulative")) {
        return(FALSE)
      }

      if (!is.null(prediction_surface_override) &&
          !is.null(prediction_surface_override())) {
        return(isTRUE(prediction_surface_override()))
      }

      isTRUE(input$show_predicted_rai_surface)
    })

    predicted_rai_surface_basis_selected <- reactive({
      if (!is.null(prediction_surface_basis_override) &&
          !is.null(prediction_surface_basis_override())) {
        basis <- prediction_surface_basis_override()
      } else {
        basis <- input$predicted_rai_surface_basis
      }

      if (is.null(basis) || !basis %in% c("line_rai", "weighted_line_rai")) {
        return("line_rai")
      }

      basis
    })

    species_display_mode_selected <- reactive({
      if (!is.null(species_display_mode_override) &&
          !is.null(species_display_mode_override())) {
        mode <- species_display_mode_override()
      } else {
        mode <- input$species_display_mode
      }

      if (is.null(mode) || !mode %in% c("combined", "separate")) {
        return("combined")
      }

      mode
    })

    show_density_location_markers_selected <- reactive({
      if (!is.null(location_markers_override) &&
          !is.null(location_markers_override())) {
        return(isTRUE(location_markers_override()))
      }

      if (is.null(input$show_density_location_markers)) {
        return(TRUE)
      }

      isTRUE(input$show_density_location_markers)
    })

    
    # --- Common Reactives ---
    current_selected_species <- reactive({
      if (!is.null(species_override) && !is.null(species_override())) {
        logger::log_debug(sprintf("mapping_module_server, %s species_override provided.", id))
        species_override()
      } else {
        logger::log_debug(sprintf("mapping_module_server, %s using input$selected_species.", id))
        req(input$selected_species)
        as.character(input$selected_species)
      }
    })
    
    current_selected_localities <- reactive({
      if (!is.null(localities_override) && !is.null(localities_override())) {
        logger::log_debug(sprintf("mapping_module_server, %s localities_override provided.", id))
        localities_override()
      } else {
        logger::log_debug(sprintf("mapping_module_server, %s using input$selected_localities.", id))
        req(input$selected_localities)
        as.character(input$selected_localities)
      }
    })
    
    # --- Generalized Helper Functions ---
    
    #' Calculate Map Bounds from Active Locations
    #'
    #' @param active_locs_df Data frame of active locations, must contain 'longitude' and 'latitude'.
    #' @param context_id_log The module ID for logging.
    #' @param context_label Short label used in bounds log messages.
    #' @return A list with min_lng, max_lng, min_lat, max_lat, or NULL if bounds cannot be calculated.
    calculate_bounds_from_locations <- function(active_locs_df, context_id_log, context_label) {
      if (nrow(active_locs_df) > 0 &&
          all(c("longitude", "latitude") %in% names(active_locs_df)) &&
          !any(is.na(active_locs_df$longitude)) && !any(is.na(active_locs_df$latitude))) {
        min_lng <- min(active_locs_df$longitude, na.rm = TRUE)
        max_lng <- max(active_locs_df$longitude, na.rm = TRUE)
        min_lat <- min(active_locs_df$latitude, na.rm = TRUE)
        max_lat <- max(active_locs_df$latitude, na.rm = TRUE)
        if(all(!is.infinite(c(min_lng, max_lng, min_lat, max_lat)))){
          return(list(min_lng = min_lng, max_lng = max_lng, min_lat = min_lat, max_lat = max_lat))
        } else {
          logger::log_warn(sprintf("mapping_module_server [%s], ID: %s - Invalid (Infinite) bounds calculated.", context_label, context_id_log))
          return(NULL)
        }
      } else {
        logger::log_warn(sprintf("mapping_module_server [%s], ID: %s - Insufficient data for bounds calculation (active_locs_df empty or missing lat/lon).", context_label, context_id_log))
        return(NULL)
      }
    }
    
    #' Invalidate Map Size
    #' (Targets the unified MAP_ID)
    invalidate_map_size <- function() {
      logger::log_debug(sprintf("mapping_module_server, ID: %s calling invalidateSize for map: %s", id, MAP_ID))
      map_id_selector_json <- jsonlite::toJSON(paste0("#", MAP_ID), auto_unbox = TRUE)
      map_id_json <- jsonlite::toJSON(MAP_ID, auto_unbox = TRUE)
      type_json <- jsonlite::toJSON("map", auto_unbox = TRUE)
      shinyjs::runjs(sprintf(
        'setTimeout(function() {
           var mapWidget = HTMLWidgets.find(%s);
           if (mapWidget) {
             var mapObj = mapWidget.getMap();
             if (mapObj) {
               mapObj.invalidateSize();
               console.log("invalidateSize() called on " + %s + " map: " + %s);
             } else { console.error("Leaflet map object not found for " + %s + " map: " + %s); }
           } else { console.error("Map widget not found for " + %s + " map: " + %s); }
         }, 100);', map_id_selector_json, type_json, map_id_json, type_json, map_id_json, type_json, map_id_json
      ))
    }
    
    #' Apply Fit Bounds to Map
    #' (Uses current_bounds() and targets MAP_ID)
    apply_map_fit_bounds <- function() {
      bounds_val <- current_bounds() # Uses the unified bounds reactiveVal
      logger::log_debug(sprintf("mapping_module_server, ID: %s apply_map_fit_bounds. Bounds are: %s", id, paste(capture.output(str(bounds_val)), collapse=",")))
      if (!is.null(bounds_val) && !is.null(bounds_val$min_lng) && !is.na(bounds_val$min_lng)) { # Added NA check for robustness
        leafletProxy(MAP_ID) %>%
          fitBounds(
            lng1 = bounds_val$min_lng, lat1 = bounds_val$min_lat,
            lng2 = bounds_val$max_lng, lat2 = bounds_val$max_lat
          )
        logger::log_info(sprintf("mapping_module_server, ID: %s fitBounds applied.", id))
      } else {
        logger::log_warn(sprintf("mapping_module_server, ID: %s no valid bounds for fitBounds.", id))
      }
    }
    
    #' Recenter Map (Generic)
    #' (Calls invalidate_map_size and apply_map_fit_bounds)
    recenter_map_generic <- function() {
      logger::log_info(sprintf("mapping_module_server, ID: %s recenter_map_generic called.", id))
      invalidate_map_size()
      shinyjs::delay(150, { # Delay as in original code
        apply_map_fit_bounds()
      })
    }

    register_map_resize_handler <- function() {
      map_id_selector_json <- jsonlite::toJSON(paste0("#", MAP_ID), auto_unbox = TRUE)
      resize_input_id_json <- jsonlite::toJSON(ns("map_resize"), auto_unbox = TRUE)
      handler_name_json <- jsonlite::toJSON(paste0("insightfulKiwiMapResize_", gsub("[^A-Za-z0-9_]", "_", MAP_ID)), auto_unbox = TRUE)
      shinyjs::runjs(sprintf(
        '(function() {
           var selector = %s;
           var inputId = %s;
           var handlerName = %s;
           if (window[handlerName]) {
             window.removeEventListener("resize", window[handlerName]);
           }
           var timer = null;
           window[handlerName] = function() {
             clearTimeout(timer);
             timer = setTimeout(function() {
               var element = document.querySelector(selector);
               if (window.Shiny && element && element.offsetParent !== null) {
                 Shiny.setInputValue(inputId, Date.now(), {priority: "event"});
               }
             }, 200);
           };
           window.addEventListener("resize", window[handlerName]);
         })();',
        map_id_selector_json,
        resize_input_id_json,
        handler_name_json
      ))
    }
    
    # --- Map Output ---
    output$map_display <- renderLeaflet({
      logger::log_debug(sprintf("mapping_module_server, %s renderLeaflet for map_display: %s", id, MAP_ID))
      leaflet::leaflet() %>%
        leaflet::addTiles(
          group = "Street",
          options = leaflet::tileOptions(crossOrigin = TRUE)
        ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satellite",
          options = leaflet::providerTileOptions(crossOrigin = TRUE)
        ) %>%
        leaflet::hideGroup("Satellite") %>%
        leaflet::addLayersControl(
          baseGroups = c("Street", "Satellite"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        )
    })
    outputOptions(output, "map_display", suspendWhenHidden = FALSE)
    register_map_resize_handler()
    observeEvent(input$map_resize, {
      recenter_map_generic()
    }, ignoreInit = TRUE)
    
    
    # --- Map Logic ---
    if (!isTRUE(enable_map_outputs)) {
      return(list(
        selected_species = current_selected_species,
        selected_localities = current_selected_localities,
        recenter_map = recenter_map_generic
      ))
    }

    {
      is_playing <- reactiveVal(FALSE)
      playback_gap_notice <- reactiveVal(NULL)
      playback_skip_resume_id <- reactiveVal(0L)

      timeline_active <- reactive({
        identical(timeline_mode, "always")
      })

      skip_playback_gaps <- function() TRUE

      timeline_period_groups <- reactive({
        if (exists("core_data", inherits = TRUE) && !is.null(core_data$period_groups)) {
          return(core_data$period_groups)
        }

        NULL
      })

      selected_period_key <- reactive({
        if (is.function(period_names)) {
          selected_periods <- as.character(period_names())
          selected_periods <- selected_periods[!is.na(selected_periods) & nzchar(selected_periods)]
          if (length(selected_periods) > 0) {
            return(paste(sort(selected_periods), collapse = ","))
          }
        }

        start_key <- if (is.function(period_start_date)) {
          format(as.Date(period_start_date()), "%Y-%m-%d")
        } else {
          "no-start"
        }
        end_key <- if (is.function(period_end_date)) {
          format(as.Date(period_end_date()), "%Y-%m-%d")
        } else {
          "no-end"
        }

        paste(start_key, end_key, sep = ",")
      })

      floor_posix_hour <- function(value) {
        value <- as.POSIXct(value, tz = timeline_actual_timezone())
        as.POSIXct(
          floor(as.numeric(value) / 3600) * 3600,
          origin = "1970-01-01",
          tz = timeline_actual_timezone()
        )
      }

      ceiling_posix_hour <- function(value) {
        timezone <- timeline_actual_timezone()
        value <- as.POSIXct(value, tz = timezone)
        if (!is.na(value) && identical(format(value, "%H:%M:%S", tz = timezone), "23:59:59")) {
          return(value)
        }

        as.POSIXct(
          ceiling(as.numeric(value) / 3600) * 3600,
          origin = "1970-01-01",
          tz = timezone
        )
      }

      timeline_bounds <- function(start, end) {
        start <- floor_posix_hour(start)
        end <- ceiling_posix_hour(end)

        if (is.na(start) || is.na(end)) {
          return(NULL)
        }

        if (end <= start) {
          end <- start + 3600
        }

        list(start = start, end = end)
      }

      timeline_observation_bounds <- reactive({
        obs_data <- obs()

        fallback_start <- if (is.function(period_start_date)) {
          timeline_date_midnight(period_start_date())
        } else {
          suppressWarnings(as.POSIXct(min(obs_data$timestamp, na.rm = TRUE), tz = timeline_actual_timezone()))
        }

        fallback_end <- if (is.function(period_end_date)) {
          timeline_date_end(period_end_date())
        } else {
          suppressWarnings(as.POSIXct(max(obs_data$timestamp, na.rm = TRUE), tz = timeline_actual_timezone()))
        }

        req(fallback_start, fallback_end)

        period_obs <- obs_data %>%
          dplyr::filter(
            timeline_as_date(timestamp) >= timeline_as_date(fallback_start),
            timeline_as_date(timestamp) <= timeline_as_date(fallback_end),
            !is.na(timestamp)
          )

        if (nrow(period_obs) > 0) {
          return(timeline_bounds(
            min(period_obs$timestamp, na.rm = TRUE),
            max(period_obs$timestamp, na.rm = TRUE)
          ))
        }

        timeline_bounds(fallback_start, fallback_end)
      })

      timeline_monitoring_window <- reactive({
        deps_data <- deps()
        if (is.null(deps_data) || nrow(deps_data) == 0 ||
            !all(c("start", "end", "locality") %in% names(deps_data))) {
          return(NULL)
        }

        monitoring_deps <- deps_data %>%
          dplyr::filter(.data$locality %in% current_selected_localities())

        if (nrow(monitoring_deps) == 0) {
          return(NULL)
        }

        start_value <- suppressWarnings(min(monitoring_deps$start, na.rm = TRUE))
        end_value <- suppressWarnings(max(monitoring_deps$end, na.rm = TRUE))

        if (is.na(start_value) || is.na(end_value)) {
          return(NULL)
        }

        list(
          start = floor_posix_hour(start_value),
          end = ceiling_posix_hour(end_value)
        )
      })

      timeline_period_start <- reactive({
        req(timeline_observation_bounds())
        base_start <- timeline_observation_bounds()$start

        if (identical(density_data_source_selected(), "monitoring")) {
          monitoring_window <- timeline_monitoring_window()
          if (!is.null(monitoring_window) &&
              !is.na(monitoring_window$start) &&
              monitoring_window$start < timeline_observation_bounds()$end) {
            return(monitoring_window$start)
          }
        }

        base_start
      })

      timeline_period_end <- reactive({
        req(timeline_observation_bounds())
        timeline_observation_bounds()$end
      })

      timeline_weather_data <- reactive({
        req(timeline_period_start(), timeline_period_end(), current_selected_localities(), deps())
        active_locations <- deps() %>%
          dplyr::filter(locality %in% current_selected_localities()) %>%
          dplyr::distinct(locationID, locality, .keep_all = TRUE)

        timeline_weather_for_deployments(
          active_locations,
          timeline_as_date(timeline_period_start()),
          timeline_as_date(timeline_period_end())
        )
      })

      timeline_step_seconds <- reactive({
        step_size <- if (is.null(input$timeline_step_size)) "day" else input$timeline_step_size
        switch(step_size,
          "hour" = 3600,
          "day_night" = 3600,
          "diel" = 3600,
          "day" = 86400,
          "week" = 604800,
          "month" = 2592000,
          "season" = 604800,
          86400
        )
      })

      timeline_initial_value <- reactive({
        req(timeline_points())
        timeline_selected_time(1L, timeline_points())
      })

      timeline_points <- reactive({
        req(timeline_period_start(), timeline_period_end())
        step_size <- if (is.null(input$timeline_step_size)) "day" else input$timeline_step_size
        view_mode <- if (is.null(input$timeline_view_mode)) "cumulative" else input$timeline_view_mode
        timeline_time_points(
          timeline_period_start(),
          timeline_period_end(),
          step_size,
          view_mode,
          timeline_weather_data(),
          timeline_period_groups()
        )
      })

      reset_timeline_slider <- function() {
        req(timeline_points())
        timeline_update_slider(session, "time_slider", timeline_points(), timeline_initial_value())
      }

      output$timeline_slider_ui <- renderUI({
        req(timeline_active(), timeline_points())
        step_size <- if (is.null(input$timeline_step_size)) "day" else input$timeline_step_size
        timeline_ui(
          ns = ns,
          points = timeline_points(),
          value = timeline_initial_value(),
          step_size = step_size
        )
      })

      set_playback_button_state <- function(playing) {
        if (!timeline_active()) {
          return()
        }

        if (isTRUE(playing)) {
          shinyjs::disable(selector = paste0("#", ns("play_btn")))
          shinyjs::enable(selector = paste0("#", ns("pause_btn")))
        } else {
          shinyjs::enable(selector = paste0("#", ns("play_btn")))
          shinyjs::disable(selector = paste0("#", ns("pause_btn")))
        }
      }

      send_playback_timer <- function() {
        if (!timeline_active()) {
          return()
        }

        playback_speed <- if (is.null(input$playback_speed)) 1 else input$playback_speed
        interval_ms <- max(50, as.numeric(playback_speed) * 1000)
        session$sendCustomMessage(
          "densityPlaybackTimer",
          list(
            id = ns("playback_tick"),
            enabled = isTRUE(is_playing() && timeline_active()),
            interval = interval_ms
          )
        )
      }

      observe({
        set_playback_button_state(is_playing())
        send_playback_timer()
      })

      observeEvent(input$play_btn, {
        req(timeline_active())
        is_playing(TRUE)
      })

      observeEvent(input$pause_btn, {
        playback_skip_resume_id(playback_skip_resume_id() + 1L)
        playback_gap_notice(NULL)
        is_playing(FALSE)
      })

      observeEvent(input$show_predicted_rai_surface, {
        req(timeline_active())
        if (isTRUE(input$show_predicted_rai_surface)) {
          current_speed <- if (is.null(input$playback_speed)) 1 else as.numeric(input$playback_speed)
          if (length(current_speed) > 0 && is.finite(current_speed) && current_speed < 2) {
            updateSliderInput(session, "playback_speed", value = 2)
          }
          current_step <- if (is.null(input$timeline_step_size)) "day" else input$timeline_step_size
          if (current_step %in% c("hour", "day_night", "diel", "day")) {
            updateSelectInput(session, "timeline_step_size", selected = "week")
          }
        }
      }, ignoreInit = TRUE)

      observeEvent(input$show_density_location_markers, {
        req(timeline_active())
        if (!isTRUE(input$show_density_location_markers)) {
          current_step <- if (is.null(input$timeline_step_size)) "day" else input$timeline_step_size
          if (current_step %in% c("hour", "day_night", "diel", "day")) {
            updateSelectInput(session, "timeline_step_size", selected = "week")
          }
        }
      }, ignoreInit = TRUE)

      observeEvent(input$reset_btn, {
        playback_skip_resume_id(playback_skip_resume_id() + 1L)
        playback_gap_notice(NULL)
        is_playing(FALSE)
        reset_timeline_slider()
      })

      observeEvent(list(input$timeline_step_size, input$timeline_view_mode), {
        if (timeline_active()) {
          playback_skip_resume_id(playback_skip_resume_id() + 1L)
          is_playing(FALSE)
          playback_gap_notice(NULL)
          reset_timeline_slider()
        }
      }, ignoreInit = TRUE)

      observe({
        req(timeline_active())
        if (identical(input$timeline_view_mode, "single")) {
          shiny::showTab(
            inputId = "density_timeline_tabs",
            target = "cumulative_data",
            session = session
          )
        } else {
          if (identical(input$density_timeline_tabs, "cumulative_data")) {
            updateTabsetPanel(
              session,
              "density_timeline_tabs",
              selected = "data"
            )
          }
          shiny::hideTab(
            inputId = "density_timeline_tabs",
            target = "cumulative_data",
            session = session
          )
        }
      })

      observeEvent(list(
        session$rootScope()$input$nav,
        session$rootScope()$input[["density_map_comparison-density_comparison_tabs"]],
        session$rootScope()$input[["monitoring_trapping_map_comparison-density_comparison_tabs"]]
      ), {
        main_nav <- session$rootScope()$input$nav
        density_tab <- if (identical(main_nav, "monitoring_trapping_map")) {
          session$rootScope()$input[["monitoring_trapping_map_comparison-density_comparison_tabs"]]
        } else {
          session$rootScope()$input[["density_map_comparison-density_comparison_tabs"]]
        }
        density_summary_active <- main_nav %in% c("density_map", "monitoring_trapping_map") &&
          (is.null(density_tab) || identical(density_tab, "map"))

        if (identical(timeline_mode, "none") && !isTRUE(density_summary_active)) {
          playback_skip_resume_id(playback_skip_resume_id() + 1L)
          playback_gap_notice(NULL)
          is_playing(FALSE)
        }
      }, ignoreInit = TRUE)

      observeEvent(list(session$rootScope()$input$nav, input$density_timeline_tabs), {
        if (identical(timeline_mode, "always") &&
            (!identical(session$rootScope()$input$nav, "density_timeline_map") ||
             !is.null(input$density_timeline_tabs) && !identical(input$density_timeline_tabs, "map"))) {
          playback_skip_resume_id(playback_skip_resume_id() + 1L)
          playback_gap_notice(NULL)
          is_playing(FALSE)
        }
      }, ignoreInit = TRUE)

      observeEvent(input$playback_tick, {
        req(is_playing(), timeline_active(), input$time_slider, timeline_points())
        current_index <- timeline_selected_index(input$time_slider, length(timeline_points()))
        current_val <- timeline_selected_time(current_index, timeline_points())
        next_index <- current_index + 1L
        next_val <- if (next_index <= length(timeline_points())) {
          timeline_selected_time(next_index, timeline_points())
        } else {
          timeline_period_end() + 1
        }

        skip_target <- if (skip_playback_gaps()) {
          playback_gap_skip_target(current_val, next_val, timeline_period_end(), timeline_period_groups())
        } else {
          NULL
        }

        if (!is.null(skip_target)) {
          step_size <- if (is.null(input$timeline_step_size)) "day" else input$timeline_step_size
          if (step_size %in% c("month", "season")) {
            timeline_skip_slider_to_time(session, "time_slider", timeline_points(), skip_target$target)
            playback_gap_notice(NULL)
            is_playing(TRUE)
            return()
          }

          is_playing(FALSE)
          playback_gap_notice(list(
            message = sprintf(
              "Skipping to %s",
              if (!is.null(skip_target$next_name) && !is.na(skip_target$next_name)) {
                skip_target$next_name
              } else {
                "next monitoring season"
              }
            )
          ))
          resume_id <- playback_skip_resume_id() + 1L
          playback_skip_resume_id(resume_id)
          shinyjs::delay(3000, {
            if (!isTRUE(timeline_active()) ||
                !identical(playback_skip_resume_id(), resume_id)) {
              return()
            }
            timeline_skip_slider_to_time(session, "time_slider", timeline_points(), skip_target$target)
            playback_gap_notice(NULL)
            is_playing(TRUE)
          })
          return()
        }

        if (next_val <= timeline_period_end()) {
          playback_gap_notice(NULL)
          timeline_update_slider(session, "time_slider", timeline_points(), next_val)
        } else {
          playback_gap_notice(NULL)
          if (current_val < timeline_period_end()) {
            timeline_update_slider(session, "time_slider", timeline_points(), timeline_period_end())
          }
          is_playing(FALSE)
        }
      }, ignoreInit = TRUE)
      
      mapping_data_density <- reactive({
        req(obs(), deps(), current_selected_species(), current_selected_localities())
        species_dens <- tolower(unname(current_selected_species()))
        localities_dens <- current_selected_localities()
        if (timeline_active()) {
          req(input$time_slider)
        }
        use_timeline <- timeline_active()
        period_key_dens <- selected_period_key()
        density_source_dens <- density_data_source_selected()
        show_monitoring_density <- density_source_dens %in% c("monitoring", "both")
        show_trapping_density <- density_source_dens %in% c("trapping", "both")
        selected_period_intervals_dens <- if (is.function(period_intervals)) {
          period_intervals()
        } else {
          NULL
        }
        
        logger::log_debug(sprintf(
          "mapping_module_server [density], %s mapping_data_density() for species: %s, localities: %s",
          id, paste(species_dens, collapse=", "), paste(localities_dens, collapse=", ")
        ))
        
        active_locations_dens <- deps() %>%
          dplyr::filter(locality %in% localities_dens) %>%
          dplyr::distinct(locationID, locality, .keep_all = TRUE) %>%
          dplyr::mutate(line = as.character(.data[["line"]]))
        
        new_map_bounds <- calculate_bounds_from_locations(active_locations_dens, id, "density")
        new_bounds_key <- if (nrow(active_locations_dens) > 0) {
          paste(sort(unique(active_locations_dens$locationID)), collapse = "|")
        } else {
          ""
        }
        old_bounds <- current_bounds()
        old_bounds_key <- if (!is.null(old_bounds$key)) old_bounds$key else NULL
        if (!identical(new_bounds_key, old_bounds_key)) {
          if (!is.null(new_map_bounds)) {
            new_map_bounds$key <- new_bounds_key
          }
          current_bounds(new_map_bounds)
          needs_fit_bounds(TRUE)
        }
        
        obs_filtered_dens <- obs() %>%
          dplyr::filter(
            scientificName_lower %in% species_dens,
            locality %in% localities_dens
          )

        obs_for_scale_dens <- obs_filtered_dens

        start_time_dens <- NULL
        current_time_dens <- NULL
        obs_cumulative_dens <- obs_filtered_dens
        if (use_timeline) {
          step_size <- if (is.null(input$timeline_step_size)) "day" else input$timeline_step_size
          current_time_dens <- timeline_effective_current_time(
            input$time_slider,
            step_size,
            timeline_period_end(),
            timeline_points()
          )
          start_time_dens <- if (identical(input$timeline_view_mode, "single")) {
            if (step_size %in% c("day_night", "diel")) {
              previous_weather_boundary(current_time_dens, timeline_period_start(), timeline_weather_data(), step_size)
            } else if (identical(step_size, "season")) {
              timeline_current_season_start(current_time_dens, timeline_period_groups(), timeline_period_start())
            } else {
              timeline_single_window_start(
                current_time_dens,
                timeline_period_start(),
                step_size,
                timeline_step_seconds()
              )
            }
          } else {
            timeline_period_start()
          }

          obs_cumulative_dens <- obs_cumulative_dens %>%
            dplyr::filter(
              timestamp >= timeline_period_start(),
              timestamp <= current_time_dens
            )

          obs_filtered_dens <- obs_filtered_dens %>%
            dplyr::filter(
              timestamp >= start_time_dens,
              timestamp <= current_time_dens
            )

          obs_for_scale_dens <- obs_for_scale_dens %>%
            dplyr::filter(
              timestamp >= start_time_dens,
              timestamp <= current_time_dens
            )
        }

        if (isTRUE(exclude_possible_duplicates_selected()) &&
            "possible_duplicate" %in% names(obs_filtered_dens)) {
          obs_filtered_dens <- obs_filtered_dens %>%
            dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
          obs_cumulative_dens <- obs_cumulative_dens %>%
            dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
          obs_for_scale_dens <- obs_for_scale_dens %>%
            dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
        }
        
        active_location_effort_dens <- deps() %>%
          dplyr::filter(locality %in% localities_dens) %>%
          dplyr::group_by(locationID) %>%
          dplyr::summarise(
            locationName = dplyr::first(locationName),
            locality = dplyr::first(locality),
            line = as.character(dplyr::first(line)),
            longitude = dplyr::first(longitude),
            latitude = dplyr::first(latitude),
            camera_hours = sum(camera_hours, na.rm = TRUE),
            .groups = "drop"
          )

        obs_location_counts_dens <- obs_filtered_dens %>%
          dplyr::group_by(locationID) %>%
          dplyr::summarise(
            count = sum(count, na.rm = TRUE),
            observation_ids = list(unique(observationID)),
            .groups = "drop"
          )

        location_summary_dens <- active_location_effort_dens %>%
          dplyr::left_join(obs_location_counts_dens, by = "locationID") %>%
          dplyr::mutate(
            count = dplyr::coalesce(.data$count, 0)
          )

        line_rai_summary_dens <- location_summary_dens %>%
          dplyr::group_by(locality, line) %>%
          dplyr::summarise(
            line_count = sum(count, na.rm = TRUE),
            line_camera_hours = sum(camera_hours, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            line_rai = dplyr::if_else(
              is.finite(.data$line_camera_hours) & .data$line_camera_hours > 0,
              (.data$line_count / .data$line_camera_hours) * config$globals$rai_norm_hours,
              NA_real_
            )
          )

        rai_location_dens <- location_summary_dens %>%
          dplyr::left_join(line_rai_summary_dens, by = c("locality", "line")) %>%
          dplyr::mutate(
            line = as.character(.data$line),
            location_line_count_share = dplyr::if_else(
              is.finite(.data$line_count) & .data$line_count > 0,
              .data$count / .data$line_count,
              0
            ),
            weighted_line_rai = dplyr::if_else(
              is.finite(.data$line_rai),
              .data$line_rai * .data$location_line_count_share,
              NA_real_
            ),
            rai = .data$line_rai
          )

        trap_data_value_dens <- current_trap_data()
        trapped_selected_species_dens <- FALSE
        if (!is.null(trap_data_value_dens) && !is.null(trap_data_value_dens$obs)) {
          trap_species_values_dens <- if ("scientificName_lower" %in% names(trap_data_value_dens$obs)) {
            trap_data_value_dens$obs$scientificName_lower
          } else if ("scientificName" %in% names(trap_data_value_dens$obs)) {
            trap_data_value_dens$obs$scientificName
          } else {
            character(0)
          }
          trap_species_values_dens <- tolower(as.character(trap_species_values_dens))
          selected_species_values_dens <- tolower(as.character(species_dens))
          trapped_selected_species_dens <- any(
            trap_species_values_dens %in% selected_species_values_dens,
            na.rm = TRUE
          )
        }

        obs_summary_location_dens <- rai_location_dens %>%
          dplyr::mutate(
            marker_dataset = "monitoring",
            marker_layer = "Observation counts"
          )
        if (!isTRUE(show_monitoring_density)) {
          obs_summary_location_dens <- obs_summary_location_dens[0, , drop = FALSE]
        }
        trap_records_dens <- dplyr::tibble()
        trap_cumulative_records_dens <- dplyr::tibble()
        trap_support_locations_dens <- dplyr::tibble()

        if (isTRUE(show_trapping_density)) {
          trap_start_date <- if (is.function(period_start_date)) {
            period_start_date()
          } else {
            suppressWarnings(min(as.Date(obs()$timestamp), na.rm = TRUE))
          }
          trap_end_date <- if (is.function(period_end_date)) {
            period_end_date()
          } else {
            suppressWarnings(max(as.Date(obs()$timestamp), na.rm = TRUE))
          }

          trap_observations_dens <- prepare_trap_observations_for_map(
            current_trap_data(),
            trap_start_date,
            trap_end_date,
            species_dens,
            localities_dens,
            trap_locality_distance_km_selected(),
            include_blank_checks = show_trap_blank_checks_selected(),
            include_unchecked_locations = show_trap_unchecked_locations_selected(),
            period_intervals = selected_period_intervals_dens
          )
          trap_cumulative_records_dens <- trap_observations_dens

          if (use_timeline && nrow(trap_cumulative_records_dens) > 0 && "trap_marker_type" %in% names(trap_cumulative_records_dens)) {
            trap_cumulative_records_dens <- trap_cumulative_records_dens %>%
              dplyr::filter(
                .data$trap_marker_type %in% c("kill", "check", "unchecked"),
                .data$display_start_time <= current_time_dens,
                .data$display_end_time >= timeline_period_start(),
                trap_intervals_overlap_periods(
                  .data$display_start_time,
                  .data$display_end_time,
                  selected_period_intervals_dens,
                  timeline_period_start(),
                  current_time_dens
                )
              ) %>%
              dplyr::mutate(
                display_start_time = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  timeline_period_start(),
                  .data$display_start_time
                ),
                display_end_time = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  current_time_dens,
                  .data$display_end_time
                ),
                first_check = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  timeline_as_date(timeline_period_start()),
                  .data$first_check
                ),
                last_check = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  timeline_as_date(current_time_dens),
                  .data$last_check
                ),
                check_span_days = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  pmax(as.integer(timeline_as_date(current_time_dens) - timeline_as_date(timeline_period_start())), 0L),
                  .data$check_span_days
                )
              )
          }

          if (use_timeline && nrow(trap_observations_dens) > 0 && "trap_marker_type" %in% names(trap_observations_dens)) {
            trap_observations_dens <- trap_observations_dens %>%
              dplyr::filter(
                .data$trap_marker_type %in% c("kill", "check", "unchecked"),
                .data$display_start_time <= current_time_dens,
                .data$display_end_time >= start_time_dens,
                trap_intervals_overlap_periods(
                  .data$display_start_time,
                  .data$display_end_time,
                  selected_period_intervals_dens,
                  start_time_dens,
                  current_time_dens
                )
              ) %>%
              dplyr::mutate(
                display_start_time = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  start_time_dens,
                  .data$display_start_time
                ),
                display_end_time = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  current_time_dens,
                  .data$display_end_time
                ),
                first_check = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  timeline_as_date(start_time_dens),
                  .data$first_check
                ),
                last_check = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  timeline_as_date(current_time_dens),
                  .data$last_check
                ),
                check_span_days = dplyr::if_else(
                  .data$trap_marker_type == "unchecked",
                  pmax(as.integer(timeline_as_date(current_time_dens) - timeline_as_date(start_time_dens)), 0L),
                  .data$check_span_days
                )
              )
          }

          if (nrow(trap_observations_dens) > 0 && "trap_marker_type" %in% names(trap_observations_dens)) {
            enabled_trap_marker_types_dens <- c(
              if (isTRUE(show_trap_kill_markers_selected())) "kill",
              if (isTRUE(show_trap_blank_checks_selected())) "check",
              if (isTRUE(show_trap_unchecked_locations_selected())) "unchecked"
            )
            trap_observations_dens <- trap_observations_dens %>%
              dplyr::filter(.data$trap_marker_type %in% enabled_trap_marker_types_dens)
            trap_cumulative_records_dens <- trap_cumulative_records_dens %>%
              dplyr::filter(.data$trap_marker_type %in% enabled_trap_marker_types_dens)
          }

          trap_records_dens <- trap_observations_dens
          trap_support_locations_dens <- dplyr::tibble()
          if (isTRUE(show_trap_blank_checks_selected())) {
            check_summary_dens <- create_trap_marker_summary(trap_observations_dens)
            if (nrow(check_summary_dens) > 0) {
              trap_support_locations_dens <- dplyr::bind_rows(
                trap_support_locations_dens,
                check_summary_dens %>%
                  dplyr::filter(.data$trap_checks > 0) %>%
                  dplyr::mutate(density_support_type = "check")
              )
            }
          }
          if (isTRUE(show_trap_unchecked_locations_selected())) {
            unchecked_summary_dens <- create_trap_unchecked_summary(trap_observations_dens)
            if (nrow(unchecked_summary_dens) > 0) {
              trap_support_locations_dens <- dplyr::bind_rows(
                trap_support_locations_dens,
                unchecked_summary_dens %>%
                  dplyr::mutate(density_support_type = "unchecked")
              )
            }
          }
          trap_kill_summary_dens <- if (isTRUE(show_trap_kill_markers_selected())) {
            create_trap_kill_summary(trap_observations_dens)
          } else {
            dplyr::tibble()
          }

          if (nrow(trap_kill_summary_dens) > 0) {
            trapped_selected_species_dens <- TRUE
            trap_density_summary_dens <- trap_kill_summary_dens %>%
              dplyr::group_by(.data$locationID, .data$locationName) %>%
              dplyr::summarise(
                locality = dplyr::first(.data$locality),
                line = as.character(dplyr::first(.data$trap_line)),
                longitude = dplyr::first(.data$longitude),
                latitude = dplyr::first(.data$latitude),
                count = sum(.data$kills, na.rm = TRUE),
                observation_ids = list(unique(unlist(.data$observation_ids, use.names = FALSE))),
                trap_checks = dplyr::first(.data$trap_checks),
                first_check = dplyr::first(.data$first_check),
                last_check = dplyr::first(.data$last_check),
                check_span_days = dplyr::first(.data$check_span_days),
                mean_check_interval_days = dplyr::first(.data$mean_check_interval_days),
                trap_days = dplyr::first(.data$trap_days),
                any_species_kill_count = dplyr::first(.data$any_species_kill_count),
                selected_species_kill_count = dplyr::first(.data$selected_species_kill_count),
                kills_per_100_trap_days_selected_species = dplyr::first(.data$kills_per_100_trap_days_selected_species),
                kills_per_100_trap_days_any_species = dplyr::first(.data$kills_per_100_trap_days_any_species),
                .groups = "drop"
              ) %>%
              dplyr::mutate(
                camera_hours = NA_real_,
                line_count = .data$count,
                line_camera_hours = NA_real_,
                location_line_count_share = NA_real_,
                line_rai = NA_real_,
                weighted_line_rai = NA_real_,
                rai = NA_real_,
                marker_dataset = "trap_kill",
            marker_layer = "Trap captures"
              )
            if (isTRUE(show_monitoring_density)) {
              obs_summary_location_dens <- dplyr::bind_rows(obs_summary_location_dens, trap_density_summary_dens)
            } else {
              obs_summary_location_dens <- trap_density_summary_dens
            }
          } else if (!isTRUE(show_monitoring_density)) {
            obs_summary_location_dens <- dplyr::tibble(
              locationID = character(),
              locationName = character(),
              locality = character(),
              line = character(),
              longitude = numeric(),
              latitude = numeric(),
              count = numeric(),
              observation_ids = list(),
              trap_checks = numeric(),
              first_check = as.Date(character()),
              last_check = as.Date(character()),
              check_span_days = integer(),
              mean_check_interval_days = numeric(),
              trap_days = numeric(),
              any_species_kill_count = numeric(),
              selected_species_kill_count = numeric(),
              kills_per_100_trap_days_selected_species = numeric(),
              kills_per_100_trap_days_any_species = numeric(),
              camera_hours = numeric(),
              line_count = numeric(),
              line_camera_hours = numeric(),
              location_line_count_share = numeric(),
              line_rai = numeric(),
              weighted_line_rai = numeric(),
              rai = numeric(),
              marker_dataset = character(),
              marker_layer = character()
            )
          }
        }

        if ("marker_dataset" %in% names(obs_summary_location_dens) && any(obs_summary_location_dens$marker_dataset == "monitoring", na.rm = TRUE)) {
          monitoring_layer_label <- if (isTRUE(trapped_selected_species_dens)) "Observation counts (trapped species)" else "Observation counts (non-trapped species)"
          obs_summary_location_dens <- obs_summary_location_dens %>%
            dplyr::mutate(
              marker_layer = dplyr::if_else(.data$marker_dataset == "monitoring", monitoring_layer_label, .data$marker_layer),
              marker_dataset = dplyr::if_else(.data$marker_dataset == "monitoring" & isTRUE(trapped_selected_species_dens), "monitoring_trapped", .data$marker_dataset),
              marker_dataset = dplyr::if_else(.data$marker_dataset == "monitoring" & !isTRUE(trapped_selected_species_dens), "monitoring_non_trapped", .data$marker_dataset)
            )
        }

        predicted_rai_surface <- NULL
        predicted_rai_surface_message <- NULL
        predicted_rai_surface_cache_key <- "surface-off"
        if (isTRUE(show_predicted_rai_surface_selected())) {
          surface_basis <- predicted_rai_surface_basis_selected()
          surface_basis_label <- if (identical(surface_basis, "weighted_line_rai")) {
            "location-weighted line RAI"
          } else {
            "line RAI"
          }
          surface_grid_n <- if (use_timeline) 38 else 45
          surface_time_dens <- current_time_dens

          if (use_timeline) {
            req(current_time_dens, timeline_period_start(), timeline_period_end())
          }

          surface_time_key <- if (use_timeline && !is.null(surface_time_dens)) {
            format(surface_time_dens, "%Y-%m-%d %H:%M:%S", tz = timeline_actual_timezone())
          } else {
            "current-selection"
          }

          surface_cache_key <- paste(
            id,
            if (use_timeline) "timeline-step" else "static",
            paste(sort(species_dens), collapse = ","),
            paste(sort(localities_dens), collapse = ","),
            period_key_dens,
            new_bounds_key,
            surface_basis,
            surface_time_key,
            exclude_possible_duplicates_selected(),
            sep = "|"
          )
          predicted_rai_surface_cache_key <- surface_cache_key

          surface_cache <- predicted_rai_surface_cache()
          if (!is.null(surface_cache) && identical(surface_cache$key, surface_cache_key)) {
            predicted_rai_surface <- surface_cache$surface
            predicted_rai_surface_message <- surface_cache$message
          } else {
            surface_rai_locations <- rai_location_dens
            if (use_timeline) {
              surface_obs_filtered_dens <- obs() %>%
                dplyr::filter(
                  scientificName_lower %in% species_dens,
                  locality %in% localities_dens,
                  timestamp >= timeline_period_start(),
                  timestamp <= surface_time_dens
                )

              if (isTRUE(exclude_possible_duplicates_selected()) &&
                  "possible_duplicate" %in% names(surface_obs_filtered_dens)) {
                surface_obs_filtered_dens <- surface_obs_filtered_dens %>%
                  dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
              }

              surface_obs_location_counts_dens <- surface_obs_filtered_dens %>%
                dplyr::group_by(locationID) %>%
                dplyr::summarise(
                  count = sum(count, na.rm = TRUE),
                  observation_ids = list(unique(observationID)),
                  .groups = "drop"
                )

              surface_location_summary_dens <- active_location_effort_dens %>%
                dplyr::left_join(surface_obs_location_counts_dens, by = "locationID") %>%
                dplyr::mutate(count = dplyr::coalesce(.data$count, 0))

              surface_line_rai_summary_dens <- surface_location_summary_dens %>%
                dplyr::group_by(locality, line) %>%
                dplyr::summarise(
                  line_count = sum(count, na.rm = TRUE),
                  line_camera_hours = sum(camera_hours, na.rm = TRUE),
                  .groups = "drop"
                ) %>%
                dplyr::mutate(
                  line_rai = dplyr::if_else(
                    is.finite(.data$line_camera_hours) & .data$line_camera_hours > 0,
                    (.data$line_count / .data$line_camera_hours) * config$globals$rai_norm_hours,
                    NA_real_
                  )
                )

              surface_rai_locations <- surface_location_summary_dens %>%
                dplyr::left_join(surface_line_rai_summary_dens, by = c("locality", "line")) %>%
                dplyr::mutate(
                  location_line_count_share = dplyr::if_else(
                    is.finite(.data$line_count) & .data$line_count > 0,
                    .data$count / .data$line_count,
                    0
                  ),
                  weighted_line_rai = dplyr::if_else(
                    is.finite(.data$line_rai),
                    .data$line_rai * .data$location_line_count_share,
                    NA_real_
                  ),
                  rai = .data$line_rai
                )
            }

            usable_rai_locations <- surface_rai_locations %>%
              dplyr::filter(
                is.finite(.data$longitude),
                is.finite(.data$latitude),
                is.finite(.data[[surface_basis]])
              )

            if (nrow(usable_rai_locations) < 3) {
              predicted_rai_surface_message <- sprintf(
                "Predicted RAI surface needs at least three active camera locations with %s data.",
                surface_basis_label
              )
            } else {
              predicted_rai_surface <- create_idw_prediction_surface(
                usable_rai_locations,
                value_col = surface_basis,
                group_col = "locality",
                grid_n = surface_grid_n
              )

              if (is.null(predicted_rai_surface) || nrow(predicted_rai_surface) == 0) {
                predicted_rai_surface_message <- "Predicted RAI surface is not available for the current camera layout."
              }
            }

            predicted_rai_surface_cache(
              list(
                key = surface_cache_key,
                surface = predicted_rai_surface,
                message = predicted_rai_surface_message
              )
            )
          }
        }
        
        max_location_count <- function(observations) {
          if (is.null(observations) || nrow(observations) == 0) {
            return(0)
          }

          counts <- observations %>%
            dplyr::group_by(locationID) %>%
            dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
            dplyr::pull(count)

          if (length(counts) == 0 || all(is.na(counts))) {
            return(0)
          }

          max(counts, na.rm = TRUE)
        }

        absolute_max <- if (identical(density_source_dens, "trapping")) {
          if (nrow(obs_summary_location_dens) > 0) {
            max(obs_summary_location_dens$count, na.rm = TRUE)
          } else {
            0
          }
        } else {
          max_location_count(obs_for_scale_dens)
        }

        if (use_timeline && identical(density_source_dens, "monitoring")) {
          timeline_absolute_max <- obs() %>%
            dplyr::filter(
              scientificName_lower %in% species_dens,
              locality %in% localities_dens,
              timestamp >= timeline_period_start(),
              timestamp <= timeline_period_end()
            )

          if (isTRUE(exclude_possible_duplicates_selected()) &&
              "possible_duplicate" %in% names(timeline_absolute_max)) {
            timeline_absolute_max <- timeline_absolute_max %>%
              dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
          }

          timeline_absolute_max <- max_location_count(timeline_absolute_max)

          if (!is.na(timeline_absolute_max) && !is.infinite(timeline_absolute_max)) {
            absolute_max <- timeline_absolute_max
          }
        }

        if (is.na(absolute_max) || is.infinite(absolute_max)) {
          absolute_max <- 0
        }

        shared_absolute_max <- absolute_max
        if (!is.null(density_scale_max_override) && is.function(density_scale_max_override)) {
          override_max <- suppressWarnings(as.numeric(density_scale_max_override()))
          if (length(override_max) > 0 && !is.na(override_max) && is.finite(override_max) && override_max > shared_absolute_max) {
            shared_absolute_max <- override_max
          }
        }
        density_marker_scale(shared_absolute_max)

        summary_title <- dplyr::case_when(
          identical(density_source_dens, "trapping") ~ "Trapping captures",
          identical(density_source_dens, "both") ~ "Density layers",
          TRUE ~ "Observation counts"
        )
        summary_control <- if (identical(density_source_dens, "both") || identical(species_display_mode_selected(), "separate")) {
          NULL
        } else {
          render_density_line_summary_control(obs_summary_location_dens, title = summary_title)
        }

        show_monitoring_marker_layer <- isTRUE(show_monitoring_density) &&
          isTRUE(show_density_location_markers_selected())
        separate_marker_input <- list(
          observations = if (isTRUE(show_monitoring_marker_layer)) obs_filtered_dens else obs_filtered_dens[0, , drop = FALSE],
          no_obs_deployments = if (isTRUE(show_monitoring_marker_layer)) {
            active_locations_dens %>%
              dplyr::filter(!.data$locationName %in% obs_filtered_dens$locationName)
          } else {
            active_locations_dens[0, , drop = FALSE]
          }
        )
        separate_markers <- create_map_markers(separate_marker_input, source_map_id = id)
        visible_trap_records_dens <- trap_records_dens
        if (nrow(visible_trap_records_dens) > 0 && "trap_marker_type" %in% names(visible_trap_records_dens)) {
          enabled_trap_marker_types_dens <- c(
            if (isTRUE(show_trap_kill_markers_selected())) "kill",
            if (isTRUE(show_trap_blank_checks_selected())) "check",
            if (isTRUE(show_trap_unchecked_locations_selected())) "unchecked"
          )
          visible_trap_records_dens <- visible_trap_records_dens %>%
            dplyr::filter(.data$trap_marker_type %in% enabled_trap_marker_types_dens)
        }
        if (nrow(visible_trap_records_dens) > 0) {
          separate_markers$markers[["trapping"]] <- list(
            species = "trapping",
            markers = create_trap_map_markers(visible_trap_records_dens),
            warning = NULL
          )
        }

        weather_control <- if (use_timeline) {
          step_size <- if (is.null(input$timeline_step_size)) "day" else input$timeline_step_size
          view_mode_dens <- if (is.null(input$timeline_view_mode)) "cumulative" else input$timeline_view_mode
          weather_time_dens <- timeline_window_reference_time(start_time_dens, current_time_dens, step_size, view_mode_dens)
          render_weather_map_control(
            weather_row_for_time(timeline_weather_data(), weather_time_dens),
            time_info = timeline_time_info_for_window(timeline_weather_data(), start_time_dens, current_time_dens, step_size, view_mode_dens)
          )
        } else {
          NULL
        }

        period_control <- if (use_timeline) {
          monitoring_window_dens <- if (density_source_dens %in% c("monitoring", "both")) {
            timeline_monitoring_window()
          } else {
            NULL
          }
          render_timeline_period_control(
            timeline_period_status(current_time_dens, timeline_period_groups()),
            monitoring_window_dens
          )
        } else {
          NULL
        }

        skip_notice <- render_playback_skip_notice(playback_gap_notice())

        observations_for_table <- dplyr::bind_rows(
          if (isTRUE(show_monitoring_density)) obs_filtered_dens else obs_filtered_dens[0, , drop = FALSE],
          visible_trap_records_dens
        )
        cumulative_observations_for_table <- dplyr::bind_rows(
          if (isTRUE(show_monitoring_density)) obs_cumulative_dens else obs_cumulative_dens[0, , drop = FALSE],
          trap_cumulative_records_dens
        )

        list(
          active_locations = active_locations_dens,
          obs_summary_location = obs_summary_location_dens,
          start_time = start_time_dens,
          current_time = current_time_dens,
          absolute_max = shared_absolute_max,
          obs_filtered = obs_filtered_dens,
          obs_cumulative = obs_cumulative_dens,
          observations_for_table = observations_for_table,
          cumulative_observations_for_table = cumulative_observations_for_table,
          trap_records = visible_trap_records_dens,
          trap_support_locations = trap_support_locations_dens,
          separate_markers = separate_markers,
          trap_legend = render_trap_marker_legend(visible_trap_records_dens, separate_marker_input$observations),
          species_display_mode = species_display_mode_selected(),
          predicted_rai_surface = predicted_rai_surface,
          predicted_rai_surface_message = predicted_rai_surface_message,
          predicted_rai_surface_basis = predicted_rai_surface_basis_selected(),
          map_update_key = paste(
            id,
            if (use_timeline) "timeline" else "static",
            density_source_dens,
            paste(sort(species_dens), collapse = ","),
            paste(sort(localities_dens), collapse = ","),
            period_key_dens,
            new_bounds_key,
            if (is.null(start_time_dens)) "no-start" else format(start_time_dens, "%Y-%m-%d %H:%M:%S", tz = timeline_actual_timezone()),
            if (is.null(current_time_dens)) "no-current" else format(current_time_dens, "%Y-%m-%d %H:%M:%S", tz = timeline_actual_timezone()),
            exclude_possible_duplicates_selected(),
            include_monitoring_records_selected(),
            include_trap_data_selected(),
            exclude_untrapped_species_selected(),
            shared_absolute_max,
            trap_locality_distance_km_selected(),
            show_trap_kill_markers_selected(),
            show_trap_blank_checks_selected(),
            show_trap_unchecked_locations_selected(),
            show_density_location_markers_selected(),
            species_display_mode_selected(),
            show_predicted_rai_surface_selected(),
            predicted_rai_surface_basis_selected(),
            predicted_rai_surface_cache_key,
            if (is.null(skip_notice)) "no-skip-notice" else skip_notice,
            sep = "|"
          ),
          show_location_markers = show_density_location_markers_selected(),
          show_zero_markers = isTRUE(show_monitoring_marker_layer),
          density_data_source = density_source_dens,
          marker_value_label = dplyr::case_when(
            identical(density_source_dens, "trapping") ~ "Trap captures",
            identical(density_source_dens, "both") ~ "Density count",
            TRUE ~ "Observation counts"
          ),
          marker_metric = "count",
          weather_control = weather_control,
          summary_control = summary_control,
          period_control = period_control,
          skip_notice = skip_notice
        )
      })
      
      # Observer to update the content of the map display
      observe({
        req(mapping_data_density())
        logger::log_debug(sprintf("mapping_module_server [density], %s observer updating map content on %s", id, MAP_ID))
        data_for_map <- mapping_data_density()

        if (!is.null(data_for_map$map_update_key) &&
            identical(last_density_map_update_key(), data_for_map$map_update_key)) {
          if (isTRUE(needs_fit_bounds())) {
            apply_map_fit_bounds()
            needs_fit_bounds(FALSE)
          }
          return()
        }
        last_density_map_update_key(data_for_map$map_update_key)
        
        if (identical(data_for_map$species_display_mode, "separate")) {
          update_map(
            data_for_map$separate_markers,
            MAP_ID,
            data_for_map$active_locations,
            weather_control_html = data_for_map$weather_control,
            period_control_html = data_for_map$period_control,
            skip_notice_html = data_for_map$skip_notice,
            trap_legend_html = data_for_map$trap_legend
          )
        } else {
          update_density_map(
            map_id = MAP_ID,
            active_locations = data_for_map$active_locations,
            obs_summary_location = data_for_map$obs_summary_location,
            show_zero = data_for_map$show_zero_markers,
            absolute_max = data_for_map$absolute_max,
            predicted_rai_surface = data_for_map$predicted_rai_surface,
            predicted_rai_surface_message = data_for_map$predicted_rai_surface_message,
            show_location_markers = data_for_map$show_location_markers,
            marker_metric = data_for_map$marker_metric,
            marker_value_label = data_for_map$marker_value_label,
            density_data_source = data_for_map$density_data_source,
            trap_support_locations = data_for_map$trap_support_locations,
            weather_control_html = data_for_map$weather_control,
            period_control_html = data_for_map$period_control,
            summary_control_html = data_for_map$summary_control,
            skip_notice_html = data_for_map$skip_notice,
            source_map_id = id
          )
        }
        if (isTRUE(needs_fit_bounds())) {
          apply_map_fit_bounds()
          needs_fit_bounds(FALSE)
        }
      })
      
      # --- Map record outputs ---
      collapse_selection_values <- function(values) {
        values <- as.character(values)
        values <- values[!is.na(values) & nzchar(values)]
        if (length(values) == 0) {
          return("")
        }

        paste(values, collapse = "; ")
      }

      format_export_datetime <- function(value) {
        if (is.null(value) || length(value) == 0 || is.na(value)) {
          return("")
        }

        format(as.POSIXct(value, tz = timeline_actual_timezone()), "%Y-%m-%d %H:%M:%S %Z", tz = timeline_actual_timezone())
      }

      format_export_date <- function(value) {
        if (is.null(value) || length(value) == 0 || is.na(value)) {
          return("")
        }

        as.character(as.Date(value, tz = timeline_actual_timezone()))
      }

      prepare_map_records_export_metadata <- function(processed_data, export_data) {
        period_intervals_value <- if (is.function(period_intervals)) {
          period_intervals()
        } else {
          NULL
        }
        period_names_value <- if (!is.null(period_intervals_value) && "period_name" %in% names(period_intervals_value)) {
          period_intervals_value$period_name
        } else {
          character()
        }
        period_interval_text <- if (!is.null(period_intervals_value) && nrow(period_intervals_value) > 0) {
          collapse_selection_values(vapply(seq_len(nrow(period_intervals_value)), function(index) {
            sprintf(
              "%s: %s to %s",
              period_intervals_value$period_name[[index]],
              format_export_date(period_intervals_value$start_date[[index]]),
              format_export_date(period_intervals_value$end_date[[index]])
            )
          }, character(1)))
        } else {
          ""
        }

        selected_start_date <- if (is.function(period_start_date)) period_start_date() else NULL
        selected_end_date <- if (is.function(period_end_date)) period_end_date() else NULL

        data.frame(
          Criterion = c(
            "Downloaded at",
            "Selected seasons",
            "Selected season intervals",
            "Selected date range",
            "Timeline window",
            "Timeline view mode",
            "Timeline increment",
            "Selected species",
            "Selected localities",
            "Include monitoring records",
            "Exclude possible duplicates",
            "Exclude untrapped species",
            "Include trapping records",
            "Show trap capture markers",
            "Show trap check counters",
            "Show unchecked traps",
            "Trap locality distance km",
            "Exported rows"
          ),
          Value = c(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = timeline_actual_timezone()),
            collapse_selection_values(period_names_value),
            period_interval_text,
            sprintf("%s to %s", format_export_date(selected_start_date), format_export_date(selected_end_date)),
            sprintf("%s to %s", format_export_datetime(processed_data$start_time), format_export_datetime(processed_data$current_time)),
            if (is.null(input$timeline_view_mode)) "" else as.character(input$timeline_view_mode),
            if (is.null(input$timeline_step_size)) "" else as.character(input$timeline_step_size),
            collapse_selection_values(current_selected_species()),
            collapse_selection_values(current_selected_localities()),
            as.character(isTRUE(include_monitoring_records_selected())),
            as.character(isTRUE(exclude_possible_duplicates_selected())),
            as.character(isTRUE(exclude_untrapped_species_selected())),
            as.character(isTRUE(include_trap_data_selected())),
            as.character(isTRUE(show_trap_kill_markers_selected())),
            as.character(isTRUE(show_trap_blank_checks_selected())),
            as.character(isTRUE(show_trap_unchecked_locations_selected())),
            as.character(trap_locality_distance_km_selected()),
            as.character(nrow(export_data))
          ),
          check.names = FALSE
        )
      }

      output$map_data_table <- DT::renderDataTable({
        processed_data <- req(mapping_data_density())
        table_data <- prepare_map_records_table(processed_data$observations_for_table)

        DT::datatable( table_data, escape = FALSE,
                       options = list( pageLength = 10, searching = TRUE, lengthChange = TRUE, order = list(list(3, 'asc')) ),
                       class = 'display', rownames = FALSE
        )
      })

      output$download_map_records_export <- downloadHandler(
        filename = function() {
          paste0("map-records-export-", format(Sys.Date(), "%Y-%m-%d"), ".csv")
        },
        content = function(file) {
          processed_data <- req(mapping_data_density())
          export_data <- prepare_map_records_export(processed_data$observations_for_table)
          metadata <- prepare_map_records_export_metadata(processed_data, export_data)
          connection <- file(file, open = "w")
          on.exit(close(connection), add = TRUE)
          utils::write.csv(metadata, connection, row.names = FALSE, na = "")
          writeLines("", connection)
          utils::write.csv(export_data, connection, row.names = FALSE, na = "")
        }
      )

      output$map_cumulative_data_table <- DT::renderDataTable({
        processed_data <- req(mapping_data_density())
        table_data <- prepare_map_records_table(processed_data$cumulative_observations_for_table)

        DT::datatable( table_data, escape = FALSE,
                       options = list( pageLength = 10, searching = TRUE, lengthChange = TRUE, order = list(list(3, 'asc')) ),
                       class = 'display', rownames = FALSE
        )
      })

      output$map_textoverlay_warning <- renderUI({
        data <- req(mapping_data_density())
        warnings <- data$separate_markers$warnings
        if (!is.null(warnings) && length(warnings) > 0) {
          div(class = "map-overlay-warning", HTML(paste("Warning: ", warnings, collapse = "<br>")))
        }
      })

      output$timeline_window_ui <- renderUI({
        req(timeline_active(), input$time_slider)

        step_size <- if (is.null(input$timeline_step_size)) "day" else input$timeline_step_size
        current_time <- timeline_effective_current_time(
          input$time_slider,
          step_size,
          timeline_period_end(),
          timeline_points()
        )
        view_mode <- if (is.null(input$timeline_view_mode)) "cumulative" else input$timeline_view_mode
        start_time <- if (identical(view_mode, "single")) {
          if (step_size %in% c("day_night", "diel")) {
            previous_weather_boundary(current_time, timeline_period_start(), timeline_weather_data(), step_size)
          } else if (identical(step_size, "season")) {
            timeline_current_season_start(current_time, timeline_period_groups(), timeline_period_start())
          } else {
            timeline_single_window_start(
              current_time,
              timeline_period_start(),
              step_size,
              timeline_step_seconds()
            )
          }
        } else {
          timeline_period_start()
        }

        HTML(timeline_window_readout(
          start_time,
          current_time,
          step_size,
          timeline_weather_data(),
          view_mode
        ))
      })

      root_session <- session$rootScope()
      if (is.null(root_session$userData$pdf_export_density_map_renderers)) {
        root_session$userData$pdf_export_density_map_renderers <- list()
      }

      root_session$userData$pdf_export_density_map_renderers[[MAP_ID]] <- function(width = NULL, height = NULL, export_dir = NULL) {
        data_for_map <- isolate(mapping_data_density())
        export_map <- create_pdf_export_density_map(
          active_locations = data_for_map$active_locations,
          obs_summary_location = data_for_map$obs_summary_location,
          show_zero = TRUE,
          predicted_rai_surface = data_for_map$predicted_rai_surface,
          show_location_markers = data_for_map$show_location_markers,
          marker_metric = data_for_map$marker_metric,
          width = width,
          height = height
        )

        render_pdf_export_leaflet_png(
          map = export_map,
          map_id = MAP_ID,
          export_dir = export_dir,
          width = width,
          height = height,
          config = config
        )
      }
      
      # Observer for auto-recentering maps when the summary view becomes active.
      observe({
        main_nav <- session$rootScope()$input$nav
        sub_tab <- if (identical(main_nav, "monitoring_trapping_map")) {
          session$rootScope()$input[["monitoring_trapping_map_comparison-density_comparison_tabs"]]
        } else {
          session$rootScope()$input[["density_map_comparison-density_comparison_tabs"]]
        }
        
        req(
          identical(timeline_mode, "none"),
          main_nav %in% c("density_map", "monitoring_trapping_map"),
          is.null(sub_tab) || sub_tab == "map"
        )
        
        shinyjs::runjs(sprintf( # GA event
          "gtag('event','tab_switch',{
             'event_category':'sub_tab_navigation',
             'event_label': %s,
             'value': %s
           });",
          jsonlite::toJSON(paste0("main_menu_", main_nav, "_tab_switch"), auto_unbox = TRUE),
          jsonlite::toJSON(sub_tab, auto_unbox = TRUE)
        ))
        
        logger::log_debug(sprintf(
          "mapping_module_server [density], %s auto-recenter due to tab switch to '%s' tab",
          id, sub_tab
        ))
        recenter_map_generic() # Use the generic recenter function
      })

      observe({
        main_nav <- session$rootScope()$input$nav
        timeline_tab <- input$density_timeline_tabs

        req(identical(timeline_mode, "always"), main_nav == "density_timeline_map")
        req(is.null(timeline_tab) || identical(timeline_tab, "map"))

        logger::log_debug(sprintf(
          "mapping_module_server [density timeline], %s auto-recenter due to navigation/tab switch",
          id
        ))
        recenter_map_generic()
      })
      
      return(list(
        selected_species = current_selected_species,
        selected_localities = current_selected_localities,
        show_predicted_rai_surface = show_predicted_rai_surface_selected,
        predicted_rai_surface_basis = predicted_rai_surface_basis_selected,
        species_display_mode = species_display_mode_selected,
        show_density_location_markers = show_density_location_markers_selected,
        show_trap_kill_markers = show_trap_kill_markers_selected,
        density_data_source = density_data_source_selected,
        density_marker_scale = density_marker_scale,
        recenter_map = recenter_map_generic # Return the generic recenter function
      ))

    }
  })
}



format_density_summary_value <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 0) {
    return(character(0))
  }

  formatted <- format(round(value), big.mark = ",", trim = TRUE)
  formatted[is.na(value) | !is.finite(value)] <- "0"
  formatted
}

render_density_line_summary_control <- function(summary_data, title = "Summary") {
  if (is.null(summary_data) || nrow(summary_data) == 0 ||
      !all(c("locality", "line", "count") %in% names(summary_data))) {
    return(NULL)
  }

  summary_rows <- summary_data %>%
    dplyr::mutate(
      locality = dplyr::if_else(is.na(.data$locality) | !nzchar(as.character(.data$locality)), "Unknown", as.character(.data$locality)),
      line = dplyr::if_else(is.na(.data$line) | !nzchar(as.character(.data$line)), "-", as.character(.data$line)),
      count = dplyr::coalesce(suppressWarnings(as.numeric(.data$count)), 0)
    ) %>%
    dplyr::group_by(.data$locality, .data$line) %>%
    dplyr::summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop")

  if (nrow(summary_rows) == 0) {
    return(NULL)
  }

  line_levels <- sort(unique(summary_rows$line), na.last = TRUE)
  locality_levels <- sort(unique(summary_rows$locality), na.last = TRUE)
  total_by_locality <- summary_rows %>%
    dplyr::group_by(.data$locality) %>%
    dplyr::summarise(total = sum(.data$count, na.rm = TRUE), .groups = "drop")

  body_rows <- vapply(locality_levels, function(locality_value) {
    locality_counts <- summary_rows %>% dplyr::filter(.data$locality == locality_value)
    cells <- vapply(line_levels, function(line_value) {
      matched <- locality_counts$count[locality_counts$line == line_value]
      if (length(matched) == 0) {
        return("<td class='density-summary-line density-summary-missing'>-</td>")
      }
      sprintf("<td class='density-summary-line'>%s</td>", htmltools::htmlEscape(format_density_summary_value(sum(matched, na.rm = TRUE))))
    }, character(1), USE.NAMES = FALSE)
    total <- total_by_locality$total[total_by_locality$locality == locality_value]
    sprintf(
      "<tr><th scope='row' class='density-summary-locality'>%s</th><td class='density-summary-total'>%s</td>%s</tr>",
      htmltools::htmlEscape(locality_value),
      htmltools::htmlEscape(format_density_summary_value(sum(total, na.rm = TRUE))),
      paste(cells, collapse = "")
    )
  }, character(1), USE.NAMES = FALSE)

  column_totals <- vapply(line_levels, function(line_value) {
    total <- sum(summary_rows$count[summary_rows$line == line_value], na.rm = TRUE)
    sprintf("<td class='density-summary-line'>%s</td>", htmltools::htmlEscape(format_density_summary_value(total)))
  }, character(1), USE.NAMES = FALSE)

  paste0(
    "<div class='density-summary-control'><strong>", htmltools::htmlEscape(title), "</strong>",
    "<table><thead><tr><th scope='col' class='density-summary-locality'>Locality</th><th scope='col' class='density-summary-total'>Total</th>",
    paste(sprintf("<th scope='col' class='density-summary-line'>%s</th>", htmltools::htmlEscape(line_levels)), collapse = ""),
    "</tr></thead><tbody>",
    paste(body_rows, collapse = ""),
    "</tbody><tfoot><tr><th scope='row' class='density-summary-locality'>Total</th>",
    "<td class='density-summary-total'>", htmltools::htmlEscape(format_density_summary_value(sum(summary_rows$count, na.rm = TRUE))), "</td>",
    paste(column_totals, collapse = ""),
    "</tr></tfoot></table></div>"
  )
}

update_density_map <- function(map_id = NULL, 
                               active_locations = NULL, 
                               obs_summary_location = NULL, 
                               show_zero = TRUE,
                               absolute_max = NULL,
                               predicted_rai_surface = NULL,
                               predicted_rai_surface_message = NULL,
                               show_location_markers = TRUE,
                               marker_metric = "count",
                               marker_value_label = NULL,
                               density_data_source = "monitoring",
                               trap_support_locations = NULL,
                               weather_control_html = NULL,
                               period_control_html = NULL,
                               summary_control_html = NULL,
                               skip_notice_html = NULL,
                               source_map_id = NULL) {

  density_base_radius <- if (!is.null(config$globals$density_base_radius)) config$globals$density_base_radius else 75
  density_max_radius <- if (!is.null(config$globals$density_max_radius)) config$globals$density_max_radius else 150
  density_min_fill_area <- if (!is.null(config$globals$density_min_fill_area)) config$globals$density_min_fill_area else 0.16
  density_pre_full_alpha <- if (!is.null(config$globals$density_pre_full_alpha)) config$globals$density_pre_full_alpha else 0.62
  
  density_scale_gears <- if (!is.null(config$globals$density_scale_gears)) config$globals$density_scale_gears else c("100" = 30, "40" = 12, "0" = 4)
  density_scale_gears <- density_scale_gears[order(as.numeric(names(density_scale_gears)), decreasing = TRUE)]

  marker_metric <- if (!is.null(marker_metric) && marker_metric %in% c("count", "rai")) marker_metric else "count"
  if (is.null(marker_value_label) || !nzchar(as.character(marker_value_label))) {
    marker_value_label <- if (identical(marker_metric, "rai")) "Line RAI" else "Count"
  }
  density_side <- if (!is.null(source_map_id) && grepl("primary|monitoring$|_monitoring$", source_map_id)) "primary" else "comparative"
  summary_position <- if (identical(density_side, "primary")) "topleft" else "topright"
  legend_position <- if (identical(density_side, "primary")) "bottomleft" else "bottomright"

  # Clear the map
  proxy <- leafletProxy(map_id) %>%
    clearMarkers() %>%
    clearShapes() %>%
    clearControls()

  if (!is.null(weather_control_html)) {
    proxy <- proxy %>% addControl(
      html = weather_control_html,
      position = "topright",
      className = "map-weather-control"
    )
  }

  if (!is.null(period_control_html)) {
    proxy <- proxy %>% addControl(
      html = period_control_html,
      position = "topright",
      className = "map-season-control"
    )
  }

  if (!is.null(summary_control_html)) {
    proxy <- proxy %>% addControl(
      html = summary_control_html,
      position = summary_position,
      className = paste("density-summary-control-wrapper", paste0("map-density-summary-control-", density_side))
    )
  }

  if (!is.null(skip_notice_html)) {
    proxy <- proxy %>% addControl(
      html = skip_notice_html,
      position = "topleft",
      className = "map-playback-skip-control"
    )
  }
  
  has_trap_support_data <- density_data_source %in% c("trapping", "both") &&
    !is.null(trap_support_locations) && nrow(trap_support_locations) > 0

  # Check if obs_summary_location has any rows
  if (nrow(obs_summary_location) > 0) {
    max_count <- max(obs_summary_location$count, na.rm = TRUE)
    if (!is.null(absolute_max) && absolute_max > 0) {
      max_count <- max(max_count, absolute_max)
    }
  } else {
    logger::log_debug(sprintf("mapping_module_server, %s update_density_map() has no results available", map_id))
    max_count <- if (!is.null(absolute_max)) absolute_max else NA
    
    # Show "No results!" message 
    if ((!show_zero || nrow(active_locations) == 0) && !isTRUE(has_trap_support_data)) {
      logger::log_debug(sprintf("mapping_module_server, %s update_density_map() creating popup saying no results", map_id))
      proxy %>% addControl(
        html = "<strong>No markers to show! Check your selection filter criteria.</strong>", 
        position = "topright", 
        className = "no-results-message"
      )
      return()
    }
  }
  
  if (show_zero) {
    zero_icon <- get_species_icon(species = "none")
    
    zero_entries <- active_locations %>%
      anti_join(obs_summary_location, by = "locationID") %>%
      mutate(count = 0)
    
    obs_summary_location <- bind_rows(obs_summary_location, zero_entries)
  }
  
  if (identical(marker_metric, "rai") && "line_rai" %in% names(obs_summary_location)) {
    obs_summary_location$marker_value <- ifelse(is.finite(obs_summary_location$line_rai), obs_summary_location$line_rai, 0)
  } else {
    obs_summary_location$marker_value <- ifelse(is.finite(obs_summary_location$count), obs_summary_location$count, 0)
  }

  max_marker_value <- if (nrow(obs_summary_location) > 0) {
    max(obs_summary_location$marker_value, na.rm = TRUE)
  } else {
    NA_real_
  }
  if (identical(marker_metric, "count") && !is.null(absolute_max) && absolute_max > 0) {
    max_marker_value <- max(max_marker_value, absolute_max)
  }

  can_scale_marker_radius <- !is.na(max_marker_value) && max_marker_value > 0

  # Bucketed dynamic scaling gears to maintain comparability across time periods.
  dynamic_base_count <- if (can_scale_marker_radius && identical(marker_metric, "count")) {
    base_val <- density_scale_gears[[length(density_scale_gears)]]
    for (i in seq_along(density_scale_gears)) {
      if (max_marker_value >= as.numeric(names(density_scale_gears)[i])) {
        base_val <- density_scale_gears[[i]]
        break
      }
    }
    unname(base_val)
  } else {
    unname(density_scale_gears[[length(density_scale_gears)]])
  }

  density_full_base_value <- if (identical(marker_metric, "count")) {
    dynamic_base_count
  } else {
    max_marker_value
  }

  # Set the variable used in the density_fill_area calculation
  density_count_full_at <- density_full_base_value

  density_radius_growth_available <- can_scale_marker_radius &&
    is.finite(density_full_base_value) &&
    max_marker_value > density_full_base_value
    


  obs_summary_location <- obs_summary_location %>%
    mutate(
      density_fill_area = dplyr::case_when(
        marker_value <= 0 | !can_scale_marker_radius ~ 0,

        identical(marker_metric, "count") ~ pmin(
          1,
          density_min_fill_area +
            (1 - density_min_fill_area) *
            ((pmin(marker_value, density_count_full_at) - 1) /
              max(density_count_full_at - 1, 1))^1.25
        ),

        TRUE ~ pmin(
          1,
          pmax(
            density_min_fill_area,
            marker_value / density_full_base_value
          )
        )
      ),

      density_halo_radius = ifelse(
        marker_value > density_full_base_value & density_radius_growth_available,
        scales::rescale(
          sqrt(marker_value),
          to = c(density_base_radius, density_max_radius),
          from = c(sqrt(density_full_base_value), sqrt(max_marker_value))
        ),
        density_base_radius
      ),

      radius = ifelse(
        marker_value > density_full_base_value & density_radius_growth_available,
        density_halo_radius,
        density_base_radius * sqrt(density_fill_area)
      ),

      density_opacity = dplyr::case_when(
        marker_value <= 0 | !can_scale_marker_radius ~ 0.26,
        density_fill_area < 1 ~ density_pre_full_alpha,
        TRUE ~ 0.76
      ),

      density_halo_opacity = ifelse(marker_value > 0 & can_scale_marker_radius, 0.18, 0.08),
      density_border_opacity = ifelse(marker_value > 0 & can_scale_marker_radius, 0.55, 0.25)
    )

  pal_domain <- if (!is.na(max_marker_value) && max_marker_value > 0) c(0, max_marker_value) else obs_summary_location$marker_value
  density_marker_color <- "#dc2626"
  pal <- colorNumeric(palette = c("#fecaca", density_marker_color), domain = pal_domain)
  if (!"marker_dataset" %in% names(obs_summary_location)) {
    obs_summary_location$marker_dataset <- if (identical(density_data_source, "trapping")) "trap_kill" else "monitoring_non_trapped"
  }
  obs_summary_location <- obs_summary_location %>%
    dplyr::mutate(
      marker_color = dplyr::case_when(
        .data$marker_dataset == "trap_kill" ~ "#dc2626",
        .data$marker_dataset == "monitoring_trapped" ~ "#9333ea",
        .data$marker_dataset == "monitoring_non_trapped" ~ "#16a34a",
        TRUE ~ density_marker_color
      )
    )

  if (!is.null(predicted_rai_surface) && nrow(predicted_rai_surface) > 0) {
    surface_max <- max(predicted_rai_surface$predicted_rai, na.rm = TRUE)
    surface_domain <- if (is.finite(surface_max) && surface_max > 0) {
      c(0, surface_max)
    } else {
      c(0, 1)
    }
    surface_pal <- colorNumeric(
      palette = "YlOrRd",
      domain = surface_domain
    )

    proxy %>%
      addPolygons(
        data = predicted_rai_surface,
        fillColor = ~surface_pal(predicted_rai),
        fillOpacity = 0.42,
        stroke = FALSE,
        smoothFactor = 0,
        label = ~sprintf("%s predicted RAI: %0.2f", locality, predicted_rai),
        group = "Predicted RAI surface"
      ) %>%
      addLegend(
        "bottomleft",
        pal = surface_pal,
        values = predicted_rai_surface$predicted_rai,
        title = "Predicted RAI",
        labFormat = labelFormat(),
        opacity = 0.8
      )
  } else if (!is.null(predicted_rai_surface_message) && nzchar(predicted_rai_surface_message)) {
    proxy %>%
      addControl(
        html = sprintf("<strong>Predicted RAI surface unavailable</strong><br><small>%s</small>", predicted_rai_surface_message),
        position = "topleft",
        className = "map-prediction-message-control"
      )
  }

  format_marker_value <- function(value, digits = 1) {
    if (is.null(value) || length(value) == 0 || !is.finite(value)) {
      return("N/A")
    }

    format(round(as.numeric(value), digits), big.mark = ",", nsmall = digits, trim = TRUE)
  }

  rai_norm_hours <- if (exists("config", inherits = TRUE) &&
                        !is.null(config$globals$rai_norm_hours)) {
    config$globals$rai_norm_hours
  } else {
    NA_real_
  }

  marker_popup_html <- function(marker_data, include_review_link = TRUE) {
    marker_field <- function(field, i, default = NA) {
      if (!field %in% names(marker_data)) {
        return(default)
      }

      marker_data[[field]][[i]]
    }

    vapply(seq_len(nrow(marker_data)), function(i) {
      observation_ids <- if ("observation_ids" %in% names(marker_data) &&
                            is.list(marker_data$observation_ids)) {
        unlist(marker_data$observation_ids[[i]], use.names = FALSE)
      } else {
        character(0)
      }
      observation_ids <- as.character(observation_ids)
      observation_ids <- observation_ids[!is.na(observation_ids) & nzchar(observation_ids)]

      safe_marker_text <- function(value) {
        if (is.null(value) || length(value) == 0 || is.na(value)) {
          return("")
        }

        htmltools::htmlEscape(as.character(value))
      }

      line_value <- marker_field("line", i, "")
      line_text <- if (!is.na(line_value) && nzchar(as.character(line_value))) {
        paste0("Line ", safe_marker_text(line_value))
      } else {
        ""
      }
      location_text <- safe_marker_text(marker_field("locationName", i, ""))
      locality_text <- safe_marker_text(marker_field("locality", i, ""))
      locality_line_text <- paste(c(locality_text, line_text)[nzchar(c(locality_text, line_text))], collapse = ", ")

      marker_dataset <- marker_field("marker_dataset", i, density_data_source)
      if (identical(marker_dataset, "trap_kill")) {
        trap_record <- marker_data[i, , drop = FALSE]
        trap_record$trap_line <- line_value
        trap_record$selected_species_kill_count <- dplyr::coalesce(
          suppressWarnings(as.numeric(marker_field("selected_species_kill_count", i, NA_real_))),
          suppressWarnings(as.numeric(marker_field("count", i, NA_real_)))
        )

        return(create_trap_location_popup_content(
          trap_record,
          capture_links_html = trap_capture_records_html(observation_ids)
        ))
      }

      review_link <- ""
      if (isTRUE(include_review_link) && length(observation_ids) > 0) {
        payload <- list(
          map_id = source_map_id,
          location_name = marker_field("locationName", i, ""),
          locality = marker_field("locality", i, ""),
          observation_ids = observation_ids
        )
        payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
        encoded_payload <- utils::URLencode(payload_json, reserved = TRUE)
        onclick_js <- sprintf(
          "Shiny.setInputValue('density_map_review_sequences_click', JSON.parse(decodeURIComponent('%s')), {priority: 'event'}); return false;",
          encoded_payload
        )
        review_link <- sprintf("<br><a href='#' onclick=\"%s\" title='Review Sequences'>Review Sequences</a>", onclick_js)
      }

      paste0(
        "<strong>Location: ", location_text, "</strong>",
        if (nzchar(locality_line_text)) paste0("<br>", locality_line_text) else "",
        "<br><br><strong>Observations:</strong>",
        "<br>Location: ", format_marker_value(marker_field("count", i), digits = 0),
        " (effort: ", format_marker_value(marker_field("camera_hours", i), digits = 1), " hours)",
        "<br>Line: ", format_marker_value(marker_field("line_count", i), digits = 0),
        " (effort: ", format_marker_value(marker_field("line_camera_hours", i), digits = 1), " hours)",
        "<br>Location share of line count: ", format_marker_value(marker_field("location_line_count_share", i) * 100, digits = 1), "%",
        "<br><br><strong>RAI:</strong>",
        "<br>Line RAI: ", format_marker_value(marker_field("line_rai", i), digits = 2),
        " (per ", format_marker_value(rai_norm_hours, digits = 0), " hours)",
        "<br>Weighted Location-Line RAI: ", format_marker_value(marker_field("weighted_line_rai", i), digits = 2),
        review_link
      )
    }, character(1))
  }

  has_trap_density_layers <- density_data_source %in% c("trapping", "both") &&
    any(obs_summary_location$marker_dataset == "trap_kill", na.rm = TRUE)
  has_trap_support_layers <- isTRUE(has_trap_support_data)
  if (isTRUE(show_location_markers) || isTRUE(has_trap_density_layers) || isTRUE(has_trap_support_layers)) {
    obs_summary_location$popup_content <- marker_popup_html(obs_summary_location, include_review_link = TRUE)
    obs_summary_location$label_content <- marker_popup_html(obs_summary_location, include_review_link = FALSE)

    circle_locations <- if (identical(marker_metric, "rai")) {
      obs_summary_location %>% filter(is.finite(marker_value))
    } else {
      obs_summary_location %>% filter(count > 0)
    }
    if (!isTRUE(show_location_markers)) {
      circle_locations <- circle_locations %>% dplyr::filter(.data$marker_dataset == "trap_kill")
    }

    add_density_circles <- function(locations) {
      if (is.null(locations) || nrow(locations) == 0) {
        return(invisible(NULL))
      }

      proxy %>%
        addCircles(
          data = locations,
          lng = ~longitude, lat = ~latitude,
          radius = ~density_halo_radius,
          fillColor = ~marker_color,
          fillOpacity = ~density_halo_opacity,
          color = ~marker_color,
          opacity = ~density_border_opacity,
          weight = 1,
          popup = ~popup_content,
          label = lapply(locations$label_content, htmltools::HTML),
          labelOptions = labelOptions(direction = "auto", opacity = 0.95)
        ) %>%
        addCircles(
          data = locations,
          lng = ~longitude, lat = ~latitude,
          radius = ~radius,
          fillColor = ~marker_color,
          fillOpacity = ~density_opacity,
          color = ~marker_color,
          opacity = ~density_border_opacity,
          weight = 1,
          popup = ~popup_content,
          label = lapply(locations$label_content, htmltools::HTML),
          labelOptions = labelOptions(direction = "auto", opacity = 0.95)
        )

      invisible(NULL)
    }

    non_trap_circle_locations <- circle_locations %>% dplyr::filter(.data$marker_dataset != "trap_kill")
    trap_kill_circle_locations <- circle_locations %>% dplyr::filter(.data$marker_dataset == "trap_kill")

    if (density_data_source %in% c("trapping", "both") &&
        !is.null(trap_support_locations) && nrow(trap_support_locations) > 0) {
      support_popup <- vapply(seq_len(nrow(trap_support_locations)), function(i) {
        support_record <- as.data.frame(trap_support_locations[i, ], stringsAsFactors = FALSE)
        type_label <- if (identical(as.character(support_record$density_support_type), "unchecked")) "Unchecked trap" else "Trap checks"
        paste0(
          "<strong>", htmltools::htmlEscape(type_label), ": ", htmltools::htmlEscape(safe_marker_value(support_record$locationName)), "</strong><br>",
          htmltools::htmlEscape(safe_marker_value(support_record$locality)),
          if ("trap_line" %in% names(support_record) && nzchar(safe_marker_value(support_record$trap_line))) paste0(", Trap line ", htmltools::htmlEscape(safe_marker_value(support_record$trap_line))) else "",
          "<br><br>",
          trap_metrics_popup_html(support_record, "selected species")
        )
      }, character(1))
      trap_support_locations$popup_content <- support_popup
      trap_support_locations$support_radius <- ifelse(trap_support_locations$density_support_type == "unchecked", 38, 50)
      trap_support_locations$support_color <- ifelse(trap_support_locations$density_support_type == "unchecked", "#6b7280", "#0284c7")
      trap_support_locations$support_fill <- ifelse(trap_support_locations$density_support_type == "unchecked", "#f3f4f6", "#e0f2fe")
      trap_support_locations$support_label <- ifelse(
        trap_support_locations$density_support_type == "unchecked",
        "0",
        format_density_summary_value(trap_support_locations$trap_checks)
      )

      proxy %>%
        addCircles(
          data = trap_support_locations,
          lng = ~longitude, lat = ~latitude,
          radius = ~support_radius,
          fillColor = ~support_fill,
          fillOpacity = 0.18,
          color = ~support_color,
          opacity = 0.75,
          weight = 1,
          dashArray = "3,6",
          popup = ~popup_content,
          label = ~support_label,
          labelOptions = labelOptions(
            permanent = TRUE,
            direction = "center",
            textOnly = TRUE,
            opacity = 1,
            textsize = "10px",
            className = "trap-check-count-label"
          )
        )
    }

    add_density_circles(non_trap_circle_locations)

    if (isTRUE(show_location_markers) && show_zero && identical(marker_metric, "count")) {
      zero_locations <- obs_summary_location %>% filter(count == 0)
      if (nrow(zero_locations) > 0) {
        proxy %>%
          addMarkers(
            data = zero_locations,
            lng = ~longitude, lat = ~latitude,
            icon = zero_icon,
            popup = ~popup_content,
            label = lapply(zero_locations$label_content, htmltools::HTML),
            labelOptions = labelOptions(direction = "auto", opacity = 0.95)
          )
      }
    }

    add_density_circles(trap_kill_circle_locations)

    if (!is.na(max_marker_value) && max_marker_value > 0) {
      visible_layers <- unique(circle_locations$marker_dataset)
      legend_rows <- c()
      if ("monitoring_trapped" %in% visible_layers) {
        legend_rows <- c(legend_rows, "<div class='trap-marker-legend-row'><span class='trap-marker-legend-swatch' style='background:#9333ea;'></span>Observation counts</div>")
      }
      if ("monitoring_non_trapped" %in% visible_layers) {
        legend_rows <- c(legend_rows, "<div class='trap-marker-legend-row'><span class='trap-marker-legend-swatch' style='background:#16a34a;'></span>Observation counts</div>")
      }
      if ("trap_kill" %in% visible_layers) {
        legend_rows <- c(legend_rows, "<div class='trap-marker-legend-row'><span class='trap-marker-legend-swatch' style='background:#dc2626;'></span>Trap captures</div>")
      }

      format_scale_val <- function(val) {
        if (identical(marker_metric, "rai")) {
          format(round(as.numeric(val), 2), nsmall = 2, trim = TRUE)
        } else {
          format_density_summary_value(val)
        }
      }

      legend_scale_text <- if (density_radius_growth_available) {
        paste0(
          "<div class='trap-marker-legend-note' style='margin-top: 6px; border-top: 1px solid rgba(0,0,0,0.15); padding-top: 5px;'>",
          "<strong>Scale (", htmltools::htmlEscape(marker_value_label), "):</strong>",
          "<div class='trap-marker-legend-row' style='margin-top: 4px;'>",
          "<span style='display:inline-block; width:8px; height:8px; border-radius:50%; background:rgba(108,117,125,0.4); border:1px solid rgba(108,117,125,0.6); margin-left:2px; margin-right:3px;'></span>",
          "<span style='white-space: nowrap;'>Colour darkens up to:&nbsp;<strong>", htmltools::htmlEscape(format_scale_val(density_full_base_value)), "</strong></span>",
          "</div>",
          "<div class='trap-marker-legend-row'>",
          "<span style='display:inline-block; width:12px; height:12px; border-radius:50%; background:rgba(108,117,125,0.76); border:1px solid rgba(108,117,125,0.6);'></span>",
          "<span style='white-space: nowrap;'>Circle grows up to:&nbsp;<strong>", htmltools::htmlEscape(format_scale_val(max_marker_value)), "</strong></span>",
          "</div>",
          "</div>"
        )
      } else {
        paste0(
          "<div class='trap-marker-legend-note' style='margin-top: 6px; border-top: 1px solid rgba(0,0,0,0.15); padding-top: 5px;'>",
          "<strong>Scale (", htmltools::htmlEscape(marker_value_label), "):</strong>",
          "<div class='trap-marker-legend-row' style='margin-top: 4px;'>",
          "<span style='display:inline-block; width:10px; height:10px; border-radius:50%; background:rgba(108,117,125,0.76); border:1px solid rgba(108,117,125,0.6); margin-left:1px; margin-right:2px;'></span>",
          "<span style='white-space: nowrap;'>Colour darkens up to:&nbsp;<strong>", htmltools::htmlEscape(format_scale_val(max_marker_value)), "</strong></span>",
          "</div>",
          "</div>"
        )
      }
      proxy %>%
        addControl(
          html = paste0("<div class='trap-marker-legend'><strong>Density key</strong>", paste(legend_rows, collapse = ""), legend_scale_text, "</div>"),
          position = legend_position,
          className = paste("trap-marker-legend-control", paste0("map-density-legend-", density_side))
        )
    }
  }
}




# interactive_map_functions.R
# Contains the functions that are ONLY used by the interactive maps
# Other functions not contained here are still required as well

# Function to create markers for given data, including the markers for other and no observations 
# deployments. The data  comes from reactive function relating to the map
# 
observation_marker_group_palette <- function() {
  c(
    stoat = "#b91c1c",
    weasel = "#8b5e34",
    ferret = "#f97316",
    rat = "#111827",
    mouse = "#737373",
    cat = "#7c3aed",
    rabbit = "#16a34a",
    hedgehog = "#0f766e",
    possum = "#2563eb",
    kiwi = "#854d0e",
    weka = "#15803d",
    bird = "#0284c7",
    mustelid = "#dc2626",
    other = "#8073ac"
  )
}

observation_marker_group_from_scientific_name <- function(scientific_name) {
  names <- tolower(ifelse(is.na(scientific_name), "", as.character(scientific_name)))
  dplyr::case_when(
    names == "mustela erminea" ~ "stoat",
    names == "mustela nivalis" ~ "weasel",
    names == "mustela putorius furo" ~ "ferret",
    names == "rattus" ~ "rat",
    names == "mus musculus" ~ "mouse",
    names == "felis catus" ~ "cat",
    names == "oryctolagus cuniculus" ~ "rabbit",
    names == "erinaceus europaeus" ~ "hedgehog",
    names == "trichosurus vulpecula" ~ "possum",
    names == "apteryx mantelli" ~ "kiwi",
    names == "gallirallus australis" ~ "weka",
    names == "aves" ~ "bird",
    names == "mustelidae" ~ "mustelid",
    TRUE ~ "other"
  )
}

observation_marker_color <- function(scientific_name) {
  group <- observation_marker_group_from_scientific_name(scientific_name)
  palette <- observation_marker_group_palette()
  unname(palette[ifelse(group %in% names(palette), group, "other")])
}

observation_species_icon_path <- function(scientific_name) {
  group <- observation_marker_group_from_scientific_name(scientific_name)
  paths <- c(
    stoat = "www/images/icons/map_icons/obs-stoat.svg",
    weasel = "www/images/icons/map_icons/obs-weasel.svg",
    ferret = "www/images/icons/map_icons/obs-ferret.svg",
    rat = "www/images/icons/map_icons/obs-rat.svg",
    mouse = "www/images/icons/map_icons/obs-mouse.svg",
    cat = "www/images/icons/map_icons/obs-cat.svg",
    rabbit = "www/images/icons/map_icons/obs-rabbit.svg",
    hedgehog = "www/images/icons/map_icons/obs-hedgehog.svg",
    possum = "www/images/icons/map_icons/obs-possum.svg",
    kiwi = "www/images/icons/map_icons/obs-kiwi.svg",
    weka = "www/images/icons/map_icons/obs-weka.svg",
    bird = "www/images/icons/map_icons/obs-bird.svg",
    mustelid = "www/images/icons/map_icons/obs-mustelid.svg",
    other = "www/images/icons/map_icons/obs-other.svg"
  )
  group <- ifelse(group %in% names(paths), group, "other")
  unname(paths[group])
}

get_observation_species_icon <- function(scientific_name) {
  makeIcon(
    iconUrl = observation_species_icon_path(scientific_name),
    iconWidth = 34,
    iconHeight = 34,
    iconAnchorX = 17,
    iconAnchorY = 17
  )
}

scale_summary_marker_radius <- function(value, max_value, range = c(8, 26)) {
  value <- suppressWarnings(as.numeric(value))
  max_value <- suppressWarnings(as.numeric(max_value))
  if (is.na(value) || !is.finite(value) || value <= 0) {
    return(range[[1]])
  }
  if (is.na(max_value) || !is.finite(max_value) || max_value <= 0) {
    return(mean(range))
  }

  scales::rescale(sqrt(value), to = range, from = c(0, sqrt(max_value)))
}

apply_summary_offsets <- function(summary_data, group_cols = c("locationID"), offset_value = NULL) {
  if (is.null(summary_data) || nrow(summary_data) == 0) {
    return(summary_data)
  }

  if (is.null(offset_value)) {
    offset_value <- config$globals$marker_offset_value
  }

  summary_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::arrange(.data$scientificName_lower, .by_group = TRUE) %>%
    dplyr::mutate(
      marker_index = dplyr::row_number(),
      marker_group_count = dplyr::n(),
      marker_angle = 2 * pi * (.data$marker_index - 1) / pmax(.data$marker_group_count, 1),
      marker_offset = dplyr::if_else(.data$marker_group_count > 1, offset_value * 1.35, 0),
      marker_latitude = .data$latitude + sin(.data$marker_angle) * .data$marker_offset,
      marker_longitude = .data$longitude + cos(.data$marker_angle) * .data$marker_offset
    ) %>%
    dplyr::ungroup()
}

safe_marker_value <- function(value, default = "") {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(default)
  }

  as.character(value)
}

format_summary_count <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (is.na(value) || !is.finite(value)) {
    return("0")
  }

  format(round(value), big.mark = ",", trim = TRUE)
}

create_monitoring_summary_marker <- function(summary_record, source_map_id = NULL) {
  summary_record <- as.data.frame(summary_record, stringsAsFactors = FALSE)
  species_label <- safe_marker_value(summary_record$species_label, summary_record$scientificName_lower)
  count_label <- format_summary_count(summary_record$count)
  observation_ids <- unlist(summary_record$observation_ids[[1]], use.names = FALSE)
  observation_ids <- observation_ids[!is.na(observation_ids) & nzchar(observation_ids)]

  review_link <- ""
  if (length(observation_ids) > 0) {
    payload <- list(
      map_id = source_map_id,
      location_name = safe_marker_value(summary_record$locationName),
      locality = safe_marker_value(summary_record$locality),
      observation_ids = as.character(observation_ids)
    )
    payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
    encoded_payload <- utils::URLencode(payload_json, reserved = TRUE)
    onclick_js <- sprintf(
      "Shiny.setInputValue('density_map_review_sequences_click', JSON.parse(decodeURIComponent('%s')), {priority: 'event'}); return false;",
      encoded_payload
    )
    review_link <- sprintf("<br><a href='#' onclick=\"%s\" title='Review Sequences'>Review sequences</a>", onclick_js)
  }

  popup_content <- sprintf(
    paste0(
      "<div>",
      "Monitoring: <strong>%s</strong><br>",
      "Location: %s<br>",
      "Locality: %s<br>",
      "Observations: %s<br>",
      "Date range: %s to %s",
      "%s",
      "</div>"
    ),
    htmltools::htmlEscape(str_to_title(species_label)),
    htmltools::htmlEscape(safe_marker_value(summary_record$locationName)),
    htmltools::htmlEscape(safe_marker_value(summary_record$locality)),
    count_label,
    htmltools::htmlEscape(as.character(as.Date(summary_record$first_timestamp))),
    htmltools::htmlEscape(as.character(as.Date(summary_record$last_timestamp))),
    review_link
  )

  list(
    marker_type = "monitoring_summary",
    lat = summary_record$marker_latitude,
    lng = summary_record$marker_longitude,
    radius = summary_record$radius,
    color = summary_record$marker_color,
    fillColor = summary_record$marker_color,
    fillOpacity = 0.26,
    weight = 1,
    opacity = 0.55,
    icon = get_observation_species_icon(summary_record$scientificName_lower),
    popup_content = popup_content,
    label = count_label,
    label_class = "observation-summary-count-label",
    label_text_size = "12px",
    label_direction = "center",
    label_offset = c(0, 0),
    group = "Monitoring observations",
    zIndexOffset = dplyr::coalesce(get_icon_importance(summary_record$scientificName_lower), 700)
  )
}

create_no_obs_marker_from_record <- function(obs_record) {
  popup_content <- sprintf(
    "Location: %s (%s)<br>No relevant monitoring observations at this location for the selected date range.",
    obs_record$locationName,
    obs_record$locality
  )

  list(
    marker_type = "icon",
    lat = obs_record$latitude,
    lng = obs_record$longitude,
    icon = get_species_icon("no_obs_deployments"),
    popup_content = popup_content,
    zIndexOffset = 100
  )
}

create_map_markers <- function(data, source_map_id = NULL) {
  all_markers_data <- list()
  all_warnings <- list()

  logger::log_debug("create_map_markers() creating summary markers for selected species")

  observations <- data$observations
  if (!is.null(observations) && nrow(observations) > 0) {
    species_summary <- observations %>%
      dplyr::group_by(.data$scientificName_lower, .data$locationID, .data$locationName) %>%
      dplyr::summarise(
        species_label = dplyr::first(.data[[config$globals$species_name_type]]),
        locality = dplyr::first(.data$locality),
        latitude = dplyr::first(.data$latitude),
        longitude = dplyr::first(.data$longitude),
        count = sum(.data$count, na.rm = TRUE),
        first_timestamp = min(.data$timestamp, na.rm = TRUE),
        last_timestamp = max(.data$timestamp, na.rm = TRUE),
        observation_ids = list(unique(.data$observationID)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        marker_color = observation_marker_color(.data$scientificName_lower)
      )

    max_count <- max(species_summary$count, na.rm = TRUE)
    species_summary <- species_summary %>%
      dplyr::mutate(
        radius = vapply(.data$count, scale_summary_marker_radius, numeric(1), max_value = max_count)
      ) %>%
      apply_summary_offsets(group_cols = c("locationID"))

    for (species in unique(species_summary$scientificName_lower)) {
      species_data <- species_summary %>%
        dplyr::filter(.data$scientificName_lower == species)

      markers <- lapply(seq_len(nrow(species_data)), function(i) {
        create_monitoring_summary_marker(species_data[i, ], source_map_id = source_map_id)
      })

      all_markers_data[[species]] <- list(
        species = species,
        markers = markers,
        warning = NULL
      )
    }
  }

  if (!is.null(data$no_obs_deployments) && nrow(data$no_obs_deployments) > 0) {
    markers <- lapply(seq_len(nrow(data$no_obs_deployments)), function(i) {
      create_no_obs_marker_from_record(data$no_obs_deployments[i, ])
    })

    all_markers_data[["no_obs_deployments"]] <- list(
      species = "no_obs_deployments",
      markers = markers,
      warning = NULL
    )
  }

  logger::log_debug("create_map_markers() created marker groups for %s species", length(all_markers_data))

  all_warnings <- lapply(all_markers_data, function(x) x$warning)
  all_warnings <- Filter(Negate(is.null), all_warnings)

  list(markers = all_markers_data, warnings = all_warnings)
}



extract_trap_tag_value <- function(tags, key) {
  prefix <- paste0(key, ":")

  vapply(tags, function(tag_value) {
    if (is.na(tag_value) || !nzchar(tag_value)) {
      return(NA_character_)
    }

    parts <- strsplit(as.character(tag_value), "\\s*\\|\\s*")[[1]]
    match <- parts[startsWith(parts, prefix)]
    if (length(match) == 0) {
      return(NA_character_)
    }

    trimws(sub(prefix, "", match[[1]], fixed = TRUE))
  }, character(1), USE.NAMES = FALSE)
}

trap_marker_group_one <- function(description) {
  description <- tolower(ifelse(is.na(description), "", description))

  if (grepl("stoat", description)) return("stoat")
  if (grepl("weasel", description)) return("weasel")
  if (grepl("ferret", description)) return("ferret")
  if (grepl("rat", description)) return("rat")
  if (grepl("mouse", description)) return("mouse")
  if (grepl("cat", description)) return("cat")
  if (grepl("rabbit", description)) return("rabbit")
  if (grepl("hedgehog", description)) return("hedgehog")
  if (grepl("possum", description)) return("possum")
  if (grepl("weka", description)) return("weka")
  if (grepl("bird", description)) return("bird")
  if (grepl("mustelid", description)) return("mustelid")

  "other"
}

trap_marker_group <- function(description) {
  vapply(description, trap_marker_group_one, character(1), USE.NAMES = FALSE)
}

trap_marker_label <- function(description) {
  labels <- c(
    stoat = "Stoat",
    weasel = "Weasel",
    ferret = "Ferret",
    rat = "Rat",
    mouse = "Mouse",
    cat = "Cat",
    rabbit = "Rabbit",
    hedgehog = "Hedgehog",
    possum = "Possum",
    weka = "Weka",
    bird = "Bird",
    mustelid = "Mustelid",
    other = "Other"
  )

  group <- trap_marker_group(description)
  unname(labels[group])
}

trap_marker_color <- function(description) {
  colors <- c(
    stoat = "darkred",
    weasel = "red",
    ferret = "orange",
    rat = "black",
    mouse = "gray",
    cat = "purple",
    rabbit = "green",
    hedgehog = "cadetblue",
    possum = "darkblue",
    weka = "darkgreen",
    bird = "blue",
    mustelid = "lightred",
    other = "darkpurple"
  )

  group <- trap_marker_group(description)
  unname(colors[group])
}

trap_icon_group_from_scientific_name <- function(scientific_name) {
  names <- tolower(ifelse(is.na(scientific_name), "", as.character(scientific_name)))
  dplyr::case_when(
    names == "mustela erminea" ~ "stoat",
    names == "mustela nivalis" ~ "weasel",
    names == "mustela putorius furo" ~ "ferret",
    names == "rattus" ~ "rat",
    names == "mus musculus" ~ "mouse",
    names == "felis catus" ~ "cat",
    names == "oryctolagus cuniculus" ~ "rabbit",
    names == "erinaceus europaeus" ~ "hedgehog",
    names == "trichosurus vulpecula" ~ "possum",
    names == "gallirallus australis" ~ "weka",
    names == "aves" ~ "bird",
    names == "mustelidae" ~ "mustelid",
    TRUE ~ "other"
  )
}

trap_marker_group_from_record <- function(scientific_name, description = NULL) {
  scientific_group <- trap_icon_group_from_scientific_name(scientific_name)
  use_description <- is.na(scientific_group) | scientific_group == "other"
  if (!is.null(description) && any(use_description, na.rm = TRUE)) {
    description_group <- trap_marker_group(description)
    scientific_group[use_description] <- description_group[use_description]
  }

  scientific_group
}

trap_kill_icon_path <- function(group) {
  paths <- c(
    stoat = "www/images/icons/map_icons/trap-kill-stoat.svg",
    weasel = "www/images/icons/map_icons/trap-kill-weasel.svg",
    ferret = "www/images/icons/map_icons/trap-kill-ferret.svg",
    rat = "www/images/icons/map_icons/trap-kill-rat.svg",
    mouse = "www/images/icons/map_icons/trap-kill-mouse.svg",
    cat = "www/images/icons/map_icons/trap-kill-cat.svg",
    rabbit = "www/images/icons/map_icons/trap-kill-rabbit.svg",
    hedgehog = "www/images/icons/map_icons/trap-kill-hedgehog.svg",
    possum = "www/images/icons/map_icons/trap-kill-possum.svg",
    weka = "www/images/icons/map_icons/trap-kill.svg",
    bird = "www/images/icons/map_icons/trap-kill-bird.svg",
    mustelid = "www/images/icons/map_icons/trap-kill-mustelid.svg",
    mixed = "www/images/icons/map_icons/trap-kill-mixed.svg",
    other = "www/images/icons/map_icons/trap-kill.svg"
  )

  group <- ifelse(is.na(group) | !group %in% names(paths), "other", group)
  unname(paths[group])
}

get_trap_kill_icon <- function(description = NULL, scientific_name = NULL, multiple_species = FALSE) {
  group <- if (isTRUE(multiple_species)) {
    "mixed"
  } else {
    trap_marker_group_from_record(scientific_name, description)
  }

  makeIcon(
    iconUrl = trap_kill_icon_path(group),
    iconWidth = 34,
    iconHeight = 34,
    iconAnchorX = 17,
    iconAnchorY = 17
  )
}

get_trap_check_icon <- function() {
  makeIcon(
    iconUrl = "www/images/icons/map_icons/trap-check.svg",
    iconWidth = 26,
    iconHeight = 26,
    iconAnchorX = 13,
    iconAnchorY = 13
  )
}

parse_trap_map_date <- function(value) {
  if (inherits(value, "Date")) {
    return(value)
  }
  if (inherits(value, "POSIXt")) {
    return(as.Date(value, tz = timeline_actual_timezone()))
  }

  raw_value <- as.character(value)
  raw_value[!nzchar(raw_value)] <- NA_character_
  date_prefix <- ifelse(
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", raw_value),
    substr(raw_value, 1, 10),
    raw_value
  )
  parsed <- tryCatch(
    suppressWarnings(as.Date(date_prefix)),
    error = function(e) rep(as.Date(NA), length(raw_value))
  )
  needs_posix <- is.na(parsed) & !is.na(raw_value)

  if (any(needs_posix)) {
    posix_value <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", raw_value[needs_posix])
    parsed_posix <- suppressWarnings(as.POSIXct(
      posix_value,
      tz = timeline_actual_timezone(),
      tryFormats = c(
        "%Y-%m-%dT%H:%M:%OS%z",
        "%Y-%m-%dT%H:%M:%S%z",
        "%Y-%m-%d %H:%M:%OS",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d"
      )
    ))
    parsed[needs_posix] <- as.Date(parsed_posix, tz = timeline_actual_timezone())
  }

  parsed
}

normalise_trap_period_intervals <- function(period_intervals) {
  if (is.null(period_intervals) || nrow(period_intervals) == 0) {
    return(NULL)
  }

  start_column <- if ("start_date" %in% names(period_intervals)) "start_date" else "start"
  end_column <- if ("end_date" %in% names(period_intervals)) "end_date" else "end"
  if (!all(c(start_column, end_column) %in% names(period_intervals))) {
    return(NULL)
  }

  intervals <- data.frame(
    start = timeline_date_midnight(period_intervals[[start_column]]),
    end = timeline_date_end(period_intervals[[end_column]]),
    stringsAsFactors = FALSE
  )
  intervals <- intervals[!is.na(intervals$start) & !is.na(intervals$end), , drop = FALSE]
  intervals <- intervals[intervals$start <= intervals$end, , drop = FALSE]

  if (nrow(intervals) == 0) {
    return(NULL)
  }

  intervals
}

trap_intervals_overlap_periods <- function(start_times,
                                           end_times,
                                           period_intervals,
                                           window_start = NULL,
                                           window_end = NULL) {
  periods <- normalise_trap_period_intervals(period_intervals)
  row_count <- length(start_times)
  if (is.null(periods)) {
    return(rep(TRUE, row_count))
  }

  timezone <- timeline_actual_timezone()
  row_start <- as.POSIXct(start_times, tz = timezone)
  row_end <- as.POSIXct(end_times, tz = timezone)

  if (!is.null(window_start) && !is.na(window_start)) {
    window_start <- as.POSIXct(window_start, tz = timezone)
    row_start <- as.POSIXct(
      pmax(as.numeric(row_start), as.numeric(window_start), na.rm = FALSE),
      origin = "1970-01-01",
      tz = timezone
    )
  }

  if (!is.null(window_end) && !is.na(window_end)) {
    window_end <- as.POSIXct(window_end, tz = timezone)
    row_end <- as.POSIXct(
      pmin(as.numeric(row_end), as.numeric(window_end), na.rm = FALSE),
      origin = "1970-01-01",
      tz = timezone
    )
  }

  valid_rows <- !is.na(row_start) & !is.na(row_end) & row_end >= row_start
  overlaps <- rep(FALSE, row_count)
  if (!any(valid_rows)) {
    return(overlaps)
  }

  for (period_index in seq_len(nrow(periods))) {
    overlaps <- overlaps |
      (periods$start[[period_index]] <= row_end & periods$end[[period_index]] >= row_start)
  }

  overlaps & valid_rows
}

prepare_trap_observations_for_map <- function(trap_data_value,
                                              start_date,
                                              end_date,
                                              selected_species,
                                              selected_localities = NULL,
                                              max_locality_distance_km = 1,
                                              include_blank_checks = FALSE,
                                              include_unchecked_locations = FALSE,
                                              period_intervals = NULL) {
  if (is.null(trap_data_value) ||
      is.null(trap_data_value$obs) ||
      is.null(trap_data_value$deps) ||
      nrow(trap_data_value$obs) == 0) {
    return(dplyr::tibble())
  }

  trap_obs <- trap_data_value$obs
  if (!"period" %in% names(trap_obs)) {
    trap_obs$period <- NA_character_
  }
  trap_deps <- trap_data_value$deps
  selected_period_intervals <- normalise_trap_period_intervals(period_intervals)

  if (!all(c("deploymentID", "latitude", "longitude", "locationName") %in% names(trap_deps))) {
    return(dplyr::tibble())
  }

  if (!is.null(selected_localities) &&
      length(selected_localities) > 0 &&
      all(c("locality", "locality_match_type", "locality_distance_km") %in% names(trap_deps))) {
    max_locality_distance_km <- suppressWarnings(as.numeric(max_locality_distance_km))
    if (length(max_locality_distance_km) == 0 || is.na(max_locality_distance_km) || max_locality_distance_km < 0) {
      max_locality_distance_km <- 1
    }

    trap_deps <- trap_deps %>%
      dplyr::filter(
        .data$locality %in% selected_localities,
        .data$locality_match_type == "within" |
          suppressWarnings(as.numeric(.data$locality_distance_km)) <= max_locality_distance_km
      )
  } else if (!is.null(selected_localities) && length(selected_localities) > 0) {
    return(dplyr::tibble())
  }

  if (nrow(trap_deps) == 0) {
    return(dplyr::tibble())
  }

  trap_deployment_fields <- trap_deps %>%
    dplyr::transmute(
      deploymentID,
      locationID,
      locationName = trimws(locationName),
      latitude,
      longitude,
      trap_line = deploymentGroups,
      line = suppressWarnings(as.integer(gsub("[^0-9]+", "", deploymentGroups))),
      locality = if ("locality" %in% names(.)) locality else NA_character_,
      locality_match_type = if ("locality_match_type" %in% names(.)) locality_match_type else NA_character_,
      locality_distance_km = if ("locality_distance_km" %in% names(.)) locality_distance_km else NA_real_,
      nearest_monitoring_locationName = if ("nearest_monitoring_locationName" %in% names(.)) nearest_monitoring_locationName else NA_character_
    )

  eligible_deployment_ids <- unique(as.character(trap_deployment_fields$deploymentID))
  trap_obs <- trap_obs %>%
    dplyr::filter(.data$deploymentID %in% eligible_deployment_ids)

  if (nrow(trap_obs) == 0) {
    return(dplyr::tibble())
  }

  if (!"prior_check_date" %in% names(trap_obs)) {
    trap_obs$prior_check_date <- parse_trap_map_date(trap_obs$eventStart)
  }

  trap_checks_in_window <- trap_obs %>%
    dplyr::mutate(
      check_date = parse_trap_map_date(eventStart),
      prior_check_date = parse_trap_map_date(prior_check_date),
      display_start_time = as.POSIXct(prior_check_date, tz = timeline_actual_timezone()),
      display_end_time = as.POSIXct(check_date + 1, tz = timeline_actual_timezone()) - 1
    ) %>%
    dplyr::filter(
      .data$display_start_time <= as.POSIXct(as.Date(end_date) + 1, tz = timeline_actual_timezone()) - 1,
      .data$display_end_time >= as.POSIXct(as.Date(start_date), tz = timeline_actual_timezone()),
      trap_intervals_overlap_periods(.data$display_start_time, .data$display_end_time, selected_period_intervals)
    ) %>%
    dplyr::left_join(trap_deployment_fields, by = "deploymentID")

  trap_location_metrics <- if (nrow(trap_checks_in_window) > 0) {
    trap_checks_in_window %>%
      dplyr::mutate(
        check_interval_metric = dplyr::coalesce(
          suppressWarnings(as.numeric(.data$check_interval)),
          as.numeric(.data$check_date - .data$prior_check_date)
        ),
        source_kill_flag = dplyr::coalesce(
          .data$observationType == "animal" |
            extract_trap_tag_value(.data$observationTags, "kill") == "1",
          FALSE
        ),
        source_count = dplyr::if_else(
          .data$source_kill_flag,
          dplyr::coalesce(suppressWarnings(as.numeric(.data$count)), 1),
          0
        ),
        selected_species_kill_flag = dplyr::coalesce(
          .data$source_kill_flag &
            !is.na(.data$scientificName) &
            tolower(.data$scientificName) %in% selected_species,
          FALSE
        ),
        selected_species_kill_count_source = dplyr::if_else(
          .data$selected_species_kill_flag,
          .data$source_count,
          0
        )
      ) %>%
      dplyr::group_by(.data$locationID) %>%
      dplyr::summarise(
        trap_checks = dplyr::n_distinct(.data$deploymentID),
        first_check = {
          values <- dplyr::coalesce(.data$prior_check_date, .data$check_date)
          values <- values[!is.na(values)]
          if (length(values) == 0) as.Date(NA) else min(values)
        },
        last_check = max(.data$check_date, na.rm = TRUE),
        mean_check_interval_days = mean(.data$check_interval_metric, na.rm = TRUE),
        median_check_interval_days = stats::median(.data$check_interval_metric, na.rm = TRUE),
        trap_days = {
          values <- .data$check_interval_metric[!duplicated(.data$deploymentID)]
          values <- values[!is.na(values) & is.finite(values) & values > 0]
          if (length(values) == 0) NA_real_ else sum(values)
        },
        any_species_kill_checks = dplyr::n_distinct(.data$deploymentID[.data$source_kill_flag]),
        any_species_kill_count = sum(.data$source_count, na.rm = TRUE),
        selected_species_kill_checks = dplyr::n_distinct(.data$deploymentID[.data$selected_species_kill_flag]),
        selected_species_kill_count = sum(.data$selected_species_kill_count_source, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        check_span_days = pmax(as.integer(.data$last_check - .data$first_check), 0L),
        no_kill_checks = pmax(.data$trap_checks - .data$any_species_kill_checks, 0L),
        no_selected_species_kill_checks = pmax(.data$trap_checks - .data$selected_species_kill_checks, 0L),
        kills_per_check_any_species = dplyr::if_else(
          .data$trap_checks > 0,
          .data$any_species_kill_count / .data$trap_checks,
          NA_real_
        ),
        kills_per_check_selected_species = dplyr::if_else(
          .data$trap_checks > 0,
          .data$selected_species_kill_count / .data$trap_checks,
          NA_real_
        ),
        kills_per_100_trap_days_any_species = dplyr::if_else(
          .data$trap_days > 0,
          100 * .data$any_species_kill_count / .data$trap_days,
          NA_real_
        ),
        kills_per_100_trap_days_selected_species = dplyr::if_else(
          .data$trap_days > 0,
          100 * .data$selected_species_kill_count / .data$trap_days,
          NA_real_
        )
      )
  } else {
    dplyr::tibble(
      locationID = character(),
      trap_checks = integer(),
      first_check = as.Date(character()),
      last_check = as.Date(character()),
      check_span_days = integer(),
      mean_check_interval_days = numeric(),
      median_check_interval_days = numeric(),
      trap_days = numeric(),
      any_species_kill_checks = integer(),
      any_species_kill_count = numeric(),
      selected_species_kill_checks = integer(),
      selected_species_kill_count = numeric(),
      no_kill_checks = integer(),
      no_selected_species_kill_checks = integer(),
      kills_per_check_any_species = numeric(),
      kills_per_check_selected_species = numeric(),
      kills_per_100_trap_days_any_species = numeric(),
      kills_per_100_trap_days_selected_species = numeric()
    )
  }

  checked_location_ids <- if (nrow(trap_checks_in_window) > 0) {
    unique(as.character(trap_checks_in_window$locationID))
  } else {
    character(0)
  }

  normalise_trap_map_row_types <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(data)
    }

    character_columns <- c(
      "observationID", "deploymentID", "mediaID", "eventID", "eventStart", "eventEnd",
      "observation_source", "observationType", "scientificName", "scientificName_lower",
      "vernacularNames.eng", "behavior", "species_class", "period", "day_night_class",
      "diel_class", "trap_marker_type", "trap_kill_type", "source_observationType",
      "source_scientificName", "outcome", "outcome_id", "locationID", "locationName",
      "trap_line", "locality", "locality_match_type", "nearest_monitoring_locationName"
    )

    data %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(character_columns), as.character))
  }

  add_trap_source_fields <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(data)
    }

    data %>%
      dplyr::mutate(
        source_observationType = .data$observationType,
        source_scientificName = .data$scientificName,
        source_count = dplyr::coalesce(suppressWarnings(as.numeric(.data$count)), 1),
        source_kill_flag = dplyr::coalesce(
          .data$observationType == "animal" |
            extract_trap_tag_value(.data$observationTags, "kill") == "1",
          FALSE
        ),
        source_any_species_kill_count = dplyr::if_else(
          .data$source_kill_flag,
          .data$source_count,
          0
        ),
        source_selected_species_kill_flag = dplyr::coalesce(
          .data$source_kill_flag &
            !is.na(.data$scientificName) &
            tolower(.data$scientificName) %in% selected_species,
          FALSE
        ),
        source_selected_species_kill_count = dplyr::if_else(
          .data$source_selected_species_kill_flag,
          .data$source_count,
          0
        )
      )
  }

  selected_kills <- trap_obs %>%
    dplyr::filter(
      .data$observationType == "animal",
      !is.na(.data$scientificName),
      tolower(.data$scientificName) %in% selected_species
    ) %>%
    add_trap_source_fields()

  blank_checks <- dplyr::tibble()
  if (isTRUE(include_blank_checks)) {
    selected_kill_deployments <- unique(as.character(selected_kills$deploymentID))
    blank_checks <- trap_obs %>%
      dplyr::filter(!.data$deploymentID %in% selected_kill_deployments) %>%
      dplyr::group_by(.data$deploymentID) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      add_trap_source_fields() %>%
      dplyr::mutate(
        source_species_label = dplyr::if_else(
          .data$source_kill_flag &
            !is.na(.data$source_scientificName) &
            nzchar(as.character(.data$source_scientificName)),
          as.character(.data$source_scientificName),
          NA_character_
        ),
        observationType = "blank",
        scientificName = .data$source_species_label,
        count = dplyr::if_else(.data$source_kill_flag, .data$source_count, 0),
        behavior = "trap_checked_no_selected_species_kill"
      ) %>%
      dplyr::select(-source_species_label)
  }

  trap_obs <- dplyr::bind_rows(selected_kills, blank_checks)

  spp_classes <- lapply(config$globals$spp_classes, function(x) tolower(unlist(x)))

  trap_obs <- if (nrow(trap_obs) > 0) {
    trap_obs %>%
      dplyr::mutate(
        timestamp = as.POSIXct(parse_trap_map_date(eventStart), tz = timeline_actual_timezone()),
        check_date = parse_trap_map_date(eventStart),
        prior_check_date = parse_trap_map_date(prior_check_date),
        display_start_time = as.POSIXct(prior_check_date, tz = timeline_actual_timezone()),
        display_end_time = as.POSIXct(check_date + 1, tz = timeline_actual_timezone()) - 1,
        observation_source = "trapping",
        outcome = extract_trap_tag_value(observationTags, "outcome"),
        outcome_id = extract_trap_tag_value(observationTags, "outcome_id"),
        trap_marker_type = dplyr::if_else(.data$observationType == "blank", "check", "kill"),
        scientificName_lower = dplyr::if_else(
          .data$trap_marker_type == "check" & !.data$source_kill_flag,
          "trap check - no selected species kill",
          tolower(.data$scientificName)
        ),
        source_kill_label = trap_marker_label(dplyr::coalesce(outcome, .data$scientificName)),
        `vernacularNames.eng` = dplyr::case_when(
          .data$trap_marker_type == "check" & .data$source_kill_flag ~ .data$source_kill_label,
          .data$trap_marker_type == "check" ~
            "No capture",
          TRUE ~ .data$source_kill_label
        ),
        species_class = dplyr::if_else(
          .data$trap_marker_type == "check",
          "Trap check",
          determine_species_class(scientificName_lower, spp_classes)
        ),
        species_rank = ifelse(
          .data$trap_marker_type == "check",
          999,
          create_species_rank(scientificName_lower, spp_classes)
        ),
        possible_duplicate = FALSE,
        period = as.character(.data$period),
        day_night_class = NA_character_,
        diel_class = NA_character_,
        classificationConfidence = NA_real_,
        trap_kill_type = dplyr::case_when(
          .data$trap_marker_type == "check" & .data$source_kill_flag ~
            dplyr::coalesce(outcome, .data$scientificName),
          .data$trap_marker_type == "check" ~
            "No capture",
          TRUE ~ dplyr::coalesce(outcome, .data$scientificName)
        )
      ) %>%
      dplyr::select(-source_kill_label) %>%
      dplyr::filter(
        .data$trap_marker_type %in% c("kill", "check"),
        .data$display_start_time <= as.POSIXct(as.Date(end_date) + 1, tz = timeline_actual_timezone()) - 1,
        .data$display_end_time >= as.POSIXct(as.Date(start_date), tz = timeline_actual_timezone()),
        trap_intervals_overlap_periods(.data$display_start_time, .data$display_end_time, selected_period_intervals)
      ) %>%
      dplyr::left_join(trap_deployment_fields, by = "deploymentID") %>%
      dplyr::left_join(trap_location_metrics, by = "locationID")
  } else {
    dplyr::tibble()
  }

  if (isTRUE(include_unchecked_locations)) {
    unchecked_locations <- trap_deployment_fields %>%
      dplyr::filter(!as.character(.data$locationID) %in% checked_location_ids) %>%
      dplyr::distinct(
        .data$locationID,
        .data$locationName,
        .data$latitude,
        .data$longitude,
        .data$trap_line,
        .data$line,
        .data$locality,
        .data$locality_match_type,
        .data$locality_distance_km,
        .data$nearest_monitoring_locationName
      )

    if (nrow(unchecked_locations) > 0) {
      unchecked_rows <- unchecked_locations %>%
        dplyr::transmute(
          observationID = paste0("trap-unchecked-", .data$locationID),
          deploymentID = NA_character_,
          mediaID = NA_character_,
          eventID = NA_character_,
          eventStart = NA_character_,
          eventEnd = NA_character_,
          timestamp = as.POSIXct(NA_real_, origin = "1970-01-01", tz = timeline_actual_timezone()),
          check_date = as.Date(NA),
          prior_check_date = as.Date(NA),
          check_interval = NA_integer_,
          display_start_time = as.POSIXct(as.Date(start_date), tz = timeline_actual_timezone()),
          display_end_time = as.POSIXct(as.Date(end_date) + 1, tz = timeline_actual_timezone()) - 1,
          observation_source = "trapping",
          observationType = "blank",
          scientificName = "Trap not checked",
          scientificName_lower = "trap not checked",
          `vernacularNames.eng` = "No trap check",
          count = 0,
          behavior = "trap_not_checked",
          species_class = "Trap check",
          species_rank = 1000,
          possible_duplicate = FALSE,
          period = NA_character_,
          day_night_class = NA_character_,
          diel_class = NA_character_,
          classificationConfidence = NA_real_,
          trap_marker_type = "unchecked",
          trap_kill_type = "No trap check",
          source_observationType = "blank",
          source_scientificName = "Trap not checked",
          source_count = 0,
          source_kill_flag = FALSE,
          source_any_species_kill_count = 0,
          source_selected_species_kill_flag = FALSE,
          source_selected_species_kill_count = 0,
          outcome = "No trap check",
          outcome_id = NA_character_,
          trap_checks = 0L,
          first_check = as.Date(start_date),
          last_check = as.Date(end_date),
          check_span_days = pmax(as.integer(as.Date(end_date) - as.Date(start_date)), 0L),
          mean_check_interval_days = NA_real_,
          median_check_interval_days = NA_real_,
          trap_days = 0,
          any_species_kill_checks = 0L,
          any_species_kill_count = 0,
          selected_species_kill_checks = 0L,
          selected_species_kill_count = 0,
          no_kill_checks = 0L,
          no_selected_species_kill_checks = 0L,
          kills_per_check_any_species = NA_real_,
          kills_per_check_selected_species = NA_real_,
          kills_per_100_trap_days_any_species = NA_real_,
          kills_per_100_trap_days_selected_species = NA_real_,
          last_actual_check = as.Date(NA),
          locationID,
          locationName,
          latitude,
          longitude,
          trap_line,
          line,
          locality,
          locality_match_type,
          locality_distance_km,
          nearest_monitoring_locationName
        )

      trap_obs <- dplyr::bind_rows(
        normalise_trap_map_row_types(trap_obs),
        normalise_trap_map_row_types(unchecked_rows)
      )
    }
  }

  normalise_trap_map_row_types(trap_obs)
}

align_trap_observation_types <- function(trap_observations, observation_template) {
  if (is.null(trap_observations) || is.null(observation_template)) {
    return(trap_observations)
  }

  common_columns <- intersect(names(trap_observations), names(observation_template))
  for (column_name in common_columns) {
    template_column <- observation_template[[column_name]]

    if (inherits(template_column, "POSIXt")) {
      trap_observations[[column_name]] <- as.POSIXct(
        trap_observations[[column_name]],
        tz = timeline_actual_timezone()
      )
    } else if (inherits(template_column, "Date")) {
      trap_observations[[column_name]] <- as.Date(trap_observations[[column_name]])
    } else if (is.integer(template_column)) {
      trap_observations[[column_name]] <- suppressWarnings(as.integer(trap_observations[[column_name]]))
    } else if (is.numeric(template_column)) {
      trap_observations[[column_name]] <- suppressWarnings(as.numeric(trap_observations[[column_name]]))
    } else if (is.character(template_column)) {
      trap_observations[[column_name]] <- as.character(trap_observations[[column_name]])
    } else if (is.logical(template_column)) {
      trap_observations[[column_name]] <- as.logical(trap_observations[[column_name]])
    } else if (is.factor(template_column)) {
      trap_observations[[column_name]] <- factor(
        trap_observations[[column_name]],
        levels = levels(template_column)
      )
    }
  }

  trap_observations
}

annotate_unchecked_last_check <- function(trap_observations, trap_data_value, reference_time) {
  if (is.null(trap_observations) || nrow(trap_observations) == 0 ||
      is.null(trap_data_value) || is.null(trap_data_value$obs) || is.null(trap_data_value$deps) ||
      !"trap_marker_type" %in% names(trap_observations)) {
    return(trap_observations)
  }

  if (!"last_actual_check" %in% names(trap_observations)) {
    trap_observations$last_actual_check <- as.Date(NA)
  }

  unchecked_location_ids <- trap_observations %>%
    dplyr::filter(.data$trap_marker_type == "unchecked") %>%
    dplyr::pull(.data$locationID) %>%
    as.character() %>%
    unique()

  if (length(unchecked_location_ids) == 0) {
    return(trap_observations)
  }

  reference_date <- as.Date(reference_time)
  if (is.na(reference_date)) {
    return(trap_observations)
  }

  deployment_lookup <- trap_data_value$deps %>%
    dplyr::select(deploymentID, locationID)

  last_checks <- trap_data_value$obs %>%
    dplyr::mutate(check_date = parse_trap_map_date(.data$eventStart)) %>%
    dplyr::left_join(deployment_lookup, by = "deploymentID") %>%
    dplyr::filter(
      as.character(.data$locationID) %in% unchecked_location_ids,
      !is.na(.data$check_date),
      .data$check_date <= reference_date
    ) %>%
    dplyr::group_by(.data$locationID) %>%
    dplyr::summarise(last_actual_check_lookup = max(.data$check_date, na.rm = TRUE), .groups = "drop")

  if (nrow(last_checks) == 0) {
    return(trap_observations)
  }

  trap_observations %>%
    dplyr::left_join(last_checks, by = "locationID") %>%
    dplyr::mutate(
      last_actual_check = dplyr::coalesce(.data$last_actual_check_lookup, .data$last_actual_check)
    ) %>%
    dplyr::select(-last_actual_check_lookup)
}

format_trap_locality_text <- function(trap_record) {
  if (!("locality" %in% names(trap_record)) ||
      is.na(trap_record$locality) ||
      !nzchar(as.character(trap_record$locality))) {
    return("")
  }

  if ("locality_match_type" %in% names(trap_record) &&
      identical(as.character(trap_record$locality_match_type), "nearest") &&
      "locality_distance_km" %in% names(trap_record) &&
      is.finite(as.numeric(trap_record$locality_distance_km))) {
    nearest_location_text <- if ("nearest_monitoring_locationName" %in% names(trap_record) &&
        !is.na(trap_record$nearest_monitoring_locationName) &&
        nzchar(as.character(trap_record$nearest_monitoring_locationName))) {
      sprintf(" from %s", trap_record$nearest_monitoring_locationName)
    } else {
      ""
    }
    return(sprintf(
      "Locality: %s (nearest, %s km%s)<br>",
      trap_record$locality,
      trap_record$locality_distance_km,
      nearest_location_text
    ))
  }

  sprintf("Locality: %s<br>", trap_record$locality)
}

format_trap_last_actual_check <- function(value) {
  value <- as.Date(value)
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return("Last checked: <strong>not found before this window</strong><br>")
  }

  sprintf("Last checked: <strong>%s</strong><br>", format(value, "%Y-%m-%d"))
}

format_trap_metric_number <- function(value, digits = 0) {
  value <- suppressWarnings(as.numeric(value))
  if (is.na(value) || !is.finite(value)) {
    return("N/A")
  }

  format(round(value, digits), big.mark = ",", nsmall = digits, trim = TRUE)
}

format_trap_metric_rate <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (is.na(value) || !is.finite(value)) {
    return("N/A")
  }

  format(round(value, 2), nsmall = 2, trim = TRUE)
}

trap_capture_records_html <- function(observation_ids) {
  observation_ids <- as.character(unlist(observation_ids, use.names = FALSE))
  observation_ids <- unique(observation_ids[!is.na(observation_ids) & nzchar(observation_ids)])
  if (length(observation_ids) == 0) {
    return("")
  }

  links <- vapply(observation_ids, function(observation_id) {
    safe_id_text <- htmltools::htmlEscape(observation_id)
    safe_id_attr <- htmltools::htmlEscape(observation_id, attribute = TRUE)
    sprintf(
      "<a href='javascript:void(0);' class='trap-observation-link' data-observationid='%s'>%s</a>",
      safe_id_attr,
      safe_id_text
    )
  }, character(1), USE.NAMES = FALSE)

  paste(links, collapse = ", ")
}

create_trap_location_popup_content <- function(trap_record, capture_links_html = "") {
  trap_record <- as.data.frame(trap_record, stringsAsFactors = FALSE)

  record_field <- function(field, default = NA) {
    if (!field %in% names(trap_record)) {
      return(default)
    }

    trap_record[[field]][[1]]
  }

  first_available <- function(fields, default = NA) {
    for (field in fields) {
      value <- record_field(field, default = NA)
      if (!is.null(value) && length(value) > 0 && !is.na(value) && nzchar(as.character(value))) {
        return(value)
      }
    }

    default
  }

  location_text <- htmltools::htmlEscape(safe_marker_value(record_field("locationName")))
  locality_text <- htmltools::htmlEscape(safe_marker_value(record_field("locality")))
  line_value <- first_available(c("trap_line", "line"), "")
  line_text <- if (nzchar(as.character(line_value))) {
    paste0("Trap line ", htmltools::htmlEscape(as.character(line_value)))
  } else {
    ""
  }
  locality_line <- paste(c(locality_text, line_text)[nzchar(c(locality_text, line_text))], collapse = ", ")

  selected_captures <- dplyr::coalesce(
    suppressWarnings(as.numeric(record_field("selected_species_kill_count", NA_real_))),
    suppressWarnings(as.numeric(record_field("kills", NA_real_))),
    suppressWarnings(as.numeric(record_field("count", NA_real_)))
  )

  capture_links_html <- safe_marker_value(capture_links_html)
  capture_records <- if (nzchar(capture_links_html)) {
    paste0("<br><br>Capture records: ", capture_links_html)
  } else {
    ""
  }

  paste0(
    "<div>",
    "<strong>Trap: ", location_text, "</strong>",
    if (nzchar(locality_line)) paste0("<br>", locality_line) else "",
    "<br>Selected-species captures: ", format_trap_metric_number(selected_captures),
    "<br><br>Overlapping checks: ", format_trap_metric_number(record_field("trap_checks", NA_real_)),
    "<br>Coverage: ", htmltools::htmlEscape(format_trap_date_range(
      record_field("first_check", NA),
      record_field("last_check", NA),
      record_field("check_span_days", NA)
    )),
    "<br>Average interval: ", format_trap_metric_number(record_field("mean_check_interval_days", NA_real_), 1), " days",
    "<br>Captures / 100 trap-days: ", format_trap_metric_rate(record_field("kills_per_100_trap_days_selected_species", NA_real_)),
    capture_records,
    "</div>"
  )
}

format_trap_date_range <- function(first_date, last_date, span_days) {
  first_date <- as.Date(first_date)
  last_date <- as.Date(last_date)
  span_days <- suppressWarnings(as.integer(span_days))
  if (is.na(first_date) || is.na(last_date)) {
    return("No checks in this window")
  }
  if (is.na(span_days) || span_days < 0) {
    span_days <- as.integer(last_date - first_date) + 1L
  }

  sprintf(
    "%s to %s (%s day%s)",
    format(first_date, "%Y-%m-%d"),
    format(last_date, "%Y-%m-%d"),
    span_days,
    ifelse(identical(span_days, 1L), "", "s")
  )
}

trap_metrics_popup_html <- function(trap_record, selected_species_label = "selected species") {
  check_count_value <- suppressWarnings(as.numeric(trap_record$trap_checks))
  check_count <- format_trap_metric_number(check_count_value)
  avg_days <- format_trap_metric_number(trap_record$mean_check_interval_days, 1)
  selected_kills <- format_trap_metric_number(trap_record$selected_species_kill_count)
  any_kills <- format_trap_metric_number(trap_record$any_species_kill_count)
  selected_label <- htmltools::htmlEscape(selected_species_label)
  date_range <- htmltools::htmlEscape(format_trap_date_range(
    trap_record$first_check,
    trap_record$last_check,
    trap_record$check_span_days
  ))
  coverage_days <- suppressWarnings(as.numeric(trap_record$check_span_days))
  if (is.na(coverage_days) || !is.finite(coverage_days)) {
    first_check <- as.Date(trap_record$first_check)
    last_check <- as.Date(trap_record$last_check)
    coverage_days <- if (!is.na(first_check) && !is.na(last_check)) {
      pmax(as.numeric(last_check - first_check), 0)
    } else {
      NA_real_
    }
  }
  trap_days <- suppressWarnings(as.numeric(trap_record$trap_days))
  show_trap_day_rates <- !is.na(coverage_days) && is.finite(coverage_days) && coverage_days >= 100 &&
    !is.na(trap_days) && is.finite(trap_days) && trap_days >= 100

  trap_day_rates_html <- if (isTRUE(show_trap_day_rates)) {
    paste0(
      "<div class='trap-popup-metrics'>",
      selected_label, " captures / 100 trap-days: <strong>",
      format_trap_metric_rate(trap_record$kills_per_100_trap_days_selected_species), "</strong><br>",
      "Total captures / 100 trap-days: <strong>",
      format_trap_metric_rate(trap_record$kills_per_100_trap_days_any_species), "</strong>",
      "</div>"
    )
  } else {
    paste0(
      "<div class='trap-popup-metrics'>",
      "Captures / 100 trap-days: <strong>not shown</strong><br>",
      "<small>Requires at least 100 days of trap coverage.</small>",
      "</div>"
    )
  }

  if (is.na(check_count_value) || check_count_value == 0) {
    last_actual_check <- if ("last_actual_check" %in% names(trap_record)) {
      trap_record$last_actual_check
    } else {
      as.Date(NA)
    }

    return(paste0(
      "<p class='trap-popup-summary'>No overlapping trap checks in this map window.<br>",
      "Window: <strong>", date_range, "</strong><br>",
      format_trap_last_actual_check(last_actual_check),
      "</p>"
    ))
  }

  paste0(
    "<p class='trap-popup-summary'>Overlapping checks: <strong>", check_count,
    "</strong><br>Coverage: <strong>", date_range,
    "</strong><br>Average interval: <strong>", avg_days,
    " days</strong><br>", selected_label, " captures: <strong>", selected_kills,
    "</strong><br>Total captures: <strong>", any_kills, "</strong></p>",
    trap_day_rates_html
  )
}


normalise_trap_metric_rows <- function(trap_observations) {
  if (is.null(trap_observations) || nrow(trap_observations) == 0 || !"trap_marker_type" %in% names(trap_observations)) {
    return(dplyr::tibble())
  }

  trap_rows <- trap_observations %>%
    dplyr::filter(.data$trap_marker_type != "unchecked")

  if (nrow(trap_rows) == 0) {
    return(trap_rows)
  }

  if (!"source_kill_flag" %in% names(trap_rows)) {
    trap_rows$source_kill_flag <- dplyr::coalesce(
      trap_rows$observationType == "animal" |
        extract_trap_tag_value(trap_rows$observationTags, "kill") == "1",
      FALSE
    )
  }
  if (!"source_count" %in% names(trap_rows)) {
    trap_rows$source_count <- suppressWarnings(as.numeric(trap_rows$count))
  }
  trap_rows$source_count <- dplyr::coalesce(suppressWarnings(as.numeric(trap_rows$source_count)), 1)

  if (!"source_any_species_kill_count" %in% names(trap_rows)) {
    trap_rows$source_any_species_kill_count <- dplyr::if_else(
      trap_rows$source_kill_flag,
      trap_rows$source_count,
      0
    )
  }
  if (!"source_selected_species_kill_flag" %in% names(trap_rows)) {
    trap_rows$source_selected_species_kill_flag <- dplyr::coalesce(
      trap_rows$trap_marker_type == "kill",
      FALSE
    )
  }
  if (!"source_selected_species_kill_count" %in% names(trap_rows)) {
    trap_rows$source_selected_species_kill_count <- dplyr::if_else(
      trap_rows$source_selected_species_kill_flag,
      trap_rows$source_count,
      0
    )
  }

  trap_rows %>%
    dplyr::mutate(
      check_interval_metric = dplyr::coalesce(
        suppressWarnings(as.numeric(.data$check_interval)),
        as.numeric(.data$check_date - .data$prior_check_date)
      ),
      source_kill_flag = dplyr::coalesce(.data$source_kill_flag, FALSE),
      source_selected_species_kill_flag = dplyr::coalesce(.data$source_selected_species_kill_flag, FALSE),
      source_any_species_kill_count = dplyr::if_else(
        .data$source_kill_flag,
        dplyr::coalesce(suppressWarnings(as.numeric(.data$source_any_species_kill_count)), .data$source_count, 1),
        0
      ),
      source_selected_species_kill_count = dplyr::if_else(
        .data$source_selected_species_kill_flag,
        dplyr::coalesce(suppressWarnings(as.numeric(.data$source_selected_species_kill_count)), .data$source_count, 1),
        0
      )
    )
}

summarise_visible_trap_metrics <- function(trap_observations) {
  trap_rows <- normalise_trap_metric_rows(trap_observations)

  if (nrow(trap_rows) == 0) {
    return(dplyr::tibble())
  }

  trap_rows %>%
    dplyr::group_by(.data$locationID, .data$locationName) %>%
    dplyr::summarise(
      latitude = dplyr::first(.data$latitude),
      longitude = dplyr::first(.data$longitude),
      trap_line = dplyr::first(.data$trap_line),
      locality = dplyr::first(.data$locality),
      locality_match_type = dplyr::first(.data$locality_match_type),
      locality_distance_km = dplyr::first(.data$locality_distance_km),
      nearest_monitoring_locationName = dplyr::first(.data$nearest_monitoring_locationName),
      checks = dplyr::n_distinct(.data$deploymentID[!is.na(.data$deploymentID)]),
      trap_checks = dplyr::n_distinct(.data$deploymentID[!is.na(.data$deploymentID)]),
      first_check = {
        values <- dplyr::coalesce(.data$prior_check_date, .data$check_date)
        values <- values[!is.na(values)]
        if (length(values) == 0) as.Date(NA) else min(values)
      },
      last_check = {
        values <- .data$check_date[!is.na(.data$check_date)]
        if (length(values) == 0) as.Date(NA) else max(values)
      },
      mean_check_interval_days = {
        values <- .data$check_interval_metric[!is.na(.data$check_interval_metric)]
        if (length(values) == 0) NA_real_ else mean(values)
      },
      median_check_interval_days = {
        values <- .data$check_interval_metric[!is.na(.data$check_interval_metric)]
        if (length(values) == 0) NA_real_ else stats::median(values)
      },
      trap_days = {
        check_rows <- !is.na(.data$deploymentID) & !duplicated(.data$deploymentID)
        values <- .data$check_interval_metric[check_rows]
        values <- values[!is.na(values) & is.finite(values) & values > 0]
        if (length(values) == 0) NA_real_ else sum(values)
      },
      any_species_kill_checks = dplyr::n_distinct(.data$deploymentID[.data$source_kill_flag]),
      any_species_kill_count = sum(.data$source_any_species_kill_count, na.rm = TRUE),
      selected_species_kill_checks = dplyr::n_distinct(.data$deploymentID[.data$source_selected_species_kill_flag]),
      selected_species_kill_count = sum(.data$source_selected_species_kill_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      check_span_days = dplyr::if_else(
        !is.na(.data$first_check) & !is.na(.data$last_check),
        pmax(as.integer(.data$last_check - .data$first_check), 0L),
        NA_integer_
      ),
      no_kill_checks = pmax(.data$trap_checks - .data$any_species_kill_checks, 0L),
      no_selected_species_kill_checks = pmax(.data$trap_checks - .data$selected_species_kill_checks, 0L),
      kills_per_check_any_species = dplyr::if_else(
        .data$trap_checks > 0,
        .data$any_species_kill_count / .data$trap_checks,
        NA_real_
      ),
      kills_per_check_selected_species = dplyr::if_else(
        .data$trap_checks > 0,
        .data$selected_species_kill_count / .data$trap_checks,
        NA_real_
      ),
      kills_per_100_trap_days_any_species = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$any_species_kill_count / .data$trap_days,
        NA_real_
      ),
      kills_per_100_trap_days_selected_species = dplyr::if_else(
        .data$trap_days > 0,
        100 * .data$selected_species_kill_count / .data$trap_days,
        NA_real_
      ),
      kills = .data$selected_species_kill_count
    )
}

create_trap_marker_summary <- function(trap_observations) {
  summarise_visible_trap_metrics(trap_observations)
}

create_trap_unchecked_summary <- function(trap_observations) {
  if (is.null(trap_observations) || nrow(trap_observations) == 0 || !"trap_marker_type" %in% names(trap_observations)) {
    return(dplyr::tibble())
  }

  checked_location_ids <- trap_observations %>%
    dplyr::filter(.data$trap_marker_type != "unchecked") %>%
    dplyr::pull(.data$locationID) %>%
    as.character() %>%
    unique()

  unchecked_rows <- trap_observations %>%
    dplyr::filter(
      .data$trap_marker_type == "unchecked",
      !as.character(.data$locationID) %in% checked_location_ids
    )

  if (nrow(unchecked_rows) == 0) {
    return(dplyr::tibble())
  }

  if (!"last_actual_check" %in% names(unchecked_rows)) {
    unchecked_rows$last_actual_check <- as.Date(NA)
  }

  unchecked_rows %>%
    dplyr::group_by(.data$locationID, .data$locationName) %>%
    dplyr::summarise(
      latitude = dplyr::first(.data$latitude),
      longitude = dplyr::first(.data$longitude),
      trap_line = dplyr::first(.data$trap_line),
      locality = dplyr::first(.data$locality),
      locality_match_type = dplyr::first(.data$locality_match_type),
      locality_distance_km = dplyr::first(.data$locality_distance_km),
      nearest_monitoring_locationName = dplyr::first(.data$nearest_monitoring_locationName),
      checks = 0L,
      trap_checks = 0L,
      first_check = dplyr::first(.data$first_check),
      last_check = dplyr::first(.data$last_check),
      check_span_days = dplyr::first(.data$check_span_days),
      mean_check_interval_days = NA_real_,
      median_check_interval_days = NA_real_,
      trap_days = 0,
      any_species_kill_checks = 0L,
      any_species_kill_count = 0,
      selected_species_kill_checks = 0L,
      selected_species_kill_count = 0,
      no_kill_checks = 0L,
      no_selected_species_kill_checks = 0L,
      kills_per_check_any_species = NA_real_,
      kills_per_check_selected_species = NA_real_,
      kills_per_100_trap_days_any_species = NA_real_,
      kills_per_100_trap_days_selected_species = NA_real_,
      kills = 0L,
      last_actual_check = dplyr::first(.data$last_actual_check),
      .groups = "drop"
    )
}

trap_metric_columns <- function() {
  c(
    "trap_checks",
    "first_check",
    "last_check",
    "check_span_days",
    "mean_check_interval_days",
    "median_check_interval_days",
    "trap_days",
    "any_species_kill_checks",
    "any_species_kill_count",
    "selected_species_kill_checks",
    "selected_species_kill_count",
    "no_kill_checks",
    "no_selected_species_kill_checks",
    "kills_per_check_any_species",
    "kills_per_check_selected_species",
    "kills_per_100_trap_days_any_species",
    "kills_per_100_trap_days_selected_species"
  )
}

create_trap_kill_summary <- function(trap_observations) {
  if (is.null(trap_observations) || nrow(trap_observations) == 0 || !"trap_marker_type" %in% names(trap_observations)) {
    return(dplyr::tibble())
  }

  kill_rows <- trap_observations %>%
    dplyr::filter(.data$trap_marker_type == "kill")

  if (nrow(kill_rows) == 0) {
    return(dplyr::tibble())
  }

  metric_columns <- trap_metric_columns()
  metric_summary <- summarise_visible_trap_metrics(trap_observations) %>%
    dplyr::select(locationID, dplyr::all_of(metric_columns))
  location_capture_ids <- kill_rows %>%
    dplyr::group_by(.data$locationID) %>%
    dplyr::summarise(
      capture_observation_ids = list(unique(.data$observationID)),
      .groups = "drop"
    )

  kill_rows %>%
    dplyr::group_by(.data$locationID, .data$locationName, .data$scientificName_lower) %>%
    dplyr::summarise(
      species_label = dplyr::first(.data[[config$globals$species_name_type]]),
      latitude = dplyr::first(.data$latitude),
      longitude = dplyr::first(.data$longitude),
      trap_line = dplyr::first(.data$trap_line),
      locality = dplyr::first(.data$locality),
      locality_match_type = dplyr::first(.data$locality_match_type),
      locality_distance_km = dplyr::first(.data$locality_distance_km),
      nearest_monitoring_locationName = dplyr::first(.data$nearest_monitoring_locationName),
      kills = sum(.data$count, na.rm = TRUE),
      kill_checks = dplyr::n_distinct(.data$deploymentID),
      kill_labels = list(sort(unique(.data$trap_kill_type))),
      observation_ids = list(unique(.data$observationID)),
      .groups = "drop"
    ) %>%
    dplyr::left_join(metric_summary, by = "locationID") %>%
    dplyr::left_join(location_capture_ids, by = "locationID")
}

create_trap_check_effort_marker <- function(trap_record, max_checks) {
  trap_record <- as.data.frame(trap_record, stringsAsFactors = FALSE)
  locality_text <- format_trap_locality_text(trap_record)
  check_label <- format_summary_count(trap_record$checks)
  selected_species_label <- "selected species"

  popup_content <- sprintf(
    paste0(
      "<div>",
      "Trap: <strong>%s</strong><br>",
      "Line: %s<br>",
      "%s",
      "%s",
      "</div>"
    ),
    htmltools::htmlEscape(safe_marker_value(trap_record$locationName)),
    htmltools::htmlEscape(safe_marker_value(trap_record$trap_line)),
    locality_text,
    trap_metrics_popup_html(trap_record, selected_species_label)
  )

  list(
    marker_type = "circle",
    lat = trap_record$latitude,
    lng = trap_record$longitude,
    radius = scale_summary_marker_radius(trap_record$checks, max_checks, range = c(5, 17)),
    color = "#7dd3fc",
    fillColor = "#e0f2fe",
    fillOpacity = 0.18,
    weight = 1,
    opacity = 0.45,
    dashArray = "2,8",
    popup_content = popup_content,
    label = check_label,
    label_class = "trap-check-count-label",
    label_text_size = "10px",
    group = "Trap checks",
    zIndexOffset = 550
  )
}

create_trap_kill_summary_marker <- function(trap_record, max_kills) {
  trap_record <- as.data.frame(trap_record, stringsAsFactors = FALSE)
  species_label <- safe_marker_value(trap_record$species_label, trap_record$scientificName_lower)
  kill_label <- format_summary_count(trap_record$kills)
  capture_ids <- if ("capture_observation_ids" %in% names(trap_record) && is.list(trap_record$capture_observation_ids)) {
    trap_record$capture_observation_ids[[1]]
  } else if ("observation_ids" %in% names(trap_record) && is.list(trap_record$observation_ids)) {
    trap_record$observation_ids[[1]]
  } else {
    character(0)
  }

  marker_color <- observation_marker_color(trap_record$scientificName_lower)
  popup_content <- create_trap_location_popup_content(
    trap_record,
    capture_links_html = trap_capture_records_html(capture_ids)
  )

  list(
    marker_type = "trap_kill_summary",
    lat = trap_record$marker_latitude,
    lng = trap_record$marker_longitude,
    radius = scale_summary_marker_radius(trap_record$kills, max_kills, range = c(8, 22)),
    color = marker_color,
    fillColor = marker_color,
    fillOpacity = 0.20,
    weight = 1,
    opacity = 0.55,
    icon = get_trap_kill_icon(species_label, trap_record$scientificName_lower),
    popup_content = popup_content,
    label = kill_label,
    label_class = "trap-kill-count-label",
    label_text_size = "10px",
    label_direction = "right",
    label_offset = c(13, 0),
    group = "Trap kills",
    zIndexOffset = 1150
  )
}


create_trap_map_markers <- function(trap_observations) {
  if (is.null(trap_observations) || nrow(trap_observations) == 0) {
    return(list())
  }

  markers <- list()

  check_summary <- create_trap_marker_summary(trap_observations)
  if (nrow(check_summary) > 0 && any(check_summary$no_selected_species_kill_checks > 0, na.rm = TRUE)) {
    max_checks <- max(check_summary$checks, na.rm = TRUE)
    check_markers <- lapply(seq_len(nrow(check_summary)), function(i) {
      create_trap_check_effort_marker(check_summary[i, ], max_checks)
    })
    markers <- c(markers, check_markers)
  }

  unchecked_summary <- create_trap_unchecked_summary(trap_observations)
  if (nrow(unchecked_summary) > 0) {
    max_checks <- if (nrow(check_summary) > 0) max(check_summary$checks, na.rm = TRUE) else 1
    unchecked_markers <- lapply(seq_len(nrow(unchecked_summary)), function(i) {
      create_trap_check_effort_marker(unchecked_summary[i, ], max_checks)
    })
    markers <- c(markers, unchecked_markers)
  }

  kill_summary <- create_trap_kill_summary(trap_observations)
  if (nrow(kill_summary) > 0) {
    max_kills <- max(kill_summary$kills, na.rm = TRUE)
    kill_summary <- kill_summary %>%
      dplyr::mutate(marker_color = observation_marker_color(.data$scientificName_lower)) %>%
      apply_summary_offsets(group_cols = c("locationID"), offset_value = config$globals$marker_offset_value * 1.55)

    kill_markers <- lapply(seq_len(nrow(kill_summary)), function(i) {
      create_trap_kill_summary_marker(kill_summary[i, ], max_kills)
    })
    markers <- c(markers, kill_markers)
  }

  markers
}

render_trap_marker_legend <- function(trap_observations, monitoring_observations = NULL) {
  has_trap_observations <- !is.null(trap_observations) && nrow(trap_observations) > 0 && "trap_marker_type" %in% names(trap_observations)
  has_monitoring_observations <- !is.null(monitoring_observations) && nrow(monitoring_observations) > 0
  if (!has_trap_observations && !has_monitoring_observations) {
    return(NULL)
  }

  kill_rows <- if (has_trap_observations) {
    trap_observations %>%
      dplyr::filter(.data$trap_marker_type == "kill")
  } else {
    dplyr::tibble()
  }

  groups <- if (nrow(kill_rows) > 0) {
    sort(unique(observation_marker_group_from_scientific_name(kill_rows$scientificName)))
  } else {
    character(0)
  }

  rows <- c(
    "<div class='trap-marker-legend-row'><span class='map-legend-circle map-legend-monitoring'></span>Monitoring observations</div>"
  )

  if (has_trap_observations && any(trap_observations$trap_marker_type == "check", na.rm = TRUE)) {
    rows <- c(
      rows,
      "<div class='trap-marker-legend-row'><span class='map-legend-circle map-legend-trap-check'></span>Trap checks</div>"
    )
  }

  group_labels <- c(
    stoat = "Stoat trap captures",
    weasel = "Weasel trap captures",
    ferret = "Ferret trap captures",
    rat = "Rat trap captures",
    mouse = "Mouse trap captures",
    cat = "Cat trap captures",
    rabbit = "Rabbit trap captures",
    hedgehog = "Hedgehog trap captures",
    possum = "Possum trap captures",
    weka = "Weka trap captures",
    bird = "Bird trap captures",
    mustelid = "Mustelid trap captures",
    other = "Other trap captures"
  )

  if (length(groups) > 0) {
    rows <- c(rows, vapply(groups, function(group) {
      sprintf(
        "<div class='trap-marker-legend-row'><span class='trap-marker-legend-swatch trap-marker-legend-swatch-%s'></span>%s</div>",
        group,
        htmltools::htmlEscape(if (group %in% names(group_labels)) unname(group_labels[[group]]) else "Other trap captures")
      )
    }, character(1), USE.NAMES = FALSE))
  }

  paste0(
    "<div class='trap-marker-legend'><strong>Map key</strong>",
    paste(rows, collapse = ""),
    "</div>"
  )
}




get_confidence_text <- function(confidence) {
  if (is.na(confidence)) return("full")
  if (confidence == 1) return("full (validated)")
  if (confidence > 0.8) return("high")
  return("uncertain")
}



update_map <- function(all_marker_data_with_warnings,
                       map_id,
                       active_locations,
                       weather_control_html = NULL,
                       period_control_html = NULL,
                       skip_notice_html = NULL,
                       trap_legend_html = NULL) {
  
  proxy <- leafletProxy(map_id) %>% 
    clearMarkers() %>% 
    clearShapes() %>% # Also clear shapes if update_map_area adds them
    clearControls()

  if (!is.null(weather_control_html)) {
    proxy <- proxy %>% addControl(
      html = weather_control_html,
      position = "topright",
      className = "map-weather-control"
    )
  }

  if (!is.null(period_control_html)) {
    proxy <- proxy %>% addControl(
      html = period_control_html,
      position = "topright",
      className = "map-season-control"
    )
  }

  if (!is.null(skip_notice_html)) {
    proxy <- proxy %>% addControl(
      html = skip_notice_html,
      position = "topleft",
      className = "map-playback-skip-control"
    )
  }

  if (!is.null(trap_legend_html)) {
    proxy <- proxy %>% addControl(
      html = trap_legend_html,
      position = "bottomleft",
      className = "trap-marker-legend-control"
    )
  }
  
  all_marker_data <- all_marker_data_with_warnings$markers
  
  # Iterate over all species
  if (length(all_marker_data) > 0) {
    for (species_data_key in names(all_marker_data)) { # Iterate by keys/names
      species_data <- all_marker_data[[species_data_key]]
      species <- species_data$species 
      markers <- species_data$markers  
      
      if (!is.null(markers) && length(markers) > 0) {
        logger::log_debug(sprintf("update_map() for map_id: %s - Adding %d markers for species: %s", 
                                  map_id, length(markers), species))
        for (marker_data in markers) {
          if (identical(marker_data$marker_type, "circle")) {
            proxy %>% addCircleMarkers(
              lng = marker_data$lng,
              lat = marker_data$lat,
              radius = marker_data$radius,
              color = marker_data$color,
              weight = marker_data$weight,
              opacity = marker_data$opacity,
              fillColor = marker_data$fillColor,
              fillOpacity = marker_data$fillOpacity,
              dashArray = if (!is.null(marker_data$dashArray)) marker_data$dashArray else NULL,
              popup = marker_data$popup_content,
              label = htmltools::HTML(marker_data$label),
              labelOptions = labelOptions(
                permanent = TRUE,
                direction = "center",
                textOnly = TRUE,
                opacity = 1,
                textsize = marker_data$label_text_size,
                className = marker_data$label_class
              ),
              options = pathOptions(zIndexOffset = marker_data$zIndexOffset)
            )
          } else if (marker_data$marker_type %in% c("monitoring_summary", "trap_kill_summary")) {
            proxy %>% addCircleMarkers(
              lng = marker_data$lng,
              lat = marker_data$lat,
              radius = marker_data$radius,
              color = marker_data$color,
              weight = marker_data$weight,
              opacity = marker_data$opacity,
              fillColor = marker_data$fillColor,
              fillOpacity = marker_data$fillOpacity,
              popup = marker_data$popup_content,
              options = pathOptions(zIndexOffset = marker_data$zIndexOffset - 5)
            )
            proxy %>% addMarkers(
              lng = marker_data$lng,
              lat = marker_data$lat,
              icon = marker_data$icon,
              popup = marker_data$popup_content,
              label = htmltools::HTML(marker_data$label),
              labelOptions = labelOptions(
                permanent = TRUE,
                direction = if (!is.null(marker_data$label_direction)) marker_data$label_direction else "right",
                offset = if (!is.null(marker_data$label_offset)) marker_data$label_offset else c(12, 0),
                textOnly = TRUE,
                opacity = 1,
                textsize = marker_data$label_text_size,
                className = marker_data$label_class
              ),
              options = markerOptions(zIndexOffset = marker_data$zIndexOffset)
            )
          } else {
            proxy %>% addMarkers(
              lng = marker_data$lng,
              lat = marker_data$lat,
              icon = marker_data$icon,
              popup = marker_data$popup_content,
              options = markerOptions(zIndexOffset = marker_data$zIndexOffset)
            )
          }
        }
      } else {
        logger::log_debug(sprintf("update_map() for map_id: %s - No markers to add for species: %s", map_id, species))
      }
    }
  } else {
    logger::log_debug(sprintf("update_map() for map_id: %s - No species data in all_marker_data.", map_id))
  }
  
  # Return the summary of markers added (from your original server.R logic, adapted)
  output_summary <- list()
  if (length(all_marker_data) > 0) {
    for (species_data_key in names(all_marker_data)) {
      species_data <- all_marker_data[[species_data_key]]
      if (!is.null(species_data$markers) && species_data$species != "no_obs_deployments") {
        output_summary[[species_data$species]] <- length(species_data$markers)
      }
    }
  }
  return(output_summary)
}

clear_map_area <- function(map_id) {
  leafletProxy(map_id) %>%
    clearGroup("Monitoring area")
}

update_map_area <- function(map_id, deployments) {
  area_data <- calculate_area_by_locality(deployments)
  visible_localities <- unique(deployments$locality)
  filtered_area_data <- area_data[area_data$locality %in% visible_localities, ]

  leafletProxy(map_id) %>%
    clearGroup("Monitoring area") %>%
    addPolygons(
      data = filtered_area_data,
      group = "Monitoring area",
      fillColor = "red",
      fillOpacity = 0.1,
      color = "white",
      weight = 1,
      smoothFactor = 0.5,
      popup = ~paste(locality, ": ", round(area_ha, 1), " ha")
    )
}



# Used forzIndexOffset which determines which icons are on top of other icons on a leaflet
get_icon_importance <- function(species) {
  
  importance_level <- list(
    "apteryx mantelli" = 1000,   # Highest priority
    "mustela erminea" = 900,
    "mustela nivalis" = 890,
    "prosthemadera novaeseelandiae" = 810,
    "felis catus" = 800,
    "erinaceus europaeus" = 800,
    "sus scrofa" = 800,
    "rattus" = 700,
    
    "gallirallus australis" = 300
  )
  
  if (species %in% tolower(names(importance_level))) {
    return(importance_level[[tolower(species)]])
  } else {
    return(NULL)
  }
}

# Function to get the icon for a given species
get_species_icon <- function(species) {
  
  # Assuming makeIcon is available in your environment
  # Define a named list of icons
  species_icons <- list(
    "apteryx mantelli" = makeIcon(
      iconUrl = "www/images/icons/map_icons/kiwi.png", # https://www.cleanpng.com/
      iconWidth = 49,
      iconHeight = 29
    ),
    "mustela erminea" = makeIcon(
      iconUrl = "www/images/icons/map_icons/stoat.png",
      iconWidth = 60,
      iconHeight = 23
    ),
    "mustela nivalis" = makeIcon(
      iconUrl = "www/images/icons/map_icons/weasel.png",
      iconWidth = 50,
      iconHeight = 36
    ),
    "felis catus" = makeIcon(
      iconUrl = "www/images/icons/map_icons/cat.png",
      iconWidth = 37,
      iconHeight = 28
    ),
    "Rattus" = makeIcon(
      iconUrl = "www/images/icons/map_icons/rat.png",
      iconWidth = 30,
      iconHeight = 26
    ),
    
    "Gallirallus australis" = makeIcon(
      iconUrl = "www/images/icons/map_icons/weka.png",
      iconWidth = 40,
      iconHeight = 35
    ),
    "Prosthemadera novaeseelandiae" = makeIcon(
      iconUrl = "www/images/icons/map_icons/tui.png",
      iconWidth = 30,
      iconHeight = 27
    ),
    "Erinaceus europaeus" = makeIcon(
      iconUrl = "www/images/icons/map_icons/hedgehog.png",
      iconWidth = 30, 
      iconHeight = 22   
    ),
    "Sus scrofa" = makeIcon(
      iconUrl = "www/images/icons/map_icons/wildboar.png",
      iconWidth = 40,
      iconHeight = 28
    ),
    
    "no_obs_deployments" = makeIcon(
      iconUrl = "www/images/icons/map_icons/cross.png",
      iconWidth = 10, 
      iconHeight = 10
    ),
    
    "none" = makeIcon(
      iconUrl = "www/images/icons/map_icons/cross.png",
      iconWidth = 10, 
      iconHeight = 10
    )
  )
  
  "default_icon" <- makeIcon(
    iconUrl = "www/images/icons/map_icons/other1.png",
    iconWidth = 10,
    iconHeight = 10
  )
  # https://icons8.com/icon/set/popular/ios-glyphs
  
  species_icons <- setNames(species_icons, tolower(names(species_icons)))
  
  if (tolower(species) %in% names(species_icons)) {
    return(species_icons[[tolower(species)]])
  } else {
    # message(sprintf("No icon found in list for %s, using default icon", species))
    return(default_icon)
  }
}
