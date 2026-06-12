#
#' Shiny module UI for selecting a time period and displaying its summary.
#'
#' This function provides the UI elements for the period selection module.
#' It can render either a dropdown select input for choosing a period or
#' a UI output placeholder for displaying a summary of the selected period's dates.
#'
#' @param id A character string. The module's instance ID.
#' @param view A character string. Determines which UI element to render.
#'   Can be "select" for a dropdown input or "summary" for a UI output
#'   placeholder. Defaults to "select".
#' @param choices A list or vector. The options to display in the select input
#'   when `view` is "select". Required if `view` is "select".
#' @param selected A character string or vector. The initially selected value(s)
#'   in the select input when `view` is "select". Defaults to NULL.
#' @param label A character string. The label for the select input when `view`
#'   is "select". Defaults to NULL, in which case no label is shown.
#' @param summary_output_id A character string. The output ID for the UI output
#'   placeholder when `view` is "summary". Defaults to "dates_summary".
#' @return A tagList containing the UI elements for the module.
#' @export

period_selection_module_ui <- function(id, 
                                           view = "select", 
                                           choices = NULL, 
                                           selected = NULL, 
                                           label = NULL, 
                                           multiple = FALSE,
                                           summary_output_id = NULL) {
  
  ns <- NS(id)
  
  if (view == "select") {
    if (is.null(choices)) {
      stop("Choices must be provided when view is 'select'.")
    }
    
    return(
      selectInput(
        ns("period_selection"),
        label = if (!is.null(label)) { tagList(icon("calendar"), label) } else { NULL },
        choices = choices,
        selected = selected,
        multiple = multiple,
        selectize = multiple
      )
    )
  } else if (view == "summary") {
    if (is.null(summary_output_id)) {
      summary_output_id <- "dates_summary"
    }
    return(
      tagList(
        uiOutput(ns(summary_output_id))
      )
    )
  }
}



#' Shiny module server logic for selecting a time period and managing its dates.
#'
#' This function provides the server logic for the period selection module.
#' It manages the start and end dates based on the user's selection from the
#' UI and provides reactive access to these dates and the selected period name.
#' It also renders the summary of the selected dates in the specified output IDs.
#'
#' @param id A character string. The module's instance ID.
#' @param period_groups A list. A named list where names are period names (e.g.,
#'   "Summer 2024") and values are lists containing `start_date` and `end_date`
#'   POSIXct objects.
#' @param summary_output_ids A character vector. The output IDs in the UI where
#'   the formatted date summary should be rendered. Defaults to "dates_summary".
#' @param selected A character string. The initial selected period name.
#' @return A list containing reactive expressions:
#'   \itemize{
#'     \item `start_date`: A reactive value holding the start date of the
#'       selected period (POSIXct).
#'     \item `end_date`: A reactive value holding the end date of the
#'       selected period (POSIXct).
#'     \item `period_name`: A reactive value holding the name of the
#'       selected period (character string).
#'   }
#' @export
#' 
period_selection_module_server <- function(id, 
                                           period_groups, 
                                           summary_output_ids = "dates_summary",
                                           selected = NULL) {
  flat_period_groups <- flatten_period_groups(period_groups)
  
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    module_namespace <- sub("-$", "", ns(""))
    logger::log_debug(sprintf(
      "period_selection_module_server() moduleServer running for %s",
      module_namespace
    ))
    
    start_date <- reactiveVal(NULL)
    end_date <- reactiveVal(NULL)
    period_name <- reactiveVal(NULL)
    period_names <- reactiveVal(NULL)
    period_intervals <- reactiveVal(NULL)

    valid_selected_periods <- function(period_selection) {
      selected_periods <- as.character(period_selection)
      selected_periods <- selected_periods[selected_periods %in% names(flat_period_groups)]

      if (length(selected_periods) == 0) {
        selected_periods <- names(flat_period_groups)[[1]]
      }

      selected_periods
    }

    set_selected_periods <- function(selected_periods) {
      selected_periods <- valid_selected_periods(selected_periods)
      selected_groups <- flat_period_groups[selected_periods]

      start_date(min(do.call(c, lapply(selected_groups, `[[`, "start_date")), na.rm = TRUE))
      end_date(max(do.call(c, lapply(selected_groups, `[[`, "end_date")), na.rm = TRUE))
      period_names(selected_periods)
      period_name(paste(selected_periods, collapse = ", "))
      period_intervals(dplyr::bind_rows(lapply(selected_periods, function(period) {
        data.frame(
          period_name = period,
          start_date = selected_groups[[period]]$start_date,
          end_date = selected_groups[[period]]$end_date,
          period_type = period_group_value(selected_groups[[period]], "period_type"),
          period_family = period_group_value(selected_groups[[period]], "period_family"),
          stringsAsFactors = FALSE
        )
      })))
    }

    initial_period_name <- if (!is.null(selected) && any(selected %in% names(flat_period_groups))) {
      selected[selected %in% names(flat_period_groups)]
    } else {
      names(flat_period_groups)[[1]]
    }
    
    set_selected_periods(initial_period_name)
    
    observeEvent(input$period_selection, {
      logger::log_info(sprintf("period_selection_module_server() period_selection changing for %s, new period is %s",
                               module_namespace, paste(input$period_selection, collapse = ", ")))
      
      if (any(input$period_selection %in% names(flat_period_groups))) {
        set_selected_periods(input$period_selection)
        
        # Trigger Google Analytics event when the selection changes
        runjs(sprintf("gtag('event', 'select', {
          'event_category': %s,
          'event_label': %s
        });", jsonlite::toJSON(id, auto_unbox = TRUE), jsonlite::toJSON(paste(input$period_selection, collapse = ", "), auto_unbox = TRUE)))
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    summary_html_content <- reactive({
      req(!is.null(start_date()) && !is.na(start_date()),
          !is.null(end_date()) && !is.na(end_date()))
      
      format_date <- function(date) {
        formatted_date <- format(date, "%d %b %Y")
        formatted_day <- format(date, "%a")
        paste0("<strong>", formatted_date, " (", formatted_day, ")</strong>")
      }
      
      start_date_value <- as.Date(start_date())
      end_date_value <- as.Date(end_date())
      
      formatted_start_date <- format_date(start_date_value)
      formatted_end_date <- format_date(end_date_value)
      
      dates_table <- data.frame(
        Label = c("Start:", "End:"),
        Date = c(formatted_start_date, formatted_end_date)
      )
      
      kable_table <- knitr::kable(dates_table,
                                  format = "html",
                                  col.names = NULL,
                                  escape = FALSE)
      
      HTML(kable_table)
    })
    
    
    # For each output ID, render the summary content
    for (summary_output_id in summary_output_ids) {
      output[[summary_output_id]] <- renderUI({
        summary_html_content() 
      })
    }
    
    return(list(
      start_date = start_date,
      end_date = end_date,
      period_name = period_name,
      period_names = period_names,
      period_intervals = period_intervals
    ))
  })
}
