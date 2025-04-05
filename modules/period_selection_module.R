#

period_selection_module_ui <- function(id, 
                                           view = "select", 
                                           choices = NULL, 
                                           selected = NULL, 
                                           label = NULL, 
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
        selectize = FALSE
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

# Updated deployment_selection_module_server
period_selection_module_server <- function(id, 
                                           period_groups, 
                                           summary_output_ids = "dates_summary") {
  
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    logger::log_debug(sprintf("period_selection_module_server() moduleServer running for %s", id))
    
    start_date <- reactiveVal(NULL)
    end_date <- reactiveVal(NULL)
    period_name <- reactiveVal(NULL)
    
    start_date(period_groups[[1]]$start_date)
    end_date(period_groups[[1]]$end_date)
    period_name(period_groups[[1]])
    
    observeEvent(input$period_selection, {
      logger::log_info(sprintf("period_selection_module_server() period_selection changing for %s, new period is %s", 
                               id, input$period_selection))
      
      if (input$period_selection %in% names(period_groups)) {
        start_date(period_groups[[input$period_selection]]$start_date)
        end_date(period_groups[[input$period_selection]]$end_date)
        period_name(input$period_selection)
        
        # Trigger Google Analytics event when the selection changes
        runjs(sprintf("gtag('event', 'select', {
          'event_category': '%s', 
          'event_label': '%s'
        });", id, input$period_selection))
      }
    }, ignoreNULL = TRUE)
    
    # For each output ID, render the summary content
    for (summary_output_id in summary_output_ids) {
      output[[summary_output_id]] <- renderUI({
        req(start_date(), end_date())
        
        format_date <- function(date) {
          formatted_date <- format(date, "%d %b %Y")
          formatted_day <- format(date, "%a")
          paste0("<strong>", formatted_date, " (", formatted_day, ")</strong>")
        }
        
        start_date_value <- as.Date(start_date())
        end_date_value <- as.Date(end_date())
        
        formatted_start_date <- format_date(start_date_value)
        formatted_end_date <- format_date(end_date_value)
        
        
        # Create a table for the date display using kable, with no border or extra styling
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
    }
    
    return(list(
      start_date = start_date,
      end_date = end_date,
      period_name = period_name 
    ))
  })
}
