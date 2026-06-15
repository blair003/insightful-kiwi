# server.R
# (c) Copyright 2024 Blair George

# Shiny App web to analyse and report on data in camtrap DP format
# Read about the camptrap DP format at https://github.com/tdwg/camtrap-dp

# PREREQUISITES
# The locationName needs to be in a specific format containing 6 digits, including a space.
# It is transformed into locality, line and location.

# Examples:
# locationName  locality_code   Line  location on the line
# KP 1_3        KP              1     3
# OH 4_2        OH              4     2
# OH 1_3        OH              1     3

# We later convert the locality_code to Location e.g. KP to Kohi Point

source("R/functions/metrics_functions.R")
source("R/functions/overview_functions.R")
source("R/modules/reporting_data_module.R")
source("R/modules/reporting_visualisations_module.R")
source("R/modules/reporting_rendering_module.R")

source("R/functions/period_filter_functions.R")
source("R/functions/report_generation_functions.R")
source("R/functions/pdf_export_functions.R")
source("R/functions/table_presentation_functions.R")
source("R/functions/media_functions.R")
source("R/functions/spatial_functions.R")
source("R/functions/visualisation_functions.R")
source("R/functions/utility_functions.R")
source("R/server/report_download_handler.R")

server <- function(input, output, session) {

  source("R/server/server_observation_handlers.R", local = TRUE)


  logger::log_debug("server.R, starting server() function")

  setBookmarkExclude(c(
    "global_share_btn",
    "share_export_pdf_btn",
    "exclude_pdf_image_carousel",
    "share_view_state",
    "pdf_export_map_request",
    "pdf_export_view_state",
    "observation_click",
    "observationID_click",
    "trap_observation_click",
    "review_nav_click",
    "review_sequences_click",
    "density_map_review_sequences_click",
    "global_setup_btn",
    "reset_button",
    "info_field_clicked",
    "is_fullscreen",
    "window_width",
    "window_height",
    "rawdata_observations_browse_rows_current",
    "rawdata_observations_browse_rows_all",
    "rawdata_observations_browse_rows_selected",
    "rawdata_observations_browse_search",
    "rawdata_observations_browse_cell_clicked",
    "rawdata_deployments_browse_rows_current",
    "rawdata_deployments_browse_rows_all",
    "rawdata_deployments_browse_rows_selected",
    "rawdata_deployments_browse_search",
    "rawdata_deployments_browse_cell_clicked"
  ))

  append_query_params <- function(url, params) {
    params <- params[!vapply(params, is.null, logical(1))]
    params <- params[nzchar(unlist(params, use.names = FALSE))]

    if (length(params) == 0) {
      return(url)
    }

    query_string <- paste(
      sprintf(
        "%s=%s",
        utils::URLencode(names(params), reserved = TRUE),
        utils::URLencode(unlist(params, use.names = FALSE), reserved = TRUE)
      ),
      collapse = "&"
    )
    separator <- if (grepl("\\?", url)) "&" else "?"
    paste0(url, separator, query_string)
  }

  session$userData$use_net_data <- reactiveVal(isTRUE(config$globals$use_net_data))
  global_use_net <- reactive({
    isTRUE(session$userData$use_net_data())
  })

  observeEvent(input$global_setup_btn, {
    showModal(modalDialog(
      title = tagList(icon("gear"), "Settings"),

      tags$h3("Global Data Filter"),
      checkboxInput(
        "global_use_net_data",
        "Exclude possible duplicate observations",
        value = global_use_net()
      ),
      tags$small(HTML(get_description("Possible Duplicate Logic"))),

      tags$hr(),
      tags$h3("Build date"),
      tags$table(
        class = "table table-sm",
        tags$tbody(
          tags$tr(
            tags$th(scope = "row", "Core data"),
            tags$td(format_core_data_build_datetime(core_data$app$core_data_updated, config))
          ),
          tags$tr(
            tags$th(scope = "row", "Core data weather"),
            tags$td(format_core_data_build_datetime(core_data$app$core_data_weather_updated, config))
          ),
          tags$tr(
            tags$th(scope = "row", "Trapping data"),
            tags$td(format_core_data_build_datetime(core_data$app$trapping_data_updated, config))
          ),
          render_overview_data_package_settings_rows(core_data)
        )
      ),

      tags$hr(),
      tags$h5("About InsightfulKiwi"),
      tags$p("InsightfulKiwi provides insights into data collected from wildlife camera monitoring programs using the Camera Trap Data Packages (Camtrap DP) format."),
      tags$p("For more information about InsightfulKiwi, contact blair@aketechnology.co.nz."),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$global_use_net_data, {
    session$userData$use_net_data(isTRUE(input$global_use_net_data))
  }, ignoreInit = TRUE)

  observeEvent(input$global_share_btn, {
    session$sendCustomMessage(type = "closeNavbarMenus", message = list())
    session$sendCustomMessage(type = "collectShareViewState", message = list(delay = 250))
  })

  pdf_export_state <- reactiveVal(NULL)

  observeEvent(input$pdf_export_map_request, {
    request <- input$pdf_export_map_request
    requested_maps <- if (is.list(request) && !is.null(request$maps)) request$maps else list()
    renderers <- session$userData$pdf_export_density_map_renderers
    export_dir <- file.path(config$env$dirs$cache, "exports", "leaflet")

    rendered_maps <- lapply(requested_maps, function(map_request) {
      map_id <- map_request$id

      if (is.null(map_id) || !nzchar(map_id) || is.null(renderers[[map_id]])) {
        return(NULL)
      }

      tryCatch({
        rendered <- renderers[[map_id]](
          width = map_request$width,
          height = map_request$height,
          export_dir = export_dir
        )

        list(
          id = map_id,
          src = rendered$src,
          width = map_request$width,
          height = map_request$height
        )
      }, error = function(error) {
        logger::log_warn(sprintf("PDF export failed to render Leaflet map %s: %s", map_id, conditionMessage(error)))
        NULL
      })
    })
    rendered_maps <- Filter(Negate(is.null), rendered_maps)

    session$sendCustomMessage(
      type = "pdfExportMapImages",
      message = list(
        nonce = request$nonce,
        maps = rendered_maps
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$pdf_export_view_state, {
    pdf_state <- input$pdf_export_view_state
    pdf_export_state(pdf_state)

    if (!is.list(pdf_state) || is.null(pdf_state$screenshot_data_url) || !nzchar(pdf_state$screenshot_data_url)) {
      showModal(modalDialog(
        title = "PDF export failed",
        tags$p("The browser could not capture the current page. Try again after the page has finished rendering."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    showModal(modalDialog(
      title = "Export this view to PDF",
      tags$p("The export uses the current page state and captures rendered plots and maps as an image before creating the PDF."),
      tags$div(
        class = "d-grid gap-2",
        downloadButton("export_current_view_pdf", "Download PDF", class = "btn-primary")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  output$export_current_view_pdf <- downloadHandler(
    filename = function() {
      state <- pdf_export_state()
      nav <- if (is.list(state) && !is.null(state$nav)) state$nav else input$nav
      tab <- if (is.list(state) && !is.null(state$tab)) state$tab else NULL
      filename_parts <- c(nav, tab, format(Sys.time(), "%Y%m%d-%H%M%S"))
      paste0(sanitize_pdf_export_filename(paste(filename_parts, collapse = "-")), ".pdf")
    },
    content = function(file) {
      state <- pdf_export_state()

      export_view_image_to_pdf(
        screenshot_data_url = if (is.list(state)) state$screenshot_data_url else NULL,
        pdf_file = file,
        export_dir = file.path(config$env$dirs$cache, "exports")
      )
    }
  )

  observeEvent(input$share_view_state, {
    share_state <- input$share_view_state

    url <- if (is.list(share_state) && !is.null(share_state$base_url) && nzchar(share_state$base_url)) {
      share_state$base_url
    } else {
      paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, session$clientData$url_pathname)
    }

    rawdata_obs_search <- input$rawdata_observations_browse_search
    rawdata_deps_search <- input$rawdata_deployments_browse_search
    nav <- input$nav
    tab <- NULL

    if (!is.null(nav) && nzchar(nav)) {
      tab <- switch(nav,
        overview = input[["overview-main_overview_tabs"]],
        reporting = input$reporting_tabs,
        density_map = input[["density_map_comparison-density_comparison_tabs"]],
        monitoring_trapping_map = input[["monitoring_trapping_map_comparison-density_comparison_tabs"]],
        density_timeline_map = input[["density_timeline_map-density_timeline_tabs"]],
        activity_patterns = input[["activity_patterns-activity_patterns_tabs"]],
        records = input$records_tabs,
        NULL
      )

      if (startsWith(nav, "species_overview_")) {
        tab <- input[[paste0(nav, "-overview_tabs")]]
      }
    }

    if (is.list(share_state)) {
      if (is.null(rawdata_obs_search) || !nzchar(rawdata_obs_search)) {
        rawdata_obs_search <- share_state$rawdata_observations_search
      }
      if (is.null(rawdata_deps_search) || !nzchar(rawdata_deps_search)) {
        rawdata_deps_search <- share_state$rawdata_deployments_search
      }
    }

    page_state <- if (is.list(share_state) && !is.null(share_state$page_state)) {
      jsonlite::toJSON(share_state$page_state, auto_unbox = TRUE, null = "null")
    } else {
      NULL
    }

    url <- append_query_params(url, list(
      nav = nav,
      tab = tab,
      state = page_state,
      rawdata_obs_search = rawdata_obs_search,
      rawdata_deps_search = rawdata_deps_search
    ))

    showModal(modalDialog(
      title = "Share this view",
      tags$p("Copy the link below to share this page view:"),
      tags$div(
        class = "input-group",
        tags$input(type = "text", class = "form-control", value = url, id = "share_url_input", readonly = "readonly"),
        tags$button(
          class = "btn btn-outline-secondary",
          type = "button",
          onclick = "copyToClipboard(document.getElementById('share_url_input').value, this)",
          icon("clipboard")
        )
      ),
      tags$hr(),
      tags$p("Export this page view:"),
      tags$div(
        class = "d-grid gap-2",
        actionButton("share_export_pdf_btn", "Export PDF", icon = icon("file-pdf"), class = "btn-primary")
      ),
      checkboxInput(
        "exclude_pdf_image_carousel",
        "Exclude image carousel from PDF",
        value = FALSE
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$share_export_pdf_btn, {
    exclude_image_carousel <- isTRUE(input$exclude_pdf_image_carousel)
    removeModal()
    session$onFlushed(function() {
      session$sendCustomMessage(
        type = "collectPdfExportViewState",
        message = list(exclude_image_carousel = exclude_image_carousel)
      )
    }, once = TRUE)
  }, ignoreInit = TRUE)

  primary_period <- period_selection_module_server(
    id = "primary_period",
    period_groups = core_data$period_groups,
    summary_output_ids = "summary_output_reporting",
    selected = core_data$app$period_defaults$primary_period
  )

  density_map_period <- period_selection_module_server(
    id = "density_map_period",
    period_groups = core_data$period_groups,
    summary_output_ids = character(0),
    selected = core_data$app$period_defaults$primary_period
  )

  comparative_period <- period_selection_module_server(
    id = "comparative_period", 
    period_groups = core_data$period_groups,
    summary_output_ids = character(0),
    selected = core_data$app$period_defaults$comparative_period
  )

  monitoring_trapping_map_period <- period_selection_module_server(
    id = "monitoring_trapping_map_period",
    period_groups = core_data$period_groups,
    summary_output_ids = character(0),
    selected = core_data$app$period_defaults$primary_period
  )

  # Reactive filtering of deps and obs for primary/reporting and map periods
  filtered_deps_primary <- reactive({
    filter_deps(core_data$deps, primary_period$start_date(), primary_period$end_date())
  })
  
  filtered_obs_primary <- reactive({
    filter_detection_obs(filter_obs(core_data$obs, primary_period$start_date(), primary_period$end_date()))
  })

  filtered_deps_density_map <- reactive({
    filter_deps_by_period_names(
      core_data$deps,
      density_map_period$period_names(),
      density_map_period$start_date(),
      density_map_period$end_date(),
      density_map_period$period_intervals()
    )
  })

  filtered_obs_density_map <- reactive({
    filter_detection_obs(filter_obs_by_period_names(
      core_data$obs,
      density_map_period$period_names(),
      density_map_period$start_date(),
      density_map_period$end_date(),
      density_map_period$period_intervals()
    ))
  })
  
  filtered_deps_comparative <- reactive({
    filter_deps_by_period_names(
      core_data$deps,
      comparative_period$period_names(),
      comparative_period$start_date(),
      comparative_period$end_date(),
      comparative_period$period_intervals()
    )
  })
  
  filtered_obs_comparative <- reactive({
    filter_detection_obs(filter_obs_by_period_names(
      core_data$obs,
      comparative_period$period_names(),
      comparative_period$start_date(),
      comparative_period$end_date(),
      comparative_period$period_intervals()
    ))
  })

  filtered_deps_monitoring_trapping_map <- reactive({
    filter_deps_by_period_names(
      core_data$deps,
      monitoring_trapping_map_period$period_names(),
      monitoring_trapping_map_period$start_date(),
      monitoring_trapping_map_period$end_date(),
      monitoring_trapping_map_period$period_intervals()
    )
  })

  filtered_obs_monitoring_trapping_map <- reactive({
    filter_detection_obs(filter_obs_by_period_names(
      core_data$obs,
      monitoring_trapping_map_period$period_names(),
      monitoring_trapping_map_period$start_date(),
      monitoring_trapping_map_period$end_date(),
      monitoring_trapping_map_period$period_intervals()
    ))
  })

  overview_state <- overview_module_server("overview", core_data = core_data, config = config, use_net = global_use_net)

  overview_plot_periods <- period_names_from_index(
    core_data$period_groups,
    period_index = core_data$app$period_defaults$primary_period_index,
    period_name = core_data$app$period_defaults$primary_period
  )
  overview_plot_deps <- filter_deps_by_period_names(
    core_data$deps,
    overview_plot_periods,
    NULL,
    NULL,
    period_intervals_for_names(core_data$period_groups, overview_plot_periods)
  )

  plotting_module_server(
    id = "spp_obs_plot_visualisations",
    type = NULL,
    obs = filter_detection_obs(core_data$obs),
    deps = overview_plot_deps,
    species_override = NULL
  )
  # Initialize species overviews dynamically (Lazy Loading)
  loaded_species_overviews <- reactiveVal(character())
  pending_species_rai_detail <- reactiveVal(NULL)

  observeEvent(input$nav, {
    nav_item <- input$nav

    if (startsWith(nav_item, "species_overview_")) {
      sci_name_make_names <- sub("^species_overview_", "", nav_item)

      # Check if already loaded
      if (!(sci_name_make_names %in% loaded_species_overviews())) {

        # Find the full scientific and vernacular names
        found <- FALSE
        for (group_name in names(core_data$app$spp_classes)) {
          species_in_group <- core_data$app$spp_classes[[group_name]]
          for (s_name in names(species_in_group)) {
            s_sci_name <- species_in_group[[s_name]]
            if (make.names(s_sci_name) == sci_name_make_names) {

              # Initialize module
              species_overview_module_server(
                id = paste0("species_overview_", make.names(s_sci_name)),
                species_name = s_sci_name,
                vernacular_name = s_name,
                obs = filtered_obs_primary,
                deps = filtered_deps_primary,
                core_data = core_data,
                rai_norm_hours = config$globals$rai_norm_hours,
                use_net = global_use_net,
                trap_data = trap_data,
                initial_rai_detail = pending_species_rai_detail()
              )
              pending_species_rai_detail(NULL)

              # Add to loaded list
              loaded_species_overviews(c(loaded_species_overviews(), sci_name_make_names))
              logger::log_debug(sprintf("server.R, lazily loaded species overview for %s", s_sci_name))

              found <- TRUE
              break
            }
          }
          if (found) break
        }
      }
    }
  })
  
  
  
  setup_table_output(output, 
                     table_id = "rawdata_deployments_browse", 
                     data = core_data$deps,
                     table_type = "paged",
                     table_order = list(list(4, 'asc')),
                     render_when = function() {
                       identical(input$nav, "records") &&
                         identical(input$records_tabs, "monitoring") &&
                         identical(input$monitoring_records_tabs, "deps")
                     },
                     cache_prepared_data = TRUE) 
  
  
  setup_table_output(output, 
                     table_id = "rawdata_observations_browse", 
                     data = prepare_raw_observations_browse_data(core_data$obs),
                     table_type = "paged",
                     table_order = list(list(4, 'asc')),
                     render_when = function() {
                       identical(input$nav, "records") &&
                         identical(input$records_tabs, "monitoring") &&
                         identical(input$monitoring_records_tabs, "obs")
                     },
                     cache_prepared_data = TRUE) 

  trap_table_options <- function(order = list(list(0, "desc"))) {
    list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50, 100),
      scrollX = TRUE,
      order = order,
      dom = "lfrtip"
    )
  }

  output$trapdata_observations_browse <- DT::renderDataTable({
    req(!is.null(trap_data))

    trap_locations <- trap_data$deps %>%
      dplyr::select(
        deploymentID,
        trap_code = locationName,
        trap_line = deploymentGroups,
        dplyr::any_of(c(
          "locality",
          "nearest_monitoring_locationName"
        )),
        latitude,
        longitude
      )

    trap_obs <- trap_data$obs %>%
      dplyr::left_join(trap_locations, by = "deploymentID") %>%
      dplyr::select(
        prior_check_date,
        check_date,
        check_interval,
        dplyr::any_of(c(
          "source_prior_check_date",
          "source_interval_days",
          "prior_check_override_applied"
        )),
        trap_code,
        dplyr::any_of(c(
          "locality",
          "nearest_monitoring_locationName"
        )),
        trap_line,
        observationType,
        scientificName,
        count,
        behavior,
        sex,
        lifeStage,
        classifiedBy,
        observationTags,
        observationComments,
        latitude,
        longitude,
        observationID,
        deploymentID
      )

    DT::datatable(
      trap_obs,
      rownames = FALSE,
      filter = "top",
      options = trap_table_options()
    )
  })

  output$trapdata_deployments_browse <- DT::renderDataTable({
    req(!is.null(trap_data))

    trap_deps <- trap_data$deps %>%
      dplyr::mutate(
        interval_days = if ("interval_days" %in% names(.)) {
          interval_days
        } else {
          as.integer(as.Date(deploymentEnd) - as.Date(deploymentStart))
        }
      ) %>%
      dplyr::select(
        check_date,
        trap_code = locationName,
        dplyr::any_of(c(
          "locality",
          "locality_match_type",
          "locality_distance_km",
          "nearest_monitoring_locationName"
        )),
        trap_line = deploymentGroups,
        deploymentStart,
        deploymentEnd,
        interval_days,
        latitude,
        longitude,
        deploymentTags,
        deploymentID
      )

    DT::datatable(
      trap_deps,
      rownames = FALSE,
      filter = "top",
      options = trap_table_options()
    )
  })

  output$trapdata_summary_browse <- DT::renderDataTable({
    req(!is.null(trap_data))

    if (!is.null(trap_data$trap_summary)) {
      trap_summary <- trap_data$trap_summary
    } else {
      trap_summary <- data.frame(
        message = "Trap summary is not available for this import.",
        stringsAsFactors = FALSE
      )
    }

    DT::datatable(
      trap_summary,
      rownames = FALSE,
      filter = "top",
      options = trap_table_options(order = list(list(0, "asc")))
    )
  })

  output$trapdata_conversion_summary <- DT::renderDataTable({
    req(!is.null(trap_data))

    summary_values <- trap_data$summary
    trap_summary <- data.frame(
      metric = names(summary_values),
      value = vapply(summary_values, function(value) {
        paste(value, collapse = ", ")
      }, character(1)),
      stringsAsFactors = FALSE
    )

    DT::datatable(
      trap_summary,
      rownames = FALSE,
      options = trap_table_options(order = list(list(0, "asc")))
    )
  })

  restore_rawdata_search <- function(query) {
    session$onFlushed(function() {
      session$sendCustomMessage(
        type = "restoreRawdataSearch",
        message = list(
          rawdata_observations_search = if (!is.null(query$rawdata_obs_search)) query$rawdata_obs_search else "",
          rawdata_deployments_search = if (!is.null(query$rawdata_deps_search)) query$rawdata_deps_search else ""
        )
      )
    }, once = TRUE)
  }

  clean_share_query_params <- function(query) {
    share_query_params <- c(
      "nav",
      "tab",
      "state",
      "rawdata_obs_search",
      "rawdata_deps_search",
      "records_tabs",
      "observation_id",
      "view_mode",
      "rai_detail",
      "species_rai_detail",
      "export_mode"
    )

    if (length(intersect(names(query), share_query_params)) == 0) {
      return()
    }

    session$onFlushed(function() {
      session$sendCustomMessage(
        type = "cleanShareQueryParams",
        message = list(params = share_query_params)
      )
    }, once = TRUE)
  }
  
  
    
    # Render UI for the overview plot
    output$overview_spp_obs_plot <- renderUI({
#      if (isTRUE(input$is_fullscreen)) {
#        # Full-screen content: 2-tab layout with data tables and plot
#        navset_card_tab(
#          nav_panel("Tab 1", plotOutput("spp_obs_plot")), # Plot goes here
#          nav_panel("Tab 2", DT::dataTableOutput("obs_data_table")) # Placeholder for datatable
#        )
#      } else {
#        # Normal content: Just the plot
        plotOutput("spp_obs_plot")
#      }
    })
    
    # Optionally render a datatable for full-screen view
    output$obs_data_table <- DT::renderDataTable({
      DT::datatable(filtered_data())
    })

    # Observe changes in input$nav and toggle sidebar accordingly
    # Not on mobile in portrait mode
    observe({
      nav_value <- input$nav
      logger::log_debug(sprintf("server.R, observer triggered, input$nav is %s", nav_value))
      
      # Debugging: Log the current value of input$nav to the console
      runjs(sprintf("console.log(%s);", jsonlite::toJSON(paste0("Nav changed to: ", nav_value), auto_unbox = TRUE)))
      
      # JavaScript code to handle sidebar toggling. Only applies on non-mobile
      # devices or likely mobile devices in portrait mode (768)
      nav_json <- jsonlite::toJSON(nav_value, auto_unbox = TRUE)
      runjs(sprintf("toggleSidebar(%s, defaultSidebarState);", nav_json))
    })

  # Listens for info_field_clicked returning from JS customInfoButtonClickHandler in ui.R head tags
  # Generates an information modal with information relevant to the field clicked
  observeEvent(input$info_field_clicked, {

    field_name <- input$info_field_clicked
    column_description <- get_column_description(field_name)
    
    if (is.null(column_description)) {
      # Check for the specific edge case where field_name includes "RAI ± SE"
      if (grepl("RAI ± SE", field_name)) {
        # If the edge case is matched, modify field_name accordingly
        field_name <- "RAI ± SE"
      } else if (grepl("Count", field_name)) {
        # If the edge case is matched, modify field_name accordingly
        field_name <- "Count"
      }
      
      column_description <- get_column_description(field_name)
    }
    
    if (!is.null(column_description)) {
      # Generate and display the modal with the field information
      generate_info_modal(field_name, column_description)
    } else {
      # Handle cases where field_name is not found in column_descriptions
      logger::log_warn(paste("Clicked field not recognised:", field_name))
    }
    
    runjs(sprintf("gtag('event', 'click', {'event_category': 'info_button', 'event_label': %s});", jsonlite::toJSON(field_name, auto_unbox = TRUE)))
    
  })
 

  


  current_reporting <- reporting_data_module_server("current_reporting", filtered_obs_primary, filtered_deps_primary)
  current_visualisations <- reporting_visualisations_module_server("current_visualisations", filtered_obs_primary)
  reporting_rendering_module_server("current_tables", current_reporting$reporting_data)
  
  # Convert ggplot object to plotly object
  
  # Optionally, you can customize the tooltip content
  # interactive_plot <- interactive_plot %>% layout(tooltip = c("x", "y", "fill"))
  
  output$locality_ggplot_daily_species_count <- renderPlot({
    current_visualisations$visualisations()$locality_ggplot_daily_species_count
    
  })
  
  output$locality_ggplotly_daily_species_count <- renderPlotly({
    ggplotly(current_visualisations$visualisations()$locality_ggplot_daily_species_count)
    
  })
  
  output$reporting_executive_summary <- renderUI({
    reporting_rendering_module_ui("current_tables", "reporting_executive_summary")
  })
  
  output$reporting_species_summary <- renderUI({
    reporting_rendering_module_ui("current_tables", "reporting_species_summary")
  })
  
  output$reporting_results_summary <- renderUI({
    reporting_rendering_module_ui("current_tables", "reporting_results_summary")
  })
  
#  output$reporting_camera_network_overview <- renderUI({
#    reporting_rendering_module_ui("current_tables", "reporting_camera_network_overview")
#  })
#  
#  output$reporting_records_browse <- renderUI({
#    reporting_rendering_module_ui("current_tables", "reporting_records_browse")
#  })
  

  
  # Updates visibility reactiveVal's based on which which menu is showing
  # Covers standard map features and autoplay features
  observeEvent(input$nav, {
    nav_item <- input$nav

    runjs(sprintf("gtag('config', %s, {'page_path': %s});",
                  jsonlite::toJSON(config$globals$ga_tag, auto_unbox = TRUE),
                  jsonlite::toJSON(paste0("/", nav_item), auto_unbox = TRUE)))

    
    if (nav_item  == "overview") {
      # Run latest images
      #message("observeEvent(input$nav, viewing overview page -- triggered image update")
      # latest_images(get_latest_images())
    }

    
    if (nav_item  == "reporting") {
      #print("Reporting menu")
      # Generate reporting data for the current period

      
      
      # Generate reporting datatables, all the action happens in reporting_rendering_module_ui, server returns nothing
      # Uses prepared_data for creation of observations and deployments browse, maybe I should do in reporting_data_module_server
      #reporting_rendering_module_server("current_tables", current$reporting_data, prepared_data)
      
      output$season_selection_text <- renderUI({
        package_date_text <- generate_package_date_text(core_data$created)
        season_selection_text <- generate_season_selection_text(
          primary_period$start_date(),
          primary_period$end_date(),
          primary_period$period_name()
        )
        
        combined_text <- paste(package_date_text, "<br><br>", season_selection_text)
        HTML(combined_text)
      })
    }


  })

  
  
  observeEvent(input$observation_click, {
    # Parse the input
   # browser()
    parts <- strsplit(input$observation_click, "\\|")[[1]]
    
    # Extract components
    observation_id <- parts[1]   # Extract observationID
    sequence_id <- parts[2]      # Extract sequenceID
    action_type <- paste(parts[3:(length(parts) - 1)], collapse = "|")  # Handle multi-part action_type
    timestamp <- parts[length(parts)]  # Extract timestamp
    
    # Remove any leading or trailing | from action_type
    action_type <- gsub("^\\|+|\\|+$", "", action_type)
    
    # Further split action_type to determine main action and mode
    action_parts <- strsplit(action_type, "\\|")[[1]]
    main_action <- action_parts[1]  # Extract main action (e.g., "view_sequence")
    view_mode <- if (length(action_parts) > 1) action_parts[2] else "modal"  # Default to modal if missing

    # Backwards compatibility: if main_action is "modal", it likely meant view_sequence in modal mode
    if (main_action == "modal") {
      main_action <- "view_sequence"
      view_mode <- "modal"
    }
    
    # Validate that at least one ID is present and valid
    if ((!nchar(observation_id) > 0 || !is_valid_UUID(observation_id)) &&
        (!nchar(sequence_id) > 0 || !is_valid_UUID(sequence_id))) {
      stop("Invalid input: either observation_id or sequence_id must be provided.")
    }
    
    # Handle view_sequence (requires observation_id)
    if (main_action == "view_sequence") {
      handle_view_sequence(observation_id, view_mode)
    } 
    # Handle edit_sequence (requires sequence_id)
    else if (main_action == "edit_sequence") {
     # handle_edit_sequence(sequence_id)
    } else {
      stop("Unknown action_type.")
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$observationID_click, {
    parts <- strsplit(input$observationID_click, "\\|")[[1]]
    observation_id <- parts[1]
    action_type <- parts[2] 

    if (nchar(observation_id) > 0) {
      if (is_valid_UUID(observation_id)) {

        observation_details <- create_observation_viewer_output(observation_id, action_type)
        
        if (!is.null(observation_details)) {

          if (action_type == "modal") {
            image_output <- create_observation_images_ui(observation_details$sequence_media_info, 
                                                         observation_id,
                                                         context = "modal", review_nav = list(current_index = current_index, total_sequences = total_sequences))
            
            show_image_modal(observation_id, image_output$ui_elements)
            session$sendCustomMessage(type = "refreshCarousel", message = list(carouselId = image_output$carousel_id))
          } else if (action_type == "pageview") {
            image_output <- create_observation_images_ui(observation_details$sequence_media_info, 
                                                         observation_id,
                                                         context = "pageview")
            
            output$observation_images_pageview <- renderUI({
              image_output$ui_elements
            })
            
            updateNavbarPage(session, "main_menu", selected = "observations")
            # Navigate to the Viewer tab if not already there
            updateTabsetPanel(session, "observations_tabsetpanel", selected = "Viewer")
 
          }
          
          # If there was no cache hit, update the image cache so we can display from cache next time. The UIx is too 
          # slow to download the images then always display from cache. Better to have the progressive loading of the 
          # jpeg and display the version from Agouti if we don't have them
          if (!image_output$cache_hit) {
            update_image_cache(observation_details$sequence_media_info)
          }
          # Reset so that future clicks to same link will work
          shinyjs::click("reset_button")
        }
      }
    }

  })
  
  
  
  observeEvent(session$clientData$url_search, {
    # Use URL parameters on initial load

    query <- parseQueryString(session$clientData$url_search)
    query_log <- if (length(query) == 0) {
      "<empty>"
    } else {
      paste(sprintf("%s=%s", names(query), unlist(query, use.names = FALSE)), collapse = "&")
    }
    logger::log_debug("server.R, query string is %s", query_log)

    if (!is.null(query$species_rai_detail) && nzchar(query$species_rai_detail)) {
      pending_species_rai_detail(query$species_rai_detail)
    }

    if (!is.null(query$nav) && nzchar(query$nav)) {
      updateNavbarPage(session, "nav", selected = query$nav)
    }

    query_tab <- if (!is.null(query$tab) && nzchar(query$tab)) {
      query$tab
    } else if (!is.null(query$records_tabs) && nzchar(query$records_tabs)) {
      query$records_tabs
    } else {
      NULL
    }

    if (!is.null(query_tab) && !is.null(query$nav) && nzchar(query$nav)) {
      tabset_id <- switch(query$nav,
        overview = "overview-main_overview_tabs",
        reporting = "reporting_tabs",
        density_map = "density_map_comparison-density_comparison_tabs",
        monitoring_trapping_map = "monitoring_trapping_map_comparison-density_comparison_tabs",
        density_timeline_map = "density_timeline_map-density_timeline_tabs",
        activity_patterns = "activity_patterns-activity_patterns_tabs",
        records = "records_tabs",
        NULL
      )

      if (startsWith(query$nav, "species_overview_")) {
        tabset_id <- paste0(query$nav, "-overview_tabs")
      }

      if (!is.null(tabset_id)) {
        updateTabsetPanel(session, tabset_id, selected = query_tab)
      }
    }

    restore_rawdata_search(query)

    if (!is.null(query$state) && nzchar(query$state)) {
      session$onFlushed(function() {
        session$sendCustomMessage(type = "restorePageState", message = list(state = query$state))
      }, once = TRUE)
    }

    session$onFlushed(function() {
      session$sendCustomMessage(type = "closeNavbarMenus", message = list())
    }, once = TRUE)

    clean_share_query_params(query)

    if (!is.null(query$rai_detail) && nzchar(query$rai_detail)) {
      overview_state$show_rai_detail_modal(query$rai_detail)
    }

    if (!is.null(query$observation_id)) {
      # Extracted observation ID from URL
      observation_id <- query$observation_id
      view_mode <- if (!is.null(query$view_mode) && query$view_mode == "modal") "modal" else "pageview"
      
      if (nchar(observation_id) > 0) {
        if (is_valid_UUID(observation_id)) {
          observation_details <- create_observation_viewer_output(observation_id, paste0("view_sequence|", view_mode))
          
          if (!is.null(observation_details)) {
            # Set the value in the text input (optional)
            updateTextInput(session, "observationID_input", value = observation_id)
            
            image_output <- create_observation_images_ui(observation_details$sequence_media_info, 
                                                         observation_id,
                                                         context = view_mode)
            
            if (view_mode == "modal") {
              show_image_modal(observation_id, image_output$ui_elements)
              session$sendCustomMessage(type = "refreshCarousel", message = list(carouselId = image_output$carousel_id))
            } else {
              output$observation_images_pageview <- renderUI({
                image_output$ui_elements
              })

              updateNavbarPage(session, "main_menu", selected = "observations")
              # Navigate to the Viewer tab if not already there
              updateTabsetPanel(session, "observations_tabsetpanel", selected = "Viewer")
            }
            
            # Check and download for next time
            if (!image_output$cache_hit) {
              update_image_cache(observation_details$sequence_media_info)
            }
          }
        } else {
          output$observation_images_pageview_error <- renderUI({ "ObservationID not found" })
        }
      }
    }
  }, ignoreInit = FALSE, once = TRUE)
  
  
  # This listens to the actionButton on the Observation viewer page
  observeEvent(input$view_observation, {
    # This always implies a page view action since it's triggered from within the Viewer tab
    observation_id <- trimws(input$observationID_input)
    output$observation_images_pageview_error <- renderUI({ NULL })
    
    if (nchar(observation_id) > 0) {
      if (is_valid_UUID(observation_id)) {
        observation_details <- create_observation_viewer_output(observation_id, "pageview")
    
        if (!is.null(observation_details)) {
          image_output <- create_observation_images_ui(observation_details$sequence_media_info, 
                                                       observation_id,
                                                       context = "pageview")
          
          output$observation_images_pageview <- renderUI({
            image_output$ui_elements
          })
          # Check and download for next time
          if (!image_output$cache_hit) {
            update_image_cache(observation_details$sequence_media_info)
          }
        }
      } else {
        output$observation_images_pageview_error <- renderUI({ "ObservationID not found" })
      }
    }
  })
  
  
  
  observeEvent(input$reset_button, {
    # This is just to trigger a change in input values; no action needed here but leave it
    # Related to shinyjs::click("reset_button")
  }, ignoreNULL = FALSE)
  
  
  
  # Initialize a reactive value to store the latest images
  latest_images <- reactiveVal()
  
  # Initially populate latest_images
  latest_images(get_latest_images())
  
  output$imageSlider <- renderUI({
    
    image_paths <- latest_images() 
    img_tags <- lapply(image_paths, function(path) {
      tags$img(src = path, style = "width: 100%;") # Ensure images fill their container
    })
    
    tagList(
      div(id = "slickSlider", class = "slider", img_tags),
      tags$script(HTML("initImageSlider('slickSlider');"))
    )
  })
  

  

  
  ########### DENSITY MAP FEATURE ###########
  
  density_map_primary <- NULL
  density_map_comparative <- NULL
  density_map_loaded <- reactiveVal(FALSE)
  
  # Used for the comparison panel headings
  output$primary_season_name <- renderText({
    density_map_period$period_name()
  })
  
  output$comparative_season_name <- renderText({
    comparative_period$period_name()
  })

  selected_localities_heading <- function(localities) {
    if (is.null(localities) || length(localities) == 0) {
      localities <- unique(core_data$deps$locality)
    }

    paste(vapply(as.character(localities), locality_display_name, character(1)), collapse = ", ")
  }

  selected_species_heading <- function(species) {
    species_choices <- unlist(unname(core_data$app$spp_classes), use.names = TRUE)

    if (is.null(species) || length(species) == 0) {
      species <- unname(species_choices)
    }

    species <- as.character(species)
    species_labels <- names(species_choices)[match(tolower(species), tolower(unname(species_choices)))]
    missing_labels <- is.na(species_labels) | !nzchar(species_labels)
    species_labels[missing_labels] <- species[missing_labels]

    paste(species_labels, collapse = ", ")
  }

  selected_map_heading <- function(species, localities) {
    paste(selected_species_heading(species), "at", selected_localities_heading(localities))
  }

  density_map_period_readout <- function(period) {
    req(period$start_date(), period$end_date())

    timezone <- if (exists("timeline_actual_timezone", mode = "function", inherits = TRUE)) {
      timeline_actual_timezone()
    } else if (!is.null(config$globals$actual_timezone) && nzchar(config$globals$actual_timezone)) {
      config$globals$actual_timezone
    } else {
      "Pacific/Auckland"
    }

    start_time <- as.POSIXct(
      paste(format(as.Date(period$start_date()), "%Y-%m-%d"), "00:00:00"),
      tz = timezone
    )
    end_time <- as.POSIXct(
      paste(format(as.Date(period$end_date()), "%Y-%m-%d"), "23:59:59"),
      tz = timezone
    )

    div(
      class = "timeline-window-readout",
      strong("Timeframe:"),
      paste(
        format(start_time, "%Y-%m-%d", tz = timezone),
        "to",
        format(end_time, "%Y-%m-%d", tz = timezone)
      )
    )
  }

  output$primary_density_map_period_readout <- renderUI({
    density_map_period_readout(density_map_period)
  })

  output$comparative_density_map_period_readout <- renderUI({
    density_map_period_readout(comparative_period)
  })




  density_trap_distance <- reactive({
    value <- suppressWarnings(as.numeric(input[["density_map_primary-trap_locality_distance_km"]]))
    if (is.na(value) || value < 0) {
      return(5)
    }
    value
  })

  density_show_trap_check_counters <- reactive({
    isTRUE(input[["density_map_primary-show_trap_blank_checks"]])
  })

  density_show_unchecked_traps <- reactive({
    isTRUE(input[["density_map_primary-show_trap_unchecked_locations"]])
  })

  density_data_source <- reactive({
    include_monitoring <- if (is.null(input[["density_map_primary-show_density_location_markers"]]) &&
                              is.null(input[["density_map_primary-show_predicted_rai_surface"]])) {
      TRUE
    } else {
      isTRUE(input[["density_map_primary-show_density_location_markers"]]) ||
        isTRUE(input[["density_map_primary-show_predicted_rai_surface"]])
    }
    include_trapping <- !is.null(trap_data) && (
      isTRUE(input[["density_map_primary-show_trap_kill_markers"]]) ||
        isTRUE(input[["density_map_primary-show_trap_unchecked_locations"]])
    )
    if (include_monitoring && include_trapping) return("both")
    if (include_trapping) return("trapping")
    if (include_monitoring) return("monitoring")
    "none"
  })

  density_max_location_count <- function(observations, species, localities) {
    if (is.null(observations) || nrow(observations) == 0 ||
        is.null(species) || length(species) == 0 ||
        is.null(localities) || length(localities) == 0) {
      return(0)
    }

    species <- tolower(unname(as.character(species)))
    counts <- observations %>%
      dplyr::filter(
        .data$scientificName_lower %in% species,
        .data$locality %in% localities
      )

    if (isTRUE(input[["density_map_primary-exclude_possible_duplicates"]]) &&
        "possible_duplicate" %in% names(counts)) {
      counts <- counts %>%
        dplyr::filter(is.na(.data$possible_duplicate) | !.data$possible_duplicate)
    }

    counts <- counts %>%
      dplyr::group_by(.data$locationID) %>%
      dplyr::summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(.data$count)

    if (length(counts) == 0 || all(is.na(counts))) {
      return(0)
    }

    max(counts, na.rm = TRUE)
  }

  density_max_trap_kills <- function(period, species, localities) {
    if (is.null(trap_data) || is.null(period) ||
        is.null(species) || length(species) == 0 ||
        is.null(localities) || length(localities) == 0) {
      return(0)
    }

    trap_rows <- prepare_trap_observations_for_map(
      trap_data,
      period$start_date(),
      period$end_date(),
      tolower(unname(as.character(species))),
      localities,
      density_trap_distance(),
      include_blank_checks = density_show_trap_check_counters(),
      include_unchecked_locations = density_show_unchecked_traps(),
      period_intervals = period$period_intervals()
    )

    kill_summary <- create_trap_kill_summary(trap_rows)
    if (nrow(kill_summary) == 0) {
      return(0)
    }

    counts <- kill_summary %>%
      dplyr::group_by(.data$locationID) %>%
      dplyr::summarise(count = sum(.data$kills, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(.data$count)

    if (length(counts) == 0 || all(is.na(counts))) {
      return(0)
    }

    max(counts, na.rm = TRUE)
  }

  density_comparison_scale_max <- reactive({
    species <- input[["density_map_primary-selected_species"]]
    localities <- input[["density_map_primary-selected_localities"]]
    data_source <- density_data_source()
    max_values <- c()
    if (data_source %in% c("monitoring", "both")) {
      max_values <- c(
        max_values,
        density_max_location_count(filtered_obs_density_map(), species, localities),
        density_max_location_count(filtered_obs_comparative(), species, localities)
      )
    }
    if (data_source %in% c("trapping", "both")) {
      max_values <- c(
        max_values,
        density_max_trap_kills(density_map_period, species, localities),
        density_max_trap_kills(comparative_period, species, localities)
      )
    }
    if (length(max_values) == 0) {
      return(0)
    }

    max_value <- max(max_values, na.rm = TRUE)
    if (is.na(max_value) || !is.finite(max_value)) {
      return(0)
    }
    max_value
  })

  output$density_map_selection_heading <- renderUI({
    species <- input[["density_map_primary-selected_species"]]
    localities <- input[["density_map_primary-selected_localities"]]
    div(class = "overview-locality-heading map-selection-heading", selected_map_heading(species, localities))
  })

  output$density_timeline_map_selection_heading <- renderUI({
    species <- input[["density_timeline_map-selected_species"]]
    localities <- input[["density_timeline_map-selected_localities"]]
    div(class = "overview-locality-heading map-selection-heading", selected_map_heading(species, localities))
  })
  
  monitoring_trapping_map_monitoring <- NULL
  monitoring_trapping_map_trapping <- NULL
  monitoring_trapping_map_loaded <- reactiveVal(FALSE)

  monitoring_trapping_map_source_monitoring <- reactive("monitoring")
  monitoring_trapping_map_source_trapping <- reactive("trapping")
  monitoring_trapping_map_show_true <- reactive(TRUE)
  monitoring_trapping_map_combined_species <- reactive("combined")

  monitoring_trapping_map_trap_distance <- reactive({
    value <- suppressWarnings(as.numeric(input[["monitoring_trapping_map_monitoring-trap_locality_distance_km"]]))
    if (is.na(value) || value < 0) {
      return(2)
    }
    value
  })

  monitoring_trapping_map_show_trap_check_counters <- reactive({
    isTRUE(input[["monitoring_trapping_map_monitoring-show_trap_blank_checks"]])
  })

  monitoring_trapping_map_show_unchecked_traps <- reactive({
    isTRUE(input[["monitoring_trapping_map_monitoring-show_trap_unchecked_locations"]])
  })

  monitoring_trapping_map_max_location_count <- function(observations, species, localities) {
    if (is.null(observations) || nrow(observations) == 0 ||
        is.null(species) || length(species) == 0 ||
        is.null(localities) || length(localities) == 0) {
      return(0)
    }

    species <- tolower(unname(as.character(species)))
    counts <- observations %>%
      dplyr::filter(
        .data$scientificName_lower %in% species,
        .data$locality %in% localities
      )

    if (isTRUE(input[["monitoring_trapping_map_monitoring-exclude_possible_duplicates"]]) &&
        "possible_duplicate" %in% names(counts)) {
      counts <- counts %>%
        dplyr::filter(is.na(.data$possible_duplicate) | !.data$possible_duplicate)
    }

    counts <- counts %>%
      dplyr::group_by(.data$locationID) %>%
      dplyr::summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(.data$count)

    if (length(counts) == 0 || all(is.na(counts))) {
      return(0)
    }

    max(counts, na.rm = TRUE)
  }

  monitoring_trapping_map_max_trap_kills <- function(species, localities) {
    if (is.null(trap_data) ||
        is.null(species) || length(species) == 0 ||
        is.null(localities) || length(localities) == 0) {
      return(0)
    }

    trap_rows <- prepare_trap_observations_for_map(
      trap_data,
      monitoring_trapping_map_period$start_date(),
      monitoring_trapping_map_period$end_date(),
      tolower(unname(as.character(species))),
      localities,
      monitoring_trapping_map_trap_distance(),
      include_blank_checks = FALSE,
      include_unchecked_locations = FALSE,
      period_intervals = monitoring_trapping_map_period$period_intervals()
    )

    kill_summary <- create_trap_kill_summary(trap_rows)
    if (nrow(kill_summary) == 0) {
      return(0)
    }

    counts <- kill_summary %>%
      dplyr::group_by(.data$locationID) %>%
      dplyr::summarise(count = sum(.data$kills, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(.data$count)

    if (length(counts) == 0 || all(is.na(counts))) {
      return(0)
    }

    max(counts, na.rm = TRUE)
  }

  monitoring_trapping_map_scale_max <- reactive({
    species <- input[["monitoring_trapping_map_monitoring-selected_species"]]
    localities <- input[["monitoring_trapping_map_monitoring-selected_localities"]]
    max_value <- max(
      monitoring_trapping_map_max_location_count(filtered_obs_monitoring_trapping_map(), species, localities),
      monitoring_trapping_map_max_trap_kills(species, localities),
      na.rm = TRUE
    )

    if (is.na(max_value) || !is.finite(max_value)) {
      return(0)
    }
    max_value
  })

  output$monitoring_trapping_map_selection_heading <- renderUI({
    species <- input[["monitoring_trapping_map_monitoring-selected_species"]]
    localities <- input[["monitoring_trapping_map_monitoring-selected_localities"]]
    div(class = "overview-locality-heading map-selection-heading", selected_map_heading(species, localities))
  })

  output$monitoring_trapping_map_monitoring_period_readout <- renderUI({
    density_map_period_readout(monitoring_trapping_map_period)
  })

  output$monitoring_trapping_map_trapping_period_readout <- renderUI({
    density_map_period_readout(monitoring_trapping_map_period)
  })

  observeEvent(input$nav, {
    req(input$nav == "density_map")

    if (!density_map_loaded()) {
      logger::log_debug("server.R, calling mapping_module_server() for density_map_primary")
      density_map_primary <<- mapping_module_server(
        id = "density_map_primary",
        obs = filtered_obs_density_map,
        deps = filtered_deps_density_map,
        period_names = density_map_period$period_names,
        period_start_date = density_map_period$start_date,
        period_end_date = density_map_period$end_date,
        period_intervals = density_map_period$period_intervals,
        use_net = global_use_net,
        trap_data = trap_data,
        density_scale_max_override = density_comparison_scale_max
      )

      logger::log_debug("server.R, calling mapping_module_server() for density_map_comparative")
      density_map_comparative <<- mapping_module_server(
        id = "density_map_comparative",
        obs = filtered_obs_comparative,
        deps = filtered_deps_comparative,
        period_names = comparative_period$period_names,
        period_start_date = comparative_period$start_date,
        period_end_date = comparative_period$end_date,
        period_intervals = comparative_period$period_intervals,
        species_override = density_map_primary$selected_species,
        localities_override = density_map_primary$selected_localities,
        prediction_surface_override = density_map_primary$show_predicted_rai_surface,
        prediction_surface_basis_override = density_map_primary$predicted_rai_surface_basis,
        species_display_mode_override = density_map_primary$species_display_mode,
        location_markers_override = density_map_primary$show_density_location_markers,
        density_data_source_override = density_data_source,
        trap_distance_override = density_trap_distance,
        trap_kill_markers_override = density_map_primary$show_trap_kill_markers,
        trap_check_counters_override = density_show_trap_check_counters,
        unchecked_traps_override = density_show_unchecked_traps,
        use_net = global_use_net,
        trap_data = trap_data,
        density_scale_max_override = density_comparison_scale_max
      )

      density_map_loaded(TRUE)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  observeEvent(input$nav, {
    req(input$nav == "monitoring_trapping_map")

    if (!monitoring_trapping_map_loaded()) {
      logger::log_debug("server.R, calling mapping_module_server() for monitoring_trapping_map_monitoring")
      monitoring_trapping_map_monitoring <<- mapping_module_server(
        id = "monitoring_trapping_map_monitoring",
        obs = filtered_obs_monitoring_trapping_map,
        deps = filtered_deps_monitoring_trapping_map,
        period_names = monitoring_trapping_map_period$period_names,
        period_start_date = monitoring_trapping_map_period$start_date,
        period_end_date = monitoring_trapping_map_period$end_date,
        period_intervals = monitoring_trapping_map_period$period_intervals,
        density_data_source_override = monitoring_trapping_map_source_monitoring,
        prediction_surface_override = reactive(FALSE),
        species_display_mode_override = monitoring_trapping_map_combined_species,
        location_markers_override = monitoring_trapping_map_show_true,
        use_net = global_use_net,
        trap_data = trap_data,
        density_scale_max_override = monitoring_trapping_map_scale_max
      )

      logger::log_debug("server.R, calling mapping_module_server() for monitoring_trapping_map_trapping")
      monitoring_trapping_map_trapping <<- mapping_module_server(
        id = "monitoring_trapping_map_trapping",
        obs = filtered_obs_monitoring_trapping_map,
        deps = filtered_deps_monitoring_trapping_map,
        period_names = monitoring_trapping_map_period$period_names,
        period_start_date = monitoring_trapping_map_period$start_date,
        period_end_date = monitoring_trapping_map_period$end_date,
        period_intervals = monitoring_trapping_map_period$period_intervals,
        species_override = monitoring_trapping_map_monitoring$selected_species,
        localities_override = monitoring_trapping_map_monitoring$selected_localities,
        prediction_surface_override = reactive(FALSE),
        species_display_mode_override = monitoring_trapping_map_combined_species,
        location_markers_override = monitoring_trapping_map_show_true,
        density_data_source_override = monitoring_trapping_map_source_trapping,
        trap_distance_override = monitoring_trapping_map_trap_distance,
        trap_kill_markers_override = monitoring_trapping_map_show_true,
        trap_check_counters_override = monitoring_trapping_map_show_trap_check_counters,
        unchecked_traps_override = monitoring_trapping_map_show_unchecked_traps,
        use_net = global_use_net,
        trap_data = trap_data,
        density_scale_max_override = monitoring_trapping_map_scale_max
      )

      monitoring_trapping_map_loaded(TRUE)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  
  
  activity_patterns_module_server(
    id = "activity_patterns",
    core_data = core_data,
    nav = reactive({
      if (identical(input$nav, "reporting") && identical(input$reporting_tabs, "activity_patterns")) {
        return("activity_patterns")
      }

      input$nav
    }),
    current_period = overview_state$current_period,
    prior_period = overview_state$prior_period,
    last_year_period = overview_state$last_year_period,
    use_net = global_use_net
  )

  ########### DENSITY TIMELINE MAP FEATURE ###########

  timeline_period <- period_selection_module_server(
    id = "timeline_period",
    period_groups = core_data$period_groups,
    selected = core_data$app$period_defaults$primary_period
  )

  filtered_deps_timeline_map <- reactive({
    filter_deps_by_period_names(
      core_data$deps,
      timeline_period$period_names(),
      timeline_period$start_date(),
      timeline_period$end_date(),
      timeline_period$period_intervals()
    )
  })

  filtered_obs_timeline_map <- reactive({
    filter_detection_obs(filter_obs_by_period_names(
      core_data$obs,
      timeline_period$period_names(),
      timeline_period$start_date(),
      timeline_period$end_date(),
      timeline_period$period_intervals()
    ))
  })

  density_timeline_map_loaded <- reactiveVal(FALSE)

  observeEvent(input$nav, {
    if (input$nav == "density_timeline_map" && !density_timeline_map_loaded()) {
      logger::log_debug("server.R, lazily calling mapping_module_server() for density_timeline_map")
      mapping_module_server(
        id = "density_timeline_map",
        obs = filtered_obs_timeline_map,
        deps = filtered_deps_timeline_map,
        period_names = timeline_period$period_names,
        period_start_date = timeline_period$start_date,
        period_end_date = timeline_period$end_date,
        period_intervals = timeline_period$period_intervals,
        timeline_mode = "always",
        use_net = global_use_net,
        trap_data = trap_data
      )
      density_timeline_map_loaded(TRUE)
    }
  })

  ########### MONITORING & TRAPPING FEATURE ###########

  monitoring_trapping_loaded <- reactiveVal(FALSE)

  observeEvent(input$nav, {
    if (input$nav == "monitoring_trapping" && !monitoring_trapping_loaded()) {
      logger::log_debug("server.R, lazily calling trapping_outcomes_module_server() for Monitoring & Trapping")
      trapping_outcomes_module_server(
        id = "monitoring_trapping",
        core_data = core_data,
        trap_data = trap_data,
        config = config,
        use_net = global_use_net
      )
      monitoring_trapping_loaded(TRUE)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  monitoring_trapping_analysis_loaded <- reactiveVal(FALSE)

  observeEvent(input$nav, {
    if (input$nav == "monitoring_trapping_analysis" && !monitoring_trapping_analysis_loaded()) {
      logger::log_debug("server.R, lazily calling monitoring_trapping_module_server() for Trapping Analysis")
      monitoring_trapping_module_server(
        id = "monitoring_trapping_analysis",
        core_data = core_data,
        trap_data = trap_data,
        config = config,
        use_net = global_use_net
      )
      monitoring_trapping_analysis_loaded(TRUE)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)


  

  register_report_download_handler(
    input = input,
    output = output,
    primary_period = primary_period,
    current_reporting = current_reporting,
    filtered_deps_primary = filtered_deps_primary,
    filtered_obs_primary = filtered_obs_primary,
    core_data = core_data,
    config = config,
    use_net = global_use_net
  )
  
  

  

  # Observe review sequences click from overview
  observeEvent(input$review_sequences_click, {
    action_data <- input$review_sequences_click

    # Extract details
    period_name <- action_data$period_name
    rai_group <- action_data$rai_group
    species_name <- action_data$species_name
    locality_filter <- action_data$locality
    supplied_observation_ids <- action_data$observation_ids

    if (!is.null(supplied_observation_ids) && length(supplied_observation_ids) > 0) {
      observation_ids <- unlist(supplied_observation_ids, use.names = FALSE)
      observation_ids <- observation_ids[nzchar(observation_ids)]

      if (length(observation_ids) > 0) {
        show_review_sequences_modal(observation_ids, 1)
      } else {
        showModal(modalDialog(
          title = "No Sequences Found",
          "There are no sequences to review for this selection.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }

      return()
    }

    if (!is.null(period_name) && period_name == "ALL") {
      filtered_obs <- filter_detection_obs(core_data$obs)
    } else if (!is.null(period_name) && period_name %in% names(flatten_period_groups(core_data$period_groups))) {
      period <- period_group_by_name(core_data$period_groups, period_name)

      # Filter obs
      filtered_obs <- filter_detection_obs(filter_obs(core_data$obs, period$start_date, period$end_date))
    } else {
      filtered_obs <- NULL
    }

    if (!is.null(filtered_obs)) {

      # Apply locality filter
      if (!is.null(locality_filter) && locality_filter != "ALL") {
        localities <- strsplit(locality_filter, ",")[[1]]
        filtered_obs <- filtered_obs %>% filter(locality %in% localities)
      }

      # Apply species filter based on either a species overview click or an RAI group click.
      if (!is.null(species_name) && nzchar(species_name)) {
        filtered_obs <- filtered_obs %>% filter(tolower(scientificName) == tolower(species_name))
      } else {
        species_list <- config$globals$rai_groups[[rai_group]]
        if (!is.null(species_list)) {
          filtered_obs <- filtered_obs %>% filter(tolower(scientificName) %in% tolower(species_list))
        }
      }

      filtered_obs <- filter_possible_duplicates_for_use_net(filtered_obs, global_use_net())

      # Extract observation IDs
      observation_ids <- filtered_obs$observationID

      if (length(observation_ids) > 0) {
        show_review_sequences_modal(observation_ids, 1)
      } else {
        showModal(modalDialog(
          title = "No Sequences Found",
          "There are no sequences to review for this selection.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    }
  })


  observeEvent(input$review_nav_click, {
    nav_click <- input$review_nav_click
    if (is.list(nav_click)) {
      new_index <- nav_click$index
      initial_slide <- if (!is.null(nav_click$initial_slide)) nav_click$initial_slide else 0
    } else {
      new_index <- nav_click
      initial_slide <- 0
    }
    state <- session$userData$review_sequences_state

    if (!is.null(state)) {
      show_review_sequences_modal(state$observation_ids, new_index, initial_slide)
    }
  })


  # Observe review sequences click from density map
  observeEvent(input$density_map_review_sequences_click, {
    action_data <- input$density_map_review_sequences_click

    location_name <- action_data$location_name
    locality <- action_data$locality
    supplied_observation_ids <- action_data$observation_ids

    if (!is.null(supplied_observation_ids) && length(supplied_observation_ids) > 0) {
      observation_ids <- unlist(supplied_observation_ids, use.names = FALSE)
      observation_ids <- observation_ids[nzchar(observation_ids)]

      if (length(observation_ids) > 0) {
        show_review_sequences_modal(observation_ids, 1)
      } else {
        showModal(modalDialog(
          title = "No Sequences Found",
          "There are no sequences to review for this selection.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }

      return()
    }

    source_map_id <- action_data$map_id

    if (identical(source_map_id, "density_map_comparative")) {
      period_obs <- filtered_obs_comparative()
      selected_species <- density_map_comparative$selected_species()
    } else {
      period_obs <- filtered_obs_density_map()
      selected_species <- density_map_primary$selected_species()
    }

    if (!is.null(period_obs) && !is.null(selected_species)) {
      # Filter to the specific location, locality and selected species
      species_to_map <- tolower(unname(selected_species))

      location_obs <- period_obs %>%
        filter(
          locationName == location_name,
          locality == locality,
          tolower(scientificName) %in% species_to_map
        )

      observation_ids <- location_obs$observationID

      if (length(observation_ids) > 0) {
        show_review_sequences_modal(observation_ids, 1)
      } else {
        showModal(modalDialog(
          title = "No Sequences Found",
          "There are no sequences to review for this selection.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    }
  })

} # server
