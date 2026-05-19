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
source("R/functions/dashboard_functions.R")
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
          )
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
        dashboard = input[["dashboard-main_dashboard_tabs"]],
        reporting = input$reporting_tabs,
        density_map = input$density_map_tabs,
        density_playback_map = input[["density_playback_map-density_playback_tabs"]],
        observation_map = input[["observation_map-observation_map_tabs"]],
        activity_patterns = input[["activity_patterns-activity_patterns_tabs"]],
        raw_data = input$raw_data_tabs,
        NULL
      )

      if (startsWith(nav, "species_dashboard_")) {
        tab <- input[[paste0(nav, "-dashboard_tabs")]]
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
    selected = core_data$period_defaults$primary_period
  )

  density_map_period <- period_selection_module_server(
    id = "density_map_period",
    period_groups = core_data$period_groups,
    summary_output_ids = character(0),
    selected = core_data$period_defaults$primary_period
  )

  comparative_period <- period_selection_module_server(
    id = "comparative_period", 
    period_groups = core_data$period_groups,
    summary_output_ids = character(0),
    selected = core_data$period_defaults$comparative_period
  )

  observation_map_period <- period_selection_module_server(
    id = "observation_map_period",
    period_groups = core_data$period_groups,
    summary_output_ids = character(0),
    selected = core_data$period_defaults$primary_period
  )

  # Reactive filtering of deps and obs for primary/reporting and map periods
  filtered_deps_primary <- reactive({
    filter_deps(core_data$deps, primary_period$start_date(), primary_period$end_date())
  })
  
  filtered_obs_primary <- reactive({
    filter_obs(core_data$obs, primary_period$start_date(), primary_period$end_date())
  })

  filtered_deps_density_map <- reactive({
    filter_deps_by_period_names(
      core_data$deps,
      density_map_period$period_names(),
      density_map_period$start_date(),
      density_map_period$end_date()
    )
  })

  filtered_obs_density_map <- reactive({
    filter_obs_by_period_names(
      core_data$obs,
      density_map_period$period_names(),
      density_map_period$start_date(),
      density_map_period$end_date()
    )
  })
  
  filtered_deps_comparative <- reactive({
    filter_deps_by_period_names(
      core_data$deps,
      comparative_period$period_names(),
      comparative_period$start_date(),
      comparative_period$end_date()
    )
  })
  
  filtered_obs_comparative <- reactive({
    filter_obs_by_period_names(
      core_data$obs,
      comparative_period$period_names(),
      comparative_period$start_date(),
      comparative_period$end_date()
    )
  })

  filtered_deps_observation_map <- reactive({
    filter_deps_by_period_names(
      core_data$deps,
      observation_map_period$period_names(),
      observation_map_period$start_date(),
      observation_map_period$end_date()
    )
  })

  filtered_obs_observation_map <- reactive({
    filter_obs_by_period_names(
      core_data$obs,
      observation_map_period$period_names(),
      observation_map_period$start_date(),
      observation_map_period$end_date()
    )
  })
  
  dashboard_state <- dashboard_module_server("dashboard", core_data = core_data, config = config, use_net = global_use_net)

  dashboard_plot_periods <- period_names_without_all(core_data$period_groups)
  dashboard_plot_periods <- dashboard_plot_periods[
    seq(core_data$period_defaults$primary_period_index, length(dashboard_plot_periods))
  ]
  dashboard_plot_deps <- core_data$deps %>%
    dplyr::filter(as.character(period) %in% dashboard_plot_periods)

  plotting_module_server(
    id = "spp_obs_plot_visualisations",
    type = NULL,
    obs = core_data$obs,
    deps = dashboard_plot_deps,
    species_override = NULL
  )
  # Initialize species dashboards dynamically (Lazy Loading)
  loaded_species_dashboards <- reactiveVal(character())
  pending_species_rai_detail <- reactiveVal(NULL)

  observeEvent(input$nav, {
    nav_item <- input$nav

    if (startsWith(nav_item, "species_dashboard_")) {
      sci_name_make_names <- sub("^species_dashboard_", "", nav_item)

      # Check if already loaded
      if (!(sci_name_make_names %in% loaded_species_dashboards())) {

        # Find the full scientific and vernacular names
        found <- FALSE
        for (group_name in names(core_data$spp_classes)) {
          species_in_group <- core_data$spp_classes[[group_name]]
          for (s_name in names(species_in_group)) {
            s_sci_name <- species_in_group[[s_name]]
            if (make.names(s_sci_name) == sci_name_make_names) {

              # Initialize module
              species_dashboard_module_server(
                id = paste0("species_dashboard_", make.names(s_sci_name)),
                species_name = s_sci_name,
                vernacular_name = s_name,
                obs = filtered_obs_primary,
                deps = filtered_deps_primary,
                core_data = core_data,
                rai_norm_hours = config$globals$rai_norm_hours,
                use_net = global_use_net,
                initial_rai_detail = pending_species_rai_detail()
              )
              pending_species_rai_detail(NULL)

              # Add to loaded list
              loaded_species_dashboards(c(loaded_species_dashboards(), sci_name_make_names))
              logger::log_debug(sprintf("server.R, lazily loaded species dashboard for %s", s_sci_name))

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
                       identical(input$nav, "raw_data") &&
                         identical(input$raw_data_tabs, "deps")
                     },
                     cache_prepared_data = TRUE) 
  
  
  setup_table_output(output, 
                     table_id = "rawdata_observations_browse", 
                     data = core_data$obs,
                     table_type = "paged",
                     table_order = list(list(4, 'asc')),
                     render_when = function() {
                       identical(input$nav, "raw_data") &&
                         identical(input$raw_data_tabs, "obs")
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
      "raw_data_tabs",
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
  
  
    
    # Render UI for the dashboard plot
    output$dash_spp_obs_plot <- renderUI({
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
#  output$reporting_raw_data_browse <- renderUI({
#    reporting_rendering_module_ui("current_tables", "reporting_raw_data_browse")
#  })
  

  
  # Updates visibility reactiveVal's based on which which menu is showing
  # Covers standard map features and autoplay features
  observeEvent(input$nav, {
    nav_item <- input$nav

    runjs(sprintf("gtag('config', %s, {'page_path': %s});",
                  jsonlite::toJSON(config$globals$ga_tag, auto_unbox = TRUE),
                  jsonlite::toJSON(paste0("/", nav_item), auto_unbox = TRUE)))

    
    if (nav_item  == "dashboard") {
      # Run latest images
      #message("observeEvent(input$nav, viewing dashboard page -- triggered image update")
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
    } else if (!is.null(query$raw_data_tabs) && nzchar(query$raw_data_tabs)) {
      query$raw_data_tabs
    } else {
      NULL
    }

    if (!is.null(query_tab) && !is.null(query$nav) && nzchar(query$nav)) {
      tabset_id <- switch(query$nav,
        dashboard = "dashboard-main_dashboard_tabs",
        reporting = "reporting_tabs",
        density_map = "density_map_tabs",
        density_playback_map = "density_playback_map-density_playback_tabs",
        observation_map = "observation_map-observation_map_tabs",
        activity_patterns = "activity_patterns-activity_patterns_tabs",
        raw_data = "raw_data_tabs",
        NULL
      )

      if (startsWith(query$nav, "species_dashboard_")) {
        tabset_id <- paste0(query$nav, "-dashboard_tabs")
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
      dashboard_state$show_rai_detail_modal(query$rai_detail)
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
  loaded_density_tabs <- reactiveVal(character())
  
  # Used for the tab names
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
    species_choices <- unlist(unname(core_data$spp_classes), use.names = TRUE)

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

  density_map_period_readout <- function() {
    current_tab <- input$density_map_tabs
    period <- if (identical(current_tab, "comparative")) {
      comparative_period
    } else {
      density_map_period
    }

    req(period$start_date(), period$end_date())

    timezone <- if (exists("playback_actual_timezone", mode = "function", inherits = TRUE)) {
      playback_actual_timezone()
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
      class = "playback-window-readout",
      strong("Current window:"),
      paste(
        format(start_time, "%Y-%m-%d %H:%M:%S", tz = timezone),
        "to",
        format(end_time, "%Y-%m-%d %H:%M:%S", tz = timezone)
      ),
      tags$span(timezone)
    )
  }

  output$density_map_selection_heading <- renderUI({
    species <- input[["density_map_primary-selected_species"]]
    localities <- input[["density_map_primary-selected_localities"]]
    tagList(
      div(class = "dashboard-locality-heading map-selection-heading", selected_map_heading(species, localities)),
      density_map_period_readout()
    )
  })

  output$observation_map_selection_heading <- renderUI({
    species <- input[["observation_map-selected_species"]]
    localities <- input[["observation_map-selected_localities"]]
    div(class = "dashboard-locality-heading map-selection-heading", selected_map_heading(species, localities))
  })

  output$density_playback_map_selection_heading <- renderUI({
    species <- input[["density_playback_map-selected_species"]]
    localities <- input[["density_playback_map-selected_localities"]]
    div(class = "dashboard-locality-heading map-selection-heading", selected_map_heading(species, localities))
  })
  
  observeEvent(list(input$nav, input$density_map_tabs), {
    req(input$nav == "density_map")

    current_tab <- input$density_map_tabs
    if (!is.null(current_tab) && !(current_tab %in% loaded_density_tabs())) {

      if (current_tab == "primary") {
        logger::log_debug("server.R, lazily calling mapping_module_server() for density_map_primary")
        density_map_primary <<- mapping_module_server(
          id = "density_map_primary",
          type = "density",
          obs = filtered_obs_density_map,
          deps = filtered_deps_density_map,
          period_start_date = density_map_period$start_date,
          period_end_date = density_map_period$end_date,
          use_net = global_use_net
        )
      } else if (current_tab == "comparative") {
        logger::log_debug("server.R, lazily calling mapping_module_server() for density_map_comparative")
        # Ensure primary is initialized first if we need its reactive properties, or fallback
        if (is.null(density_map_primary)) {
          density_map_primary <<- mapping_module_server(
            id = "density_map_primary",
            type = "density",
            obs = filtered_obs_density_map,
            deps = filtered_deps_density_map,
            period_start_date = density_map_period$start_date,
            period_end_date = density_map_period$end_date,
            use_net = global_use_net
          )
          loaded_density_tabs(c(loaded_density_tabs(), "primary"))
        }

        density_map_comparative <<- mapping_module_server(
          id = "density_map_comparative",
          type = "density",
          obs = filtered_obs_comparative,
          deps = filtered_deps_comparative,
          species_override = density_map_primary$selected_species,
          localities_override = density_map_primary$selected_localities,
          prediction_surface_override = density_map_primary$show_predicted_rai_surface,
          prediction_surface_basis_override = density_map_primary$predicted_rai_surface_basis,
          location_markers_override = density_map_primary$show_density_location_markers,
          marker_metric_override = density_map_primary$density_marker_metric,
          use_net = global_use_net
        )
      }

      loaded_density_tabs(c(loaded_density_tabs(), current_tab))
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  
  
  activity_patterns_module_server(
    id = "activity_patterns",
    core_data = core_data,
    nav = reactive(input$nav),
    current_period = dashboard_state$current_period,
    prior_period = dashboard_state$prior_period,
    last_year_period = dashboard_state$last_year_period,
    use_net = global_use_net
  )

  ########### DENSITY PLAYBACK MAP FEATURE ###########

  playback_period <- period_selection_module_server(
    id = "playback_period",
    period_groups = core_data$period_groups,
    selected = core_data$period_defaults$primary_period
  )

  filtered_deps_playback_map <- reactive({
    filter_deps_by_period_names(
      core_data$deps,
      playback_period$period_names(),
      playback_period$start_date(),
      playback_period$end_date()
    )
  })

  filtered_obs_playback_map <- reactive({
    filter_obs_by_period_names(
      core_data$obs,
      playback_period$period_names(),
      playback_period$start_date(),
      playback_period$end_date()
    )
  })

  density_playback_map_loaded <- reactiveVal(FALSE)

  observeEvent(input$nav, {
    if (input$nav == "density_playback_map" && !density_playback_map_loaded()) {
      logger::log_debug("server.R, lazily calling mapping_module_server() for density_playback_map")
      mapping_module_server(
        id = "density_playback_map",
        type = "density",
        obs = filtered_obs_playback_map,
        deps = filtered_deps_playback_map,
        period_start_date = playback_period$start_date,
        period_end_date = playback_period$end_date,
        playback_mode = "always",
        use_net = global_use_net
      )
      density_playback_map_loaded(TRUE)
    }
  })

  ########### OBSERVATION MAP FEATURE ###########
  
  observation_map <- NULL
  observation_map_loaded <- reactiveVal(FALSE)
  
  observeEvent(input$nav, {
    if (input$nav == "observation_map" && !observation_map_loaded()) {
      logger::log_debug("server.R, lazily calling mapping_module_server() for observation_map")
      observation_map <<- mapping_module_server(
        id = "observation_map",
        type = "observation",
        obs = filtered_obs_observation_map,
        deps = filtered_deps_observation_map,
        period_start_date = observation_map_period$start_date,
        period_end_date = observation_map_period$end_date,
        playback_mode = "always",
        use_net = global_use_net,
        trap_data = reactive(trap_data)
        # The module will use its own internal input$selected_species and input$enhance_map_details
        # from the UI elements defined by mapping_module_ui in the sidebar.
      )
      observation_map_loaded(TRUE)
    }
  })
  

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
  
  

  

  # Observe review sequences click from dashboard
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
      filtered_obs <- core_data$obs
    } else if (!is.null(period_name) && period_name %in% names(core_data$period_groups)) {
      period <- core_data$period_groups[[period_name]]

      # Filter obs
      filtered_obs <- filter_obs(core_data$obs, period$start_date, period$end_date)
    } else {
      filtered_obs <- NULL
    }

    if (!is.null(filtered_obs)) {

      # Apply locality filter
      if (!is.null(locality_filter) && locality_filter != "ALL") {
        localities <- strsplit(locality_filter, ",")[[1]]
        filtered_obs <- filtered_obs %>% filter(locality %in% localities)
      }

      # Apply species filter based on either a species dashboard click or an RAI group click.
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

    # We need to know which period we are in. Check if we are on primary or comparative tab.
    active_tab <- input$density_map_tabs

    if (is.null(active_tab) || active_tab == "primary") {
      period_obs <- filtered_obs_primary()
      selected_species <- density_map_primary$selected_species()
    } else {
      period_obs <- filtered_obs_comparative()
      selected_species <- density_map_comparative$selected_species()
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
