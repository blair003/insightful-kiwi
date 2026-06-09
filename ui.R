# ui.R

ui <- function(request) {
  season_choices <- period_names_without_all(core_data$period_groups, assignable_only = FALSE)

  tagList(
  useShinyjs(), 
  
  # true = show sidebar open by default on this page
  tags$script(HTML("
    var defaultSidebarState = {
      'dashboard': false,
      'plots': true,
      'reporting': true,
      'density_map': true,
      'density_timeline_map': true,
      'observation_map': true,
      'monitoring_trapping': true,
      'trapping_performance': true,
      'activity_patterns': true,
      'raw_data': false
    };
    
  ")),
  
  page_navbar(
    id = "nav",
    title = tags$div(
      style = "display: flex; align-items: center; height: 100%; padding-top: 8px; padding-right: 20px;",
      tags$img(
        src = "images/icons/favicon.png",
        height = "32",
        width = "32",
        style = "margin-right: 10px;"
      ),
      "InsightfulKiwi"
    ),
    fillable = FALSE,
    fillable_mobile = FALSE,
    
    theme = bs_theme(version = 5, font_scale = 0.9, bootswatch = 'default'),
    
    header = tagList(
      add_busy_spinner(spin = "cube-grid", color = "#cf6819", onstart = TRUE), 
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$link(rel = "icon", href = "images/icons/favicon.png", type = "image/png"),
        tags$script(src = "custom.js"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.21/lodash.min.js"),
        tags$script(src = "https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.jsdelivr.net/npm/slick-carousel@1.8.1/slick/slick.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.jsdelivr.net/npm/slick-carousel@1.8.1/slick/slick-theme.css"),
        tags$script(src = "https://cdn.jsdelivr.net/npm/slick-carousel@1.8.1/slick/slick.min.js"),
        
        # Conditionally add Google Analytics code if ga_config is not null or empty
        if (!is.null(config$globals$ga_tag) && nzchar(config$globals$ga_tag)) {
          tagList(
            tags$script(async = NA, src = sprintf("https://www.googletagmanager.com/gtag/js?id=%s", config$globals$ga_tag)),
            tags$script(HTML(sprintf("
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
            gtag('js', new Date());
            gtag('config', '%s'); 
          ", config$globals$ga_tag)))
          )
        }
      )
    ),
    

    # Global sidebar
    sidebar = sidebar(
      id = "global_sidebar",
      
      conditionalPanel(
        condition = "input.nav === 'reporting'",

        period_selection_module_ui(
          id = "primary_period",
          view = "select",
          choices = season_choices,
          selected = core_data$app$period_defaults$primary_period,
          label = "Primary season:"
        )
      ),

      conditionalPanel(
        condition = "input.nav === 'density_map' && input.density_map_tabs === 'primary'",

        period_selection_module_ui(
          id = "density_map_period",
          view = "select",
          choices = season_choices,
          selected = core_data$app$period_defaults$primary_period,
          label = "Season selection:",
          multiple = TRUE
        )
      ),
      
      conditionalPanel(
        condition = "input.nav === 'density_map' && input.density_map_tabs === 'comparative'",
        
        period_selection_module_ui(
          "comparative_period",
          view = "select",
          choices = season_choices,
          selected = core_data$app$period_defaults$comparative_period,
          label = "Season selection:",
          multiple = TRUE
        )
      ),
      
      dashboard_module_ui("dashboard", view = "sidebar", core_data = core_data, config = config),
      conditionalPanel(
        condition = "input.nav === 'plots'",
        plotting_module_ui(
          id = "spp_obs_plot_visualisations",
          view = "select_species",
          choices = core_data$app$spp_classes,
          selected = c(
            core_data$app$spp_classes[[1]][1],
            core_data$app$spp_classes[[1]][2],
            core_data$app$spp_classes[[1]][3]
          )
        ),
        plotting_module_ui(
          id = "spp_obs_plot_visualisations",
          view = "select_localities",
          choices = unique(core_data$deps$locality),
          selected = unique(core_data$deps$locality)
        ),
        plotting_module_ui(
          id = "spp_obs_plot_visualisations",
          view = "select_plot_options"
        )
      ),

      activity_patterns_module_ui(
        id = "activity_patterns",
        view = "sidebar",
        species_choices = core_data$app$spp_classes,
        selected_species = c(
          core_data$app$spp_classes[[1]][1],
          core_data$app$spp_classes[[1]][2],
          core_data$app$spp_classes[[1]][3]
        ),
        locality_choices = unique(core_data$deps$locality),
        selected_localities = unique(core_data$deps$locality)
      ),
      
      # Conditional content for Report
      conditionalPanel(
        condition = "input.nav === 'reporting'",
        
        period_selection_module_ui(id = "primary_period", 
                                   view = "summary", 
                                   summary_output_id = "summary_output_reporting")
        
      ), # conditionalPanel
      
      
      ######### DENSITY MAP #########
      # Conditional content for Density Map
      conditionalPanel(
        condition = "input.nav === 'density_map'",

        mapping_module_ui(
          id = "density_map_primary",
          view = "select_species",
          choices = core_data$app$spp_classes,
          selected = c(
            core_data$app$spp_classes[[1]][1],  # First species from the first list
            core_data$app$spp_classes[[1]][2],  # Second species from the first list
            core_data$app$spp_classes[[1]][3]  # Third species from the first list
          )
        ),
        
        mapping_module_ui(
          id = "density_map_primary",
          view = "select_localities",
          choices = unique(core_data$deps$locality),  # Set choices to unique localities
          selected = unique(core_data$deps$locality)  # Default selection is all localities
        ),

        mapping_module_ui(
          id = "density_map_primary",
          view = "density_options"
        )
      ), # conditionalPanel
      
      conditionalPanel(
        condition = "input.nav === 'density_timeline_map'",

        period_selection_module_ui(
          id = "playback_period",
          view = "select",
          choices = season_choices,
          selected = core_data$app$period_defaults$primary_period,
          label = "Season selection:",
          multiple = TRUE
        ),

        mapping_module_ui(
          id = "density_timeline_map",
          view = "select_species",
          choices = core_data$app$spp_classes,
          selected = c(
            core_data$app$spp_classes[[1]][1],
            core_data$app$spp_classes[[1]][2],
            core_data$app$spp_classes[[1]][3]
          )
        ),

        mapping_module_ui(
          id = "density_timeline_map",
          view = "select_localities",
          choices = unique(core_data$deps$locality),
          selected = unique(core_data$deps$locality)
        ),

        mapping_module_ui(
          id = "density_timeline_map",
          view = "density_options",
          include_prediction_option = FALSE,
          include_marker_options = FALSE
        ),

        mapping_module_ui(
          id = "density_timeline_map",
          view = "density_timeline_controls",
          include_marker_options = TRUE
        )
      ),
      
      
      ######### OBSERVATION MAP #########
      # Global sidebar conditional content for Observation Map
      conditionalPanel(
        condition = "input.nav === 'observation_map'",

        period_selection_module_ui(
          id = "observation_map_period",
          view = "select",
          choices = season_choices,
          selected = core_data$app$period_defaults$primary_period,
          label = "Season selection:",
          multiple = TRUE
        ),
        
        # Call module UI for species selection
        mapping_module_ui(
          id = "observation_map", # Must match the server and main layout ID
          view = "select_species",
          choices = core_data$app$spp_classes, # Pass choices
          selected = c( # Default selected
            core_data$app$spp_classes[[1]][1],  # First species from the first list
            core_data$app$spp_classes[[1]][2],  # Second species from the first list
            core_data$app$spp_classes[[1]][3]  # Third species from the first list
          ),
          label = "Species selection:",
          show_combined_species_note = FALSE
        ),
        # Call module UI for locality selection
         mapping_module_ui(
           id = "observation_map",
           view = "select_localities",
           choices = unique(core_data$deps$locality),
           selected = unique(core_data$deps$locality),
           label = "Locality selection:"
         ),
        
        # Call module UI for other options
        mapping_module_ui(
          id = "observation_map",
          view = "select_observation_map_options"
        ),

        mapping_module_ui(
          id = "observation_map",
          view = "density_timeline_controls",
          include_prediction_option = FALSE,
          include_monitoring_area_option = TRUE,
          include_observation_layer_options = TRUE
        )
        
      ), # conditionalPanel
      
      
      conditionalPanel(
        condition = "input.nav === 'monitoring_trapping'",
        monitoring_trapping_module_ui("monitoring_trapping", core_data = core_data, config = config, trap_data = trap_data, view = "sidebar")
      ),

      conditionalPanel(
        condition = "input.nav === 'trapping_performance'",
        trapping_performance_module_ui("trapping_performance", core_data = core_data, config = config, trap_data = trap_data, view = "sidebar")
      ),

      conditionalPanel(
        condition = "input.nav === 'raw_data'",
        tags$small("Raw data for the entire project across all seasons is shown here.")
      ),

      species_dashboard_sidebar_controls(),
      
      ), # End of global sidebar
      
      nav_spacer(),

      dashboard_module_ui("dashboard", view = "main", core_data = core_data, config = config),

      species_dashboard_nav_menu(),
      
      nav_panel(
        "Report",
        value = "reporting",
        icon = icon("book"),
        
        # Main content area for Reporting
        navset_card_tab(
          id = "reporting_tabs",
          selected = "exec_summary",
          # Default selected panel
          nav_panel(
            title = "Executive summary",
            value = "exec_summary",
            h1("Executive Summary"),
            #uiOutput("season_selection_text"),
            div(
              class = "inline-elements",
              selectInput("report_format", "", choices = c("PDF" = "pdf", "HTML" = "html")),
              downloadButton("download_report", "Download Report")
            ),
            
            hr(),
            uiOutput("reporting_executive_summary")
          ),
          nav_panel(
            title = "Results summary",
            value = "results_summary",
            h1("Results Summary"),
            p(
              "This section summarises results, by Locality (most summarised), by Line and by Location (least summarised)."
            ),
            uiOutput("reporting_results_summary")
          ),
          nav_panel(
            title = "Species summary",
            value = "species_summary",
            h1("Species Summary"),
            p(
              "This section shows species summary information, by Locality, by Line and by Location."
            ),
            uiOutput("reporting_species_summary")
          ),
          nav_panel(
            title = "Visualisations",
            value = "visualisations",
            h1("Visualisations"),
              nav_panel(
                "Daily species counts",
          #      p("plotly version, you can mouseover to see more data"),
                plotlyOutput("locality_ggplotly_daily_species_count"),
              )

          ),
          
          nav_spacer()
        )
      ),

      ######### VISUALISATIONS MENU #########
      nav_menu(
        title = "Visualisations",
        icon = icon("chart-bar"),

        ######### DENSITY MAP OUTPUT #########
        nav_panel(
          title = "Density Summary",
         # icon = icon("bullseye"),
          icon = icon("layer-group"),
          value = "density_map",
          div(
            class = "map-page-heading",
            h2("Density Summary"),
            uiOutput("density_map_selection_heading")
          ),

          navset_tab(
            id = "density_map_tabs",
            nav_panel(
              div(textOutput("primary_season_name")),  # dynamic season name
              mapping_module_ui("density_map_primary", view = "map"),
              value = "primary"
            ),
            nav_panel(
              div(textOutput("comparative_season_name")),  # dynamic comparative season name
              mapping_module_ui("density_map_comparative", view = "map"),
              value = "comparative"
            )
          )
        ),

        ######### DENSITY TIMELINE MAP OUTPUT #########
        nav_panel(
          title = "Density Timeline",
          icon = icon("play-circle"),
          value = "density_timeline_map",
          div(
            class = "map-page-heading",
            h2("Density Timeline"),
            uiOutput("density_timeline_map_selection_heading")
          ),
          mapping_module_ui("density_timeline_map", view = "density_timeline_layout")
        ),

        ######### OBSERVATION MAP OUTPUT #########
        nav_panel(
          title = "Observation Timeline",
          # icon = icon("bullseye"),
          # icon = icon("layer-group"),
          icon = icon("map-location-dot"), # Example new icon
          value = "observation_map",
          div(
            class = "map-page-heading",
            h2("Observation Timeline"),
            uiOutput("observation_map_selection_heading")
          ),
          # Call the module UI for the main layout
          mapping_module_ui(id = "observation_map", view = "observation_map_layout")
        ),

        ######### MONITORING VS TRAPPING OUTPUT #########
        if (!is.null(trap_data)) {
          nav_panel(
            title = "Monitoring vs Trapping",
            icon = icon("scale-balanced"),
            value = "monitoring_trapping",
            monitoring_trapping_module_ui("monitoring_trapping", core_data = core_data, config = config, trap_data = trap_data, view = "main")
          )
        },

        ######### TRAPPING PERFORMANCE OUTPUT #########
        if (!is.null(trap_data)) {
          nav_panel(
            title = "Trap Performance",
            icon = icon("gauge-high"),
            value = "trapping_performance",
            trapping_performance_module_ui("trapping_performance", core_data = core_data, config = config, trap_data = trap_data, view = "main")
          )
        },

        ######### PLOTS OUTPUT #########
        nav_panel(
          title = "Plots",
          icon = icon("chart-line"),
          value = "plots",
          card(
            class = "dashboard-plot-card",
            card_header(
              tagList(icon("eye"), "Species observations, grouped by time period")
            ),
            plotting_module_ui(id = "spp_obs_plot_visualisations", view = "plot"),
            full_screen = FALSE
          )
        ),

        ######### ACTIVITY PATTERNS OUTPUT #########
        activity_patterns_module_ui("activity_patterns", view = "main")
      ),
        
    
      nav_panel(
        "Raw Data",
        value = "raw_data",
        icon = icon("table"),
      
        navset_tab( 
          id = "raw_data_tabs",
          nav_panel(
            "Observations",
            DT::dataTableOutput("rawdata_observations_browse"),
            value = "obs"
          ),
          nav_panel(
            "Deployments",
            DT::dataTableOutput("rawdata_deployments_browse"),
            value = "deps"
          ),
          if (!is.null(trap_data)) {
            nav_panel(
              "Trap Data",
              value = "trap_data",
              navset_tab(
                id = "trap_data_tabs",
                nav_panel(
                  "Trap observations",
                  DT::dataTableOutput("trapdata_observations_browse"),
                  value = "trap_obs"
                ),
                nav_panel(
                  "Trap deployments",
                  DT::dataTableOutput("trapdata_deployments_browse"),
                  value = "trap_deps"
                ),
                nav_panel(
                  "Trap summary",
                  DT::dataTableOutput("trapdata_summary_browse"),
                  value = "trap_summary"
                ),
                nav_panel(
                  "Conversion summary",
                  DT::dataTableOutput("trapdata_conversion_summary"),
                  value = "conversion_summary"
                )
              )
            )
          }
        )
      ),
    
      nav_spacer(),
      
      nav_item(actionLink("global_share_btn", label = NULL, icon = icon("share-nodes"), title = "Share this view")),

      nav_item(
        tags$a(
          href = "#",
          class = "nav-link",
          onclick = "Shiny.setInputValue('global_setup_btn', Math.random(), {priority: 'event'}); return false;",
          style = "cursor: pointer;",
          tags$span(
            tags$i(
              class = "fa fa-gear",
              title = "Setup",
              style = "margin-right: 5px;",
              ""
            )
          )
        ),
        
        nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
        
        footer = tagList(
          hidden(actionButton("reset_button", "Reset"))
        )
      )
  )
  )
}
