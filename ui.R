# ui.R

ui <- function(request) {
  period_choices <- period_selection_choices(core_data$period_groups, config = config)

  tagList(
  useShinyjs(), 
  
  # true = show sidebar open by default on this page
  tags$script(HTML("
    var defaultSidebarState = {
      'overview': false,
      'plots': true,
      'reporting': true,
      'density_map': true,
      'monitoring_trapping_map': true,
      'density_timeline_map': true,
      'activity_pattern_map': true,
      'monitoring_trapping': true,
      'monitoring_trapping_analysis': true,
      'activity_patterns': true,
      'records': false
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
        condition = "input.nav === 'reporting' && ['exec_summary', 'results_summary', 'species_summary', 'visualisations'].indexOf(input.reporting_tabs) !== -1",

        period_selection_module_ui(
          id = "primary_period",
          view = "select",
          choices = period_choices,
          selected = core_data$app$period_defaults$primary_period,
          label = "Primary period:"
        )
      ),

      conditionalPanel(
        condition = "input.nav === 'density_map'",

        period_selection_module_ui(
          id = "density_map_period",
          view = "select",
          choices = period_choices,
          selected = core_data$app$period_defaults$primary_period,
          label = "Primary period:",
          multiple = TRUE
        ),
        period_selection_module_ui(
          id = "comparative_period",
          view = "select",
          choices = period_choices,
          selected = core_data$app$period_defaults$comparative_period,
          label = "Compare with:",
          multiple = TRUE
        )
      ),
      
      overview_module_ui("overview", view = "sidebar", core_data = core_data, config = config),
      conditionalPanel(
        condition = "input.nav === 'reporting' && input.reporting_tabs === 'plots'",
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
        condition = "input.nav === 'reporting' && ['exec_summary', 'results_summary', 'species_summary', 'visualisations'].indexOf(input.reporting_tabs) !== -1",
        
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
          ),
          include_species_display_mode = TRUE
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
      
      if (!is.null(trap_data)) {
        conditionalPanel(
          condition = "input.nav === 'monitoring_trapping_map'",

          period_selection_module_ui(
            id = "monitoring_trapping_map_period",
            view = "select",
            choices = period_choices,
            selected = core_data$app$period_defaults$primary_period,
            label = "Period:",
            multiple = TRUE
          ),

          mapping_module_ui(
            id = "monitoring_trapping_map_monitoring",
            view = "select_species",
            choices = core_data$app$spp_classes,
            selected = c(
              core_data$app$spp_classes[[1]][1],
              core_data$app$spp_classes[[1]][2],
              core_data$app$spp_classes[[1]][3]
            ),
            show_combined_species_note = FALSE
          ),

          mapping_module_ui(
            id = "monitoring_trapping_map_monitoring",
            view = "select_localities",
            choices = unique(core_data$deps$locality),
            selected = unique(core_data$deps$locality)
          ),

          mapping_module_ui(
            id = "monitoring_trapping_map_monitoring",
            view = "monitoring_trapping_density_options"
          )
        )
      },
      
      conditionalPanel(
        condition = "input.nav === 'density_timeline_map'",

        period_selection_module_ui(
          id = "timeline_period",
          view = "select",
          choices = period_choices,
          selected = core_data$app$period_defaults$primary_period,
          label = "Period:",
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
          ),
          include_species_display_mode = TRUE
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
          include_marker_options = TRUE,
          include_observation_layer_options = TRUE
        )
      ),

      conditionalPanel(
        condition = "input.nav === 'activity_pattern_map'",

        period_selection_module_ui(
          id = "activity_pattern_period",
          view = "select",
          choices = period_choices,
          selected = core_data$app$period_defaults$primary_period,
          label = "Period:",
          multiple = TRUE
        ),

        mapping_module_ui(
          id = "activity_pattern_map",
          view = "select_species",
          choices = core_data$app$spp_classes,
          selected = c(
            core_data$app$spp_classes[[1]][1],
            core_data$app$spp_classes[[1]][2],
            core_data$app$spp_classes[[1]][3]
          ),
          include_species_display_mode = TRUE
        ),

        mapping_module_ui(
          id = "activity_pattern_map",
          view = "select_localities",
          choices = unique(core_data$deps$locality),
          selected = unique(core_data$deps$locality)
        ),

        mapping_module_ui(
          id = "activity_pattern_map",
          view = "density_options",
          include_prediction_option = FALSE,
          include_marker_options = FALSE,
          include_density_trap_option = FALSE
        ),

        mapping_module_ui(
          id = "activity_pattern_map",
          view = "density_timeline_controls",
          include_marker_options = TRUE,
          include_observation_layer_options = FALSE,
          include_prediction_option = FALSE,
          lock_observation_markers = TRUE
        )
      ),


      conditionalPanel(
        condition = "input.nav === 'monitoring_trapping'",
        trapping_outcomes_module_ui("monitoring_trapping", core_data = core_data, config = config, trap_data = trap_data, view = "sidebar")
      ),

      conditionalPanel(
        condition = "input.nav === 'monitoring_trapping_analysis'",
        monitoring_trapping_module_ui("monitoring_trapping_analysis", core_data = core_data, config = config, trap_data = trap_data, view = "sidebar")
      ),

      conditionalPanel(
        condition = "input.nav === 'records'",
        tags$small("Records for the entire project across all periods are shown here.")
      ),

      species_overview_sidebar_controls(),
      
      ), # End of global sidebar
      
      nav_spacer(),

      overview_module_ui("overview", view = "main", core_data = core_data, config = config),

            ######### MAP/VISUALISATIONS MENU #########
      nav_menu(
        title = "Maps",
        icon = icon("map"),

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

          mapping_module_ui(
            id = "density_map_comparison",
            view = "density_comparison_layout",
            primary_map_id = "density_map_primary",
            comparative_map_id = "density_map_comparative",
            primary_heading_output_id = "primary_season_name",
            comparative_heading_output_id = "comparative_season_name",
            primary_meta_output_id = "primary_density_map_period_readout",
            comparative_meta_output_id = "comparative_density_map_period_readout"
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

                ######### ACTIVITY PATTERN MAP OUTPUT #########
        nav_panel(
          title = "Activity Pattern Density",
          icon = icon("clock"),
          value = "activity_pattern_map",
          div(
            class = "map-page-heading",
            h2("Activity Pattern Density"),
            uiOutput("activity_pattern_map_selection_heading")
          ),
          mapping_module_ui(
            "activity_pattern_map",
            view = "density_timeline_layout",
            timeline_step_size_choices = c(
              "Hourly" = "hour",
              "Diel activity" = "diel",
              "Daily - Day/Night" = "day_night"
            ),
            timeline_step_size_selected = "hour"
          )
        ),


        ######### MONITORING VS TRAPPING MAP OUTPUT #########
        if (!is.null(trap_data)) {
          nav_panel(
            title = "Monitoring vs Trapping",
            icon = icon("scale-balanced"),
            value = "monitoring_trapping_map",
            div(
              class = "map-page-heading",
              h2("Monitoring vs Trapping"),
              uiOutput("monitoring_trapping_map_selection_heading")
            ),
            mapping_module_ui(
              id = "monitoring_trapping_map_comparison",
              view = "density_comparison_layout",
              primary_map_id = "monitoring_trapping_map_monitoring",
              comparative_map_id = "monitoring_trapping_map_trapping",
              primary_meta_output_id = "monitoring_trapping_map_monitoring_period_readout",
              comparative_meta_output_id = "monitoring_trapping_map_trapping_period_readout",
              primary_title = "Monitoring data",
              comparative_title = "Trapping data",
              primary_data_label = "Monitoring",
              comparative_data_label = "Trapping",
              primary_data_title = "Monitoring records",
              comparative_data_title = "Trapping records"
            )
          )
        },


        ######### MONITORING & TRAPPING OUTPUT #########
        # Supercded by new Density Map and Density Map Timeline
      #  if (!is.null(trap_data)) {
      #    nav_panel(
      #      title = "Monitoring & Trapping",
      #      icon = icon("scale-balanced"),
      #      value = "monitoring_trapping",
      #      trapping_outcomes_module_ui("monitoring_trapping", core_data = core_data, config = config, trap_data = trap_data, view = "main")
      #    )
      #  },

        ######### TRAPPING ANALYSIS REFERENCE OUTPUT #########
        if (!is.null(trap_data)) {
          nav_panel(
            title = "Trapping Analysis",
            icon = icon("chart-simple"),
            value = "monitoring_trapping_analysis",
            monitoring_trapping_module_ui("monitoring_trapping_analysis", core_data = core_data, config = config, trap_data = trap_data, view = "main")
          )
        }
      ),
      
      species_overview_nav_menu(),
      
      nav_panel(
        "Reports",
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

          ######### PLOTS OUTPUT #########
          nav_panel(
            title = "Plots",
            icon = icon("chart-line"),
            value = "plots",
            card(
              class = "overview-plot-card",
              card_header(
                tagList(icon("eye"), "Species observations, grouped by time period")
              ),
              plotting_module_ui(id = "spp_obs_plot_visualisations", view = "plot"),
              full_screen = FALSE
            )
          ),

          ######### ACTIVITY PATTERNS OUTPUT #########
          activity_patterns_module_ui("activity_patterns", view = "main"),
          
          nav_spacer()
        )
      ),
        
    
      nav_panel(
        "Records",
        value = "records",
        icon = icon("table"),
      
        navset_tab( 
          id = "records_tabs",
          selected = "monitoring",
          nav_panel(
            "Monitoring",
            value = "monitoring",
            navset_tab(
              id = "monitoring_records_tabs",
              nav_panel(
                "Observations",
                DT::dataTableOutput("rawdata_observations_browse"),
                value = "obs"
              ),
              nav_panel(
                "Deployments",
                DT::dataTableOutput("rawdata_deployments_browse"),
                value = "deps"
              )
            )
          ),
          if (!is.null(trap_data)) {
            nav_panel(
              "Trapping",
              value = "trapping",
              navset_tab(
                id = "trapping_records_tabs",
                nav_panel(
                  "Observations",
                  DT::dataTableOutput("trapdata_observations_browse"),
                  value = "trap_obs"
                ),
                nav_panel(
                  "Deployments",
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
