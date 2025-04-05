# ui.R

ui <- tagList(
  useShinyjs(), 
  
  # true = show sidebar open by default on this page
  tags$script(HTML("
    var defaultSidebarState = {
      'dashboard': true,
      'reporting': true,
      'density_map': true,
      'explorer_map': true,
      'raw_data': false
    };
    
  ")),
  
  page_navbar(
    id = "nav",
    title = tags$div(
      style = "display: flex; align-items: center; height: 100%; padding-top: 8px; padding-right: 20px;",
      tags$img(
        src = "favicon.png",
        height = "32",
        width = "32",
        style = "margin-right: 10px;"
      ),
      "InsightfulKiwi"
    ),
    
    theme = bs_theme(version = 5, font_scale = 0.9, bootswatch = 'default'),
    
    header = tagList(
      add_busy_spinner(spin = "cube-grid", color = "#cf6819", onstart = TRUE), 
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$link(rel = "icon", href = "favicon.png", type = "image/png"),
        tags$script(src = "custom.js"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.21/lodash.min.js"),
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
        condition = "['reporting', 'explorer_map'].includes(input.nav) || 
        (input.nav === 'density_map' && input.density_map_tabs === 'primary')",

        period_selection_module_ui(
          id = "primary_period",
          view = "select",
          choices = names(core_data$period_groups),
          selected = names(core_data$period_groups)[config$globals$default_primary_period],
          label = "Primary season:"
        )
      ),
      
      conditionalPanel(
        condition = "input.nav === 'density_map' && input.density_map_tabs === 'comparative'",
        
        period_selection_module_ui(
          "comparative_period",
          view = "select",
          choices = names(core_data$period_groups),
          selected = names(core_data$period_groups)[config$globals$default_comparative_period], 
          label = "Comparative season:"
        )
      ),
      
      conditionalPanel(
        condition = "input.nav === 'dashboard'",
        # Species selection input
        
        plotting_module_ui(
          id = "spp_obs_plot_dashboard",
          view = "select_species",
          choices = core_data$spp_classes,
          selected = c(
            core_data$spp_classes[[1]][1],  # First species from the first list
            core_data$spp_classes[[1]][2],  # First species from the first list
            core_data$spp_classes[[1]][3]  # First species from the first list
            
            #core_data$spp_classes[[2]][1],  # First species from the second list
            #core_data$spp_classes[[2]][2]   # Second species from the second list
          )
        ),
        plotting_module_ui(
          id = "spp_obs_plot_dashboard",
          view = "select_localities",
          choices = unique(core_data$deps$locality),  # Set choices to unique localities
          selected = unique(core_data$deps$locality)  # Default selection is all localities
        ),
        plotting_module_ui(
          id = "spp_obs_plot_dashboard",
          view = "select_plot_options"
        )
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
          choices = core_data$spp_classes,
          selected = c(
            core_data$spp_classes[[2]][1],   # First species from the second list
            core_data$spp_classes[[2]][2]   # Second species from the second list
          )
        ),
        
        mapping_module_ui(
          id = "density_map_primary",
          view = "select_localities",
          choices = unique(core_data$deps$locality),  # Set choices to unique localities
          selected = unique(core_data$deps$locality)  # Default selection is all localities
        ),
        
        hr()
        
      ), # conditionalPanel
      
      conditionalPanel(
        condition = "input.nav === 'density_map' && input.density_map_tabs === 'primary'",
        
        div(class = "sidebar_heading", "OBSERVATIONS SUMMARY"),
        
        period_selection_module_ui(
          id = "primary_period", 
          view = "summary", 
          summary_output_id = "summary_output_density_map"
        ),
        
        mapping_module_ui(id = "density_map_primary", view = "summary")
      ),
      
      conditionalPanel(
        condition = "input.nav === 'density_map' && input.density_map_tabs === 'comparative'",
        
        div(class = "sidebar_heading", "OBSERVATIONS SUMMARY"),
        period_selection_module_ui(id = "comparative_period", view = "summary"),
        mapping_module_ui("density_map_comparative", view = "summary")
      ),
      
      ######### EXPLORER MAP #########
      conditionalPanel(
        condition = "input.nav === 'explorer_map'",
        
        selectizeInput(
          inputId = "explorer_map_selected_species",
          label = tagList(icon("paw"), "Species selection:"),
          choices = core_data$spp_classes,
          selected = c(
            core_data$spp_classes[[1]][1],  # First species from the first list
            core_data$spp_classes[[2]][1],  # First species from the second list
            core_data$spp_classes[[2]][2]   # Second species from the second list
          ),
          multiple = TRUE,
          options = list(
            placeholder = "Select species...",
            closeAfterSelect = TRUE
          )
        ),
        
 
        checkboxInput(
          inputId = "enhance_map_details",
          label = "Show area calc"
        ),
        
#        materialSwitch(
#          inputId = "enhance_map_details",
#          label = "Show area calc",
#          status = "primary"
#        ),
        
        hr(),
        # period_selection_module_ui(id = "primary_period", view = "summary", summary_output_id = "summary_output_explorer_map"),
        uiOutput("explorer_map_obs_summary"),
        
      ), # conditionalPanel
      
      conditionalPanel(
        condition = "input.nav === 'raw_data'",
        tags$small("Raw data for the entire project across all seasons is shown here.")
      ),
      
      ), # End of global sidebar
      
      nav_spacer(),

      nav_panel(
        "Dashboard",
        value = "dashboard",
        icon = icon("dashboard"),
        
          card(
              card_header(
                tagList(icon("eye"), "Species Observation, grouped by time period")  
              ),
              plotting_module_ui(id = "spp_obs_plot_dashboard", view = "plot"),
          #  full_screen = TRUE
            full_screen = FALSE
        ),
          
        layout_column_wrap(
          width = 1 / 4,
          
          card(
            card_header(
              tagList(icon("kiwi-bird"), "Kiwi Detections")
            ),
            card_body(div(textOutput("dashcard_kiwi_observations"), class = "dashcard-output")),
            full_screen = FALSE
          ),
          
          card(
            card_header(
              tagList(icon("paw"), "Animal Detections")
            ),
            card_body(div(textOutput("dashcard_animal_detections"), class = "dashcard-output")),
            full_screen = FALSE
          ),
          
          card(
            card_header(
              tagList(icon("camera"), "Camera Hours")
            ),
            card_body(div(textOutput("dashcard_camera_hours"), class = "dashcard-output")),
            full_screen = FALSE
          ),
          
          card(
            card_header(
              tagList(icon("rotate"), "Data Updated")
            ),
            card_body(div(textOutput("dashcard_data_updated"), class = "dashcard-output")),
            full_screen = FALSE
          )
        )
        
        
      ),
      
      nav_panel(
        "Report",
        value = "reporting",
        icon = icon("book"),
        
        # Main content area for Reporting
        navset_card_tab(
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
      ######### DENSITY MAP OUTPUT #########
      nav_panel(
        title = "Density Map", 
       # icon = icon("bullseye"),
        icon = icon("layer-group"),
        value = "density_map",
      #  h3(textOutput("density_map_title"), style = "text-align: center;"),
       
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
      
      nav_panel(
        title = "Explorer Map",
        #icon = icon("kiwi-bird"),
        icon = icon("map"),
        value = "explorer_map",
        navset_tab(
          id = "explorer_map_tabs",  # Assign an ID to track tab changes
          nav_panel(
            "Map",
            leafletOutput("explorer_map", height = config$globals$leaflet_height),
            uiOutput("explorer_map_textoverlay"),
            uiOutput("explorer_map_textoverlay_warning"),
            value = "explorer_map_map"
          ),
          nav_panel(
            "Data",
            h3("Browse observations shown on the map"),
            p(
              "The map is showing graphically the (unfiltered) results in this table, 
              subject to limits imposed for species with high counts (see notes tab)."
            ),
            DT::dataTableOutput("network_observations_browse"),
            value = "explorer_map_data"
          )
        )
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
          )
        )
      ),
    
      nav_spacer(),
      
      nav_item(
        tags$a(
          href = "#",
          # Prevent default navigation behavior
          class = "nav-link",
          onclick = "customInfoButtonClickHandler('About InsightfulKiwi', 'about_app_button')",
          # JavaScript function call for both icon and text
          style = "cursor: pointer;", # Set cursor to pointer to indicate clickability
          tags$span(
            tags$i(class = "fa fa-info-circle",
                   style = "margin-right: 5px;",  # Adds some space between icon and text),
                   ""  # Text to display next to the icon
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