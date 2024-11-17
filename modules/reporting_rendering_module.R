reporting_rendering_module_ui <- function(id, page_id) {
  ns <- NS(id)
  
  # Function to generate UI for a specific report section
  generate_report_section_ui <- function(section_id) {

    # Depending on the section_id, return different UI components
    switch(section_id,
           "locality_camera_monitoring_network_overview" = tagList(
             uiOutput(ns("locality_camera_monitoring_network_overview_title")),
             dataTableOutput(ns("locality_camera_monitoring_network_overview")),
           ),
           "locality_mean_rai_se" = tagList(
             uiOutput(ns("locality_mean_rai_se_title")),
             dataTableOutput(ns("locality_mean_rai_se")),
           ),
           
           
           "locality_deployments_overview" = tagList(
             uiOutput(ns("locality_deployments_overview_title")),
             dataTableOutput(ns("locality_deployments_overview")),
           ),
           "locality_observations_overview" = tagList(
             uiOutput(ns("locality_observations_overview_title")),
             dataTableOutput(ns("locality_observations_overview")),
           ),

           
           "line_deployments_overview" = tagList(
             uiOutput(ns("line_deployments_overview_title")),
             dataTableOutput(ns("line_deployments_overview")),
           ),
           "line_observations_overview" = tagList(
             uiOutput(ns("line_observations_overview_title")),
             dataTableOutput(ns("line_observations_overview")),
           ),

           "location_deployments_overview" = tagList(
             uiOutput(ns("location_deployments_overview_title")),
             dataTableOutput(ns("location_deployments_overview")),
           ),
           "location_observations_overview" = tagList(
             uiOutput(ns("location_observations_overview_title")),
             dataTableOutput(ns("location_observations_overview")),
           ),

           "network_spp_summary" = tagList(
             uiOutput(ns("network_spp_summary_title")),
             dataTableOutput(ns("network_spp_summary")),
           ),
           "locality_spp_summary" = tagList(
             uiOutput(ns("locality_spp_summary_title")),
             dataTableOutput(ns("locality_spp_summary")),
             uiOutput(ns("locality_spp_summary_footnote")),
           ),
           "line_spp_summary" = tagList(
             uiOutput(ns("line_spp_summary_title")),
             dataTableOutput(ns("line_spp_summary")),
             uiOutput(ns("line_spp_summary_footnote")),
           ),
           "location_spp_summary" = tagList(
             uiOutput(ns("location_spp_summary_title")),
             dataTableOutput(ns("location_spp_summary")),
           ),
           

           
           
           "locality_species_mean_rai_showing_class" = tagList(
             uiOutput(ns("locality_species_mean_rai_showing_class_title")),
             dataTableOutput(ns("locality_species_mean_rai_showing_class")),
             uiOutput(ns("locality_species_mean_rai_showing_class_footnote")),
             hr()
           )
           
       #    "locality_species_rai_important_classes" = tagList(
      #       uiOutput(ns("locality_species_rai_important_classes_title")),
      #       dataTableOutput(ns("locality_species_rai_important_classes")),
      #       uiOutput(ns("locality_species_rai_important_classes_footnote")),
      #       hr()
      #     ),
      #     "locality_species_rai_less_important_classes" = tagList(
      #       uiOutput(ns("locality_species_rai_less_important_classes_title")),
      #       dataTableOutput(ns("locality_species_rai_less_important_classes")),
      #       uiOutput(ns("locality_species_rai_less_important_classes_footnote")),
      #       hr()
      #     )
           
           # observations_browse = dataTableOutput(ns("observations_browse")),
           #   deployments_browse = dataTableOutput(ns("deployments_browse")),
           #  global_species_exclusions = uiOutput(ns("global_species_exclusions"))
           

      ) # end of switch
  } # generate_report_section_ui
  
  generate_report_page_ui <- function(page_id) {
  # Depending on the page_id, assemble UI components for that page
  
    switch(page_id,
           "reporting_executive_summary" = tagList(
             generate_report_section_ui("locality_species_mean_rai_showing_class"),
             # generate_report_section_ui("locality_species_rai_important_classes"),
            #  generate_report_section_ui("locality_species_rai_less_important_classes"),
              generate_report_section_ui("network_spp_summary")
            ),


           "reporting_species_summary" = tagList(
             tabsetPanel(id = ns("reporting_spp_sum_tabsetpanel"),
                         tabPanel("Species by Locality",  
                                  generate_report_section_ui("locality_spp_summary"),
                         ),
                         tabPanel("Species by Line", 
                                  generate_report_section_ui("line_spp_summary"),
                         ),
                         tabPanel("Species by Location", 
                                  generate_report_section_ui("location_spp_summary"),
                         )
             ) # tabsetPanel
           ), # reporting_species_summary
            "reporting_camera_network_overview" = tagList(
              generate_report_section_ui("locality_camera_monitoring_network_overview"),
            ),
          
            "reporting_results_summary" = tagList(
              tabsetPanel(id = ns("reporting_results_summary_tabsetpanel"),
                tabPanel("Results by Locality",  
                  generate_report_section_ui("locality_deployments_overview"),
                  generate_report_section_ui("locality_observations_overview"),
                  generate_report_section_ui("locality_priority_species_count"),
                  generate_report_section_ui("locality_other_species_count")
                ),
                tabPanel("Results by Line", 
                  generate_report_section_ui("line_deployments_overview"),
                  generate_report_section_ui("line_observations_overview"),
                  generate_report_section_ui("line_priority_species_count"),
                  generate_report_section_ui("line_other_species_count"),
                  generate_report_section_ui("line_priority_species_rai")
                ),
                tabPanel("Results by Location", 
                  generate_report_section_ui("location_deployments_overview"),
                  generate_report_section_ui("location_observations_overview"),
                  generate_report_section_ui("location_priority_species_count"),
                  generate_report_section_ui("location_other_species_count")
                )
              ) # tabsetPanel
            ), # reporting_results_summary

    ) # switch
  } # generate_page_ui
  
  
  # Generate and return the UI for the current page
  generate_report_page_ui(page_id)
} 

#deps_filtered <- core_data$deps %>%
#  dplyr::filter(start >= selected_dates$start_date & end <= selected_dates$end_date)

#obs_filtered <- core_data$obs %>%
#  dplyr::filter(deploymentID %in% deps_filtered$deploymentID)



reporting_rendering_module_server <- function(id, reporting_data) {
  moduleServer(id, function(input, output, session) {

  # Ensure these calls to setup_table_output re-trigger on changes to reactive data  
  observe({

   # req(reporting_data())
    local_data <- reporting_data()
   # browser()
    setup_table_output(output, 
                       table_id = "locality_species_mean_rai_showing_class", 
                       data = local_data$spp_summary$locality_species_mean_rai_showing_class, 
                       table_type = "non-paged",
                       table_order = list(list(0, 'asc')))
    
#    setup_table_output(output, 
#                       table_id = "locality_species_rai_important_classes", 
#                       data = local_data$spp_summary$locality_species_rai_important_classes, 
#                       table_type = "non-paged",
#                       table_order = list(list(0, 'asc')))
    
    
#    setup_table_output(output, 
#                       table_id = "locality_species_rai_less_important_classes", 
#                       data = local_data$spp_summary$locality_species_rai_less_important_classes, 
#                       table_type = "non-paged",
#                       table_order = list(list(0, 'asc')))
    
    setup_table_output(output, 
                       table_id = "locality_camera_monitoring_network_overview", 
                       data = local_data$camera_network_overview, 
                       table_type = "non-paged",
                       table_order = list(list(0, 'asc')))
    
    setup_table_output(output, 
                       table_id = "locality_mean_rai_se", 
                       data = local_data$locality, 
                       table_type = "non-paged",
                       table_order = list(list(0, 'asc')))
    
    setup_table_output(output, 
                       table_id = "locality_deployments_overview", 
                       data = local_data$summary_data$locality, 
                       table_type = "non-paged",
                       table_order = list(list(0, 'asc')))
    
    setup_table_output(output, 
                       table_id = "locality_observations_overview", 
                       data = local_data$summary_data$locality,  
                       table_type = "non-paged",
                       table_order = list(list(0, 'asc')))
 
    
    ### LOCALITY-LINE OUTPUT ###
    
    setup_table_output(output, 
                       table_id = "line_deployments_overview", 
                       data = local_data$summary_data$line,
                       table_type = "non-paged",
                       table_order = list(list(0, 'asc')))
    
    setup_table_output(output, 
                       table_id = "line_observations_overview", 
                       data = local_data$summary_data$line,
                       table_type = "non-paged",
                       table_order = list(list(0, 'asc')))
    
    
    
    ### LOCATION OUTPUT ###
    setup_table_output(output, 
                       table_id = "location_deployments_overview", 
                       data = local_data$summary_data$location, 
                       table_type = "paged",
                       table_order = list(list(0, 'asc')))
    
    setup_table_output(output, 
                       table_id = "location_observations_overview", 
                       data = local_data$summary_data$location,
                       table_type = "paged",
                       table_order = list(list(0, 'asc')))
 
    
    setup_table_output(output, 
                       table_id = "network_spp_summary", 
                       data = local_data$spp_summary$network,
                       table_type = "paged",
                       table_order = list(list(1, 'asc'), list(0, 'asc')))
    
    setup_table_output(output, 
                       table_id = "locality_spp_summary", 
                       data = local_data$spp_summary$locality,
                       table_type = "paged",
                       table_order = list(list(2, 'asc'), list(1, 'asc'), list(0, 'asc')))
    
    setup_table_output(output, 
                       table_id = "line_spp_summary", 
                       data = local_data$spp_summary$line,
                       table_type = "paged",
                       table_order = list(list(3, 'asc'), list(2, 'asc'), list(0, 'asc'), list(1, 'asc')))
    
    
    setup_table_output(output, 
                       table_id = "location_spp_summary", 
                       data = local_data$spp_summary$location,
                       table_type = "paged",
                       table_order = list(list(4, 'asc'), list(3, 'asc'), list(2, 'asc')))
    

    
  })

    
    
  })
}


