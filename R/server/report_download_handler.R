register_report_download_handler <- function(input,
                                             output,
                                             primary_period,
                                             current_reporting,
                                             filtered_deps_primary,
                                             filtered_obs_primary,
                                             core_data,
                                             config) {
  output$download_report <- downloadHandler(
    filename = function() {
      generate_report_filename(primary_period$period_name(), core_data$created, input$report_format)
    },

    content = function(file) {
      reports_cache_dir <- "cache/reports"
      density_maps_dir <- paste0(reports_cache_dir, "/density_maps")
      plots_dir <- paste0(reports_cache_dir, "/plots")

      period_name <- primary_period$period_name()
      package_created_date <- core_data$created
      package_date_string <- format(as.POSIXct(package_created_date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"), format = "%Y%m%d%H%M", tz = "Pacific/Auckland")

      ensure_directories_exist(reports_cache_dir, density_maps_dir, plots_dir)

      report_html <- file.path(reports_cache_dir, gsub(" ", "_", paste0(period_name, "_deployment_report_", package_date_string, ".html")))

      if (!file.exists(report_html)) {
        ensure_pandoc_available()

        start_date <- primary_period$start_date()
        end_date <- primary_period$end_date()
        reporting_data <- current_reporting$reporting_data()
        data_to_export <- collate_reporting_data(
          start_date,
          end_date,
          period_name,
          reporting_data,
          filtered_deps_primary(),
          config
        )

        named_class_species <- reporting_data$spp_summary$locality %>%
          dplyr::filter(species_class != config$globals$spp_class_unclassified) %>%
          dplyr::mutate(scientificName = factor(scientificName, levels = unname(unlist(config$globals$spp_classes)), ordered = TRUE)) %>%
          dplyr::arrange(scientificName) %>%
          dplyr::distinct(scientificName, vernacularNames.eng, .keep_all = TRUE) %>%
          dplyr::select(scientificName, vernacularNames.eng)

        data_to_export$data$density_maps <- generate_density_maps(
          named_class_species,
          period_name,
          reports_cache_dir,
          package_date_string,
          filtered_obs_primary(),
          filtered_deps_primary(),
          config$globals$species_name_type
        )

        unique_localities <- filtered_deps_primary() %>%
          dplyr::distinct(locality) %>%
          dplyr::pull(locality)

        data_to_export$data$plots <- generate_locality_plots(
          filtered_obs_primary(),
          unique_localities,
          period_name,
          reports_cache_dir,
          plots_dir,
          package_date_string
        )

        render_report(
          period_name,
          package_date_string,
          reports_cache_dir,
          data_to_export
        )
      }

      if (input$report_format == "pdf") {
        report_pdf <- file.path(reports_cache_dir, gsub(" ", "-", paste0(period_name, "_deployment_report_", package_date_string, ".pdf")))
        convert_to_pdf(report_html, report_pdf)
        file.copy(report_pdf, file, overwrite = TRUE)
      } else {
        file.copy(report_html, file, overwrite = TRUE)
      }
    }
  )
}
