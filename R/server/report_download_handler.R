register_report_download_handler <- function(input,
                                             output,
                                             primary_period,
                                             current_reporting,
                                             filtered_deps_primary,
                                             filtered_obs_primary,
                                             core_data,
                                             config,
                                             use_net = reactive(config$globals$use_net_data)) {
  output$download_report <- downloadHandler(
    filename = function() {
      generate_report_filename(primary_period$period_name(), core_data$created, input$report_format, use_net())
    },

    content = function(file) {
      reports_cache_dir <- config$env$dirs$reports
      abundance_maps_dir <- file.path(reports_cache_dir, "abundance_maps")
      plots_dir <- file.path(reports_cache_dir, "plots")

      period_name <- primary_period$period_name()
      package_created_date <- core_data$created
      package_date_string <- generate_report_package_date_string(package_created_date)

      ensure_directories_exist(reports_cache_dir, abundance_maps_dir, plots_dir)

      report_html <- file.path(reports_cache_dir, generate_report_output_filename(period_name, package_date_string, "html", use_net()))

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
          config,
          use_net()
        )

        report_obs <- filter_possible_duplicates_for_use_net(filtered_obs_primary(), use_net())

        named_class_species <- reporting_data$spp_summary$locality %>%
          dplyr::filter(species_class != config$globals$spp_class_unclassified) %>%
          dplyr::mutate(scientificName = factor(scientificName, levels = unname(unlist(config$globals$spp_classes)), ordered = TRUE)) %>%
          dplyr::arrange(scientificName) %>%
          dplyr::distinct(scientificName, vernacularNames.eng, .keep_all = TRUE) %>%
          dplyr::select(scientificName, vernacularNames.eng)

        data_to_export$data$abundance_maps <- generate_abundance_maps(
          named_class_species,
          period_name,
          reports_cache_dir,
          package_date_string,
          report_obs,
          filtered_deps_primary(),
          config$globals$species_name_type
        )

        unique_localities <- filtered_deps_primary() %>%
          dplyr::distinct(locality) %>%
          dplyr::pull(locality)

        data_to_export$data$plots <- generate_locality_plots(
          report_obs,
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
          data_to_export,
          use_net()
        )
      }

      if (!file.exists(report_html)) {
        stop("Report HTML was not created: ", report_html, call. = FALSE)
      }

      if (input$report_format == "pdf") {
        report_pdf <- file.path(reports_cache_dir, generate_report_output_filename(period_name, package_date_string, "pdf", use_net()))
        convert_to_pdf(report_html, report_pdf)
        if (!file.copy(report_pdf, file, overwrite = TRUE)) {
          stop("Failed to copy report PDF to download file.", call. = FALSE)
        }
      } else {
        if (!file.copy(report_html, file, overwrite = TRUE)) {
          stop("Failed to copy report HTML to download file.", call. = FALSE)
        }
      }
    }
  )
}
