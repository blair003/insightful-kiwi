



filter_deps <- function(deps, start_date, end_date) {
  deps %>%
    dplyr::filter(
      start <= as.Date(end_date),
      end >= as.Date(start_date)
    )
}

filter_obs <- function(obs, start_date, end_date) {
  obs %>%
    dplyr::filter(
      timestamp >= as.Date(start_date), 
      timestamp <= as.Date(end_date)
    )
}


# Stuff to do with report generation

generate_report_filename <- function(period_name, package_created_date, report_format) {
  package_date_posix <- as.POSIXct(package_created_date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  package_date_string <- format(package_date_posix, format = "%Y%m%d%H%M", tz = "Pacific/Auckland")
  
  # Construct the initial file name with potential spaces
  initial_filename <- paste(period_name, "deployment_report", package_date_string, sep = "_")
  
  # Determine the file extension based on the selected report format
  file_extension <- ifelse(report_format == "pdf", ".pdf", ".html")
  
  # Append the file extension to the filename and remove spaces
  final_filename <- gsub(" ", "_", paste0(initial_filename, file_extension))
  
  return(final_filename)
}


ensure_directories_exist <- function(reports_cache_dir, density_maps_dir, plots_dir) {
  if (!dir_exists(reports_cache_dir)) { 
    dir_create(reports_cache_dir)
  }
  if (!dir_exists(density_maps_dir)) { 
    dir_create(density_maps_dir)
  }
  if (!dir_exists(plots_dir)) { 
    dir_create(plots_dir)
  }
}


collate_reporting_data <- function(start_date, end_date, period_name, reporting_data, deps, config) {

  column_descriptions <- get_column_description()
  
  data_to_export <- list(
    dates = list(
      report_created = format(Sys.time(), "%d %B %Y %I:%M%p"),
      package_created_date = Sys.time(),
      deployments_period = period_name,
      deployments_start_date = start_date,
      deployments_end_date = end_date
    ),
    config = list(
      globals = list(
        species_name_type = config$globals$species_name_type
      ),
      
      meta = list(
        organisation_name = config$meta$organisation_name,
        organisation_header_logo = config$meta$organisation_header_logo,
        project_name = config$meta$project_name
      )
    
    ),
    data = list(
      camera_network_overview = reporting_data$camera_network_overview,
      spp_summary = reporting_data$spp_summary,
      summary_data = reporting_data$summary_data,
      column_descriptions = column_descriptions,
      deployments = deps
    )
  )
  
  return(data_to_export)
}

generate_density_maps <- function(named_class_species, period_name, reports_cache_dir, package_date_string, obs, deps, species_name_type) {
  density_maps <- list()
  
  for(i in seq_len(nrow(named_class_species))) {
    species_name_for_file <- as.character(named_class_species[[species_name_type]])[i]
    species_name_safe <- gsub(" ", "_", species_name_for_file)
    species_scientificName <- as.character(named_class_species$scientificName[i])
    
    map_html_file_path <- file.path(reports_cache_dir, "density_maps", gsub(" ", "_", paste0(period_name, "_", species_name_safe, "_map_", package_date_string, ".html")))
    map_png_file_path <- file.path(reports_cache_dir, "density_maps", gsub(" ", "_", paste0(period_name, "_", species_name_safe, "_map_", package_date_string, ".png")))
    
    if (!file.exists(map_png_file_path)) {
      density_map <- create_density_map(obs, deps, species_scientificName, TRUE)
      mapview::mapshot(density_map, url = map_html_file_path)
      webshot2::webshot(url = map_html_file_path, file = map_png_file_path)
    }
    
    density_maps[[species_name_for_file]] <- gsub(paste0("^", reports_cache_dir, "/"), "", map_png_file_path)
    
    
  }
  
  return(density_maps)
}




generate_locality_plots <- function(obs, unique_localities, period_name, reports_cache_dir, plots_dir, package_date_string) {
  plots_list <- list()
  
  for (locality_name in unique_localities) {
   # browser()
    ggplot_png_file_path <- file.path(plots_dir, 
                                      gsub(" ", "_", paste0(period_name, "_", locality_name, "_spp_obs_density_grid_", package_date_string, ".png")))
    
    num_unique_species <- obs %>% 
      dplyr::filter(locality == locality_name) %>% 
      dplyr::distinct(scientificName) %>% 
      nrow()
    
    plot_height <- num_unique_species * 0.4
    
    if (!file.exists(ggplot_png_file_path)) {
      spp_obs_density_grid <- create_species_observation_density_grid(obs, locality_name)
      ggsave(filename = ggplot_png_file_path, plot = spp_obs_density_grid, width = 10, height = plot_height + 2, dpi = 300)
    }
    
    # Save path relative to where report is (cache dir)
    plots_list[[locality_name]] <- gsub(paste0("^", reports_cache_dir, "/"), "", ggplot_png_file_path)
    
  }
  
  return(plots_list)
}


render_report <- function(period_name, package_date_string, reports_cache_dir, data_to_export) {
  report_template <- "resources/templates/deployment_report.Rmd"
  report_css <- "resources/templates/custom_report.css"
  
  if (!file.exists(report_template)) stop("Can't find deployment_report.Rmd, stopping")
  
  report_template_copy <- file.path(reports_cache_dir, gsub(" ", "_", paste0(period_name, "_deployment_report_", package_date_string, ".Rmd")))
  file.copy(report_template, report_template_copy, overwrite = TRUE)
  
  report_css_copy <- file.path(reports_cache_dir, "custom_report.css")
  file.copy(report_css, report_css_copy, overwrite = TRUE)
  
  rmarkdown::render(
    input = report_template_copy,
    output_format = "html_document",
    params = data_to_export,
    quiet = FALSE,
    envir = new.env(parent = globalenv())
  )
}


convert_to_pdf <- function(report_html, report_pdf) {
  #browser()
  if (!file.exists(report_pdf)) {
    system2("weasyprint", args = c(shQuote(report_html), shQuote(report_pdf)))
  }
  
  if (!file.exists(report_pdf)) stop("File not found: ", report_pdf)
}





