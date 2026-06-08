
generate_report_package_date_string <- function(package_created_date) {
  package_date_posix <- as.POSIXct(package_created_date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  format(package_date_posix, format = "%Y%m%d%H%M", tz = config$globals$actual_timezone)
}

get_report_count_basis <- function(use_net) {
  if (isTRUE(use_net)) "net" else "total"
}

generate_report_output_filename <- function(period_name, package_date_string, report_format, use_net = NULL) {
  filename_parts <- c(period_name)
  if (!is.null(use_net)) {
    filename_parts <- c(filename_parts, get_report_count_basis(use_net))
  }
  filename_parts <- c(filename_parts, "deployment_report", package_date_string)

  file_extension <- ifelse(report_format == "pdf", ".pdf", ".html")
  gsub(" ", "_", paste0(paste(filename_parts, collapse = "_"), file_extension))
}

generate_report_filename <- function(period_name, package_created_date, report_format, use_net = NULL) {
  package_date_string <- generate_report_package_date_string(package_created_date)
  generate_report_output_filename(period_name, package_date_string, report_format, use_net)
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


collate_reporting_data <- function(start_date, end_date, period_name, reporting_data, deps, config, use_net = get_use_net_data_setting()) {

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
        species_name_type = config$globals$species_name_type,
        use_net_data = isTRUE(use_net)
      ),
      
      meta = list(
        organisation_name = config$meta$organisation_name,
        organisation_header_logo = config$meta$organisation_header_logo,
        project_name = config$meta$project_name
      ),

      paths = list(
        report_shared_resources = normalizePath(config$env$dirs$report_resources, mustWork = FALSE)
      )
    ),
    data = list(
      camera_network_overview = reporting_data$camera_network_overview,
      spp_summary = reporting_data$spp_summary,
      summary_data = reporting_data$summary_data,
      column_descriptions = column_descriptions,
      count_basis = if (isTRUE(use_net)) "net" else "total",
      count_basis_label = if (isTRUE(use_net)) "Net data: possible duplicates excluded" else "Total data: possible duplicates included",
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
    
    # Define file paths
    html_output_dir <- file.path(reports_cache_dir, "density_maps", "html")
    png_output_dir <- file.path(reports_cache_dir, "density_maps", "png")
    
    # Ensure directories exist
    if (!dir.exists(html_output_dir)) dir.create(html_output_dir, recursive = TRUE)
    if (!dir.exists(png_output_dir)) dir.create(png_output_dir, recursive = TRUE)
    
    map_html_file_path <- file.path(html_output_dir, gsub(" ", "_", paste0(period_name, "_", species_name_safe, "_map_", package_date_string, ".html")))
    map_png_file_path <- file.path(png_output_dir, gsub(" ", "_", paste0(period_name, "_", species_name_safe, "_map_", package_date_string, ".png")))
    
    if (!file.exists(map_png_file_path)) {
      density_map <- create_density_map(obs, deps, species_scientificName, TRUE)
      
      # Save as dependency-backed HTML so map screenshots do not require Pandoc.
      suppressWarnings(
        htmlwidgets::saveWidget(density_map, map_html_file_path, selfcontained = FALSE, libdir = "map_libs")
      )
      
      # Check if HTML file was created successfully before attempting to webshot it
      if (file.exists(map_html_file_path)) {
        logger::log_info(sprintf("Successfully created HTML map: %s", map_html_file_path))
        # Convert HTML to PNG using webshot2
        webshot2::webshot(url = map_html_file_path, file = map_png_file_path, delay = 2) # Added a small delay
        logger::log_info(sprintf("Successfully created PNG map: %s", map_png_file_path))
      } else {
        logger::log_error(sprintf("Failed to create HTML map: %s", map_html_file_path))
      }
    }
    
    # Store the relative path to the PNG for the report
    density_maps[[species_name_for_file]] <- file.path("density_maps", "png", basename(map_png_file_path))
  }
  
  return(density_maps)
}


ensure_pandoc_available <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("The rmarkdown package is required to render reports.", call. = FALSE)
  }

  if (rmarkdown::pandoc_available("1.12.3")) {
    return(invisible(TRUE))
  }

  candidate_dirs <- c(
    Sys.getenv("RSTUDIO_PANDOC", unset = NA_character_),
    file.path(Sys.getenv("ProgramFiles"), "RStudio", "resources", "app", "bin", "quarto", "bin", "tools"),
    file.path(Sys.getenv("ProgramFiles"), "RStudio", "resources", "app", "bin", "pandoc"),
    file.path(Sys.getenv("ProgramFiles(x86)"), "RStudio", "resources", "app", "bin", "pandoc")
  )
  candidate_dirs <- unique(candidate_dirs[!is.na(candidate_dirs) & nzchar(candidate_dirs)])

  for (candidate_dir in candidate_dirs) {
    pandoc_exe <- file.path(candidate_dir, if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc")
    if (file.exists(pandoc_exe)) {
      Sys.setenv(RSTUDIO_PANDOC = candidate_dir)
      if (rmarkdown::pandoc_available("1.12.3")) {
        logger::log_info(sprintf("Using Pandoc from %s", candidate_dir))
        return(invisible(TRUE))
      }
    }
  }

  stop(
    paste(
      "Pandoc 1.12.3 or higher is required to render reports, but it was not found.",
      "Install Pandoc or set RSTUDIO_PANDOC to the folder containing pandoc.exe."
    ),
    call. = FALSE
  )
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


render_report <- function(period_name, package_date_string, reports_cache_dir, data_to_export, use_net) {
  #browser()
  ensure_pandoc_available()

  previous_use_net_data <- config$globals$use_net_data
  config$globals$use_net_data <<- isTRUE(data_to_export$config$globals$use_net_data)
  on.exit({
    config$globals$use_net_data <<- previous_use_net_data
  }, add = TRUE)

  report_template_dir <- file.path(config$env$dirs$report_templates, "deployment_report")
  report_template <- file.path(report_template_dir, "deployment_report.Rmd")
  report_css <- file.path(report_template_dir, "custom_report.css")
  
  if (!file.exists(report_template)) stop("Can't find deployment_report.Rmd, stopping")
  if (!file.exists(report_css)) stop("Can't find custom_report.css, stopping")
  
  report_html_filename <- generate_report_output_filename(period_name, package_date_string, "html", use_net)
  report_template_copy <- file.path(reports_cache_dir, sub("\\.html$", ".Rmd", report_html_filename))
  file.copy(report_template, report_template_copy, overwrite = TRUE)
  
  report_template_resources_path <- normalizePath(file.path(report_template_dir, "resources"), mustWork = FALSE)
  report_shared_resources_path <- normalizePath(config$env$dirs$report_resources, mustWork = FALSE)
  data_to_export$config$paths$report_template_resources <- report_template_resources_path
  data_to_export$config$paths$report_shared_resources <- report_shared_resources_path

  reports_cache_path <- normalizePath(reports_cache_dir, mustWork = FALSE)
  report_shared_resources_relative <- fs::path_rel(report_shared_resources_path, start = reports_cache_path)
  report_shared_resources_relative <- gsub("\\\\", "/", report_shared_resources_relative)

  report_css_copy <- file.path(reports_cache_dir, "custom_report.css")
  report_css_content <- readLines(report_css, warn = FALSE)
  report_css_content <- gsub("{{REPORT_SHARED_RESOURCES_PATH}}", report_shared_resources_relative, report_css_content, fixed = TRUE)
  writeLines(report_css_content, report_css_copy)
  
  rmarkdown::render(
    input = report_template_copy,
    output_format = "html_document",
    output_file = report_html_filename,
    output_dir = reports_cache_dir,
    params = data_to_export,
    quiet = FALSE,
    envir = new.env(parent = globalenv())
  )
}


convert_to_pdf <- function(report_html, report_pdf) {
  # browser()
  if (!file.exists(report_pdf)) {
    system2("weasyprint", args = c(shQuote(report_html), shQuote(report_pdf)))
  }
  
  if (!file.exists(report_pdf)) stop("File not found: ", report_pdf)
}



