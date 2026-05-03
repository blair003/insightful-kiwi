# Function returns columns to round and decimal places to round to
# Separate to make it easier to edit without touching code

get_columns_to_round <- function() {
  
  columns_to_round <- list(
    camera_hours = 0,
    mean_detection_interval = 2,
    encompassed_area_km2 = 2,
    camera_density_km2 = 2,
    mean_location_pair_spacing = 0,
    blank_detections_percentage = 1,
    possible_duplicates_percentage = 1,
    RAI = 2,
    RAI_net = 2
  )
  
  return(columns_to_round)
}






# Active table_id values that route through get_table_specification().
# The table_id convention is "<view>_<table_name>"; get_table_name_and_view()
# treats the first underscore-separated token as the view and the remainder as
# the table name.
#
# Raw data page:
# - rawdata_observations_browse
# - rawdata_deployments_browse
#
# Observation map:
# - observationmap_observations_browse
#
# Reporting:
# - locality_species_mean_rai_showing_class
# - locality_camera_monitoring_network_overview
# - locality_mean_rai_se
# - locality_deployments_overview
# - locality_observations_overview
# - line_deployments_overview
# - line_observations_overview
# - location_deployments_overview
# - location_observations_overview
# - network_spp_summary
# - locality_spp_summary
# - line_spp_summary
# - location_spp_summary
#
# Observation viewer:
# - special_observation_viewer
# - sequence_editor_viewer
get_table_specification <- function(table_id = NULL) {
   
  table <- get_table_name_and_view(table_id)
  table_name = table$name
  table_view = table$view
  
  output_data <- list()
  formatted_table_view <- str_to_title(str_replace_all(table_view, "_", " "))
  output_data$footnote = NULL
  
  switch(table_name,
         
         "species_mean_rai_showing_class" = {
           names_list <- names(config$globals$spp_classes)
           all_classes_list = paste(names_list, collapse = ", ")
           
           # Generate class lists with linked species names
           classes_with_links <- lapply(
             seq_along(names_list),
             function(i) generate_linked_species_names(
               config$globals$spp_classes[[i]],
               core_data$taxonomic,
               config$globals$species_name_type
             )
           )
           names(classes_with_links) <- names_list
           
           # Construct the title and caption
           output_data$title = "Mean RAI ± SE: important species"
           
           output_data$caption = sprintf(
              "Table showing species mean RAI ± SE per %s camera hours by Locality, 
              for each species in a named class (%s). If a species in a named class 
              is absent from this table, it means there were no observations of that 
              species in any Locality during the deployment selection period.", 
              config$globals$rai_norm_hours , all_classes_list)
           
           # Construct footnote
           footnote_parts <- sapply(
             names(classes_with_links),
             function(name) sprintf(
               "Full <strong>%s</strong> species list: %s",
               name, classes_with_links[[name]]
             ),
             USE.NAMES = FALSE
           )
           combined_footnote <- paste(footnote_parts, collapse = ".<br>")
           
           if (config$globals$rai_net_count) {
             output_data$footnote <- paste(
               "The RAI ± SE figures have been calculated based on 
                Net Individuals Count (i.e. excluding Possible Duplicates).<br><br>",
               combined_footnote
             )
           } else {
             output_data$footnote <- combined_footnote
           }
           
           output_data$fields <- c("ALL")
         },
         
         "deployments_browse" = {
           output_data$title <- "Deployments browse"
           output_data$caption <- paste(
              "Table showing deployments included in the deployment selection. 
              Default sorting is by start date, which shows the order in which 
              they were deployed."
           )
           
           
           switch(table_view,
                  "network" = {
                    output_data$fields <- c("locality", 
                                            "line", 
                                            "locationName", 
                                            "start",
                                            "end",
                                            "camera_hours",
                                            "blank_detections_count",
                                            "unknown_detections_count")
                    
                  },
                  "rawdata" = {
                    output_data$fields <- c("locality", 
                                            "line", 
                                            "locationName", 
                                            "period", 
                                            "start",
                                            "end",
                                            "camera_hours",
                                            "blank_detections_count",
                                            "unknown_detections_count")
                  }
           )
           
         },
         
         "observations_browse" = {
           output_data$title = "Observations browse"

           switch(table_view,
                  "network" = {
                    output_data$caption <- "Table showing observations during the 
                    deployment selection."
                    
                    output_data$fields <- c("locality", 
                                            "line", 
                                            "locationName",
                                            "timestamp", 
                                            config$globals$species_name_type,
                                            "count",
                                            "possible_duplicate",
                                            "observationID")
                    
                  },
                  "rawdata" = {
                    output_data$caption <- "Table showing all observation records 
                    from the entire project."
                    
                    output_data$fields <- c("locality", 
                                            "line", 
                                            "locationName", 
                                            "period",
                                            "timestamp",
                                            config$globals$species_name_type,
                                            "count",
                                            "possible_duplicate",
                                            "observationID")

                  },
                  "observationmap" = {
                    output_data$caption <- "Table showing observations currently
                    selected for the Observation Map."

                    output_data$fields <- c("locality",
                                            "line",
                                            "locationName",
                                            "period",
                                            "timestamp",
                                            config$globals$species_name_type,
                                            "count",
                                            "possible_duplicate",
                                            "observationID")
                    
                  }
           )
         },
         
         
         "observation_viewer" = {
           output_data$title <- "Observations browse"
           output_data$caption <- NULL
           output_data$fields <- c("locality", "locationName", "timestamp", 
                                   "count", config$globals$species_name_type, "comments")
           
         },
         
         
         "camera_monitoring_network_overview" = {
           output_data$title <- "Spatial overview of the network"
           output_data$caption <- paste(
              "Table showing an overview including spatial data for each Locality 
              that was active during the deployment selection period. Spatial data 
              has been automatically calculated based on the latittude and longitude 
              coordinates of each Location, and will not change over time unless 
              camera lines change, or the underlying methodology changes."
           )
           
           output_data$fields <- c("locality", 
                                   "lines",
                                   "locations",
                                   "encompassed_area_ha", 
                                   "camera_density_ha", 
                                   "mean_location_pair_spacing",
                                   "location_pair_exceptions") #"camera_rest_interval"
           
         },
         
         
         "deployments_overview" = {
           output_data$title <- "Deployments summary"
           output_data$caption <- sprintf(
              "Table summarising deployments included in the deployment selection 
              period, grouped by %s.", str_to_title(formatted_table_view))
           
           switch(table_view,
                  "locality" = {
                    output_data$fields <- c("locality", 
                                            "lines", 
                                            "locations", 
                                            "deployments", 
                                            "camera_hours", 
                                            "animal_detections", 
                                            "mean_detection_interval",
                                            "blank_detections_percentage")
                    
                  },
                  "line" = {
                    output_data$fields <- c("locality", 
                                            "line", 
                                            "locations", 
                                            "deployments", 
                                            "camera_hours", 
                                            "animal_detections", 
                                            "mean_detection_interval",
                                            "blank_detections_percentage")
                    
                  },
                  "location" = {
                    output_data$fields <- c("locality", 
                                            "line", 
                                            "locationName",
                                            "deployments", 
                                            "camera_hours", 
                                            "animal_detections", 
                                            "mean_detection_interval",
                                            "blank_detections_percentage")
                    
                  }
           )
         },
         
         
         "observations_overview" = {
           output_data$title <- "Observations summary"
           output_data$caption <- sprintf(
              "Table summarising observations relating to deployments in the deployment 
              selection period, grouped by %s.", str_to_title(formatted_table_view))
           
           spp_class_names <- names(config$globals$spp_classes)
           
           # Prefixing each element of the names_list
           spp_count_fields <- paste0("individuals_count_", spp_class_names)
           
           switch(table_view,
                  "locality" = {
                    output_data$fields <- c("locality", 
                                            "unique_species_count",
                                            "individuals_count",
                                            spp_count_fields)
                    
                  },
                  
                  "line" = {
                    output_data$fields <- c("locality",
                                            "line",
                                            "unique_species_count",
                                            "individuals_count",
                                            spp_count_fields)
                    
                  },
                  
                  "location" = {
                    output_data$fields <- c("locality",
                                            "line",
                                            "locationName",
                                            "unique_species_count",
                                            "individuals_count",
                                            spp_count_fields)
                    
                  }
           )
         },
         
         "spp_summary" = {
           output_data$title <- "Species observations summary"
           output_data$caption <- sprintf(
              "Table summarising species observations, grouped by '%s'. By default 
              this table is arranged by species class, most important at top.", 
              str_to_title(formatted_table_view))
           
           
           switch(table_view,
                  "location" = {
                    output_data$fields <- c("locality",
                                            "line",
                                            "locationName",
                                            config$globals$species_name_type, 
                                            "species_class", 
                                            "individuals_count",
                                            "possible_duplicates_count",
                                            "net_individuals_count")
                    
                  },
                  
                  "line" = {
                    output_data$fields <- c("locality",
                                            "line",
                                            config$globals$species_name_type, 
                                            "species_class", 
                                            "individuals_count",
                                            "possible_duplicates_count",
                                            "net_individuals_count")
                    
                    if (config$globals$rai_net_count) {
                      output_data$fields <- c(output_data$fields, "RAI_net")
                      output_data$footnote <- paste(
                        "The RAI figures have been calculated based on Net Individuals 
                        Count (i.e. excluding Possible Duplicates)<br><br>"
                      )
                    } else {
                      output_data$fields <- c(output_data$fields, "RAI")
                      output_data$footnote <- paste(
                        "All RAI calculations are based on Individuals Count, 
                        <strong>including</strong> Possible Duplicates."
                      )
                    }
                    
                    if (config$globals$spp_summary_rm_zeros$line) {
                      output_data$footnote <-  paste(
                          output_data$footnote, 
                          "For the purpose of calculating accurate standard error (SE) 
                          figures to accompany mean RAI calculations, we create a '0' 
                          count record for every species on every Line that contained 
                          no observations of that species. However, the configuration 
                          option to remove records where the Individuals Count is 0 
                          (after performing SE calcualtions) is enabled for this table. 
                          You will only see a record if there were observations of that 
                          species on that Line within that Locality."
                      )
                    }
                    
                  },
                  
                  "locality" = {
                    output_data$fields <- c("locality",
                                            config$globals$species_name_type, 
                                            "species_class", 
                                            "individuals_count",
                                            "possible_duplicates_count",
                                            "net_individuals_count")
                    
                    if (config$globals$rai_net_count) {
                      output_data$fields <- c(output_data$fields, "mRAI_SE_net")
                      output_data$footnote <-  paste(
                        "All RAI calculations are based on Net Individuals Count 
                        (i.e. excluding Possible Duplicates).<br><br>"
                      )
                    } else {
                      output_data$fields <- c(output_data$fields, "mRAI_SE")
                      output_data$footnote <- paste(
                        "All RAI calculations are based on Individuals Count,
                        <strong>including</strong> Possible Duplicates.<br><br>"
                      )
                    }
                    
                    if (config$globals$spp_summary_rm_zeros$locality) {
                      output_data$footnote <- paste(
                        output_data$footnote, 
                        "For the purpose of calculating accurate standard error (SE) 
                        figures to accompany mean RAI calculations, we create a '0' 
                        count record for every species on every Line that contained 
                        no observations of that species. This Locality view is an 
                        aggreation of the Line data, and would show these '0' entries. 
                        However, the configuration option to remove records where the 
                        Individuals Count is 0 (after performing SE calcualtions) is 
                        enabled for this table. You will only see a record if there 
                        were observations of that species in that Locality."
                      )
                    }
                    
                  },

                  "network" = {
                    
                    output_data$caption <- paste(
                      "Table summarising species observations across the entire network. 
                      By default this table is arranged by species class, most important at top."
                    )
                    
                    output_data$fields <- c(config$globals$species_name_type, 
                                            "species_class", 
                                            "individuals_count",
                                            "possible_duplicates_count",
                                            "net_individuals_count")
                    
                  }
           )
         }
  )
  
  return(output_data)
}


# Called to setup a datatable, title and footnote for the given 

setup_table_output <- function(output, 
                               table_id = NULL,
                               data = NULL,
                               table_type = NULL,
                               table_order = list()) {
  
  # Common options for all table_type's
  common_options <- list(
    columnDefs = list(list(orderable = TRUE, targets = "_all")),
    order = table_order,
    rowCallback = JS(
      "function(row, data, index) {",
      "  if (data[0] === 'Aggregate') {", 
      "    $(row).addClass('aggregate-row');",
      "  }",
      "}"
    )
  )
  
  # Adjust options based on table_type
  if (!is.null(table_type) && table_type == "paged") {
    dt_options <- modifyList(list(
      searching = TRUE, 
      pageLength = 10, 
      paging = TRUE, 
      lengthChange = TRUE
    ), common_options)
  } else {
    dt_options <- modifyList(list(
      dom = 'tp', 
      searching = FALSE, 
      paging = FALSE, 
      lengthChange = FALSE
    ), common_options)
    
    # Disable ordering explicitly for non-paged tables
    dt_options$columnDefs <- list(list(orderable = FALSE, targets = "_all"))
  }
  
  table_spec <- get_table_specification(table_id)
  
  # Incorporate custom columnDefs from table_spec into dt_options
  if (!is.null(table_spec$columnDefs)) {
    dt_options$columnDefs <- c(dt_options$columnDefs, table_spec$columnDefs)
  }
  
  output[[paste0(table_id, "_title")]] <- renderUI({
    HTML(paste0('<h3>', table_spec$title, '</h3>'))
  })
  

  output[[table_id]] <- DT::renderDataTable({
    logger::log_debug(
      sprintf("setup_table_output() is rendering %s output for %s\n", 
              table_type, table_id)
    )
    
    output_data <- prepare_table_data(data, 
                                      table_id,
                                      fields = table_spec$fields,
                                      column_help = TRUE)
    
    datatable(
      output_data$table_data,
      escape = FALSE,
      caption = table_spec$caption,
      options = dt_options,
      class = 'display',
      rownames = FALSE
    )
  })
  
  output[[paste0(table_id, "_footnote")]] <- renderUI({
    HTML(paste0(
      '<div style="padding-top: 10px;"><small><em>', 
      table_spec$footnote, 
      '</em></small></div>')
    )
  })
  
}

# Similar to above but for a kable instead of datatable
setup_kable_output <- function(table_id = NULL,
                               data = NULL,
                               table_type = NULL,
                               column_spec = list(),
                               heading_level = "##") {
  
  table_attr = "class='short'"
  
  if (!is.null(table_type) && table_type == "long" || table_type == "data-only") {
    table_attr = "class='long'"
  }
  
  table <- get_table_name_and_view(table_id)
  table_name = table$name
  table_view = table$view
  
  table_spec <- get_table_specification(table_id)
  
  output_data <- prepare_table_data(data, 
                                    table_id,
                                    fields = table_spec$fields,
                                    column_help = FALSE)
  
  table_caption <- table_spec$caption
  
  if (table_view != "special" && table_type != "data-only") {
    formatted_title <- paste0(heading_level, " ", table_spec$title)
    cat(formatted_title, "\n\n")
  } else {
    table_caption <- NULL
  }
  
  # Initialize kable
  options(knitr.kable.NA = '')
  table_output <- knitr::kable(output_data$table_data,
                               table.attr = table_attr, 
                               format = "html", 
                               escape = FALSE,
                               caption = table_caption) %>%
    kableExtra::kable_styling()
  
  # Apply column specifications
  if (!is.null(column_spec)) {
    for (cs in seq_along(column_spec)) {
      table_output <- table_output %>%
        kableExtra::column_spec(column_spec[[cs]]$column, extra_css = column_spec[[cs]]$extra_css)
    }
  }
  
  # Add bold styling to the totals row if needed
  last_row_index <- nrow(output_data$table_data)
  
  if ("Locality" %in% names(output_data$table_data) && 
      output_data$table_data[last_row_index, "Locality"] == "Aggregate") {
    
    bold_css <- "font-weight: bold; border-top: 2px solid rgba(121, 161, 47, 0.3);"
    
    table_output <- table_output %>%
      kableExtra::row_spec(
        row = last_row_index, 
        extra_css = bold_css
      )
  }
  
  if (table_view == "special") {
    # invisible() so it does not output to console
    invisible(htmltools::HTML(table_output))
  } else {
    table_output <- HTML(table_output)
    print(table_output)
    
    
    if (!is.null(table_spec$footnote) && (table_type != "data-only")) { 
      cat("<small><em>", table_spec$footnote, "</em></small><br>\n") 
    }
  }
}



# Selects the relevant table_data and applies any last minute transformations to the data
# Including updating and formatting column headings and data for final output
prepare_table_data <- function(data,
                               table_id = NULL,
                               fields = NULL,
                               column_help = FALSE) {

  # Initialize output_data
  output_data <- list()
  
  # If species_class is in our data, and the fields we want to select including ALL fields
  if ("species_class" %in% names(data) && ("species_class" %in% fields || fields[1] == "ALL")) {
    # Convert species class to a factor so data effectively is 'grouped' by species class
    specific_order <- c(names(config$globals$spp_classes), config$globals$spp_class_unclassified)
    
    # Order by species_class then alphabetically by name (scientific or vernacular per globals config)
    data <- data %>%
      mutate(species_class = factor(species_class, levels = specific_order)) %>%
      arrange(species_class, .data[[config$globals$species_name_type]])
  }
  
  if(length(fields) == 1 || fields[1] == "ALL") {
    selected_data <- data
  } else {
    selected_data <- data %>% 
      select(all_of(fields))
  }
  
  if ("observationID" %in% names(data) && "observationID" %in% fields) {
    project_id <- data_package$project$id
    truncate_uuid <- TRUE
    #action_type <- "edit_sequence|modal" 
    action_type <- "view_sequence|modal"
    
    # Pre-fetch observation-to-sequence mappings to avoid repeated filtering
    observation_to_sequence <- core_data$obs %>%
      select(observationID, sequenceID) %>%
      distinct() %>%
      filter(observationID %in% selected_data$observationID)
    
    # Convert to a named vector for fast lookup
    sequence_lookup <- setNames(observation_to_sequence$sequenceID, 
                                observation_to_sequence$observationID)
    
    # Define the hyperlink format based on action_type
    hyperlink_format <- if (startsWith(action_type, "edit_sequence")) {
      "<a href='https://www.agouti.eu/#/project/%s/annotate/sequence/%s' 
     target='_blank' class='observation-link' 
     data-observationid='%s' data-sequenceid='%s' 
     data-action-type='%s'>%s</a>"
    } else if (startsWith(action_type, "view_sequence")) {
      "<a href='javascript:void(0);' class='observation-link' 
     data-observationid='%s' data-action-type='%s'>%s</a>"
    } else {
      stop("Invalid action_type specified.")
    }
    
    # Generate links using sapply
    selected_data$observationID <- sapply(selected_data$observationID, function(id) {
      sequence_id <- sequence_lookup[[id]]  # Fast lookup for sequenceID
      display_id <- if (truncate_uuid) paste0(substr(id, 1, 8), "...") else id
      
      if (startsWith(action_type, "edit_sequence")) {
        sprintf(hyperlink_format, 
                project_id, sequence_id, id, sequence_id, action_type, display_id)
      } else {
        sprintf(hyperlink_format, id, action_type, display_id)
      }
    })
  }
  
  output_data$table_data <- format_fieldnames(selected_data)
  
  if (column_help) {
    final_colnames <- colnames(output_data$table_data)
    enhanced_columns <- enhance_colnames_with_column_help(final_colnames, table_id)
    colnames(output_data$table_data) <- enhanced_columns
  }
  
  
  return(output_data)
}


prepare_spec_table_data <- function(data,
                                    table_id,
                                    column_help = FALSE) {

  table_spec <- get_table_specification(table_id)

  prepare_table_data(
    data = data,
    table_id = table_id,
    fields = table_spec$fields,
    column_help = column_help
  )
}


# This forms and formats the final field names, including any species field headings
format_fieldnames <- function(data) {
  
  date_columns <- list("start", "end", "timestamp")

  columns_to_round <- get_columns_to_round()
  
  # Loop through each specified date column name
  for (col_name in date_columns) {
    if (col_name %in% names(data)) {
      # Convert the column to POSIXct with the specified format, they will be chr strings
      data[[col_name]] <- format(
        as.POSIXct(data[[col_name]], tz = config$globals$timezone), 
        format = "%Y-%m-%d %H:%M:%S"
      )
    }
  }
  
  
  # Apply specific rounding based on complete column name matches
  # Have to do it down here so it includes the totals row
  for (col_name in names(columns_to_round)) {
    if (col_name %in% names(data)) {
      data[[col_name]] <- round(data[[col_name]], columns_to_round[[col_name]])
    }
  }
  
  
  # This actually does the formatting of the column name. Looks for edge cases, then
  # splits field name up and applies str_to_title() to each word
  format_column_name <- function(name) {
    
    # Check if the column name matches the pattern for special formatting
    if (grepl("^individuals_count_", name)) {
      # Manually change the order of the words in heading for species_counts
      parts <- unlist(strsplit(name, "_"))
      new_name <- paste(c(parts[3:length(parts)], parts[1:2]), collapse = " ")
      formatted_name <- str_to_title(new_name)
    } else {
      # Remove underscore and apply str_to_title to all other column headings
      formatted_name <- str_to_title(str_replace_all(name, "_", " "))
    }
    
    return(formatted_name)
  }
  
  # Identify columns in data that exist in config$globals$column_renames
  columns_to_rename_override <- intersect(names(data), names(config$globals$column_renames))
  
  # Create a list of renamed columns from columns_to_rename_override so we don't re-rename them
  renamed_columns <- sapply(columns_to_rename_override, function(col) config$globals$column_renames[[col]])
  
  # Rename columns in data based on config$globals$column_renames
  for (col in columns_to_rename_override) {
    new_col_name <- config$globals$column_renames[[col]]
    names(data)[names(data) == col] <- new_col_name
  }
  
  # Apply format_column_name to all other columns in data (excluding the renamed columns_to_rename_override)
  columns_to_format <- setdiff(names(data), renamed_columns)
  new_column_names <- sapply(columns_to_format, format_column_name)
  names(data)[names(data) %in% columns_to_format] <- new_column_names
  
  return(data)
}





# Wrapper function to update final_colnames with version enhanced with html help link
enhance_colnames_with_column_help <- function(final_colnames, table_id) {
  column_html <- generate_colnames_with_html(final_colnames, table_id)
  updated_colnames <- update_column_names(final_colnames, column_html)
  
  return (updated_colnames)
}



# Takes the column names and enhances them with the column_html generated by 
# helper function generate_column_info_link 
generate_colnames_with_html <- function(final_colnames, table_id) {
  
  generate_column_info_link <- function(field, table_id) {
    btn_id <- paste("info_btn", table_id, field, sep = "_")
    # Update the column_html to include a console.log statement for debugging
    column_html <- sprintf(
      "<i class=\"fa fa-info-circle\" 
      onclick=\"customInfoButtonClickHandler('%s', '%s')\" style=\"cursor: pointer;\"></i>",
      field, btn_id
    )
    
    return(column_html)
  }
  
  column_html <- sapply(final_colnames, function(field) generate_column_info_link(field, table_id))
  names(column_html) <- final_colnames
  return(column_html)
}

# Updates the column names with the enhanced html version
update_column_names <- function(col_names, column_html) {
  updated_colnames <- sapply(col_names, function(colname) {
    if (colname %in% names(column_html)) {
      # Split the column name into words
      words <- strsplit(colname, " ")[[1]]
      
      # Combine all but the last word
      main_text <- paste(words[-length(words)], collapse = " ")
      
      # Combine the last word with the icon, preventing wrapping
      last_word_with_icon <- paste0("<span style='white-space: nowrap;'>", 
                                    words[length(words)], 
                                    " ", column_html[[colname]], "</span>")
      
      # Combine everything
      paste0(main_text, " ", last_word_with_icon)
    } else {
      colname
    }
  })
  return(updated_colnames)
}

# Takes a table_id and breaks it into table_view and table_name
get_table_name_and_view <- function(table_id) {
  # Split the string by underscore
  parts <- unlist(strsplit(table_id, "_", fixed = TRUE))
  
  # The table_view is the first part of the split
  table_view <- parts[1]
  
  # The table_name is the rest of the parts joined back with underscore
  table_name <- paste(parts[-1], collapse = "_")
  
  return(list(view = table_view, name = table_name))
}






# Returns the column description of column_name if specified, all column descriptions if non column_name specified, and
# NULL if a column_name is specified that doesn't exist.
get_column_description <- function(column_name = NULL) {

column_descriptions <- list(
  "ObID" = "
    Observation ID, the unique identifier for the observation record.<br><br>

    This identifier traces the observation back to its source data, revealing all details, including
    the image sequence that generated the observation. Note: May be truncated in some views due to space.
  ",

  "Deployment ID" = "
    The unique identifier for the deployment record.<br><br>

    This identifier traces a deployment back to its source data, showing all details such as the Location
    and camera setup.
  ",


  "Start" = "
    The start date of the deployment, indicated by the first 'setup' images taken at the time of camera deployment.<br><br>

    The start date is based on the camera's local time set at the start of each deployment.
  ",

  "End" = "
    The end date of the deployment, indicated by the last 'setup' images taken when the camera is retrieved.<br><br>

    The end date is based on the camera's local time set at the start of each deployment.
  ",

  "Locality" = "
    The general geographical location where cameras are deployed, consisting of multiple predefined
    camera lines strategically positioned according to the methodology.
  ",

  "Line" = "
    A camera line, being a grouping of cameras, spaced at predefined Locations in a line in accordance
    with the methodology.
  ",

  "Lines" = "
    A Line (camera line) is a grouping of cameras, spaced at predefined Locations in a line in accordance
    with the methodology. Lines (plural) refers to the number of Lines in a Locality.
  ",

    "Location" = "
    A predefined point on a Line where a camera is deployed, identified by latitude and longitude coordinates.
  ",

  "Locations" = "
    A Location is a predefined point on a Line where a camera is deployed, identified by latitude and longitude coordinates.
    Locations (plural) refers to the number of Locations in the Locality or on the Line, depending on context.
  ",

  "Timestamp" = "
    Usually refers to the date and time of the first camera image in the sequence that contained the observed species.<br><br>
    The timestamp is based on the camera time and time. The camera is set to local time/time at the start of every deployment.
  ",

  "Deployments" = "
    A camera installed at a Location for the observation period, in accordance with the methodology.<br><br>
    <em>(The number of Deployments will always equal the number of Locations, unless your deployment selection criteria
    includes multiple deployments to a Location e.g. 'ALL' deployments).</em>
  ",

  "Animal Detections" = "
    The number of times the camera's motion detection was triggered by an animal of any type during the deployment.
  ",

  "Count" = "
    The number of individual's of the species observed. Some detections show several individual
    animals of the same species in a single frame, so the count is not always 1.
  ",

  "Species" = "
    The name of the species observed, in scientific or common vernacular, depending on the settings.
  ",

  "Scientific Name" = "
    The scientific name of the species observed, with a matching entry on
    <a href=\"https://www.catalogueoflife.org/\" target=\"_blank\">https://www.catalogueoflife.org</a>.
  ",

  "Common Name" = "
    The vernacular name of the species, based on information in the data package. Common names can vary based on language and region.
  ",

  "Camera Hours" = "
    The number of hours a camera was deployed in the field for during the deployment selection period.<br><br>
    In the 'Locality' or 'Line' grouping summaries, it is the combined total for all cameras in the Locality or on the Line.
    Rounded to the nearest hour for output.
  ",

  "Detection Interval" = "
    The mean number of hours between each animal detection. Calculation: Camera hours / Animal detections.
  ",

  "Species Class" = sprintf("
    The classification assigned to a species by the project, used by InsightfulKiwi to ensure data of most
    interest is highlighted. Species in a named class are collectively referred to as 'important species'.
    The named classes are:<br><br>

    <em>target:</em> Species specifically targetted by the protocol.<br>
    <em>interesting:</em> Non-target species of most interest<br>
    <em>other:</em> Not a target species nor as interesting, but still likely to be useful data.
    <br><br>

    Any species not falling into a named class is labelled as %s.
  ", config$globals$spp_class_unclassified),


  "Total Individuals" = "
    The total number of individuals observed. A single Animal Detection may show many different
    individuals in frame; this is a count of the total individuals.
  ",

  "Net Count" = "
    The total number of individuals of that species observed, minus the possible duplicate observations
    of the species.
  ",

  "Dup Count" = sprintf("
    Short for possible duplicate count. An observation is considered a possible duplicate if it matches the same species,
    count, and life stage as another observation within the previous %s minutes at the same Location. These are legitimate
    observations based on the data, but may be candidates for removal from RAI calculations or other analyses, depending on
    the methodology of the monitoring program.<br><br>

    The threshold (%s minutes, decimal) is defined globally in the configuration. It can be calibrated for the project based on the
    composition of camera lines, camera rest interval, and behaviours of target species. The same threshold is applied to all species.
  ", config$globals$dup_detect_threshold, config$globals$dup_detect_threshold),

  # See "Possible Duplicate" as well, at the end. Possible duplicates is at the summary level
  # Possible Duplicate is at the indivudal observation level

  "Possible Duplicates %" = sprintf("
    The percentage of the Animal Count (or Total Count if in a species view) that are possible duplicates based
    on the Possible Duplicates logic.<br><br>

    An observation is considered a possible duplicate if it matches the same species, count, and life stage as
    another observation within the previous %s minutes at the same Location.
  ", config$globals$dup_detect_threshold),


  "Blank Observations" = "
    Blank observations are where camera detection was triggered, but no species was observed. This is usually due
    to wind causing foliage to move, or causing trees to sway and the resulting shadows triggering detection.
    A high level of blank observations could be a reason to reposition a camera.
  ",

  "RAI" = sprintf("
    The Relative Abundance Index (RAI) provides a standardised measure of species detections within a Line,
    facilitating comparisons over time and across Lines. However, it does not directly estimate population size and may
    not be comparable between species due to differences in detection probability.

    This calculation is based on on Individuals Count, divided by the total camera hours for all locations on the line,
    normalised to %s camera hours.<br><br>

    The RAI figures for each Line are the basis for calculating the standard error of the mean RAI for the Locality.
  ", config$globals$rai_norm_hours),

  "RAI (Net)" = sprintf("
    The Relative Abundance Index (RAI) provides a standardised measure of species detections within a Line,
    facilitating comparisons over time and across Lines. However, it does not directly estimate population size and may
    not be comparable between species due to differences in detection probability.

    This calculation is based on on Net Individuals Count (excluding possible duplicates), divided by the total camera hours
    for all locations on the line, normalised to %s camera hours. <br><br>

    The RAI figures for each Line are the basis for calculating the standard error of the mean RAI for the Locality.
  ", config$globals$rai_norm_hours),

  "RAI ± SE" = sprintf("
    RAI ± SE: The Relative Abundance Index (RAI) provides a standardised measure of species detections within a Locality,
    facilitating comparisons over time and across Localities. However, it does not directly estimate population size and may
    not be comparable between species due to differences in detection probability.

    The ± SE (standard error) is calculated as the standard deviation of RAI across Lines within a Locality, divided by
    the square root of the number of Lines. This reflects the variability in detection rates within a Locality.

    RAI values are standardised to %s camera hours to enable more consistent comparisons across deployments.<br><br>

    The accuracy of these metrics in representing species trends depends on the monitoring methodology and its consistent application.
    Key methodological factors include target species behavior, camera line and camera positioning, deployment duration, and sampling effort.
  ", config$globals$rai_norm_hours),

  "Network RAI ± SE" = "
    The Network RAI (Relative Abundance Index) provides a standardised measure of species detections across the entire project,
    facilitating comparisons over time. However, it does not directly estimate population size and may
    not be comparable between species due to differences in detection probability.<b,r><br>

    The ± SE calculations are based on RAI figures for the species in each Locality.<br><br>

    The Network RAI ± SE data is intended to be a quick glance into species detections. The RAI ± SE data for each Locality is
    more relevant to measure the results of control programs, which are normally organised within the Locality.
  ",

  "Coverage Area (ha)" = "
    This is the area (in hectares) <strong><em>encompassed by the outermost Locations (cameras)</em></strong>
    within the Locality. It has been measured solely based on latitude and longitude coordinates of each camera Location,
    factoring in the curvature of Earth. This calculated coverage area does not factor in any of the area beyond the virtual
    border defined by the outer locations of cameras in the Locality, and that area is likely to be considered part of the
    catchment area to some extent.
  ",

  "Camera Density (per hectare)" = "
    The average number of cameras per hectare in the Locality, based on the Coverage Area.
    Calculation: Locations divided by Coverage Area. Does not account for the distribution of the cameras within the Locality.
  ",

  "Mean Location Pair Spacing" = "
    This is the mean distance (in meters) between locations on different lines. Calculation: From any
    given location, there is a distance to every other location. We exclude distances to locations on the same line, but calculate
    all of the others for every line, excluding duplicate pairings. This is the mean (or average) resulting from those calculations.
  ",

  "Location Pair Exceptions" = sprintf("
    The number of location-pairs (being locations on different lines) that are within %s meters of each other.
  ", config$globals$min_distance_threshold),

  "Camera Rest Interval" = "
    After motion detection triggers, this is the amount of time (in seconds) each camera is configured to disable motion detection for,
    before being ready to trigger again.
  ",

  "Unique Species" = "
    The number of different types of species observed. The aggregate unique species is the number of unique species observed
    across all localities.
  ",

  "Blanks %" = "
    The percentage of total detections that contained no animal observations.<br><br>

    Each time detection is triggered, the camera rest interval begins, during which time nothing can be detected, including animals.
    High levels of blank detections should be investigated and resolved to reduce the chance of missing animal detections.
  ",

  "Blanks" = "
    The number of detections that contained no animal observations.<br><br>

    Each time detection is triggered, the camera rest interval begins, during which time nothing can be detected, including animals.
    High levels of blank detections should be investigated and resolved to reduce the chance of missing animal detections.
  ",

  "About InsightfulKiwi" = sprintf("
          InsightfulKiwi provides insights into data collected from wildlife camera monitoring
          programs using the Camera Trap Data Packages (Camtrap DP) format.<br><br>

          For more information about InsightfulKiwi, contact
         <a href='mailto:blair@aketechnology.co.nz'>blair@aketechnology.co.nz</a>.<br><br>

          The data loaded here was collected by and remains property of %s", config$meta$organisation_name)

)


  # Possible Duplicate references "Possible Duplicates" in the list above
  # Possible Duplicate is the description shown if user is looking at a view showing individual observations

column_descriptions[["Dup?"]] <- paste0(
  column_descriptions[["Dup Count"]],  # Existing description from "Possible Duplicates"
  "<br><br>
  If the data looks incorrect, it is likely because you can't see the life stage,
  which may only be recorded when obvious, which is infrequently."  # Additional information
)


  if (is.null(column_name)) {
    return(column_descriptions)
  }
  else if (column_name %in% names(column_descriptions)) {
    return(column_descriptions[[column_name]])
  } else {
    return(NULL)
  }

}
