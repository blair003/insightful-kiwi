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
           
           # Initialize an empty list to hold class lists with links
           classes_list_with_links <- list()
           
           # Loop through each class in names_list to generate linked species names
           for (i in seq_along(names_list)) {
             class_name <- names_list[i]
             # Generate linked species names for each class
             classes_list_with_links[[class_name]] <- generate_linked_species_names(config$globals$spp_classes[[i]],
                                                                                    core_data$taxonomic,
                                                                                    config$globals$species_name_type)
           }
           
           # Construct the title and caption
           output_data$title = "Mean RAI ± SE: important species"
           
           output_data$caption = sprintf("Table showing species mean RAI ± SE per %s camera hours by Locality, for each species in a named class (%s).
                                          If a species in a named class is absent from this table, it means there were no observations of that species 
                                         in any Locality during the deployment selection period.", 
                                         config$globals$rai_norm_hours , all_classes_list)
           
           # Dynamically construct the footnote based on the actual names and their lists
           footnote_parts <- sapply(names(classes_list_with_links), function(name) {
             sprintf("Full <strong>%s</strong> species list: %s", name, classes_list_with_links[[name]])
           }, USE.NAMES = FALSE)
           
           footnote_parts_combined = paste(footnote_parts, collapse = ".<br>")
           
           if (config$globals$rai_net_count) {
             output_data$footnote = "The RAI ± SE figures have been calcualted based on Net Individuals Count (i.e. excluding Possible Duplicates)<br><br>"
           }
           
           output_data$footnote = paste(output_data$footnote, footnote_parts_combined)
           
           output_data$fields <- c("ALL")
         },  
         
         
         "deployments_browse" = {
           output_data$title = "Deployments browse"
           output_data$caption = "Table showing deployments included in the deployment selection. Default sorting is by start date, which 
                         shows the order in which they were deployed."
           
           
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
                    output_data$caption = "Table showing observations during the deployment selection."
                    
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
                    output_data$caption = "Table showing all observation records from the entire project."
                    
                    output_data$fields <- c("locality", 
                                            "line", 
                                            "locationName", 
                                            "period",
                                            "timestamp",
                                            config$globals$species_name_type,
                                            "count",
                                            "possible_duplicate",
                                            "observationID",
                                            "sequenceID")
                    
                  }
           )
           
           
           
           #      output_data$columnDefs <- list(
           #         list(targets = 3, className = "nowrap"),   # "Timestamp" with no wrap
           #         list(targets = 5, className = "nowrap"),   # Output Species name with no wrap
           #         list(targets = 6, className = "wrap", width = "10%")      # "Possible Duplicate" wrap
           #       )
           
         },
         
         
         "observation_viewer" = {
           output_data$title = "Observations browse"
           output_data$caption = NULL
           output_data$fields <- c("locality", "line", "locationName", "timestamp", "count", config$globals$species_name_type, "comments")
           
         },
         
         
         "camera_monitoring_network_overview" = {
           output_data$title = "Spatial overview of the network"
           output_data$caption = "Table showing an overview including spatial data for each Locality that was active during the 
           deployment selection period. Spatial data has been automatically calculated based on the latittude and longitude coordinates of 
           each Location, and will not change over time unless camera lines change, or the underlying methodology changes."
           
           output_data$fields <- c("locality", 
                                   "lines",
                                   "locations",
                                   "encompassed_area_ha", 
                                   "camera_density_ha", 
                                   "mean_location_pair_spacing",
                                   "location_pair_exceptions") #"camera_rest_interval"
           
         },
         
         
         "deployments_overview" = {
           
           #output_data$title = sprintf("Deployments summary, by %s", str_to_title(formatted_table_view))
           output_data$title = "Deployments summary"
           output_data$caption = sprintf("Table summarising deployments included in the deployment selection 
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
           
           output_data$title = "Observations summary"
           output_data$caption = sprintf("Table summarising observations relating to deployments in the deployment 
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
           #  output_data$title = sprintf("Species observations summary, by %s", str_to_title(formatted_table_view))
           output_data$title = "Species observations summary"
           output_data$caption = sprintf("Table summarising species observations, grouped by '%s'. By default this
           table is arranged by species class, most important at top.", str_to_title(formatted_table_view))
           
           
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
                      output_data$footnote <-  "The RAI figures have been calculated based on Net Individuals Count (i.e. excluding Possible Duplicates)<br><br>"
                    } else {
                      output_data$fields <- c(output_data$fields, "RAI")
                      output_data$footnote <-  sprintf("All RAI calculations are based on Individuals Count, <strong>including</strong> Possible Duplicates.")
                    }
                    
                    if (config$globals$spp_summary_rm_zeros$line) {
                      output_data$footnote <-  paste(output_data$footnote, "For the purpose of calculating accurate standard error (SE) figures to accompany mean RAI calculations, 
                      we create a '0' count record for every species on every Line that contained no observations of that species. 
                      However, the configuration option to remove records where the Individuals Count is 0 (after performing SE calcualtions) 
                      is enabled for this table. You will only see a record if there were observations of that species on that Line within that Locality.")
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
                      output_data$footnote <-  sprintf("All RAI calculations are based on Net Individuals Count (i.e. excluding Possible Duplicates).")
                    } else {
                      output_data$fields <- c(output_data$fields, "mRAI_SE")
                      output_data$footnote <-  sprintf("All RAI calculations are based on Individuals Count, <strong>including</strong> Possible Duplicates.")
                    }
                    
                    if (config$globals$spp_summary_rm_zeros$locality) {
                      output_data$footnote <-   paste(output_data$footnote, "For the purpose of calculating accurate standard error (SE) figures 
                      to accompany mean RAI calculations, we create a '0' count record for every species on every Line that contained no observations of 
                      that species. This Locality view is an aggreation of the Line data, and would show these '0' entries. However, the configuration 
                      option to remove records where the Individuals Count is 0 (after performing SE calcualtions) is enabled for this table. You will 
                      only see a record if there were observations of that species in that Locality.")
                    }
                    
                  },
                  # Not currenty showing Network RAI ± SE (mmRAI_SE) , possibly too confusing and Locality data better
                  "network" = {
                    
                    output_data$caption = "Table summarising species observations across the entire network. By default this
                     table is arranged by species class, most important at top."
                    output_data$fields <- c(config$globals$species_name_type, 
                                            "species_class", 
                                            "individuals_count",
                                            "possible_duplicates_count",
                                            "net_individuals_count")
                    
                    # output_data$footnote <- "NOTE: The Network RAI ± SE is the mean RAI of the species across the entire camera monitoring
                    # network (all Localities), with the ± SE data being calculated from the mean RAI data for each Locality."
                    
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
    logger::log_debug(sprintf("setup_table_output() is rendering %s output for %s\n", 
                              table_type, table_id))
    
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
    HTML(paste0('<div style="padding-top: 10px;"><small><em>', table_spec$footnote, '</em></small></div>'))
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
  
  if ("Locality" %in% names(output_data$table_data) && output_data$table_data[last_row_index, "Locality"] == "Aggregate") {
    table_output <- table_output %>%
      kableExtra::row_spec(row = last_row_index, extra_css = "font-weight: bold; border-top: 2px solid rgba(121, 161, 47, 0.3);")
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
    action_type <- "edit_sequence|modal" 
    #action_type <- "view_sequence"
    
    # Pre-fetch observation-to-sequence mappings to avoid repeated filtering
    observation_to_sequence <- core_data$obs %>%
      select(observationID, sequenceID) %>%
      distinct() %>%
      filter(observationID %in% selected_data$observationID)
    
    # Convert to a named vector for fast lookup
    sequence_lookup <- setNames(observation_to_sequence$sequenceID, observation_to_sequence$observationID)
    
    # Define the hyperlink format based on action_type
    hyperlink_format <- if (startsWith(action_type, "edit_sequence")) {
      "<a href='https://www.agouti.eu/#/project/%s/annotate/sequence/%s' target='_blank' class='observation-link' data-observationid='%s' data-sequenceid='%s' data-action-type='%s'>%s</a>"
    } else if (startsWith(action_type, "view_sequence")) {
      "<a href='javascript:void(0);' class='observation-link' data-observationid='%s' data-action-type='%s'>%s</a>"
    } else {
      stop("Invalid action_type specified.")
    }
    
    # Generate links using sapply
    selected_data$observationID <- sapply(selected_data$observationID, function(id) {
      sequence_id <- sequence_lookup[[id]]  # Fast lookup for sequenceID
      display_id <- if (truncate_uuid) paste0(substr(id, 1, 8), "...") else id
      
      if (startsWith(action_type, "edit_sequence")) {
        sprintf(hyperlink_format, project_id, sequence_id, id, sequence_id, action_type, display_id)
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


# This forms and formats the final field names, including any species field headings
format_fieldnames <- function(data) {
  
  date_columns <- list("start", "end", "timestamp")
  
  # Moved to global.R  
  # source("includes/static_column_rename_override.R")
  # source("includes/columns_to_round.R")
  columns_to_round <- get_columns_to_round()
  
  # Loop through each specified date column name
  for (col_name in date_columns) {
    if (col_name %in% names(data)) {
      # Convert the column to POSIXct with the specified format, they will be chr strings
      data[[col_name]] <- format(as.POSIXct(data[[col_name]], tz = config$globals$timezone), format = "%Y-%m-%d %H:%M:%S")
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
      formatted_name <- str_replace_all(name, "_", " ")
      words <- str_split(formatted_name, " ")[[1]]
      formatted_words <- sapply(words, function(word) {
        if (toupper(word) != word) {
          str_to_title(word)
        } else {
          word
        }
      })
      formatted_name <- paste(formatted_words, collapse = " ")
    }
    
    return(formatted_name)
  }
  
  # Identify columns in data that exist in static_column_rename_override
  columns_to_rename_override <- intersect(names(data), names(static_column_rename_override))
  
  # Create a list of renamed columns from columns_to_rename_override so we don't re-rename them
  renamed_columns <- sapply(columns_to_rename_override, function(col) static_column_rename_override[[col]])
  
  # Rename columns in data based on static_column_rename_override
  for (col in columns_to_rename_override) {
    new_col_name <- static_column_rename_override[[col]]
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



# Takes the column names and enhances them with the column_html generated by helper function generate_column_info_link 
generate_colnames_with_html <- function(final_colnames, table_id) {
  
  generate_column_info_link <- function(field, table_id) {
    btn_id <- paste("info_btn", table_id, field, sep = "_")
    # Update the column_html to include a console.log statement for debugging
    column_html <- sprintf(
      "<i class=\"fa fa-info-circle\" onclick=\"customInfoButtonClickHandler('%s', '%s')\" style=\"cursor: pointer;\"></i>",
      field, btn_id  # Passing both field and btn_id to the JavaScript function
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






