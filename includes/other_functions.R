

generate_select_input <- function(id, choices, selected, multiple = TRUE, label = "Species selection:") {
  selectizeInput(
    inputId = id,
    label = tagList(icon("paw"), label),  # Label with icon
    choices = choices,  # Choices passed as a parameter
    selected = selected,  # Dynamically selected species
    multiple = multiple,  # Allow multiple selections if TRUE
    options = list(
      placeholder = "Select species...",  # Placeholder text
      closeAfterSelect = TRUE  # Close dropdown after each selection
    )
  )
}



get_taxonID_urls <- function(scientificNames, taxonomic = NULL) {

  # Initialize an empty list to store the URLs
  urls <- setNames(vector("list", length(scientificNames)), scientificNames)
  
  # Iterate over scientificNames
  for (name in scientificNames) {
    # Search for the name in the taxonomic data
    for (taxon in taxonomic) {
      if (taxon$scientificName == name) {
        # Assign the URL to the corresponding name in the list
        urls[[name]] <- taxon$taxonID
        break # Stop looking once we find the first match
      }
    }
    # Assign NA if no URL is found
    if (is.null(urls[[name]])) {
      urls[[name]] <- NA
    }
  }
  
  return(urls)
}


generate_linked_species_names <- function(scientificNames, taxonomic = NULL, species_name_type) {

  urls <- get_taxonID_urls(scientificNames, taxonomic)
  
  links <- mapply(function(name, url) {
    display_name <- name  # Default to scientific name
    
    if (species_name_type == "vernacularNames.eng") {
      taxon <- Filter(function(taxon) taxon$scientificName == name, taxonomic)
      
      if (length(taxon) > 0 && !is.null(taxon[[1]]$vernacularNames) && !is.null(taxon[[1]]$vernacularNames$eng)) {
        display_name <- taxon[[1]]$vernacularNames$eng  # Corrected access to vernacularNames.eng
      }
    }
    
    if (!is.na(url)) {
      sprintf('<a href="%s" target="_blank">%s</a>', url, display_name)
    } else {
      display_name
    }
  }, names(urls), urls, SIMPLIFY = FALSE)
  
  return(paste(links, collapse = ", "))
}




# Return a list of lists, with each item containing filePath and fileName
get_sequence_media_urls <- function(media) {

  # Initialize an empty list to store URLs and file names
  sequence_media_info <- list()
  
  # Limit to up to 3 entries, because we only have 3 images
  n_items <- min(nrow(media), 3)
  
  if (n_items > 0) {
    
    # Loop through each entry
    for (i in 1:n_items) {
      # Append filePath (URL) and fileName to the list as a named list
      sequence_media_info[[i]] <- list(
        filePath = media$filePath[i],
        fileName = media$fileName[i],
        favourite = media$favourite[i]
      )
    }
  }

  return(sequence_media_info) 
}


# Check local cache for file and download an image from Agouti if we don't have it, reducing load
# on their server over time. Also creates a resized versions of image on download, coping
# extras to /favourites if it is tagged as favourite


# For future call
plan(multisession)

update_image_cache <- function(sequence_media_info) {
  local_cache_dir <- "www/cache/images"
  
  # Define the favourites directories
  favourites_dir <- paste0(local_cache_dir, "/favourites")

  # Loop through each image_info item
  for (image_info in sequence_media_info) {
    favourite <- image_info$favourite
    
    path_components <- unlist(strsplit(image_info$filePath, "/"))
    hash_dir_name <- path_components[length(path_components) - 1]
    image_dir_path <- file.path(local_cache_dir, hash_dir_name)
    
    # Create the directory if it doesn't exist
    if (!dir.exists(image_dir_path)) {
      dir.create(image_dir_path, recursive = TRUE)
    }
    
    local_file_path <- file.path(image_dir_path, image_info$fileName)
    
    # Download and process the image if it doesn't exist
    if (!file.exists(local_file_path)) {
      future({
        tryCatch({
          logger::log_info("Downloading image %s to %s", image_info$filePath, local_file_path)
          download_image(image_info$filePath, local_file_path)
          resized_file_path <- create_resized_image(local_file_path, config$globals$image_resize_width_pixels)
          
          if (!is.character(resized_file_path)) {
            logger::log_error("Resized file path is not a character vector")
            stop("Resized file path is not a character vector.")
          }
          
          # If the image is a favourite, store the resized image in the favourites directories
          if (!is.null(favourite) && favourite == TRUE) {
            
            # Ensure the favourites directories exist
            if (!dir.exists(favourites_dir)) {
              dir.create(favourites_dir, recursive = TRUE)
            }
            
            # Copy additional versions to make other website functionality e.g. gallery simpler
            favourite_file_path <- file.path(favourites_dir, basename(resized_file_path))
            
            file.copy(resized_file_path, favourite_file_path)

          }
        }, error = function(e) {
          logger::log_error("Error in future block: ", e$message)
        })
      }, seed = TRUE) %...>% {
        logger::log_info("Download and processing completed for: ", local_file_path)
      }
    } else {
      logger::log_info("Image exists: %s", local_file_path)
    }
  }
}




# Download image always downloads the image and overwrites, assuming something else 
# has checked regarding existing images
download_image <- function(source_url, destination_file) {
  res <- tryCatch({
    # Attempt to download the image
    download_response <- GET(source_url, write_disk(destination_file, overwrite = TRUE))
    
    # Check if download was successful
    if (!grepl("success", http_status(download_response)$category, ignore.case = TRUE)) {
      logger::log_error("Download did not succeed, HTTP status: ", http_status(download_response)$category)
      stop("Download did not succeed, HTTP status: ", http_status(download_response)$category)
    }
    # Using future
    return(invisible(TRUE))
  }, error = function(e) {
    # If an error occurred (e.g., download did not succeed), delete the partially downloaded file
    if (file.exists(destination_file)) {
      file.remove(destination_file)
    }
    # Optionally, re-throw the error or handle it as needed
    stop(e)
  })
  
  return(res)
}

# Using Magick to create a smaller _resized version of a cached image
create_resized_image <- function(image_file, resized_width) {
  
  image_file_resized <- sub("\\.JPG$", "_resized.JPG", image_file, ignore.case = TRUE)
  
  # Read an image and resize it based on width while maintaining aspect ratio
  image <- magick::image_read(image_file)
  resized_image <- magick::image_resize(image, resized_width)
  
  magick::image_write(resized_image, image_file_resized)
  
  # Return the path of the resized image
  return(image_file_resized)
}


# Use imagemagick to overlay logo onto image. Not using at this stage, but could easily fix
# Need to get different logo versions, struggled with opacity while still overwriting the Browning logo
# Update to work on the resized image only
add_logo <- function(image_file) {
  main_image <- image_read(image_file)
  logo <- image_read("www/wkt-logo.png")
  
  # Adjust logo if necessary and make transparent
  logo <- image_background(logo, "none")
  logo <- image_transparent(logo, "white", fuzz = 10)
  
  # Calculate logo position and add to image
  main_image_size <- image_info(main_image)
  offset_x <- 0
  offset_y <- main_image_size$height - image_info(logo)$height
  composite_image <- image_composite(main_image, logo, offset = paste0("+", offset_x, "+", offset_y), operator = "atop")
  
  image_write(composite_image, image_file)
  
  return(invisible(TRUE))
}

# Show image modal with images, called from server.R after create_observation_viewer_output()
# and 
show_image_modal <- function(observation_id, ui_elements) {
  
  showModal(modalDialog(
    title = sprintf("Obs ID: %s", observation_id),
    ui_elements,
    uiOutput("observation_record_table_modal"),
    size = "l",  # Large modal
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

}




create_observation_images_ui <- function(sequence_media_info, observation_id, context = "pageview") {

  local_cache_dir <- "www/cache/images"  # Directory for cached images
  carousel_id <- paste0("carousel_", observation_id) # Unique ID for the carousel container
  #carousel_id <- "observation_image_viewer_carousel"
  
  loading_placeholder <- paste0("carousel_loading_placeholder_", config$globals$image_resize_width_pixels, ".png") # Path to a loading image, www is assumed
  cache_hits <- vector("logical", length = length(sequence_media_info)) # Prepare cache_hits vector
  
  # Start building the carousel HTML with a placeholder for each image
  carousel_html <- sprintf('<div id="%s" class="slick-carousel">', carousel_id)

  
  # Define CSS based on context
  image_css <- if(context == "modal") {
    "width:100%; height:auto; display:block; margin-left:auto; margin-right:auto;"
  } else {  # context == "pageview"
    paste0("width:100%; max-width:", config$globals$image_resize_width_pixels, "px; max-height:100%; height:auto; display:block; margin-left:auto; margin-right:auto;")
  }
  
  # Placeholder initially, actual images will be loaded dynamically

    carousel_html <- paste0(
      carousel_html, 
      sprintf('<div><img src="%s" alt="Loading..." class="carousel-image-placeholder" style="%s"/></div>', loading_placeholder, image_css)
    )

  # Close the carousel container
  carousel_html <- paste0(carousel_html, "</div>")

  #instructions_html <- "Clickidy click."
  instructions_html <- "Use the forward/back arrows to cycle through images"
  complete_html <- paste(carousel_html, instructions_html, sep = "")
  # Preparing the array of actual image sources (src)
  
  image_sources <- lapply(sequence_media_info, function(image_info, i) {
    path_components <- unlist(strsplit(image_info$filePath, "/"))
    hash_dir_name <- path_components[length(path_components) - 1]
    local_file_system_path <- file.path(local_cache_dir, hash_dir_name, image_info$fileName)
    resized_filename <- sub("\\.JPG$", "_resized.JPG", image_info$fileName, ignore.case = TRUE)
    
    cache_hit <- file.exists(local_file_system_path)
    cache_hits[i] <<- cache_hit
    
    if (cache_hit) {
      return(paste0(paste0(gsub("^www/", "", local_cache_dir), "/"), hash_dir_name, "/", resized_filename))
    } else {
      return(image_info$filePath)
    }
  }, seq_along(sequence_media_info))
  
  image_sources_js_array <- jsonlite::toJSON(image_sources, auto_unbox = TRUE)
  
  
  carousel_js <- sprintf(
    "$(document).ready(function(){
        var imageSources = %s;
        var totalImages = imageSources.length;
        var loadedImagesCount = 0;
        var loadedImages = [];

        // Function to preload images and track loading progress
        function preloadImages(images, callback) {
            images.forEach(function(src, index) {
                var img = new Image();
                img.onload = function() {
                    loadedImagesCount++;
                    loadedImages[index] = '<div><img src=\"' + src + '\" alt=\"Loaded image\" style=\"%s\"></div>';
                    // Check if all images are loaded
                    if(loadedImagesCount === totalImages) {
                        callback(loadedImages); // Call the callback function with loaded images
                    }
                };
                img.src = src;
            });
        }

        // Function to initialize the slider with loaded images
        function initializeSliderWithImages(loadedImagesHtml) {
            var carousel = $('#%s');
            // Remove the initial placeholder
            carousel.empty(); // Or carousel.slick('slickRemove', 0); if already initialized

            // Add loaded images to the carousel and then initialize/slickAdd
            loadedImagesHtml.forEach(function(imgHtml) {
                carousel.append(imgHtml); // Assuming slider not initialized
            });

            // Initialize or reinitialize the slider
            carousel.slick({
                infinite: false,
                slidesToShow: 1,
                adaptiveHeight: true,
                arrows: true,
                dots: true,
                accessibility: true,
                speed: 0
            });

            // Optional: Setup keyboard navigation for the slider
            setupKeyboardNavigation(carousel);
        }

        // Preload images and initialize the slider
        preloadImages(imageSources, initializeSliderWithImages);

      function setupKeyboardNavigation(carousel) {
          $(document).on('keydown', function(e) {
              if (e.key === 'ArrowLeft') {
                  e.preventDefault();
                  carousel.slick('slickPrev');
              } else if (e.key === 'ArrowRight') {
                  e.preventDefault();
                  carousel.slick('slickNext');
              }
          });
      }
    });",
    image_sources_js_array,
    image_css,
    carousel_id
  )
  


  list(
    ui_elements = tagList(
      HTML(complete_html),
      tags$script(HTML(carousel_js))
    ),
    cache_hit = all(cache_hits),
    carousel_id = carousel_id
  )
}




# Checks the image cache and returns list of paths to images up to max_images in length
# NEed to re-write based on new directory structure iwth speciesgroup and favourites
# Bring in species_group_definitions
# Check for favourites folder for each, see if that gets us to 40?
# Consider usage of function e.g. iimage page by species? Favourites for everything?
get_latest_images <- function(max_images = 40) {
  base_dir <- "www/cache/images/favourites"
  
  if (!dir_exists(base_dir)) { 
    dir_create(base_dir)
    return(NULL)
  }
  
  # List and sort image files by modification time
  img_files <- fs::dir_ls(base_dir, regexp = "_resized\\.JPG$", ignore.case = TRUE)
  file_info <- fs::file_info(img_files)
  
  sorted_files <- img_files[order(file_info$modification_time, decreasing = TRUE)]
  
  # Limit to max_images
  latest_images <- head(sorted_files, max_images)
  
  # Format paths for web, Shiny assumes www when serving whereas we need to include for filesystem functions
  web_paths <- sub("^www/", "", latest_images)
  
  return(web_paths)
}




generate_info_modal <- function(title, description) {
  showModal(modalDialog(
    title = title,
    HTML(description),
    
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
}



get_spp_consol_details <- function(spp_consol_defs) {
  
  if (is.null(spp_consol_defs)) {
    return("No species consolidation have been defined.")
  }
  report_lines <- sapply(names(spp_consol_defs), function(name) {
    entry <- spp_consol_defs[[name]]
    old_names <- paste(entry$old_scientificName, collapse = ", ")
    new_vernacular_name <- entry$new_vernacularNames.eng
    sprintf("Any observations of %s were consolidated and will appear as %s (%s).", old_names, name, new_vernacular_name)
  }, USE.NAMES = FALSE)
  
  return(report_lines)
}





int_to_text <- function(x) {
  number_words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  if (x %in% 1:10) {
    return(number_words[x])
  } else {
    return(NA)  # Return NA for numbers outside 1-10
  }
}







# This function computes the area enclosed by the outermost points among a set of geographical coordinates 
# within each locality. It determines the smallest convex shape that encompasses all points, providing a more 
# precise representation of the area, particularly for irregularly spread points. By accounting for 
# Earth's curvature, it ensures accuracy for expansive or polar regions. This method offers a clearer insight into 
# the spatial distribution or extent of points across a locality, without limiting the area to a rectangular shape.

calculate_area_by_locality <- function(deployments) {
  # Check for NULL or empty dataframe
  if (is.null(deployments) || nrow(deployments) == 0) {
    return(NULL)
  }
  
  # Convert data to sf object
  deployments_sf <- deployments %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Calculate area for each locality without converting to km^2
  area_data <- deployments_sf %>%
    group_by(locality) %>%
    summarise(geometry = st_combine(geometry)) %>%
    mutate(geometry = st_convex_hull(geometry)) %>%
#    mutate(area_m2 = st_area(geometry)) %>% # Keep area in square meters
#    select(locality, area_m2) # Rename column to reflect unit
    mutate(area_ha = as.numeric(st_area(geometry) / 10000)) %>% # Convert m² to hectares
    select(locality, area_ha) # Rename column to reflect unit
      
  return(area_data)
}






# Returns a tibbble of locality, locationName1, locationName2, and distance where distance is
# the distance between the two locations. Output includes results where the distance is
# less than min_distance_threshold, which can be 0 if you want all marker distances
calculate_distance_between_line_locations <- function(deployments, min_distance_threshold) {

  distances <- deployments %>%
    group_by(locality) %>%
    do({
      data <- .
      pairs <- expand.grid(index1 = 1:nrow(data), index2 = 1:nrow(data))
      pairs <- subset(pairs, index1 < index2 & data$line[index1] != data$line[index2])
      
      distances <- mapply(function(x, y) distm(c(data$longitude[x], data$latitude[x]), 
                                               c(data$longitude[y], data$latitude[y]), 
                                               fun = distHaversine),
                          pairs$index1, pairs$index2)
      
      pairs$distance <- distances
      # Filter within the threshold
      pairs <- pairs[which(pairs$distance <= min_distance_threshold),]
      
      # Proceed only if pairs exist after filtering
      if(nrow(pairs) > 0) {
        pairs$locality <- rep(data$locality[1], nrow(pairs))
        pairs$locationName1 <- data$locationName[pairs$index1]
        pairs$locationName2 <- data$locationName[pairs$index2]
        
        # Select and rename columns as necessary, avoiding explicit return
        pairs <- pairs %>%
          select(locality, locationName1, locationName2, distance)
      } else {
        # Create an empty dataframe with the same structure
        pairs <- tibble(locality = character(), locationName1 = character(), locationName2 = character(), distance = numeric())
      }
      
      pairs # Implicitly return the resulting dataframe/tibble
    }) %>%
    ungroup()
  
  return(distances)
}







is_valid_UUID <- function(id) {
  grepl("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$", id)
}


generate_season_selection_text <- function(start_date, end_date, season) {
  html_string <- sprintf(
    "The deployment selection period is <strong>%s</strong>, which based on the data package 
    includes deployments starting  on or after <strong>%s</strong>, and ending on or before <strong>%s 23:59:59.</strong>",
    season,
    format(as.Date(start_date), "%Y-%m-%d"),
    format(as.Date(end_date), "%Y-%m-%d")
  )
  return(html_string)
}

generate_package_date_text <- function(package_date) {

  package_date_posix <- as.POSIXct(package_date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  formatted_date <- format(package_date_posix, format = "%Y-%m-%d %H:%M", tz = "Pacific/Auckland")
  
  html_string <- sprintf(
    "The data package on which all reporting is based was created on <strong>%s</strong>.", formatted_date
  )
  return(html_string)
}



create_density_map <- function(obs, deps, species, show_zero = TRUE) {

  max_scale <- 1
  radius_range <- c(10, 50)
  
  # Filter deployments and observations based on the period and species
  active_locations <- deps %>%
    distinct(locationID, locality, .keep_all = TRUE)
  
  obs_filtered <- obs %>%
    dplyr::filter(
      scientificName_lower == tolower(species)
    )
  
  obs_summary_location <- obs_filtered %>%
    group_by(locationID, locationName, longitude, latitude) %>%
    summarise(count = sum(count), .groups = "drop")
  
  if (nrow(obs_summary_location) > 0) {
    max_count <- max(obs_summary_location$count, na.rm = TRUE)
  } else {
    #logger::log_info("density_map_module_server, update_density_map() has no results of selected species and daterange")
    max_count <- NA
  }
  
  if (show_zero) {
    zero_entries <- active_locations %>%
      anti_join(obs_summary_location, by = "locationID") %>%
      mutate(count = 0)
    
    obs_summary_location <- bind_rows(obs_summary_location, zero_entries)
  }
  
  zero_icon <- get_species_icon(species = "none")
  
  obs_summary_location <- obs_summary_location %>%
    mutate(radius = ifelse(count > 0,
                           scales::rescale(count, to = radius_range, from = c(0, max_count)),
                           radius_range[1]))
  
  pal <- colorNumeric(palette = "inferno", domain = obs_summary_location$count)
  
  # Start with the base Leaflet map
  leaflet_map <- leaflet() %>%
    addTiles() %>%
    #addProviderTiles(providers$Esri.WorldImagery) %>%
    addCircleMarkers(
      data = obs_summary_location %>% filter(count > 0),
      lng = ~longitude, lat = ~latitude,
      radius = ~radius * max_scale,
      fillColor = ~pal(count),
      fillOpacity = 0.8,
      stroke = FALSE
    ) %>%
    addLegend(
      "bottomright", 
      pal = pal, 
      values = obs_summary_location$count, 
      title = "Number of individuals", 
      labFormat = labelFormat(), 
      opacity = 1
    )
  
  # Conditionally add markers for count == 0 if show_zero is TRUE
  if (show_zero) {
    leaflet_map <- leaflet_map %>%
      addMarkers(
        data = obs_summary_location %>% filter(count == 0),
        lng = ~longitude, lat = ~latitude,
        icon = zero_icon,
        popup = ~paste(locationName, "<br>Count: 0")
      )
  }
  
  # Return the final Leaflet map
  leaflet_map
  
}




create_species_observation_density_grid <- function(obs, locality = NULL) {
  #browser()
  spp_class_names <- names(config$globals$spp_classes)
  species_name_type <- config$globals$species_name_type
  
  # Filter data if a specific locality is provided
  filtered_obs <- obs %>%
    mutate(
      date = as.Date(timestamp),
      species_name = .data[[species_name_type]]
    )
  
  if (!is.null(locality)) {
    filtered_obs <- filtered_obs %>% filter(locality == !!locality)
  }
  
  # Group and summarise the data by date, species, and locality
  filtered_obs <- filtered_obs %>%
    group_by(date, species_name, locality) %>%
    summarise(count = sum(count), .groups = 'drop')
  
  # Proceed with your data transformation
  obs_wide <- filtered_obs %>%
    pivot_wider(names_from = species_name, values_from = count, 
                values_fill = list(count = 0), id_cols = c(date, locality)) %>%
    as.data.frame()
  
  # Transforming the data back to long format for ggplot
  obs_long <- pivot_longer(
    obs_wide,
    cols = -c(date, locality), # Ensure locality is also excluded from pivoting
    names_to = "species",
    values_to = "count"
  )
  
  
  # Summarize total counts by species
  species_counts <- obs_long %>%
    group_by(species) %>%
    summarise(total_count = sum(count)) %>%
    arrange(total_count)
  
  # Reorder species factor levels based on total count
  obs_long$species <- factor(obs_long$species,
                             levels = species_counts$species)
  
  
  # Determine the earliest start date for each locality
  earliest_dates <- filtered_obs %>%
    group_by(locality) %>%
    summarise(earliest_start_date = min(date)) %>%
    arrange(earliest_start_date) %>%
    ungroup()
  
  obs_long$locality <- factor(obs_long$locality,
                              levels = earliest_dates$locality)
  
  breaks <- c(0, 1, 2, 3, 4, 5, 10, 20, 50, 100) # Example breakpoints
  colors <- c("#F8F8F8", "#67a9cf", "#2166ac", "red") 
  positions <- breaks / max(breaks) # Calculate positions for each break
  
  spp_obs_density_grid <- ggplot(obs_long, aes(x = date, y = species, fill = count)) +
    geom_tile(color = "white", linewidth = 0.2) + 
    facet_wrap(~locality, scales = "free_x") +
    scale_fill_gradientn(colors = colorRampPalette(colors)(length(breaks)), 
                         values = rescale(positions), # Ensure positions are correctly scaled
                         guide = guide_colourbar(barwidth = 0.5, barheight = 15, 
                                                 title.position = "top", title.hjust = 0.5,
                                                 label.position = "right", label.hjust = 1)) +
    scale_x_date(date_breaks = "3 days", date_labels = "%d-%b") +
    theme_insightful_report() +
    labs(title = "Daily Species Counts",
         subtitle = "Colours scaled to make subtle variation in lower counts more prominent.",
         caption = sprintf("Data source: %s deployments | InsightfulKiwi", config$globals$organisation_abbrev),
         x = "Date",
         y = "Species",
         fill = "Count")
  
  
  
  return(spp_obs_density_grid)
  
}


theme_insightful_report <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Set the overall plot background
      plot.background = element_rect(fill = "white", color = NA),  # Ensure full plot background is white
      
      # Title and padding adjustments
      plot.title = element_text(size = rel(1.2), face = "bold", margin = margin(10, 0, 10, 0), hjust = 0),
      plot.subtitle = element_text(size = rel(1), margin = margin(5, 0, 10, 0)),  # Adjust subtitle margin
      plot.caption = element_text(size = rel(0.85), margin = margin(10, 0, 0, 0), hjust = 1),  # Caption at the bottom with margin
      
      # Panel and Grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),  # Ensure panel background is white
      
      # Axis
      axis.title = element_text(size = rel(0.95), face = "bold"),
      axis.text = element_text(size = rel(0.95)),
      axis.text.x = element_text(angle = 45),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
      
      # Legend
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      
      # Facet Labels (strip)
      strip.background = element_rect(fill = "white", color = "white"),  # Ensure facet labels background is white
      strip.text = element_text(size = rel(0.85), face = "bold", color = "black", margin = margin(5,0,5,0)),
      
      # Adjust the plot margins to add more space around the entire plot
      plot.margin = margin(t = 20, r = 10, b = 20, l = 10)  # Adjust as needed for more space
    )
}

