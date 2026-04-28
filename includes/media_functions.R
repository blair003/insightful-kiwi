# Generates URLs to view images for sequences from Agouti\. For cached observations,
# they are still viewed via original Agouti urls \(it's fast enough now they are progressive jpegs\)\.
# We are not caching empty images
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


update_image_cache <- function(sequence_media_info) {
  local_cache_dir <- "www/cache/images"
  favourites_dir <- file.path(local_cache_dir, "favourites")
  
  for (image_info in sequence_media_info) {
    local({
      image_info_local <- image_info
      favourite_local <- image_info_local$favourite
      
      path_components <- unlist(strsplit(image_info_local$filePath, "/"))
      hash_dir_name <- path_components[length(path_components) - 1]
      
      image_dir_path <- file.path(local_cache_dir, hash_dir_name)
      dir.create(image_dir_path, recursive = TRUE, showWarnings = FALSE)
      
      local_file_path <- file.path(image_dir_path, image_info_local$fileName)
      source_file_path <- image_info_local$filePath
      file_name <- image_info_local$fileName
      
      if (!file.exists(local_file_path)) {
        future({
          tryCatch({
            logger::log_info("Downloading image %s to %s", source_file_path, local_file_path)
            
            download_image(source_file_path, local_file_path)
            
            resized_file_path <- create_resized_image(
              local_file_path,
              config$globals$image_resize_width_pixels
            )
            
            if (!is.character(resized_file_path)) {
              stop("Resized file path is not a character vector.")
            }
            
            if (!is.null(favourite_local) && favourite_local == TRUE) {
              dir.create(favourites_dir, recursive = TRUE, showWarnings = FALSE)
              
              favourite_file_path <- file.path(favourites_dir, basename(resized_file_path))
              file.copy(resized_file_path, favourite_file_path, overwrite = TRUE)
            }
            
          }, error = function(e) {
            logger::log_error("Error in future block for %s: %s", local_file_path, e$message)
          })
        }, seed = TRUE) %...>% {
          logger::log_info("Download and processing completed for: %s", local_file_path)
        }
      } else {
        logger::log_info("Image exists: %s", local_file_path)
      }
    })
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
  
  share_btn_html <- sprintf(
    "<button class='btn btn-sm btn-outline-secondary' onclick='copyObservationUrl(\"%s\", this)' title='Share this observation'><i class='fa fa-share-nodes'></i> Share</button>",
    observation_id
  )

  showModal(modalDialog(
    title = tagList(
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 20px;",
        tags$span(sprintf("Obs ID: %s", observation_id)),
        HTML(share_btn_html)
      )
    ),
    ui_elements,
    uiOutput("observation_record_table_modal"),
    size = "l",  # Large modal
    easyClose = TRUE,
    footer = modalButton("Close")
  ))

}




create_observation_images_ui <- function(sequence_media_info, observation_id, context = "pageview", review_nav = NULL) {

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
  
  
  review_nav_json <- if (!is.null(review_nav)) jsonlite::toJSON(review_nav, auto_unbox = TRUE) else "null"

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
            var reviewNav = %s;
            var initialSlide = 0;
            if (reviewNav && reviewNav.initial_slide === 'last') {
                initialSlide = Math.max(totalImages - 1, 0);
            } else if (reviewNav && Number.isInteger(reviewNav.initial_slide)) {
                initialSlide = Math.min(Math.max(reviewNav.initial_slide, 0), Math.max(totalImages - 1, 0));
            }

            carousel.slick({
                infinite: false,
                slidesToShow: 1,
                initialSlide: initialSlide,
                adaptiveHeight: true,
                arrows: true,
                dots: true,
                accessibility: true,
                speed: 0
            });

            // Setup infinite navigation if in review mode
            if (%s && %s !== 'null') {
                carousel.on('edge', function(event, slick, direction) {
                    if (direction === 'left' && reviewNav.current_index < reviewNav.total_sequences) {
                        Shiny.setInputValue('review_nav_click', {index: reviewNav.current_index + 1, initial_slide: 0}, {priority: 'event'});
                    } else if (direction === 'right' && reviewNav.current_index > 1) {
                        Shiny.setInputValue('review_nav_click', {index: reviewNav.current_index - 1, initial_slide: 'last'}, {priority: 'event'});
                    }
                });

                var nextWasDisabled = false;
                var prevWasDisabled = false;

                carousel.find('.slick-next').on('mousedown touchstart', function() {
                    nextWasDisabled = $(this).hasClass('slick-disabled');
                });

                carousel.find('.slick-prev').on('mousedown touchstart', function() {
                    prevWasDisabled = $(this).hasClass('slick-disabled');
                });

                // Slick may update the current slide before click handlers run, so
                // use the arrow state captured before the click to detect edge-only
                // sequence navigation.
                carousel.find('.slick-next').on('click', function() {
                    var currentSlide = carousel.slick('slickCurrentSlide');
                    if (nextWasDisabled && currentSlide === totalImages - 1 && reviewNav.current_index < reviewNav.total_sequences) {
                        Shiny.setInputValue('review_nav_click', {index: reviewNav.current_index + 1, initial_slide: 0}, {priority: 'event'});
                    }
                    nextWasDisabled = false;
                });

                carousel.find('.slick-prev').on('click', function() {
                    var currentSlide = carousel.slick('slickCurrentSlide');
                    if (prevWasDisabled && currentSlide === 0 && reviewNav.current_index > 1) {
                        Shiny.setInputValue('review_nav_click', {index: reviewNav.current_index - 1, initial_slide: 'last'}, {priority: 'event'});
                    }
                    prevWasDisabled = false;
                });

                // Arrow keys for edge detection
                $(document).on('keydown', function(e) {
                    var currentSlide = carousel.slick('slickCurrentSlide');
                    if (e.key === 'ArrowRight' && currentSlide === totalImages - 1 && reviewNav.current_index < reviewNav.total_sequences) {
                        Shiny.setInputValue('review_nav_click', {index: reviewNav.current_index + 1, initial_slide: 0}, {priority: 'event'});
                    } else if (e.key === 'ArrowLeft' && currentSlide === 0 && reviewNav.current_index > 1) {
                        Shiny.setInputValue('review_nav_click', {index: reviewNav.current_index - 1, initial_slide: 'last'}, {priority: 'event'});
                    }
                });
            }


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
    carousel_id,
    review_nav_json,
    ifelse(is.null(review_nav), "false", "true"),
    review_nav_json
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




