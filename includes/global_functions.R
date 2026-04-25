# Function to check and install missing packages
#
# This function checks if specified R packages are installed. If any are missing,
# it installs them from either CRAN or GitHub.
#
# @param packages A character vector of package names to check and install.
# @param source A character string indicating the source of the packages.
#   Must be "cran" for CRAN or "github" for GitHub.
# @examples
# \dontrun{
#   install_if_missing(c("dplyr", "ggplot2"), source = "cran")
#   install_if_missing(c("r-lib/devtools"), source = "github")
# }
install_if_missing <- function(packages, source = c("cran", "github")) {
  if (source == "cran") {
    missing_packages <- setdiff(packages, installed.packages()[, "Package"])
    if (length(missing_packages) > 0) {
      install.packages(missing_packages)
    }
  } else if (source == "github") {
    missing_packages <- setdiff(sapply(packages, basename), installed.packages()[, "Package"])
    if (length(missing_packages) > 0) {
      devtools::install_github(packages[match(missing_packages, sapply(packages, basename))])
    }
  } else {
    stop("Invalid source argument. Must be either 'cran' or 'github'.")
  }
}

# Function to create directories if they don't exist
#
# This function takes a named list of directory paths and creates them if they
# do not already exist. It logs the creation of new directories and notes
# when a directory already exists.
#
# @param dirs A named list where names are logical identifiers for the directories
#   and values are the character paths to the directories.
# @examples
# \dontrun{
#   create_directories_if_missing(list(data = "data/", output = "output/reports"))
# }
create_directories_if_missing <- function(dirs) {
  if (length(dirs) == 0) {
    logger::log_warn("No directories provided to ensure_directories_exist().")
    return(invisible(NULL))
  }
  
  lapply(names(dirs), function(name) {
    dir_path <- dirs[[name]]
    if (!fs::dir_exists(dir_path)) {
      fs::dir_create(dir_path)
      logger::log_info(sprintf("Created directory '%s' at path: %s", name, dir_path))
    } else {
      logger::log_debug(sprintf("Directory '%s' already exists at path: %s", name, dir_path))
    }
  })
}



#' Caches favourite and target species images using a pre-calculated 'period' column.
#'
#' Assumes the input media_df already contains a 'period' column for subfolder
#' sorting. It processes all favourites and target species, downloading missing ones in the background
#' or copying existing ones, and organizes them into period-based subfolders.
#'
#' @param media_df The dataframe of media entries (e.g., core_data$media), which
#'   must include a 'period' column.
#' @param obs_df The dataframe of observations.
#' @param config A configuration list containing globals, such as resize width.
#' @return Invisibly returns NULL. Called for its side-effect of managing cached files.

cache_selected_images <- function(media_df, obs_df, config) {
  local_cache_dir <- "www/cache/images"
  favourites_base_dir <- file.path(local_cache_dir, "favourites")
  
  if (!dir.exists(favourites_base_dir)) {
    dir.create(favourites_base_dir, recursive = TRUE)
  }
  
  # Identify favourite images
  favourite_images <- media_df[which(media_df$favourite == TRUE), ]
  if (nrow(favourite_images) > 0) {
    favourite_images$category <- "favourite"
  } else {
    favourite_images$category <- character(0)
  }
  
  # Identify target species images
  target_species <- config$globals$spp_classes[["target"]]
  target_obs <- obs_df[tolower(obs_df$scientificName) %in% tolower(target_species), ]
  target_media <- media_df[media_df$sequenceID %in% target_obs$sequenceID, ]
  
  if (nrow(target_media) > 0) {
    # Merge with obs to get the scientificName for the subfolder
    # A sequence might have multiple observations, so we might have duplicates here,
    # but since we are just caching, we just need to copy the image to each species folder it belongs to.
    target_media_merged <- merge(target_media, target_obs[, c("sequenceID", "scientificName")], by = "sequenceID")
    target_media_merged$category <- "target"
  } else {
    target_media_merged <- data.frame(category = character(0))
  }
  
  # Combine
  all_images <- list()
  
  if (nrow(favourite_images) > 0) {
    for (i in 1:nrow(favourite_images)) {
      all_images[[length(all_images) + 1]] <- list(
        filePath = favourite_images$filePath[i],
        fileName = favourite_images$fileName[i],
        period = favourite_images$period[i],
        category = "favourite",
        species = NA
      )
    }
  }
  
  if (nrow(target_media_merged) > 0) {
    for (i in 1:nrow(target_media_merged)) {
      all_images[[length(all_images) + 1]] <- list(
        filePath = target_media_merged$filePath[i],
        fileName = target_media_merged$fileName[i],
        period = target_media_merged$period[i],
        category = "target",
        species = target_media_merged$scientificName[i]
      )
    }
  }
  
  total_images <- length(all_images)
  
  if (total_images == 0) {
    logger::log_info("No favourite or target species images found to cache.")
    return(invisible(NULL))
  }
  
  logger::log_info("Found %d images to cache (favourites & target species). Checking cache status...", total_images)
  
  for (i in seq_len(total_images)) {
    image_info <- all_images[[i]]
    
    period_folder <- image_info$period
    if (is.na(period_folder) || nchar(trimws(period_folder)) == 0) {
      period_folder <- "Uncategorized"
    }
    period_folder <- gsub("[^a-zA-Z0-9_ -]", "", period_folder)
    
    if (image_info$category == "favourite") {
      dest_dir <- file.path(favourites_base_dir, period_folder)
    } else {
      # Target species: nested inside species name
      species_folder <- image_info$species
      if (is.na(species_folder) || nchar(trimws(species_folder)) == 0) {
        species_folder <- "Unknown"
      }
      species_folder <- gsub("[^a-zA-Z0-9_ -]", "", species_folder)
      dest_dir <- file.path(favourites_base_dir, period_folder, species_folder)
    }
    
    path_components <- unlist(strsplit(image_info$filePath, "/"))
    hash_dir_name <- path_components[length(path_components) - 1]
    image_dir_path <- file.path(local_cache_dir, hash_dir_name)
    local_file_path <- file.path(image_dir_path, image_info$fileName)
    dest_file_path <- file.path(dest_dir, basename(local_file_path))
    
    tryCatch({
      if (!file.exists(local_file_path)) {
        logger::log_info("[%d/%d] Downloading: %s", i, total_images, image_info$fileName)
        
        if (!dir.exists(image_dir_path)) dir.create(image_dir_path, recursive = TRUE)
        if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
        
        download_image(image_info$filePath, local_file_path)
        create_resized_image(local_file_path, config$globals$image_resize_width_pixels)
        file.copy(local_file_path, dest_file_path, overwrite = TRUE)
        
      } else {
        logger::log_info("[%d/%d] Image already cached: %s", i, total_images, local_file_path)
        
        resized_file_path <- sub("\\.JPG$", "_resized.JPG", local_file_path, ignore.case = TRUE)
        
        if (!file.exists(resized_file_path)) {
          create_resized_image(local_file_path, config$globals$image_resize_width_pixels)
        }
        
        if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
        
        if (!file.exists(dest_file_path)) {
          logger::log_info("[%d/%d] Copying existing image to %s: %s", i, total_images, image_info$category, dest_file_path)
          file.copy(local_file_path, dest_file_path, overwrite = TRUE)
        }
      }
    }, error = function(e) {
      logger::log_error(
        "Failed to process %s. Error: %s",
        image_info$filePath,
        conditionMessage(e)
      )
    })
  }
  
  invisible(NULL)
}

