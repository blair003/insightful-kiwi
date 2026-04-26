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



#' Caches favourite and selected species images into period folders.
#'
#' Uses the media period when available, otherwise derives the period from the
#' matching observation sequence. Images without a period are skipped rather
#' than cached into an unhelpful Uncategorized folder.
#'
#' @param media_df The dataframe of media entries (e.g., core_data$media).
#' @param obs_df The dataframe of observations.
#' @param config A configuration list containing globals, such as resize width.
#' @return Invisibly returns NULL. Called for its side-effect of managing cached files.

cache_selected_images <- function(media_df, obs_df, config) {
  local_cache_dir <- "www/cache/images"
  favourites_base_dir <- file.path(local_cache_dir, "favourites")
  cache_species_classes <- c("target", "interesting")

  get_cache_scalar <- function(value, fallback = NA_character_) {
    if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
      return(fallback)
    }

    value <- trimws(as.character(value[[1]]))
    if (nchar(value) == 0) {
      return(fallback)
    }

    value
  }

  sanitize_cache_folder <- function(value, fallback) {
    folder <- get_cache_scalar(value, fallback)
    if (is.na(folder)) {
      return(fallback)
    }

    folder <- gsub("[^a-zA-Z0-9_ -]", "", folder)

    if (is.na(folder) || nchar(trimws(folder)) == 0) {
      return(fallback)
    }

    folder
  }

  get_column_value <- function(df, column, row_index, fallback = NA_character_) {
    if (!column %in% names(df)) {
      return(fallback)
    }

    get_cache_scalar(df[[column]][row_index], fallback)
  }

  sequence_period_lookup <- character(0)
  if (all(c("sequenceID", "period") %in% names(obs_df))) {
    obs_periods <- obs_df[
      !is.na(obs_df$sequenceID) & !is.na(obs_df$period),
      c("sequenceID", "period"),
      drop = FALSE
    ]
    obs_periods <- obs_periods[!duplicated(obs_periods$sequenceID), , drop = FALSE]
    sequence_period_lookup <- setNames(as.character(obs_periods$period), obs_periods$sequenceID)
  }

  get_sequence_period <- function(sequence_id, existing_period = NA_character_) {
    period <- get_cache_scalar(existing_period)
    if (!is.na(period)) {
      return(period)
    }

    sequence_id <- get_cache_scalar(sequence_id)
    if (is.na(sequence_id) || !sequence_id %in% names(sequence_period_lookup)) {
      return(NA_character_)
    }

    sequence_period_lookup[[sequence_id]]
  }

  if (!dir.exists(favourites_base_dir)) {
    dir.create(favourites_base_dir, recursive = TRUE)
  }

  uncategorized_dir <- file.path(favourites_base_dir, "Uncategorized")
  if (dir.exists(uncategorized_dir)) {
    unlink(uncategorized_dir, recursive = TRUE, force = TRUE)
    logger::log_info("Removed stale uncategorized image cache folder: %s", uncategorized_dir)
  }

  # Identify favourite images
  favourite_flags <- rep(FALSE, nrow(media_df))
  if ("favourite" %in% names(media_df)) {
    favourite_flags <- !is.na(media_df$favourite) & media_df$favourite == TRUE
  }
  favourite_images <- media_df[which(favourite_flags), , drop = FALSE]
  if (nrow(favourite_images) > 0) {
    favourite_images$category <- "favourite"
  } else {
    favourite_images$category <- character(0)
  }

  # Identify selected species images
  spp_classes <- config$globals$spp_classes
  selected_species <- unique(unlist(
    spp_classes[intersect(cache_species_classes, names(spp_classes))],
    use.names = FALSE
  ))

  excluded_species <- config$globals$image_cache_excluded_species
  if (is.null(excluded_species)) {
    excluded_species <- character(0)
  }

  selected_species <- selected_species[
    !tolower(trimws(selected_species)) %in% tolower(trimws(excluded_species))
  ]

  selected_obs <- obs_df[tolower(obs_df$scientificName) %in% tolower(selected_species), , drop = FALSE]
  selected_media <- media_df[media_df$sequenceID %in% selected_obs$sequenceID, , drop = FALSE]

  if (nrow(selected_media) > 0) {
    # Merge with obs to get the scientificName for the subfolder
    # A sequence might have multiple observations, so we might have duplicates here,
    # but since we are just caching, we just need to copy the image to each species folder it belongs to.
    selected_obs_for_merge <- selected_obs[, c("sequenceID", "scientificName"), drop = FALSE]
    selected_obs_for_merge$obs_period <- if ("period" %in% names(selected_obs)) {
      selected_obs$period
    } else {
      NA_character_
    }

    selected_media_merged <- merge(
      selected_media,
      selected_obs_for_merge,
      by = "sequenceID"
    )
    selected_media_merged$category <- "selected_species"
  } else {
    selected_media_merged <- data.frame(category = character(0))
  }
  
  # Combine
  all_images <- list()
  
  if (nrow(favourite_images) > 0) {
    for (i in 1:nrow(favourite_images)) {
      all_images[[length(all_images) + 1]] <- list(
        filePath = favourite_images$filePath[i],
        fileName = favourite_images$fileName[i],
        period = get_sequence_period(
          favourite_images$sequenceID[i],
          get_column_value(favourite_images, "period", i)
        ),
        category = "favourite",
        species = NA
      )
    }
  }
  
  if (nrow(selected_media_merged) > 0) {
    for (i in 1:nrow(selected_media_merged)) {
      all_images[[length(all_images) + 1]] <- list(
        filePath = selected_media_merged$filePath[i],
        fileName = selected_media_merged$fileName[i],
        period = get_sequence_period(
          selected_media_merged$sequenceID[i],
          get_column_value(selected_media_merged, "period", i, selected_media_merged$obs_period[i])
        ),
        category = "selected_species",
        species = selected_media_merged$scientificName[i]
      )
    }
  }
  
  total_images <- length(all_images)

  if (total_images == 0) {
    logger::log_info("No favourite or selected species images found to cache.")
    return(invisible(NULL))
  }

  logger::log_info(
    "Found %d images to cache (favourites & selected species). Excluded species: %s",
    total_images,
    paste(excluded_species, collapse = ", ")
  )

  for (i in seq_len(total_images)) {
    image_info <- all_images[[i]]

    file_path <- get_cache_scalar(image_info$filePath)
    file_name <- get_cache_scalar(image_info$fileName)

    if (is.na(file_path) || is.na(file_name)) {
      logger::log_warn(
        "[%d/%d] Skipping image with missing file path or name.",
        i,
        total_images
      )
      next
    }

    period_folder <- sanitize_cache_folder(image_info$period, NA_character_)
    if (is.na(period_folder)) {
      logger::log_warn(
        "[%d/%d] Skipping image with no period group: %s",
        i,
        total_images,
        file_name
      )
      next
    }

    if (image_info$category == "favourite") {
      dest_dir <- file.path(favourites_base_dir, period_folder)
    } else {
      # Selected species: nested inside species name
      species_folder <- sanitize_cache_folder(image_info$species, "Unknown")
      dest_dir <- file.path(favourites_base_dir, period_folder, species_folder)
    }

    path_components <- unlist(strsplit(file_path, "/"))
    if (length(path_components) < 2) {
      logger::log_warn("[%d/%d] Skipping image with invalid file path: %s", i, total_images, file_path)
      next
    }

    hash_dir_name <- path_components[length(path_components) - 1]
    image_dir_path <- file.path(local_cache_dir, hash_dir_name)
    local_file_path <- file.path(image_dir_path, file_name)
    dest_file_path <- file.path(dest_dir, basename(local_file_path))

    tryCatch({
      if (!file.exists(local_file_path)) {
        logger::log_info("[%d/%d] Downloading: %s", i, total_images, file_name)

        if (!dir.exists(image_dir_path)) dir.create(image_dir_path, recursive = TRUE)
        if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

        download_image(file_path, local_file_path)
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
        file_path,
        conditionMessage(e)
      )
    })
  }
  
  invisible(NULL)
}

