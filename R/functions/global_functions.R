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
      remotes::install_github(packages[match(missing_packages, sapply(packages, basename))])
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

get_use_net_data_setting <- function(default = NULL) {
  if (is.null(default) && exists("config", inherits = TRUE)) {
    default <- config$globals$use_net_data
  }

  domain <- shiny::getDefaultReactiveDomain()
  if (!is.null(domain)) {
    root_session <- domain$rootScope()
    setting <- root_session$userData$use_net_data
    if (is.function(setting)) {
      value <- tryCatch(
        setting(),
        error = function(e) shiny::isolate(setting())
      )
      return(isTRUE(value))
    }
  }

  isTRUE(default)
}

filter_possible_duplicates_for_use_net <- function(obs, use_net = NULL) {
  if (is.null(obs) || !isTRUE(if (is.null(use_net)) get_use_net_data_setting() else use_net)) {
    return(obs)
  }

  if (!"possible_duplicate" %in% names(obs)) {
    return(obs)
  }

  obs %>%
    dplyr::filter(is.na(possible_duplicate) | !possible_duplicate)
}


configure_image_cache_logger <- function(config, log_file = NULL) {
  if (is.null(log_file)) {
    log_file <- file.path(
      config$env$dirs$logs,
      sprintf("image-cache-%s.log", format(Sys.Date(), "%Y-%m-%d"))
    )
  }

  log_dir <- dirname(log_file)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }

  logger::log_appender(logger::appender_file(log_file, append = TRUE))
  logger::log_formatter(logger::formatter_sprintf)
  if (!is.null(config$globals$log_threshold)) {
    logger::log_threshold(config$globals$log_threshold)
  }

  log_file
}


append_image_cache_log <- function(log_file, level, message, ...) {
  tryCatch({
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }

    line <- sprintf(
      "%s [%s] %s",
      toupper(level),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      sprintf(message, ...)
    )

    cat(line, "\n", file = log_file, append = TRUE)
  }, error = function(e) {
    invisible(NULL)
  })

  invisible(NULL)
}


cache_media_is_public <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return(TRUE)
  }

  value <- value[[1]]
  if (is.logical(value)) {
    return(isTRUE(value))
  }

  !tolower(trimws(as.character(value))) %in% c("false", "f", "no", "n", "0")
}


get_media_public_flags <- function(media_df, config = NULL) {
  public_column <- intersect(c("filePublic", "isPublic"), names(media_df))
  if (length(public_column) > 0) {
    public_column <- public_column[[1]]
    return(list(
      flags = vapply(media_df[[public_column]], cache_media_is_public, logical(1)),
      column = public_column
    ))
  }

  if (!is.null(config) && "mediaID" %in% names(media_df)) {
    raw_media_path <- file.path(config$env$dirs$camtrap_package, "media.csv")
    if (file.exists(raw_media_path)) {
      raw_media <- tryCatch(
        utils::read.csv(raw_media_path, stringsAsFactors = FALSE),
        error = function(e) {
          logger::log_warn(
            "Could not read raw media.csv for filePublic lookup: %s",
            conditionMessage(e)
          )
          NULL
        }
      )

      if (!is.null(raw_media) && all(c("mediaID", "filePublic") %in% names(raw_media))) {
        raw_public_lookup <- stats::setNames(raw_media$filePublic, raw_media$mediaID)
        matched_public <- raw_public_lookup[as.character(media_df$mediaID)]
        return(list(
          flags = vapply(matched_public, cache_media_is_public, logical(1)),
          column = "media.csv:filePublic"
        ))
      }
    }
  }

  list(
    flags = rep(TRUE, nrow(media_df)),
    column = NA_character_
  )
}


normalize_cache_file_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}


get_served_image_cache_dir <- function() {
  normalize_cache_file_path(file.path("www", "cache", "images"))
}


get_configured_image_cache_dir <- function(config) {
  normalize_cache_file_path(file.path(config$env$dirs$cache, "images"))
}


get_image_cache_dirs <- function(config) {
  cache_dirs <- c(
    get_served_image_cache_dir(),
    get_configured_image_cache_dir(config)
  )

  unique(cache_dirs[nzchar(cache_dirs)])
}


get_primary_image_cache_dir <- function(config) {
  get_image_cache_dirs(config)[[1]]
}


image_cache_file_url <- function(file_path) {
  normalized_file_path <- normalize_cache_file_path(file_path)
  www_dir <- normalize_cache_file_path("www")
  www_prefix <- paste0(www_dir, "/")

  if (!startsWith(normalized_file_path, www_prefix)) {
    return(NA_character_)
  }

  sub_path <- substring(normalized_file_path, nchar(www_prefix) + 1)
  gsub("\\\\", "/", sub_path)
}


find_cached_image_file <- function(config, hash_dir_name, file_name) {
  candidate_paths <- file.path(get_image_cache_dirs(config), hash_dir_name, file_name)
  existing_paths <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing_paths) == 0) {
    return(NA_character_)
  }

  existing_paths[[1]]
}



#' Caches favourite and selected species images.
#'
#' Selected species images are cached under their source image hash directory.
#' Favourite images are additionally copied into favourites folders and recorded
#' in a manifest so UI images can open their source sequence for review.
#'
#' @param media_df The dataframe of media entries (e.g., core_data$media).
#' @param obs_df The dataframe of observations.
#' @param config A configuration list containing globals, such as resize width.
#' @return Invisibly returns NULL. Called for its side-effect of managing cached files.

cache_selected_images <- function(media_df, obs_df, config) {
  local_cache_dir <- get_primary_image_cache_dir(config)
  favourites_base_dir <- file.path(local_cache_dir, "favourites")
  favourites_manifest_path <- file.path(favourites_base_dir, "_manifest.csv")
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

  is_true_value <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
      return(FALSE)
    }

    value <- value[[1]]
    if (is.logical(value)) {
      return(isTRUE(value))
    }

    tolower(trimws(as.character(value))) %in% c("true", "t", "yes", "y", "1")
  }

  manifest_rows <- list()
  cache_stats <- new.env(parent = emptyenv())
  cache_stats$downloaded <- 0L
  cache_stats$already_cached <- 0L
  cache_stats$resized_created <- 0L
  cache_stats$skipped_missing_path <- 0L
  cache_stats$skipped_invalid_path <- 0L
  cache_stats$skipped_not_public <- 0L
  cache_stats$failed <- 0L
  cache_stats$favourite_period_copies <- 0L
  cache_stats$favourite_species_copies <- 0L

  add_manifest_row <- function(cached_file_path,
                               context,
                               sequence_id,
                               observation_id,
                               period,
                               scientific_name,
                               file_name,
                               source_file_path) {
    web_path <- image_cache_file_url(cached_file_path)

    manifest_rows[[length(manifest_rows) + 1]] <<- data.frame(
      web_path = web_path,
      context = context,
      sequenceID = sequence_id,
      observationID = observation_id,
      period = period,
      scientificName = scientific_name,
      fileName = file_name,
      sourceFilePath = source_file_path,
      stringsAsFactors = FALSE
    )
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

  sequence_obs_lookup <- data.frame()
  if (all(c("sequenceID", "observationID", "scientificName") %in% names(obs_df))) {
    obs_lookup_columns <- c("sequenceID", "observationID", "scientificName")
    if ("period" %in% names(obs_df)) {
      obs_lookup_columns <- c(obs_lookup_columns, "period")
    }

    sequence_obs_lookup <- obs_df[
      !is.na(obs_df$sequenceID) & !is.na(obs_df$observationID),
      obs_lookup_columns,
      drop = FALSE
    ]
    sequence_obs_lookup$sequenceID <- as.character(sequence_obs_lookup$sequenceID)
    sequence_obs_lookup$observationID <- as.character(sequence_obs_lookup$observationID)
    sequence_obs_lookup$scientificName <- as.character(sequence_obs_lookup$scientificName)
    if (!"period" %in% names(sequence_obs_lookup)) {
      sequence_obs_lookup$period <- NA_character_
    }
  }

  get_sequence_observations <- function(sequence_id) {
    sequence_id <- get_cache_scalar(sequence_id)
    if (is.na(sequence_id) || nrow(sequence_obs_lookup) == 0) {
      return(data.frame())
    }

    sequence_obs_lookup[sequence_obs_lookup$sequenceID == sequence_id, , drop = FALSE]
  }

  if (!dir.exists(favourites_base_dir)) {
    dir.create(favourites_base_dir, recursive = TRUE)
  }

  uncategorized_dir <- file.path(favourites_base_dir, "Uncategorized")
  if (dir.exists(uncategorized_dir)) {
    unlink(uncategorized_dir, recursive = TRUE, force = TRUE)
    logger::log_info("Removed stale uncategorized image cache folder: %s", uncategorized_dir)
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
  selected_sequence_ids <- unique(as.character(selected_obs$sequenceID))

  favourite_flags <- rep(FALSE, nrow(media_df))
  if ("favourite" %in% names(media_df)) {
    favourite_flags <- vapply(media_df$favourite, is_true_value, logical(1))
  }

  public_lookup <- get_media_public_flags(media_df, config)
  public_flags <- public_lookup$flags

  selected_flags <- as.character(media_df$sequenceID) %in% selected_sequence_ids
  cache_candidate_flags <- selected_flags | favourite_flags
  cache_stats$skipped_not_public <- sum(cache_candidate_flags & !public_flags)
  media_to_cache <- media_df[cache_candidate_flags & public_flags, , drop = FALSE]

  if (nrow(media_to_cache) > 0) {
    media_to_cache <- media_to_cache[!duplicated(paste(media_to_cache$sequenceID, media_to_cache$fileName, sep = "|")), , drop = FALSE]
  } else {
    media_to_cache <- data.frame()
  }

  total_images <- nrow(media_to_cache)

  if (total_images == 0) {
    logger::log_info("No favourite or selected species images found to cache.")
    logger::log_info(
      "Image cache summary: total=0, downloaded=0, already_cached=0, resized_created=0, skipped_missing_path=0, skipped_invalid_path=0, skipped_not_public=%d, failed=0, favourite_period_copies=0, favourite_species_copies=0, manifest_rows=0",
      cache_stats$skipped_not_public
    )
    return(invisible(NULL))
  }

  logger::log_info(
    "Found %d source images to cache (favourites & selected species). Excluded species: %s",
    total_images,
    paste(excluded_species, collapse = ", ")
  )
  logger::log_info(
    "Image cache selection summary: selected_species=%d, selected_sequences=%d, favourite_media=%d, public_column=%s, skipped_not_public=%d",
    length(selected_species),
    length(selected_sequence_ids),
    sum(favourite_flags),
    ifelse(is.na(public_lookup$column), "<none>", public_lookup$column),
    cache_stats$skipped_not_public
  )

  for (i in seq_len(total_images)) {
    file_path <- get_column_value(media_to_cache, "filePath", i)
    file_name <- get_column_value(media_to_cache, "fileName", i)
    sequence_id <- get_column_value(media_to_cache, "sequenceID", i)
    is_favourite <- if ("favourite" %in% names(media_to_cache)) {
      is_true_value(media_to_cache$favourite[i])
    } else {
      FALSE
    }

    if (is.na(file_path) || is.na(file_name)) {
      cache_stats$skipped_missing_path <- cache_stats$skipped_missing_path + 1L
      logger::log_warn(
        "[%d/%d] Skipping image with missing file path or name.",
        i,
        total_images
      )
      next
    }

    path_components <- unlist(strsplit(file_path, "/"))
    if (length(path_components) < 2) {
      cache_stats$skipped_invalid_path <- cache_stats$skipped_invalid_path + 1L
      logger::log_warn("[%d/%d] Skipping image with invalid file path: %s", i, total_images, file_path)
      next
    }

    hash_dir_name <- path_components[length(path_components) - 1]
    image_dir_path <- file.path(local_cache_dir, hash_dir_name)
    local_file_path <- file.path(image_dir_path, file_name)
    resized_file_path <- sub("\\.JPG$", "_resized.JPG", local_file_path, ignore.case = TRUE)

    tryCatch({
      if (!file.exists(local_file_path)) {
        logger::log_info("[%d/%d] Downloading sequence %s image: %s", i, total_images, sequence_id, file_name)

        if (!dir.exists(image_dir_path)) dir.create(image_dir_path, recursive = TRUE)

        download_image(file_path, local_file_path)
        cache_stats$downloaded <- cache_stats$downloaded + 1L
      } else {
        logger::log_info("[%d/%d] Image already cached: %s", i, total_images, local_file_path)
        cache_stats$already_cached <- cache_stats$already_cached + 1L
      }

      if (!file.exists(resized_file_path)) {
        resized_file_path <- create_resized_image(local_file_path, config$globals$image_resize_width_pixels)
        cache_stats$resized_created <- cache_stats$resized_created + 1L
      }

      if (is_favourite) {
        sequence_obs <- get_sequence_observations(sequence_id)
        observation_id <- if (nrow(sequence_obs) > 0) sequence_obs$observationID[[1]] else NA_character_
        period <- get_sequence_period(sequence_id, get_column_value(media_to_cache, "period", i))
        period_folder <- sanitize_cache_folder(period, NA_character_)

        if (is.na(period_folder)) {
          logger::log_warn(
            paste(
              "[%d/%d] Favourite image has no period group, skipping favourite copy:",
              "sequenceID=%s, observationID=%s, fileName=%s, filePath=%s"
            ),
            i,
            total_images,
            sequence_id,
            observation_id,
            file_name,
            file_path
          )
        } else {
          period_dest_dir <- file.path(favourites_base_dir, period_folder)
          if (!dir.exists(period_dest_dir)) dir.create(period_dest_dir, recursive = TRUE)
          period_dest_file_path <- file.path(period_dest_dir, basename(resized_file_path))
          file.copy(resized_file_path, period_dest_file_path, overwrite = TRUE)
          cache_stats$favourite_period_copies <- cache_stats$favourite_period_copies + 1L
          add_manifest_row(
            cached_file_path = period_dest_file_path,
            context = "period",
            sequence_id = sequence_id,
            observation_id = observation_id,
            period = period,
            scientific_name = if (nrow(sequence_obs) > 0) sequence_obs$scientificName[[1]] else NA_character_,
            file_name = file_name,
            source_file_path = file_path
          )
        }

        if (nrow(sequence_obs) > 0) {
          sequence_obs <- sequence_obs[!duplicated(sequence_obs$scientificName), , drop = FALSE]
          for (obs_index in seq_len(nrow(sequence_obs))) {
            species_folder <- sanitize_cache_folder(sequence_obs$scientificName[obs_index], "Unknown")
            species_dest_dir <- file.path(favourites_base_dir, species_folder)
            if (!dir.exists(species_dest_dir)) dir.create(species_dest_dir, recursive = TRUE)
            species_dest_file_path <- file.path(species_dest_dir, basename(resized_file_path))
            file.copy(resized_file_path, species_dest_file_path, overwrite = TRUE)
            cache_stats$favourite_species_copies <- cache_stats$favourite_species_copies + 1L
            add_manifest_row(
              cached_file_path = species_dest_file_path,
              context = "species",
              sequence_id = sequence_id,
              observation_id = sequence_obs$observationID[obs_index],
              period = period,
              scientific_name = sequence_obs$scientificName[obs_index],
              file_name = file_name,
              source_file_path = file_path
            )
          }
        }
      }
    }, error = function(e) {
      cache_stats$failed <- cache_stats$failed + 1L
      logger::log_error(
        "Failed to process %s. Error: %s",
        file_path,
        conditionMessage(e)
      )
    })
  }

  if (length(manifest_rows) > 0) {
    manifest <- do.call(rbind, manifest_rows)
    manifest <- manifest[!duplicated(paste(manifest$web_path, manifest$observationID, sep = "|")), , drop = FALSE]
    if (!dir.exists(favourites_base_dir)) {
      dir.create(favourites_base_dir, recursive = TRUE)
    }
    utils::write.csv(manifest, favourites_manifest_path, row.names = FALSE)
    logger::log_info("Wrote favourites image manifest: %s", favourites_manifest_path)
  }

  logger::log_info(
    paste(
      "Image cache summary:",
      "total=%d, downloaded=%d, already_cached=%d, resized_created=%d,",
      "skipped_missing_path=%d, skipped_invalid_path=%d, skipped_not_public=%d, failed=%d,",
      "favourite_period_copies=%d, favourite_species_copies=%d, manifest_rows=%d"
    ),
    total_images,
    cache_stats$downloaded,
    cache_stats$already_cached,
    cache_stats$resized_created,
    cache_stats$skipped_missing_path,
    cache_stats$skipped_invalid_path,
    cache_stats$skipped_not_public,
    cache_stats$failed,
    cache_stats$favourite_period_copies,
    cache_stats$favourite_species_copies,
    length(manifest_rows)
  )
  
  invisible(NULL)
}
