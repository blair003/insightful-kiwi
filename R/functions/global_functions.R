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
  source <- match.arg(source)
  packages <- unique(stats::na.omit(as.character(packages)))
  packages <- packages[nzchar(packages)]
  if (length(packages) == 0) {
    return(invisible(NULL))
  }

  installed_packages <- rownames(utils::installed.packages())

  if (source == "cran") {
    missing_packages <- setdiff(packages, installed_packages)
    if (length(missing_packages) > 0) {
      utils::install.packages(missing_packages)
    }
  } else {
    github_package_names <- basename(packages)
    missing_packages <- setdiff(github_package_names, installed_packages)
    if (length(missing_packages) > 0) {
      if (!requireNamespace("remotes", quietly = TRUE)) {
        stop("The remotes package is required to install GitHub packages.", call. = FALSE)
      }
      remotes::install_github(packages[match(missing_packages, github_package_names)])
    }
  }

  invisible(NULL)
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
    logger::log_warn("No directories provided to create_directories_if_missing().")
    return(invisible(NULL))
  }

  for (name in names(dirs)) {
    dir_path <- dirs[[name]]
    if (!fs::dir_exists(dir_path)) {
      fs::dir_create(dir_path)
      logger::log_info(sprintf("Created directory '%s' at path: %s", name, dir_path))
    } else {
      logger::log_debug(sprintf("Directory '%s' already exists at path: %s", name, dir_path))
    }
  }

  invisible(NULL)
}

parse_env_flag <- function(value, name) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }

  value <- as.character(value[[1]])
  if (!nzchar(value)) {
    return(NULL)
  }

  normalized_value <- tolower(trimws(value))
  if (normalized_value %in% c("true", "t", "1", "yes", "y", "on")) {
    return(TRUE)
  }
  if (normalized_value %in% c("false", "f", "0", "no", "n", "off")) {
    return(FALSE)
  }

  logger::log_warn("Ignoring invalid %s value '%s'. Use TRUE or FALSE.", name, value)
  NULL
}

prepare_runtime_core_data <- function(core_data, trap_data = NULL, config = NULL) {
  if (is.null(core_data$app)) {
    core_data$app <- list()
  }

  core_data <- update_year_period_bounds_from_observations(core_data, trap_data, config)
  core_data$app$period_defaults <- get_default_complete_period_selection(
    core_data$deps,
    core_data$period_groups,
    config
  )

  core_data
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
    log_file <- image_cache_log_path(config)
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

    cat(paste0(line, "\n"), file = log_file, append = TRUE)
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

  list(
    flags = rep(TRUE, nrow(media_df)),
    column = NA_character_
  )
}


normalize_cache_file_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}


normalize_path_separators <- function(path) {
  gsub("\\\\", "/", path)
}


encode_web_path <- function(web_path) {
  vapply(
    strsplit(normalize_path_separators(web_path), "/", fixed = TRUE),
    function(path_parts) paste(utils::URLencode(path_parts, reserved = TRUE), collapse = "/"),
    character(1)
  )
}


resolve_image_cache_config <- function(config = NULL) {
  if (!is.null(config)) {
    return(config)
  }

  if (exists("config", inherits = TRUE)) {
    return(get("config", inherits = TRUE))
  }

  stop("Image cache config was not provided and no global config exists.")
}


get_primary_image_cache_dir <- function(config) {
  if (is.null(config$env$dirs$public_media_cache)) {
    stop("config$env$dirs$public_media_cache is not configured.")
  }

  normalize_path_separators(config$env$dirs$public_media_cache)
}


get_image_cache_web_path <- function(config) {
  if (is.null(config$env$web_paths$public_media_cache)) {
    stop("config$env$web_paths$public_media_cache is not configured.")
  }

  normalize_path_separators(config$env$web_paths$public_media_cache)
}


get_favourites_manifest_path <- function(config) {
  if (is.null(config$env$dirs$media_cache_metadata)) {
    stop("config$env$dirs$media_cache_metadata is not configured.")
  }

  file.path(config$env$dirs$media_cache_metadata, "favourites_manifest.csv")
}

# --- Background Caching of favourite and selected species images on Startup ---
rebuild_favourites_manifest_from_local_cache <- function(runtime_core_data = NULL, config = NULL) {
  tryCatch({
    if (is.null(runtime_core_data)) {
      runtime_core_data <- get("core_data", inherits = TRUE)
    }
    config <- resolve_image_cache_config(config)

    rebuild_favourites_manifest_from_cached_files(
      runtime_core_data$media,
      runtime_core_data$obs,
      config
    )
  }, error = function(e) {
    logger::log_warn(
      "global.R, favourites image manifest rebuild from local cache failed: %s",
      conditionMessage(e)
    )
  })
}


image_cache_file_url <- function(file_path, config = NULL) {
  if (is.null(file_path) || is.na(file_path) || !nzchar(file_path)) {
    return(NA_character_)
  }

  config <- resolve_image_cache_config(config)
  cache_dir <- normalize_cache_file_path(get_primary_image_cache_dir(config))
  normalized_file_path <- normalize_cache_file_path(file_path)
  cache_prefix <- paste0(cache_dir, "/")

  if (!startsWith(normalized_file_path, cache_prefix)) {
    return(NA_character_)
  }

  sub_path <- substring(normalized_file_path, nchar(cache_prefix) + 1)
  normalize_path_separators(file.path(get_image_cache_web_path(config), sub_path))
}


find_cached_image_file <- function(config, hash_dir_name, file_name) {
  cached_file <- file.path(get_primary_image_cache_dir(config), hash_dir_name, file_name)

  if (!file.exists(cached_file)) {
    return(NA_character_)
  }

  cached_file
}


image_cache_folder_name <- function(value, fallback = NA_character_) {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return(fallback)
  }

  folder <- gsub("[^a-zA-Z0-9_ -]", "", trimws(as.character(value[[1]])))
  if (!nzchar(folder)) {
    return(fallback)
  }

  folder
}


empty_favourites_manifest <- function() {
  data.frame(
    web_path = character(0),
    context = character(0),
    sequenceID = character(0),
    observationID = character(0),
    period = character(0),
    scientificName = character(0),
    fileName = character(0),
    sourceFilePath = character(0),
    stringsAsFactors = FALSE
  )
}


write_favourites_manifest <- function(manifest, config) {
  manifest_path <- get_favourites_manifest_path(config)
  manifest_dir <- dirname(manifest_path)
  if (!dir.exists(manifest_dir)) {
    dir.create(manifest_dir, recursive = TRUE)
  }

  utils::write.csv(manifest, manifest_path, row.names = FALSE)
  manifest_path
}


cached_resized_to_original_filename <- function(file_name) {
  sub("_resized(?=\\.[Jj][Pp][Gg]$)", "", file_name, perl = TRUE)
}


rebuild_favourites_manifest_from_cached_files <- function(media_df, obs_df, config) {
  favourites_dir <- file.path(get_primary_image_cache_dir(config), "favourites")
  manifest <- empty_favourites_manifest()

  if (!dir.exists(favourites_dir)) {
    manifest_path <- write_favourites_manifest(manifest, config)
    logger::log_info(
      "No favourites image cache directory found at %s. Wrote empty favourites manifest: %s",
      favourites_dir,
      manifest_path
    )
    return(invisible(0L))
  }

  image_files <- list.files(
    favourites_dir,
    pattern = "_resized\\.[Jj][Pp][Gg]$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(image_files) == 0) {
    manifest_path <- write_favourites_manifest(manifest, config)
    logger::log_info("No cached favourite images found. Wrote empty favourites manifest: %s", manifest_path)
    return(invisible(0L))
  }

  if (!all(c("fileName", "sequenceID", "filePath") %in% names(media_df)) ||
      !all(c("sequenceID", "observationID", "scientificName") %in% names(obs_df))) {
    manifest_path <- write_favourites_manifest(manifest, config)
    logger::log_warn(
      "Cannot rebuild favourites manifest because core_data media/obs columns are incomplete. Wrote empty manifest: %s",
      manifest_path
    )
    return(invisible(0L))
  }

  public_lookup <- get_media_public_flags(media_df, config)
  media_df <- media_df[public_lookup$flags, , drop = FALSE]
  media_df$fileName <- as.character(media_df$fileName)
  media_df$sequenceID <- as.character(media_df$sequenceID)

  obs_df$sequenceID <- as.character(obs_df$sequenceID)
  obs_df$observationID <- as.character(obs_df$observationID)
  obs_df$scientificName <- as.character(obs_df$scientificName)
  if (!"period" %in% names(obs_df)) {
    obs_df$period <- NA_character_
  }

  rows <- list()
  favourites_prefix <- paste0(normalize_cache_file_path(favourites_dir), "/")

  add_row <- function(web_path, context, sequence_id, observation_id, period, scientific_name, file_name, source_file_path) {
    rows[[length(rows) + 1L]] <<- data.frame(
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

  for (cached_file in image_files) {
    normalized_cached_file <- normalize_cache_file_path(cached_file)
    if (!startsWith(normalized_cached_file, favourites_prefix)) {
      next
    }

    relative_path <- substring(normalized_cached_file, nchar(favourites_prefix) + 1)
    path_parts <- strsplit(relative_path, "/", fixed = TRUE)[[1]]
    if (length(path_parts) < 2) {
      next
    }

    folder <- path_parts[[1]]
    resized_file_name <- basename(cached_file)
    source_file_name <- cached_resized_to_original_filename(resized_file_name)
    media_matches <- media_df[media_df$fileName == source_file_name, , drop = FALSE]
    if (nrow(media_matches) == 0) {
      next
    }

    web_path <- image_cache_file_url(cached_file, config)
    if (is.na(web_path)) {
      next
    }

    for (media_index in seq_len(nrow(media_matches))) {
      sequence_id <- media_matches$sequenceID[[media_index]]
      sequence_obs <- obs_df[obs_df$sequenceID == sequence_id, , drop = FALSE]
      if (nrow(sequence_obs) == 0) {
        next
      }

      period_matches <- !is.na(sequence_obs$period) &
        vapply(sequence_obs$period, image_cache_folder_name, character(1)) == folder
      if (any(period_matches)) {
        obs_row <- sequence_obs[which(period_matches)[[1]], , drop = FALSE]
        add_row(
          web_path = web_path,
          context = "period",
          sequence_id = sequence_id,
          observation_id = obs_row$observationID[[1]],
          period = obs_row$period[[1]],
          scientific_name = obs_row$scientificName[[1]],
          file_name = source_file_name,
          source_file_path = media_matches$filePath[[media_index]]
        )
      }

      species_matches <- vapply(sequence_obs$scientificName, image_cache_folder_name, character(1)) == folder
      if (any(species_matches)) {
        species_obs <- sequence_obs[species_matches, , drop = FALSE]
        species_obs <- species_obs[!duplicated(species_obs$scientificName), , drop = FALSE]
        for (obs_index in seq_len(nrow(species_obs))) {
          add_row(
            web_path = web_path,
            context = "species",
            sequence_id = sequence_id,
            observation_id = species_obs$observationID[[obs_index]],
            period = species_obs$period[[obs_index]],
            scientific_name = species_obs$scientificName[[obs_index]],
            file_name = source_file_name,
            source_file_path = media_matches$filePath[[media_index]]
          )
        }
      }
    }
  }

  if (length(rows) > 0) {
    manifest <- do.call(rbind, rows)
    manifest <- manifest[!duplicated(paste(manifest$web_path, manifest$context, manifest$observationID, sep = "|")), , drop = FALSE]
  }

  manifest_path <- write_favourites_manifest(manifest, config)
  logger::log_info(
    "Rebuilt favourites image manifest from local cache: %d rows written to %s",
    nrow(manifest),
    manifest_path
  )

  invisible(nrow(manifest))
}


get_image_cache_log_run_id <- function() {
  if (exists("image_cache_log_run_id", envir = .GlobalEnv, inherits = FALSE)) {
    return(get("image_cache_log_run_id", envir = .GlobalEnv))
  }

  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
}


image_cache_log_path <- function(config, prefix = "image-cache") {
  file.path(
    config$env$dirs$logs,
    sprintf("%s-%s.log", prefix, get_image_cache_log_run_id())
  )
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
  favourites_manifest_path <- get_favourites_manifest_path(config)
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
    web_path <- image_cache_file_url(cached_file_path, config)

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
        period_folder <- image_cache_folder_name(period, NA_character_)

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
            species_folder <- image_cache_folder_name(sequence_obs$scientificName[obs_index], "Unknown")
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
    favourites_manifest_dir <- dirname(favourites_manifest_path)
    if (!dir.exists(favourites_manifest_dir)) {
      dir.create(favourites_manifest_dir, recursive = TRUE)
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
