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
