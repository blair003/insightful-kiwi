# Generic current-view PDF export helpers.

pdf_export_file_url <- function(path) {
  normalized_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  paste0("file:///", utils::URLencode(normalized_path, reserved = FALSE))
}

sanitize_pdf_export_filename <- function(value) {
  value <- if (is.null(value) || !nzchar(value)) "current-view" else value
  value <- gsub("[^A-Za-z0-9_-]+", "-", value)
  value <- gsub("(^-+|-+$)", "", value)

  if (!nzchar(value)) {
    value <- "current-view"
  }

  value
}

write_pdf_from_image_file <- function(image_file, pdf_file, export_dir) {
  html_file <- file.path(export_dir, paste0(tools::file_path_sans_ext(basename(image_file)), ".html"))
  image_url <- pdf_export_file_url(image_file)
  image_info <- magick::image_info(magick::image_read(image_file))
  page_width <- 8.27
  page_height <- 11.69
  image_display_height <- page_width * image_info$height[1] / image_info$width[1]
  page_count <- max(1, ceiling(image_display_height / page_height))

  page_html <- paste(
    vapply(seq_len(page_count), function(page_index) {
      top_offset <- (page_index - 1) * page_height
      paste0(
        "<section class=\"page\">",
        sprintf(
          "<img src=\"%s\" alt=\"Exported application view\" style=\"top:-%.3fin;\">",
          image_url,
          top_offset
        ),
        "</section>"
      )
    }, character(1)),
    collapse = ""
  )

  html <- paste0(
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<style>",
    sprintf("@page{size:%.3fin %.3fin;margin:0;}", page_width, page_height),
    "html,body{margin:0;padding:0;background:#fff;}",
    sprintf(".page{width:%.3fin;height:%.3fin;overflow:hidden;position:relative;page-break-after:always;break-after:page;background:#fff;}", page_width, page_height),
    ".page:last-child{page-break-after:auto;break-after:auto;}",
    sprintf(".page img{display:block;position:absolute;left:0;width:%.3fin;height:auto;}", page_width),
    "</style></head><body>",
    page_html,
    "</body></html>"
  )

  writeLines(html, html_file, useBytes = TRUE)
  write_pdf_from_html(html_file, pdf_file)
}

write_pdf_from_html <- function(html_file, pdf_file) {
  session <- chromote::ChromoteSession$new()
  on.exit(
    try(session$close(), silent = TRUE),
    add = TRUE
  )

  session$Page$navigate(pdf_export_file_url(html_file), wait_ = FALSE)
  Sys.sleep(1)

  pdf_result <- session$Page$printToPDF(
    printBackground = TRUE,
    preferCSSPageSize = TRUE,
    marginTop = 0,
    marginRight = 0,
    marginBottom = 0,
    marginLeft = 0
  )

  writeBin(jsonlite::base64_dec(pdf_result$data), pdf_file)
}

export_view_image_to_pdf <- function(screenshot_data_url, pdf_file, export_dir) {
  if (is.null(screenshot_data_url) || !nzchar(screenshot_data_url)) {
    stop("PDF export failed: the browser did not provide a page screenshot.")
  }

  png_prefix <- "data:image/png;base64,"
  if (!startsWith(screenshot_data_url, png_prefix)) {
    stop("PDF export failed: browser screenshot was not a PNG data URL.")
  }

  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  export_id <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample.int(999999, 1))
  image_file <- file.path(export_dir, paste0("view-export-", export_id, ".png"))

  image_data <- sub(paste0("^", png_prefix), "", screenshot_data_url)
  writeBin(jsonlite::base64_dec(image_data), image_file)

  if (!file.exists(image_file)) {
    stop("PDF export failed: page screenshot was not created.")
  }

  write_pdf_from_image_file(image_file, pdf_file, export_dir)

  if (!file.exists(pdf_file)) {
    stop("PDF export failed: PDF file was not created.")
  }

  invisible(pdf_file)
}

pdf_export_www_url <- function(path) {
  normalized_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  www_root <- normalizePath("www", winslash = "/", mustWork = TRUE)

  if (!startsWith(normalized_path, paste0(www_root, "/"))) {
    stop("PDF export failed: generated image is not under www.")
  }

  sub(paste0("^", www_root, "/"), "", normalized_path)
}

create_pdf_export_density_map <- function(active_locations, obs_summary_location, show_zero = TRUE, width = NULL, height = NULL) {
  max_scale <- 1
  radius_range <- c(10, 50)

  if (is.null(active_locations)) {
    active_locations <- data.frame()
  }
  if (is.null(obs_summary_location)) {
    obs_summary_location <- data.frame()
  }

  if (nrow(obs_summary_location) > 0) {
    max_count <- max(obs_summary_location$count, na.rm = TRUE)
  } else {
    max_count <- NA
  }

  if (show_zero && nrow(active_locations) > 0) {
    zero_entries <- active_locations %>%
      dplyr::anti_join(obs_summary_location, by = "locationID") %>%
      dplyr::mutate(count = 0)

    obs_summary_location <- dplyr::bind_rows(obs_summary_location, zero_entries)
  }

  if (!"count" %in% names(obs_summary_location)) {
    obs_summary_location$count <- numeric(0)
  }

  obs_summary_location <- obs_summary_location %>%
    dplyr::mutate(
      radius = ifelse(
        count > 0 & !is.na(max_count) & max_count > 0,
        scales::rescale(count, to = radius_range, from = c(0, max_count)),
        radius_range[1]
      )
    )

  pal <- leaflet::colorNumeric(palette = "inferno", domain = obs_summary_location$count)
  map <- leaflet::leaflet(width = width, height = height) %>%
    leaflet::addTiles(options = leaflet::tileOptions(crossOrigin = TRUE))

  non_zero_locations <- obs_summary_location %>% dplyr::filter(count > 0)
  if (nrow(non_zero_locations) > 0) {
    map <- map %>%
      leaflet::addCircleMarkers(
        data = non_zero_locations,
        lng = ~longitude,
        lat = ~latitude,
        radius = ~radius * max_scale,
        fillColor = ~pal(count),
        fillOpacity = 0.8,
        stroke = FALSE
      )
  }

  if (show_zero && nrow(obs_summary_location) > 0) {
    zero_locations <- obs_summary_location %>% dplyr::filter(count == 0)
    if (nrow(zero_locations) > 0) {
      map <- map %>%
        leaflet::addMarkers(
          data = zero_locations,
          lng = ~longitude,
          lat = ~latitude,
          icon = get_species_icon(species = "none"),
          popup = ~paste(locationName, "<br>Count: 0")
        )
    }
  }

  if (!is.na(max_count) && max_count > 0) {
    map <- map %>%
      leaflet::addLegend(
        "bottomright",
        pal = pal,
        values = obs_summary_location$count,
        title = "Number of individuals",
        labFormat = leaflet::labelFormat(),
        opacity = 1
      )
  }

  if (nrow(active_locations) > 0 &&
      all(c("longitude", "latitude") %in% names(active_locations)) &&
      !any(is.na(active_locations$longitude)) &&
      !any(is.na(active_locations$latitude))) {
    map <- map %>%
      leaflet::fitBounds(
        lng1 = min(active_locations$longitude, na.rm = TRUE),
        lat1 = min(active_locations$latitude, na.rm = TRUE),
        lng2 = max(active_locations$longitude, na.rm = TRUE),
        lat2 = max(active_locations$latitude, na.rm = TRUE)
      )
  }

  map
}

render_pdf_export_leaflet_png <- function(map, map_id, export_dir, width = NULL, height = NULL) {
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  public_dir <- file.path("www", "cache", "pdf_export_maps")
  dir.create(public_dir, recursive = TRUE, showWarnings = FALSE)

  export_id <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample.int(999999, 1))
  safe_map_id <- sanitize_pdf_export_filename(map_id)
  html_file <- file.path(export_dir, paste0("leaflet-", safe_map_id, "-", export_id, ".html"))
  png_file <- file.path(public_dir, paste0("leaflet-", safe_map_id, "-", export_id, ".png"))

  suppressWarnings(
    htmlwidgets::saveWidget(
      map,
      html_file,
      selfcontained = FALSE,
      libdir = paste0(tools::file_path_sans_ext(basename(html_file)), "_libs")
    )
  )

  if (!file.exists(html_file)) {
    stop("PDF export failed: Leaflet HTML file was not created.")
  }

  shot_width <- if (is.null(width) || is.na(width)) 1200 else as.integer(width)
  shot_height <- if (is.null(height) || is.na(height)) 700 else as.integer(height)

  webshot2::webshot(
    url = html_file,
    file = png_file,
    vwidth = max(320, shot_width),
    vheight = max(240, shot_height),
    delay = 2
  )

  if (!file.exists(png_file)) {
    stop("PDF export failed: Leaflet PNG file was not created.")
  }

  list(file = png_file, src = pdf_export_www_url(png_file))
}
