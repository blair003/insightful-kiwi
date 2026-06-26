# highlights.R — "Highlights": a hero slider of curated favourite camera images (media$favorite),
# auto-advancing every few seconds. A friendly, low-jargon window into the monitoring data (the set
# includes pests, hence "Highlights", not "Favourites"). Data from ik_favourite_media_view();
# uncached favourites are pre-warmed into the media cache in the background on first view.

IK_HERO_MAX <- 30L      # newest N favourites in the slider (keeps the loop a sane length)
IK_HERO_MS  <- 5000L    # auto-advance interval (ms)

#' Build a Bootstrap-5 carousel from a favourites view (ik_favourite_media_view). @keywords internal
.hero_carousel <- function(df, ns, interval = IK_HERO_MS, n = IK_HERO_MAX) {
  df  <- utils::head(df, n)
  cid <- ns("carousel")
  show_ind <- nrow(df) <= 12                                   # dot indicators only when few; else arrows alone
  item <- function(i) {
    parts <- c(df$location[i], df$reserve[i]); parts <- parts[!is.na(parts) & nzchar(parts)]
    when  <- tryCatch(format(as.Date(df$timestamp[i]), "%d %b %Y"), error = function(e) "")
    sub   <- paste(c(paste(parts, collapse = " · "), when)[nzchar(c(paste(parts, collapse = " · "), when))], collapse = " — ")
    tags$div(class = paste0("carousel-item", if (i == 1) " active" else ""),
      tags$img(src = df$src[i], class = "d-block ik-hero-img", loading = "lazy",
               alt = df$label[i] %||% "Highlight"),
      tags$div(class = "carousel-caption ik-hero-cap",
        tags$h5(df$label[i] %||% "—"),
        if (nzchar(sub)) tags$p(sub)))
  }
  ind <- function(i) tags$button(type = "button", `data-bs-target` = paste0("#", cid),
    `data-bs-slide-to` = i - 1L, class = if (i == 1) "active",
    `aria-current` = if (i == 1) "true", `aria-label` = paste("Slide", i))
  ctrl <- function(dir, lab) tags$button(class = paste0("carousel-control-", dir), type = "button",
    `data-bs-target` = paste0("#", cid), `data-bs-slide` = dir,
    tags$span(class = paste0("carousel-control-", dir, "-icon"), `aria-hidden` = "true"),
    tags$span(class = "visually-hidden", lab))
  tags$div(id = cid, class = "carousel slide ik-hero-carousel",
    `data-bs-ride` = "carousel", `data-bs-interval` = interval, `data-bs-pause` = "hover",
    if (show_ind) tags$div(class = "carousel-indicators", lapply(seq_len(nrow(df)), ind)),
    tags$div(class = "carousel-inner", lapply(seq_len(nrow(df)), item)),
    ctrl("prev", "Previous"), ctrl("next", "Next"))
}

#' Highlights nav panel. @param id Module id. @param ik_data The container.
highlights_ui <- function(id, ik_data = NULL) {
  ns <- NS(id)
  nav_panel(
    "Highlights", value = "highlights", icon = icon("images"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/highlights.css")),
    tags$script(src = .ik_asset("js/highlights.js")),
    div(class = "ik-hero",
        .ik_page_header("Highlights",
            description = tagList("Favourite images from the cameras — the wildlife (and pests) caught on camera, ",
              "rotating automatically. Hover to pause; use the arrows to step through.")),
        uiOutput(ns("gallery")))
  )
}

#' Highlights server.
#' @param id Module id. @param ik_data The container.
#' @param active reactive: is this page's nav visible (gates the one-off background pre-warm).
highlights_server <- function(id, ik_data, active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    warmed <- reactiveVal(FALSE)
    observeEvent(active(), {                                   # pre-warm uncached favourites once, in the background
      if (!isTRUE(active()) || warmed()) return()
      warmed(TRUE)
      try(ik_cache_favourites_async(ik_data), silent = TRUE)
    })
    output$gallery <- renderUI({
      v <- ik_favourite_media_view(ik_data)
      if (is.null(v) || !nrow(v))
        return(tags$p(class = "ik-hero-empty", "No highlight images have been tagged yet."))
      vc <- v[v$cached, , drop = FALSE]                        # show only what's local — no live hits
      if (!nrow(vc))
        return(tags$p(class = "ik-hero-empty",
          "Highlights are downloading in the background — check back shortly."))
      .hero_carousel(vc, session$ns)
    })
  })
}
