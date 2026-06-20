# selection.R — the shared data-selection sidebar. One reusable control set across the
# four user axes (period · geography · method · species); its server returns a reactive
# ik_select() result that any view can consume (Records is the first). Geography is a
# cascading set of grouped selects (reserve -> line -> location); species labels follow
# the Settings name preference. The model is in docs/data-model/04-data-selection.md.

#' Natural sort: numeric values by magnitude (so 2 < 10 < 192), any non-numeric entries
#' alphabetically after them. Used for line numbers, which are character. @keywords internal
nat_sort <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  x[order(is.na(n), n, x)]
}

#' Selection sidebar UI (drop into a sidebar / conditionalPanel).
#' @param id   Module id.
#' @param show Optional character vector of control ids to render, in order — any of
#'   `period`, `compare`, `reserve`, `line`, `location`, `device`, `species`, `net`. `NULL`
#'   (default) shows the six axis controls. The server is unaffected: an unrendered control
#'   reads as `NULL` (no filter), so a view can pick a subset without changing selection logic.
#' @param ik_data The ik_data container. Used here only to render the Period control already
#'   populated and pointing at its default season (`ik_default_period`), so it never passes
#'   through an empty → "all data" state on first load (which would double-resolve the view).
#'   The other axes' choices are still populated server-side. If `NULL`, Period renders empty.
#' @return A tagList of controls.
selection_ui <- function(id, show = NULL, ik_data = NULL) {
  ns <- NS(id)
  ctrls <- list(
    dataset  = selectizeInput(ns("dataset"), "Dataset", choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All datasets")),
    period   = selectInput(ns("period"), "Period",
                           choices  = if (!is.null(ik_data)) ik_period_choices(ik_data),
                           selected = if (!is.null(ik_data)) ik_default_period(ik_data)),
    compare  = selectInput(ns("compare"), "Compare to",
                           choices = c("No comparison" = "none", "Prior period" = "prior",
                                       "Same period last year" = "last_year")),
    reserve  = selectizeInput(ns("reserve"),  "Reserve",  choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All reserves")),
    line     = selectizeInput(ns("line"),     "Line",     choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All lines")),
    location = selectizeInput(ns("location"), "Location", choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All locations")),
    device   = selectizeInput(ns("device"),   "Device",   choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All devices")),
    species  = selectizeInput(ns("species"),  "Species",  choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All species")),
    # Duplicate handling is a data-subset choice (it removes rows), so it lives here in the
    # sidebar, not as an in-table filter. Camera-only (traps have no duplicate flag).
    net      = checkboxInput(ns("net"), "Exclude possible duplicates (net)", FALSE)
  )
  # default shows the six axis controls; `compare` and `net` are opt-in (request via `show`).
  ctrls <- ctrls[show %||% c("period", "reserve", "line", "location", "device", "species")]
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/selection.css"),
    div(class = "ik-selection", ctrls)
  )
}

#' Selection sidebar server.
#'
#' @param id                Module id.
#' @param ik_data           The ik_data container.
#' @param prefer_scientific A reactive returning TRUE to label species scientifically.
#' @param datasets Optional named id→label vector enabling the Dataset axis (e.g. for Records,
#'   which filters datasets INDEPENDENTLY of the global Settings toggle). When given, an empty
#'   pick means ALL datasets EXPLICITLY (so it overrides the toggle); NULL omits the axis.
#' @return A reactive returning `ik_select()` output (observations, deployments, effort).
selection_server <- function(id, ik_data, prefer_scientific, datasets = NULL) {
  moduleServer(id, function(input, output, session) {
    locs        <- ik_data$app$geography$locations
    dp          <- ik_deployment_period(ik_data)
    reserves    <- sort(unique(locs$reserve[!is.na(locs$reserve)]))
    devices     <- sort(unique(dp$source_type[!is.na(dp$source_type)]))   # device = source_type
    species_sci <- sort(unique(stats::na.omit(
      ik_observations(ik_data, with_location = FALSE)$scientificName)))

    # Period choices + default season are rendered in the UI (selection_ui), so the control
    # loads already pointing at the default and never round-trips through an empty → "all data"
    # state (which would resolve the view twice on first load). See ik_default_period().
    updateSelectizeInput(session, "reserve", choices = reserves, server = FALSE)
    updateSelectizeInput(session, "device",  choices = devices,  server = FALSE)
    if (!is.null(datasets))                                     # Dataset axis (Records)
      updateSelectizeInput(session, "dataset", choices = datasets, server = FALSE)

    # Locations restricted to the chosen reserve(s) (all when none chosen).
    reserve_locs <- reactive({
      l <- locs
      if (!is.null(.ik_nz(input$reserve))) l <- l[l$reserve %in% input$reserve, , drop = FALSE]
      l
    })

    # Cascade: lines within the chosen reserves.
    observeEvent(reserve_locs(), {
      lines <- nat_sort(unique(reserve_locs()$line[!is.na(reserve_locs()$line)]))
      updateSelectizeInput(session, "line", choices = lines,
                           selected = intersect(input$line, lines), server = FALSE)
    }, ignoreNULL = FALSE)

    # Cascade: locations within the chosen reserves + lines (value = id, label = name).
    # `isolate(input$location)` preserves the current pick WITHOUT depending on it — else
    # this observer would re-fire (and re-render the select) every time you pick a location.
    observe({
      l <- reserve_locs()
      if (!is.null(.ik_nz(input$line))) l <- l[l$line %in% input$line, , drop = FALSE]
      choices <- stats::setNames(l$location_id, l$name)
      # server-side: with ~1000+ locations, send matches as the user types rather than the whole
      # list to the browser (silences the selectize "large number of options" warning + faster).
      updateSelectizeInput(session, "location", choices = choices[order(names(choices))],
                           selected = intersect(isolate(input$location), l$location_id),
                           server = TRUE)
    })

    # Species labels follow the Settings name preference; values stay scientific names.
    # `isolate(input$species)` for the same reason — depend only on the name preference.
    observe({
      prefer  <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      labels  <- ik_species_label(species_sci, ik_data, prefer)
      choices <- stats::setNames(species_sci, labels)
      updateSelectizeInput(session, "species", choices = choices[order(names(choices))],
                           selected = isolate(input$species), server = FALSE)
    })

    # Return a reusable SPEC (axis values), not resolved data — a view resolves it with
    # ik_resolve(), optionally overriding an axis (e.g. the Overview splits by device).
    reactive(list(
      period      = input$period,                       # raw encoded value (for comparison)
      season      = ik_expand_period(input$period, ik_data),
      reserve     = input$reserve,
      line        = input$line,
      location    = input$location,
      source_type = input$device,
      species     = input$species,
      # Dataset axis active (Records): an empty pick = ALL datasets EXPLICITLY (overrides the
      # global toggle). Axis absent (other views): NULL → the global toggle applies.
      dataset     = if (!is.null(datasets))
                      (if (length(.ik_nz(input$dataset))) input$dataset else unname(datasets)) else NULL,
      compare     = input$compare,                      # NULL unless the control is shown
      net         = isTRUE(input$net)                    # exclude possible duplicates (camera)
    ))
  })
}
