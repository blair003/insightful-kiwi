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
selection_ui <- function(id, show = NULL, ik_data = NULL, period_default = NULL, device_default = NULL) {
  ns <- NS(id)
  dev_choices <- if (!is.null(ik_data))                         # device = source_type; baked here so the
    stats::setNames(sort(unique(vapply(ik_data$datasets,        # default survives while the sidebar panel
      function(d) d$meta$source_type %||% NA_character_, character(1)))), NULL) else NULL  # is hidden
  if (length(dev_choices)) dev_choices <- stats::setNames(dev_choices, tools::toTitleCase(dev_choices))
  ctrls <- list(
    dataset  = selectizeInput(ns("dataset"), "Dataset", choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All available")),
    period   = selectInput(ns("period"), "Period",
                           choices  = if (!is.null(ik_data)) ik_period_choices(ik_data),
                           selected = if (!is.null(ik_data)) period_default %||% ik_default_period(ik_data)),
    compare  = selectInput(ns("compare"), "Compare to",
                           choices = c("No comparison" = "none", "Prior period" = "prior",
                                       "Same period last year" = "last_year"),
                           selected = if (!is.null(ik_data)) ik_data$meta$overview$default_compare %||% "none" else "none"),
    reserve  = selectizeInput(ns("reserve"),  "Reserve",  choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All reserves")),
    line     = selectizeInput(ns("line"),     "Line",     choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All lines")),
    location = selectizeInput(ns("location"), "Location", choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All locations")),
    device   = selectizeInput(ns("device"),   "Device",   choices = dev_choices, selected = device_default,
                              multiple = TRUE, options = list(placeholder = "All devices")),
    species  = selectizeInput(ns("species"),  "Species",  choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All species")),
    # Duplicate handling is a data-subset choice (it removes rows), so it lives here in the
    # sidebar, not as an in-table filter. Camera-only (traps have no duplicate flag).
    net      = checkboxInput(ns("net"), "Exclude possible duplicates observations", FALSE)
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
#' @param datasets Optional named id→label vector enabling the Dataset axis (Records). The axis is
#'   a sub-filter WITHIN the datasets currently shown in global Settings: its choices track the
#'   active set, and an empty pick = "All available" (the active datasets, like every other view).
#'   NULL omits the axis.
#' @return A reactive returning `ik_select()` output (observations, deployments, effort).
selection_server <- function(id, ik_data, prefer_scientific, datasets = NULL, species_default = NULL) {
  moduleServer(id, function(input, output, session) {
    locs        <- ik_data$app$geography$locations
    reserves    <- sort(unique(locs$reserve[!is.na(locs$reserve)]))
    group_map   <- ik_group_taxa(ik_data)   # label -> sci, for resolving the unified Species picker

    # Period choices + default season are rendered in the UI (selection_ui), so the control
    # loads already pointing at the default and never round-trips through an empty → "all data"
    # state (which would resolve the view twice on first load). See ik_default_period().
    updateSelectizeInput(session, "reserve", choices = reserves, server = FALSE)
    # device choices + default are baked in the UI (selection_ui) so they survive while the sidebar
    # conditionalPanel is hidden — no server-side device update here.
    # Dataset axis (Records): list only the datasets ACTIVE in the global Settings toggle, so the
    # filter respects show/hide like every other view; re-fires when the toggle changes (keeping
    # any pick that's still available). Empty pick = all available (handled in the spec below).
    if (!is.null(datasets)) observe({
      active <- ik_active_datasets() %||% unname(datasets)     # reactive; NULL (no toggle) → all
      avail  <- datasets[unname(datasets) %in% active]         # named id→label, active only
      updateSelectizeInput(session, "dataset", choices = avail,
                           selected = intersect(isolate(input$dataset), unname(avail)), server = FALSE)
    })

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

    # Unified Species picker: configured groups (split per project flag) + every ungrouped species;
    # values grp:/sci:. Labels follow the Settings name preference (relabel on change; keep the pick).
    observe({
      prefer  <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      cur     <- isolate(input$species)
      updateSelectizeInput(session, "species", choices = ik_species_choices_full(ik_data, prefer),
                           selected = if (length(cur)) cur else species_default, server = FALSE)
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
      # Species: resolve the grp:/sci: picks to scientificNames for filtering (ik_select), and also
      # pass the raw choice so a view that GROUPS by species (the map surface) can rebuild its taxa.
      species        = ik_resolve_species_choice(input$species, group_map),
      species_choice = input$species,
      # Dataset axis (Records): a non-empty pick narrows to those datasets; an empty pick =
      # "All available" → NULL, which lets the ambient global toggle (ik_active_datasets) apply,
      # exactly like every other view. Axis absent (other views): also NULL.
      dataset     = if (!is.null(datasets)) .ik_nz(input$dataset) else NULL,
      compare     = input$compare,                      # NULL unless the control is shown
      net         = isTRUE(input$net)                    # exclude possible duplicates (camera)
    ))
  })
}
