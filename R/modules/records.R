# records.R — Records view: browse observations as a table with sane default
# columns and user-toggleable column visibility (DT colvis button). Species names
# follow the Settings name preference. Shiny module: records_ui() / records_server().

# Column spec: source column -> display header, in table order. The first
# RECORDS_DEFAULT_COLS are shown by default; the rest are available via the table's
# "Columns" button. Columns not listed here are not offered (noise — see the
# sketch in the build notes). `Species` is derived (see records_server).
RECORDS_COLUMNS <- c(
  eventEnd                  = "When",   # check date (trap) / detection end (camera)
  Species                   = "Species",
  count                     = "Count",
  observationType           = "Type",
  source_type               = "Device",
  locationName              = "Location",
  # --- available, hidden by default ---
  dataset                   = "Dataset",
  eventStart                = "Event start",
  scientificName            = "Scientific name",
  lifeStage                 = "Life stage",
  sex                       = "Sex",
  behavior                  = "Behaviour",
  cameraSetupType           = "Camera setup",
  deploymentID              = "Deployment ID",
  latitude                  = "Latitude",
  longitude                 = "Longitude",
  individualID              = "Individual",
  classificationMethod      = "Classification method",
  classifiedBy              = "Classified by",
  classificationProbability = "Classification prob.",
  observationComments       = "Comments",
  possible_duplicate              = "Possible duplicate",   # from app$relations (camera only)
  minutes_since_prev_same_species = "Mins since prev",
  observationID             = "Observation ID"
)
RECORDS_DEFAULT_COLS <- c("eventEnd", "Species", "count", "observationType",
                          "source_type", "locationName")

# Low-cardinality columns rendered as factors so DT's column filter is a dropdown.
RECORDS_FACTOR_COLS <- c("Type", "Device", "Dataset")

# Datetime columns are pre-formatted to LOCAL text for display. DT serializes a
# POSIXct as UTC (adds a Z and shifts the clock), which undoes the timezone
# correction — so we format the corrected wall-clock to a string ourselves. Trade:
# DT then text-filters this column instead of a date-range widget (a proper date
# filter belongs in the sidebar). Date-only when the local time is exactly midnight
# (date-only sources like traps), else date + time:
# "2024-01-10 13:37:40" (camera) / "2023-01-30" (trap).
RECORDS_DATETIME_COLS <- c("When", "Event start")

format_records_datetime <- function(x) {
  ifelse(
    is.na(x), NA_character_,
    ifelse(format(x, "%H:%M:%S") == "00:00:00",
           format(x, "%Y-%m-%d"),
           format(x, "%Y-%m-%d %H:%M:%S"))
  )
}

#' Records nav panel UI.
#'
#' @param id Module id.
#' @return A bslib nav_panel for page_navbar().
records_ui <- function(id, label = "Records", value = "records", heading = NULL) {
  ns <- NS(id)
  nav_panel(
    label, value = value, icon = icon("table"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/observation.css")),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/records.css")),
    .ik_titlebar(tags$h3(heading %||% label)),
    DT::DTOutput(ns("table"))
  )
}

#' Records server.
#'
#' @param id                Module id.
#' @param ik_data           The ik_data container.
#' @param prefer_scientific A reactive returning TRUE to show scientific names.
#' @param selection         A reactive returning the selection SPEC (sidebar); resolved
#'   here via `ik_resolve()` — its `$observations` drive the table.
records_server <- function(id, ik_data, prefer_scientific, selection) {
  moduleServer(id, function(input, output, session) {

    # The selected observations + location, with the Species display column applied,
    # then narrowed/renamed to the offered columns. Reactive on selection + name pref.
    records <- reactive({
      obs    <- ik_resolve(ik_data, selection())$observations   # resolve the spec
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      obs$Species <- ik_species_label(obs$scientificName, ik_data, prefer)
      # trap "no capture" (observationType "blank") reads as "Empty"; camera blanks unchanged
      obs$observationType <- ik_obs_type_label(obs$observationType,
                                               !is.na(obs$source_type) & obs$source_type == "trap")

      # surface the duplicate metrics (app$relations; camera/minute-resolution only). For trap-only
      # data these relation columns are absent/non-numeric, so guard before rounding/flagging.
      rel <- ik_relations(ik_data)
      mi  <- match(obs$observationID, rel$observationID)
      dup <- rel$possible_duplicate[mi]; gap <- rel$minutes_since_prev_same_species[mi]
      obs$possible_duplicate              <- if (is.logical(dup)) dup else NA
      obs$minutes_since_prev_same_species <- if (is.numeric(gap)) round(gap, 1) else NA_real_

      # net view (sidebar toggle): drop flagged duplicates so the records match the RAI
      # net individuals. Camera-only — traps have no duplicate flag, so they're unaffected.
      if (isTRUE(selection()$net)) {
        obs <- obs[is.na(obs$possible_duplicate) | !obs$possible_duplicate, , drop = FALSE]
      }

      present <- intersect(names(RECORDS_COLUMNS), names(obs))
      out <- obs[, present, drop = FALSE]
      names(out) <- RECORDS_COLUMNS[present]
      for (col in intersect(RECORDS_DATETIME_COLS, names(out))) {
        out[[col]] <- format_records_datetime(out[[col]])
      }
      for (col in intersect(RECORDS_FACTOR_COLS, names(out))) {
        out[[col]] <- as.factor(out[[col]])
      }
      out
    })

    # Build the widget ONCE (isolate), then push new rows via the proxy on each
    # selection / name-preference change. A full re-render per change flickers the
    # column filters and silently resets the user's column visibility + in-table
    # filters — replaceData() keeps all of that and avoids the flash.
    output$table <- DT::renderDT(server = TRUE, {
      df <- isolate(records())

      default_headers <- unname(RECORDS_COLUMNS[RECORDS_DEFAULT_COLS])
      hidden <- which(!names(df) %in% default_headers) - 1L  # 0-based for DT

      DT::datatable(
        df,
        rownames   = FALSE,
        filter     = "top",
        selection  = "single",                    # row click → observation viewer
        class      = "stripe hover row-border ik-row-click",
        extensions = "Buttons",
        options = list(
          # Columns button + global search share one toolbar row (button left, search right); the
          # button is styled quiet via .ik-colvis-btn (records.css) so it doesn't shout.
          dom          = "<'records-toolbar'Bf>rtip",
          buttons      = list(list(extend = "colvis", text = "Columns", className = "ik-colvis-btn")),
          columnDefs   = list(
            list(visible = FALSE, targets = hidden),
            list(className = "ik-nowrap",                          # datetime cells stay one line
                 targets = which(names(df) %in% RECORDS_DATETIME_COLS) - 1L)),
          pageLength   = 25,
          scrollX      = TRUE
        )
      )
    })

    proxy <- DT::dataTableProxy("table")
    observeEvent(records(), DT::replaceData(proxy, records(), rownames = FALSE),
                 ignoreInit = TRUE)

    # Row click → open the observation viewer for that record, then clear the selection so
    # the same row can be reopened. The hidden "Observation ID" column carries the id.
    observeEvent(input$table_rows_selected, {
      i <- input$table_rows_selected
      df <- records()
      if (length(i) && "Observation ID" %in% names(df)) {
        show_observation_modal(ik_data, df[["Observation ID"]][i], isTRUE(prefer_scientific()))
      }
      DT::selectRows(proxy, NULL)
    }, ignoreInit = TRUE)
  })
}
