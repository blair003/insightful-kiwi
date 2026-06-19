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
  source_type               = "Source",
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
  classificationProbability = "Classification prob.",
  observationComments       = "Comments",
  observationID             = "Observation ID"
)
RECORDS_DEFAULT_COLS <- c("eventEnd", "Species", "count", "observationType",
                          "source_type", "locationName")

# Low-cardinality columns rendered as factors so DT's column filter is a dropdown.
RECORDS_FACTOR_COLS <- c("Type", "Source", "Dataset")

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
records_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Records", value = "records", icon = icon("table"),
    DT::DTOutput(ns("table"))
  )
}

#' Records server.
#'
#' @param id                Module id.
#' @param ik_data           The ik_data container.
#' @param prefer_scientific A reactive returning TRUE to show scientific names.
records_server <- function(id, ik_data, prefer_scientific) {
  moduleServer(id, function(input, output, session) {

    # Observations + location, with the Species display column applied, then
    # narrowed/renamed to the offered columns. Reactive on the name preference.
    records <- reactive({
      obs    <- ik_observations(ik_data)   # all datasets, unified
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      obs$Species <- ik_species_label(obs$scientificName, ik_data, prefer)

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

    output$table <- DT::renderDT(server = TRUE, {
      df <- records()

      default_headers <- unname(RECORDS_COLUMNS[RECORDS_DEFAULT_COLS])
      hidden <- which(!names(df) %in% default_headers) - 1L  # 0-based for DT

      DT::datatable(
        df,
        rownames   = FALSE,
        filter     = "top",
        extensions = "Buttons",
        options = list(
          dom          = "Bfrtip",
          buttons      = list(list(extend = "colvis", text = "Columns")),
          columnDefs   = list(list(visible = FALSE, targets = hidden)),
          pageLength   = 25,
          scrollX      = TRUE
        )
      )
    })
  })
}
