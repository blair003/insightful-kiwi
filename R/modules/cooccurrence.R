# cooccurrence.R (module) — "Co-occurrence": protected ↔ predator timing on camera. How close in
# time do protected species and predators share the same camera? A gap-bucket distribution (click a
# bar → the detections behind it → the record viewer) plus a seasonal trend of the median gap
# (rising = they're separating in time, e.g. trapping working). A deeper analysis than the
# everyone-can-read "Are we winning?", so it sits on its own page under the Outcomes menu.
# Camera-only; both sides selectable from species_groups roles, defaulting to the highest-concern
# group of each role (species_groups order). Data from ik_predator_protected_gaps().

GAP_BREAKS <- c(0, 1, 6, 24, 168, 720, Inf)                                  # hours
GAP_LABELS <- c("< 1 h", "1–6 h", "6–24 h", "1–7 d", "1–4 wk", "> 1 mo")

#' "How to read this" help body for Co-occurrence — a tabbed walkthrough. @keywords internal
cooccurrence_help_body <- function() {
  P <- function(...) tags$p(...)
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "For every ", tags$b("protected"), "species detection on camera, this measures how long ",
        "until the nearest ", tags$b("predator"), " detection — at the ", tags$b("same camera"), ", or, if you ",
        "set a ", tags$b("Within"), " radius, anywhere in the camera's ", tags$b("neighbourhood"),
        " (the camera plus that radius around it) — the ", tags$b("time gap"),
        " between the two species sharing the same ground."),
      P("That neighbourhood reuses the Coverage map's proximity engine: cameras are sparse, so ",
        "“same camera” is strict — a wider radius counts a predator seen a few hundred metres away as nearby."),
      P("Two views: a ", tags$b("distribution"), " of those gaps (how often predator and protected are close ",
        "in time vs far apart) and a ", tags$b("seasonal trend"), " of the median gap. Click a distribution ",
        "bar to see the detections behind it, down to the record."),
      P("The default predator is your project's ", tags$b("highest-concern"), " one — switch to the ",
        "predator that matters to you.")),
    tabPanel(
      "Reading it", icon = icon("chart-column"),
      P(tags$br(), tags$b("Short gaps"), " (left of the distribution) mean the two species share the same ",
        "ground close in time; ", tags$b("long gaps"), " mean they mostly don't overlap."),
      tags$ul(
        tags$li("The ", tags$b("median gap by season"), " ", tags$b("rising"), " over time ⇒ they're ",
                "separating in time — what you'd hope to see where control is thinning predators or shifting ",
                "their behaviour."),
        tags$li(tags$b("Falling / flat"), " ⇒ predator and protected are overlapping as much as ever."),
        tags$li(tags$b("Predator after only"), " restricts to gaps where the predator arrives ", tags$em("after"),
                " the protected animal — the “stalking” direction (following), rather than either order."),
        tags$li(tags$b("Ground it against RAI"), " — a rising gap can simply mean fewer detections of one ",
                "species, not real separation. Overlay each species' RAI on the Trend (right axis) to read ",
                "the gap against how much they're actually being seen.")),
      P("It's ", tags$b("exploratory"), ": sharing a camera close in time is suggestive of risk, not proof of ",
        "a kill or even a meeting. And once the median gap reaches ", tags$b("weeks or more"), ", the two barely ",
        "share ground at all — treat the trend as a weak signal there.")),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Pairing"), " — each protected detection is matched to the ", tags$b("nearest in time"),
                " predator detection at the same camera, or — with a ", tags$b("Within"), " radius — at any camera ",
                "within it; the gap is the hours between them, and the modal shows how far apart the cameras were."),
        tags$li(tags$b("Direction"), " — by default the nearest predator detection either side counts; ",
                tags$b("Predator after only"), " keeps just those where the predator came later."),
        tags$li(tags$b("Beyond the reserve"), " — with a reserve selected, ",
                tags$b("Include nearby cameras beyond the reserve"), " (on by default, needs a Within radius) lets a ",
                "protected detection pair with a predator on a camera in a neighbouring reserve within that radius ",
                "(a boundary buffer); off = strict to the reserve."),
        tags$li(tags$b("Buckets"), " — gaps are grouped < 1 h · 1–6 h · 6–24 h · 1–7 d · 1–4 wk · > 1 mo."),
        tags$li(tags$b("Seasonal median"), " — within each season, the median gap (robust to a few extreme ",
                "values), plotted over time."),
        tags$li(tags$b("Needs both in range"), " — a protected detection only forms a pair if a predator was ",
                "seen at the same camera (or within the radius); an isolated detection never pairs.")),
      P(tags$em("Camera timestamps must be true clock times for this (trapping has no clock time, so it's ",
                "camera-only). Treat proximity in time as co-occurrence risk, not a confirmed interaction.")))
  )
}

#' The Co-occurrence "View options" controls (Predator / Protected / Within / Predator-after). Built here
#' but rendered in the GLOBAL sidebar (ui.R) as the tinted feature-control group at the TOP of the rail,
#' above the shared Filters (Period/Reserve) — the server reads them by this module's namespace wherever
#' they sit. Choices for the species pickers are populated server-side. Matches the Maps page's
#' `.ik-view-controls` group (maps_controls). @keywords internal
cooccurrence_controls <- function(id) {
  ns <- NS(id)
  # `.ik-selection` matches the main controls' label/spacing; `.ik-view-controls` makes it the tinted,
  # accent-bordered "View options" group (same as the Maps page). Pickers are selectize, populated server-side.
  div(class = "ik-selection ik-view-controls",
    tags$div(class = "ik-view-controls-h", "View options"),
    selectizeInput(ns("pred"), "Predator",  choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Choose predator(s)")),
    selectizeInput(ns("prot"), "Protected", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Choose protected")),
    selectInput(ns("radius"), "Within",
                choices = c("Same camera" = 0, "250 m" = 250, "500 m" = 500, "750 m" = 750, "1 km" = 1000),
                selected = 0),
    .ik_cross_boundary_input(ns("cross_boundary"), "cameras"),
    checkboxInput(ns("after_only"),
      tagList("Predator ", tags$b("after"), " only ",
              .ik_hint("Stalking — count only predators that arrive AFTER the protected animal (predator follows).")),
      value = FALSE))
}

#' Co-occurrence nav panel — Distribution / Map / Trend tabs over the predator↔protected gaps. The
#' filters (Period, Reserve, Predator, Protected, Within, Predator-after) live in the sidebar; the
#' Distribution + Map honour the selected Period, the Trend spans all seasons. @param id Module id.
cooccurrence_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Co-occurrence", value = "cooccurrence", icon = icon("hourglass-half"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/cooccurrence.css")),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),
    tags$script(src = .ik_asset("js/maps.js")),                       # leaflet resize-on-tab-show
    div(class = "ik-cooc",
        .ik_page_header("Co-occurrence",
            description = "How close in time do predators and protected species share the same ground?",
            help = .ik_info(ns("cooc_help"), "Co-occurrence — how to read this", cooccurrence_help_body()),
            banner = div(class = "ik-page-period", uiOutput(ns("period_banner")))),
        tabsetPanel(
          id = ns("cooc_view"), type = "tabs",
          tabPanel("Distribution", icon = icon("chart-column"),
            uiOutput(ns("intro")),
            tags$p(class = "ik-cooc-hint", tags$b("Click a bar"), " for the detections behind it."),
            plotOutput(ns("dist"), height = "340px", click = ns("dist_click"))),
          tabPanel("Map", icon = icon("map-location-dot"),
            uiOutput(ns("map_intro")),
            # Map beside the records table (wraps to stacked on narrow) — same side-by-side as the Maps page.
            layout_columns(
              class = "ik-maps-split", col_widths = breakpoints(sm = 12, lg = c(8, 4)),
              leaflet::leafletOutput(ns("map"), height = "60vh"),
              div(class = "ik-maps-side", style = "max-height:60vh;",
                div(style = "margin-bottom:0.45rem;",
                  downloadButton(ns("dl_pairs"), "Download pairs (CSV)", class = "btn-sm")),
                DT::DTOutput(ns("map_table"))))),
          tabPanel("Trend", icon = icon("chart-line"),
            uiOutput(ns("trend_intro")),
            div(class = "ik-cooc-trend-ctrl",
                checkboxGroupInput(ns("trend_rai"), inline = TRUE,
                  label = tagList("Overlay RAI ",
                    .ik_hint(paste0("Ground the gap trend against how much each species is actually being seen. A gap can ",
                      "rise simply because the protected species is thinning out (or the predator is) — not because they're ",
                      "truly separating. RAI shares one right-hand axis; ± SE shown as a shadow."))),
                  choices = c("Protected" = "prot", "Predator" = "pred"), selected = character(0))),  # off by default — read the gap first
            plotOutput(ns("trend"), height = "360px"))))
  )
}

#' Wire the co-occurrence "click a bar → co-detections" drill, SHARED by the standalone Co-occurrence
#' page and each Species page's Co-occurrence tab so they behave identically: a 2-tab modal
#' (Detections → Record details) whose Detections table links BOTH species — protected AND predator —
#' separately to their own record, shows the gap WITH direction (predator earlier / later / same time),
#' and — in a proximity (Within radius) view — which camera the predator was at and how far. Registers
#' its outputs/observers once on the module; returns an opener `function(rows, title, subtitle)` to
#' call from the bar-click handler. `rows` is the ik_predator_protected_gaps() subset for the clicked
#' bucket (always protected-anchored: observationID/when = protected, pred_id/pred_when = predator).
#' @param prefer reactive name-preference. @param radius_m reactive metres (drives the proximity cols).
#' @keywords internal
ik_cooc_drill <- function(input, output, session, ik_data, prefer, radius_m = reactive(0)) {
  locs    <- ik_data$app$geography$locations
  rows_rv <- reactiveVal(NULL)
  rec_obs <- reactiveVal(NULL)
  .fmt_gap <- function(h) if (h < 1) sprintf("%d min", round(h * 60)) else
    if (h < 48) sprintf("%.1f h", h) else sprintf("%.1f d", h / 24)
  .fmt_m   <- function(m) if (is.na(m)) "—" else if (m >= 1000) sprintf("%.1f km", m / 1000) else sprintf("%.0f m", m)

  output$cooc_bucket_table <- DT::renderDT({
    rows <- rows_rv(); validate(need(!is.null(rows) && nrow(rows), "No detections."))
    .lnk <- function(label, id) sprintf(                              # species cell → open that record
      "<a class='ik-link' onclick=\"Shiny.setInputValue('%s',{id:'%s'},{priority:'event'})\">%s</a>",
      session$ns("cooc_view_obs"), id, htmltools::htmlEscape(label))
    dir <- ifelse(rows$signed_h > 0, "later", ifelse(rows$signed_h < 0, "earlier", "same time"))
    df <- data.frame(
      Camera = locs$name[match(rows$location_id, locs$location_id)],
      Protected = mapply(.lnk, ik_species_label(rows$scientificName, ik_data, prefer()), rows$observationID),
      `Protected seen` = format(rows$when, "%d %b %Y · %H:%M"),
      Predator = mapply(.lnk, ik_species_label(rows$pred_sci, ik_data, prefer()), rows$pred_id),
      `Predator seen` = format(rows$pred_when, "%d %b %Y · %H:%M"),
      check.names = FALSE, stringsAsFactors = FALSE)
    if (isTRUE(radius_m() > 0) && "pred_dist_m" %in% names(rows)) {    # proximity view: where the predator was
      df$`Predator camera` <- locs$name[match(rows$pred_loc, locs$location_id)]
      df$Apart <- ifelse(rows$pred_dist_m <= 0, "same camera", vapply(rows$pred_dist_m, .fmt_m, character(1)))
    }
    df$Gap <- paste0("predator ", vapply(rows$gap_h, .fmt_gap, character(1)), " ", dir)
    esc <- -which(names(df) %in% c("Protected", "Predator"))         # leave the link cells un-escaped
    DT::datatable(df, rownames = FALSE, selection = "none", escape = esc,
      class = "stripe hover row-border",
      options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE))
  })

  # Click either species → load it in the Record-details tab and switch (no nested modal, so the
  # Detections list is still there to come back to).
  observeEvent(input$cooc_view_obs, {
    id <- input$cooc_view_obs$id
    if (length(id) && nzchar(id)) {
      rec_obs(id); showTab(session = session, inputId = "cooc_tabs", target = "Record details", select = TRUE)
    }
  })
  observeEvent(input$cooc_tab_back, updateTabsetPanel(session, input$cooc_tab_back$tabset, selected = input$cooc_tab_back$to))

  output$cooc_record <- renderUI({
    if (is.null(rec_obs()))
      return(tags$p(class = "ik-cooc-lead", "Click a species in the Detections tab to see its record here."))
    ob <- ik_observation(ik_data, rec_obs()); if (is.null(ob)) return(tags$p("Record not found."))
    tagList(.ik_tab_back(session$ns("cooc_tab_back"), "cooc_tabs", "Detections", "Back to detections"),
            .ovw_title(ik_data, ob, prefer()),
            .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("cooc_rec_subtabs")))
  })

  function(rows, title, subtitle = NULL) {
    rows_rv(rows[order(rows$gap_h), , drop = FALSE]); rec_obs(NULL)
    showModal(modalDialog(
      title = .ik_modal_title(title, subtitle), size = "l", easyClose = TRUE, footer = modalButton("Close"),
      tabsetPanel(id = session$ns("cooc_tabs"),
        tabPanel("Detections",     icon = icon("list"),        DT::dataTableOutput(session$ns("cooc_bucket_table"))),
        tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("cooc_record"))))))
    hideTab(session = session, inputId = "cooc_tabs", target = "Record details")   # appears on first drill
  }
}

# ============================ Shared Co-occurrence view builders ============================
# Distribution / Map / Trend are shared by the standalone Co-occurrence page AND each Species page's
# Co-occurrence tab, so both behave identically. The plot builders are PURE (resolved values in → a ggplot
# or a validation-message string out); the Map registers its own outputs/observers, so it also takes
# (input, output, session) plus the reactives it needs. ids let a caller pick its own output ids (the
# standalone uses "map"/…, a species page "cooc_map"/… since "map"/"trend" are taken by its own tabs).

#' Distribution bar chart of predator↔protected gap buckets. @param g gaps df with a $bucket factor.
#' @param keep bucket levels kept (within the pairing window). @keywords internal
ik_cooc_dist_plot <- function(g, x_lab, is_dark, keep, fill = "#6a3d9a", y_lab = "Protected detections") {
  d <- as.data.frame(table(bucket = g$bucket))
  d <- d[d$bucket %in% keep, , drop = FALSE]
  d$bucket <- factor(d$bucket, levels = keep)
  ggplot2::ggplot(d, ggplot2::aes(.data$bucket, .data$Freq)) +
    ggplot2::geom_col(fill = fill, width = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Freq), vjust = -0.3, size = 3.6, colour = ik_plot_ink(is_dark)) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ik_ggtheme(is_dark) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
}

#' Seasonal median-gap trend, optionally overlaying protected/predator RAI on a shared right axis. Returns
#' a ggplot, or a character validation message when there's nothing (or too few seasons) to plot.
#' @param g all-seasons gaps df. @param reserve reserve scope for the RAI overlay (NULL = all).
#' @param rai_specs named list keyed "prot"/"pred", each list(sci = <scientificNames>). @param rai_want
#'   which of those to overlay. @keywords internal
ik_cooc_trend_plot <- function(g, ik_data, is_dark, prefer, reserve = NULL,
                               rai_specs = list(), rai_want = character(0)) {
  if (is.null(g) || !nrow(g)) return("No co-detections to chart.")
  op <- ik_observation_period(ik_data)                         # deployment-anchored season (camera)
  g$season <- op$calendar_season[match(g$observationID, op$observationID)]
  g <- g[!is.na(g$season), , drop = FALSE]
  if (!nrow(g)) return("No co-detections to chart.")
  ag <- do.call(rbind, lapply(split(g, g$season), function(s)
    data.frame(season = s$season[1], med = stats::median(s$gap_h), t = mean(as.numeric(s$when)), n = nrow(s))))
  if (is.null(ag) || nrow(ag) < 2) return("Need at least two seasons for a trend.")
  ag <- ag[order(ag$t), ]; ag$season <- factor(ag$season, levels = ag$season)
  use_days <- max(ag$med, na.rm = TRUE) >= 48                  # auto hours↔days by the data's scale
  ag$y <- if (use_days) ag$med / 24 else ag$med
  gap_col <- "#6a3d9a"                                         # purple = gap (predator timing)
  ser_col <- c("Protected RAI" = "#2e7d32", "Predator RAI" = "#c62828")
  ser_for <- c(prot = "Protected RAI", pred = "Predator RAI")

  # Grounding: overlay PROTECTED and/or PREDATOR RAI per season (± SE) on a shared second axis, so a
  # rising gap can be read against whether the detections actually hold up. Reserve applies.
  want <- intersect(rai_want, names(ser_for))
  rai_rows <- lapply(want, function(key) {
    sci <- rai_specs[[key]]$sci; if (!length(sci)) return(NULL)
    tr  <- tryCatch(ik_species_trend(ik_data, stats::setNames(list(sci), ser_for[[key]]),
                                     by = "season", reserve = reserve), error = function(e) NULL)
    if (is.null(tr)) return(NULL)
    tr <- tr[tr$metric_type == "camera_rai", , drop = FALSE]; if (!nrow(tr)) return(NULL)
    mi <- match(as.character(ag$season), as.character(tr$period))
    val <- tr$value[mi]; se <- tr$se[mi]; if (!any(is.finite(val))) return(NULL)
    data.frame(season = ag$season, series = ser_for[[key]], value = val,
               lo = pmax(0, val - ifelse(is.na(se), 0, se)), hi = val + ifelse(is.na(se), 0, se),
               stringsAsFactors = FALSE)
  })
  rai_df <- do.call(rbind, Filter(Negate(is.null), rai_rows))

  p <- ggplot2::ggplot(ag, ggplot2::aes(.data$season, group = 1))
  if (!is.null(rai_df) && nrow(rai_df)) {
    rai_top <- max(rai_df$hi, na.rm = TRUE)
    k <- if (is.finite(rai_top) && rai_top > 0) max(ag$y, na.rm = TRUE) / rai_top else NA_real_
  } else k <- NA_real_
  rai_on <- is.finite(k) && k > 0
  if (rai_on) {
    rai_df$series <- factor(rai_df$series, levels = names(ser_col))
    rai_df$season <- factor(rai_df$season, levels = levels(ag$season))
    rai_df$val_s  <- rai_df$value * k; rai_df$lo_s <- rai_df$lo * k; rai_df$hi_s <- rai_df$hi * k
    p <- p +
      ggplot2::geom_ribbon(data = rai_df, ggplot2::aes(x = .data$season, ymin = .data$lo_s, ymax = .data$hi_s,
                  fill = .data$series, group = .data$series), alpha = 0.12, inherit.aes = FALSE, na.rm = TRUE) +
      ggplot2::geom_line(data = rai_df, ggplot2::aes(x = .data$season, y = .data$val_s, colour = .data$series, group = .data$series),
                linewidth = 0.9, inherit.aes = FALSE, na.rm = TRUE) +
      ggplot2::geom_point(data = rai_df, ggplot2::aes(x = .data$season, y = .data$val_s, colour = .data$series),
                 size = 2.2, inherit.aes = FALSE, na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = ser_col, guide = "none")
  }
  # Median gap is a COLOUR aesthetic ONLY when RAI is overlaid (so the legend tells it apart). With RAI
  # off, a FIXED colour → no colour aesthetic → no colour legend, so only the "Pairs" size key shows.
  p <- p + (if (rai_on) list(
      ggplot2::geom_line(ggplot2::aes(y = .data$y, colour = "Median gap"), linewidth = 0.9),
      ggplot2::geom_point(ggplot2::aes(y = .data$y, colour = "Median gap", size = .data$n)),
      ggplot2::scale_colour_manual(NULL, values = c("Median gap" = gap_col, ser_col))
    ) else list(
      ggplot2::geom_line(ggplot2::aes(y = .data$y), colour = gap_col, linewidth = 0.9),
      ggplot2::geom_point(ggplot2::aes(y = .data$y, size = .data$n), colour = gap_col)
    ))
  p <- p +
    # one row always — else many distinct small pair-counts wrap the size key onto two rows (reads as a dup).
    ggplot2::scale_size_area(name = "Pairs", max_size = 6.5, guide = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::labs(x = NULL, subtitle = if (rai_on)
           "RAI overlaid for context — a gap trend can track detection rates, not just timing"
         else "exploratory — large gaps mean the two rarely share the same ground") +
    ik_ggtheme(is_dark) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), panel.grid.minor = ggplot2::element_blank(),
          legend.position = "top", axis.title.y.left = ggplot2::element_text(colour = gap_col))
  gap_name <- sprintf("Median gap (%s)", if (use_days) "days" else "hours")
  if (rai_on)
    p + ggplot2::scale_y_continuous(name = gap_name, sec.axis = ggplot2::sec_axis(~ . / k, name = "RAI (per camera-hrs)"))
  else p + ggplot2::scale_y_continuous(name = gap_name)
}

#' Wire the Co-occurrence Map view — base leaflet + proxy Surface/Pairs/Cameras/Boundary layers, the
#' per-camera table, CSV download, row-hover preview and the marker/row drill. Registers outputs
#' ids$map / ids$table / ids$dl / ids$intro and their observers. @param gaps reactive season-scoped gaps.
#' @param reserve/radius_m/cross_boundary/is_dark/on_map reactives. @param cooc_open drill opener.
#' @param prot_l/pred_l reactive labels (intro). @param max_pd pairing window (days). @keywords internal
ik_cooc_map <- function(input, output, session, ik_data, prefer, gaps, reserve, radius_m,
                        cross_boundary, is_dark, on_map, cooc_open, prot_l, pred_l, max_pd,
                        ids = list(map = "cooc_map", table = "cooc_map_table", dl = "cooc_dl_pairs", intro = "cooc_map_intro")) {
  locs0     <- ik_data$app$geography$locations
  in_marker <- paste0(ids$map, "_marker_click")
  in_rows   <- paste0(ids$table, "_rows_selected")
  in_hover  <- paste0(ids$table, "_hover")
  .fmt_gap_short <- function(h) ifelse(h < 48, sprintf("%.0f h", h), sprintf("%.1f d", h / 24))

  # Map tab description — tailored to the selected species (the sidebar is far below on mobile).
  output[[ids$intro]] <- renderUI({
    r <- radius_m()
    phrase <- if (r <= 0) sprintf("on the same camera within %d days", max_pd)
              else sprintf("on any camera within %s, within %d days",
                           if (r >= 1000) sprintf("%g km", r / 1000) else sprintf("%g m", r), max_pd)
    tags$p(class = "ik-cooc-hint",
      "Each ", tags$b(prot_l()), " detection, coloured by how soon a ", tags$b(pred_l()),
      " turns up ", phrase, " ", tags$b("(red = soonest)"),
      " — dot size = number of pairs; the ", tags$b("Surface"),
      " layer interpolates between cameras. ", tags$b("Click a camera"), " for the detections behind it.")
  })

  # Download the pair-level temporal data (both cameras, both timestamps, distance, gap + direction) —
  # honours the current Period / Reserve / Within / species / stalking selection (same gaps() the Map shows).
  output[[ids$dl]] <- downloadHandler(
    filename = function() sprintf("cooccurrence-pairs-%s.csv", Sys.Date()),
    content  = function(file) {
      g <- gaps()
      if (is.null(g) || !nrow(g)) { utils::write.csv(data.frame(), file, row.names = FALSE); return() }
      nm <- function(id) locs0$name[match(id, locs0$location_id)]
      df <- data.frame(
        reserve            = locs0$reserve[match(g$location_id, locs0$location_id)],
        protected_camera   = nm(g$location_id),
        protected_species  = ik_species_label(g$scientificName, ik_data, "vernacular"),
        protected_seen     = format(g$when, "%Y-%m-%d %H:%M"),
        predator_camera    = nm(g$pred_loc),
        distance_m         = round(g$pred_dist_m),
        predator_species   = ik_species_label(g$pred_sci, ik_data, "vernacular"),
        predator_seen      = format(g$pred_when, "%Y-%m-%d %H:%M"),
        gap_hours          = round(g$gap_h, 2),
        gap_days           = round(g$gap_h / 24, 2),
        predator_direction = ifelse(g$signed_h > 0, "after protected", "before protected"),
        check.names = FALSE, stringsAsFactors = FALSE)
      utils::write.csv(df[order(df$reserve, df$protected_camera, df$protected_seen), , drop = FALSE],
                       file, row.names = FALSE)
    })

  # Per protected camera: median gap to nearest predator + pair count (the map dots + table rows).
  timing_loc <- reactive({
    g <- gaps(); if (is.null(g) || !nrow(g)) return(NULL)
    agg <- do.call(rbind, lapply(split(g, g$location_id), function(s)
      data.frame(location_id = s$location_id[1], median_gap_h = stats::median(s$gap_h),
                 n = nrow(s), stringsAsFactors = FALSE)))
    mi <- match(agg$location_id, locs0$location_id)
    agg$name <- locs0$name[mi]; agg$reserve <- locs0$reserve[mi]
    agg$latitude <- locs0$latitude[mi]; agg$longitude <- locs0$longitude[mi]
    agg <- agg[is.finite(agg$latitude) & is.finite(agg$longitude), , drop = FALSE]
    if (nrow(agg)) agg else NULL
  })
  # All camera locations in scope (the "Cameras" layer) — reserve-scoped like the data; with cross-boundary
  # ON + a Within radius, also the neighbouring-reserve cameras within that radius (the buffer it pairs with).
  cam_locs <- reactive({
    cam_ds <- names(ik_data$datasets)[vapply(ik_data$datasets,
                function(d) isTRUE(d$meta$source_type == "camera"), logical(1))]
    al <- ik_active_locations(ik_data)
    al <- al[al$dataset %in% cam_ds & is.finite(al$latitude) & is.finite(al$longitude), , drop = FALSE]
    rv <- reserve()
    if (length(rv) && "reserve" %in% names(al)) {
      keep <- al$reserve %in% rv
      if (isTRUE(cross_boundary()) && radius_m() > 0 && any(keep)) {
        nb <- ik_within_distance(ik_data, al$location_id[keep], radius_m = radius_m(), of = "camera")
        if (nrow(nb)) keep <- keep | al$location_id %in% nb$to_id
      }
      al <- al[keep, , drop = FALSE]
    }
    if (nrow(al)) al else NULL
  })

  # Base map: built ONCE, no volatile deps — data layers pushed via leafletProxy in the observers (the
  # maps.R pattern), so a setting change never re-renders the widget into a 0-size hidden container.
  output[[ids$map]] <- leaflet::renderLeaflet({
    canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
    m <- leaflet::leaflet() |>
      leaflet::addProviderTiles(canvas, group = "Street") |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite")
    pns <- c("boundary", "surface", "cameras", "pairs")
    for (pn in pns) m <- leaflet::addMapPane(m, pn, zIndex = 410 + 10 * match(pn, pns))
    m <- leaflet::addLayersControl(m, baseGroups = c("Street", "Satellite"),
      overlayGroups = c("Surface", "Pairs", "Cameras", "Boundary"),
      options = leaflet::layersControlOptions(collapsed = FALSE))
    cam_ds <- names(ik_data$datasets)[vapply(ik_data$datasets, function(d) isTRUE(d$meta$source_type == "camera"), logical(1))]
    gl <- locs0[locs0$dataset %in% cam_ds & is.finite(locs0$latitude) & is.finite(locs0$longitude), , drop = FALSE]
    if (nrow(gl)) m <- leaflet::fitBounds(m, min(gl$longitude), min(gl$latitude), max(gl$longitude), max(gl$latitude))
    m
  })
  outputOptions(output, ids$map, suspendWhenHidden = FALSE)   # eager so proxy layers land before first view
  cproxy <- function() leaflet::leafletProxy(ids$map, session)

  # All proxy draws gated on the Map tab being LIVE: a proxy op to a hidden tab (uninitialised widget) is
  # lost. Reading on_map() also re-fires each observer on tab-show, so the layers (re)draw onto the sized widget.
  observe({
    if (!on_map()) return()
    leaflet::addProviderTiles(leaflet::clearGroup(cproxy(), "Street"),
      if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron, group = "Street")
  })
  observe({                                                   # Boundary + Cameras footprint; re-frame to it
    if (!on_map()) return()
    p <- cproxy(); leaflet::clearGroup(p, "Boundary"); leaflet::clearGroup(p, "Cameras")
    cams <- cam_locs(); if (is.null(cams) || !nrow(cams)) return()
    hulls <- tryCatch(ik_selection_hulls(cams, "reserve"), error = function(e) NULL)
    if (!is.null(hulls) && nrow(hulls)) leaflet::addPolygons(p, data = hulls, group = "Boundary",
      fill = FALSE, color = "#6c757d", weight = 1.5, dashArray = "5,5", label = ~reserve,
      options = leaflet::pathOptions(pane = "boundary"))
    leaflet::addCircleMarkers(p, data = cams, lng = ~longitude, lat = ~latitude, group = "Cameras",
      radius = 3, fill = TRUE, fillColor = "#2c7fb8", fillOpacity = 0.55, stroke = FALSE,
      label = ~sprintf("%s — camera", name), options = leaflet::pathOptions(pane = "cameras"))
    leaflet::fitBounds(p, min(cams$longitude), min(cams$latitude), max(cams$longitude), max(cams$latitude))
  })
  observe({                                                   # Surface + Pairs + legend (the co-detection data)
    if (!on_map()) return()
    p <- cproxy(); leaflet::clearGroup(p, "Surface"); leaflet::clearGroup(p, "Pairs"); leaflet::clearControls(p)
    d <- timing_loc(); if (is.null(d) || !nrow(d)) return()
    use_days <- max(d$median_gap_h, na.rm = TRUE) >= 48
    conv <- function(h) if (use_days) h / 24 else h
    d$gval <- conv(d$median_gap_h); mx <- max(d$gval, na.rm = TRUE)
    pal  <- leaflet::colorNumeric(c("#e31a1c", "#fd8d3c", "#fecc5c", "#ffffb2"), c(0, mx))  # red = soonest
    surf <- tryCatch(ik_idw_surface(d, "median_gap_h", "reserve"), error = function(e) NULL)
    if (!is.null(surf) && nrow(surf)) {
      surf$gval <- pmin(conv(pmax(0, surf$predicted)), mx)
      leaflet::addPolygons(p, data = surf, group = "Surface", stroke = FALSE,
        fillColor = ~pal(gval), fillOpacity = if (is_dark()) 0.45 else 0.35,
        options = leaflet::pathOptions(pane = "surface"))
    }
    leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, layerId = ~location_id, group = "Pairs",
      radius = ik_marker_radius(d$n, 4, 22, cap = 5),   # 1 pair → small, 5+ → large (clipped at 5) so 1–5 are easy to tell apart
      fillColor = ~pal(gval), fillOpacity = 0.9, stroke = TRUE, color = "#333333", weight = 1,
      label = ~sprintf("%s — median gap %s (%d pair%s)", name, .fmt_gap_short(median_gap_h), n, ifelse(n == 1, "", "s")),
      options = leaflet::pathOptions(pane = "pairs"))
    leaflet::addLegend(p, "bottomright", pal = pal, values = d$gval,
      title = sprintf("Median gap (%s)<br/><span style='font-weight:400'>red = soonest</span>",
                      if (use_days) "days" else "hours"), opacity = 0.9)
  })

  # Per-camera table under the map (each row = a map dot), soonest first; shares its sorted order with the
  # click handler. DT returns selections as indices into the data AS PASSED (robust to column re-sorting).
  map_tab <- reactive({ d <- timing_loc(); if (is.null(d)) return(NULL); d[order(d$median_gap_h), , drop = FALSE] })
  output[[ids$table]] <- DT::renderDT({
    d <- map_tab(); validate(need(!is.null(d) && nrow(d), "No co-detections for this selection."))
    df <- data.frame(Camera = d$name, Reserve = d$reserve,
                     `Median gap` = .fmt_gap_short(d$median_gap_h), Pairs = d$n,
                     .loc = d$location_id, check.names = FALSE)
    loc_i <- ncol(df) - 1L                                   # 0-based index of the hidden .loc column
    DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
      options = list(dom = "tip", pageLength = 7,
        columnDefs = list(list(visible = FALSE, targets = loc_i)),
        createdRow = DT::JS(sprintf(                          # hover a row → preview that camera's popup on the map
          "function(row,data,i){var L=data[%d];row.addEventListener('mouseenter',function(){Shiny.setInputValue('%s',L,{priority:'event'});});row.addEventListener('mouseleave',function(){Shiny.setInputValue('%s','',{priority:'event'});});}",
          loc_i, session$ns(in_hover), session$ns(in_hover)))))
  })
  open_cam <- function(id, nm) {                              # marker OR row click → the shared drill
    g <- gaps(); if (is.null(g) || is.null(id)) return()
    rows <- g[g$location_id == id, , drop = FALSE]; if (!nrow(rows)) return()
    cooc_open(rows, sprintf("Co-detections at %s", nm),
      sprintf("%s protected detection%s with the nearest predator — click either species for its record",
              format(nrow(rows), big.mark = ","), if (nrow(rows) == 1) "" else "s"))
  }
  observeEvent(input[[in_marker]], {
    id <- input[[in_marker]]$id
    open_cam(id, locs0$name[match(id, locs0$location_id)])
  })
  observeEvent(input[[in_rows]], {
    i <- input[[in_rows]]; d <- map_tab(); if (is.null(d) || !length(i)) return()
    open_cam(d$location_id[i], d$name[i])
  })
  # Hover a table row → preview that camera's popup on the map (debounced); leaving clears it.
  hover_loc <- shiny::debounce(reactive(input[[in_hover]]), 120)
  observeEvent(hover_loc(), {
    p <- leaflet::leafletProxy(ids$map, session); leaflet::clearGroup(p, "Hover")
    loc <- hover_loc(); if (is.null(loc) || !nzchar(loc)) return()
    d <- map_tab(); if (is.null(d)) return()
    r <- d[d$location_id == loc, , drop = FALSE]
    if (!nrow(r) || !is.finite(r$longitude[1])) return()
    leaflet::addPopups(p, lng = r$longitude[1], lat = r$latitude[1], group = "Hover",
      popup = sprintf("<b>%s</b><br/>median gap %s &middot; %d pair%s", r$name[1],
                      .fmt_gap_short(r$median_gap_h[1]), r$n[1], ifelse(r$n[1] == 1, "", "s")),
      options = leaflet::popupOptions(closeButton = FALSE, autoPan = FALSE))
  }, ignoreNULL = FALSE)
}

#' Co-occurrence server.
#' @param id Module id. @param ik_data The container.
#' @param prefer_scientific reactive name preference (for the record viewer).
cooccurrence_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                                color_mode = reactive("light"), selection = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    # Period banner tracks the active tab: Distribution + Map honour the selected window; the Trend
    # spans every season, so it reads "All data".
    output$period_banner <- renderUI(.ik_period_banner(ik_data, selection(),
      all_data = identical(input$cooc_view, "Trend")))
    is_dark <- reactive(identical(color_mode(), "dark"))
    sg <- ik_species_groups(ik_data)
    .role_taxa <- function(role) { l <- unique(sg$label[sg$role == role & !is.na(sg$monitor)])
      stats::setNames(lapply(l, function(x) sg$scientificName[sg$label == x & !is.na(sg$scientificName)]), l) }
    pred_taxa <- .role_taxa("predator"); prot_taxa <- .role_taxa("protected")
    splits <- unique(sg$label[which(sg$split)])
    .pred_def <- paste0("grp:", names(pred_taxa)[1])
    .prot_def <- paste0("grp:", names(prot_taxa)[1])
    .fmt_gap <- function(h) if (h < 1) sprintf("%d min", round(h * 60)) else
      if (h < 48) sprintf("%.1f h", h) else sprintf("%.1f d", h / 24)
    radius_m <- reactive(as.numeric(input$radius %||% 0))
    prox     <- reactive({ r <- radius_m()                      # the "where" phrase, woven into the copy
      if (r <= 0) "at the same camera" else sprintf("within %s", if (r >= 1000) sprintf("%g km", r / 1000) else sprintf("%g m", r)) })
    prefer      <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    # Selected Protected / Predator labels — woven (bold) into every tab's dynamic copy so the data text
    # reflects the view options. STANDARD: the page title + the description above the tabs stay generic;
    # the per-tab text under the date banner reflects what's selected (the sidebar is at the very bottom on mobile).
    prot_l <- reactive(paste(ik_choice_labels(input$prot %||% .prot_def, ik_data, prefer()), collapse = " + "))
    pred_l <- reactive(paste(ik_choice_labels(input$pred %||% .pred_def, ik_data, prefer()), collapse = " + "))
    max_pd <- ik_data$meta$cooccurrence$max_pair_days %||% 30                       # pairing window (days) — config
    gap_labels_capped <- GAP_LABELS[GAP_BREAKS[-length(GAP_BREAKS)] < max_pd * 24]   # drop buckets beyond the window
    # split-aware grouped pickers (group whole, or sub-species per the project flag) — same as the Map.
    observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      updateSelectizeInput(session, "pred", choices = ik_species_choices(pred_taxa, ik_data, p, splits), selected = keep(isolate(input$pred), .pred_def))
      updateSelectizeInput(session, "prot", choices = ik_species_choices(prot_taxa, ik_data, p, splits), selected = keep(isolate(input$prot), .prot_def))
    })
    cooc_open <- ik_cooc_drill(input, output, session, ik_data, prefer, radius_m = radius_m)   # the shared drill

    # Resolve the predator/protected pickers + scope to `seasons`, the selected Reserve and the Within
    # radius, then (optionally) keep only stalking pairs. Used twice: season-scoped (Distribution + Map)
    # and across all seasons (the Trend). Reserve + Within apply to both.
    .compute_gaps <- function(seasons) {
      preds <- input$pred; if (!length(preds)) preds <- .pred_def
      prots <- input$prot; if (!length(prots)) prots <- .prot_def
      g <- ik_predator_protected_gaps(ik_data, ik_resolve_species_choice(preds, pred_taxa),
                                      ik_resolve_species_choice(prots, prot_taxa),
                                      seasons = seasons, reserve = .ik_nz(selection()$reserve),
                                      radius_m = radius_m(), cross_boundary = isTRUE(input$cross_boundary))
      if (is.null(g)) return(NULL)
      if (isTRUE(input$after_only)) {                  # stalking: predator detected AFTER the protected animal
        g <- g[g$signed_h > 0, , drop = FALSE]
        if (!nrow(g)) return(NULL)
      }
      g$bucket <- cut(g$gap_h, GAP_BREAKS, GAP_LABELS, right = FALSE)
      g
    }
    gaps     <- reactive(.compute_gaps(.ik_nz(selection()$season)))   # Distribution + Map (honour Period)
    gaps_all <- reactive(.compute_gaps(NULL))                         # Trend (all seasons; Reserve + Within still apply)

    output$intro <- renderUI({
      g <- gaps(); after <- isTRUE(input$after_only); px <- prox(); pl <- prot_l(); dl <- pred_l()
      win <- sprintf("Selected Predator/Protected pairings within %d days of one another: ", max_pd)   # the pairing window leads the summary
      if (is.null(g)) return(tags$p(class = "ik-cooc-hint",
        if (after) tagList("No ", tags$b(pl), " detections were followed by a ", tags$b(dl), " ", px, sprintf(" within %d days.", max_pd))
        else       tagList("No ", tags$b(pl), " and ", tags$b(dl), " detections were found ", px, sprintf(" within %d days.", max_pd))))
      pct <- sprintf(" · %d%% within 24 h.", round(100 * mean(g$gap_h <= 24)))
      if (after) tags$p(class = "ik-cooc-hint", win,
        format(nrow(g), big.mark = ","), " ", tags$b(pl), " detections followed by a ", tags$b(dl), " ", px,
        " · median ", .fmt_gap(stats::median(g$gap_h)), " until the predator arrives", pct)
      else tags$p(class = "ik-cooc-hint", win,
        format(nrow(g), big.mark = ","), " ", tags$b(pl), " detections · median ", .fmt_gap(stats::median(g$gap_h)),
        " to the nearest ", tags$b(dl), " ", px, pct)
    })

    # Map tab — its description, leaflet, per-camera table, CSV download and drill are all wired by the
    # shared ik_cooc_map() call below (its outputs land on map_intro / dl_pairs / map / map_table).

    # Trend tab description — species + Within + stalking aware.
    output$trend_intro <- renderUI({
      r <- radius_m()
      where <- if (r <= 0) "at the same camera"
               else sprintf("at the same camera, or any camera within %s",
                            if (r >= 1000) sprintf("%g km", r / 1000) else sprintf("%g m", r))
      lead <- if (isTRUE(input$after_only))
        tagList("Median time until a ", tags$b(pred_l()), " arrives after a ", tags$b(prot_l()), " ", where, sprintf(", up to %d days apart, grouped by season.", max_pd))
      else
        tagList("Median gap between camera detections of ", tags$b(prot_l()), " and ", tags$b(pred_l()), " ", where, sprintf(", up to %d days apart, grouped by season.", max_pd))
      tags$p(class = "ik-cooc-hint", lead,
        " Large gaps (weeks+) indicate the two rarely share ground. Optionally overlay RAI to tell a real timing shift from simply changing detection rates.")
    })

    output$dist <- renderPlot({
      g <- gaps(); validate(need(!is.null(g) && nrow(g), "No co-detections to chart."))
      ik_cooc_dist_plot(g, sprintf("Time to nearest predator %s", prox()), is_dark(), gap_labels_capped)
    }, bg = "transparent")

    output$trend <- renderPlot({
      rs <- list(prot = list(sci = ik_resolve_species_choice(input$prot %||% .prot_def, prot_taxa)),
                 pred = list(sci = ik_resolve_species_choice(input$pred %||% .pred_def, pred_taxa)))
      p <- ik_cooc_trend_plot(gaps_all(), ik_data, is_dark(), prefer(), reserve = .ik_nz(selection()$reserve),
                              rai_specs = rs, rai_want = input$trend_rai %||% character(0))
      validate(need(inherits(p, "ggplot"), if (is.character(p)) p else "No co-detections to chart."))
      p
    }, bg = "transparent")

    # Click a distribution bar → the shared co-occurrence drill (the bucket's co-detections, each with
    # both species linked to their record + the gap direction). See ik_cooc_drill().
    observeEvent(input$dist_click, {
      g <- gaps(); cl <- input$dist_click; if (is.null(g) || is.null(cl$x)) return()
      xi <- round(cl$x); if (xi < 1 || xi > length(gap_labels_capped)) return()
      bk <- gap_labels_capped[xi]; rows <- g[as.character(g$bucket) == bk, , drop = FALSE]
      if (!nrow(rows)) return()
      cooc_open(rows,
        sprintf("Detections %s from a predator", bk),
        sprintf("%s %s detection%s paired with the nearest predator — click either species for its record",
                format(nrow(rows), big.mark = ","),
                paste(ik_choice_labels(input$prot %||% .prot_def, ik_data, prefer()), collapse = " + "),
                if (nrow(rows) == 1) "" else "s"))
    })

    # ---- Map: each protected camera coloured by how soon a predator turns up nearby — leaflet + surface,
    #      the per-camera table, the CSV download, the row-hover preview and the marker/row drill, all
    #      shared with each Species page's Co-occurrence Map. See ik_cooc_map(). ----
    ik_cooc_map(input, output, session, ik_data, prefer,
                gaps = gaps, reserve = reactive(.ik_nz(selection()$reserve)),
                radius_m = radius_m, cross_boundary = reactive(isTRUE(input$cross_boundary)),
                is_dark = is_dark, on_map = reactive(identical(input$cooc_view, "Map")),
                cooc_open = cooc_open, prot_l = prot_l, pred_l = pred_l, max_pd = max_pd,
                ids = list(map = "map", table = "map_table", dl = "dl_pairs", intro = "map_intro"))
  })
}
