# trap_hero.R — "Top traps": the best-performing traps on a map. Ranked by catch RATE (captures per
# norm_trap_days trap-nights) among traps with enough effort to compare fairly, so a one-night fluke
# can't top the list. Species-filterable; period + reserve from the sidebar. Data from
# ik_location_metric(..., source_type = "trap").

#' "How to read this" help body for Top traps — tabbed. `norm` = catch-rate unit (trap-nights);
#' `min_td` = the minimum trap-nights a trap needs to qualify. @keywords internal
trap_hero_help_body <- function(norm = 100, min_td = 30) {
  P   <- function(...) tags$p(...)
  ntn <- paste0(format(norm, big.mark = ","), " trap-nights")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "Your ", tags$b("best-performing traps"), " — the ones catching the most per night of ",
        "effort, not just the most overall. Ranked by ", tags$b("catch rate"), " (captures per ", ntn,
        "), so a trap run only a few nights can't beat a steady performer on one lucky catch."),
      P("The map ", tags$b("highlights"), " the top traps; every other trap is faint for context. ",
        "Pick a ", tags$b("species"), " (top) and the ", tags$b("period / reserve"), " (sidebar).")),
    tabPanel(
      "How it's ranked", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Catch rate"), " — captures of the chosen species ÷ that trap's trap-nights × ",
                paste0(format(norm, big.mark = ","), ".")),
        tags$li(tags$b("Fair sample"), " — only traps with at least ", tags$b(paste(min_td, "trap-nights")),
                " of effort in the period qualify; thinner ones can't be ranked reliably."),
        tags$li(tags$b("Top N"), " — the highest-rate qualifying traps, ties broken by raw captures."),
        tags$li(tags$b("Checks & mean interval"), " — how often the trap was serviced (a property of the ",
                "trap, not the species), for context on the effort behind the rate."),
        tags$li(tags$b("Catch %"), " — captures ÷ checks: the share of visits that find a catch. Near ",
                tags$b("100%"), " means almost every check finds one — the trap is likely full between visits, so ",
                "you're probably missing catches and could ", tags$b("check it more often"), ". A practical ",
                "“needs more attention” signal alongside the per-night rate."),
        tags$li(tags$b("Coordless traps"), " — a qualifying trap with no location is still listed in the ",
                "table, just not drawn on the map.")),
      P(tags$em("Rate rewards efficiency, not just busy lines — a quietly effective trap on a low-traffic ",
                "line can out-rank a much-checked one on a hot line."))))
}

#' Top-traps nav panel. @param id Module id. @param ik_data The container (species choices baked into
#'   the UI — a dropdown nav panel whose selectize renders lazily and would miss a server-set default).
#' Top-traps "View options" sidebar controls (Captures-of species + Top-N). Built in the module's
#' namespace but rendered in the global sidebar (ui.R) above the shared Filters — like maps_controls().
#' @keywords internal
trap_hero_controls <- function(id, ik_data) {
  ns  <- NS(id)
  sg  <- ik_species_groups(ik_data)
  ctl <- ik_taxa_groups(sg, "control", "target")
  splits     <- unique(sg$label[which(sg$split)])
  sp_choices <- ik_species_choices(ctl, ik_data, "vernacular", splits,
                                   all_label = "All predators", all_value = "__all__")
  div(class = "ik-selection ik-view-controls",
    tags$div(class = "ik-view-controls-h", "View options"),
    selectInput(ns("species"), "Captures of", choices = sp_choices, selected = "__all__"),
    selectInput(ns("topn"), "Show",
                choices = c("Top 10" = 10, "Top 20" = 20, "Top 50" = 50), selected = 10))
}

trap_hero_ui <- function(id, ik_data) {
  ns  <- NS(id)
  norm <- ik_data$meta$trapping$rate$norm_trap_days %||% 100
  ntn  <- paste0(format(norm, big.mark = ","), " trap-nights")
  nav_panel(
    "Top traps", value = "trap-hero", icon = icon("trophy"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/trap_hero.css")),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),  # .ik-maps-split / -side
    tags$script(src = .ik_asset("js/maps.js")),                            # resize-on-tab-show fix
    div(class = "ik-hero",
        .ik_page_header("Top traps — your best performers",
            description = tagList("The traps catching the most per night of effort — ranked by catch rate (captures per ",
              tags$b(ntn), "), not raw totals. The map highlights them; the rest are faint for context."),
            help = .ik_info(ns("hero_help"), "Top traps — how to read this", trap_hero_help_body(norm)),
            banner = div(class = "ik-page-period", uiOutput(ns("period_banner")))),
        uiOutput(ns("note")),
        # Map beside the table (wraps to stacked on narrow); the View options live in the sidebar now.
        layout_columns(class = "ik-maps-split", col_widths = breakpoints(sm = 12, lg = c(8, 4)),
          leaflet::leafletOutput(ns("map"), height = "62vh"),
          div(class = "ik-maps-side", style = "max-height:62vh;", DT::DTOutput(ns("table")))))
  )
}

#' Top-traps server.
#' @param id Module id. @param ik_data The container. @param prefer_scientific reactive name pref.
#' @param selection reactive selection SPEC (Period + Reserve from sidebar). @param color_mode theme.
#' @param active reactive: is this page's nav visible (gates the heavy compute + leaflet).
trap_hero_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                             selection = reactive(list()), color_mode = reactive("light"),
                             active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    output$period_banner <- renderUI(.ik_period_banner(ik_data, selection()))
    is_dark <- reactive(identical(color_mode(), "dark"))
    sg      <- ik_species_groups(ik_data)
    ctl     <- ik_taxa_groups(sg, "control", "target")
    norm    <- ik_data$meta$trapping$rate$norm_trap_days %||% 100
    MIN_TD  <- 30                                              # trap-nights needed to qualify (fair sample)

    # Reserve choices already come from the sidebar; species choices follow the active datasets.
    observe({
      splits <- unique(sg$label[which(sg$split)])
      ch <- ik_species_choices(ctl, ik_data, if (isTRUE(prefer_scientific())) "scientific" else "vernacular",
                               splits, all_label = "All predators", all_value = "__all__")
      updateSelectInput(session, "species", choices = ch,
        selected = if ((isolate(input$species) %||% "__all__") %in% ch) isolate(input$species) else "__all__")
    })

    # One group = the chosen species (or all predators pooled), so a trap's metric is its total catches
    # of that group per trap-night.
    metric <- reactive({
      req(active())
      g <- if (identical(input$species %||% "__all__", "__all__"))
             list(top = unique(unlist(ctl, use.names = FALSE)))
           else list(top = ik_resolve_species_choice(input$species, ctl))
      m <- tryCatch(ik_location_metric(ik_data, selection(), g, "trap", norm = norm), error = function(e) NULL)
      if (is.null(m) || !nrow(m)) return(NULL)
      m
      # active() IS in the key: this reactive req()s it, but bindCache only re-runs on a key change, so
      # without it the cached (hidden, empty) result would stick until another key changed.
    }) |> bindCache(active(), input$species, selection()$period, selection()$season, selection()$reserve, ik_active_datasets())

    # Per-trap servicing — checks & mean check interval. A property of the TRAP (independent of species),
    # so it's the same whichever species is chosen; joined onto the ranked traps below.
    servicing <- reactive({ req(active())
      tryCatch(ik_trap_review(ik_data, seasons = .ik_nz(selection()$season)), error = function(e) NULL)
    }) |> bindCache(active(), selection()$season, ik_active_datasets())

    ranked <- reactive({
      m <- metric(); if (is.null(m)) return(NULL)
      e <- m[m$trap_days >= MIN_TD & m$captures > 0, , drop = FALSE]   # fair-sample qualifiers only
      if (!nrow(e)) return(NULL)
      e <- e[order(-e$metric, -e$captures), , drop = FALSE]
      n <- suppressWarnings(as.integer(input$topn %||% 10)); if (is.na(n)) n <- 10L
      e <- utils::head(e, n)
      e$rank <- seq_len(nrow(e))
      sv <- servicing()                                              # join checks + mean interval
      mi <- if (is.null(sv)) NA_integer_ else match(e$location_id, sv$location)
      e$n_checks <- if (is.null(sv)) NA_integer_ else sv$n_checks[mi]
      e$mean_interval_days <- if (is.null(sv)) NA_real_ else sv$mean_interval_days[mi]
      e$catch_pct <- ifelse(is.na(e$n_checks) | e$n_checks == 0, NA_real_, 100 * e$captures / e$n_checks)  # % of checks with a catch
      e
    })

    output$note <- renderUI({
      r <- ranked()
      if (is.null(r)) return(tags$p(class = "ik-hero-note",
        sprintf("No traps with at least %d trap-nights and a catch of this species in the selection.", MIN_TD)))
      nomap <- sum(!is.finite(r$latitude) | !is.finite(r$longitude))
      tagList(
        tags$p(class = "ik-hero-note",
          sprintf("Top %d traps by catch rate, of those with ≥ %d trap-nights of effort. ", nrow(r), MIN_TD),
          if (nomap > 0) sprintf("%d of them have no coordinates and are listed below the map only.", nomap)))
    })

    # ---- Map: all traps faint for context, the top N highlighted (sized by rate, ranked label) ----
    proxy <- function() leaflet::leafletProxy("map", session)
    output$map <- leaflet::renderLeaflet({
      # Base map fits to ALL located sites (static — no selection/active dependency, so it always
      # renders; a req() in here would silently abort the whole map). Markers come via proxy below.
      locs   <- ik_data$app$geography$locations
      locs   <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      canvas <- if (isolate(is_dark())) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
      m <- leaflet::addProviderTiles(m, canvas, group = "Map")
      m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
      m <- leaflet::addLayersControl(m, baseGroups = c("Map", "Satellite"),
             options = leaflet::layersControlOptions(collapsed = TRUE))
      m <- leaflet::addMapPane(m, "context", zIndex = 410)      # faint all-traps layer underneath …
      m <- leaflet::addMapPane(m, "top",     zIndex = 430)      # … the highlighted top traps on top
      if (nrow(locs))
        m <- leaflet::fitBounds(m, min(locs$longitude), min(locs$latitude), max(locs$longitude), max(locs$latitude))
      m
    })
    # Render from load (not just when the tab is shown) so the proxy layers below land on an existing
    # map — otherwise the first markers are drawn before the widget exists and silently dropped.
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    observeEvent(color_mode(), {                              # dark/light base tiles
      leaflet::addProviderTiles(leaflet::clearGroup(proxy(), "Map"),
        if (is_dark()) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron, group = "Map")
    }, ignoreInit = TRUE)

    observe({                                                  # faint context: every trap
      p <- proxy(); leaflet::clearGroup(p, "AllTraps")
      m <- metric(); req(!is.null(m))
      d <- m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE]; if (!nrow(d)) return()
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "AllTraps",
        layerId = paste0("T|", d$location_id), radius = 3, stroke = FALSE, fillColor = "#9aa3ab", fillOpacity = 0.45,
        options = leaflet::pathOptions(pane = "context"),
        label = ~lapply(sprintf("<b>%s</b><br>%s captures / %s trap-nights · click for history", name, captures, round(trap_days)), htmltools::HTML))
    })

    observe({                                                  # the heroes
      p <- proxy(); leaflet::clearGroup(p, "Top")
      r <- ranked(); req(!is.null(r))
      d <- r[is.finite(r$latitude) & is.finite(r$longitude), , drop = FALSE]; if (!nrow(d)) return()
      rad <- 8 + 16 * (d$metric - min(d$metric)) / (max(d$metric) - min(d$metric) + 1e-9)   # size by rate
      # Hero markers get a DISTINCT layerId prefix ("H|") from the faint AllTraps layer ("T|"). They share
      # a location, and leaflet's layer manager replaces any existing marker with the same layerId — so with
      # a shared id the two layers clobbered each other on every redraw (last writer won), which is why a
      # reserve change left the faint dots but dropped the numbered heroes. Distinct ids → both coexist.
      leaflet::addCircleMarkers(p, data = d, lng = ~longitude, lat = ~latitude, group = "Top",
        layerId = paste0("H|", d$location_id), radius = rad, stroke = TRUE, color = "#3d1f6a", weight = 1.5,
        fillColor = "#6a3d9a", fillOpacity = 0.85, options = leaflet::pathOptions(pane = "top"),
        label = ~lapply(as.character(rank), function(x) htmltools::HTML(sprintf("<b>%s</b>", x))),
        labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
          style = list(color = "white", "font-weight" = "700", "font-size" = "11px")))
      leaflet::addLegend(p, "bottomright", layerId = "leg", title = sprintf("Catch rate / %s TN", format(norm, big.mark = ",")),
        pal = leaflet::colorNumeric("Purples", d$metric), values = d$metric)
    })

    # Re-frame the map to the current selection's traps whenever the sidebar Period / Reserve changes. A
    # proxy marker redraw alone doesn't move the view (nor, with the canvas renderer on mobile, reliably
    # repaint it), so after filtering to a reserve the new top markers sat off-screen and looked like they
    # hadn't redrawn. fitBounds re-centres on the filtered traps AND forces a repaint. (Not tied to the
    # species/top-N view tweaks — those keep the user's current pan/zoom.)
    observeEvent(list(selection()$period, selection()$season, selection()$reserve), {
      m <- metric(); if (is.null(m)) return()
      d <- m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE]; if (!nrow(d)) return()
      leaflet::fitBounds(proxy(), min(d$longitude), min(d$latitude), max(d$longitude), max(d$latitude))
    }, ignoreInit = TRUE)

    output$table <- DT::renderDT({
      r <- ranked(); validate(need(!is.null(r), "No qualifying traps in this selection."))
      df <- data.frame(
        Rank = r$rank, Trap = r$name, Reserve = r$reserve,
        Line = ifelse(is.na(r$line), "—", r$line),
        rate = round(r$metric, 1), Captures = r$captures,
        `Trap-nights` = round(r$trap_days),
        Checks = ifelse(is.na(r$n_checks), NA_integer_, as.integer(r$n_checks)),
        `Mean interval (d)` = ifelse(is.na(r$mean_interval_days), NA_real_, round(r$mean_interval_days)),
        `Catch %` = ifelse(is.na(r$catch_pct), NA_real_, round(r$catch_pct)),
        check.names = FALSE, stringsAsFactors = FALSE)
      names(df)[names(df) == "rate"] <- sprintf("Catch rate / %s TN", format(norm, big.mark = ","))
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 7, scrollX = TRUE, dom = "tip", order = list()))
    })

    # ---- click a trap (map marker or table row) → its full check history → Record Details ----
    th_loc <- reactiveVal(NULL); th_open <- reactiveVal(NULL)
    .open_history <- function(loc) {
      al <- ik_active_locations(ik_data); nm <- al$name[match(loc, al$location_id)]
      th_loc(loc); th_open(NULL)
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s — check history", nm %||% loc),
                                "Every check at this trap (all-time) — click one for its full record."),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("th_tabs"),
          tabPanel("Trap history",   icon = icon("clock-rotate-left"), DT::dataTableOutput(session$ns("th_table"))),
          tabPanel("Record details", icon = icon("circle-info"),       uiOutput(session$ns("th_record"))))))
      hideTab(session = session, inputId = "th_tabs", target = "Record details")
    }
    observeEvent(input$map_marker_click, {                       # markers carry layerId "T|<loc>" (faint) or "H|<loc>" (hero)
      cid <- input$map_marker_click$id
      if (!is.null(cid) && grepl("^[TH]\\|", cid)) .open_history(sub("^[TH]\\|", "", cid))
    })
    observeEvent(input$table_rows_selected, {
      i <- input$table_rows_selected; r <- ranked()
      if (length(i) && !is.null(r) && i <= nrow(r)) .open_history(r$location_id[i])
      DT::selectRows(DT::dataTableProxy("table"), NULL)
    })
    output$th_table <- DT::renderDT({
      req(th_loc())
      ch <- ik_trap_checks(ik_data, th_loc(), NULL)              # full history, newest first
      validate(need(!is.null(ch) && nrow(ch), "No checks recorded for this trap."))
      df <- data.frame(
        Date = format(ch$check_date, "%d %b %Y"),
        Interval = ifelse(ch$is_first, "—", paste0(ch$interval_days, " d")),
        Outcome = ch$outcome, Bait = ifelse(is.na(ch$bait), "—", ch$bait),
        Volunteer = ifelse(is.na(ch$volunteer), "—", ch$volunteer), ObsID = ch$observationID,
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip",
          columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))   # hide ObsID
    })
    observeEvent(input$th_table_rows_selected, {
      i <- input$th_table_rows_selected; ch <- ik_trap_checks(ik_data, th_loc(), NULL)
      if (length(i) && !is.null(ch) && i <= nrow(ch)) {
        th_open(ch$observationID[i]); showTab(session = session, inputId = "th_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("th_table"), NULL)
    })
    observeEvent(input$th_back, updateTabsetPanel(session, input$th_back$tabset, selected = input$th_back$to))
    prefer <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    output$th_record <- renderUI({
      if (is.null(th_open())) return(tags$p(class = "ik-hero-note", "Pick a check from the Trap history tab."))
      ob <- ik_observation(ik_data, th_open()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(session$ns("th_back"), "th_tabs", "Trap history", "Back to trap history"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("th_subtabs")))
    })
  })
}
