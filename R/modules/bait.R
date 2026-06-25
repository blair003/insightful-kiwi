# bait.R — the Bait effectiveness view (Outcomes → Bait effectiveness): trap capture rate (or
# total captures) by bait RECIPE, for a period and an optional target species. A ranked
# horizontal bar chart; click a bar to drill into the individual captures behind it, and from
# there into the observation viewer. Data from ik_bait_effectiveness() / ik_bait_captures()
# (prior-check bait attribution, the whole recipe as the unit).

#' Bait effectiveness nav panel. @param id Module id. @param ik_data The container (for the
#'   catch-rate normalisation unit woven into the help/labels).
bait_ui <- function(id, ik_data = NULL) {
  ns <- NS(id)
  norm <- (ik_data$meta$trapping$rate %||% list())$norm_trap_days %||% 100
  nav_panel(
    "Bait effectiveness", value = "bait", icon = icon("drumstick-bite"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/bait.css")),
    div(class = "ik-bait",
        div(class = "ik-bait-head",
            tags$h3(class = "ik-bait-title", "Bait effectiveness"),
            .ik_info(ns("help"), "Bait effectiveness — how to read this", bait_help_body(norm))),
        tags$p(class = "ik-bait-lead",
          "Captures by bait — attributed to the bait that was in the trap during the catching ",
          "interval (the ", tags$b("prior check's"), " bait). ", tags$b("Click a bar"),
          " to see the captures behind it, then a trap to see its bait history."),
        div(class = "bait-controls",
            selectInput(ns("species"), "Captures of", choices = NULL, width = "200px"),
            radioButtons(ns("group"), "Group by", inline = TRUE,
                         choices = c("Full recipe" = "recipe", "Ingredient" = "ingredient"),
                         selected = "recipe"),
            numericInput(ns("min_cap"), "Min captures", value = 2, min = 1, step = 1,
                         width = "120px")),
        # "Measure" only re-ranks the bars (rate vs total), so it sits small + plot-local, not in
        # the data-selection row above.
        div(class = "bait-plot-controls",
            radioButtons(ns("measure"), "Measure", inline = TRUE,
                         choices = c("Capture rate" = "rate", "Total caught" = "count"),
                         selected = "rate")),
        plotOutput(ns("plot"), height = "560px", click = ns("plot_click")))
  )
}

#' The body of the "how to read this" help modal for the bait view. `norm` = trap-nights the rate
#' is normalised to (from project config), woven in so the help matches the chart. @keywords internal
bait_help_body <- function(norm = 100) {
  ntn <- paste0(format(norm, big.mark = ","), " trap-nights")
  tagList(
    tags$p("This ranks baits by how often they catch — ", tags$b(paste0("captures per ", ntn)),
           " (a rate that accounts for how long each bait was out), or total captures if you ",
           "switch ", tags$em("Measure"), " to “Total caught”."),
    tags$h6("Which bait gets the credit"),
    tags$p("A catch is found at a check, but it was caught by whatever was in the trap ",
           tags$em("before"), " that check — so each catch is credited to the ", tags$b("prior"),
           " check's bait, not the fresh bait set on the day it was found."),
    tags$h6("Group by: Full recipe vs Ingredient"),
    tags$p(tags$b("Full recipe"), " treats the whole bait set as one thing — “Ping Pong + ",
           "Salted Rabbit” is its own line, separate from either alone. Cleanest to read, but it ",
           "splits the data into many small recipes."),
    tags$p(tags$b("Ingredient"), " credits ", tags$em("every"), " ingredient for the catches in ",
           "intervals where it appeared, so one catch on a two-ingredient bait counts for both. ",
           "Useful for spotting ingredients that show up in baits that work — but it is a ",
           tags$b("correlation, not proof"), ": if two ingredients almost always travel together, ",
           "their numbers blur and you can't tell which one did the work. Read it as ",
           "“appears in successful baits”, not “causes catches”."),
    tags$h6("What's left off"),
    tags$p("Baits with under ", ntn, " of use, or fewer than the ", tags$em("Min captures"),
           " floor, are dropped — too little to compare (one lucky catch on a rarely-used bait ",
           "would otherwise top the chart). The chart shows the top 15."),
    tags$h6("Digging in"),
    tags$p("Click a bar for the individual captures, click a catch to open its full record, or ",
           "click a trap name to see that trap's whole check history and watch the bait change ",
           "around each catch.")
  )
}

#' Bait effectiveness server.
#' @param id Module id. @param ik_data The ik_data container.
#' @param prefer_scientific reactive() name-display preference (for the observation viewer).
bait_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                        color_mode = reactive("light"), selection = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    is_dark <- reactive(identical(color_mode(), "dark"))
    norm <- ik_data$meta$trapping$rate$norm_trap_days %||% 100   # rate normalisation (config)
    ntn  <- paste0(format(norm, big.mark = ","), " trap-nights")
    sg  <- ik_species_groups(ik_data)
    ctl <- ik_taxa_groups(sg, "control", "target")           # trapped predators
    # Period now comes from the sidebar (selection); default set there (latest whole year).

    # "Captures of" choices: every control-target group, plus indented sub-species for any group
    # resolved as >1 species (Mustelids → Stoat / Weasel / Ferret) so mustelids can be viewed
    # whole OR split. Labels follow the scientific-name preference. Values: __all__ /
    # grp:<label> / sci:<scientificName>.
    species_choices <- function(prefer) {
      ch <- list("All captures" = "__all__")
      for (lbl in names(ctl)) {
        ch[[lbl]] <- paste0("grp:", lbl)
        spp <- ctl[[lbl]][grepl(" ", ctl[[lbl]], fixed = TRUE)]   # resolved species (binomials)
        if (length(spp) >= 2)
          for (sn in spp)
            ch[[paste0("  ", ik_species_label(sn, ik_data, prefer))]] <- paste0("sci:", sn)
      }
      ch
    }
    # Set the initial choices directly (so the widget shows "All captures" on load, not blank),
    # then only RELABEL on a scientific-name preference change. (Doing the initial set inside the
    # observeEvent let `input$species` resolve to selectize's empty "" → a blank display.)
    updateSelectInput(session, "species", choices = species_choices("vernacular"), selected = "__all__")
    observeEvent(prefer_scientific(), {
      pref <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      sel  <- if (is.null(input$species) || !nzchar(input$species)) "__all__" else input$species
      updateSelectInput(session, "species", choices = species_choices(pref), selected = sel)
    }, ignoreInit = TRUE)

    # readable label for the selected period value (e.g. "year:2025" → "2025/26 (whole year)")
    pc <- ik_period_choices(ik_data)                    # period choice groups (label → encoded value)
    period_label <- function(val) {
      for (i in seq_along(pc)) {
        m <- match(val, pc[[i]]); if (is.na(m)) next
        nm <- names(pc[[i]])[m]
        if (is.null(nm) || !nzchar(nm)) return(names(pc)[i])
        return(if (identical(nm, "Whole year")) paste(names(pc)[i], "(whole year)") else nm)
      }
      val
    }

    seasons <- reactive(.ik_nz(selection()$season))
    species <- reactive({
      v <- input$species %||% "__all__"
      if (identical(v, "__all__")) NULL
      else if (startsWith(v, "grp:")) ctl[[sub("^grp:", "", v)]]
      else if (startsWith(v, "sci:")) sub("^sci:", "", v)
      else NULL
    })
    # readable label of the current species selection — for the captures-modal subtitle
    species_label <- reactive({
      v <- input$species %||% "__all__"
      if (identical(v, "__all__")) NULL
      else if (startsWith(v, "grp:")) sub("^grp:", "", v)
      else ik_species_label(sub("^sci:", "", v), ik_data,
                            if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    })

    group <- reactive(input$group %||% "recipe")
    # The expensive (~7s) per-interval scan, computed ONCE per (period × species) and shared by the
    # chart AND the captures drill — so clicking a bar reuses it instead of re-scanning every check.
    intervals <- reactive(.bait_intervals(ik_data, seasons(), species()))
    data <- reactive(ik_bait_effectiveness(
      ik_data, seasons(), species = species(), group = group(), norm = norm,
      min_captures = max(1, input$min_cap %||% 3),
      health = NULL, by_health = FALSE, intervals = intervals()))

    # the data currently plotted, ordered + factored — read by the bar-click handler
    plotted <- reactiveVal(NULL)

    output$plot <- renderPlot({
      d <- data()
      validate(need(!is.null(d) && nrow(d), "No bait captures for this selection."))
      count <- identical(input$measure, "count")

      total <- nrow(d)
      d <- d[order(if (count) -d$captures else -d$rate), , drop = FALSE]
      d <- utils::head(d, 15)
      d$bait <- factor(d$bait, levels = rev(d$bait))           # best on top
      plotted(d)

      xval <- if (count) d$captures else d$rate
      lab  <- if (count)
        sprintf("  %s caught  (%s trap-days)", format(d$captures, big.mark = ","),
                format(round(d$trap_days), big.mark = ","))
      else
        sprintf("  %.2f  (%s caught · %s trap-days)", d$rate,
                format(d$captures, big.mark = ","), format(round(d$trap_days), big.mark = ","))
      unit <- if (identical(group(), "ingredient")) "ingredients" else "recipes"

      ggplot(d, aes(xval, .data$bait, fill = xval)) +
        geom_col(width = 0.72) +
        geom_text(aes(label = lab), hjust = 0, size = 3.4, colour = ik_plot_ink(is_dark())) +
        scale_fill_gradient(low = "#d7ccc8", high = "#5d4037", guide = "none") +
        scale_x_continuous(expand = expansion(mult = c(0, 0.55))) +
        labs(x = if (count) "Total captures" else paste0("Captures per ", ntn), y = NULL,
             subtitle = sprintf("Top %d of %d %s, by %s", nrow(d), total, unit,
                                if (count) "total caught" else "capture rate")) +
        ik_ggtheme(is_dark()) +
        theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
              axis.text.y = element_text(face = "bold"))
    }, bg = "transparent")

    # --- drill: click a bar → ONE modal, 3 tabs (Captures → Trap history → Record Details) ---
    # Standard whole-row interaction (one action per row, no cell links → no showModal race):
    # click a CAPTURE row → its trap's history with that catch highlighted; click a HISTORY row
    # → that check's record. Tabs flow left→right as you drill deeper.
    caps      <- reactiveVal(NULL)   # captures for the open recipe — computed once
    sel_trap  <- reactiveVal(NULL)   # the trap whose history is shown
    sel_hl    <- reactiveVal(NULL)   # observationID of the catch you came from (highlighted)
    sel_obs   <- reactiveVal(NULL)   # observationID shown in Check record
    observeEvent(input$plot_click, {
      d <- plotted(); cl <- input$plot_click
      if (is.null(d) || is.null(cl) || is.null(cl$y)) return()
      lv  <- levels(d$bait); idx <- round(cl$y)
      if (idx < 1 || idx > length(lv)) return()
      sel_trap(NULL); sel_hl(NULL); sel_obs(NULL)
      caps(ik_bait_captures(ik_data, lv[[idx]], seasons(), species(), group(), NULL,
                            intervals = intervals()))                # reuse → instant modal
      showModal(modalDialog(
        title = .ik_modal_title(paste("Captures on", lv[[idx]]),
                                paste0(period_label(selection()$period),
                                       if (!is.null(species_label())) paste0(" · ", species_label()) else "")),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(
          id = session$ns("cap_tabs"),
          tabPanel("Captures", icon = icon("list"),
            tags$p(class = "bait-cap-lead",
                   "Each row is a catch found while this bait was in the trap. ", tags$b("Click a row"),
                   " to open that trap's check history with this catch highlighted — then click the ",
                   "highlighted row for its full record. (The catch is credited to the bait set at the ",
                   tags$em("prior"), " check.)"),
            DT::dataTableOutput(session$ns("cap_table"))),
          tabPanel("Trap history", icon = icon("clock-rotate-left"),
            uiOutput(session$ns("hist_ui"))),
          tabPanel("Record details", icon = icon("magnifying-glass"),
            uiOutput(session$ns("obs_ui"))))
      ))
      # Progressive disclosure: the downstream tabs are dependent on a selection in the one before, so
      # they stay hidden until you drill into them (then remain shown — no jarring re-hide).
      hideTab("cap_tabs", "Trap history",   session = session)
      hideTab("cap_tabs", "Record details", session = session)
    })

    observeEvent(input$tab_back, updateTabsetPanel(session, input$tab_back$tabset, selected = input$tab_back$to))

    output$cap_table <- DT::renderDT({
      cap <- caps()
      validate(need(!is.null(cap) && nrow(cap), "No captures found."))
      df <- data.frame(Date = format(cap$check_date, "%d %b %Y"), Trap = cap$trap,
                       Species = cap$species, check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single",
                    class = "stripe hover row-border ik-row-click",
                    options = list(pageLength = 10, scrollX = TRUE, dom = "ftip"))
    })

    observeEvent(input$cap_table_rows_selected, {                # capture row → its trap history
      i <- input$cap_table_rows_selected; cap <- caps()
      if (length(i) && !is.null(cap) && i <= nrow(cap)) {
        sel_trap(cap$locationID[i]); sel_hl(cap$observationID[i]); sel_obs(NULL)
        showTab("cap_tabs", "Trap history", session = session)   # reveal + jump to it
        updateTabsetPanel(session, "cap_tabs", selected = "Trap history")
      }
      DT::selectRows(DT::dataTableProxy("cap_table"), NULL)      # clear → same row re-clickable
    })

    output$hist_ui <- renderUI({
      if (is.null(sel_trap()))
        return(tags$p(class = "bait-cap-lead", "Click a capture in the Captures tab to see its trap's check history here."))
      locs <- ik_data$app$geography$locations
      nm <- locs$name[match(sel_trap(), locs$location_id)]
      tagList(
        .ik_tab_back(session$ns("tab_back"), "cap_tabs", "Captures", "Back to captures"),
        tags$p(class = "bait-cap-lead", "Trap ", tags$b(nm %||% ""),
               " — every check, newest first. The ", tags$b("highlighted"), " row is the catch you came ",
               "from; ", tags$b("click any row"), " for its record. ", tags$b("Rebaited"),
               " is the volunteer's re-bait flag; ", tags$b("Yes · new recipe"),
               " marks a re-bait with a different bait TYPE than the previous check."),
        DT::dataTableOutput(session$ns("hist_table")))
    })

    output$hist_table <- DT::renderDT({
      req(sel_trap())
      ch <- ik_trap_checks(ik_data, sel_trap(), NULL)            # full history, newest first
      validate(need(!is.null(ch) && nrow(ch), "No checks found for this trap."))
      combo <- vapply(ch$bait, .bait_combo, character(1))        # recipe per check
      older <- c(combo[-1], NA)                                  # the check below (older)
      recipe_changed <- !is.na(older) & nzchar(combo) & combo != older   # bait TYPE differs from prior
      reb  <- tolower(trimws(ifelse(is.na(ch$rebaited), "", ch$rebaited)))
      # One "Rebaited" column (no background tint — an in-cell marker instead): a recipe change
      # implies a re-bait, so it shows "Yes · new recipe" (a different bait type); otherwise the
      # raw Yes/No flag, or "—" when the volunteer recorded nothing.
      swap <- as.character(icon("arrows-rotate"))
      rebaited <- ifelse(recipe_changed,
          paste0("Yes <span style='color:#6c757d;font-size:.85em' ",
                 "title='Re-baited with a different bait type than the previous check'>",
                 swap, " new recipe</span>"),
        ifelse(reb == "yes", "Yes",
        ifelse(reb == "no",  "<span style='color:#adb5bd'>No</span>",
                             "<span style='color:#adb5bd'>—</span>")))
      interval <- ifelse(ch$is_first, "—", paste0(ch$interval_days, " d"))
      df <- data.frame(
        Date = format(ch$check_date, "%d %b %Y"),
        Interval = interval, Outcome = ch$outcome, Bait = ch$bait,
        Rebaited = rebaited, Volunteer = ch$volunteer, ObsID = ch$observationID,
        check.names = FALSE, stringsAsFactors = FALSE)
      dt <- DT::datatable(df, rownames = FALSE, selection = "single",
        escape = -which(names(df) == "Rebaited"),                # Rebaited holds an icon + tooltip
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 15, scrollX = TRUE, dom = "ftip",
                       columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))  # hide ObsID
      # Highlight the catch you came from (tints the whole row's cells). See .ik_dt_highlight_row.
      dt <- .ik_dt_highlight_row(dt, "ObsID", sel_hl())
      dt
    })

    observeEvent(input$hist_table_rows_selected, {               # history row → its record
      i <- input$hist_table_rows_selected
      ch <- ik_trap_checks(ik_data, sel_trap(), NULL)
      if (length(i) && !is.null(ch) && i <= nrow(ch)) {
        sel_obs(ch$observationID[i])
        showTab("cap_tabs", "Record details", session = session)   # reveal + jump to it
        updateTabsetPanel(session, "cap_tabs", selected = "Record details")
      }
      DT::selectRows(DT::dataTableProxy("hist_table"), NULL)
    })

    output$obs_ui <- renderUI({
      if (is.null(sel_obs()))
        return(tags$p(class = "bait-cap-lead", "Click a check in the Trap history tab to see its full record here."))
      ob <- ik_observation(ik_data, sel_obs())
      if (is.null(ob)) return(tags$p("Record not found."))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      tagList(.ik_tab_back(session$ns("tab_back"), "cap_tabs", "Trap history", "Back to trap history"),
              .ovw_title(ik_data, ob, prefer),                       # same viewer as everywhere else
              .ovw_tabs(ik_data, ob, prefer, tabset_id = session$ns("rec_subtabs")))
    })
  })
}
