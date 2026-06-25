# outcomes.R — the "Are we winning?" view: the trust's causal chain on one seasonal timeline.
# Three stacked panels (read top→bottom): trap catches (the control), predator camera RAI
# (want DOWN), kiwi camera RAI (want UP). Network level (mean over reserves ± SE). Data from
# ik_outcome_series(); plotted as ggplot2 small multiples.

# Chart colours by ROLE / sentiment — no hard-coded species. Protected → greens, predators → warm,
# anything else → neutral; distinct within each family, assigned in the series' concern order (so the
# top predator leads with the strongest colour). `.outcome_palette(keys, roles)` → a label→colour map.
.OUTCOME_FAMILY <- list(
  protected = c("#2e7d32", "#1b5e20", "#43a047", "#00897b", "#558b2f", "#66bb6a"),
  predator  = c("#c62828", "#ef6c00", "#8e24aa", "#6d4c41", "#ad1457", "#d84315", "#795548"),
  other     = c("#4a7fb5", "#5c6bc0", "#6c757d", "#0097a7"))
.outcome_palette <- function(keys, roles) {
  out  <- stats::setNames(character(length(keys)), keys); done <- logical(length(keys))
  for (r in c("protected", "predator", "other")) {
    sel <- !is.na(roles) & roles == r; if (!any(sel)) next
    cols <- .OUTCOME_FAMILY[[r]]; out[keys[sel]] <- cols[((seq_len(sum(sel)) - 1) %% length(cols)) + 1]
    done <- done | sel
  }
  if (any(!done)) { cols <- .OUTCOME_FAMILY$other                    # unknown/NA role → neutral family
    out[keys[!done]] <- cols[((seq_len(sum(!done)) - 1) %% length(cols)) + 1] }
  out
}

#' Stacked-panel titles. `norm` (trap-nights normalisation, from project config) is woven into the
#' catches title so it never disagrees with the actual rate. @keywords internal
outcome_panels <- function(norm = 100) c(
  sprintf("Catches · per %s trap-nights", format(norm, big.mark = ",")),
  "Predators · camera RAI (lower is better)",
  "Protected · camera RAI (higher is better)")

#' "How to read this" help body for "Are we winning?" — tabbed. `norm_trap`/`norm_hours` are the
#' trap-night and camera-hour norms from project config, woven into the calculation tab. @keywords internal
outcomes_help_body <- function(norm_trap = 100, norm_hours = 2000) {
  P  <- function(...) tags$p(...)
  nt <- paste0(format(norm_trap, big.mark = ","), " trap-nights")
  nh <- paste0(format(norm_hours, big.mark = ","), " camera-hours")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "The idea", icon = icon("circle-question"),
      P(tags$br(), "One screen for the whole control story, on a shared seasonal timeline. It lays out the ",
        tags$b("causal chain"), " the trust is betting on, top to bottom:"),
      tags$ul(
        tags$li(tags$b("Catches"), " — predators removed by trapping (the action)."),
        tags$li(tags$b("Predators on camera"), " — should ", tags$b("fall"), " as trapping bites (want ↓)."),
        tags$li(tags$b("Protected on camera"), " — should ", tags$b("rise"), " as predators thin (want ↑).")),
      P("Read it top→bottom: effort in, predators down, protected up. Pick which predators and protected ",
        "species to chart up top (the default is your highest-concern predator vs protected).")),
    tabPanel(
      "Reading it", icon = icon("chart-line"),
      P(tags$br(), "“Winning” looks like ", tags$b("catches sustained"), ", then ", tags$b("predator activity "),
        tags$b("trending down"), ", then ", tags$b("protected activity trending up"), " — usually with a ",
        tags$b("lag"), " between the links (it takes time for fewer predators to show up as more kiwi)."),
      P("Each line is the ", tags$b("network mean across reserves"), "; the band is ± 1 standard error — wide ",
        "where reserves disagree, tight where they move together. ", tags$b("Click any point"), " for its ",
        "per-reserve breakdown and the records behind it."),
      P("The three panels use ", tags$b("different units"), " (catches vs camera activity) and ", tags$b("can't"),
        " be read off one shared scale — compare each panel's ", tags$em("shape over time"), ", not heights ",
        "between panels.")),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Catches"), " — predators caught per ", nt, " (catch rate), so effort is divided out."),
        tags$li(tags$b("Camera activity"), " — RAI: detections per ", nh, ", net of likely duplicates (predators ",
                "and protected each on their own panel)."),
        tags$li(tags$b("Network value"), " — computed per reserve, then averaged to a ", tags$b("mean ± SE"),
                " so no single big reserve dominates."),
        tags$li(tags$b("Timeline"), " — one point per season, in calendar order.")),
      P(tags$em("Observational, not a controlled trial — many things move with the seasons, so treat aligned ",
                "trends as encouraging evidence, not proof that trapping caused the change. Co-occurrence and ",
                "Neighbourhood dig deeper.")))
  )
}

#' The outcomes content (intro + predator/protected pickers + the seasonal plot) — split out of
#' outcomes_ui so it can be embedded as the "Trends" TAB on the Overview page (the are-we-winning
#' view) as well as stand alone. @keywords internal
outcomes_panel_body <- function(id) {
  ns <- NS(id)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/outcomes.css")),
    div(class = "ik-outcomes",
        uiOutput(ns("intro")),
        div(class = "out-controls",
            selectInput(ns("predators"), "Predators", choices = NULL, multiple = TRUE, width = "320px"),
            selectInput(ns("protected"), "Protected", choices = NULL, multiple = TRUE, width = "240px")),
        plotOutput(ns("plot"), height = "660px", click = ns("plot_click")))
  )
}

#' Outcomes nav panel UI (standalone). @param id Module id.
outcomes_ui <- function(id) {
  nav_panel(
    "Are we winning?", value = "outcomes", icon = icon("chart-line"),
    outcomes_panel_body(id))
}

#' Outcomes server. @param id Module id. @param ik_data The ik_data container.
#' @param prefer_scientific A reactive TRUE to show scientific names (drill records/viewer).
outcomes_server <- function(id, ik_data, prefer_scientific, color_mode = reactive("light"),
                            selection = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    is_dark <- reactive(identical(color_mode(), "dark"))
    norm <- ik_data$meta$trapping$rate$norm_trap_days %||% 100   # trap-nights normalisation (config)
    OP   <- outcome_panels(norm)                                 # panel titles (catches title uses norm)
    rsv    <- reactive(.ik_nz((selection() %||% list())$reserve))   # sidebar Reserve (NULL = whole network)
    series <- reactive(ik_outcome_series(ik_data, reserve = rsv())) |>  # slow per-season scan → cache it
      bindCache("outcome_series", rsv(), ik_active_datasets())   # once per (reserve, active datasets), across sessions

    # Predator / Protected pickers — same unified grouped control as the Map (group whole or split
    # into sub-species per the project `split` flag). Default to the highest-concern predator vs protected so
    # the chart reads cleanly (rats otherwise dominate the predator scale); the rest are opt-in.
    sg     <- ik_species_groups(ik_data)
    otx    <- ik_outcome_taxa(ik_data)                          # superset label->sci (for the drill)
    splits <- unique(sg$label[which(sg$split)])
    .role_groups <- function(role) { s <- sg[sg$role == role & !is.na(sg$monitor), , drop = FALSE]
      s <- s[order(s$priority), , drop = FALSE]; split(s$scientificName, factor(s$label, levels = unique(s$label))) }
    pred_groups <- .role_groups("predator"); prot_groups <- .role_groups("protected")
    .pred_def <- paste0("grp:", names(pred_groups)[1])
    .prot_def <- paste0("grp:", names(prot_groups)[1])
    observe({ p <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
      updateSelectInput(session, "predators", choices = ik_species_choices(pred_groups, ik_data, p, splits), selected = keep(isolate(input$predators), .pred_def))
      updateSelectInput(session, "protected", choices = ik_species_choices(prot_groups, ik_data, p, splits), selected = keep(isolate(input$protected), .prot_def))
    })
    # selected picker values -> stable series taxon keys (grp:->label; sci:->vernacular species name)
    .sel_keys <- function(values) vapply(values, function(v)
      if (startsWith(v, "grp:")) sub("^grp:", "", v)
      else if (startsWith(v, "sci:")) ik_species_label(sub("^sci:", "", v), ik_data, "vernacular") else v,
      character(1), USE.NAMES = FALSE)
    sel_taxa <- reactive(c(.sel_keys(input$predators %||% .pred_def), .sel_keys(input$protected %||% .prot_def)))

    output$intro <- renderUI({
      projects <- unique(unlist(lapply(ik_data$datasets, function(d) d$meta$project)))
      tagList(
        .ik_titlebar(
            tags$h3(class = "ik-out-title", "Are we winning?"),
            .ik_info(session$ns("out_help"), "Are we winning? — how to read this",
                     outcomes_help_body(ik_data$meta$trapping$rate$norm_trap_days %||% 100,
                                        ik_data$meta$camera$rai$norm_hours %||% 2000))),
        tags$p(class = "ik-out-lead",
          tagList(paste(projects, collapse = " · "), " — the control story across seasons. ",
            "We ", tags$b("trap predators"), " → predator detections on camera should ",
            tags$b("fall"), " → protected detections should ", tags$b("rise."), " ",
            if (is.null(rsv())) "Lines are the network mean across reserves; bands are ± 1 SE. "
            else sprintf("Scoped to %s (from the sidebar Reserve). ", paste(rsv(), collapse = ", ")),
            tags$b("Click a point"), " for its per-reserve breakdown."))
      )
    })

    # the data currently plotted (panel + ordered season factor) — read by the point-click handler
    plotted <- reactiveVal(NULL)

    output$plot <- renderPlot({
      s <- series()
      validate(need(!is.null(s) && nrow(s), "Not enough data to chart outcomes."))
      s <- s[s$taxon %in% sel_taxa(), , drop = FALSE]
      validate(need(nrow(s) > 0, "Select at least one predator or protected species to chart."))
      s$panel <- factor(ifelse(s$metric_type == "trap_rate", OP[1],
                        ifelse(s$role == "protected", OP[3], OP[2])),
                        levels = OP)
      s$season <- factor(s$season, levels = unique(s$season[order(s$season_order)]))
      plotted(s)
      keys  <- unique(s$taxon)                                  # in concern order (series sorts by priority)
      roles <- s$role[match(keys, s$taxon)]                     # colour by role/sentiment — no hard-coded species
      pal   <- .outcome_palette(keys, roles)

      ggplot(s, aes(.data$season, .data$value, colour = .data$taxon,
                    fill = .data$taxon, group = .data$taxon)) +
        geom_ribbon(aes(ymin = .data$value - .data$se, ymax = .data$value + .data$se),
                    alpha = 0.13, colour = NA, na.rm = TRUE) +
        geom_line(linewidth = 0.8, na.rm = TRUE) +
        geom_point(size = 1.9, na.rm = TRUE) +
        facet_wrap(vars(.data$panel), ncol = 1, scales = "free_y", strip.position = "top") +
        scale_colour_manual(values = pal, breaks = names(pal)) +
        scale_fill_manual(values = pal, breaks = names(pal)) +
        labs(x = NULL, y = NULL, colour = NULL, fill = NULL) +
        ik_ggtheme(is_dark()) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.05),
                                    colour = ik_plot_ink(is_dark())),
          strip.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1.1, "lines"))
    }, bg = "transparent")

    # Click a point → the SAME tabbed drill as the Overview: Summary (per-reserve basis) → Records
    # (a reserve's auditable records, via ik_metric_obs) → Record Details (the inline viewer). The
    # series keeps only network means, so we recompute the one season's per-reserve metric here.
    # Precision follows the Overview's adaptive rule (.ov_dp): camera 1 dp, trap 2–3 dp.
    prefer  <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")
    drill   <- reactiveVal(NULL)   # {cand, sp, taxa, is_cam, R, nrec, dp, unit}
    records <- reactiveVal(NULL)   # records behind a clicked reserve
    rec_ctx <- reactiveVal(NULL)   # caption for the Records tab
    rec_obs <- reactiveVal(NULL)   # observationID open in Record Details

    observeEvent(input$plot_click, {
      cl <- input$plot_click; s <- plotted()
      if (is.null(cl) || is.null(s) || !nrow(s)) return()
      lv <- levels(s$season); xi <- round(cl$x)                  # discrete x → nearest season slot
      if (is.na(xi) || xi < 1 || xi > length(lv)) return()
      cand <- s[s$season == lv[xi] & (is.null(cl$panelvar1) | s$panel == cl$panelvar1) &
                  !is.na(s$value), , drop = FALSE]
      if (!nrow(cand)) return()
      cand <- cand[which.min(abs(cand$value - cl$y)), , drop = FALSE]   # nearest line by value

      is_cam <- identical(cand$metric_type, "camera_rai")
      sci    <- if (is_cam) otx$cam[[cand$taxon]] else otx$trap[[cand$taxon]]   # superset map (covers sub-species)
      taxa   <- stats::setNames(list(sci), cand$taxon)
      sp     <- list(season = ik_expand_period(paste0("season:", cand$season), ik_data), reserve = rsv())
      res <- if (is_cam) ik_rai(ik_data, sp, taxa) else ik_trap_rate(ik_data, sp, taxa)
      R    <- res$summary[res$summary$taxon == cand$taxon & !is.na(res$summary$metric), , drop = FALSE]
      R    <- R[order(R$reserve), , drop = FALSE]
      cnt  <- if (is_cam) "individuals" else "captures"
      Ld   <- res$lines[res$lines$taxon == cand$taxon, , drop = FALSE]
      nrec <- tapply(Ld[[cnt]], Ld$reserve, sum)
      dp   <- .ov_dp(c(cand$value, cand$se, R$metric, R$se), if (is_cam) 1L else 2L, if (is_cam) 1L else 3L)
      unit <- if (is_cam) "camera RAI" else sprintf("captures / %s trap-nights", format(norm, big.mark = ","))
      drill(list(cand = cand, sp = sp, taxa = taxa, is_cam = is_cam, R = R, nrec = nrec, dp = dp, unit = unit))
      records(NULL); rec_obs(NULL); rec_ctx(NULL)
      fd <- paste0("%.", dp, "f")
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s · %s", cand$taxon, cand$season),
          sprintf("network %s = %s%s over %d reserves — open a reserve for its records", unit,
                  sprintf(fd, cand$value),
                  if (!is.na(cand$se)) sprintf(" ± %s SE", sprintf(fd, cand$se)) else "", cand$n_reserves)),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("out_tabs"),
          tabPanel("Summary",        icon = icon("table-list"),  uiOutput(session$ns("out_breakdown"))),
          tabPanel("Records",        icon = icon("list"),        uiOutput(session$ns("out_records_ui"))),
          tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("out_record"))))))
      hideTab(session = session, inputId = "out_tabs", target = "Records")          # revealed on drill
      hideTab(session = session, inputId = "out_tabs", target = "Record details")
    })

    # Summary tab: the per-reserve basis (Reserve · count · RAI/rate · Lines). A reserve with
    # records is clickable → its records (Records tab).
    output$out_breakdown <- renderUI({
      d <- drill(); req(d); R <- d$R; fd <- paste0("%.", d$dp, "f")
      if (!nrow(R)) return(tags$p(class = "ik-drill-summary", "No per-reserve records for this point."))
      cnt_lab <- if (d$is_cam) "Detections" else "Catches"
      tags$table(class = "ik-drill-table",
        tags$thead(tags$tr(tags$th("Reserve"), tags$th(cnt_lab),
                           tags$th(if (d$is_cam) "RAI" else "Rate"), tags$th("Lines"))),
        tags$tbody(lapply(seq_len(nrow(R)), function(i) {
          cn <- d$nrec[R$reserve[i]]; cn <- if (length(cn) == 0 || is.na(cn)) 0L else as.integer(cn)
          drillable <- cn > 0
          tags$tr(class = if (drillable) "ik-drill-row" else NULL,
            title = if (drillable) "Show this reserve's records" else NULL,
            onclick = if (drillable) sprintf(
              "Shiny.setInputValue('%s',{reserve:%s},{priority:'event'})",
              session$ns("out_obs"), .ik_jsq(R$reserve[i])) else NULL,
            tags$td(R$reserve[i]), tags$td(.ov_num(cn)),
            tags$td(sprintf(fd, R$metric[i]), if (!is.na(R$se[i])) sprintf(paste0(" ± ", fd), R$se[i])),
            tags$td(.ov_num(R$n_lines[i])))
        })))
    })

    observeEvent(input$out_obs, {
      d <- drill(); req(d)
      records(ik_metric_obs(ik_data, d$sp, d$taxa, d$cand$taxon, reserve = input$out_obs$reserve,
                            source_type = if (d$is_cam) "camera" else "trap"))
      rec_obs(NULL)
      rec_ctx(sprintf("The %s behind %s · %s · %s. ", if (d$is_cam) "detections" else "captures",
                      d$cand$taxon, input$out_obs$reserve, d$cand$season))
      showTab(session = session, inputId = "out_tabs", target = "Records", select = TRUE)
    })

    observeEvent(input$tab_back, updateTabsetPanel(session, input$tab_back$tabset, selected = input$tab_back$to))

    output$out_records_ui <- renderUI({
      if (is.null(records()))
        return(tags$p(class = "ik-drill-summary", "Open a reserve in the Summary tab to list its records."))
      tagList(
        .ik_tab_back(session$ns("tab_back"), "out_tabs", "Summary", "Back to summary"),
        tags$p(class = "ik-drill-summary", rec_ctx(), tags$b("Click a row"), " for the full record."),
        DT::dataTableOutput(session$ns("out_records_table")))
    })

    output$out_records_table <- DT::renderDT({
      o <- records(); validate(need(!is.null(o) && nrow(o), "No records."))
      when_lab <- .ik_when_label(o$when)
      df <- data.frame(When = when_lab, Species = ik_species_label(o$scientificName, ik_data, prefer()),
                       Count = o$count, Location = o$locationName, ObsID = o$observationID,
                       check.names = FALSE, stringsAsFactors = FALSE)
      df$.when_sort <- as.numeric(o$when)                            # chronological sort key for "When"
      dt <- DT::datatable(df, rownames = FALSE, selection = "single",
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          columnDefs = .ik_dt_when_defs(df, "When", "ObsID")))
      .ik_dt_highlight_row(dt, "ObsID", rec_obs())
    })

    observeEvent(input$out_records_table_rows_selected, {
      i <- input$out_records_table_rows_selected; o <- records()
      if (length(i) && !is.null(o) && i <= nrow(o)) {
        rec_obs(o$observationID[i]); showTab(session = session, inputId = "out_tabs", target = "Record details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("out_records_table"), NULL)
    })

    output$out_record <- renderUI({
      if (is.null(rec_obs()))
        return(tags$p(class = "ik-drill-summary", "Click a record in the Records tab to see it here."))
      ob <- ik_observation(ik_data, rec_obs()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(session$ns("tab_back"), "out_tabs", "Records", "Back to records"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("out_rec_subtabs")))
    })
  })
}
