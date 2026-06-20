# outcomes.R — the "Are we winning?" view: the trust's causal chain on one seasonal timeline.
# Three stacked panels (read top→bottom): trap catches (the control), predator camera RAI
# (want DOWN), kiwi camera RAI (want UP). Network level (mean over reserves ± SE). Data from
# ik_outcome_series(); plotted as ggplot2 small multiples.

# Thematic palette: predators warm, kiwi green.
OUTCOME_PALETTE <- c(Mustelids = "#c62828", Rats = "#ef6c00", Cats = "#8e24aa",
                     Dogs = "#6d4c41", Hedgehogs = "#5d4037", Possums = "#795548",
                     Kiwi = "#2e7d32")

OUTCOME_PANELS <- c("Catches · per 100 trap-nights",
                    "Predators · camera RAI (lower is better)",
                    "Kiwi · camera RAI (higher is better)")

#' Outcomes nav panel UI. @param id Module id.
outcomes_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Are we winning?", value = "outcomes", icon = icon("chart-line"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/outcomes.css"),
    div(class = "ik-outcomes",
        uiOutput(ns("intro")),
        div(class = "out-controls",
            checkboxGroupInput(ns("taxa"), "Show species", choices = NULL, inline = TRUE)),
        plotOutput(ns("plot"), height = "660px", click = ns("plot_click")))
  )
}

#' Outcomes server. @param id Module id. @param ik_data The ik_data container.
outcomes_server <- function(id, ik_data) {
  moduleServer(id, function(input, output, session) {
    series <- reactive(ik_outcome_series(ik_data))    # ~7s, computed once per session

    # Species toggle — default to the core stoat-vs-kiwi story so the chart reads cleanly
    # (rats otherwise dominate the predator scale); the rest are opt-in.
    sg    <- ik_species_groups(ik_data)
    avail <- { s <- sg[sg$role %in% c("predator", "protected") & !is.na(sg$monitor), , drop = FALSE]
               unique(s$label[order(s$priority)]) }
    updateCheckboxGroupInput(session, "taxa", choices = avail,
                             selected = intersect(c("Mustelids", "Kiwi"), avail))

    output$intro <- renderUI({
      projects <- unique(unlist(lapply(ik_data$datasets, function(d) d$meta$project)))
      tagList(
        tags$h3(class = "ik-out-title", "Are we winning?"),
        tags$p(class = "ik-out-lead",
          tagList(paste(projects, collapse = " · "), " — the control story across seasons. ",
            "We ", tags$b("trap predators"), " → predator abundance on camera should ",
            tags$b("fall"), " → kiwi abundance should ", tags$b("rise"), ". ",
            "Lines are the network mean across reserves; bands are ± 1 SE. ",
            tags$b("Click a point"), " for its per-reserve breakdown."))
      )
    })

    # the data currently plotted (panel + ordered season factor) — read by the point-click handler
    plotted <- reactiveVal(NULL)

    output$plot <- renderPlot({
      s <- series()
      validate(need(!is.null(s) && nrow(s), "Not enough data to chart outcomes."))
      s <- s[s$taxon %in% input$taxa, , drop = FALSE]
      validate(need(nrow(s) > 0, "Select at least one species to chart."))
      s$panel <- factor(ifelse(s$metric_type == "trap_rate", OUTCOME_PANELS[1],
                        ifelse(s$role == "protected", OUTCOME_PANELS[3], OUTCOME_PANELS[2])),
                        levels = OUTCOME_PANELS)
      s$season <- factor(s$season, levels = unique(s$season[order(s$season_order)]))
      plotted(s)
      pal <- OUTCOME_PALETTE[names(OUTCOME_PALETTE) %in% unique(s$taxon)]

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
        theme_minimal(base_size = 13) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.05)),
          strip.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1.1, "lines"))
    })

    # Click a point → the per-reserve breakdown behind that (taxon × season) network value. The
    # series keeps only network means, so we recompute the one season's per-reserve metric here
    # (~0.5s) — wrapped in a progress indicator so the click clearly registers.
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
      sci    <- sg$scientificName[sg$label == cand$taxon & !is.na(sg$scientificName)]
      taxa   <- stats::setNames(list(sci), cand$taxon)
      sp     <- list(season = ik_expand_period(paste0("season:", cand$season), ik_data))
      R <- withProgress(message = "Loading breakdown…", value = 0.6, {
        res <- if (is_cam) ik_rai(ik_data, sp, taxa) else ik_trap_rate(ik_data, sp, taxa)
        res$summary
      })
      R  <- R[R$taxon == cand$taxon & !is.na(R$metric), , drop = FALSE]
      R  <- R[order(R$reserve), , drop = FALSE]
      dg <- if (is_cam) 2L else 3L; fd <- paste0("%.", dg, "f")
      unit <- if (is_cam) "camera RAI" else "captures / 100 trap-nights"
      body <- if (!nrow(R)) tags$p("No per-reserve records for this point.")
        else tags$table(class = "ik-drill-table",
          tags$thead(tags$tr(tags$th("Reserve"), tags$th(if (is_cam) "RAI" else "Rate"), tags$th("Lines"))),
          tags$tbody(lapply(seq_len(nrow(R)), function(i) tags$tr(
            tags$td(R$reserve[i]),
            tags$td(sprintf(fd, R$metric[i]), if (!is.na(R$se[i])) sprintf(paste0(" ± ", fd), R$se[i])),
            tags$td(.ov_num(R$n_lines[i]))))))
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s · %s", cand$taxon, cand$season),
          sprintf("network %s = %s%s over %d reserves — the per-reserve basis below", unit,
                  sprintf(fd, cand$value),
                  if (!is.na(cand$se)) sprintf(" ± %s SE", sprintf(fd, cand$se)) else "",
                  cand$n_reserves)),
        body, easyClose = TRUE, size = "m", footer = modalButton("Close")))
    })
  })
}
