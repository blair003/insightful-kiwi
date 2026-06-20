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
        plotOutput(ns("plot"), height = "660px"))
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
            "Lines are the network mean across reserves; bands are ± 1 SE."))
      )
    })

    output$plot <- renderPlot({
      s <- series()
      validate(need(!is.null(s) && nrow(s), "Not enough data to chart outcomes."))
      s <- s[s$taxon %in% input$taxa, , drop = FALSE]
      validate(need(nrow(s) > 0, "Select at least one species to chart."))
      s$panel <- factor(ifelse(s$metric_type == "trap_rate", OUTCOME_PANELS[1],
                        ifelse(s$role == "protected", OUTCOME_PANELS[3], OUTCOME_PANELS[2])),
                        levels = OUTCOME_PANELS)
      s$season <- factor(s$season, levels = unique(s$season[order(s$season_order)]))
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
  })
}
