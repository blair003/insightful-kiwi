# reserve_report.R — "Reserve report": the whole predator-control story for one reserve on a single
# time axis — effort (check interval) → servicing health → catch rate (trapping outcome) → predator
# activity on camera → protected activity on camera. The OVER-TIME companion to "Are we winning?"
# (which is the snapshot). Cross-device synthesis (camera + trap), so it lives under Insights. Pure
# assembly of ik_trap_review_series() (3 trapping panels) + ik_neighbourhood_series(reserve) (camera).

#' Combined per-period series for one reserve. Tidy df period·order·facet·series·value, the five
#' panels ordered effort→outcome; NULL when neither device has data. @keywords internal
.reserve_report_series <- function(ik_data, reserve, by, predator_sci, protected_sci) {
  tr <- tryCatch(ik_trap_review_series(ik_data, by, reserve), error = function(e) NULL)
  nb <- tryCatch(ik_neighbourhood_series(ik_data, "reserve", reserve, 0, predator_sci, protected_sci, by),
                 error = function(e) NULL)
  if (is.null(tr) && is.null(nb)) return(NULL)
  catch_facet <- if (is.null(tr)) NULL else grep("^Catch rate", unique(tr$facet), value = TRUE)[1]
  parts <- if (is.null(tr)) list() else list(tr)
  if (!is.null(nb)) {
    cam <- nb[nb$facet == attr(nb, "cam_facet"), , drop = FALSE]
    if (!is.null(tr)) cam <- cam[cam$period %in% tr$period, , drop = FALSE]   # align to completed periods
    if (nrow(cam)) {
      cam$facet <- ifelse(cam$series == "Predator", "Predators on camera (RAI)", "Protected on camera (RAI)")
      parts[[length(parts) + 1]] <- cam[, c("period", "order", "facet", "series", "value")]
    }
  }
  out <- do.call(rbind, parts); if (is.null(out) || !nrow(out)) return(NULL)
  base <- if (!is.null(tr)) tr else nb                                          # canonical period order
  out$period <- factor(out$period, levels = unique(base$period[order(base$order)]))
  flev <- c("Servicing (% of judged traps)", "Median check interval (days)", catch_facet,
            "Predators on camera (RAI)", "Protected on camera (RAI)")
  flev <- flev[!is.na(flev) & flev %in% out$facet]
  out$facet <- factor(out$facet, levels = flev)
  out <- out[!is.na(out$facet) & !is.na(out$period), , drop = FALSE]
  attr(out, "incomplete_period") <- attr(tr, "incomplete_period")
  out
}

#' "How to read this" help body for Reserve report. @keywords internal
reserve_report_help_body <- function() {
  P <- function(...) tags$p(...)
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "The whole control story for one reserve, season by season, as a single stack — read it ",
        "top to bottom as a ", tags$b("chain"), ":"),
      tags$ul(
        tags$li(tags$b("Servicing"), " — the mix of good / watch / neglected traps (are they being run?)."),
        tags$li(tags$b("Check interval"), " — the median days between checks (the effort behind it)."),
        tags$li(tags$b("Catch rate"), " — captures per trap-night (the trapping outcome)."),
        tags$li(tags$b("Predators on camera"), " — is control actually thinning predators?"),
        tags$li(tags$b("Protected on camera"), " — are the natives responding?")),
      P("If the chain holds — good servicing, catches, predators falling, protected rising — you're winning. ",
        "Where it breaks (e.g. catches up but predators-on-camera flat) tells you ", tags$b("where"), " to look.")),
    tabPanel(
      "Reading it", icon = icon("chart-line"),
      P(tags$br(), "Each panel has its ", tags$b("own y-axis"), " — read each for ", tags$b("shape and direction"),
        " over time, not absolute height against the others. The camera lines are effort-adjusted rates (RAI); ",
        "the catch line is a rate per trap-night; servicing is a percentage of judged traps."),
      P("It's ", tags$b("observational"), ": the panels moving together is suggestive of the control working, not ",
        "proof — weather, season and immigration all play a part. The current in-progress period is omitted ",
        "(too few re-checks to read).")))
}

#' Reserve-report nav panel. @param id Module id. @param ik_data The container (choices baked in — a
#'   dropdown nav panel renders its selectize lazily and would miss a server-set default).
#' Reserve-report sidebar controls — the reserve / grain / predator / protected pickers, moved out of the
#' page into the rail (DATA PERIOD · View options · Filters). Same input ids, so the server reads
#' input$reserve / grain / pred / prot unchanged. @keywords internal
reserve_report_controls <- function(id, ik_data) {
  ns <- NS(id)
  sg <- ik_species_groups(ik_data)
  rt <- function(role) { l <- unique(sg$label[sg$role == role & !is.na(sg$monitor)])
    stats::setNames(lapply(l, function(x) sg$scientificName[sg$label == x & !is.na(sg$scientificName)]), l) }
  splits <- unique(sg$label[which(sg$split)])
  pred_taxa <- rt("predator"); prot_taxa <- rt("protected")
  res <- sort(unique(ik_data$app$geography$locations$reserve[!is.na(ik_data$app$geography$locations$reserve)]))
  tagList(
    selection_all_data(),                                  # DATA PERIOD · All data (the report spans every season)
    div(class = "ik-selection ik-view-controls",
        tags$div(class = "ik-view-controls-h", "View options"),
        selectInput(ns("pred"), "Predator",
                    choices = ik_species_choices(pred_taxa, ik_data, "vernacular", splits),
                    selected = paste0("grp:", names(pred_taxa)[1]), multiple = TRUE),
        selectInput(ns("prot"), "Protected",
                    choices = ik_species_choices(prot_taxa, ik_data, "vernacular", splits),
                    selected = paste0("grp:", names(prot_taxa)[1]), multiple = TRUE),
        radioButtons(ns("grain"), "Group by",
                     choices = c("By season" = "season", "By year" = "year"), selected = "season")),
    div(class = "ik-selection",
        tags$div(class = "ik-sel-section-h", "Filters"),
        selectInput(ns("reserve"), "Reserve", choices = res, selected = res[1]))
  )
}

reserve_report_ui <- function(id, ik_data) {
  ns  <- NS(id)
  nav_panel(
    "Reserve report", value = "reserve-report", icon = icon("layer-group"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/reserve_report.css")),
    div(class = "ik-rr",
        .ik_page_header("Reserve report — the whole story over time",
            description = tagList("One reserve, season by season, as a single chain: effort → servicing → catches → predators on ",
              "camera → protected on camera. Read top-to-bottom — if it all moves the right way, you're winning."),
            help = .ik_info(ns("rr_help"), "Reserve report — how to read this", reserve_report_help_body()),
            banner = div(class = "ik-page-period", .ik_period_banner(ik_data, NULL, all_data = TRUE))),  # toggles the rail (esp. on mobile)
        uiOutput(ns("note")),
        plotOutput(ns("plot"), height = "780px"))
  )
}

#' Reserve-report server. @param id Module id. @param ik_data The container.
#' @param prefer_scientific reactive name pref. @param color_mode reactive theme.
reserve_report_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                                  color_mode = reactive("light")) {
  moduleServer(id, function(input, output, session) {
    is_dark <- reactive(identical(color_mode(), "dark"))
    sg <- ik_species_groups(ik_data)
    rt <- function(role) { l <- unique(sg$label[sg$role == role & !is.na(sg$monitor)])
      stats::setNames(lapply(l, function(x) sg$scientificName[sg$label == x & !is.na(sg$scientificName)]), l) }
    pred_taxa <- rt("predator"); prot_taxa <- rt("protected")

    series <- reactive({
      .reserve_report_series(ik_data, input$reserve, input$grain %||% "season",
        predator_sci  = ik_resolve_species_choice(input$pred, pred_taxa),
        protected_sci = ik_resolve_species_choice(input$prot, prot_taxa))
    }) |> bindCache(input$reserve, input$grain, input$pred, input$prot, ik_active_datasets())

    output$note <- renderUI({
      s <- series(); inc <- if (is.null(s)) NULL else attr(s, "incomplete_period")
      if (is.null(inc)) return(NULL)
      tags$p(class = "ik-rr-note",
        sprintf("Completed periods only — the current period (%s) is still in progress and is omitted.", inc))
    })

    output$plot <- renderPlot({
      s <- series(); validate(need(!is.null(s) && nrow(s), "Not enough data to build the report for this reserve."))
      pal <- c(Good = "#2e7d32", Watch = "#f9a825", Neglected = "#c62828", Interval = "#5a6b7b",
               `Catch rate` = "#6a3d9a", Predator = "#c62828", Protected = "#2e7d32")
      ggplot2::ggplot(s, ggplot2::aes(.data$period, .data$value, colour = .data$series, group = .data$series)) +
        ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) + ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
        ggplot2::facet_wrap(ggplot2::vars(.data$facet), ncol = 1, scales = "free_y", strip.position = "top") +
        ggplot2::scale_colour_manual(values = pal, breaks = c("Good", "Watch", "Neglected")) +
        ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.08))) +
        ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom",
          strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = ggplot2::rel(1.02), colour = ik_plot_ink(is_dark())),
          strip.background = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1, "lines"))
    }, bg = "transparent")
  })
}
