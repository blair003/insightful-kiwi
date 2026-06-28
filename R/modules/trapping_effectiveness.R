# trapping_effectiveness.R — "Trapping effectiveness": does checking more often catch more PER
# trap-night? Catch rate (per norm_trap_days trap-nights, default 100) by check cadence, faceted by austral season so the
# seasonal peak isn't mistaken for a checking effect. Species-filterable; a basis toggle switches the
# denominator between nominal trap-nights (counts dead/sprung time as effort) and operational
# (availability-corrected). Sibling to Bait effectiveness. Data from ik_trap_effectiveness().

#' Body of the "how to read this" help modal — a tabbed (paginated) walkthrough. `norm` is the
#' catch-rate normalisation unit (trap-nights) from project config, woven into the prose so the
#' modal never disagrees with the chart's axis. @keywords internal
trapping_effectiveness_help_body <- function(norm = 100) {
  P <- function(...) tags$p(...)
  ntn <- paste0(format(norm, big.mark = ","), " trap-nights")   # e.g. "100 trap-nights"
  tabsetPanel(
    type = "tabs",

    tabPanel(
      "The question", icon = icon("circle-question"),
      P(tags$br(), tags$b("“Surely checking traps more often catches more?”"), " In raw totals it usually ",
        "looks that way — but two things can inflate that link even when how often you check makes no real ",
        "difference. This screen strips both out so the ", tags$b("data"), " can answer, rather than assuming it."),
      tags$ul(
        tags$li(tags$b("Season."), " Catch rates rise and fall through the year — ", tags$em("when"),
                " the peak falls depends on the species and the place — and people often check more in the ",
                "busier stretches. So checking and catching can move together for reasons that have nothing ",
                "to do with cause and effect."),
        tags$li(tags$b("Effort."), " More checks simply mean more chances to find and record a catch. A raw ",
                "count partly measures how much work was done, not how well the trapping worked.")),
      P("To remove both, the chart uses a ", tags$b("rate"), " (catches per ", ntn, ", not raw counts) ",
        "and compares only ", tags$b("within a season."), " What's left is the real question:"),
      P(tags$em("Per trap-night, and within a given season, does a trap that's checked more often actually ",
                "catch more than one checked rarely?")),
      P("It's a live question because most control here is ", tags$b("kill traps"), ": once one fires it ",
        "can't catch again until someone resets it — so in principle a trap left a long time between checks ",
        "spends much of that gap unable to catch. The chart lets you see whether that shows up in your data.")),

    tabPanel(
      "Reading the chart", icon = icon("chart-column"),
      P(tags$br(), "One ", tags$b("panel per season."), " Within a panel, bars are grouped by how often a ",
        "trap is checked — ", tags$b("tightest (≤7 d) on the left"), " to ", tags$b("sparsest (>30 d) on the right"),
        "; bar height is the catch rate (per ", ntn, "); the figure above each band is how many ",
        tags$b("trap-seasons"), " it pools (across years)."),
      tags$ul(
        tags$li("Bars stepping ", tags$b("down to the right"), " ⇒ tighter checking catches more per ",
                "trap-night — the hypothesis holds for that season."),
        tags$li("Roughly ", tags$b("flat"), " ⇒ how often you check makes little difference; the link was ",
                "season + effort, not checking."),
        tags$li("Which season is busiest varies by species and place — switch ", tags$b("species"), " / ",
                tags$b("reserve"), " (top) to ask of one predator or area.")),
      tags$h6("The two bars: Nominal vs Operational"),
      P(tags$b("Nominal"), " counts every trap-night in full. A kill trap that fires (or is set off) early ",
        "in a long gap still racks up trap-nights it had no chance of catching on — so under nominal effort, ",
        "sparse checking reads as a low rate."),
      P(tags$b("Operational"), " strips out that dead time. Once a kill trap has fired it can't catch again ",
        "until someone resets it at the next check, so the rest of that interval is ", tags$b("dead"),
        ": effort on paper, no chance of a catch. We don't know ", tags$em("when"), " in the interval it ",
        "fired — taking the catch as equally likely at any moment, the trap was live for about ", tags$b("half"),
        " the interval on average, so an interval ending in a catch or sprung trap is credited as a ",
        tags$b("half-night"), " rather than a full one. (This is the standard ", tags$b("Nelson–Clark"),
        " sprung-trap correction — the expected live time under the simplest assumption about catch timing, ",
        "not an arbitrary fudge.)"),
      P("Operational therefore has the ", tags$b("smaller"), " denominator and the higher rate. If switching ",
        "to it ", tags$b("lifts the sparse (right-hand) bars and flattens"), " the gradient, the apparent ",
        "“checking more catches more” was really just traps sitting idle and dead between checks — not the ",
        "checking itself.")),

    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Cadence"), " = a trap's ", tags$b("mean check interval"), " over the data (the ",
                "average number of days between its checks)."),
        tags$li(tags$b("Catch rate"), " = catches of the chosen species ÷ trap-nights × ",
                paste0(format(norm, big.mark = ","), ".")),
        tags$li(tags$b("Bands"), " = ≤7 d · 7–14 d · 14–30 d · >30 d, by that mean interval."),
        tags$li(tags$b("Pooling"), " = each (season-of-year × band) sums catches and trap-nights across ",
                "all years of that season (e.g. every summer pooled, every autumn pooled), for sample ",
                "size and to hold season-of-year fixed."),
        tags$li(tags$b("Operational trap-nights"), " = nominal, but an interval ending in a catch/sprung ",
                "event counts as a half-night.")),
      P(tags$em("Frozen on import — the bands recompute only when data is re-imported, so they don't shift ",
                "as you click around.")),
      tags$h6("Caveats"),
      tags$ul(
        tags$li(tags$b("Small bands are noisy"), " — bands under ", tags$b(ntn), " are dropped; ",
                "the figure above each band is the trap-seasons it pools."),
        tags$li(tags$b("Which lines, not just how often"), " — within-season holds season constant, but not ",
                "which traplines are worked (keen volunteers tend to work the high-predator lines too), so read ",
                "a gradient as ", tags$b("suggestive, not proof.")),
        tags$li(tags$b("Species mix"), " — “All predators” is dominated by rats; pick a species (mustelids sit ",
                "much lower) to see its own pattern.")))
  )
}

#' Catch-efficiency TAB body — embedded under "Check frequency → Catch efficiency" (no longer a top-level
#' nav). The species picker is inline; Period + Reserve come from the shared trap-review sidebar selection.
#' @param id Module id. @param ik_data The container (species choices baked in — selectize renders lazily).
trapping_effectiveness_body <- function(id, ik_data) {
  ns  <- NS(id)
  sg  <- ik_species_groups(ik_data)
  ctl <- ik_taxa_groups(sg, "control", "target")
  splits <- unique(sg$label[which(sg$split)])
  sp_choices  <- ik_species_choices(ctl, ik_data, "vernacular", splits,
                                     all_label = "All predators", all_value = "__all__")
  norm <- ik_data$meta$trapping$rate$norm_trap_days %||% 100        # catch-rate normalisation unit (trap-nights)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/trapping_effectiveness.css")),
    div(class = "trap-tab-help",
        tags$span(class = "trap-lead", "Does checking traps more often result in a higher catch rate?"),
        .ik_info(ns("teff_help"), "Catch efficiency — how to read this",
                 trapping_effectiveness_help_body(norm))),
    div(class = "teff-controls",
        selectInput(ns("species"), "Captures of", choices = sp_choices, selected = "__all__", width = "200px")),
    uiOutput(ns("note")),
    plotOutput(ns("plot"), height = "440px"))
}

#' Trapping effectiveness server.
#' @param id Module id. @param ik_data The container.
#' @param prefer_scientific reactive name preference (reserved; labels here are predators).
#' @param color_mode reactive theme ("light"/"dark").
trapping_effectiveness_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                                          color_mode = reactive("light"), selection = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    is_dark <- reactive(identical(color_mode(), "dark"))
    sg  <- ik_species_groups(ik_data)
    ctl <- ik_taxa_groups(sg, "control", "target")
    taxa_r    <- reactive({ v <- input$species %||% "__all__"; if (identical(v, "__all__")) NULL else ik_resolve_species_choice(v, ctl) })
    reserve_r <- reactive(.ik_nz(selection()$reserve))        # Reserve now comes from the shared sidebar selection
    output$period_banner <- renderUI(.ik_period_banner(ik_data, selection()))

    data <- reactive(ik_trap_effectiveness(ik_data, taxa_r(), reserve_r(), seasons = .ik_nz(selection()$season))) |>
      bindCache(input$species, selection()$reserve, selection()$season, ik_active_datasets())

    output$note <- renderUI({
      d <- data(); if (is.null(d) || !nrow(d)) return(NULL)
      tags$p(class = "teff-note",
        tags$b("n above each band = the trap-seasons behind it"), " — the smaller it is, the noisier the bar, ",
        "so weight tall bars with a big n. Two bars per band: ", tags$b("Nominal"), " vs ",
        tags$b("Operational"), " trap-nights. (Period defaults to the last year — pick ", tags$b("All data"),
        " in the sidebar for the fullest pattern.)")
    })

    output$plot <- renderPlot({
      d <- data(); validate(need(!is.null(d) && nrow(d), "No trap captures to analyse in this selection."))
      norm <- attr(d, "norm")
      ggplot2::ggplot(d, ggplot2::aes(.data$band, .data$rate, fill = .data$basis)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.72, na.rm = TRUE) +
        # one sample-size label per band, above the taller (Operational) bar — the reliability cue, so make
        # it read: bold, "n=" prefixed (it's the trap-seasons behind the bar, not a catch count).
        ggplot2::geom_text(data = d[d$basis == "Operational", , drop = FALSE],
                           ggplot2::aes(label = paste0("n=", .data$n_traps)), vjust = -0.5, size = 3.5,
                           fontface = "bold", colour = ik_plot_ink(is_dark()), na.rm = TRUE, show.legend = FALSE) +
        ggplot2::facet_wrap(ggplot2::vars(.data$season), nrow = 1) +
        ggplot2::scale_fill_manual(values = c(Nominal = "#6a3d9a", Operational = "#b39ddb")) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
        ggplot2::labs(x = "Check cadence (mean interval)",
                      y = sprintf("Catch rate / %g trap-nights", norm), fill = NULL) +
        ik_ggtheme(is_dark()) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "bottom",
          strip.text = ggplot2::element_text(face = "bold", colour = ik_plot_ink(is_dark())),
          strip.background = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(1, "lines"))
    }, bg = "transparent")
  })
}
