# top_trappers.R (module) — "Top trappers": a gamified per-season leaderboard. A completed-season
# picker, a top-3 podium, and a full ranked table with trophy + badge chips. Data from
# ik_top_trappers(). By volunteer ID for now (name mappings later). Sits under the Trapping menu.

#' "How to read this" help body — tabbed. `weights` named catch-weights, `labels` matching vernaculars.
#' @keywords internal
top_trappers_help_body <- function(weights, labels) {
  P <- function(...) tags$p(...)
  ord <- order(-weights)
  wlist <- tags$ul(lapply(ord, function(i) if (weights[i] > 0)
    tags$li(tags$b(labels[i]), " × ", weights[i])))
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "A friendly ", tags$b("leaderboard"), " of the volunteers checking the traps, for one ",
        tags$b("completed season"), " at a time. Each trap check is credited to whoever did it; a check ",
        "that found a kill is a ", tags$b("catch"), ".")),
    tabPanel(
      "Awards", icon = icon("medal"),
      tags$h6("Trophies (one winner each)"),
      tags$ul(
        tags$li(tags$b("Top Trapper"), " — highest weighted score."),
        tags$li(tags$b("Most Catches"), " · ", tags$b("Hardest Worker"), " (most checks) · ",
                tags$b("Most Variety"), " (most species) · ", tags$b("Most Ground Covered"), " (most traps)."),
        tags$li(tags$b("Rat Patrol"), " (most rats) · ", tags$b("Bait Innovator"), " (most baits tried) · ",
                tags$b("Sharpshooter"), " (best catches-per-check, ≥ 24 checks)."),
        tags$li(tags$b("Reserve champion"), " — top score in each reserve, so every area has a winner.")),
      tags$h6("Badges (everyone who clears the bar earns one)"),
      tags$ul(
        tags$li(tags$b("Stoat Slayer"), " (caught a stoat — rare, so it's a badge of honour, not a trophy) · ",
                tags$b("Mustelid Hunter"), " (caught any mustelid)."),
        tags$li(tags$b("All-rounder"), " (3+ species) · ", tags$b("Quarter-century"), " / ",
                tags$b("Half-century"), " (25 / 50+ catches) · ", tags$b("Stalwart"), " (40+ checks).")),
      P("Badges are the backbone — most active trappers earn at least one.")),
    tabPanel(
      "How it's scored", icon = icon("calculator"),
      P(tags$br(), "Score = the sum of catch ", tags$b("weights"), " — predators that matter most to ",
        "kiwi count for more, so quality beats quantity (a stoat outweighs several rats):"),
      wlist,
      P("Weights are project settings (", tags$code("trapping$target_weights"), " in project.R). Bycatch ",
        "(birds, unidentified) scores zero."),
      tags$h6("Caveat"),
      P("A catch is credited to whoever ", tags$b("checked"), " the trap and found it — so this rewards ",
        "active checkers, which is rather the point, but it isn't the same as who set the trap well.")))
}

#' Top-trappers nav panel. @param id Module id. @param ik_data The container (season choices + the
#'   help's weight table baked in — a dropdown nav panel renders its selectize lazily).
top_trappers_ui <- function(id, ik_data) {
  ns <- NS(id)
  ss <- ik_top_trapper_seasons(ik_data)
  ch <- if (is.null(ss)) c("No completed seasons" = "") else
    stats::setNames(paste(ss$season_year, ss$season, sep = "|"), ss$calendar_season)
  w  <- ik_data$meta$trapping$target_weights %||% IK_DEFAULT_TRAP_WEIGHTS
  lbl <- ik_species_label(names(w), ik_data, "vernacular")
  nav_panel(
    "Top trappers", value = "top-trappers", icon = icon("medal"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/top_trappers.css")),
    div(class = "ik-tt",
        .ik_page_header("Top trappers — season leaderboard",
            description = tagList("Who's checking the traps and catching the predators that matter — ranked for one completed ",
              "season. Scored by weighted catches (a stoat counts for more than a rat); badges reward effort ",
              "and variety, not just the top of the table."),
            help = .ik_info(ns("tt_help"), "Top trappers — how it works", top_trappers_help_body(w, lbl))),
        div(class = "ik-tt-controls",
            selectInput(ns("season"), "Season", choices = ch, selected = unname(ch)[1], width = "220px")),
        uiOutput(ns("note")),
        uiOutput(ns("podium")),
        DT::DTOutput(ns("board")))
  )
}

#' Top-trappers server.
#' @param id Module id. @param ik_data The container. @param prefer_scientific reactive name pref.
#' @param active reactive: is this page's nav visible.
top_trappers_server <- function(id, ik_data, prefer_scientific = reactive(FALSE), active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    board <- reactive({
      req(active())
      sv <- strsplit(input$season %||% "", "|", fixed = TRUE)[[1]]
      if (length(sv) >= 2) ik_top_trappers(ik_data, season_year = as.integer(sv[1]), season = sv[2])
      else                 ik_top_trappers(ik_data)               # default: latest completed
    }) |> bindCache(active(), input$season, ik_active_datasets())

    .chips <- function(troph, badg) {
      t <- vapply(troph, function(x) sprintf('<span class="tt-chip tt-trophy">%s</span>', htmltools::htmlEscape(x)), "")
      b <- vapply(badg,  function(x) sprintf('<span class="tt-chip tt-badge">%s</span>',  htmltools::htmlEscape(x)), "")
      v <- c(t, b); if (!length(v)) "<span class=\"tt-none\">—</span>" else paste(v, collapse = " ")
    }

    output$note <- renderUI({
      tt <- board(); if (is.null(tt)) return(NULL)
      tags$p(class = "ik-tt-note",
        tags$b(tt$label), " — ", tt$n_active, " trappers, ",
        tags$b(sprintf("%d earned an award", tt$n_awarded)),
        sprintf(" (%.0f%%). Sorted by weighted score; a stoat counts most.", 100 * tt$n_awarded / max(tt$n_active, 1)))
    })

    output$podium <- renderUI({
      tt <- board(); req(!is.null(tt))
      p <- utils::head(tt$per, 3)
      card <- function(i) div(class = sprintf("ik-tt-pod ik-tt-pod-%d", p$rank[i]),
        div(class = "ik-tt-medal", p$rank[i]),
        div(class = "ik-tt-vol", paste0("Volunteer ", p$volunteer[i])),
        div(class = "ik-tt-score", p$score[i], tags$span(class = "ik-tt-score-u", " pts")),
        div(class = "ik-tt-podstat", sprintf("%d catches · %d checks · top: %s",
            p$catches[i], p$checks[i], p$top_species[i])),
        div(class = "ik-tt-podchips", HTML(.chips(p$trophies[[i]], p$badges[[i]]))))
      div(class = "ik-tt-podium", lapply(seq_len(nrow(p)), card))
    })

    output$board <- DT::renderDT({
      tt <- board(); validate(need(!is.null(tt), "No completed season with trapper data to rank."))
      p <- tt$per
      df <- data.frame(
        Rank = p$rank, Volunteer = paste0("#", p$volunteer), Score = p$score, Catches = p$catches,
        Checks = p$checks, `Catch/10` = p$rate10, Species = p$species, `Top species` = p$top_species,
        `Top bait` = ifelse(is.na(p$top_bait), "—", p$top_bait),
        Awards = mapply(.chips, p$trophies, p$badges, USE.NAMES = FALSE),
        check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "none", class = "stripe hover row-border ik-tt-board",
        escape = which(names(df) != "Awards"),                    # render the Awards chips as HTML
        options = list(pageLength = 15, scrollX = TRUE, dom = "tip", order = list()))
    })
  })
}
