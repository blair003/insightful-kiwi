# duplicates.R — the "Duplicate window" tuner, in the Cameras tab of Data → Quality. Helps judge the
# project.R `duplicate_window`: a gap-distribution histogram (targets vs the rest) + a live
# sensitivity table as you drag the window, and a burst inspector that puts the actual images of a
# camera × species's detections in a column so you can confirm a flagged run is one individual.
# Camera-only (traps are date-only). All views derive from ik_duplicate_gaps().

#' "How to read this" help body for the Duplicate window tuner — tabbed. @keywords internal
duplicates_help_body <- function() {
  P <- function(...) tags$p(...)
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "One animal that lingers in front of a camera trips it again and again — counted naively, ",
        "that one visitor looks like ", tags$em("many"), " and inflates RAI. The ", tags$b("duplicate window"),
        " collapses a quick run of the ", tags$b("same species at the same camera"), " into one visit."),
      P("This screen helps you ", tags$b("choose"), " that window: drag it and watch how many detections ",
        "collapse, then confirm with the actual images. The chosen window lives in project config (with ",
        "per-species overrides); RAI is reported ", tags$b("net"), " of duplicates.")),
    tabPanel(
      "Reading it", icon = icon("chart-column"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Gap distribution"), " — minutes between consecutive same-species detections at a camera, ",
                "for monitoring targets vs the rest. A ", tags$b("trough"), " between a tight cluster (one animal ",
                "lingering) and the spread-out rest is a natural place to set the window."),
        tags$li(tags$b("Impact by species"), " — at the current window, how many detections each species loses ",
                "as duplicates. Big drops flag species (or a camera) where lingering really matters."),
        tags$li(tags$b("Burst inspector"), " — pick a camera × species and look at the images side by side: is a ",
                "flagged run genuinely ", tags$b("one individual"), " (set the window to catch it) or different ",
                "animals (don't)?"))),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("The flag"), " — a detection is a ", tags$b("possible duplicate"), " if the gap to the ",
                "previous detection of the ", tags$b("same species at the same camera"), " is ≤ the window."),
        tags$li(tags$b("The window"), " — a default minutes value in project config, with optional ",
                tags$b("per-species"), " overrides (a slow lingerer can warrant a longer window than a quick one)."),
        tags$li(tags$b("Net counts"), " — RAI and the Overview detection counts exclude flagged duplicates, so a ",
                "lingering animal counts once."),
        tags$li(tags$b("Camera-only"), " — needs real clock times; trapping is date-only, so it doesn't apply there.")),
      P(tags$em("It's a heuristic on time gaps, not animal recognition — the burst inspector is how you sanity-",
                "check that the window matches what the images actually show.")))
  )
}

#' Duplicate-window tuner UI (a section within the Quality > Cameras tab). @param id Module id.
duplicates_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/duplicates.css")),
    div(class = "ik-dups",
        .ik_page_header("Duplicate window",
            help = .ik_info(ns("dups_help"), "Duplicate window — how to read this", duplicates_help_body())),
        uiOutput(ns("intro")),
        sliderInput(ns("window"), "Possible-duplicate window (minutes)",
                    min = 5, max = 240, value = 30, step = 5, width = "100%"),
        div(class = "ik-dups-grid",
            div(class = "ik-dups-panel",
                tags$h6("Gap distribution — monitoring targets vs the rest"),
                plotOutput(ns("hist"), height = "300px")),
            div(class = "ik-dups-panel",
                tags$h6("Impact by species at this window"),
                DT::dataTableOutput(ns("sens")))),
        tags$hr(),
        tags$h6("Inspect the images — is a flagged run one individual?", class = "ik-dups-subhead"),
        div(class = "ik-dups-pickers",
            selectInput(ns("camera"),  "Camera",  choices = NULL),
            selectInput(ns("species"), "Species", choices = NULL)),
        uiOutput(ns("burst_intro")),
        DT::dataTableOutput(ns("bursts")))
  )
}

#' Duplicate-window tuner server.
#' @param id      Module id.
#' @param ik_data The ik_data container.
duplicates_server <- function(id, ik_data, color_mode = reactive("light")) {
  moduleServer(id, function(input, output, session) {
    is_dark <- reactive(identical(color_mode(), "dark"))
    gaps    <- ik_data$app$duplicate_gaps            # static (all camera data); precomputed at build
    sg         <- ik_species_groups(ik_data)
    target_sci <- unlist(ik_taxa_groups(sg, "monitor", "target"), use.names = FALSE)   # match by sci (robust)
    target_lab <- { l <- unique(sg$label[!is.na(sg$monitor) & sg$monitor == "target"]); if (length(l)) paste(l, collapse = " · ") else "—" }
    fmt_dt  <- function(x) ifelse(is.na(x), "—", format(x, "%d %b %Y · %H:%M"))
    w <- reactive(input$window %||% 30)

    observe({
      req(gaps)
      dup_by_cam <- tapply(gaps$gap, gaps$camera, function(g) sum(g <= 30, na.rm = TRUE))
      updateSelectInput(session, "camera",  choices = names(sort(dup_by_cam, decreasing = TRUE)))  # worst first
      spp <- names(sort(table(gaps$species), decreasing = TRUE))   # most possible-duplicates first
      updateSelectInput(session, "species", choices = spp, selected = spp[1])
    })

    output$intro <- renderUI({
      if (is.null(gaps)) return(tags$p(class = "ik-dups-lead", "No camera detections to assess."))
      W <- w(); n <- nrow(gaps); fl <- sum(gaps$gap <= W, na.rm = TRUE)
      tags$p(class = "ik-dups-lead",
        tags$b(sprintf("%s%% of camera detections flagged at %d min", round(100 * fl / n), W)),
        tags$span(class = "ik-dups-sub", sprintf(
          " — %s of %s. A possible duplicate is the same species at the same camera within the window (excluded from net RAI). Drag the window and watch the monitoring-target groups (%s) stay near 0 — raise it until they start to be affected.",
          format(fl, big.mark = ","), format(n, big.mark = ","), target_lab)))
    })

    output$hist <- renderPlot({
      req(gaps); W <- w()
      d <- gaps[!is.na(gaps$gap) & gaps$gap <= 240, , drop = FALSE]
      tlab <- sprintf("Monitoring targets (%s)", target_lab)
      d$grp <- factor(ifelse(d$scientificName %in% target_sci, tlab, "Other species"),
                      levels = c(tlab, "Other species"))
      ggplot2::ggplot(d, ggplot2::aes(x = gap)) +
        ggplot2::geom_histogram(binwidth = 5, boundary = 0, fill = "#4a7fb5") +
        ggplot2::geom_vline(xintercept = W, linetype = "dashed", colour = "#c0392b", linewidth = 0.7) +
        ggplot2::facet_wrap(~grp, ncol = 1, scales = "free_y") +
        ggplot2::labs(x = "Minutes since previous same-species detection at the camera", y = "Detections") +
        ik_ggtheme(is_dark())
    }, bg = "transparent")

    output$sens <- DT::renderDT({
      req(gaps); W <- w()
      ag <- do.call(rbind, lapply(split(gaps, gaps$species), function(s) {
        rec <- nrow(s); fl <- sum(s$gap <= W, na.rm = TRUE)
        data.frame(Species = s$species[1], Records = rec, Flagged = fl, Net = rec - fl,
                   Rate = round(100 * fl / rec, 1), Target = s$scientificName[1] %in% target_sci,
                   stringsAsFactors = FALSE)
      }))
      ag <- ag[order(-ag$Records), , drop = FALSE]
      DT::datatable(ag, rownames = FALSE, selection = "none", class = "stripe hover row-border",
        options = list(pageLength = 12, dom = "tip",
          columnDefs = list(list(visible = FALSE, targets = which(names(ag) == "Target") - 1L)))) |>
        DT::formatStyle("Target", target = "row",
          fontWeight      = DT::styleEqual(TRUE, "bold"),
          backgroundColor = DT::styleEqual(TRUE, "rgba(47,158,68,0.10)"))
    })

    # Burst inspector — detections of the chosen camera × species, capped to the most recent 150 for
    # the thumbnails. `selcap` + `thumbs` depend on the SELECTION only (not the window), so dragging
    # the slider re-groups/re-flags without re-fetching images.
    selcap <- reactive({
      req(gaps, input$camera, input$species)
      d <- gaps[gaps$camera == input$camera & gaps$species == input$species, , drop = FALSE]
      d <- d[order(d$when), , drop = FALSE]
      d[utils::tail(seq_len(nrow(d)), 150), , drop = FALSE]
    })
    thumbs <- reactive({
      d <- selcap()
      vapply(d$eventID, function(e) {
        if (is.na(e)) return("")
        v <- tryCatch(ik_event_media_view(ik_data, e), error = function(z) NULL)
        if (is.null(v) || !nrow(v)) "" else sprintf('<img src="%s" height="46" loading="lazy">', v$src[1])
      }, character(1))
    })

    output$burst_intro <- renderUI({
      d <- tryCatch(selcap(), error = function(z) NULL); req(d)
      if (!nrow(d)) return(tags$p(class = "ik-dups-sub", "No detections for this camera × species."))
      W <- w(); nb <- max(cumsum(is.na(d$gap) | d$gap > W))
      tags$p(class = "ik-dups-sub", sprintf(
        "Most recent %s detections in %s burst%s at %d min. A burst is what the window collapses to ONE — scan the photos down a burst: same individual, or a new arrival?",
        nrow(d), nb, if (nb == 1) "" else "s", W))
    })

    output$bursts <- DT::renderDT({
      d <- selcap(); validate(need(nrow(d), "No detections.")); W <- w()
      burst <- cumsum(is.na(d$gap) | d$gap > W)
      df <- data.frame(
        Burst = burst, When = fmt_dt(d$when),
        `Gap (min)` = ifelse(is.na(d$gap), "—", as.character(round(d$gap))),
        Photo = thumbs(),
        Flag = ifelse(!is.na(d$gap) & d$gap <= W, "duplicate", ""),
        check.names = FALSE, stringsAsFactors = FALSE)
      df <- df[order(-df$Burst, seq_along(df$Burst)), , drop = FALSE]   # newest burst first, time-asc within
      DT::datatable(df, rownames = FALSE, selection = "none", escape = FALSE,
        class = "stripe hover row-border ik-dups-bursts",
        options = list(pageLength = 15, dom = "tip", ordering = FALSE))
    })
  })
}
