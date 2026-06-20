# overview.R — the Overview page. A project header for the current selection, then a
# section PER DEVICE: camera monitoring (detections · camera-hours) and trapping
# (catches · trap-nights) — because a camera detection and a trap kill are different
# things, use different effort units, and care about different species. Driven by a
# selection SPEC (period/geography), resolved per device with ik_resolve().
# First increment — RAI/metrics come later.

#' Overview nav panel UI.
#' @param id Module id.
#' @return A bslib nav_panel for page_navbar().
overview_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Overview", value = "overview", icon = icon("gauge"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/overview.css"),
    div(
      class = "ik-overview",
      uiOutput(ns("header")),
      uiOutput(ns("camera")),
      uiOutput(ns("trapping"))
    )
  )
}

#' Format a count with thousands separators; `—` for NA/empty. @keywords internal
.ov_num <- function(x) if (length(x) == 0 || is.na(x)) "—" else format(x, big.mark = ",")

#' Change direction at DISPLAY precision — compares values rounded to the same decimals the
#' cell shows, so two figures that render identically (e.g. "0.0" vs "0.0") read as flat, not
#' a spurious ▲/▼ off invisible floating-point difference. @keywords internal
.ov_dir <- function(cur, prv, digits) {
  c <- round(cur, digits); p <- round(prv, digits)
  if (c > p) "up" else if (c < p) "down" else "flat"
}

#' onclick that fires the species-panel drill: `input$<spp_drill> = {kind, label, sci[]}`.
#' `sci` is the scientificName(s) the item stands for (a group may be several). @keywords internal
.ov_spp_onclick <- function(spp_drill, kind, sci, label) {
  sprintf("Shiny.setInputValue('%s',{kind:'%s',label:%s,sci:%s},{priority:'event'})",
          spp_drill, kind, jsonlite::toJSON(label, auto_unbox = TRUE),
          jsonlite::toJSON(as.character(sci)))
}

#' One titled block of label/count items (faint when zero). When `sci_sets` (a list parallel to
#' `labels`, each the item's scientificName vector) and `spp_drill` are given, every NON-ZERO
#' item with a known species set is clickable → its records. @keywords internal
.ov_class_section <- function(title, labels, counts, sci_sets = NULL, kind = NULL, spp_drill = NULL) {
  if (!length(labels)) return(NULL)
  drillable <- !is.null(sci_sets) && !is.null(spp_drill)
  tags$div(
    class = "ik-spp-class",
    tags$h6(title),
    tags$div(class = "ik-spp-grid", lapply(seq_along(labels), function(i) {
      can <- drillable && counts[i] > 0 && length(sci_sets[[i]]) > 0
      cls <- c("ik-spp-item", if (counts[i] == 0) "ik-spp-zero", if (can) "ik-spp-click")
      tags$div(
        class   = paste(cls, collapse = " "),
        title   = if (can) "Show these records",
        onclick = if (can) .ov_spp_onclick(spp_drill, kind, sci_sets[[i]], labels[i]),
        tags$span(class = "ik-spp-label", labels[i]),
        tags$span(class = "ik-spp-count", .ov_num(counts[i]))
      )
    }))
  )
}

#' Monitoring (camera) species panel: the curated registry groups by `monitor` class
#' (target, then interesting), zeros shown — a fixed scorecard. @keywords internal
.ov_panel_monitor <- function(obs, sg, kind = NULL, spp_drill = NULL) {
  obs$group <- sg$group[match(obs$scientificName, sg$scientificName)]
  reg <- sg[!duplicated(sg$group) & !is.na(sg$monitor), c("group", "label", "monitor", "priority")]
  reg <- reg[order(reg$priority), ]
  counts <- table(obs$group)
  reg$n  <- as.integer(counts[reg$group]); reg$n[is.na(reg$n)] <- 0L
  grp_sci <- function(g) unique(sg$scientificName[!is.na(sg$group) & sg$group == g & !is.na(sg$scientificName)])

  classes <- c(intersect(c("target", "interesting"), unique(reg$monitor)),
               setdiff(unique(reg$monitor), c("target", "interesting")))
  sections <- lapply(classes, function(cl) {
    r <- reg[reg$monitor == cl, , drop = FALSE]
    .ov_class_section(tools::toTitleCase(cl), r$label, r$n,
                      sci_sets = lapply(r$group, grp_sci), kind = kind, spp_drill = spp_drill)
  })
  tagList(sections, tags$div(class = "ik-spp-other",
    sprintf("Other species: %s detections", .ov_num(sum(is.na(obs$group))))))
}

#' Control (trapping) species panel: the control TARGET groups (zeros shown), then ALL
#' OTHER species actually caught (data-driven, by registry label or vernacular, sorted by
#' count) — so bycatch shows up and monitoring-only species don't sit at 0. @keywords internal
.ov_panel_control <- function(obs, sg, ik_data, prefer, kind = NULL, spp_drill = NULL) {
  obs$group <- sg$group[match(obs$scientificName, sg$scientificName)]
  obs$label <- sg$label[match(obs$scientificName, sg$scientificName)]

  ct <- sg[!duplicated(sg$group) & !is.na(sg$control) & sg$control == "target",
           c("group", "label", "priority")]
  ct <- ct[order(ct$priority), ]
  counts <- table(obs$group)
  ct$n   <- as.integer(counts[ct$group]); ct$n[is.na(ct$n)] <- 0L
  grp_sci <- function(g) unique(sg$scientificName[!is.na(sg$group) & sg$group == g & !is.na(sg$scientificName)])

  # everything else caught (not a control-target group), by display name, count desc
  other <- obs[!(obs$group %in% ct$group), , drop = FALSE]
  other_section <- NULL
  if (nrow(other)) {
    disp <- other$label
    nl   <- is.na(disp)
    disp[nl] <- ik_species_label(other$scientificName[nl], ik_data, prefer)
    disp[is.na(disp)] <- "Unidentified"
    oc       <- sort(table(disp), decreasing = TRUE)
    by_disp  <- tapply(other$scientificName, disp, function(x) unique(x[!is.na(x)]))  # sci per row
    other_section <- .ov_class_section("All other caught", names(oc), as.integer(oc),
      sci_sets = lapply(names(oc), function(d) by_disp[[d]]), kind = kind, spp_drill = spp_drill)
  }
  tagList(
    .ov_class_section("Target", ct$label, ct$n,
                      sci_sets = lapply(ct$group, grp_sci), kind = kind, spp_drill = spp_drill),
    other_section)
}

#' Caption suffix for the active comparison mode. @keywords internal
.ov_compare_note <- function(compare) {
  switch(compare %||% "none",
         prior     = " · vs prior period",
         last_year = " · vs same period last year",
         "")
}

#' Fewest decimals in `[lo, hi]` that don't render any of `vals` (non-zero) as 0 — so a tiny
#' RAI/rate isn't shown as 0.00, while ordinary cells stay at the lean lower precision. A true
#' zero is fine at `lo`. @keywords internal
.ov_dp <- function(vals, lo, hi) {
  v <- vals[!is.na(vals)]
  d <- lo
  while (d < hi && any(v != 0 & round(v, d) == 0)) d <- d + 1L
  d
}

#' A metric table: target-group rows × reserve columns, cells "value ± SE". When `prev`
#' (a comparison summary) is given, each cell also shows the comparison value + a coloured
#' ▲/▼ — **green where the change is desirable, red where it isn't**: for a pest/predator
#' down is good, for a protected species (`desirable[[taxon]] == "up"`) up is good. When
#' `drill_id` is set, data cells are clickable (set that Shiny input → drill-down modal).
#' Precision is per-cell adaptive in `[min_digits, digits]` (driven by the metric/prior, not SE):
#' most cells stay at `min_digits`, only a near-zero value bumps up so it isn't shown as 0.
#' @keywords internal
.ov_metric_table <- function(summary, taxa_order, caption, prev = NULL, desirable = NULL,
                             drill_id = NULL, kind = "", colour = TRUE, digits = 1,
                             min_digits = digits) {
  reserves <- unique(summary$reserve)
  reserves <- c(sort(setdiff(reserves, "Combined")),                  # combined (network) last
                if ("Combined" %in% reserves) "Combined")

  body <- function(tx, rs) {
    row <- summary[summary$taxon == tx & summary$reserve == rs, , drop = FALSE]
    if (!nrow(row) || is.na(row$metric)) return(list(content = "–", drill = FALSE))
    pr  <- if (!is.null(prev)) prev[prev$taxon == tx & prev$reserve == rs, , drop = FALSE]
    dp  <- .ov_dp(c(row$metric, if (length(pr) && nrow(pr)) pr$metric), min_digits, digits)
    fmt <- paste0("%.", dp, "f")
    # keep "0.43 ± 0.15" intact (no wrap mid-value)
    main <- tags$span(class = "ik-metric-main",
      if (is.na(row$se)) sprintf(fmt, row$metric) else sprintf(paste0(fmt, " ± ", fmt), row$metric, row$se))
    if (is.null(prev)) return(list(content = main, drill = TRUE))
    if (!nrow(pr) || is.na(pr$metric))
      return(list(content = tagList(main, " ",
        tags$span(class = "ik-metric-cmp ik-metric-prev", "· –")), drill = TRUE))
    dir   <- .ov_dir(row$metric, pr$metric, dp)         # compare at the cell's precision
    want  <- desirable[[tx]] %||% "down"                # "up" for protected, "down" for pests
    cls   <- if (!colour || dir == "flat") "neutral" else if (dir == want) "good" else "bad"
    arrow <- c(up = "▲", down = "▼", flat = "–")[[dir]]
    # arrow + prior stay together ("▲ 0.35" never splits); the cell can wrap only BETWEEN
    # "value ± se" and the comparison group, so cells wrap uniformly.
    content <- tagList(main, " ",
      tags$span(class = "ik-metric-cmp",
        tags$span(class = paste0("ik-arrow ik-arrow-", cls), arrow), " ",
        tags$span(class = "ik-metric-prev", sprintf(fmt, pr$metric))))
    list(content = content, drill = TRUE)
  }

  cell_td <- function(tx, rs) {
    b <- body(tx, rs)
    if (b$drill && !is.null(drill_id)) {
      tags$td(class = "ik-drill", b$content, title = "Show the breakdown",
              onclick = sprintf(
                "Shiny.setInputValue('%s',{taxon:'%s',reserve:'%s',kind:'%s'},{priority:'event'})",
                drill_id, tx, rs, kind))
    } else tags$td(b$content)
  }

  tags$div(
    class = "ik-metric",
    tags$div(class = "ik-metric-cap", caption),
    tags$table(
      class = "ik-metric-table",
      tags$thead(tags$tr(tags$th(""), lapply(reserves, function(r) tags$th(r)))),
      tags$tbody(lapply(taxa_order, function(tx) {
        tags$tr(tags$td(class = "ik-metric-label", tx),
                lapply(reserves, function(rs) cell_td(tx, rs)))
      }))
    )
  )
}

#' One device card: metric value-boxes + an outcome-metric table + a species panel. Each value
#' box is clickable when `box_drill` (a Shiny input id) is given — clicking sets it to
#' `{kind, metric}` (metric = effort/deployments/records/species), handled into a drill modal.
#' @keywords internal
.ov_device_section <- function(title, res, effort_label, effort_value,
                               deploy_label, record_label, sg, panel_fn, metric = NULL,
                               kind = "", box_drill = NULL, spp_drill = NULL) {
  obs <- res$observations
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  spp <- length(unique(stats::na.omit(obs$scientificName)))
  box <- function(metric_key, ...) {                       # value box, clickable when wired
    vb <- value_box(...)
    if (is.null(box_drill)) return(vb)
    div(class = "ov-box-click", title = "Show the underlying records",
        onclick = sprintf("Shiny.setInputValue('%s',{kind:'%s',metric:'%s'},{priority:'event'})",
                          box_drill, kind, metric_key),
        vb)
  }
  card(
    card_header(title),
    card_body(
      layout_column_wrap(
        width = 1/4,
        box("effort",      effort_label, effort_value,                   showcase = icon("clock")),
        box("deployments", deploy_label, .ov_num(nrow(res$deployments)), showcase = icon("camera")),
        box("records",     record_label, .ov_num(nrow(obs)),             showcase = icon("paw")),
        box("species",     "Species",    .ov_num(spp),                   showcase = icon("dna"))
      ),
      metric,
      panel_fn(obs, sg, kind, spp_drill)
    )
  )
}

#' Overview server.
#'
#' @param id                Module id.
#' @param ik_data           The ik_data container.
#' @param prefer_scientific A reactive returning TRUE to show scientific names (used for
#'   non-registry caught species in the trap panel).
#' @param selection         A reactive returning the selection SPEC (period/geography).
overview_server <- function(id, ik_data, prefer_scientific, selection) {
  moduleServer(id, function(input, output, session) {
    sg       <- ik_species_groups(ik_data)
    locs     <- ik_data$app$geography$locations
    projects <- unique(unlist(lapply(ik_data$datasets, function(d) d$meta$project)))
    mon_targets <- ik_taxa_groups(sg, "monitor", "target")   # camera RAI taxa
    ctl_targets <- ik_taxa_groups(sg, "control", "target")   # trap capture-rate taxa
    # desirable change direction: "up" for protected species, "down" for pests/predators.
    desire  <- function(taxa) stats::setNames(
      ifelse(sg$role[match(names(taxa), sg$label)] == "protected", "up", "down"), names(taxa))
    mon_dir <- desire(mon_targets)
    ctl_dir <- desire(ctl_targets)

    all_dev <- reactive(ik_resolve(ik_data, selection()))
    cam     <- reactive(ik_resolve(ik_data, selection(), source_type = "camera"))
    trp     <- reactive(ik_resolve(ik_data, selection(), source_type = "trap"))
    # Append the combined row (mean ± SE over reserves) when >1 reserve — the right-most
    # "Combined" column. Only meaningful with several reserves to average.
    with_network <- function(summary) {
      if (length(unique(summary$reserve)) > 1) dplyr::bind_rows(summary, ik_metric_combine(summary))
      else summary
    }
    # Compute a metric for the selection + (when a comparison is set) the comparison period,
    # keeping the comparison's per-line table too so the drill modal can show prior values.
    metric_react <- function(fn, taxa) reactive({
      m  <- fn(ik_data, selection(), taxa)
      m$summary <- with_network(m$summary)
      cs <- ik_comparison_spec(ik_data, selection())
      if (!is.null(cs)) {
        p <- fn(ik_data, cs, taxa)
        m$prev       <- with_network(p$summary)
        m$prev_lines <- p$lines
      } else { m$prev <- NULL; m$prev_lines <- NULL }
      m
    })
    rai_r  <- metric_react(ik_rai, mon_targets)
    rate_r <- metric_react(ik_trap_rate, ctl_targets)

    fmt_date <- function(x) if (!is.finite(as.numeric(x))) "—" else format(x, "%d %b %Y")

    output$header <- renderUI({
      r   <- all_dev()
      obs <- r$observations
      gl  <- locs[locs$location_id %in% unique(r$deployments$locationID), , drop = FALSE]
      # the instance ORGANISATION (project.R) leads; the data sources (dataset names) sit
      # smaller on a second line. Falls back to the dataset/project tags if no org is set.
      org    <- ik_data$meta$organisation
      dnames <- unique(vapply(ik_data$datasets, function(d) d$meta$name %||% "", character(1)))
      dnames <- dnames[nzchar(dnames)]
      tags$div(
        class = "ik-ov-header",
        tags$h3(org %||% paste(if (length(dnames)) dnames else projects, collapse = " · ")),
        if (!is.null(org) && length(dnames))
          tags$div(class = "ik-ov-datasets", paste(dnames, collapse = " · ")),
        tags$div(
          class = "ik-ov-sub",
          sprintf("%s – %s", fmt_date(suppressWarnings(min(obs$eventEnd, na.rm = TRUE))),
                  fmt_date(suppressWarnings(max(obs$eventEnd, na.rm = TRUE)))),
          tags$span(class = "ik-ov-dot", "·"),
          sprintf("%d reserves · %d lines · %d locations",
                  length(unique(gl$reserve[!is.na(gl$reserve)])),
                  length(unique(gl$line[!is.na(gl$line)])),
                  length(unique(gl$location_id)))
        )
      )
    })

    output$camera <- renderUI({
      rc  <- ik_data$meta$camera$rai
      m   <- rai_r()
      cap <- sprintf("RAI · per %s camera-hrs%s%s", format(rc$norm_hours, big.mark = ","),
                     if (isTRUE(rc$use_net)) " · net" else "",
                     if (!is.null(m$prev)) .ov_compare_note(selection()$compare) else "")
      .ov_device_section("Camera monitoring", cam(),
                         "Camera-hrs", .ov_num(round(cam()$effort_hours)),
                         "Deployments", "Detections", sg, .ov_panel_monitor,
                         metric = .ov_metric_table(m$summary, names(mon_targets), cap, prev = m$prev,
                           desirable = mon_dir, drill_id = session$ns("drill"), kind = "camera"),
                         kind = "camera", box_drill = session$ns("box_drill"),
                         spp_drill = session$ns("spp_drill"))
    })

    output$trapping <- renderUI({
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      m      <- rate_r()
      cap    <- sprintf("Captures · per %s trap-nights%s", ik_data$meta$trapping$rate$norm_trap_days,
                        if (!is.null(m$prev)) .ov_compare_note(selection()$compare) else "")
      .ov_device_section("Trapping (control)", trp(),
                         "Trap-nights", .ov_num(round(trp()$effort_hours / 24)),
                         "Checks", "Catches", sg,
                         function(o, s, k, sd) .ov_panel_control(o, s, ik_data, prefer, k, sd),
                         metric = .ov_metric_table(m$summary, names(ctl_targets), cap, prev = m$prev,
                           desirable = ctl_dir, drill_id = session$ns("drill"), kind = "trap",
                           colour = FALSE, digits = 3, min_digits = 2),   # 2 dp, 3 only for tiny rates
                         kind = "trap", box_drill = session$ns("box_drill"),
                         spp_drill = session$ns("spp_drill"))
    })

    # Drill-down: a clicked metric cell opens its breakdown (the auditable basis) — a
    # reserve cell drills to its per-LINE values; the "Combined" cell drills to the
    # per-RESERVE values the combined mean averages. When a comparison is active, each row
    # also shows the prior-period value + a desirability-coloured arrow (same key as the
    # table), so the comparison number is verifiable too.
    # The arrow describes how the CURRENT value moved, so it binds to the current cell (next
    # to the RAI/rate value), with the prior shown plainly in its own column as the reference.
    arrow_span <- function(cur, prv, want, digits = 2, colour = TRUE) {  # arrow, NULL if no prior
      if (length(prv) == 0 || is.na(prv)) return(NULL)
      dir <- .ov_dir(cur, prv, digits)                   # at the cell's display precision
      cls <- if (!colour || dir == "flat") "neutral" else if (dir == want) "good" else "bad"
      tagList(" ", tags$span(class = paste0("ik-arrow ik-arrow-", cls),
                             c(up = "▲", down = "▼", flat = "–")[[dir]]))
    }
    prior_td <- function(prv, digits) tags$td(class = "ik-metric-prev",
      if (length(prv) == 0 || is.na(prv)) "–" else sprintf(paste0("%.", digits, "f"), prv))
    # State for the ONE tabbed drill modal (Breakdown → Records → Record), so a deeper click
    # switches tabs in place instead of stacking modals. Mirrors the Bait effectiveness drill.
    drill_spec <- reactiveVal(NULL)   # the clicked metric cell: {taxon, reserve, kind}
    records    <- reactiveVal(NULL)   # the records behind a clicked Breakdown count
    rec_ctx    <- reactiveVal(NULL)   # caption for the Records tab (which line)
    rec_obs    <- reactiveVal(NULL)   # observationID open in the Record tab (also the highlight)

    observeEvent(input$drill, {
      drill_spec(input$drill)
      records(NULL); rec_ctx(NULL); rec_obs(NULL)         # fresh drill
      d <- input$drill
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s · %s", d$taxon, d$reserve),
          "the auditable basis — open a count for its records, then a record for the full detail"),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("drill_tabs"),
          tabPanel("Breakdown",      icon = icon("table-list"),  uiOutput(session$ns("drill_breakdown"))),
          tabPanel("Records",        icon = icon("list"),        uiOutput(session$ns("drill_records_ui"))),
          tabPanel("Record Details", icon = icon("circle-info"), uiOutput(session$ns("drill_record"))))))
    })

    # Breakdown tab: the per-line (reserve cell) or per-reserve (Combined cell) basis the metric
    # averages, with comparison arrows. A non-zero count cell is clickable → sets input$drill_obs
    # (handled below → Records tab).
    output$drill_breakdown <- renderUI({
      d <- drill_spec(); req(d)
      is_cam <- identical(d$kind, "camera")
      res    <- if (is_cam) rai_r() else rate_r()
      sm     <- res$summary[res$summary$taxon == d$taxon & res$summary$reserve == d$reserve, ]
      name   <- if (is_cam) "RAI" else "rate"
      want   <- (if (is_cam) mon_dir else ctl_dir)[[d$taxon]] %||% "down"
      cmp    <- !is.null(res$prev)                       # comparison active?
      clab   <- switch(selection()$compare %||% "none", prior = "Prior", last_year = "Last yr", "Prev")
      colour <- is_cam                                   # trap arrows neutral (per project decision)
      dg     <- if (is_cam) 1L else 3L                   # reserve/summary precision (rate 3 dp)
      dg_ln  <- if (is_cam) 2L else 3L                   # per-line precision
      fd     <- paste0("%.", dg, "f")                    # reserve/summary fmt
      fl     <- paste0("%.", dg_ln, "f")                 # per-line fmt

      if (identical(d$reserve, "Combined")) {            # combined cell → per-reserve breakdown
        R  <- res$summary[res$summary$taxon == d$taxon & res$summary$reserve != "Combined", , drop = FALSE]
        R  <- R[order(R$reserve), , drop = FALSE]
        pv <- if (cmp) res$prev[res$prev$taxon == d$taxon & res$prev$reserve != "Combined", , drop = FALSE]
        # records per reserve (sum over its lines) → a reserve row is clickable when it has any,
        # drilling to that reserve's records across ALL its lines (line omitted).
        cnt  <- if (is_cam) "individuals" else "captures"
        Ld   <- res$lines[res$lines$taxon == d$taxon, , drop = FALSE]
        nrec <- tapply(Ld[[cnt]], Ld$reserve, sum)
        tbl <- tags$table(class = "ik-drill-table",
          tags$thead(tags$tr(tags$th("Reserve"), tags$th(tools::toTitleCase(name)),
                             if (cmp) tags$th(clab), tags$th("Lines"))),
          tags$tbody(lapply(seq_len(nrow(R)), function(i) {
            prv   <- if (cmp) pv$metric[pv$reserve == R$reserve[i]] else NULL
            drill <- isTRUE(nrec[R$reserve[i]] > 0)
            tags$tr(
              class   = if (drill) "ik-drill-row" else NULL,
              title   = if (drill) "Show this reserve's records" else NULL,
              onclick = if (drill) sprintf(
                "Shiny.setInputValue('%s',{taxon:'%s',reserve:'%s',kind:'%s'},{priority:'event'})",
                session$ns("drill_obs"), d$taxon, R$reserve[i], d$kind) else NULL,
              tags$td(R$reserve[i]),
              tags$td(sprintf(fd, R$metric[i]),
                      if (!is.na(R$se[i])) sprintf(paste0(" ± ", fd), R$se[i]),
                      if (cmp) arrow_span(R$metric[i], prv, want, dg, colour)),
              if (cmp) prior_td(prv, dg),
              tags$td(.ov_num(R$n_lines[i])))
          })))
        return(tagList(
          tags$p(class = "ik-drill-summary", sprintf(
            paste0("Combined = ", fd, "%s over %d reserves — mean of the per-reserve %s below."),
            sm$metric, if (!is.na(sm$se)) sprintf(paste0(" ± ", fd, " SE"), sm$se) else "", sm$n_lines, name)),
          tbl))
      }

      L   <- res$lines[res$lines$taxon == d$taxon & res$lines$reserve == d$reserve, , drop = FALSE]
      L   <- L[order(suppressWarnings(as.numeric(L$line)), L$line), , drop = FALSE]
      pl  <- if (cmp) res$prev_lines[res$prev_lines$taxon == d$taxon &
                                     res$prev_lines$reserve == d$reserve, , drop = FALSE]
      cnt <- if (identical(d$kind, "camera")) "individuals" else "captures"
      eff <- if (identical(d$kind, "camera")) "camera_hours" else "trap_days"
      hdr <- if (identical(d$kind, "camera")) c("Individuals", "Camera-hrs", "RAI")
             else c("Captures", "Trap-days", "Rate")
      tbl <- tags$table(class = "ik-drill-table",
        tags$thead(tags$tr(tags$th("Line"), tags$th(hdr[1]), tags$th(hdr[2]), tags$th(hdr[3]),
                           if (cmp) tags$th(clab))),
        tags$tbody(lapply(seq_len(nrow(L)), function(i) {
          prv  <- if (cmp) pl$metric[pl$line == L$line[i]] else NULL
          drill <- L[[cnt]][i] > 0                          # one action per row → whole row clickable
          tags$tr(
            class   = if (drill) "ik-drill-row" else NULL,
            title   = if (drill) "Show these records" else NULL,
            onclick = if (drill) sprintf(
              "Shiny.setInputValue('%s',{taxon:'%s',reserve:'%s',line:'%s',kind:'%s'},{priority:'event'})",
              session$ns("drill_obs"), d$taxon, d$reserve, L$line[i], d$kind) else NULL,
            tags$td(L$line[i]),
            tags$td(.ov_num(L[[cnt]][i])),
            tags$td(.ov_num(round(L[[eff]][i]))),
            tags$td(sprintf(fl, L$metric[i]), if (cmp) arrow_span(L$metric[i], prv, want, dg_ln, colour)),
            if (cmp) prior_td(prv, dg_ln))
        })))
      tagList(
        tags$p(class = "ik-drill-summary", sprintf(
          paste0("Reserve = ", fd, "%s over %d lines — mean of the per-line %s below."),
          sm$metric, if (!is.na(sm$se)) sprintf(paste0(" ± ", fd, " SE"), sm$se) else "", sm$n_lines, name)),
        tbl)
    })

    # A clicked Breakdown count → list its records in the Records tab (no new modal).
    observeEvent(input$drill_obs, {
      d    <- input$drill_obs
      is_cam <- identical(d$kind, "camera")
      taxa <- if (is_cam) mon_targets else ctl_targets
      records(ik_metric_obs(ik_data, selection(), taxa, d$taxon,
                            reserve = d$reserve, line = d$line,
                            source_type = if (is_cam) "camera" else "trap"))
      rec_obs(NULL)
      noun  <- if (is_cam) "detections" else "captures"
      where <- if (!is.null(d$line)) sprintf("%s · Line %s", d$reserve, d$line) else d$reserve
      rec_ctx(sprintf("The %s behind %s · %s. ", noun, d$taxon, where))
      updateTabsetPanel(session, "drill_tabs", selected = "Records")
    })

    output$drill_records_ui <- renderUI({
      if (is.null(records()))
        return(tags$p(class = "ik-drill-summary",
          "Open a non-zero count in the Breakdown tab to list the records behind it here."))
      tagList(
        tags$p(class = "ik-drill-summary", rec_ctx(),
               tags$b("Click a row"), " for that record's full detail — then come back for another."),
        DT::dataTableOutput(session$ns("drill_records")))
    })

    output$drill_records <- DT::renderDT({
      o <- records()
      validate(need(!is.null(o) && nrow(o), "No records."))
      prefer  <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      has_t   <- format(o$when, "%H:%M:%S") != "00:00:00"
      when_lab <- ifelse(has_t, format(o$when, "%d %b %Y · %H:%M"), format(o$when, "%d %b %Y"))
      df <- data.frame(When = when_lab,
                       Species = ik_species_label(o$scientificName, ik_data, prefer),
                       Count = o$count, Location = o$locationName, ObsID = o$observationID,
                       check.names = FALSE, stringsAsFactors = FALSE)
      dt <- DT::datatable(df, rownames = FALSE, selection = "single",   # whole row → the record
                    class = "stripe hover row-border ik-row-click",
                    options = list(pageLength = 12, scrollX = TRUE, dom = "tip",
                      columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))  # hide ObsID
      .ik_dt_highlight_row(dt, "ObsID", rec_obs())                  # the record you're viewing
    })

    observeEvent(input$drill_records_rows_selected, {
      i <- input$drill_records_rows_selected; o <- records()
      if (length(i) && !is.null(o) && i <= nrow(o)) {
        rec_obs(o$observationID[i]); updateTabsetPanel(session, "drill_tabs", selected = "Record Details")
      }
      DT::selectRows(DT::dataTableProxy("drill_records"), NULL)      # clear → same row re-clickable
    })

    # Record Details tab: the full observation inline, with the SAME Photos/Details/Provenance
    # sub-tabs as the standalone record viewer (reused via .ovw_tabs). Mirrors the Bait drill.
    output$drill_record <- renderUI({
      if (is.null(rec_obs()))
        return(tags$p(class = "ik-drill-summary", "Click a record in the Records tab to see it in full here."))
      ob <- ik_observation(ik_data, rec_obs())
      if (is.null(ob)) return(tags$p("Record not found."))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      tagList(.ovw_title(ik_data, ob, prefer),
              .ovw_tabs(ik_data, ob, prefer, tabset_id = session$ns("rec_subtabs")))
    })

    # --- value-box drills: each box opens the rows behind its number for the current selection.
    # records (Detections/Catches) → the observations (whole-row → the record viewer); species →
    # species × count; deployments/effort → the deployment list with per-deployment effort.
    box_records <- reactiveVal(NULL)   # prepared records df (When·Species·Count·Location·ObsID)
    box_deps    <- reactiveVal(NULL)   # prepared deployments df
    box_rec_obs <- reactiveVal(NULL)   # observationID open in the box drill's Record Details tab
    fmt_dt <- function(x) ifelse(is.na(x), "—",
      ifelse(format(x, "%H:%M:%S") == "00:00:00", format(x, "%d %b %Y"), format(x, "%d %b %Y · %H:%M")))

    # Shared records modal (value-box "records" + the species-panel drill): a 2-tab modal —
    # Records (a DT) → Record Details (the inline viewer) — so you can step into a record AND tab
    # back to the list, same as the metric-cell drill (no replace-modal dead end).
    show_records_modal <- function(obs, is_cam, title, subtitle) {
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      when   <- if (is_cam) obs$eventStart else obs$eventEnd
      box_records(data.frame(
        When = fmt_dt(when), Species = ik_species_label(obs$scientificName, ik_data, prefer),
        Count = obs$count, Location = obs$locationName, ObsID = obs$observationID,
        check.names = FALSE, stringsAsFactors = FALSE))
      box_rec_obs(NULL)
      showModal(modalDialog(
        title = .ik_modal_title(title, subtitle),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("box_tabs"),
          tabPanel("Records", icon = icon("list"),
                   DT::dataTableOutput(session$ns("box_records_table"))),
          tabPanel("Record Details", icon = icon("circle-info"),
                   uiOutput(session$ns("box_record"))))))
    }

    observeEvent(input$box_drill, {
      bd     <- input$box_drill
      is_cam <- identical(bd$kind, "camera")
      r      <- if (is_cam) cam() else trp()
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      dev    <- if (is_cam) "Camera monitoring" else "Trapping (control)"
      obs    <- r$observations
      obs    <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]

      if (identical(bd$metric, "records")) {
        noun <- if (is_cam) "Detections" else "Catches"
        show_records_modal(obs, is_cam, sprintf("%s · %s", dev, noun),
          "every record in the current selection — click a row for the full record")

      } else if (identical(bd$metric, "species")) {
        noun <- if (is_cam) "Detections" else "Catches"
        lab  <- ik_species_label(obs$scientificName, ik_data, prefer); lab[is.na(lab)] <- "Unidentified"
        tb   <- sort(table(lab), decreasing = TRUE)
        tbl  <- tags$table(class = "ik-drill-table",
          tags$thead(tags$tr(tags$th("Species"), tags$th(noun))),
          tags$tbody(lapply(seq_along(tb), function(i)
            tags$tr(tags$td(names(tb)[i]), tags$td(.ov_num(as.integer(tb[i])))))))
        showModal(modalDialog(
          title = .ik_modal_title(sprintf("%s · Species", dev),
            sprintf("%d species · %s %s", length(tb), .ov_num(nrow(obs)), tolower(noun))),
          if (length(tb)) tbl else tags$p("No animals recorded for this selection."),
          size = "m", easyClose = TRUE, footer = modalButton("Close")))

      } else {                                            # deployments / effort
        lbl <- if (is_cam) "Deployments" else "Checks"
        dep <- r$deployments
        eff <- as.numeric(difftime(dep$deploymentEnd, dep$deploymentStart, units = "hours"))
        df  <- data.frame(
          Location = dep$locationName, Reserve = dep$reserve, Line = dep$line,
          Start = fmt_dt(dep$deploymentStart), End = fmt_dt(dep$deploymentEnd),
          check.names = FALSE, stringsAsFactors = FALSE)
        df[[if (is_cam) "Camera-hrs" else "Trap-nights"]] <-
          if (is_cam) round(eff) else round(eff / 24, 1)
        box_deps(df)
        showModal(modalDialog(
          title = .ik_modal_title(sprintf("%s · %s", dev, lbl),
            sprintf("%s %s in the current selection", .ov_num(nrow(dep)), tolower(lbl))),
          size = "l", easyClose = TRUE, footer = modalButton("Close"),
          DT::dataTableOutput(session$ns("box_deps_table"))))
      }
    })

    output$box_records_table <- DT::renderDT({
      df <- box_records()
      validate(need(!is.null(df) && nrow(df), "No records."))
      dt <- DT::datatable(df, rownames = FALSE, selection = "single",  # whole row → Record Details
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))  # hide ObsID
      .ik_dt_highlight_row(dt, "ObsID", box_rec_obs())             # the record you're viewing
    })

    observeEvent(input$box_records_table_rows_selected, {
      i <- input$box_records_table_rows_selected; df <- box_records()
      if (length(i) && !is.null(df) && i <= nrow(df)) {
        box_rec_obs(df$ObsID[i]); updateTabsetPanel(session, "box_tabs", selected = "Record Details")
      }
      DT::selectRows(DT::dataTableProxy("box_records_table"), NULL)
    })

    output$box_record <- renderUI({
      if (is.null(box_rec_obs()))
        return(tags$p(class = "ik-drill-summary", "Click a record in the Records tab to see it in full here."))
      ob <- ik_observation(ik_data, box_rec_obs())
      if (is.null(ob)) return(tags$p("Record not found."))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      tagList(.ovw_title(ik_data, ob, prefer),
              .ovw_tabs(ik_data, ob, prefer, tabset_id = session$ns("box_rec_subtabs")))
    })

    output$box_deps_table <- DT::renderDT({
      df <- box_deps()
      validate(need(!is.null(df) && nrow(df), "No deployments."))
      DT::datatable(df, rownames = FALSE, selection = "none",
        class = "stripe hover row-border",
        options = list(pageLength = 12, scrollX = TRUE, dom = "tip"))
    })

    # Species-panel drill: click a species/group item below a device card → its records (the same
    # records modal as the Detections/Catches box, filtered to that item's scientificName set).
    observeEvent(input$spp_drill, {
      sd     <- input$spp_drill
      is_cam <- identical(sd$kind, "camera")
      r      <- if (is_cam) cam() else trp()
      dev    <- if (is_cam) "Camera monitoring" else "Trapping (control)"
      obs    <- r$observations
      obs    <- obs[!is.na(obs$observationType) & obs$observationType == "animal" &
                      obs$scientificName %in% sd$sci, , drop = FALSE]
      show_records_modal(obs, is_cam, sprintf("%s · %s", dev, sd$label),
        sprintf("%s %s in the current selection — click a row for the full record",
                .ov_num(nrow(obs)), if (is_cam) "detections" else "catches"))
    })
  })
}
