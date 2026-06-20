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

#' One titled block of label/count items (faint when zero). @keywords internal
.ov_class_section <- function(title, labels, counts) {
  if (!length(labels)) return(NULL)
  tags$div(
    class = "ik-spp-class",
    tags$h6(title),
    tags$div(class = "ik-spp-grid", lapply(seq_along(labels), function(i) {
      tags$div(
        class = if (counts[i] == 0) "ik-spp-item ik-spp-zero" else "ik-spp-item",
        tags$span(class = "ik-spp-label", labels[i]),
        tags$span(class = "ik-spp-count", .ov_num(counts[i]))
      )
    }))
  )
}

#' Monitoring (camera) species panel: the curated registry groups by `monitor` class
#' (target, then interesting), zeros shown — a fixed scorecard. @keywords internal
.ov_panel_monitor <- function(obs, sg) {
  obs$group <- sg$group[match(obs$scientificName, sg$scientificName)]
  reg <- sg[!duplicated(sg$group) & !is.na(sg$monitor), c("group", "label", "monitor", "priority")]
  reg <- reg[order(reg$priority), ]
  counts <- table(obs$group)
  reg$n  <- as.integer(counts[reg$group]); reg$n[is.na(reg$n)] <- 0L

  classes <- c(intersect(c("target", "interesting"), unique(reg$monitor)),
               setdiff(unique(reg$monitor), c("target", "interesting")))
  sections <- lapply(classes, function(cl) {
    r <- reg[reg$monitor == cl, , drop = FALSE]
    .ov_class_section(tools::toTitleCase(cl), r$label, r$n)
  })
  tagList(sections, tags$div(class = "ik-spp-other",
    sprintf("Other species: %s detections", .ov_num(sum(is.na(obs$group))))))
}

#' Control (trapping) species panel: the control TARGET groups (zeros shown), then ALL
#' OTHER species actually caught (data-driven, by registry label or vernacular, sorted by
#' count) — so bycatch shows up and monitoring-only species don't sit at 0. @keywords internal
.ov_panel_control <- function(obs, sg, ik_data, prefer) {
  obs$group <- sg$group[match(obs$scientificName, sg$scientificName)]
  obs$label <- sg$label[match(obs$scientificName, sg$scientificName)]

  ct <- sg[!duplicated(sg$group) & !is.na(sg$control) & sg$control == "target",
           c("group", "label", "priority")]
  ct <- ct[order(ct$priority), ]
  counts <- table(obs$group)
  ct$n   <- as.integer(counts[ct$group]); ct$n[is.na(ct$n)] <- 0L

  # everything else caught (not a control-target group), by display name, count desc
  other <- obs[!(obs$group %in% ct$group), , drop = FALSE]
  other_section <- NULL
  if (nrow(other)) {
    disp <- other$label
    nl   <- is.na(disp)
    disp[nl] <- ik_species_label(other$scientificName[nl], ik_data, prefer)
    disp[is.na(disp)] <- "Unidentified"
    oc <- sort(table(disp), decreasing = TRUE)
    other_section <- .ov_class_section("All other caught", names(oc), as.integer(oc))
  }
  tagList(.ov_class_section("Target", ct$label, ct$n), other_section)
}

#' Caption suffix for the active comparison mode. @keywords internal
.ov_compare_note <- function(compare) {
  switch(compare %||% "none",
         prior     = " · vs prior period",
         last_year = " · vs same period last year",
         "")
}

#' A metric table: target-group rows × reserve columns, cells "value ± SE". When `prev`
#' (a comparison summary) is given, each cell also shows the comparison value + a coloured
#' ▲/▼ — **green where the change is desirable, red where it isn't**: for a pest/predator
#' down is good, for a protected species (`desirable[[taxon]] == "up"`) up is good. When
#' `drill_id` is set, data cells are clickable (set that Shiny input → drill-down modal).
#' @keywords internal
.ov_metric_table <- function(summary, taxa_order, caption, prev = NULL, desirable = NULL,
                             drill_id = NULL, kind = "", colour = TRUE, digits = 1) {
  fmt <- paste0("%.", digits, "f")
  reserves <- unique(summary$reserve)
  reserves <- c(sort(setdiff(reserves, "Combined")),                  # combined (network) last
                if ("Combined" %in% reserves) "Combined")

  body <- function(tx, rs) {
    row <- summary[summary$taxon == tx & summary$reserve == rs, , drop = FALSE]
    if (!nrow(row) || is.na(row$metric)) return(list(content = "–", drill = FALSE))
    main <- if (is.na(row$se)) sprintf(fmt, row$metric)
            else sprintf(paste0(fmt, " ± ", fmt), row$metric, row$se)
    content <- main
    if (!is.null(prev)) {
      pr <- prev[prev$taxon == tx & prev$reserve == rs, , drop = FALSE]
      if (!nrow(pr) || is.na(pr$metric)) {
        content <- tagList(main, tags$span(class = "ik-metric-prev", " · –"))
      } else {
        dir   <- .ov_dir(row$metric, pr$metric, digits)   # compare at the cell's precision
        want  <- desirable[[tx]] %||% "down"        # "up" for protected, "down" for pests
        cls   <- if (!colour || dir == "flat") "neutral" else if (dir == want) "good" else "bad"
        arrow <- c(up = "▲", down = "▼", flat = "–")[[dir]]
        # arrow sits between current and prior — it describes how the CURRENT value moved,
        # so it binds to the current value (left), with the faint prior as the reference.
        content <- tagList(main, " ",
          tags$span(class = paste0("ik-arrow ik-arrow-", cls), arrow),
          tags$span(class = "ik-metric-prev", sprintf(paste0(" ", fmt), pr$metric)))
      }
    }
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

#' One device card: metric value-boxes + an outcome-metric table + a species panel.
#' @keywords internal
.ov_device_section <- function(title, res, effort_label, effort_value,
                               deploy_label, record_label, sg, panel_fn, metric = NULL) {
  obs <- res$observations
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  spp <- length(unique(stats::na.omit(obs$scientificName)))
  card(
    card_header(title),
    card_body(
      layout_column_wrap(
        width = 1/4,
        value_box(effort_label, effort_value,                   showcase = icon("clock")),
        value_box(deploy_label, .ov_num(nrow(res$deployments)), showcase = icon("camera")),
        value_box(record_label, .ov_num(nrow(obs)),             showcase = icon("paw")),
        value_box("Species",    .ov_num(spp),                   showcase = icon("dna"))
      ),
      metric,
      panel_fn(obs, sg)
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
                           desirable = mon_dir, drill_id = session$ns("drill"), kind = "camera"))
    })

    output$trapping <- renderUI({
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      m      <- rate_r()
      cap    <- sprintf("Captures · per %s trap-nights%s", ik_data$meta$trapping$rate$norm_trap_days,
                        if (!is.null(m$prev)) .ov_compare_note(selection()$compare) else "")
      .ov_device_section("Trapping (control)", trp(),
                         "Trap-nights", .ov_num(round(trp()$effort_hours / 24)),
                         "Checks", "Catches", sg,
                         function(o, s) .ov_panel_control(o, s, ik_data, prefer),
                         metric = .ov_metric_table(m$summary, names(ctl_targets), cap, prev = m$prev,
                           desirable = ctl_dir, drill_id = session$ns("drill"), kind = "trap",
                           colour = FALSE, digits = 3))   # captures: neutral arrows, 3 dp (rare catches)
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
    observeEvent(input$drill, {
      d      <- input$drill
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
        tbl <- tags$table(class = "ik-drill-table",
          tags$thead(tags$tr(tags$th("Reserve"), tags$th(tools::toTitleCase(name)),
                             if (cmp) tags$th(clab), tags$th("Lines"))),
          tags$tbody(lapply(seq_len(nrow(R)), function(i) {
            prv <- if (cmp) pv$metric[pv$reserve == R$reserve[i]] else NULL
            tags$tr(
              tags$td(R$reserve[i]),
              tags$td(sprintf(fd, R$metric[i]),
                      if (!is.na(R$se[i])) sprintf(paste0(" ± ", fd), R$se[i]),
                      if (cmp) arrow_span(R$metric[i], prv, want, dg, colour)),
              if (cmp) prior_td(prv, dg),
              tags$td(.ov_num(R$n_lines[i])))
          })))
        showModal(modalDialog(
          title = sprintf("%s · Combined", d$taxon),
          tags$p(class = "ik-drill-summary", sprintf(
            paste0("Combined = ", fd, "%s over %d reserves — mean of the per-reserve %s below."),
            sm$metric, if (!is.na(sm$se)) sprintf(paste0(" ± ", fd, " SE"), sm$se) else "", sm$n_lines, name)),
          tbl, easyClose = TRUE, size = "m", footer = modalButton("Close")))
        return()
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
          prv <- if (cmp) pl$metric[pl$line == L$line[i]] else NULL
          tags$tr(
            tags$td(L$line[i]),
            tags$td(class = if (L[[cnt]][i] > 0) "ik-drill" else NULL,
                    title = if (L[[cnt]][i] > 0) "Show these records" else NULL,
                    onclick = if (L[[cnt]][i] > 0) sprintf(
                      "Shiny.setInputValue('%s',{taxon:'%s',reserve:'%s',line:'%s',kind:'%s'},{priority:'event'})",
                      session$ns("drill_obs"), d$taxon, d$reserve, L$line[i], d$kind) else NULL,
                    .ov_num(L[[cnt]][i])),
            tags$td(.ov_num(round(L[[eff]][i]))),
            tags$td(sprintf(fl, L$metric[i]), if (cmp) arrow_span(L$metric[i], prv, want, dg_ln, colour)),
            if (cmp) prior_td(prv, dg_ln))
        })))
      showModal(modalDialog(
        title = sprintf("%s · %s", d$taxon, d$reserve),
        tags$p(class = "ik-drill-summary", sprintf(
          paste0("Reserve = ", fd, "%s over %d lines — mean of the per-line %s below."),
          sm$metric, if (!is.na(sm$se)) sprintf(paste0(" ± ", fd, " SE"), sm$se) else "", sm$n_lines, name)),
        tbl, easyClose = TRUE, size = "m", footer = modalButton("Close")))
    })

    # Deeper drill: a clicked Individuals/Captures number opens the actual records behind it
    # (the auditable basis), each a link into the observation viewer. Records use explicit links
    # (no DT row-selection) so they don't race a second modal open.
    drill_obs <- reactiveVal(NULL)
    observeEvent(input$drill_obs, {
      d    <- input$drill_obs
      cam  <- identical(d$kind, "camera")
      taxa <- if (cam) mon_targets else ctl_targets
      drill_obs(ik_metric_obs(ik_data, selection(), taxa, d$taxon,
                              reserve = d$reserve, line = d$line,
                              source_type = if (cam) "camera" else "trap"))
      noun <- if (cam) "detections" else "captures"
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s · %s · Line %s", d$taxon, d$reserve, d$line),
          sprintf("the %s behind this figure — click a row for the full record", noun)),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        DT::dataTableOutput(session$ns("drill_obs_table"))))
    })

    output$drill_obs_table <- DT::renderDT({
      o <- drill_obs()
      validate(need(!is.null(o) && nrow(o), "No records."))
      prefer  <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      has_t   <- format(o$when, "%H:%M:%S") != "00:00:00"
      when_lab <- ifelse(has_t, format(o$when, "%d %b %Y · %H:%M"), format(o$when, "%d %b %Y"))
      df <- data.frame(When = when_lab,
                       Species = ik_species_label(o$scientificName, ik_data, prefer),
                       Count = o$count, Location = o$locationName,
                       check.names = FALSE, stringsAsFactors = FALSE)
      DT::datatable(df, rownames = FALSE, selection = "single",       # whole row → the record
                    class = "stripe hover row-border ik-row-click",
                    options = list(pageLength = 12, scrollX = TRUE, dom = "tip"))
    })

    observeEvent(input$drill_obs_table_rows_selected, {
      i <- input$drill_obs_table_rows_selected; o <- drill_obs()
      if (length(i) && !is.null(o) && i <= nrow(o))
        show_observation_modal(ik_data, o$observationID[i], isTRUE(prefer_scientific()))
      DT::selectRows(DT::dataTableProxy("drill_obs_table"), NULL)
    })
  })
}
