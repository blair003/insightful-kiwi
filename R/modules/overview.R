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
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/overview.css")),
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

#' One COLLAPSIBLE block (HTML <details>, collapsed by default) of label/count items — the "other
#' species" section, kept less prominent than the cards. The summary shows a generic paw + the
#' species count; expanding reveals the items. When `sci_sets` (a list parallel to `labels`, each
#' the item's scientificName vector) and `spp_drill` are given, every item with a known species set
#' is clickable → its records. @keywords internal
.ov_class_section <- function(title, labels, counts, sci_sets = NULL, kind = NULL, spp_drill = NULL) {
  if (!length(labels)) return(NULL)
  drillable <- !is.null(sci_sets) && !is.null(spp_drill)
  grid <- tags$div(class = "ik-spp-grid", lapply(seq_along(labels), function(i) {
    can <- drillable && counts[i] > 0 && length(sci_sets[[i]]) > 0
    cls <- c("ik-spp-item", if (can) "ik-spp-click")
    tags$div(
      class   = paste(cls, collapse = " "),
      title   = if (can) "Show these records",
      onclick = if (can) .ov_spp_onclick(spp_drill, kind, sci_sets[[i]], labels[i]),
      tags$span(class = "ik-spp-label", labels[i]),
      tags$span(class = "ik-spp-count", .ov_num(counts[i]))
    )
  }))
  tags$details(
    class = "ik-spp-class",
    tags$summary(class = "ik-spp-summary",
      ik_species_icon("other", "ik-spp-summary-icon"),
      sprintf("%s · %s species", title, .ov_num(length(labels)))),
    grid)
}

#' The "other species" section beneath a device's cards: every species detected/caught that ISN'T
#' already a card (group not in `card_groups`). `itemise = TRUE` lists each as its own clickable
#' drill-down entry (by registry label or vernacular, count desc); FALSE rolls them into a single
#' "N <noun> across M species" total. @keywords internal
.ov_other_species <- function(obs, sg, ik_data, prefer, card_groups, itemise, noun, title,
                              kind = NULL, spp_drill = NULL) {
  obs$group <- sg$group[match(obs$scientificName, sg$scientificName)]
  obs$label <- sg$label[match(obs$scientificName, sg$scientificName)]
  other <- obs[!(obs$group %in% card_groups), , drop = FALSE]
  if (!nrow(other)) return(NULL)
  if (!isTRUE(itemise)) {                                  # rolled-up total (+ unique-species count)
    n_spp <- length(unique(stats::na.omit(other$scientificName)))
    return(tags$div(class = "ik-spp-other", sprintf(
      "Other species: %s %s across %s species", .ov_num(nrow(other)), noun, .ov_num(n_spp))))
  }
  disp <- other$label; nl <- is.na(disp)                   # itemised: one drill-down entry per species
  disp[nl] <- ik_species_label(other$scientificName[nl], ik_data, prefer)
  disp[is.na(disp)] <- "Unidentified"
  oc      <- sort(table(disp), decreasing = TRUE)
  by_disp <- tapply(other$scientificName, disp, function(x) unique(x[!is.na(x)]))
  .ov_class_section(title, names(oc), as.integer(oc),
    sci_sets = lapply(names(oc), function(d) by_disp[[d]]), kind = kind, spp_drill = spp_drill)
}

#' Monitoring (camera) species panel: target/interesting are CARDS above; here the OTHER detected
#' species (incidental + bycatch). @keywords internal
.ov_panel_monitor <- function(obs, sg, ik_data, prefer, itemise, kind = NULL, spp_drill = NULL) {
  mon_groups <- sg$group[!duplicated(sg$group) & !is.na(sg$monitor) & sg$monitor %in% c("target", "interesting")]
  .ov_other_species(obs, sg, ik_data, prefer, mon_groups, itemise, "detections",
                    "All other detected", kind, spp_drill)
}

#' Control (trapping) species panel: control targets are CARDS above; here ALL OTHER species caught
#' (bycatch + non-target), so they still surface. @keywords internal
.ov_panel_control <- function(obs, sg, ik_data, prefer, itemise, kind = NULL, spp_drill = NULL) {
  ct_groups <- sg$group[!duplicated(sg$group) & !is.na(sg$control) & sg$control == "target"]
  .ov_other_species(obs, sg, ik_data, prefer, ct_groups, itemise, "catches",
                    "All other caught", kind, spp_drill)
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
                "Shiny.setInputValue('%s',{taxon:%s,reserve:%s,kind:'%s'},{priority:'event'})",
                drill_id, .ik_jsq(tx), .ik_jsq(rs), kind))
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

#' Total detections/captures per taxon, from a metric `$lines` table (col `individuals`/`captures`).
#' @keywords internal
.ov_counts <- function(lines) {
  if (is.null(lines) || !nrow(lines)) return(NULL)
  col <- intersect(c("individuals", "captures"), names(lines))[1]
  if (is.na(col)) return(NULL)
  tapply(lines[[col]], lines$taxon, sum, na.rm = TRUE)
}

#' Per-species metric CARDS — one card per target/interesting species, the headline view. Camera:
#' RAI ± SE is the HEADLINE, detection count secondary; trap: catch COUNT headline, capture rate
#' secondary (the device emphasis differs). A desirability-coloured ▲/▼ vs the comparison period
#' sits on the canonical metric (RAI / rate). Click a card → its per-reserve breakdown (the same
#' drill as a matrix cell, reserve = "Combined"). `compact = TRUE` renders a denser, smaller card
#' (used for the secondary "of interest" tier). @keywords internal
.ov_metric_cards <- function(summary, prev, counts, taxa_order, kind, desirable, drill_id,
                             headline = c("rai", "count"), rate_unit = "",
                             digits = 1, min_digits = digits, colour = TRUE, compact = FALSE,
                             sg = NULL, sort_by = "value") {
  headline <- match.arg(headline)
  one <- function(tx) {
    s   <- summary[summary$taxon == tx & summary$reserve == "Combined", , drop = FALSE]
    # No "Combined" row when a SINGLE reserve is in scope (with_network only adds it for >1) — fall
    # back to that reserve's own row, so the card still shows its value and drills into it.
    if (!nrow(s)) s <- summary[summary$taxon == tx, , drop = FALSE]
    rsv <- if (nrow(s)) s$reserve[1] else "Combined"
    cnt <- as.integer(counts[tx] %||% 0L); if (length(cnt) == 0 || is.na(cnt)) cnt <- 0L
    has <- nrow(s) == 1 && !is.na(s$metric); metric_el <- tags$span(class = "ik-card-na", "–"); arrow <- NULL
    if (has) {
      pr <- if (!is.null(prev)) prev[prev$taxon == tx & prev$reserve == rsv, , drop = FALSE] else NULL
      pm <- if (!is.null(pr) && nrow(pr)) pr$metric else NA_real_
      dp <- .ov_dp(c(s$metric, pm), min_digits, digits); fmt <- paste0("%.", dp, "f")
      val <- if (is.na(s$se)) sprintf(fmt, s$metric) else sprintf(paste0(fmt, " ± ", fmt), s$metric, s$se)
      metric_el <- tagList(tags$span(class = "ik-card-metric", val),
                           if (nzchar(rate_unit)) tags$span(class = "ik-card-unit", rate_unit))
      if (!is.na(pm)) {
        dir <- .ov_dir(s$metric, pm, dp); want <- desirable[[tx]] %||% "down"
        cls <- if (!colour || dir == "flat") "neutral" else if (dir == want) "good" else "bad"
        arrow <- tagList(" ", tags$span(class = paste0("ik-arrow ik-arrow-", cls),
                                        c(up = "▲", down = "▼", flat = "–")[[dir]]))
      }
    }
    if (headline == "rai") {
      big <- tagList(metric_el, arrow)
      small <- tags$span(class = "ik-card-cnt", sprintf("%s detections", .ov_num(cnt)))
    } else {
      big <- tagList(tags$span(class = "ik-card-metric", .ov_num(cnt)),
                     tags$span(class = "ik-card-unit", " catches"))
      small <- tagList(metric_el, arrow)
    }
    tags$div(
      class   = paste("ik-metric-card", if (has) "ik-spp-click"),
      title   = if (has) "Show the reserve breakdown",
      onclick = if (has && !is.null(drill_id)) sprintf(
        "Shiny.setInputValue('%s',{taxon:%s,reserve:%s,kind:'%s'},{priority:'event'})", drill_id, .ik_jsq(tx), .ik_jsq(rsv), kind),
      tags$div(class = "ik-card-name",
               if (!is.null(sg)) {
                 gi   <- match(tx, sg$label)
                 # sentiment tint on CAMERA cards only (trapping targets are all pests → all red, no
                 # signal; and it keeps the icon colour from competing with the comparison arrows there).
                 icls <- paste0("ik-card-icon",
                   if (identical(kind, "camera") && !is.na(sg$sentiment[gi])) paste0(" ik-icon-", sg$sentiment[gi]))
                 ik_species_icon(sg$group[gi], icls)
               }, tx),
      tags$div(class = "ik-card-big", big),
      tags$div(class = "ik-card-small", small))
  }
  # Order by the headline value (RAI for camera, catch count for trap), biggest first, so the row
  # reads by volume and wraps tidily; no-data species sort last (cheap — just an order() at render).
  # Project-configurable (overview$sort_cards_by): "value" sorts; anything else keeps config order.
  if (identical(sort_by, "value")) {
    hv <- vapply(taxa_order, function(tx) {
      if (headline == "count") { c0 <- as.integer(counts[tx] %||% 0L); return(if (is.na(c0)) 0 else as.numeric(c0)) }
      s <- summary[summary$taxon == tx & summary$reserve == "Combined", , drop = FALSE]
      if (nrow(s) && !is.na(s$metric)) s$metric else -Inf
    }, numeric(1))
    taxa_order <- taxa_order[order(-hv, seq_along(taxa_order))] # -Inf (no data) last; stable on ties
  }
  tags$div(class = paste("ik-metric-cards", if (isTRUE(compact)) "ik-compact"), lapply(taxa_order, one))
}

#' Drop possible-duplicate observations — the project-defined NET view used across the analytical
#' surface (cards, value boxes, drill-downs). Camera-only in effect: trap obs carry no duplicate
#' flag, so they pass through. NULL `ik_data` is a safe no-op. @keywords internal
.ov_net_obs <- function(obs, ik_data) {
  if (is.null(obs) || !nrow(obs) || is.null(ik_data)) return(obs)
  rel <- ik_relations(ik_data)
  d <- rel$possible_duplicate[match(obs$observationID, rel$observationID)]
  if (is.null(d) || !is.logical(d)) return(obs)               # no duplicate flag (e.g. trap-only data) → no dedup
  obs[is.na(d) | !d, , drop = FALSE]                          # keep non-duplicates
}

#' One device card: metric value-boxes + an outcome-metric table + a species panel. Each value
#' box is clickable when `box_drill` (a Shiny input id) is given — clicking sets it to
#' `{kind, metric}` (metric = effort/deployments/records/species), handled into a drill modal.
#' Counts are NET (possible duplicates excluded) when `ik_data` is supplied. @keywords internal
.ov_device_section <- function(title, res, effort_label, effort_value,
                               deploy_label, record_label, sg, panel_fn, metric = NULL,
                               kind = "", box_drill = NULL, spp_drill = NULL, subs = NULL,
                               ik_data = NULL) {
  obs <- res$observations
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  obs <- .ov_net_obs(obs, ik_data)                            # net by default (trap no-op)
  spp <- length(unique(stats::na.omit(obs$scientificName)))
  box <- function(metric_key, ...) {                       # value box, clickable when wired
    args <- list(...)                                      # optional secondary sub-line under the value
    if (!is.null(subs[[metric_key]])) args <- c(args, list(tags$div(class = "ik-box-sub", subs[[metric_key]])))
    vb <- do.call(value_box, args)
    if (is.null(box_drill)) return(vb)
    div(class = "ov-box-click", title = "Show the underlying records",
        onclick = sprintf("Shiny.setInputValue('%s',{kind:'%s',metric:'%s'},{priority:'event'})",
                          box_drill, kind, metric_key),
        vb)
  }
  card(
    card_header(title, class = paste0("ik-device-head ik-device-", kind)),
    card_body(
      layout_column_wrap(
        width = 1/4,
        box("effort",      effort_label, effort_value,                   showcase = icon("clock")),
        box("deployments", deploy_label, .ov_num(nrow(res$deployments)),
            showcase = icon(if (identical(kind, "trap")) "clipboard-check" else "camera")),
        box("records",     record_label, .ov_num(nrow(obs)),             showcase = icon("paw")),
        box("species",     "Species",    .ov_num(spp),                   showcase = icon("dna"))
      ),
      metric,
      panel_fn(obs, sg, kind, spp_drill)
    )
  )
}

#' Empty-state for a device section (no data loaded / nothing in the selection) — keeps the styled
#' header so the page still reads as two sections. @keywords internal
.ov_empty_section <- function(title, kind, msg) {
  card(
    card_header(title, class = paste0("ik-device-head ik-device-", kind)),
    card_body(tags$p(class = "ik-spp-other", msg)))
}

#' Camera possible-duplicate tally for a resolved selection: raw animal records, net (non-duplicate)
#' count, duplicate count and %. A possible duplicate is a repeat of the same species at the same
#' camera within the project window; excluded from net RAI. Drives the Detections sub-line + RAI
#' caption. @keywords internal
.ov_cam_dup <- function(res, ik_data) {
  obs <- res$observations
  obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
  if (!nrow(obs)) return(list(raw = 0L, net = 0L, dup = 0L, pct = 0))
  rel <- ik_relations(ik_data)
  d   <- rel$possible_duplicate[match(obs$observationID, rel$observationID)]; d[is.na(d)] <- FALSE
  list(raw = nrow(obs), net = sum(!d), dup = sum(d), pct = round(100 * sum(d) / nrow(obs)))
}

#' The four camera value-box sub-lines for a resolved camera selection: Camera-hrs → cameras · lines
#' (lines = the RAI replication unit); Deployments → median deployment length (protocol ≈ 21 nights);
#' Detections → net count + possible duplicates (so the headline raw count reconciles to the net
#' figure the cards/RAI use); Species → most-detected. NULL when no deployments. @keywords internal
.ov_camera_subs <- function(res, ik_data, prefer) {
  deps <- res$deployments
  if (is.null(deps) || !nrow(deps)) return(NULL)
  det  <- res$observations
  det  <- det[!is.na(det$observationType) & det$observationType == "animal", , drop = FALSE]
  n_cam  <- length(unique(deps$locationID))
  # line numbers repeat across reserves (1..4 in each), so count distinct reserve×line pairs — the
  # RAI replication unit (matches the metric's summed n_lines), not the bare line value.
  n_line <- nrow(unique(deps[!is.na(deps$line), c("reserve", "line"), drop = FALSE]))
  med_n  <- stats::median(as.numeric(difftime(deps$deploymentEnd, deps$deploymentStart, units = "hours")) / 24, na.rm = TRUE)
  dd  <- .ov_cam_dup(res, ik_data)                          # the box headline is NET; sub shows raw + dup
  net <- .ov_net_obs(det, ik_data)                          # most-detected from the net records
  top <- if (nrow(net)) {
    lab <- ik_species_label(net$scientificName, ik_data, prefer); lab[is.na(lab)] <- "Unidentified"
    names(sort(table(lab), decreasing = TRUE))[1]
  } else NULL
  list(
    effort      = sprintf("%s camera%s · %s line%s", .ov_num(n_cam), if (n_cam == 1) "" else "s",
                          .ov_num(n_line), if (n_line == 1) "" else "s"),
    deployments = if (is.finite(med_n)) sprintf("median %s nights", .ov_num(round(med_n))) else "—",
    records     = sprintf("%s raw · %s duplicates", .ov_num(dd$raw), .ov_num(dd$dup)),
    species     = if (!is.null(top)) sprintf("most: %s", top) else "—")
}

#' The four trapping value-box sub-lines for a resolved trap selection: Trap-nights → distinct traps;
#' Checks → servicing-health split (good/watch/neglected, canonical thresholds); Catches → traps that
#' caught something; Species → the most-caught species. NULL when the selection has no checks.
#' @keywords internal
.ov_trap_subs <- function(res, ik_data, prefer) {
  deps <- res$deployments
  if (is.null(deps) || !nrow(deps)) return(NULL)
  det  <- res$observations
  det  <- det[!is.na(det$observationType) & det$observationType == "animal", , drop = FALSE]
  n_traps <- length(unique(deps$locationID))
  hh   <- table(factor(ik_trap_health(.trap_mean_intervals(deps), ik_data),
                       levels = c("good", "watch", "neglected")))
  succ <- unique(deps$locationID[match(det$deploymentID, deps$deploymentID)])
  n_succ <- length(succ[!is.na(succ)])
  top  <- if (nrow(det)) {
    lab <- ik_species_label(det$scientificName, ik_data, prefer); lab[is.na(lab)] <- "Unidentified"
    names(sort(table(lab), decreasing = TRUE))[1]
  } else NULL
  list(
    effort      = sprintf("%s trap%s", .ov_num(n_traps), if (n_traps == 1) "" else "s"),
    deployments = sprintf("%d good · %d watch · %d neglected", hh[["good"]], hh[["watch"]], hh[["neglected"]]),
    records     = sprintf("%s trap%s caught", .ov_num(n_succ), if (n_succ == 1) "" else "s"),
    species     = if (!is.null(top)) sprintf("most: %s", top) else NULL)
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
    has_dev  <- function(st) any(vapply(ik_data$datasets, function(d) identical(d$meta$source_type, st), logical(1)))
    has_camera <- has_dev("camera"); has_trap <- has_dev("trap")
    mon_t <- ik_taxa_groups(sg, "monitor", "target")        # lead cards (protocol targets)
    mon_i <- ik_taxa_groups(sg, "monitor", "interesting")   # secondary "of interest" tier
    mon_targets <- c(mon_t, mon_i)                          # camera metric/drill set = target + interesting
    ctl_targets <- ik_taxa_groups(sg, "control", "target")          # trap cards: control targets
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
    # Cached: the result is numeric + deterministic in (selection axes, comparison, active datasets),
    # so it's keyed on those and shared across sessions in-process — the FIRST visitor to the default
    # landing view warms it and every later visitor gets it free (until restart / re-import). `key`
    # distinguishes the metrics (bindCache shares one cache, so each cached reactive needs a unique key).
    metric_react <- function(fn, taxa, key) reactive({
      m  <- fn(ik_data, selection(), taxa)
      m$summary <- with_network(m$summary)
      cs <- ik_comparison_spec(ik_data, selection())
      if (!is.null(cs)) {
        p <- fn(ik_data, cs, taxa)
        m$prev       <- with_network(p$summary)
        m$prev_lines <- p$lines
      } else { m$prev <- NULL; m$prev_lines <- NULL }
      m
    }) |> bindCache(key, selection()$period, selection()$compare, selection()$season,
                    selection()$reserve, selection()$line, selection()$location, ik_active_datasets())
    rai_r  <- metric_react(ik_rai, mon_targets, "rai")
    rate_r <- metric_react(ik_trap_rate, ctl_targets, "rate")

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
      if (is.null(cam()$deployments) || !nrow(cam()$deployments))
        return(.ov_empty_section("Camera monitoring", "camera",
          if (has_camera) "No camera monitoring data for the current selection." else "No camera monitoring data loaded."))
      rc  <- ik_data$meta$camera$rai
      m   <- rai_r()
      prefer  <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      itemise <- isTRUE(ik_data$meta$overview$list_other_species)
      dd  <- .ov_cam_dup(cam(), ik_data)                     # possible-duplicate share, for the caption
      cap <- sprintf("RAI · per %s camera-hrs%s%s", format(rc$norm_hours, big.mark = ","),
                     if (isTRUE(rc$use_net)) sprintf(" · net (%s%% excluded)", dd$pct) else "",
                     if (!is.null(m$prev)) .ov_compare_note(selection()$compare) else "")
      cnts   <- .ov_counts(m$lines)
      mk     <- function(taxa, compact) .ov_metric_cards(m$summary, m$prev, cnts, names(taxa),
                  "camera", mon_dir, session$ns("drill"), headline = "rai", compact = compact, sg = sg,
                  sort_by = ik_data$meta$overview$sort_cards_by %||% "value")
      cards  <- tagList(
        if (length(mon_t)) mk(mon_t, FALSE),                              # protocol targets lead
        if (length(mon_i)) mk(mon_i, TRUE))                               # of-interest species follow: smaller, set off by the gap
      matrix <- if (isTRUE(ik_data$meta$overview$show_rai_matrix_by_reserve))
        .ov_metric_table(m$summary, names(mon_targets), "By reserve", prev = m$prev,
                         desirable = mon_dir, drill_id = session$ns("drill"), kind = "camera")
      .ov_device_section("Camera monitoring", cam(),
                         "Camera-hrs", .ov_num(round(cam()$effort_hours)),
                         "Deployments", "Detections", sg,
                         function(o, s, k, sd) .ov_panel_monitor(o, s, ik_data, prefer, itemise, k, sd),
                         metric = tagList(tags$div(class = "ik-metric-cap", cap), cards, matrix),
                         kind = "camera", box_drill = session$ns("box_drill"),
                         spp_drill = session$ns("spp_drill"), subs = .ov_camera_subs(cam(), ik_data, prefer),
                         ik_data = ik_data)
    })

    output$trapping <- renderUI({
      if (is.null(trp()$deployments) || !nrow(trp()$deployments))
        return(.ov_empty_section("Trapping (control)", "trap",
          if (has_trap) "No trapping data for the current selection." else "No trapping data loaded."))
      prefer  <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      itemise <- isTRUE(ik_data$meta$overview$list_other_species)
      m       <- rate_r()
      cap    <- sprintf("Captures · per %s trap-nights%s", ik_data$meta$trapping$rate$norm_trap_days,
                        if (!is.null(m$prev)) .ov_compare_note(selection()$compare) else "")
      cards  <- .ov_metric_cards(m$summary, m$prev, .ov_counts(m$lines), names(ctl_targets),
                                 "trap", ctl_dir, session$ns("drill"), headline = "count",
                                 rate_unit = " /100 TN", colour = FALSE, digits = 3, min_digits = 2, sg = sg,
                                 sort_by = ik_data$meta$overview$sort_cards_by %||% "value")
      matrix <- if (isTRUE(ik_data$meta$overview$show_rai_matrix_by_reserve))
        .ov_metric_table(m$summary, names(ctl_targets), "By reserve", prev = m$prev,
                         desirable = ctl_dir, drill_id = session$ns("drill"), kind = "trap",
                         colour = FALSE, digits = 3, min_digits = 2)
      .ov_device_section("Trapping (control)", trp(),
                         "Trap-nights", .ov_num(round(trp()$effort_hours / 24)),
                         "Checks", "Catches", sg,
                         function(o, s, k, sd) .ov_panel_control(o, s, ik_data, prefer, itemise, k, sd),
                         metric = tagList(tags$div(class = "ik-metric-cap", cap), cards, matrix),
                         kind = "trap", box_drill = session$ns("box_drill"),
                         spp_drill = session$ns("spp_drill"), subs = .ov_trap_subs(trp(), ik_data, prefer),
                         ik_data = ik_data)
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
      rlab <- if (identical(d$reserve, "Combined")) "all reserves" else d$reserve   # plainer than "Combined"
      showModal(modalDialog(
        title = .ik_modal_title(sprintf("%s · %s", d$taxon, rlab),
          "the auditable basis — open a count for its records, then a record for the full detail"),
        size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("drill_tabs"),
          tabPanel("Summary",        icon = icon("table-list"),  uiOutput(session$ns("drill_breakdown"))),
          tabPanel("Records",        icon = icon("list"),        uiOutput(session$ns("drill_records_ui"))),
          tabPanel("Record Details", icon = icon("circle-info"), uiOutput(session$ns("drill_record"))))))
      hideTab(session = session, inputId = "drill_tabs", target = "Records")        # revealed on drill
      hideTab(session = session, inputId = "drill_tabs", target = "Record Details")
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
        cnt     <- if (is_cam) "individuals" else "captures"
        cnt_lab <- if (is_cam) "Detections" else "Catches"        # concrete count beside the metric
        Ld   <- res$lines[res$lines$taxon == d$taxon, , drop = FALSE]
        nrec <- tapply(Ld[[cnt]], Ld$reserve, sum)
        tbl <- tags$table(class = "ik-drill-table",
          tags$thead(tags$tr(tags$th("Reserve"), tags$th(cnt_lab), tags$th(tools::toTitleCase(name)),
                             if (cmp) tags$th(clab), tags$th("Lines"))),
          tags$tbody(lapply(seq_len(nrow(R)), function(i) {
            prv   <- if (cmp) pv$metric[pv$reserve == R$reserve[i]] else NULL
            cn    <- nrec[R$reserve[i]]; cn <- if (length(cn) == 0 || is.na(cn)) 0L else as.integer(cn)
            drill <- cn > 0
            tags$tr(
              class   = if (drill) "ik-drill-row" else NULL,
              title   = if (drill) "Show this reserve's records" else NULL,
              onclick = if (drill) sprintf(
                "Shiny.setInputValue('%s',{taxon:%s,reserve:%s,kind:'%s'},{priority:'event'})",
                session$ns("drill_obs"), .ik_jsq(d$taxon), .ik_jsq(R$reserve[i]), d$kind) else NULL,
              tags$td(R$reserve[i]),
              tags$td(.ov_num(cn)),
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
              "Shiny.setInputValue('%s',{taxon:%s,reserve:%s,line:%s,kind:'%s'},{priority:'event'})",
              session$ns("drill_obs"), .ik_jsq(d$taxon), .ik_jsq(d$reserve), .ik_jsq(L$line[i]), d$kind) else NULL,
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
    # One handler for all "← Back" links in this module's drill modals.
    observeEvent(input$tab_back, updateTabsetPanel(session, input$tab_back$tabset, selected = input$tab_back$to))

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
      showTab(session = session, inputId = "drill_tabs", target = "Records", select = TRUE)
    })

    output$drill_records_ui <- renderUI({
      if (is.null(records()))
        return(tags$p(class = "ik-drill-summary",
          "Open a non-zero count in the Summary tab to list the records behind it here."))
      tagList(
        .ik_tab_back(session$ns("tab_back"), "drill_tabs", "Summary", "Back to summary"),
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
                    options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
                      columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))  # hide ObsID
      .ik_dt_highlight_row(dt, "ObsID", rec_obs())                  # the record you're viewing
    })

    observeEvent(input$drill_records_rows_selected, {
      i <- input$drill_records_rows_selected; o <- records()
      if (length(i) && !is.null(o) && i <= nrow(o)) {
        rec_obs(o$observationID[i]); showTab(session = session, inputId = "drill_tabs", target = "Record Details", select = TRUE)
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
      tagList(.ik_tab_back(session$ns("tab_back"), "drill_tabs", "Records", "Back to records"),
              .ovw_title(ik_data, ob, prefer),
              .ovw_tabs(ik_data, ob, prefer, tabset_id = session$ns("rec_subtabs")))
    })

    # --- value-box drills: each box opens the rows behind its number for the current selection.
    # records (Detections/Catches) → the observations (whole-row → the record viewer); species →
    # species × count; deployments/effort → the deployment list with per-deployment effort.
    box_records <- reactiveVal(NULL)   # prepared records df (When·Species·Count·Location·ObsID)
    box_deps    <- reactiveVal(NULL)   # prepared CAMERA deployments df
    box_traps   <- reactiveVal(NULL)   # prepared per-TRAP summary df (trap drill: Traps tab)
    box_checks  <- reactiveVal(NULL)   # the selected trap's individual checks df (Checks tab)
    box_checks_ctx <- reactiveVal(NULL)# caption for the Checks tab (which trap)
    box_rec_obs <- reactiveVal(NULL)   # observationID open in the box drill's Record Details tab
    fmt_dt <- function(x) ifelse(is.na(x), "—",
      ifelse(format(x, "%H:%M:%S") == "00:00:00", format(x, "%d %b %Y"), format(x, "%d %b %Y · %H:%M")))

    # Trap drill is grouped by TRAP (locationID): one row per trap (Traps tab) → its checks (Checks
    # tab) → the check record. Both derive from the resolved trap selection `trp()` so they match the
    # value boxes exactly. Per-trap servicing health reuses the canonical .trap_mean_intervals/
    # ik_trap_health; per-check outcome/bait/volunteer come from the full observation (one per check).
    build_trap_traps <- function() {
      deps <- trp()$deployments
      if (is.null(deps) || !nrow(deps)) return(NULL)
      det  <- trp()$observations
      det  <- det[!is.na(det$observationType) & det$observationType == "animal", , drop = FALSE]
      cap  <- table(deps$locationID[match(det$deploymentID, deps$deploymentID)])
      mi   <- .trap_mean_intervals(deps)
      nights <- as.numeric(difftime(deps$deploymentEnd, deps$deploymentStart, units = "hours")) / 24
      ag <- dplyr::summarise(dplyr::group_by(deps, .data$locationID),
        Location = dplyr::first(.data$locationName), Reserve = dplyr::first(.data$reserve),
        Line = dplyr::first(.data$line), Checks = dplyr::n(),
        .last = suppressWarnings(max(.data$deploymentEnd, na.rm = TRUE)), .groups = "drop")
      tn <- tapply(nights, deps$locationID, sum, na.rm = TRUE)
      ag$Nights  <- round(as.numeric(tn[as.character(ag$locationID)]), 1)
      ag$Catches <- as.integer(cap[as.character(ag$locationID)]); ag$Catches[is.na(ag$Catches)] <- 0L
      hh <- as.character(ik_trap_health(mi[as.character(ag$locationID)], ik_data)); hh[is.na(hh)] <- "—"
      df <- data.frame(Location = ag$Location, Reserve = ag$Reserve, Line = ag$Line,
        Checks = ag$Checks, Nights = ag$Nights, Catches = ag$Catches, Health = tools::toTitleCase(hh),
        Last = fmt_dt(ag$.last), check.names = FALSE, stringsAsFactors = FALSE)
      df$.last_sort <- as.numeric(ag$.last); df$.locid <- ag$locationID
      df
    }
    build_trap_checks <- function(locid) {
      deps <- trp()$deployments
      here <- deps[!is.na(deps$locationID) & deps$locationID == locid, , drop = FALSE]
      if (!nrow(here)) return(NULL)
      here <- here[order(here$deploymentEnd, decreasing = TRUE), , drop = FALSE]
      ao  <- ik_observations(ik_data, with_location = FALSE)
      oi  <- match(here$deploymentID, ao$deploymentID)             # exactly one observation per check
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      lab <- ik_species_label(ao$scientificName[oi], ik_data, prefer)
      status <- vapply(ao$observationTags[oi], .ovw_tag, character(1), key = "status")
      outcome <- .ik_trap_outcome(lab, ao$scientificName[oi], status, ao$observationType[oi])
      bait <- vapply(ao$observationTags[oi], .ovw_tag, character(1), key = "bait")
      vol  <- vapply(ao$observationTags[oi], .ovw_tag, character(1), key = "volunteer")
      bait[is.na(bait) | !nzchar(bait)] <- "—"; vol[is.na(vol) | !nzchar(vol)] <- "—"
      df <- data.frame(
        End = fmt_dt(here$deploymentEnd),
        Nights = round(as.numeric(difftime(here$deploymentEnd, here$deploymentStart, units = "hours")) / 24, 1),
        Outcome = outcome, Bait = bait, Volunteer = vol, check.names = FALSE, stringsAsFactors = FALSE)
      df$.end_sort <- as.numeric(here$deploymentEnd); df$.obsid <- ao$observationID[oi]
      df
    }

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
      hideTab(session = session, inputId = "box_tabs", target = "Record Details")   # revealed on row click
    }

    observeEvent(input$box_drill, {
      bd     <- input$box_drill
      box_records(NULL); box_deps(NULL); box_traps(NULL); box_checks(NULL); box_rec_obs(NULL)  # drop last modal
      is_cam <- identical(bd$kind, "camera")
      r      <- if (is_cam) cam() else trp()
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      dev    <- if (is_cam) "Camera monitoring" else "Trapping (control)"
      obs    <- r$observations
      obs    <- obs[!is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
      obs    <- .ov_net_obs(obs, ik_data)              # drills show the NET (de-duplicated) records

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

      } else if (is_cam) {                                # CAMERA deployments → one row per deployment
        dep <- r$deployments
        eff <- as.numeric(difftime(dep$deploymentEnd, dep$deploymentStart, units = "hours"))
        df  <- data.frame(
          Location = dep$locationName, Reserve = dep$reserve, Line = dep$line,
          Start = fmt_dt(dep$deploymentStart), End = fmt_dt(dep$deploymentEnd),
          check.names = FALSE, stringsAsFactors = FALSE)
        df$Effort      <- round(eff)                       # camera-hours (unit in the subtitle)
        df$.start_sort <- as.numeric(dep$deploymentStart)  # hidden: chronological sort + the row drill key
        df$.end_sort   <- as.numeric(dep$deploymentEnd)
        df$.depid      <- dep$deploymentID
        box_deps(df)
        showModal(modalDialog(
          title = .ik_modal_title("Camera monitoring · Deployments",
            sprintf("%s deployments · effort in camera-hours — click one for its detections", .ov_num(nrow(dep)))),
          size = "l", easyClose = TRUE, footer = modalButton("Close"),
          DT::dataTableOutput(session$ns("box_deps_table"))))

      } else {                                            # TRAP checks → grouped by trap: Traps → Checks → Record
        df <- build_trap_traps()
        box_traps(df); box_checks(NULL); box_checks_ctx(NULL)
        showModal(modalDialog(
          title = .ik_modal_title("Trapping (control) · Traps",
            sprintf("%s traps · %s checks — open a trap for its checks, then a check for the record",
                    .ov_num(if (is.null(df)) 0L else nrow(df)), .ov_num(nrow(r$deployments)))),
          size = "l", easyClose = TRUE, footer = modalButton("Close"),
          tabsetPanel(id = session$ns("trap_tabs"),
            tabPanel("Traps",  icon = icon("location-dot"), DT::dataTableOutput(session$ns("box_traps_table"))),
            tabPanel("Checks", icon = icon("list"),         uiOutput(session$ns("box_checks_ui"))),
            tabPanel("Record", icon = icon("circle-info"),  uiOutput(session$ns("box_record"))))))
        hideTab(session = session, inputId = "trap_tabs", target = "Checks")        # revealed on drill
        hideTab(session = session, inputId = "trap_tabs", target = "Record")
      }
    })

    output$box_records_table <- DT::renderDT({
      df <- box_records()
      validate(need(!is.null(df) && nrow(df), "No records."))
      dt <- DT::datatable(df, rownames = FALSE, selection = "single",  # whole row → Record Details
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))  # hide ObsID
      .ik_dt_highlight_row(dt, "ObsID", box_rec_obs())             # the record you're viewing
    })

    observeEvent(input$box_records_table_rows_selected, {
      i <- input$box_records_table_rows_selected; df <- box_records()
      if (length(i) && !is.null(df) && i <= nrow(df)) {
        box_rec_obs(df$ObsID[i]); showTab(session = session, inputId = "box_tabs", target = "Record Details", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("box_records_table"), NULL)
    })

    output$box_record <- renderUI({
      if (is.null(box_rec_obs()))
        return(tags$p(class = "ik-drill-summary", "Click a record in the Records tab to see it in full here."))
      ob <- ik_observation(ik_data, box_rec_obs())
      if (is.null(ob)) return(tags$p("Record not found."))
      prefer <- if (isTRUE(prefer_scientific())) "scientific" else "vernacular"
      # box_record is shared by the camera Records modal (box_tabs) and the trap Checks modal
      # (trap_tabs) — point the back link at whichever list this record was opened from.
      back <- if (!is.null(box_traps())) .ik_tab_back(session$ns("tab_back"), "trap_tabs", "Checks", "Back to checks")
              else                       .ik_tab_back(session$ns("tab_back"), "box_tabs", "Records", "Back to records")
      tagList(back,
              .ovw_title(ik_data, ob, prefer),
              .ovw_tabs(ik_data, ob, prefer, tabset_id = session$ns("box_rec_subtabs")))
    })

    output$box_deps_table <- DT::renderDT({
      df <- box_deps()
      validate(need(!is.null(df) && nrow(df), "No deployments."))
      ix <- function(col) match(col, names(df)) - 1L                  # 0-based column index for DT
      DT::datatable(df, rownames = FALSE, selection = "single",       # row → its record(s)
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          order = list(list(ix(".start_sort"), "asc")),               # default: oldest → newest
          columnDefs = list(
            list(targets = ix("Start"), orderData = ix(".start_sort")),  # sort Start by its key
            list(targets = ix("End"),   orderData = ix(".end_sort")),
            list(targets = c(ix(".start_sort"), ix(".end_sort"), ix(".depid")), visible = FALSE))))
    })

    # Camera deployment drill: a deployment has many detections → list them (a row → the full record).
    observeEvent(input$box_deps_table_rows_selected, {
      i <- input$box_deps_table_rows_selected; df <- box_deps()
      if (!length(i) || is.null(df) || i > nrow(df)) return()
      obs <- cam()$observations
      obs <- obs[!is.na(obs$deploymentID) & obs$deploymentID == df$.depid[i] &
                   !is.na(obs$observationType) & obs$observationType == "animal", , drop = FALSE]
      obs <- .ov_net_obs(obs, ik_data)                  # net: exclude possible duplicates at this deployment
      show_records_modal(obs, TRUE, sprintf("Camera deployment · %s", df$Location[i]),
        sprintf("%s detection%s at this deployment — click a row for the full record",
                .ov_num(nrow(obs)), if (nrow(obs) == 1) "" else "s"))
      DT::selectRows(DT::dataTableProxy("box_deps_table"), NULL)
    })

    # Trap drill, level 1 — the TRAPS (one row per locationID). Click a trap → its checks (level 2).
    output$box_traps_table <- DT::renderDT({
      df <- box_traps()
      validate(need(!is.null(df) && nrow(df), "No traps."))
      ix <- function(col) match(col, names(df)) - 1L
      DT::datatable(df, rownames = FALSE, selection = "single",
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          order = list(list(ix("Catches"), "desc")),               # most productive traps first
          columnDefs = list(
            list(targets = ix("Last"), orderData = ix(".last_sort")),
            list(targets = c(ix(".last_sort"), ix(".locid")), visible = FALSE))))
    })
    observeEvent(input$box_traps_table_rows_selected, {
      i <- input$box_traps_table_rows_selected; df <- box_traps()
      if (!length(i) || is.null(df) || i > nrow(df)) return()
      box_checks(build_trap_checks(df$.locid[i])); box_rec_obs(NULL)
      box_checks_ctx(sprintf("The %s check%s of %s · %s. ", .ov_num(df$Checks[i]),
        if (df$Checks[i] == 1) "" else "s", df$Location[i],
        if (!is.na(df$Line[i])) paste("Line", df$Line[i]) else df$Reserve[i]))
      showTab(session = session, inputId = "trap_tabs", target = "Checks", select = TRUE)
      DT::selectRows(DT::dataTableProxy("box_traps_table"), NULL)
    })

    # Trap drill, level 2 — the selected trap's CHECKS. Click a check → its record (level 3).
    output$box_checks_ui <- renderUI({
      if (is.null(box_checks()))
        return(tags$p(class = "ik-drill-summary", "Open a trap in the Traps tab to list its checks here."))
      tagList(
        .ik_tab_back(session$ns("tab_back"), "trap_tabs", "Traps", "Back to traps"),
        tags$p(class = "ik-drill-summary", box_checks_ctx(),
               tags$b("Click a check"), " for its full record (catch or empty)."),
        DT::dataTableOutput(session$ns("box_checks_table")))
    })
    output$box_checks_table <- DT::renderDT({
      df <- box_checks()
      validate(need(!is.null(df) && nrow(df), "No checks."))
      ix <- function(col) match(col, names(df)) - 1L
      DT::datatable(df, rownames = FALSE, selection = "single",
        class = "stripe hover row-border ik-row-click",
        options = list(pageLength = 12, scrollX = TRUE, dom = "ftip", destroy = TRUE,
          order = list(list(ix("End"), "desc")),                   # most recent check first
          columnDefs = list(
            list(targets = ix("End"), orderData = ix(".end_sort")),
            list(targets = c(ix(".end_sort"), ix(".obsid")), visible = FALSE))))
    })
    observeEvent(input$box_checks_table_rows_selected, {
      i <- input$box_checks_table_rows_selected; df <- box_checks()
      if (length(i) && !is.null(df) && i <= nrow(df)) {
        box_rec_obs(df$.obsid[i]); showTab(session = session, inputId = "trap_tabs", target = "Record", select = TRUE)
      }
      DT::selectRows(DT::dataTableProxy("box_checks_table"), NULL)
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
      obs    <- .ov_net_obs(obs, ik_data)               # net: panel counts above are net, so is the list
      show_records_modal(obs, is_cam, sprintf("%s · %s", dev, sd$label),
        sprintf("%s %s in the current selection — click a row for the full record",
                .ov_num(nrow(obs)), if (is_cam) "detections" else "catches"))
    })
  })
}
