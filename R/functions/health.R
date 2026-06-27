# health.R — a compact "network health" strip: a row of red→green gauges, one per metric.
#
# DELIBERATELY simple + modular so it can be redone/extended as the health concept firms up. Two parts:
#   • ik_health_strip(items)        — the generic UI (knows nothing about trapping/monitoring).
#   • ik_trapping_health(ik_data,…) — the trapping METRIC builder: edit/add/reweight metrics HERE.
# A "metric" is just list(label, frac, value, tip) where `frac` is 0 (red / worst) … 1 (green / best).
# When you're ready for a single composite "health rank", combine the fracs in the builder — the strip
# itself doesn't change. A monitoring builder (ik_monitoring_health) can follow the same shape.

#' Render a row of red→green gauges. @param items list of list(label, frac[0..1], value, tip); NULL items
#' or non-finite fracs are dropped. @param title optional heading. @return a div, or NULL when empty.
#' @keywords internal
ik_health_strip <- function(items, title = NULL, help = NULL) {
  items <- Filter(function(it) !is.null(it) && length(it$frac) && is.finite(it$frac), items)
  if (!length(items)) return(NULL)
  gauge <- function(it) {
    f <- max(0, min(1, it$frac))                                  # clamp onto the red→green track
    div(class = "ik-health-item",
      div(class = "ik-health-head",
        tags$span(class = "ik-health-label", it$label),
        tags$span(class = "ik-health-value", it$value %||% "")),
      div(class = "ik-health-track", title = it$tip %||% "",
        div(class = "ik-health-marker", style = sprintf("left:%.1f%%", f * 100))))
  }
  div(class = "ik-health-strip",
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/health.css")),
    if (!is.null(title) || !is.null(help))
      tags$div(class = "ik-health-title", title, if (!is.null(help)) tagList(" ", help)),
    div(class = "ik-health-row", lapply(items, gauge)))
}

#' Help-modal body for the Network-health strip — explains each gauge + how the Period shapes it, and links
#' to the Trap check review for the per-trap detail (so "Network active" doesn't need its own drill).
#' @keywords internal
health_help_body <- function(ik_data) {
  P  <- function(...) tags$p(...)
  gd <- (ik_data$meta$trapping$health %||% list())$good_max
  tagList(
    P(tags$br(), "A quick read on whether the trapping network is ", tags$b("firing"), ". Each gauge sits on a ",
      tags$b("red → green"), " scale (a marker further right = better), and all of them honour the sidebar ",
      tags$b("Period"), " and ", tags$b("Reserve"), "."),
    tags$h6("Network active"),
    P("The share of the network's traps that were ", tags$b("checked at least once in the selected period"),
      " — checked traps ÷ all traps in scope. So the window matters: ", tags$b("Latest 12 months"), " asks ",
      tags$em("“what fraction of traps saw a visit in the last year”"), "; a shorter period asks the same over ",
      "less time, so fewer traps qualify and the figure is lower. 100% = every trap was visited within the window."),
    tags$h6("Well serviced"),
    P("Of those traps, the share on a ", tags$b("Good"), " servicing cadence — the mean gap between checks is tight ",
      "enough", if (!is.null(gd) && is.finite(gd)) sprintf(" (around %d days or less for this project)", round(gd)),
      " to read as ", tags$b("good"), " rather than ", tags$b("watch"), " or ", tags$b("neglected"),
      ", judged over the period's checks."),
    P(tags$br(), tags$b("Want the per-trap detail?"), " These are network-wide summaries. To see ",
      tags$b("which"), " traps are active, each one's cadence and status, and its full check history, open the ",
      tags$a(href = "#", `data-bs-dismiss` = "modal",
             onclick = "Shiny.setInputValue('ik_goto_nav','trap-review',{priority:'event'})",
             tags$b("Trap check review")), ".")
  )
}

#' Build the TRAPPING network-health metrics for the strip (Trapping overview). All-data-cheap; honours
#' the sidebar Reserve/Period via `selection`. v1 = two clearly-directional gauges (higher = greener);
#' add more (catch rate, catches, …) here once their good/bad direction is settled. @param trp optional
#' pre-resolved trap data (ik_resolve) to avoid a second pass. @return list of strip items.
#' @keywords internal
ik_trapping_health <- function(ik_data, selection, trp = NULL) {
  reserve <- .ik_nz(selection$reserve)
  if (is.null(trp)) trp <- ik_resolve(ik_data, selection, source_type = "trap")

  # 1. Network active — traps checked this period ÷ traps in the network (in scope).
  active <- length(unique(trp$deployments$locationID))
  tl <- ik_active_locations(ik_data, "trap")
  if (!is.null(reserve)) tl <- tl[tl$reserve %in% reserve, , drop = FALSE]
  total <- length(unique(tl$location_id))
  active_frac <- if (total > 0) active / total else NA_real_

  # 2. Well serviced — of the network's traps, the share on a "Good" servicing cadence (vs watch/neglected).
  per <- ik_trap_review(ik_data, .ik_nz(selection$season))
  if (!is.null(per) && !is.null(reserve)) per <- per[per$reserve %in% reserve, , drop = FALSE]
  good_frac <- if (!is.null(per) && nrow(per)) mean(per$status == "good", na.rm = TRUE) else NA_real_

  pctv <- function(f) if (is.finite(f)) sprintf("%.0f%%", 100 * f) else "—"
  list(
    list(label = "Network active", frac = active_frac, value = pctv(active_frac),
         tip = sprintf("%s of %s traps in the network were checked this period.",
                       format(active, big.mark = ","), format(total, big.mark = ","))),
    list(label = "Well serviced", frac = good_frac, value = pctv(good_frac),
         tip = "Share of the network's traps on a 'Good' servicing cadence (vs watch / neglected).")
  )
}
