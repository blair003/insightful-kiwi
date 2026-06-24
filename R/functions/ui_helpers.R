# ui_helpers.R — small shared UI helpers used across modules.

#' A modal title with an optional small muted SUBTITLE underneath — our convention for
#' carrying an id (deployment / observation / event) without cluttering the field list.
#'
#' @param title    The main title (string or tag).
#' @param subtitle Optional sub-line (e.g. "Deployment <id>").
#' @return A tag suitable for `modalDialog(title = …)`.
.ik_modal_title <- function(title, subtitle = NULL) {
  tags$div(
    tags$div(title),
    if (!is.null(subtitle)) tags$div(class = "ik-modal-subtitle", subtitle)
  )
}

#' A small "← Back" link for a deeper modal tab — returns the tabset to an earlier tab. The drill
#' modals auto-advance (click a row → next tab), so this makes the way back obvious. `input_id` is a
#' namespaced input the module observes once: `observeEvent(input$tab_back, updateTabsetPanel(session,
#' input$tab_back$tabset, selected = input$tab_back$to))`. @keywords internal
.ik_tab_back <- function(input_id, tabset, to, label = "Back") {
  tags$a(href = "#", class = "ik-tab-back",
    onclick = sprintf("event.preventDefault();Shiny.setInputValue('%s',{tabset:'%s',to:'%s'},{priority:'event'});",
                      input_id, tabset, to),
    HTML("&larr;"), paste0(" ", label))
}

#' JSON-encode a scalar for SAFE embedding in an inline-JS `Shiny.setInputValue` value — escapes
#' apostrophes, quotes, backslashes, etc. A reserve like "Bob's Cove Sanctuary" or an odd line name
#' would otherwise close the single-quoted JS string and silently break the click. Returns a quoted
#' literal (e.g. `"Bob's Cove"`), so embed it as `reserve:%s` (no surrounding quotes). NA → `""`.
#' @keywords internal
.ik_jsq <- function(x) as.character(jsonlite::toJSON(
  if (length(x) && !is.na(x)) as.character(x) else "", auto_unbox = TRUE))

#' Append a cache-busting version query to a `www/` asset URL so browsers always fetch the CURRENT
#' file after an edit. Shiny serves `www/` with caching headers and our `<script>`/`<link>` srcs are
#' static paths, so a changed `maps.js` or module CSS otherwise keeps serving the browser's cached
#' copy — the cause of "the fix is in the file but the page still shows the old behaviour" (and why a
#' map fix can look like it works in one environment/browser and not another). Version = file mtime,
#' so the URL changes exactly when the file does; falls back to a constant if the file isn't found.
#' @param path Path under `www/` (e.g. "js/maps.js", "styles/maps.css"). @keywords internal
.ik_asset <- function(path) {
  v <- tryCatch(as.integer(file.mtime(file.path("www", path))), error = function(e) NA_integer_)
  paste0(path, "?v=", if (is.na(v)) 1L else v)
}

#' Pretty datetime label for the record/drill MODAL tables — "15 Mar 2026 · 14:30", or date-only
#' "15 Mar 2026" when the time is midnight (date-only obs, e.g. traps). NA → "—". Pair with
#' `.ik_dt_when_defs()` + a hidden `as.numeric(when)` key for chronological sorting. (The main Records
#' table uses ISO via format_records_datetime(); this is the friendlier modal format.) @keywords internal
.ik_when_label <- function(x)
  ifelse(is.na(x), "—",
    ifelse(format(x, "%H:%M:%S") == "00:00:00", format(x, "%d %b %Y"), format(x, "%d %b %Y · %H:%M")))

#' A page/section title row: a heading tag + an inline help (?) button side by side. The one home for
#' the `div(flex, h3, .ik_info())` titlebar repeated across the feature pages. @param title a heading
#' tag (e.g. `tags$h3(class=…, "…")`). @param help an `.ik_info(...)` tagList (or NULL). @keywords internal
.ik_titlebar <- function(title, help = NULL) tags$div(class = "ik-titlebar", title, help)

#' DT `columnDefs` that make a formatted date column (shown as a nice string like "15 Mar 2026 ·
#' 14:30") sort CHRONOLOGICALLY, plus hide any helper columns. Append a numeric sort key to the data
#' first (`df$.when_sort <- as.numeric(<POSIXct>)`), then pass the df here. Mirrors the Deployments
#' Start/End fix (orderData → a hidden numeric key) so EVERY modal "When" column is sortable.
#' @param df       The data.frame (with `.when_sort` already appended).
#' @param when_col Display column to order chronologically (default "When").
#' @param hide     Extra column names to hide (e.g. "ObsID", ".loc").
#' @return A list for `DT::datatable(options = list(columnDefs = ...))`. @keywords internal
.ik_dt_when_defs <- function(df, when_col = "When", hide = character()) {
  idx <- function(nm) match(nm, names(df)) - 1L              # DT targets are 0-based
  hidden <- unname(vapply(unique(c(".when_sort", hide)), idx, integer(1)))
  list(list(visible = FALSE, targets = hidden),
       list(orderData = idx(".when_sort"), targets = idx(when_col)))
}

#' An inline help affordance: a small "?" (or "i") button that opens a Bootstrap modal with a
#' description. Pure client-side (Bootstrap's own modal JS) — no server observer needed, so it
#' drops into any UI. Use a "?" for whole-feature/page explainers, an "i" for a single field.
#'
#' @param id    Unique element id for the modal. In a module pass `ns("something_help")`.
#' @param title Modal heading.
#' @param ...   Modal body content (tags / text).
#' @param icon_name FontAwesome name for the trigger ("circle-question" default; "circle-info").
#' @param class Extra class(es) for the trigger button.
#' @return A tagList: the trigger button + the (hidden) modal markup.
.ik_info <- function(id, title, ..., icon_name = "question", class = NULL) {
  tagList(
    tags$button(type = "button", class = paste("ik-info-btn", class),
                title = "About this", `aria-label` = "About this",
                `data-bs-toggle` = "modal", `data-bs-target` = paste0("#", id),
                icon(icon_name)),
    tags$div(class = "modal fade ik-info-modal", id = id, tabindex = "-1", `aria-hidden` = "true",
      tags$div(class = "modal-dialog modal-dialog-centered modal-lg",
        tags$div(class = "modal-content",
          tags$div(class = "modal-header",
            tags$h5(class = "modal-title", title),
            tags$button(type = "button", class = "btn-close",
                        `data-bs-dismiss` = "modal", `aria-label` = "Close")),
          tags$div(class = "modal-body", ...),
          tags$div(class = "modal-footer",
            tags$button(type = "button", class = "btn btn-secondary",
                        `data-bs-dismiss` = "modal", "Close")))))
  )
}

#' A HOVER quick-help: a small ⓘ that shows `tip` as a native tooltip on mouseover — no click,
#' unlike the heavier `.ik_info` (?) modal. Use for a one-line clarification beside a heading or a
#' table-column label; reach for `.ik_info` when the explanation needs room. @param tip Plain-text
#' tooltip ("\n" for line breaks). @keywords internal
.ik_hint <- function(tip) tags$span(class = "ik-hint", title = tip, HTML("&#9432;"))

#' Highlight ONE row of a DT by a (usually hidden) id column — the "the row you came from"
#' marker in our drill modals. Robust where a plain `target = "row"` background is NOT: under
#' bslib/Bootstrap (and modern DataTables) zebra striping is painted on the CELLS (`td`), which
#' sits ON TOP of any `<tr>` background and hides it. So we tint each VISIBLE cell directly
#' (inline style beats the striping), driven by the id column via `valueColumns`. Apply this
#' BEFORE any per-cell `formatStyle` (e.g. an amber status cell) you want to win over the tint.
#'
#' @param dt     A DT datatable (already built with `DT::datatable`).
#' @param id_col Name of the column holding the row id (hide it via `columnDefs visible=FALSE`).
#' @param id     The id value to highlight; `NULL`/`NA` → no-op (returns `dt` unchanged).
#' @param colour Background colour (default our highlight tint — a clear, but not loud, blue).
#' @return The datatable with the highlight applied.
.ik_dt_highlight_row <- function(dt, id_col, id, colour = "#cfe2ff") {
  if (is.null(id) || (length(id) == 1L && is.na(id))) return(dt)
  vis <- setdiff(names(dt$x$data), id_col)
  DT::formatStyle(dt, columns = vis, valueColumns = id_col,
                  backgroundColor = DT::styleEqual(id, colour))
}

#' Shared ggplot theme that FOLLOWS the app's dark/light mode, so plots sit on the card cleanly the
#' way the leaflet basemap does. The plot/panel backgrounds are transparent (the bslib card shows
#' through, light or dark); only the ink — text, axes, gridlines — flips with the mode. Pair every
#' `renderPlot()` that uses this with `bg = "transparent"` (renderPlot defaults to white, which would
#' punch a white box through a dark card). Put module-specific `theme()` tweaks AFTER this so they win.
#'
#' @param dark      Logical — is the app in dark mode? (e.g. `identical(color_mode(), "dark")`).
#' @param base_size Base font size, passed to `theme_minimal()`.
#' @return A ggplot2 theme object to add to a plot.
ik_ggtheme <- function(dark = FALSE, base_size = 13) {
  fg   <- if (dark) "#e9ecef" else "#212529"
  grid <- if (dark) "#3a3f44" else "#dee2e6"
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key       = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = grid),
      panel.grid.minor = ggplot2::element_line(colour = grid),
      text          = ggplot2::element_text(colour = fg),
      axis.text     = ggplot2::element_text(colour = fg),
      plot.title    = ggplot2::element_text(colour = fg),
      plot.subtitle = ggplot2::element_text(colour = fg),
      plot.caption  = ggplot2::element_text(colour = fg),
      strip.text    = ggplot2::element_text(colour = fg))
}

#' The muted "ink" colour for in-plot annotations (data labels, reference lines) at the current mode
#' — so a `geom_text`/`geom_vline` stays legible on a dark card. @param dark Logical. @keywords internal
ik_plot_ink <- function(dark = FALSE) if (dark) "#ced4da" else "#444444"
