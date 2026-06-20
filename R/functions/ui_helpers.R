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
.ik_info <- function(id, title, ..., icon_name = "circle-question", class = NULL) {
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
