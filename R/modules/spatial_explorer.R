# spatial_explorer.R (module) — "Spatial explorer": an Insights → Combined-analysis CUSTOM map that
# composites camera (Monitoring) + trap (Trapping) data onto one canvas. Where Predator pressure
# answers a FIXED question (predators-high × protected-low), the explorer is the open-ended "expert
# mode": pick any species (or group) and read camera DETECTIONS against trap CATCHES.
#
# Built in phases on the shared map_draw.R draw tier — it owns its own leaflet base(s) and DRAWS only
# through the helpers, like predator_pressure.R (NOT maps_server). SPECIES modes (what a marker means):
# Combined (merged to one detection + one catch value per location) · Per species (one toggleable
# glyph-icon layer per picked entry) · Predator-vs-Protected (the priority surface + catches). DISPLAY
# modes (how many panes): Standard (one map) · Side by side (two maps — `make_pane()` is instantiated
# once per pane, so both share the whole draw pipeline; a chain/padlock LOCK mirrors pane A's picks
# into pane B or frees B to its own; "Link views" syncs pan/zoom). Marker → drill modal is stage-two.
#
# Layers (native addLayersControl, top-right) depend on the species mode; Detections (camera RAI) and
# Catches (trap count) are ALWAYS separate layers + legends (units differ — rate vs count), the device
# read by COLOUR + SHAPE (teal disc = camera, amber square = trap), size ∝ value.

.SPEX_DETECT <- "#0d9488"   # teal  — camera device-field context (faint)
.SPEX_CATCH  <- "#d97706"   # amber — trap device-field context (faint)
# Markers are coloured by species ROLE (predator / protected / other — "bad vs good"), and shaped by
# DEVICE (disc = camera detection, square = trap catch); size = value. The role split is what keeps a
# mustelid from looking like a kiwi at the same camera.
.SPEX_ROLE       <- c(predator = "#d62728", protected = "#2e9e3f", other = "#8a8a8a")   # red / green / grey
.SPEX_ROLE_LABEL <- c(predator = "Predators", protected = "Protected", other = "Other")

#' Body of the Spatial-explorer "how to read this" help modal — tabbed, matching the app help format.
#' `cam_norm` = the per-camera scale the detection rates are shown at (project config). @keywords internal
spatial_explorer_help_body <- function(cam_norm = 500) {
  P  <- function(...) tags$p(...)
  ch <- format(cam_norm, big.mark = ",")
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "What it shows", icon = icon("circle-question"),
      P(tags$br(), "An ", tags$b("expert-mode"), " map: pick any species (or a group like Mustelids) and see ",
        "where the ", tags$b("cameras"), " detect it against where the ", tags$b("traps"), " catch it — ",
        "the two devices on one canvas, for the sidebar's period and reserve."),
      P(tags$b("Colour says the role"), " — ", tags$b(tags$span(style = "color:#d62728", "predators")), " (red), ",
        tags$b(tags$span(style = "color:#2e9e3f", "protected")), " (green), ", tags$b("other"), " (grey) — so a mustelid never ",
        "looks like a kiwi. ", tags$b("Shape says the device"), ": a ", tags$b("disc"), " is a camera detection, a ",
        tags$b("square"), " is a trap catch. ", tags$b("Size says how much"), ". Where more than one role is at a ",
        "location the marker is ", tags$b("split"), " (e.g. half red / half green) so neither hides the other — ",
        tags$b("click it for the full per-species breakdown"), "."),
      P(tags$b("Markers"), " (sidebar): ", tags$b("By role"), " draws one role-split marker per location; ",
        tags$b("By species"), " gives each picked species its own toggleable layer (role-coloured, with its silhouette; ",
        "markers fan out where species share a spot)."),
      P(tags$b("Display"), " switches between one map and two ", tags$b("side by side"), ". Break a ",
        tags$b("chain"), " (on Data period or Species) to compare a different period — ", tags$b("prior period"),
        ", ", tags$b("same period last year"), ", or any other — or a different species on the right map; ",
        tags$b("Link views"), " pans/zooms them together. Reserve is always shared."),
      P(tags$b("Hint:"), " select a single ", tags$b("Reserve"), " and zoom in — the markers are most readable at reserve scale.")),
    tabPanel(
      "The layers", icon = icon("layer-group"),
      P(tags$br(), "Toggle these top-right."),
      tags$ul(
        tags$li(tags$b("Detections"), " — camera markers (discs), one per location, role-split. ",
                tags$b("Catches"), " — trap markers (squares). (In Per-species mode there's one layer per species instead.)"),
        tags$li(tags$b("Cameras / Traps"), " — the device field (every active camera / trap), faint context dots. Off by default."),
        tags$li(tags$b("Boundary"), " — the monitored footprint (convex hull of the devices) per reserve; hover for its name."))),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Detections (RAI)"), " — detections of the picked species ÷ that camera's camera-hours × ",
                ch, " (the per-camera scale from config), net of likely duplicates. Combined = all picked species summed."),
        tags$li(tags$b("Catches"), " — captures of the picked species at each trap in the period (raw count)."),
        tags$li(tags$b("Role split"), " — at a location the marker is divided into equal sectors for the roles present (predator / protected / other); click for the per-species numbers."),
        tags$li(tags$b("Boundary"), " — the convex hull of the deployed devices, per reserve.")),
      P(tags$em("All for the sidebar's period & reserve; everything redraws when you change them.")))
  )
}

#' Spatial-explorer sidebar controls — a "Map options" group (display mode + Link-views + species mode),
#' then the Data period and Species as v0.1-style LINKABLE items: each carries a chain/broken-chain
#' toggle (side-by-side only); breaking the chain reveals that setting's tinted "Comparison map"
#' control for pane B. Reserve (rendered by selection_ui, below) is always shared. The primary period
#' inputs live in the two selection_servers' namespaces so those servers build the period SPECS; the
#' species pickers live in this module's namespace. Choices are baked here / populated server-side.
#' @keywords internal
spatial_explorer_controls <- function(id, ik_data = NULL) {
  ns   <- NS(id)
  selA <- NS("spatial_explorer_selection")     # primary period input → the main selection_server's spec
  per_choices <- if (!is.null(ik_data)) ik_period_choices(ik_data)
  per_default <- if (!is.null(ik_data)) (ik_default_period(ik_data) %||% "rolling12") else "rolling12"
  # The comparison period is the full picker with two RELATIVE quick options on top — "Prior period" /
  # "Same period last year" (computed from the left map). Falls back to the left map when the relative
  # window doesn't resolve (e.g. the left map is on a rolling window, which has no clean prior).
  cmp_choices <- c(list("Prior period" = "prior", "Same period last year" = "last_year"), per_choices)

  # A LINKABLE item: a chain/broken-chain toggle (side-by-side only), the primary widget, and a tinted
  # "Comparison map" widget revealed when the chain is broken.
  linkable <- function(heading, link_id, primary, comparison) tagList(
    div(class = "ik-spex-setting-h", tags$span(heading),
      conditionalPanel("input.display != 'single'", ns = ns,
        tags$span(class = "ik-spex-link", title = "Link or unlink this setting across the two maps",
          checkboxInput(ns(link_id), label = tagList(
            tags$span(class = "ik-spex-linked",   icon("link")),
            tags$span(class = "ik-spex-unlinked", icon("link-slash"))), value = TRUE)))),
    primary,
    conditionalPanel(sprintf("input.display != 'single' && input.%s == false", link_id), ns = ns,
      div(class = "ik-spex-compare-field",
        div(class = "ik-spex-compare-tag", icon("clone"), " Comparison map"),
        comparison)))

  species_pickers <- function(sfx = "") selectInput(ns(paste0("species", sfx)), NULL, choices = NULL, multiple = TRUE)
  per_input       <- function(sel_ns) selectInput(sel_ns("period"), NULL, choices = per_choices, selected = per_default)

  div(class = "ik-selection ik-view-controls ik-spex-controls",
    tags$div(class = "ik-view-controls-h", "Map options"),
    radioButtons(ns("display"), NULL, inline = TRUE,
      choiceNames  = list(tagList(icon("map"), " One"), tagList(icon("table-columns"), " Side by side"),
                          tagList(icon("arrows-left-right"), " Swipe")),
      choiceValues = c("single", "sbs", "swipe"), selected = "single"),
    conditionalPanel("input.display == 'sbs'", ns = ns,   # swipe always links (the two halves must align)
      checkboxInput(ns("link"), tagList(icon("link"), " Link views (pan/zoom together)"), value = TRUE)),
    radioButtons(ns("mode"), "Markers",
      choices = c("By role" = "combined", "By species" = "separate"), selected = "combined"),
    linkable("Data period", "link_period", per_input(selA),
      selectInput(ns("compare_period"), NULL, choices = cmp_choices, selected = "prior")),
    linkable("Species",     "link_species", species_pickers(""),   species_pickers("_b")))
}

#' Spatial-explorer nav panel. @param id Module id. @param ik_data The container (camera-hour scale).
#' @keywords internal
spatial_explorer_ui <- function(id, ik_data = NULL) {
  ns <- NS(id)
  cam_norm <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500
  .map_panel <- function(out) leaflet::leafletOutput(ns(out), height = "100%")
  nav_panel(
    "Spatial explorer", value = "spatial-explorer", icon = icon("layer-group"),
    tags$link(rel = "stylesheet", type = "text/css", href = .ik_asset("styles/maps.css")),   # .ik-maps-split / -side / records
    tags$script(src = .ik_asset("js/maps.js")),                                              # resize-on-tab-show fix
    tags$script(src = .ik_asset("js/spatial_explorer.js")),                                  # pane invalidate + link-views sync
    div(class = "ik-maps ik-spex ik-map-fill",                  # fill the viewport (map row grows under the header)
        .ik_page_header("Spatial explorer",
            description = "Composite the cameras and the traps for any species onto one map — where it's detected against where it's caught.",
            help = .ik_info(ns("spex_help"), "Spatial explorer — how to read this", spatial_explorer_help_body(cam_norm)),
            banner = div(class = "ik-page-period", uiOutput(ns("period_banner")))),
        uiOutput(ns("caption")),
        # A flex row: pane A (always) · pane B (side-by-side only) · the records side (one-map only).
        # The conditionalPanel wrappers render display:contents when shown (transparent to flex), so the
        # FLEX ITEM is the inner classed div — pane A is a plain direct child, the rest sit inside.
        div(class = "ik-spex-row",
          div(class = "ik-spex-main", .map_panel("map")),
          conditionalPanel("input.display != 'single'", ns = ns,         # the comparison map (side-by-side OR swipe)
            div(class = "ik-spex-compare-pane", .map_panel("map_b"))),
          conditionalPanel("input.display == 'swipe'", ns = ns,          # the draggable curtain divider
            div(class = "ik-spex-swipe-divider", tags$span(class = "ik-spex-swipe-grip"))),
          conditionalPanel("input.display == 'single'", ns = ns,
            div(class = "ik-spex-side",
              div(class = "ik-maps-records",
                  div(class = "ik-maps-records-header",
                      uiOutput(ns("records_caption")),
                      downloadButton(ns("download_csv"), "Download CSV", class = "btn-sm")),
                  DT::DTOutput(ns("table")))))),
        uiOutput(ns("unplaced")))
  )
}

#' Spatial-explorer server.
#' @param id Module id. @param ik_data The container. @param prefer_scientific reactive name pref.
#' @param selection reactive selection SPEC (Period + Reserve from the sidebar). @param color_mode theme.
#' @param active reactive, TRUE when this tab is current (gates the heavy metric reactives).
spatial_explorer_server <- function(id, ik_data, prefer_scientific = reactive(FALSE),
                                    selection = reactive(list()), color_mode = reactive("light"),
                                    active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    output$period_banner <- renderUI(.ik_period_banner(ik_data, selection()))
    is_dark <- reactive(identical(color_mode(), "dark"))
    prefer  <- reactive(if (isTRUE(prefer_scientific())) "scientific" else "vernacular")

    per_cam  <- (ik_data$meta$camera$rai %||% list())$camera_hours %||% 500   # per-camera marker scale
    grp_taxa <- ik_group_taxa(ik_data)
    all_sci  <- sort(unique(stats::na.omit(ik_observations(ik_data, with_location = FALSE)$scientificName)))
    .sg      <- ik_species_groups(ik_data)
    # Default selection: the FIRST predator group + the FIRST protected group in species_groups order
    # (priority order — Mustelids, then Kiwi for WKT), so the role split shows out of the box. Same
    # "first of the role" rule the predator/protected pickers use elsewhere — not hard-coded names.
    .first_role <- function(role) { l <- unique(.sg$label[!is.na(.sg$role) & .sg$role == role]); if (length(l)) paste0("grp:", l[1]) else NULL }
    .sp_default  <- { d <- c(.first_role("predator"), .first_role("protected")); if (length(d)) d else "__all__" }
    # The conservation ROLE of each scientificName (predator / protected / other) — drives marker colour.
    .role_of <- function(sci) { r <- .sg$role[match(sci, .sg$scientificName)]
      r[is.na(r) | !(r %in% c("predator", "protected"))] <- "other"; r }
    # The icon glyph KEY for a picked value (group → its group key; species → its group's key, else paw).
    .glyph_key <- function(v) {
      if (startsWith(v, "grp:")) (.sg$group[match(sub("^grp:", "", v), .sg$label)] %||% "other")
      else if (startsWith(v, "sci:")) (.sg$group[match(sub("^sci:", "", v), .sg$scientificName)] %||% "other")
      else "other"
    }

    # Populate a pane's Species picker, keeping any existing pick. `sfx` = "" (pane A) or "_b" (comparison).
    .populate_pickers <- function(sfx = "", species_default = .sp_default) {
      observe({ p <- prefer()
        sel <- isolate(input[[paste0("species", sfx)]]); if (!length(sel)) sel <- species_default
        updateSelectInput(session, paste0("species", sfx),
          choices  = ik_species_choices_full(ik_data, p, all_label = "All species", all_value = "__all__"), selected = sel)
      })
    }
    .populate_pickers("")     # pane A (B is populated on first unlock, below)

    reserve_hulls <- reactive({ req(active()); ik_reserve_boundary(ik_data, selection()$reserve) })

    .dev_context <- function(source) {
      locs <- ik_active_locations(ik_data, source)
      locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
      rsv  <- .ik_nz(selection()$reserve); if (!is.null(rsv)) locs <- locs[locs$reserve %in% rsv, , drop = FALSE]
      if (!nrow(locs)) NULL else locs
    }

    # ── Marker click → a tabbed MODAL of that location: a LIST (a camera's detections / a trap's checks
    # in the selected period) that DRILLS into a record's full detail (second tab). Shared across both
    # panes; one modal open at a time. ───────────────────────────────────────────────────────────────
    modal_kind <- reactiveVal(NULL)   # "camera" | "trap"
    modal_loc  <- reactiveVal(NULL)
    modal_spec <- reactiveVal(NULL)   # the clicked pane's selection (its period)
    modal_all  <- reactiveVal(FALSE)  # trap only: show the all-time history vs the selected period
    modal_obs  <- reactiveVal(NULL)   # the observationID open in the Record-details tab

    .open_location_modal <- function(kind, loc, spec) {
      modal_kind(kind); modal_loc(loc); modal_spec(spec); modal_all(FALSE); modal_obs(NULL)
      nm  <- ik_data$app$geography$locations$name[match(loc, ik_data$app$geography$locations$location_id)] %||% loc
      sub <- if (identical(kind, "trap")) "Trap · its check history" else "Camera · what was detected here, in the selected period"
      lst <- if (identical(kind, "trap")) "Checks" else "Detections"
      showModal(modalDialog(
        title = .ik_modal_title(nm, sub), size = "l", easyClose = TRUE, footer = modalButton("Close"),
        tabsetPanel(id = session$ns("spex_modal_tabs"),
          tabPanel(lst, icon = icon("list"),
                   DT::dataTableOutput(session$ns("spex_modal_list")), uiOutput(session$ns("spex_modal_more"))),
          tabPanel("Record details", icon = icon("circle-info"), uiOutput(session$ns("spex_modal_record"))))))
      hideTab(session = session, inputId = "spex_modal_tabs", target = "Record details")
    }

    .camera_modal_rows <- reactive({
      loc <- modal_loc(); if (is.null(loc) || !identical(modal_kind(), "camera")) return(NULL)
      obs <- ik_observations(ik_data, with_location = TRUE)
      obs <- obs[!is.na(obs$observationType) & obs$observationType == "animal" & !is.na(obs$locationID) &
                   obs$locationID == loc & !is.na(obs$eventStart), , drop = FALSE]
      seas <- .ik_nz(modal_spec()$season)
      if (!is.null(seas) && nrow(obs)) { op <- ik_observation_period(ik_data)
        osea <- op$calendar_season[match(obs$observationID, op$observationID)]
        obs <- obs[!is.na(osea) & osea %in% seas, , drop = FALSE] }
      if (!nrow(obs)) return(NULL)
      obs$role <- .role_of(obs$scientificName)
      obs[order(match(obs$role, c("predator", "protected", "other")), obs$eventStart), , drop = FALSE]
    })
    .trap_modal_rows <- reactive({
      loc <- modal_loc(); if (is.null(loc) || !identical(modal_kind(), "trap")) return(NULL)
      ik_trap_checks(ik_data, loc, if (isTRUE(modal_all())) NULL else .ik_nz(modal_spec()$season))
    })

    output$spex_modal_list <- DT::renderDT({
      if (identical(modal_kind(), "trap")) {
        ch <- .trap_modal_rows(); validate(need(!is.null(ch) && nrow(ch), "No checks in the selected period."))
        df <- data.frame(Date = format(ch$check_date, "%d %b %Y"), Outcome = ch$outcome,
          Bait = ifelse(is.na(ch$bait), "—", ch$bait), Volunteer = ifelse(is.na(ch$volunteer), "—", ch$volunteer),
          ObsID = ch$observationID, check.names = FALSE, stringsAsFactors = FALSE)
        DT::datatable(df, rownames = FALSE, selection = "single", class = "stripe hover row-border ik-row-click",
          options = list(pageLength = 10, scrollX = TRUE, dom = "ftip", columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))))
      } else {
        det <- .camera_modal_rows(); validate(need(!is.null(det) && nrow(det), "No detections here in this period."))
        .lnk <- function(label, oid) sprintf(
          "<a class='ik-link' onclick=\"Shiny.setInputValue('%s',{id:'%s'},{priority:'event'})\">%s</a>",
          session$ns("spex_modal_view"), oid, htmltools::htmlEscape(label))
        df <- data.frame(Species = mapply(.lnk, ik_species_label(det$scientificName, ik_data, prefer()), det$observationID),
          Role = unname(.SPEX_ROLE_LABEL[det$role]), When = .ik_when_label(det$eventStart),
          .when_sort = as.numeric(det$eventStart), check.names = FALSE, stringsAsFactors = FALSE)
        DT::datatable(df, rownames = FALSE, selection = "none", escape = -1, class = "stripe hover row-border",
          options = list(pageLength = 10, scrollX = TRUE, dom = "ftip", columnDefs = .ik_dt_when_defs(df, "When")))
      }
    })
    output$spex_modal_more <- renderUI({
      if (!identical(modal_kind(), "trap") || isTRUE(modal_all())) return(NULL)
      tags$div(class = "ik-spex-modal-more",
        actionLink(session$ns("spex_modal_loadall"), tagList(icon("clock-rotate-left"), " Show the full history (all periods)")))
    })
    observeEvent(input$spex_modal_loadall, modal_all(TRUE))

    observeEvent(input$spex_modal_view, {                      # camera: species link → its record
      oid <- input$spex_modal_view$id
      if (length(oid) && nzchar(oid)) { modal_obs(oid); showTab(session = session, inputId = "spex_modal_tabs", target = "Record details", select = TRUE) }
    })
    observeEvent(input$spex_modal_list_rows_selected, {        # trap: check row → its record
      i <- input$spex_modal_list_rows_selected; ch <- .trap_modal_rows()
      if (length(i) && !is.null(ch) && i <= nrow(ch)) { modal_obs(ch$observationID[i]); showTab(session = session, inputId = "spex_modal_tabs", target = "Record details", select = TRUE) }
      DT::selectRows(DT::dataTableProxy("spex_modal_list"), NULL)
    })
    observeEvent(input$spex_modal_back, updateTabsetPanel(session, input$spex_modal_back$tabset, selected = input$spex_modal_back$to))
    output$spex_modal_record <- renderUI({
      if (is.null(modal_obs())) return(tags$p(class = "ik-maps-hint", "Pick a row in the list to see its full record here."))
      ob <- ik_observation(ik_data, modal_obs()); if (is.null(ob)) return(tags$p("Record not found."))
      tagList(.ik_tab_back(session$ns("spex_modal_back"), "spex_modal_tabs",
                           if (identical(modal_kind(), "trap")) "Checks" else "Detections", "Back to the list"),
              .ovw_title(ik_data, ob, prefer()),
              .ovw_tabs(ik_data, ob, prefer(), tabset_id = session$ns("spex_modal_rec_sub")))
    })

    # ── one PANE = the whole draw pipeline for a single leaflet output, parameterised by its input
    # getters (so panes A and B share every observer). Returns a handle of its data reactives so the
    # records side can read pane A. ───────────────────────────────────────────────────────────────
    make_pane <- function(map_id, side, pane_active, get_selection, get_species, get_mode) {
      proxy <- function() leaflet::leafletProxy(map_id, session)
      # In SWIPE, the LEFT map (side "a") moves its layers/legend to the left so they don't cover the
      # right map's controls; otherwise everything sits on the right.
      ctrl <- reactive(if (identical(side, "a") && identical(input$display, "swipe"))
        list(layers = "topleft", legend = "bottomleft") else list(layers = "topright", legend = "bottomright"))

      sp_sci <- reactive({ v <- get_species(); if (!length(v) || "__all__" %in% v) all_sci else ik_resolve_species_choice(v, grp_taxa) })
      sp_lab <- reactive({ v <- get_species(); if (!length(v) || "__all__" %in% v) "all species" else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })
      mode   <- reactive(if (identical(get_mode(), "separate")) "separate" else "combined")
      entries <- reactive({ v <- get_species(); if (!length(v) || "__all__" %in% v) paste0("grp:", unique(.sg$label)) else v })

      detect_all <- reactive({ req(pane_active()); sci <- sp_sci(); if (!length(sci)) return(NULL)
        ik_location_metric(ik_data, get_selection(), list(Selected = sci), "camera", norm = per_cam) })
      catch_all  <- reactive({ req(pane_active()); sci <- sp_sci(); if (!length(sci)) return(NULL)
        ik_location_metric(ik_data, get_selection(), list(Selected = sci), "trap") })
      .placed    <- function(m) if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE]
      detect_pts <- reactive(.placed(detect_all()))
      catch_pts  <- reactive(.placed(catch_all()))

      combined <- reactive({                                   # union of locations (camera RAI and/or trap count)
        d <- detect_pts(); cc <- catch_pts()
        dd <- if (is.null(d))  NULL else data.frame(location_id = d$location_id, name = d$name,
          reserve = d$reserve, line = d$line, rai = d$metric, captures = NA_real_, stringsAsFactors = FALSE)
        tt <- if (is.null(cc)) NULL else data.frame(location_id = cc$location_id, name = cc$name,
          reserve = cc$reserve, line = cc$line, rai = NA_real_, captures = cc$captures, stringsAsFactors = FALSE)
        m <- rbind(dd, tt); if (is.null(m) || !nrow(m)) return(NULL)
        do.call(rbind, lapply(split(seq_len(nrow(m)), m$location_id), function(ix) {  # a site with both → one row
          g <- m[ix, , drop = FALSE]
          g$rai[1] <- suppressWarnings(max(g$rai, na.rm = TRUE)); g$captures[1] <- suppressWarnings(max(g$captures, na.rm = TRUE))
          g[1, , drop = FALSE]
        })) -> ag
        ag$rai[!is.finite(ag$rai)] <- NA; ag$captures[!is.finite(ag$captures)] <- NA
        ag[order(-ifelse(is.na(ag$rai), 0, ag$rai), -ifelse(is.na(ag$captures), 0, ag$captures)), , drop = FALSE]
      })

      output[[map_id]] <- leaflet::renderLeaflet({
        locs <- ik_data$app$geography$locations
        locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
        ik_map_base(panes = c("boundary", "cameras", "traps", "detections", "catches", "selected"),
          overlay_groups = c("Cameras", "Traps", "Boundary"),   # value layers are added by the dynamic control
          is_dark = isolate(is_dark()), fit = locs, hide_groups = c("Cameras", "Traps"), pane_z0 = 410)
      })
      outputOptions(output, map_id, suspendWhenHidden = FALSE)

      observeEvent(color_mode(), ik_swap_theme_tiles(proxy(), is_dark()), ignoreInit = TRUE)

      observe({                                                # re-frame to the data extent
        pts <- rbind(
          { d <- detect_pts(); if (is.null(d)) NULL else d[, c("longitude", "latitude")] },
          { c <- catch_pts();  if (is.null(c)) NULL else c[, c("longitude", "latitude")] })
        if (is.null(pts) || !nrow(pts)) return(); p <- proxy()
        if (nrow(pts) == 1 || (diff(range(pts$longitude)) == 0 && diff(range(pts$latitude)) == 0))
          leaflet::setView(p, pts$longitude[1], pts$latitude[1], zoom = 14)
        else leaflet::fitBounds(p, min(pts$longitude), min(pts$latitude), max(pts$longitude), max(pts$latitude),
          options = list(padding = c(30, 30)))
      })

      observe(ik_draw_device_layer(proxy(), .dev_context("camera"),   # Cameras / Traps — faint device field (off by default)
        fill_color = .SPEX_DETECT, group = "Cameras", pane = "cameras", radius = 3, fill_opacity = 0.35, label = ~name))
      observe(ik_draw_device_layer(proxy(), .dev_context("trap"),
        fill_color = .SPEX_CATCH, group = "Traps", pane = "traps", radius = 3, fill_opacity = 0.35, label = ~name))

      # ── COMBINED: ONE role-PIE marker per location (Detections discs / Catches squares) ──────────
      # The per-location role breakdown for one device — long: location × present role × value.
      role_breakdown <- function(source) {
        sci <- sp_sci(); if (!length(sci)) return(NULL)
        roles <- .role_of(sci); out <- list()
        for (r in c("predator", "protected", "other")) {
          s <- sci[roles == r]; if (!length(s)) next
          m <- ik_location_metric(ik_data, get_selection(), stats::setNames(list(s), "x"), source,
                                  norm = if (source == "camera") per_cam else NULL)
          if (is.null(m) || !nrow(m)) next
          val <- if (source == "camera") m$metric else m$captures
          keep <- is.finite(val) & val > 0 & is.finite(m$latitude) & is.finite(m$longitude)
          if (!any(keep)) next
          out[[r]] <- data.frame(loc = m$location_id[keep], name = m$name[keep], lat = m$latitude[keep],
                                 lng = m$longitude[keep], role = r, value = val[keep], stringsAsFactors = FALSE)
        }
        if (!length(out)) NULL else do.call(rbind, out)
      }
      .draw_pies <- function(p, source, group) {
        bd <- if (identical(mode(), "combined")) role_breakdown(source) else NULL
        if (is.null(bd) || !nrow(bd)) { leaflet::clearGroup(p, group); return(invisible(p)) }
        ord  <- c("predator", "protected", "other")
        recs <- lapply(split(seq_len(nrow(bd)), bd$loc), function(ix) {
          g <- bd[ix, , drop = FALSE]; g <- g[order(match(g$role, ord)), , drop = FALSE]
          list(loc = g$loc[1], name = g$name[1], lat = g$lat[1], lng = g$lng[1], roles = g$role, values = g$value, total = sum(g$value)) })
        total <- vapply(recs, function(x) x$total, numeric(1))
        sz <- if (source == "camera") round(2 * ik_marker_radius(total, 6, 18, cap_pctl = 0.98) + 6)
              else round(2 * ik_marker_radius(total, 5, 15, cap = ik_robust_cap(total, 0.9)) + 6)
        urls <- vapply(recs, function(x) ik_role_pie_svg(x$roles, .SPEX_ROLE, if (source == "camera") "circle" else "square"), character(1))
        lab  <- vapply(recs, function(x) sprintf("%s — %s · click for detail", x$name,
          paste(sprintf("%s %s", unname(.SPEX_ROLE_LABEL[x$roles]),
                        if (source == "camera") sprintf("RAI %.2f", x$values) else paste0(as.integer(x$values), " caught")), collapse = " · ")), character(1))
        leaflet::clearGroup(p, group)
        leaflet::addMarkers(p, lng = vapply(recs, function(x) x$lng, numeric(1)), lat = vapply(recs, function(x) x$lat, numeric(1)),
          group = group, layerId = paste0(if (source == "camera") "C|" else "K|", vapply(recs, function(x) x$loc, character(1))),
          icon = leaflet::icons(iconUrl = urls, iconWidth = sz, iconHeight = sz, iconAnchorX = sz / 2, iconAnchorY = sz / 2), label = lab)
        invisible(p)
      }
      observe(.draw_pies(proxy(), "camera", "Detections"))
      observe(.draw_pies(proxy(), "trap",   "Catches"))

      # ── BY SPECIES: one toggleable LAYER per picked ENTRY (role-coloured, silhouette). Where several
      # species share a location their markers are FANNED OUT around the point so none hides another,
      # and each keeps a UNIQUE layerId (C|<species>|<loc>) so leaflet doesn't drop the duplicates. ──
      drawn_groups <- reactiveVal(character(0))
      .draw_fanned <- function(p, ent, which, source, prefix) {
        rows <- do.call(rbind, lapply(ent, function(e) { m <- e[[which]]; if (is.null(m)) return(NULL)
          val <- if (source == "camera") m$metric else m$captures
          keep <- is.finite(m$latitude) & is.finite(m$longitude) & is.finite(val) & val > 0
          if (!any(keep)) return(NULL)
          data.frame(label = e$label, key = e$key, colour = e$colour, loc = m$location_id[keep],
            name = m$name[keep], lat = m$latitude[keep], lng = m$longitude[keep], value = val[keep], stringsAsFactors = FALSE) }))
        if (is.null(rows) || !nrow(rows)) return()
        rows <- rows[order(rows$loc), , drop = FALSE]
        rows$idx  <- stats::ave(seq_len(nrow(rows)), rows$loc, FUN = seq_along)   # 1..n within a location
        rows$n    <- stats::ave(rows$idx, rows$loc, FUN = max)
        ang <- ifelse(rows$n > 1, 2 * pi * (rows$idx - 1) / rows$n, 0); rr <- 0.00038   # ~42 m fan radius
        rows$olat <- ifelse(rows$n > 1, rows$lat + rr * sin(ang), rows$lat)
        rows$olng <- ifelse(rows$n > 1, rows$lng + rr * cos(ang) / cos(rows$lat * pi / 180), rows$lng)
        for (lbl in unique(rows$label)) {
          s  <- rows[rows$label == lbl, , drop = FALSE]
          sz <- if (source == "camera") round(2 * ik_marker_radius(s$value, 6, 18, cap_pctl = 0.98) + 6)
                else round(2 * ik_marker_radius(s$value, 5, 15, cap = ik_robust_cap(s$value, 0.9)) + 6)
          leaflet::addMarkers(p, lng = s$olng, lat = s$olat, group = lbl,
            icon = ik_species_marker_icon(s$key, s$colour[1], if (source == "camera") "circle" else "square", sz),
            layerId = paste0(prefix, lbl, "|", s$loc),
            label = sprintf("%s — %s %s · click for detail", s$name, lbl,
                            if (source == "camera") sprintf("RAI %.2f", s$value) else paste0(as.integer(s$value), " caught")))
        }
      }
      observe({
        p <- proxy()
        for (g in drawn_groups()) leaflet::clearGroup(p, g)
        if (!identical(mode(), "separate")) { drawn_groups(character(0)); return() }
        req(pane_active())
        ent <- Filter(Negate(is.null), lapply(entries(), function(v) {
          taxa <- ik_choice_taxa(v, grp_taxa, ik_data, isolate(prefer())); if (is.null(taxa)) return(NULL)
          sci  <- unlist(taxa, use.names = FALSE)
          role <- names(sort(table(.role_of(sci)), decreasing = TRUE))[1] %||% "other"
          list(label = names(taxa)[1], key = .glyph_key(v), colour = unname(.SPEX_ROLE[role]),
               det   = ik_location_metric(ik_data, get_selection(), stats::setNames(list(sci), "x"), "camera", norm = per_cam),
               catch = ik_location_metric(ik_data, get_selection(), stats::setNames(list(sci), "x"), "trap")) }))
        .draw_fanned(p, ent, "det",   "camera", "C|")
        .draw_fanned(p, ent, "catch", "trap",   "K|")
        drawn_groups(unique(vapply(ent, function(e) e$label, character(1))))
      })

      observe({                                                # dynamic layers control — position by side (swipe)
        p <- proxy()
        overlays <- if (identical(mode(), "combined")) c("Detections", "Catches", "Cameras", "Traps", "Boundary")
                    else c(drawn_groups(), "Cameras", "Traps", "Boundary")
        leaflet::addLayersControl(p, baseGroups = c("Map", "Satellite"), overlayGroups = overlays,
          position = ctrl()$layers, options = leaflet::layersControlOptions(collapsed = FALSE))
        shown <- isolate(input[[paste0(map_id, "_groups")]])
        for (g in c("Cameras", "Traps")) if (is.null(shown) || !(g %in% shown)) leaflet::hideGroup(p, g)
      })

      observe({ p <- proxy(); leaflet::clearGroup(p, "Boundary")  # Boundary
        ik_add_reserve_boundary(p, reserve_hulls(), color = if (is_dark()) "#cfd8dc" else "#37474f") })

      observe({                                                # legend (role key) + shape key — position by side
        p <- proxy(); leaflet::clearControls(p)
        present <- intersect(c("predator", "protected", "other"), unique(.role_of(sp_sci())))
        if (!length(present)) return()
        leaflet::addLegend(p, ctrl()$legend, colors = unname(.SPEX_ROLE[present]), labels = unname(.SPEX_ROLE_LABEL[present]),
          title = sprintf("%s &middot; by role", sp_lab()), opacity = 0.9)
        leaflet::addControl(p, position = ctrl()$legend, className = "ik-maps-shapekey",
          html = "&#9679; camera detection &nbsp; &#9632; trap catch<br/><small>size = amount; click for detail</small>")
      })

      observeEvent(input[[paste0(map_id, "_marker_click")]], {  # marker → MODAL for that location
        cid <- input[[paste0(map_id, "_marker_click")]]$id; if (is.null(cid)) return()
        .open_location_modal(if (startsWith(cid, "K|")) "trap" else "camera", sub(".*\\|", "", cid), get_selection())
      })

      list(detect_pts = detect_pts, catch_pts = catch_pts, detect_all = detect_all, catch_all = catch_all,
           combined = combined, sp_lab = sp_lab, mode = mode)
    }

    # ── pane A (always) + pane B (the comparison map, only in side-by-side) ───────────────────────
    two_pane <- reactive(input$display %in% c("sbs", "swipe"))   # side-by-side OR swipe ⇒ pane B is live
    # Per-setting LINKS: linked (or one-map) ⇒ pane B mirrors pane A for that setting; unlinked ⇒ B
    # reads its own comparison control. Period and Species link independently; reserve is always shared.
    .per_linked <- reactive(!isTRUE(two_pane()) || isTRUE(input$link_period))
    .spp_linked <- reactive(!isTRUE(two_pane()) || isTRUE(input$link_species))
    sel_b <- reactive({
      if (.per_linked()) return(selection())                    # period chained → mirror the left map
      tok <- input$compare_period
      per <- if (length(tok) && tok %in% c("prior", "last_year")) ik_prev_period(selection()$period, tok, ik_data) else tok
      if (is.null(per) || is.na(per) || !nzchar(per)) per <- selection()$period   # relative window doesn't resolve → mirror A
      s <- selection(); s$period <- per; s$season <- ik_expand_period(per, ik_data); s   # B = A's reserve, B's period
    })

    paneA <- make_pane("map", "a", active, selection, reactive(input$species), reactive(input$mode))
    # Pane B follows A's species MODE always; its species mirror A unless the Species chain is broken,
    # and its period mirrors A unless the Period chain is broken (sel_b handles the period).
    paneB <- make_pane("map_b", "b", reactive(active() && two_pane()), sel_b,
      reactive(if (.spp_linked()) input$species else input$species_b), reactive(input$mode))

    # Pane B's picker always carries choices; seed it from A the first time the Species chain is broken.
    .populate_pickers("_b")
    b_init <- reactiveVal(FALSE)
    observe({
      if (isTRUE(two_pane()) && !isTRUE(input$link_species) && !isTRUE(b_init())) {
        updateSelectInput(session, "species_b", selected = isolate(input$species))
        b_init(TRUE)
      }
    })

    # Resize the panes when the display mode changes (a conditionalPanel reveal doesn't fire a map
    # resize), and (re)wire pan/zoom linking. Both handled by spatial_explorer.js.
    observeEvent(list(input$display, input$link), {
      disp <- input$display %||% "single"
      session$sendCustomMessage("spex-sync", list(
        a = session$ns("map"), b = session$ns("map_b"), display = disp,
        twoPane = isTRUE(two_pane()), swipe = identical(disp, "swipe"),
        link = isTRUE(two_pane()) && (identical(disp, "swipe") || isTRUE(input$link))))  # swipe forces link
    }, ignoreNULL = FALSE)

    # ── records side (pane A) — a plain per-location list + CSV (the marker click opens a MODAL, not
    # this table). ───────────────────────────────────────────────────────────────────────────────────
    output$records_caption <- renderUI({
      d <- paneA$combined(); n <- if (is.null(d)) 0L else nrow(d)
      tags$small(sprintf("%s location%s with %s detections or catches. Click a map marker for its detail.",
                         format(n, big.mark = ","), if (n == 1L) "" else "s", paneA$sp_lab()))
    })

    output$table <- DT::renderDT({
      d <- paneA$combined()
      validate(need(!is.null(d) && nrow(d), "No detections or catches in the current selection."))
      df <- data.frame(Location = d$name, Reserve = d$reserve, Line = d$line,
        Detections = ifelse(is.na(d$rai), NA, round(d$rai, 2)),
        Catches = ifelse(is.na(d$captures), NA, as.integer(d$captures)),
        check.names = FALSE, stringsAsFactors = FALSE)
      names(df)[4] <- "Detections (RAI)"; names(df)[5] <- "Catches (n)"
      DT::datatable(df, rownames = FALSE, selection = "none", class = "stripe hover row-border",
        options = list(pageLength = 8, scrollX = TRUE, dom = "ftip"))
    })

    output$caption <- renderUI({
      d <- paneA$detect_pts(); cc <- paneA$catch_pts()
      nd <- if (is.null(d))  0L else sum(d$metric   > 0, na.rm = TRUE)
      nc <- if (is.null(cc)) 0L else sum(cc$captures > 0, na.rm = TRUE)
      tags$p(class = "ik-maps-meta", sprintf(
        "%s camera site%s detecting %s · %s trap%s catching it. Toggle layers top-right; click a marker for its detail.",
        format(nd, big.mark = ","), if (identical(nd, 1L)) "" else "s", paneA$sp_lab(),
        format(nc, big.mark = ","), if (identical(nc, 1L)) "" else "s"))
    })

    output$unplaced <- renderUI({
      du <- { m <- paneA$detect_all(); if (is.null(m)) 0L else sum(!(is.finite(m$latitude) & is.finite(m$longitude))) }
      cu <- { m <- paneA$catch_all();  if (is.null(m)) 0L else sum(!(is.finite(m$latitude) & is.finite(m$longitude))) }
      n <- du + cu; if (!n) return(NULL)
      div(class = "ik-maps-unplaced", icon("triangle-exclamation"),
          sprintf(" %s coordless site%s not shown on the map (no location fix).", format(n, big.mark = ","), if (n == 1L) "" else "s"))
    })

    output$download_csv <- downloadHandler(
      filename = function() sprintf("spatial-explorer-%s.csv", Sys.Date()),
      content = function(file) {
        d <- paneA$combined(); if (is.null(d)) d <- data.frame(); utils::write.csv(d, file, row.names = FALSE) })
  })
}
