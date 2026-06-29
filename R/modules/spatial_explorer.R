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
        tags$b("square"), " is a trap catch. ", tags$b("Size says how much"), "."),
      P(tags$b("Species mode"), " (sidebar) reshapes what a marker means: ", tags$b("Combined"),
        " sums the picked species within each role → one Predators / Protected / Other layer; ", tags$b("Per species"),
        " gives each picked species its own toggleable layer (role-coloured, with its silhouette); ",
        tags$b("Predator vs protected"), " draws the camera ", tags$b("pressure surface"),
        " (predators high × protected low) with the predator's catches over it."),
      P(tags$b("Display"), " switches between one map and two ", tags$b("side by side"), ". Break a ",
        tags$b("chain"), " (on Data period or Species) to compare a different period — ", tags$b("prior period"),
        ", ", tags$b("same period last year"), ", or any other — or a different species on the right map; ",
        tags$b("Link views"), " pans/zooms them together. Reserve is always shared."),
      P(tags$b("Hint:"), " select a single ", tags$b("Reserve"), " and zoom in — the markers are most readable at reserve scale.")),
    tabPanel(
      "The layers", icon = icon("layer-group"),
      P(tags$br(), "Toggle these top-right. A layer holds both camera detections (discs) and trap catches (squares)."),
      tags$ul(
        tags$li(tags$b(tags$span(style = "color:#d62728", "Predators")), " / ",
                tags$b(tags$span(style = "color:#2e9e3f", "Protected")), " / ", tags$b("Other"),
                " — the picked species by role (Combined), or one layer per species (Per species)."),
        tags$li(tags$b("Cameras / Traps"), " — the device field (every active camera / trap), faint context dots. Off by default."),
        tags$li(tags$b("Boundary"), " — the monitored footprint (convex hull of the devices) per reserve; hover for its name."))),
    tabPanel(
      "How it's calculated", icon = icon("calculator"),
      tags$ul(
        tags$br(),
        tags$li(tags$b("Detections (RAI)"), " — detections of the picked species ÷ that camera's camera-hours × ",
                ch, " (the per-camera scale from config), net of likely duplicates. Combined = all picked species summed."),
        tags$li(tags$b("Catches"), " — captures of the picked species at each trap in the period (raw count). Combined = all picked species summed."),
        tags$li(tags$b("Predator pressure"), " — each camera's predator RAI × (1 − protected RAI), both scaled to the busiest camera; interpolated (IDW) into the surface."),
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
      conditionalPanel("input.display == 'sbs'", ns = ns,
        tags$span(class = "ik-spex-link", title = "Link or unlink this setting across the two maps",
          checkboxInput(ns(link_id), label = tagList(
            tags$span(class = "ik-spex-linked",   icon("link")),
            tags$span(class = "ik-spex-unlinked", icon("link-slash"))), value = TRUE)))),
    primary,
    conditionalPanel(sprintf("input.display == 'sbs' && input.%s == false", link_id), ns = ns,
      div(class = "ik-spex-compare-field",
        div(class = "ik-spex-compare-tag", icon("clone"), " Comparison map"),
        comparison)))

  # the species picker(s) for one pane — Species OR Predator/Protected, keyed off the shared MODE.
  species_pickers <- function(sfx = "") tagList(
    conditionalPanel("input.mode != 'pvp'", ns = ns,
      selectInput(ns(paste0("species", sfx)), NULL, choices = NULL, multiple = TRUE)),
    conditionalPanel("input.mode == 'pvp'", ns = ns,
      selectInput(ns(paste0("pred", sfx)), "Predator(s)", choices = NULL, multiple = TRUE),
      selectInput(ns(paste0("prot", sfx)), "Protected",   choices = NULL, multiple = TRUE)))
  per_input <- function(sel_ns) selectInput(sel_ns("period"), NULL, choices = per_choices, selected = per_default)

  div(class = "ik-selection ik-view-controls ik-spex-controls",
    tags$div(class = "ik-view-controls-h", "Map options"),
    radioButtons(ns("display"), NULL, inline = TRUE,
      choiceNames  = list(tagList(icon("map"), " One map"), tagList(icon("table-columns"), " Side by side")),
      choiceValues = c("single", "sbs"), selected = "single"),
    conditionalPanel("input.display == 'sbs'", ns = ns,
      checkboxInput(ns("link"), tagList(icon("link"), " Link views (pan/zoom together)"), value = TRUE)),
    radioButtons(ns("mode"), "Species mode",
      choices = c("Combined" = "combined", "Per species" = "separate", "Predator vs protected" = "pvp"), selected = "combined"),
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
          conditionalPanel("input.display == 'sbs'", ns = ns,
            div(class = "ik-spex-compare-pane", .map_panel("map_b"))),
          conditionalPanel("input.display != 'sbs'", ns = ns,
            div(class = "ik-spex-side",
              div(class = "ik-maps-records",
                  uiOutput(ns("drill_chip")),
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
    # Default the picker to the predator-role groups — a "pest pressure vs control" front door.
    .pred_groups <- unique(.sg$label[!is.na(.sg$role) & .sg$role == "predator"])
    .sp_default  <- if (length(.pred_groups)) paste0("grp:", .pred_groups) else "__all__"
    .role_taxa <- function(role) { l <- unique(.sg$label[!is.na(.sg$role) & .sg$role == role])
      stats::setNames(lapply(l, function(x) .sg$scientificName[.sg$label == x & !is.na(.sg$scientificName)]), l) }
    pred_taxa <- .role_taxa("predator"); prot_taxa <- .role_taxa("protected")
    .splits   <- unique(.sg$label[which(.sg$split)])
    .pred_def <- if (length(pred_taxa)) paste0("grp:", names(pred_taxa)[1]) else NULL
    .prot_def <- if (length(prot_taxa)) paste0("grp:", names(prot_taxa)[1]) else NULL
    # The conservation ROLE of each scientificName (predator / protected / other) — drives marker colour.
    .role_of <- function(sci) { r <- .sg$role[match(sci, .sg$scientificName)]
      r[is.na(r) | !(r %in% c("predator", "protected"))] <- "other"; r }
    # The icon glyph KEY for a picked value (group → its group key; species → its group's key, else paw).
    .glyph_key <- function(v) {
      if (startsWith(v, "grp:")) (.sg$group[match(sub("^grp:", "", v), .sg$label)] %||% "other")
      else if (startsWith(v, "sci:")) (.sg$group[match(sub("^sci:", "", v), .sg$scientificName)] %||% "other")
      else "other"
    }

    # Populate a pane's pickers (species + pred/prot), keeping any existing pick. `sfx` = "" (pane A)
    # or "_b" (comparison). Pane B is seeded from A's current picks the first time it's populated.
    .populate_pickers <- function(sfx = "", species_default = .sp_default) {
      observe({ p <- prefer()
        sel <- isolate(input[[paste0("species", sfx)]]); if (!length(sel)) sel <- species_default
        updateSelectInput(session, paste0("species", sfx),
          choices  = ik_species_choices_full(ik_data, p, all_label = "All species", all_value = "__all__"), selected = sel)
      })
      observe({ p <- prefer(); keep <- function(cur, def) if (length(cur) && all(nzchar(cur))) cur else def
        updateSelectInput(session, paste0("pred", sfx), choices = ik_species_choices(pred_taxa, ik_data, p, .splits), selected = keep(isolate(input[[paste0("pred", sfx)]]), .pred_def))
        updateSelectInput(session, paste0("prot", sfx), choices = ik_species_choices(prot_taxa, ik_data, p, .splits), selected = keep(isolate(input[[paste0("prot", sfx)]]), .prot_def))
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

    # ── one PANE = the whole draw pipeline for a single leaflet output, parameterised by its input
    # getters (so panes A and B share every observer). Returns a handle of its data reactives so the
    # records side can read pane A. ───────────────────────────────────────────────────────────────
    make_pane <- function(map_id, pane_active, get_selection, get_species, get_mode, get_pred, get_prot) {
      proxy <- function() leaflet::leafletProxy(map_id, session)

      sp_sci <- reactive({ v <- get_species(); if (!length(v) || "__all__" %in% v) all_sci else ik_resolve_species_choice(v, grp_taxa) })
      sp_lab <- reactive({ v <- get_species(); if (!length(v) || "__all__" %in% v) "all species" else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })
      mode   <- reactive({ m <- get_mode(); if (length(m) && m %in% c("separate", "pvp")) m else "combined" })
      pred_sci <- reactive(ik_resolve_species_choice(get_pred(), pred_taxa))
      prot_sci <- reactive(ik_resolve_species_choice(get_prot(), prot_taxa))
      pred_lab <- reactive({ v <- get_pred(); if (!length(v)) "predator"  else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })
      prot_lab <- reactive({ v <- get_prot(); if (!length(v)) "protected" else paste(ik_choice_labels(v, ik_data, prefer()), collapse = " + ") })

      prio_all  <- reactive({ req(pane_active()); ik_priority_metric(ik_data, get_selection(), pred_sci(), prot_sci(), norm = per_cam) })
      prio_pts  <- reactive({ m <- prio_all(); if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
      trap_pred <- reactive({ req(pane_active()); if (!length(pred_sci())) return(NULL)
        m <- ik_location_metric(ik_data, get_selection(), list(P = pred_sci()), "trap")
        if (is.null(m)) NULL else m[is.finite(m$latitude) & is.finite(m$longitude), , drop = FALSE] })
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

      selected <- reactiveVal(NULL)   # NULL | list(id, label) — a clicked marker's location
      observeEvent(list(get_species(), get_pred(), get_prot(), get_mode(), get_selection()), selected(NULL), ignoreInit = TRUE)

      output[[map_id]] <- leaflet::renderLeaflet({
        locs <- ik_data$app$geography$locations
        locs <- locs[is.finite(locs$latitude) & is.finite(locs$longitude), , drop = FALSE]
        ik_map_base(panes = c("surface", "boundary", "cameras", "traps", "detections", "catches", "selected"),
          overlay_groups = c("Cameras", "Traps", "Boundary"),   # role/species value layers are added by the dynamic control
          is_dark = isolate(is_dark()), fit = locs, hide_groups = c("Cameras", "Traps"), pane_z0 = 410)
      })
      outputOptions(output, map_id, suspendWhenHidden = FALSE)

      observeEvent(color_mode(), ik_swap_theme_tiles(proxy(), is_dark()), ignoreInit = TRUE)

      observe({                                                # re-frame to the data extent
        pts <- rbind(
          { d <- detect_pts(); if (is.null(d)) NULL else d[, c("longitude", "latitude")] },
          { c <- catch_pts();  if (is.null(c)) NULL else c[, c("longitude", "latitude")] },
          { d <- prio_pts();   if (is.null(d)) NULL else d[, c("longitude", "latitude")] })
        if (is.null(pts) || !nrow(pts)) return(); p <- proxy()
        if (nrow(pts) == 1 || (diff(range(pts$longitude)) == 0 && diff(range(pts$latitude)) == 0))
          leaflet::setView(p, pts$longitude[1], pts$latitude[1], zoom = 14)
        else leaflet::fitBounds(p, min(pts$longitude), min(pts$latitude), max(pts$longitude), max(pts$latitude),
          options = list(padding = c(30, 30)))
      })

      # Cameras / Traps — the faint device field (off by default); camera context clears in P-v-P.
      observe(ik_draw_device_layer(proxy(), if (identical(mode(), "pvp")) NULL else .dev_context("camera"),
        fill_color = .SPEX_DETECT, group = "Cameras", pane = "cameras", radius = 3, fill_opacity = 0.35, label = ~name))
      observe(ik_draw_device_layer(proxy(), .dev_context("trap"), fill_color = .SPEX_CATCH,
        group = "Traps", pane = "traps", radius = 3, fill_opacity = 0.35, label = ~name))

      observe({                                                # P-v-P only: the predator's trap catches (amber "Control")
        d <- if (identical(mode(), "pvp")) trap_pred() else NULL
        d <- if (is.null(d)) NULL else d[is.finite(d$captures) & d$captures > 0, , drop = FALSE]
        ik_draw_metric_markers(proxy(), d, value = d$captures, group = "Catches",
          layerId = paste0("K|", d$location_id), lo = 3, hi = 12, cap = ik_robust_cap(d$captures, 0.9),
          fill_color = .SPEX_CATCH, fill_opacity = 0.8, color = "#ffffff", weight = 1, pane = "catches",
          label = sprintf("%s — %s caught %d", d$name, pred_lab(), as.integer(d$captures)))
      })

      # One toggleable LAYER per group — Combined groups by ROLE (Predators/Protected/Other), Per-species
      # groups by picked ENTRY. Each layer = that group's camera detections (disc) + trap catches (square),
      # coloured by its ROLE, the species silhouette knocked out (Per-species) or blank (Combined).
      drawn_groups <- reactiveVal(character(0))
      .draw_group_layer <- function(p, e) {
        grp <- e$label
        det <- if (is.null(e$det))   NULL else e$det[is.finite(e$det$latitude) & is.finite(e$det$longitude) & is.finite(e$det$metric) & e$det$metric > 0, , drop = FALSE]
        cat <- if (is.null(e$catch)) NULL else e$catch[is.finite(e$catch$latitude) & is.finite(e$catch$longitude) & is.finite(e$catch$captures) & e$catch$captures > 0, , drop = FALSE]
        if (!is.null(det) && nrow(det)) {
          sz <- round(2 * ik_marker_radius(det$metric, 6, 18, cap_pctl = 0.98) + 6)
          leaflet::addMarkers(p, lng = det$longitude, lat = det$latitude, group = grp,
            icon = ik_species_marker_icon(rep(e$key, nrow(det)), e$colour, "circle", sz),
            layerId = paste0("C|", grp, "|", det$location_id), label = sprintf("%s — %s detections, RAI %.2f", det$name, grp, det$metric))
        }
        if (!is.null(cat) && nrow(cat)) {
          sz <- round(2 * ik_marker_radius(cat$captures, 5, 15, cap = ik_robust_cap(cat$captures, 0.9)) + 6)
          leaflet::addMarkers(p, lng = cat$longitude, lat = cat$latitude, group = grp,
            icon = ik_species_marker_icon(rep(e$key, nrow(cat)), e$colour, "square", sz),
            layerId = paste0("K|", grp, "|", cat$location_id), label = sprintf("%s — %s caught %d", cat$name, grp, as.integer(cat$captures)))
        }
        grp
      }
      # the groups to draw for the current mode: by ROLE (Combined) or by ENTRY (Per-species).
      draw_groups <- reactive({
        sci_metric <- function(sci) list(
          det   = ik_location_metric(ik_data, get_selection(), stats::setNames(list(sci), "x"), "camera", norm = per_cam),
          catch = ik_location_metric(ik_data, get_selection(), stats::setNames(list(sci), "x"), "trap"))
        if (identical(mode(), "combined")) {
          sci <- sp_sci(); if (!length(sci)) return(list()); roles <- .role_of(sci)
          Filter(Negate(is.null), lapply(c("predator", "protected", "other"), function(r) {
            s <- sci[roles == r]; if (!length(s)) return(NULL)
            c(list(label = unname(.SPEX_ROLE_LABEL[r]), key = "", colour = unname(.SPEX_ROLE[r])), sci_metric(s))
          }))
        } else if (identical(mode(), "separate")) {
          Filter(Negate(is.null), lapply(entries(), function(v) {
            taxa <- ik_choice_taxa(v, grp_taxa, ik_data, isolate(prefer())); if (is.null(taxa)) return(NULL)
            sci  <- unlist(taxa, use.names = FALSE)
            role <- names(sort(table(.role_of(sci)), decreasing = TRUE))[1] %||% "other"   # the entry's dominant role
            c(list(label = names(taxa)[1], key = .glyph_key(v), colour = unname(.SPEX_ROLE[role])), sci_metric(sci))
          }))
        } else list()
      })
      observe({
        p <- proxy()
        for (g in drawn_groups()) leaflet::clearGroup(p, g)
        if (mode() == "pvp") { drawn_groups(character(0)); return() }
        req(pane_active())
        drawn_groups(unique(vapply(draw_groups(), function(e) .draw_group_layer(p, e), character(1))))
      })

      # P-v-P: pressure surface (IDW of priority) + per-camera priority markers.
      surface_idw <- reactive({ if (!identical(mode(), "pvp")) return(NULL)
        d <- prio_pts(); if (is.null(d) || !nrow(d)) return(NULL)
        d <- d[!d$reserve %in% c(GEO_UNPLACED_RESERVE, GEO_OUTSIDE_RESERVE), , drop = FALSE]
        if (!nrow(d)) return(NULL)
        ik_idw_surface(d, "metric", "reserve") })
      observe({
        s <- if (identical(mode(), "pvp")) ik_clip_surface_to_reserves(surface_idw(), reserve_hulls()) else NULL
        ik_draw_idw_surface(proxy(), s, group = "Pressure surface", pal = .PP_RAMP, is_dark = is_dark())
      })
      observe({
        d <- if (identical(mode(), "pvp")) prio_pts() else NULL
        d <- if (is.null(d)) NULL else d[is.finite(d$metric) & d$metric > 0, , drop = FALSE]
        ik_draw_metric_markers(proxy(), d, value = d$metric, group = "Camera pressure",
          layerId = paste0("C|", d$location_id), lo = 5, hi = 18, cap_pctl = 0.98, pal = .PP_RAMP,
          fill_opacity = 0.85, color = if (is_dark()) "#1a1a1a" else "#ffffff", weight = 1.4, pane = "detections",
          label = sprintf("%s — pressure %.2f (%s %.2f vs %s %.2f)", d$name, d$metric, pred_lab(), d$predator, prot_lab(), d$protected))
      })

      observe({                                                # dynamic layers control (overlay set per mode)
        p <- proxy()
        overlays <- if (identical(mode(), "pvp")) c("Pressure surface", "Camera pressure", "Catches", "Traps", "Boundary")
                    else c(drawn_groups(), "Cameras", "Traps", "Boundary")   # role (Combined) or species (Per-species) layers
        leaflet::addLayersControl(p, baseGroups = c("Map", "Satellite"), overlayGroups = overlays,
          options = leaflet::layersControlOptions(collapsed = FALSE))
        shown <- isolate(input[[paste0(map_id, "_groups")]])
        for (g in c("Cameras", "Traps")) if (is.null(shown) || !(g %in% shown)) leaflet::hideGroup(p, g)
      })

      observe({ p <- proxy(); leaflet::clearGroup(p, "Boundary")  # Boundary
        ik_add_reserve_boundary(p, reserve_hulls(), color = if (is_dark()) "#cfd8dc" else "#37474f") })

      observe({ sel <- selected(); locs <- ik_data$app$geography$locations   # selection ring
        i <- if (is.null(sel)) NA_integer_ else match(sel$id, locs$location_id)
        ik_draw_selection_ring(proxy(), if (is.na(i)) NULL else locs$longitude[i], if (is.na(i)) NULL else locs$latitude[i]) })

      observe({                                                # legend — mode-aware
        p <- proxy(); leaflet::clearControls(p)
        if (identical(mode(), "pvp")) {
          d <- prio_pts(); if (is.null(d) || !nrow(d)) return()
          cap <- ik_max_cap(d$metric); pf <- leaflet::colorNumeric(.PP_RAMP, c(0, cap))
          ik_draw_metric_legend(p, pf, pmin(d$metric, cap), sprintf("Predator pressure &middot; %s high, %s low", pred_lab(), prot_lab()))
          leaflet::addLegend(p, "bottomleft", colors = .SPEX_CATCH, labels = sprintf("%s caught (traps)", pred_lab()), title = "Control", opacity = 0.9)
          return()
        }
        sci <- sp_sci()
        present <- intersect(c("predator", "protected", "other"), unique(.role_of(sci)))
        if (!length(present)) return()
        leaflet::addLegend(p, "bottomright", colors = unname(.SPEX_ROLE[present]), labels = unname(.SPEX_ROLE_LABEL[present]),
          title = sprintf("%s &middot; by role", sp_lab()), opacity = 0.9)
        leaflet::addControl(p, position = "bottomright", className = "ik-maps-shapekey",
          html = "&#9679; camera detection &nbsp; &#9632; trap catch<br/><small>size = amount</small>")
      })

      observeEvent(input[[paste0(map_id, "_marker_click")]], {  # marker → filter table to that location
        cid <- input[[paste0(map_id, "_marker_click")]]$id; if (is.null(cid)) return()
        loc <- sub(".*\\|", "", cid); locs <- ik_data$app$geography$locations
        selected(list(id = loc, label = locs$name[match(loc, locs$location_id)] %||% loc))
      })

      list(detect_pts = detect_pts, catch_pts = catch_pts, prio_pts = prio_pts, prio_all = prio_all,
           trap_pred = trap_pred, detect_all = detect_all, catch_all = catch_all, combined = combined,
           selected = selected, sp_lab = sp_lab, pred_lab = pred_lab, prot_lab = prot_lab, mode = mode)
    }

    # ── pane A (always) + pane B (the comparison map, only in side-by-side) ───────────────────────
    sbs <- reactive(identical(input$display, "sbs"))
    # Per-setting LINKS: linked (or one-map) ⇒ pane B mirrors pane A for that setting; unlinked ⇒ B
    # reads its own comparison control. Period and Species link independently; reserve is always shared.
    .per_linked <- reactive(!isTRUE(sbs()) || isTRUE(input$link_period))
    .spp_linked <- reactive(!isTRUE(sbs()) || isTRUE(input$link_species))
    sel_b <- reactive({
      if (.per_linked()) return(selection())                    # period chained → mirror the left map
      tok <- input$compare_period
      per <- if (length(tok) && tok %in% c("prior", "last_year")) ik_prev_period(selection()$period, tok, ik_data) else tok
      if (is.null(per) || is.na(per) || !nzchar(per)) per <- selection()$period   # relative window doesn't resolve → mirror A
      s <- selection(); s$period <- per; s$season <- ik_expand_period(per, ik_data); s   # B = A's reserve, B's period
    })

    paneA <- make_pane("map", active, selection,
      reactive(input$species), reactive(input$mode), reactive(input$pred), reactive(input$prot))
    # Pane B follows A's species MODE always; its species mirror A unless the Species chain is broken,
    # and its period mirrors A unless the Period chain is broken (sel_b handles the period).
    paneB <- make_pane("map_b", reactive(active() && sbs()), sel_b,
      reactive(if (.spp_linked()) input$species else input$species_b),
      reactive(input$mode),
      reactive(if (.spp_linked()) input$pred else input$pred_b),
      reactive(if (.spp_linked()) input$prot else input$prot_b))

    # Pane B's pickers always carry choices; seed them from A the first time the Species chain is broken.
    .populate_pickers("_b")
    b_init <- reactiveVal(FALSE)
    observe({
      if (isTRUE(sbs()) && !isTRUE(input$link_species) && !isTRUE(b_init())) {
        updateSelectInput(session, "species_b", selected = isolate(input$species))
        updateSelectInput(session, "pred_b",    selected = isolate(input$pred))
        updateSelectInput(session, "prot_b",    selected = isolate(input$prot))
        b_init(TRUE)
      }
    })

    # Resize the panes when the display mode changes (a conditionalPanel reveal doesn't fire a map
    # resize), and (re)wire pan/zoom linking. Both handled by spatial_explorer.js.
    observeEvent(list(input$display, input$link), {
      session$sendCustomMessage("spex-sync", list(
        a = session$ns("map"), b = session$ns("map_b"),
        sbs = isTRUE(sbs()), link = isTRUE(sbs()) && isTRUE(input$link)))
    }, ignoreNULL = FALSE)

    # ── records side (pane A) ─────────────────────────────────────────────────────────────────────
    output$drill_chip <- renderUI({
      sel <- paneA$selected(); if (is.null(sel)) return(NULL)
      div(class = "ik-maps-drill", icon("location-dot"), sprintf(" Showing: %s ", sel$label),
          actionLink(session$ns("drill_clear"), "show all", class = "ik-maps-drill-clear"))
    })
    observeEvent(input$drill_clear, paneA$selected(NULL))

    output$records_caption <- renderUI({
      if (identical(paneA$mode(), "pvp")) { d <- paneA$prio_pts(); n <- if (is.null(d)) 0L else nrow(d)
        return(tags$small(sprintf("%s camera site%s · pressure = %s high & %s low (exploratory). Click a marker to filter.",
          format(n, big.mark = ","), if (n == 1L) "" else "s", paneA$pred_lab(), paneA$prot_lab())))
      }
      d <- paneA$combined(); n <- if (is.null(d)) 0L else nrow(d)
      tags$small(sprintf("%s location%s with %s detections or catches. Click a marker to filter.",
                         format(n, big.mark = ","), if (n == 1L) "" else "s", paneA$sp_lab()))
    })

    output$table <- DT::renderDT({
      sel <- paneA$selected()
      if (identical(paneA$mode(), "pvp")) {
        d <- paneA$prio_pts(); if (!is.null(sel)) d <- d[d$location_id == sel$id, , drop = FALSE]
        validate(need(!is.null(d) && nrow(d), "Pick a predator and a protected species to compare."))
        d <- d[order(-d$metric), , drop = FALSE]
        df <- data.frame(Camera = d$name, Reserve = d$reserve, Line = d$line,
          predator = round(d$predator, 2), protected = round(d$protected, 2), Pressure = round(d$metric, 2),
          check.names = FALSE, stringsAsFactors = FALSE)
        names(df)[4:5] <- c(paste(paneA$pred_lab(), "RAI"), paste(paneA$prot_lab(), "RAI"))
        return(DT::datatable(df, rownames = FALSE, selection = "none", class = "stripe hover row-border",
          options = list(pageLength = 8, scrollX = TRUE, dom = "ftip", order = list(list(5, "desc")))))
      }
      d <- paneA$combined()
      if (!is.null(sel)) d <- d[d$location_id == sel$id, , drop = FALSE]
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
      if (identical(paneA$mode(), "pvp")) { d <- paneA$prio_pts()
        nh <- if (is.null(d)) 0L else sum(d$metric > 0, na.rm = TRUE)
        ca <- paneA$trap_pred(); nc <- if (is.null(ca)) 0L else sum(ca$captures > 0, na.rm = TRUE)
        return(tags$p(class = "ik-maps-meta", sprintf(
          "%s camera site%s under pressure from %s · %s trap%s caught it. Toggle layers top-right; click a marker for detail.",
          format(nh, big.mark = ","), if (identical(nh, 1L)) "" else "s", paneA$pred_lab(),
          format(nc, big.mark = ","), if (identical(nc, 1L)) "" else "s")))
      }
      d <- paneA$detect_pts(); cc <- paneA$catch_pts()
      nd <- if (is.null(d))  0L else sum(d$metric   > 0, na.rm = TRUE)
      nc <- if (is.null(cc)) 0L else sum(cc$captures > 0, na.rm = TRUE)
      tags$p(class = "ik-maps-meta", sprintf(
        "%s camera site%s detecting %s · %s trap%s catching it. Toggle layers top-right; click a marker to filter the table.",
        format(nd, big.mark = ","), if (identical(nd, 1L)) "" else "s", paneA$sp_lab(),
        format(nc, big.mark = ","), if (identical(nc, 1L)) "" else "s"))
    })

    output$unplaced <- renderUI({
      n <- if (identical(paneA$mode(), "pvp")) { m <- paneA$prio_all(); if (is.null(m)) 0L else sum(!(is.finite(m$latitude) & is.finite(m$longitude))) }
        else { du <- { m <- paneA$detect_all(); if (is.null(m)) 0L else sum(!(is.finite(m$latitude) & is.finite(m$longitude))) }
               cu <- { m <- paneA$catch_all();  if (is.null(m)) 0L else sum(!(is.finite(m$latitude) & is.finite(m$longitude))) }
               du + cu }
      if (!n) return(NULL)
      div(class = "ik-maps-unplaced", icon("triangle-exclamation"),
          sprintf(" %s coordless site%s not shown on the map (no location fix).", format(n, big.mark = ","), if (n == 1L) "" else "s"))
    })

    output$download_csv <- downloadHandler(
      filename = function() sprintf("spatial-explorer-%s.csv", Sys.Date()),
      content = function(file) {
        d <- if (identical(paneA$mode(), "pvp")) paneA$prio_pts() else paneA$combined()
        if (is.null(d)) d <- data.frame(); utils::write.csv(d, file, row.names = FALSE) })
  })
}
