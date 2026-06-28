# map_draw.R — the shared DRAW tier for every Leaflet map view.
#
# These helpers are the view-layer twin of spatial.R (geometry) and metrics.R (per-location
# values): each takes a leaflet map OR a leafletProxy as its first argument and RETURNS it (the
# same contract as ik_add_reserve_boundary in spatial.R), so it composes both inside a base
# renderLeaflet() and inside a proxy observer. They own a leaflet GROUP — clearGroup + redraw —
# but NOTHING above the draw layer: no drill state, no modals, no species pickers, no composite
# maths. Those stay bespoke in each module.
#
# Extracted (byte-for-byte) from the inline draws that were duplicated across maps.R,
# predator_pressure.R, coverage.R, cooccurrence.R, overview.R, trapping.R, neighbourhood.R and
# trap_hero.R — see the audit. Style differences (palette, opacity, halo, pane, lo/hi, cap mode,
# label/popup) are PARAMETERS with defaults set to the most common case; pass the variant in.
#
# Caching note: callers still compute/cache the heavy inputs (ik_location_metric, ik_idw_surface,
# ik_clip_surface_to_reserves) — these helpers only DRAW, so caching stays per-module.

#' The colour-scale domain top: the TRUE max of the positive values (never below it, never zero),
#' so a legend reads the real maximum. This is the file-local `.robust_cap` that maps.R and
#' predator_pressure.R each defined privately — promoted here so the draw helpers and the modules
#' share ONE definition. Distinct from `ik_robust_cap()` (a PERCENTILE clamp, spatial.R) used for
#' marker SIZE. @keywords internal
ik_max_cap <- function(v) {
  m <- suppressWarnings(max(v, na.rm = TRUE))
  if (is.finite(m) && m > 0) m else 1
}

#' Build the standard map base: a canvas-rendered leaflet with CartoDB (theme-aware) + Esri
#' satellite tiles, a stack of custom panes (z-ordered by their position in `panes`), a layers
#' control over the caller's `overlay_groups`, optionally some groups hidden, and an optional
#' initial fit. `overlay_groups` is CALLER-SUPPLIED (the fix for maps_server's baked group list —
#' a composite map can declare its own Cameras/Detections/Traps/Catches/Boundary set).
#'
#' @param panes Character vector of pane names, lowest z first (e.g. c("surface","boundary",
#'   "device","points","selected")). Each gets zIndex `pane_z0 + 10*position`.
#' @param overlay_groups Toggleable layer-group names for the layers control.
#' @param base_groups Base-tile group names (default Map/Satellite — must match the tile groups).
#' @param is_dark Theme; picks DarkMatter vs Positron for the "Map" base.
#' @param fit Optional data frame with `longitude`/`latitude` columns — fitBounds to its extent.
#' @param hide_groups Overlay groups to hide initially (opt-in layers).
#' @param collapsed Layers-control collapsed state (default FALSE — shown open).
#' @param pane_z0 Base zIndex for the pane stack (maps.R uses 400, predator_pressure.R 410).
#' @param map_group Group name for the themed CartoDB base tile (default "Map"; co-occurrence
#'   names its street base "Street" — must match the `ik_swap_theme_tiles(group=)` it later calls).
#' @return the leaflet map. @keywords internal
ik_map_base <- function(panes, overlay_groups, base_groups = c("Map", "Satellite"),
                        is_dark = FALSE, fit = NULL, hide_groups = NULL,
                        collapsed = FALSE, pane_z0 = 400, map_group = "Map") {
  canvas <- if (isTRUE(is_dark)) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron
  m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  m <- leaflet::addProviderTiles(m, canvas, group = map_group)
  m <- leaflet::addProviderTiles(m, leaflet::providers$Esri.WorldImagery, group = "Satellite")
  for (pn in panes) m <- leaflet::addMapPane(m, pn, zIndex = pane_z0 + 10 * match(pn, panes))
  m <- leaflet::addLayersControl(m, baseGroups = base_groups, overlayGroups = overlay_groups,
    options = leaflet::layersControlOptions(collapsed = collapsed))
  for (g in hide_groups) m <- leaflet::hideGroup(m, g)
  if (!is.null(fit) && nrow(fit))
    m <- leaflet::fitBounds(m, min(fit$longitude), min(fit$latitude), max(fit$longitude), max(fit$latitude))
  m
}

#' Swap the "Map" base tiles to match the colour theme — the body of every map's
#' `observeEvent(color_mode())`. clearGroup + re-add the themed CartoDB tiles. @keywords internal
ik_swap_theme_tiles <- function(proxy, is_dark, group = "Map") {
  leaflet::clearGroup(proxy, group)
  leaflet::addProviderTiles(proxy,
    if (isTRUE(is_dark)) leaflet::providers$CartoDB.DarkMatter else leaflet::providers$CartoDB.Positron,
    group = group)
}

#' Draw an IDW prediction surface (already computed + clipped by the caller) as colour-graded
#' polygons. Colour domain is c(0, cap); `cap` defaults to the true max (ik_max_cap) so the legend
#' matches. `pal` is a colorNumeric palette name or colour vector (viridis on the rate maps,
#' .PP_RAMP on predator pressure). Theme drives the fill opacity. @keywords internal
ik_draw_idw_surface <- function(proxy, surface, group = "Surface", pal = "viridis",
                                cap = NULL, is_dark = FALSE, pane = "surface") {
  leaflet::clearGroup(proxy, group)
  if (is.null(surface) || !nrow(surface)) return(invisible(proxy))
  if (is.null(cap)) cap <- ik_max_cap(surface$predicted)
  pf <- leaflet::colorNumeric(pal, c(0, cap))
  leaflet::addPolygons(proxy, data = surface, group = group, weight = 0.5,
    color = ~pf(pmin(predicted, cap)), fillColor = ~pf(pmin(predicted, cap)),
    fillOpacity = if (isTRUE(is_dark)) 0.5 else 0.4,
    options = leaflet::pathOptions(pane = pane))
}

#' Draw value-sized circle markers (the per-location metric layer). Size comes from
#' `ik_marker_radius(value, lo, hi, cap_pctl=, cap=)` — the single sizing home. Colour is EITHER a
#' value ramp (pass `pal` → colorNumeric over c(0, colour_cap), default true max) OR a fixed
#' `fill_color` (pass `pal = NULL`, e.g. purple catches). Size comes from `ik_marker_radius` unless
#' a precomputed `radius` vector is supplied (the escape hatch for a map with bespoke sizing, e.g.
#' trap_hero's linear rank scale). Pass-through `label`/`popup` are built by the caller. clearGroup
#' + redraw. @keywords internal
ik_draw_metric_markers <- function(proxy, data, value, group, layerId = NULL,
                                   lo = 5, hi = 20, cap_pctl = NULL, cap = NULL, radius = NULL,
                                   pal = NULL, colour_cap = NULL, fill_color = NULL,
                                   fill_opacity = 0.85, stroke = TRUE, color = "#ffffff", weight = 1.5,
                                   pane = "points", lng = ~longitude, lat = ~latitude,
                                   label = NULL, label_options = NULL, popup = NULL, popup_autopan = FALSE) {
  leaflet::clearGroup(proxy, group)
  if (is.null(data) || !nrow(data)) return(invisible(proxy))
  if (!is.null(pal)) {                                     # colour by value
    if (is.null(colour_cap)) colour_cap <- ik_max_cap(value)
    pf <- leaflet::colorNumeric(pal, c(0, colour_cap))
    v  <- pmin(value, colour_cap)
    fc <- pf(v)
  } else {                                                 # fixed colour
    v  <- value
    fc <- fill_color
  }
  rad <- if (!is.null(radius)) radius else ik_marker_radius(v, lo, hi, cap_pctl = cap_pctl, cap = cap)
  args <- list(proxy, data = data, lng = lng, lat = lat, group = group,
    radius = rad, fillColor = fc, fillOpacity = fill_opacity, stroke = stroke,
    color = color, weight = weight, options = leaflet::pathOptions(pane = pane))
  if (!is.null(layerId))       args$layerId      <- layerId
  if (!is.null(label))         args$label        <- label
  if (!is.null(label_options)) args$labelOptions <- label_options
  if (!is.null(popup)) {
    args$popup        <- popup
    args$popupOptions <- leaflet::popupOptions(autoPan = popup_autopan)
  }
  do.call(leaflet::addCircleMarkers, args)
}

#' Draw the bottom CONTEXT field of fixed-radius device dots (every camera/trap location). Stroke
#' off, small radius. `fill_color` may be a scalar (blue camera / grey trap) or a vector (e.g. trap
#' status). Optional `layerId`/`label`. clearGroup + redraw. @keywords internal
ik_draw_device_layer <- function(proxy, data, fill_color, group, pane = "device",
                                 radius = 3, fill_opacity = 0.55, lng = ~longitude, lat = ~latitude,
                                 label = NULL, layerId = NULL, popup = NULL) {
  leaflet::clearGroup(proxy, group)
  if (is.null(data) || !nrow(data)) return(invisible(proxy))
  args <- list(proxy, data = data, lng = lng, lat = lat, group = group,
    radius = radius, fill = TRUE, fillColor = fill_color, fillOpacity = fill_opacity, stroke = FALSE,
    options = leaflet::pathOptions(pane = pane))
  if (!is.null(layerId)) args$layerId <- layerId
  if (!is.null(label))   args$label   <- label
  if (!is.null(popup))   args$popup   <- popup
  do.call(leaflet::addCircleMarkers, args)
}

#' Draw the stroke-only highlight ring at a drilled location (the orange "Selected" ring; also the
#' hover/highlight rings on coverage/neighbourhood with a colour/radius arg). clearGroup + guard +
#' draw. @keywords internal
ik_draw_selection_ring <- function(proxy, lng, lat, group = "Selected", radius = 16,
                                   color = "#ff5722", weight = 3, opacity = 0.95, pane = "selected") {
  leaflet::clearGroup(proxy, group)
  if (is.null(lng) || is.null(lat) || !is.finite(lng) || !is.finite(lat)) return(invisible(proxy))
  leaflet::addCircleMarkers(proxy, lng = lng, lat = lat, group = group, radius = radius,
    fill = FALSE, stroke = TRUE, color = color, weight = weight, opacity = opacity,
    options = leaflet::pathOptions(pane = pane))
}

#' The debounced table-row hover preview: a non-interactive popup (pointer-events:none via the
#' ik-hover-popup class) at a location. clearGroup + guard + addPopups. @keywords internal
ik_add_hover_popup <- function(proxy, lng, lat, html, group = "Hover") {
  leaflet::clearGroup(proxy, group)
  if (is.null(lng) || is.null(lat) || !is.finite(lng) || !is.finite(lat)) return(invisible(proxy))
  leaflet::addPopups(proxy, lng = lng, lat = lat, popup = html, group = group,
    options = leaflet::popupOptions(closeButton = FALSE, autoPan = FALSE, className = "ik-hover-popup"))
}

#' Add ONE colorNumeric metric legend. Does NOT clearControls — the caller manages that once, so
#' it can stack several legends (e.g. predator pressure's gradient + trapping key). The fixed
#' colour-KEY legends (addLegend(colors=, labels=)) stay inline; this is only for the numeric ramp.
#' @keywords internal
ik_draw_metric_legend <- function(proxy, pal, values, title, position = "bottomright", opacity = 0.9) {
  leaflet::addLegend(proxy, position = position, pal = pal, values = values, title = title, opacity = opacity)
}
