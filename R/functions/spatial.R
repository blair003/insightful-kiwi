# spatial.R — pure spatial-computation helpers for the map views (and any future
# spatial analysis): an inverse-distance-weighting prediction SURFACE and per-group
# convex-HULL footprints. Named for the DOMAIN (geometry on points), not the view —
# a hotspot composite, a report, or a per-group footprint can all reuse these.
#
# Runtime helpers fed by ik_location_metric() (per-location point values + coords);
# they pair with geography.R (which BUILDS app$geography at import). Both return NULL
# gracefully (too few points, no coords) so a caller can simply skip a layer. Derived
# columns are snake_case; sf geometries are WGS84 (EPSG:4326), ready for Leaflet.

#' Circle-marker radius scaled to a value vector — the single home for the map modules' marker sizing.
#' Area-fair (`sqrt`), clamped to `[lo, hi]`; a degenerate range collapses to the midpoint. With
#' `cap_pctl` set, values are first capped at that percentile (so one low-effort outlier can't flatten
#' the rest, scaling from 0); with `cap_pctl = NULL` it scales over the data range. @keywords internal
ik_marker_radius <- function(v, lo = 5, hi = 20, cap_pctl = NULL) {
  v <- pmax(as.numeric(v), 0)
  if (is.null(cap_pctl)) {
    r <- range(v, na.rm = TRUE)
    if (!length(v) || !is.finite(diff(r)) || diff(r) == 0) return(rep((lo + hi) / 2, length(v)))
    return(unname(scales::rescale(sqrt(v), to = c(lo, hi))))
  }
  # unname()/names = FALSE are LOad-bearing: stats::quantile() returns a NAMED value ("98%"), and that
  # name propagates to the result. A length-1 NAMED numeric is serialised by leaflet (auto_unbox) as a
  # JSON OBJECT {"98%":r} rather than the scalar r — which breaks a LONE CircleMarker's radius so it
  # draws empty (the "a single point on the map doesn't show" bug). Multi-point vectors serialise as a
  # plain array, so the bug only bites when exactly one location has a value. Keep them unnamed.
  cap <- stats::quantile(v[v > 0], cap_pctl, na.rm = TRUE, names = FALSE)
  if (!is.finite(cap) || cap <= 0) return(rep((lo + hi) / 2, length(v)))
  unname(scales::rescale(sqrt(pmin(v, cap)), to = c(lo, hi), from = c(0, sqrt(cap))))
}

#' Robust upper cap for a colour/size scale: the `pctl` percentile of the positive values (never below
#' the min, never zero). The single home for the maps colour/size clamp. @keywords internal
ik_robust_cap <- function(v, pctl = 0.95) {
  pos <- v[is.finite(v) & v > 0]; if (!length(pos)) return(1)
  max(as.numeric(stats::quantile(pos, pctl, names = FALSE, type = 7)), min(pos), 1e-9)
}

#' Inverse-distance-weighted prediction surface over point values, per group.
#'
#' Ported in algorithm from v0.1's `create_idw_prediction_surface` (reference only):
#' for each group it projects the points to a metric CRS (EPSG:3857), draws a convex-hull
#' footprint buffered by the (inferred) inter-point spacing, lays a grid over it, and
#' estimates the value at each grid-cell centroid by 1/d^idp inverse-distance weighting of
#' that group's points (exact value at a coincident point). Cells are clipped to the
#' footprint and returned as WGS84 polygons ready for `leaflet::addPolygons()`.
#'
#' @param points  data.frame with `longitude`, `latitude`, `value_col`, `group_col`
#'   — exactly `ik_location_metric()` output (latitude/longitude/metric/reserve).
#' @param value_col Column to interpolate (default "metric").
#' @param group_col Facet column; one surface per group (default "reserve"). NULL → one
#'   surface over all points.
#' @param min_points Minimum DISTINCT points a group needs, else it is skipped.
#' @param idp     Inverse-distance power (2 = standard IDW).
#' @param grid_n  Target cells across the longest footprint side.
#' @param max_cells Cap on grid cells (cell size grows to respect it).
#' @param buffer_m Fixed footprint buffer (m); NULL = infer from point spacing.
#' @param output_col Name of the predicted-value column on the result.
#' @return An sf data.frame (`group_col`, `output_col`, geometry) in EPSG:4326, or NULL.
ik_idw_surface <- function(points, value_col = "metric", group_col = "reserve",
                           min_points = 3, idp = 2, grid_n = 45, max_cells = 1800,
                           buffer_m = NULL, output_col = "predicted") {
  required <- c("longitude", "latitude", value_col)
  if (is.null(points) || nrow(points) == 0 || !all(required %in% names(points))) return(NULL)
  gcol <- if (!is.null(group_col)) group_col else "group"

  surface_for_group <- function(group_data, group_name) {
    values <- suppressWarnings(as.numeric(group_data[[value_col]]))
    usable <- group_data |>
      dplyr::mutate(.surface_value = values) |>
      dplyr::filter(is.finite(.data$longitude), is.finite(.data$latitude),
                    is.finite(.data$.surface_value))
    if (nrow(usable) < min_points) return(NULL)

    pts <- sf::st_transform(
      sf::st_as_sf(usable, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE), 3857)
    coords <- sf::st_coordinates(pts)
    if (nrow(unique(round(coords, 2))) < min_points) return(NULL)

    dmat <- as.matrix(stats::dist(coords)); dmat[dmat == 0] <- NA_real_
    nn   <- suppressWarnings(apply(dmat, 1, min, na.rm = TRUE)); nn <- nn[is.finite(nn)]
    inferred <- if (length(nn)) max(stats::median(nn, na.rm = TRUE) * 0.75, 50) else 100
    buf <- if (is.null(buffer_m)) inferred else buffer_m

    footprint <- sf::st_buffer(sf::st_convex_hull(sf::st_union(sf::st_geometry(pts))), buf)
    if (length(footprint) == 0 || any(sf::st_is_empty(footprint))) return(NULL)

    bb <- sf::st_bbox(footprint)
    w  <- as.numeric(bb[["xmax"]] - bb[["xmin"]]); h <- as.numeric(bb[["ymax"]] - bb[["ymin"]])
    extent <- max(w, h)
    if (!is.finite(extent) || extent <= 0) return(NULL)
    cell <- max(extent / grid_n, 25)
    est  <- (w / cell) * (h / cell)
    if (is.finite(est) && est > max_cells) cell <- cell * sqrt(est / max_cells)

    grid <- sf::st_make_grid(footprint, cellsize = cell, what = "polygons", square = TRUE)
    if (length(grid) == 0) return(NULL)
    grid_sf <- sf::st_sf(.group = as.character(group_name), geometry = grid)
    centres <- suppressWarnings(sf::st_centroid(grid_sf))
    inside  <- lengths(sf::st_within(centres, footprint)) > 0
    grid_sf <- grid_sf[inside, ]; centres <- centres[inside, ]
    if (nrow(grid_sf) == 0) return(NULL)

    cc <- sf::st_coordinates(centres)
    pv <- pts$.surface_value
    pred <- vapply(seq_len(nrow(cc)), function(i) {
      d <- sqrt((coords[, 1] - cc[i, 1])^2 + (coords[, 2] - cc[i, 2])^2)
      if (any(d == 0)) return(mean(pv[d == 0], na.rm = TRUE))
      wts <- 1 / (d ^ idp)
      sum(wts * pv, na.rm = TRUE) / sum(wts, na.rm = TRUE)
    }, numeric(1))

    grid_sf[[output_col]] <- pred
    grid_sf <- dplyr::filter(grid_sf, is.finite(.data[[output_col]]))
    grid_sf <- sf::st_transform(grid_sf, 4326)
    if (nrow(grid_sf) == 0) return(NULL)
    grid_sf[, c(".group", output_col)]
  }

  grouped <- if (!is.null(group_col) && group_col %in% names(points)) {
    split(points, points[[group_col]], drop = TRUE)
  } else {
    list("Selected area" = points)
  }

  surfaces <- Filter(Negate(is.null), Map(surface_for_group, grouped, names(grouped)))
  if (length(surfaces) == 0) return(NULL)
  out <- do.call(rbind, surfaces)
  names(out)[names(out) == ".group"] <- gcol
  out
}

#' Per-group convex-hull footprint of the SELECTED points (WGS84), one row per group.
#'
#' A selection-responsive boundary overlay: unlike the sparse, import-time
#' `app$geography$reserve_hulls`, this hulls whatever points are currently selected.
#' A group with fewer than `min_points` distinct points yields no hull (a hull needs 3).
#'
#' @param points data.frame with `longitude`, `latitude`, `group_col`.
#' @param group_col Column to group by (default "reserve").
#' @param min_points Minimum distinct points to form a hull.
#' @return An sf data.frame (`group_col`, geometry) in EPSG:4326, or NULL.
ik_selection_hulls <- function(points, group_col = "reserve", min_points = 3) {
  if (is.null(points) || nrow(points) == 0 ||
      !all(c("longitude", "latitude") %in% names(points))) return(NULL)
  pts <- points[is.finite(points$longitude) & is.finite(points$latitude), , drop = FALSE]
  if (nrow(pts) == 0) return(NULL)
  if (!group_col %in% names(pts)) pts[[group_col]] <- "Selected area"

  groups <- split(pts, pts[[group_col]], drop = TRUE)
  hulls <- lapply(names(groups), function(g) {
    gd <- groups[[g]]
    if (nrow(unique(round(gd[, c("longitude", "latitude")], 5))) < min_points) return(NULL)
    geom <- sf::st_convex_hull(sf::st_union(
      sf::st_geometry(sf::st_as_sf(gd, coords = c("longitude", "latitude"), crs = 4326))))
    row <- data.frame(g, stringsAsFactors = FALSE); names(row) <- group_col
    sf::st_sf(row, geometry = geom)
  })
  hulls <- Filter(Negate(is.null), hulls)
  if (length(hulls) == 0) return(NULL)
  do.call(rbind, hulls)
}

#' Per-group centroid (mean lng/lat) + point count — for line/reserve-grain markers.
#'
#' A plain data.frame (NOT sf): one row per group with the mean of its points' coordinates,
#' for placing a single marker per line/reserve (e.g. the protocol's per-LINE RAI). Operates on
#' the SELECTED points so the centroid reflects the current selection.
#'
#' @param points data.frame with `longitude`, `latitude`, and the `group_cols`.
#' @param group_cols Grouping columns (default reserve · line).
#' @return data.frame(group_cols…, longitude, latitude, n), or NULL.
ik_group_centroids <- function(points, group_cols = c("reserve", "line")) {
  if (is.null(points) || nrow(points) == 0 ||
      !all(c("longitude", "latitude", group_cols) %in% names(points))) return(NULL)
  pts <- points[is.finite(points$longitude) & is.finite(points$latitude), , drop = FALSE]
  pts <- pts[stats::complete.cases(pts[, group_cols, drop = FALSE]), , drop = FALSE]
  if (!nrow(pts)) return(NULL)
  g <- dplyr::summarise(
    dplyr::group_by(pts, dplyr::across(dplyr::all_of(group_cols))),
    longitude = mean(.data$longitude), latitude = mean(.data$latitude),
    n = dplyr::n(), .groups = "drop")
  as.data.frame(g)
}
