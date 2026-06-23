# coverage.R — per-reserve COVERAGE (density) stats: is the network dense enough to work at all?
# The structural counterpart to the operational coverage-gap finder. Built once at import from the
# reserve footprints (convex hulls, geography.R) + located points. Static (not period-scoped) — it
# describes the network's design density, not what ran this season.
#
# Area = the convex-hull footprint of the reserve's located points (NZTM, hectares). Density =
# count / area. Spacing = mean nearest-neighbour distance (m) among traps / cameras (the metric
# trappers actually think in). Spacing needs trap↔trap distances, computed here per reserve (cheap).

#' Build per-reserve coverage stats. @param locations app$geography$locations. @param datasets
#'   ik_data$datasets (source_type map). The pseudo-reserves (coordless "Unknown" / "Outside
#'   monitored areas") are excluded — they aren't coherent areas.
#' @return data.frame: reserve · area_km2 · n_cameras · n_traps · cameras_per_km2 · traps_per_km2 ·
#'   mean_trap_spacing_m · mean_cam_spacing_m (ordered by area desc); NULL when none.
build_coverage <- function(locations, datasets) {
  st_map <- vapply(datasets, function(d) d$meta$source_type %||% NA_character_, character(1))
  loc <- locations[is.finite(locations$latitude) & is.finite(locations$longitude), , drop = FALSE]
  loc$source_type <- unname(st_map[loc$dataset])
  loc <- loc[!is.na(loc$source_type) & !is.na(loc$reserve) &
               !loc$reserve %in% c(GEO_UNPLACED_RESERVE, GEO_OUTSIDE_RESERVE), , drop = FALSE]
  if (!nrow(loc)) return(NULL)

  to_nztm <- function(pts) sf::st_transform(sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326), 2193)
  # convex-hull footprint area (km²) of the reserve's points, and mean nearest-neighbour spacing (m).
  area_km2_of <- function(pts) { if (nrow(pts) < 3) return(NA_real_)
    as.numeric(units::drop_units(sf::st_area(sf::st_convex_hull(sf::st_union(to_nztm(pts)))))) / 1e6 }
  spacing <- function(pts) { if (nrow(pts) < 2) return(NA_real_)
    dm <- units::drop_units(sf::st_distance(to_nztm(pts))); diag(dm) <- NA
    mean(apply(dm, 1, function(r) min(r, na.rm = TRUE)), na.rm = TRUE) }

  rows <- lapply(sort(unique(loc$reserve)), function(rn) {
    r   <- loc[loc$reserve == rn, , drop = FALSE]
    cam <- r[r$source_type == "camera", , drop = FALSE]
    tr  <- r[r$source_type == "trap",   , drop = FALSE]
    area_km2 <- area_km2_of(r)
    dens <- function(n) if (!is.na(area_km2) && area_km2 > 0) n / area_km2 else NA_real_
    data.frame(reserve = rn, area_km2 = area_km2, n_cameras = nrow(cam), n_traps = nrow(tr),
               cameras_per_km2 = dens(nrow(cam)), traps_per_km2 = dens(nrow(tr)),
               mean_trap_spacing_m = spacing(tr), mean_cam_spacing_m = spacing(cam),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); if (is.null(out)) return(NULL)
  out[order(-out$area_km2), , drop = FALSE]
}

#' Per-reserve coverage stats. @param ik_data The container. @return ik_data$app$coverage.
ik_coverage <- function(ik_data) ik_data$app$coverage
