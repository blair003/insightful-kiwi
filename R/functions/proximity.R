# proximity.R — precomputed point-to-point neighbour adjacency (ik_data$app$proximity), built once
# at import. The spatial counterpart to the temporal app$relations: "which traps/cameras are within
# distance D of each monitoring (camera) location". Anchored on CAMERA locations (the unit the
# co-occurrence / spatial-temporal views key on); neighbours are any located camera OR trap.
#
# Distances are metres in NZTM (EPSG:2193) — the same projected, metre-accurate pattern geography.R
# uses (assign_reserves_spatial), NOT the 3857 web-mercator that ik_idw_surface grids in. Stored as a
# tidy long table so a runtime accessor can just FILTER it (no sf calls per request). Built to a
# generous `max_radius_m`; views narrow to a smaller radius at query time.

.proximity_cols <- c("from_id", "from_type", "to_id", "to_type", "distance_m")
.proximity_empty <- function() stats::setNames(
  data.frame(character(), character(), character(), character(), numeric(), stringsAsFactors = FALSE),
  .proximity_cols)

#' Build the camera→neighbour within-distance adjacency.
#'
#' @param locations   ik_data$app$geography$locations (needs location_id, dataset, latitude, longitude).
#' @param datasets    ik_data$datasets (for the dataset → source_type map).
#' @param max_radius_m Largest neighbour distance to retain (metres). Runtime queries narrow within this.
#' @return tidy data.frame: from_id · from_type · to_id · to_type · distance_m (one row per directed
#'   camera→neighbour pair within `max_radius_m`, self excluded; ordered by from_id then distance).
build_proximity <- function(locations, datasets, max_radius_m = 2000) {
  st_map <- ik_dataset_source_types(datasets)
  loc <- locations
  loc$source_type <- unname(st_map[loc$dataset])
  loc <- loc[is.finite(loc$latitude) & is.finite(loc$longitude) & !is.na(loc$source_type), , drop = FALSE]
  cams <- loc[loc$source_type == "camera", , drop = FALSE]
  if (!nrow(cams) || nrow(loc) < 2) return(.proximity_empty())

  loc_sf <- sf::st_transform(sf::st_as_sf(loc,  coords = c("longitude", "latitude"), crs = 4326), 2193)
  cam_sf <- sf::st_transform(sf::st_as_sf(cams, coords = c("longitude", "latitude"), crs = 4326), 2193)
  dmat   <- units::drop_units(sf::st_distance(cam_sf, loc_sf))   # cameras × all-located, metres (tiny)

  rows <- lapply(seq_len(nrow(cams)), function(i) {
    d <- as.numeric(dmat[i, ])
    keep <- which(d <= max_radius_m & loc$location_id != cams$location_id[i])   # within D, not self
    if (!length(keep)) return(NULL)
    data.frame(from_id = cams$location_id[i], from_type = "camera",
               to_id = loc$location_id[keep], to_type = loc$source_type[keep],
               distance_m = round(d[keep]), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) return(.proximity_empty())
  out[order(out$from_id, out$distance_m), , drop = FALSE]
}

#' Neighbours of camera location(s) within a radius — a pure FILTER over app$proximity (no geometry).
#'
#' @param ik_data   The ik_data container.
#' @param location_id One or more camera location_ids (the anchor).
#' @param radius_m  Optional cap (≤ the built max_radius_m); NULL = everything stored.
#' @param of        Optional neighbour kind(s) to keep: "camera" / "trap"; NULL = both.
#' @return data.frame subset of app$proximity (from_id · from_type · to_id · to_type · distance_m).
ik_within_distance <- function(ik_data, location_id, radius_m = NULL, of = NULL) {
  px <- ik_data$app$proximity
  if (is.null(px) || !nrow(px)) return(.proximity_empty())
  d <- px[px$from_id %in% location_id, , drop = FALSE]
  if (!is.null(radius_m)) d <- d[d$distance_m <= radius_m, , drop = FALSE]
  if (!is.null(of))       d <- d[d$to_type %in% of, , drop = FALSE]
  d
}
