# geography.R — build the shared canonical place hierarchy (ik_data$app$geography)
# from each dataset's locations + its per-dataset config (manifest `geography`).
# Canonical levels: location < line < reserve < global.
#
# `line` and `reserve` are INDEPENDENT location attributes. Each dataset names a
# registered DERIVER (`derive`) that produces what it can from its own data
# (camera: line + canonical reserve; trap: line only). Datasets whose reserve is
# not in their data declare a `reserve_match` strategy that assigns the reserve by
# SPATIALLY matching to another dataset's canonical reserves (the cross-dataset
# "hard open problem"; done in the app layer, never in a converter).
#
# Naming: our derived app$ tables use snake_case (location_id, within_monitored_area, ...)
# so the casing distinguishes them from camtrapdp package fields (camelCase:
# locationID, eventStart). `location_id` is taken straight from the package and is
# globally unique already: cameras use Agouti hashes; converters namespace their
# locationIDs at conversion (e.g. wkt_trapping_SS24). No separate source id is kept.

# Placeholder reserve for locations that can't be placed (e.g. coordless / mobile traps): keeps
# them in the reserve→line→location hierarchy so they show in non-spatial views.
GEO_UNPLACED_RESERVE <- "Unplaced"

# ---- derivers: deriver(package, config) -> data.frame(line, reserve) per location ----

#' Geography deriver for location names of the form `CODE LINE_SUBLOC` — a reserve CODE, a
#' space, a LINE, an underscore, then the on-line position (e.g. "OH 1_2", "OHI North_4"). It
#' produces `line` (for grouping) and `reserve` (the CODE mapped via `config$reserves`); the
#' on-line position isn't parsed — the full locationName is already the unique location id.
#'
#' Grammar / what it tolerates (so you know when a NEW deriver is needed):
#'   CODE    = the first whitespace-delimited token, ANY length ("OH", "OHI", "OF1"). Looked up
#'             in `config$reserves` (e.g. `c(OH = "Ohope")`); an unmapped code → reserve NA.
#'   <space> = the FIRST space splits CODE from the rest (required to find the line).
#'   LINE    = from that space up to the FIRST underscore, ANY length ("1", "AA", "North").
#'             No underscore → LINE is everything after the space.
#'   _SUBLOC = ignored here (it only makes the location id unique).
#' So "NNN A_B", "NN AA_B", "NN AA_BB" all parse. Limits: it needs exactly one space to split
#' CODE|LINE (a space INSIDE the line is swallowed into LINE), and CODE must be in the reserves
#' map. A fundamentally different scheme (no code prefix, other delimiters) wants its own deriver.
derive_coded_locationname <- function(package, config) {
  ln       <- camtrapdp::locations(package)$locationName
  code     <- sub(" .*$", "", ln)                       # first token, e.g. "OH"
  line_num <- sub("_.*$", "", sub("^\\S+ ", "", ln))    # "1" from "1_2" (after first space, pre "_")
  data.frame(
    line    = ifelse(is.na(ln), NA_character_, line_num),  # bare "1" (not "OH 1")
    reserve = unname(config$reserves[code]),               # "Ohope" (canonical)
    stringsAsFactors = FALSE
  )
}

#' WKT trap deriver: line (name) from deploymentGroups "line:<name>"; reserve NA
#' (filled by the reserve_match pass).
derive_wkt_trap_line <- function(package, config) {
  locs <- camtrapdp::locations(package)
  deps <- camtrapdp::deployments(package)
  line <- trimws(ifelse(grepl("line:", deps$deploymentGroups),
                        sub(".*line:\\s*([^|]*).*", "\\1", deps$deploymentGroups),
                        NA_character_))
  by_loc <- tapply(line, as.character(deps$locationID), function(v) {
    v <- v[!is.na(v)]; if (length(v)) v[[1]] else NA_character_
  })
  data.frame(
    line    = unname(by_loc[as.character(locs$locationID)]),
    reserve = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' trap.NZ deriver: reserve from the trap.NZ `project`, line from `line` — both carried in
#' deploymentGroups "project:<p> | line:<l>" by the trapnz converter. Optional `config$reserves`
#' remaps a project value to a canonical reserve (e.g. align a trap project with a camera
#' reserve); default identity (the project IS the reserve).
derive_trapnz <- function(package, config) {
  locs <- camtrapdp::locations(package)
  deps <- camtrapdp::deployments(package)
  getg <- function(key) {
    v <- ifelse(grepl(paste0(key, ":"), deps$deploymentGroups),
                trimws(sub(paste0(".*", key, ":\\s*([^|]*).*"), "\\1", deps$deploymentGroups)),
                NA_character_)
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  }
  by_loc <- function(v) tapply(v, as.character(deps$locationID), function(x) {
    x <- x[!is.na(x)]; if (length(x)) x[[1]] else NA_character_ })
  pr <- by_loc(getg("project")); li <- by_loc(getg("line"))
  reserve <- unname(pr[as.character(locs$locationID)])
  if (!is.null(config$reserves)) {                       # optional project → canonical reserve map
    mapped  <- unname(config$reserves[reserve])
    reserve <- ifelse(is.na(mapped), reserve, mapped)
  }
  data.frame(line = unname(li[as.character(locs$locationID)]), reserve = reserve,
             stringsAsFactors = FALSE)
}

ik_register("geography_deriver", "coded_locationname", derive_coded_locationname)
ik_register("geography_deriver", "wkt_trap_line",      derive_wkt_trap_line)
ik_register("geography_deriver", "trapnz",             derive_trapnz)

# ---- build ----------------------------------------------------------------------

#' Per-dataset base locations + self-derived line/reserve. @keywords internal
build_geo_base <- function(id, ds) {
  locs <- camtrapdp::locations(ds$package)
  base <- data.frame(
    location_id        = as.character(locs$locationID),  # globally unique already
    dataset            = id,
    name               = locs$locationName,
    latitude           = locs$latitude,
    longitude          = locs$longitude,
    line               = NA_character_,
    reserve            = NA_character_,
    within_monitored_area     = NA,
    nearest_monitoring_location    = NA_character_,
    nearest_monitoring_distance_km = NA_real_,
    stringsAsFactors = FALSE
  )
  cfg <- ds$meta$geography
  if (!is.null(cfg) && !is.null(cfg$derive)) {
    deriver <- ik_registered("geography_deriver", cfg$derive)
    if (is.null(deriver)) {
      logger::log_warn("geography: no deriver '%s' for dataset '%s' — locations only.",
                       cfg$derive, id)
    } else {
      d <- deriver(ds$package, cfg)
      base$line    <- d$line
      base$reserve <- d$reserve
      # A dataset that supplies its own reserve and isn't spatially matched is
      # canonical -> its locations are "within" by definition.
      if (is.null(cfg$reserve_match)) base$within_monitored_area <- !is.na(base$reserve)
    }
  }
  base
}

#' Convex hulls (NZTM) per reserve from canonical points. @keywords internal
reserve_hulls_nztm <- function(canon_sf, reserves) {
  hulls <- list()
  for (rn in unique(reserves)) {
    pts <- canon_sf[reserves == rn, ]
    if (nrow(pts) >= 3) hulls[[rn]] <- sf::st_convex_hull(sf::st_union(pts))
  }
  hulls
}

#' Spatially assign reserves for one dataset against a canonical dataset's reserves.
#' Returns the computed columns (keyed by location_id) + WGS84 reserve hulls.
#' @keywords internal
assign_reserves_spatial <- function(locations, id, rm) {
  buffer_m <- rm$buffer_m %||% 0
  canon <- locations[locations$dataset == rm$canonical & !is.na(locations$reserve) &
                       !is.na(locations$latitude) & !is.na(locations$longitude), ]
  target <- locations[locations$dataset == id &
                        !is.na(locations$latitude) & !is.na(locations$longitude), ]
  if (nrow(canon) == 0 || nrow(target) == 0) return(NULL)

  canon_sf  <- sf::st_transform(sf::st_as_sf(canon,  coords = c("longitude", "latitude"), crs = 4326), 2193)
  target_sf <- sf::st_transform(sf::st_as_sf(target, coords = c("longitude", "latitude"), crs = 4326), 2193)

  # Nearest monitoring location (always): distance to the camera POINTS, not hull edges.
  dmat <- units::drop_units(sf::st_distance(target_sf, canon_sf))
  ni   <- apply(dmat, 1, which.min)
  near_km <- apply(dmat, 1, min) / 1000

  # Containment in buffered hulls.
  hulls   <- reserve_hulls_nztm(canon_sf, canon$reserve)
  within  <- rep(FALSE, nrow(target))
  contain <- rep(NA_character_, nrow(target))
  for (rn in names(hulls)) {
    inside <- lengths(sf::st_intersects(target_sf, sf::st_buffer(hulls[[rn]], buffer_m))) > 0
    sel <- inside & is.na(contain)
    contain[sel] <- rn
    within[inside] <- TRUE
  }

  list(
    cols = data.frame(
      location_id = target$location_id,
      reserve = ifelse(within, contain, canon$reserve[ni]),  # assigned: containing or nearest
      within_monitored_area = within,
      nearest_monitoring_location    = canon$name[ni],
      nearest_monitoring_distance_km = round(near_km, 3),
      stringsAsFactors = FALSE
    ),
    hulls = lapply(hulls, function(h) sf::st_transform(h, 4326))
  )
}

#' Build ik_data$app$geography from all datasets.
#'
#' The line/reserve hierarchy is carried on `locations` (group/filter by the
#' dataset/reserve/line columns directly); an explicit groups/tree table is added
#' only when a feature needs it.
#'
#' @param datasets The ik_data$datasets list (each `$package` + `$meta$geography`).
#' @return list(levels, locations, reserve_hulls).
build_geography <- function(datasets) {
  # Pass 1 — self-derive.
  locations <- dplyr::bind_rows(lapply(names(datasets),
                                       function(id) build_geo_base(id, datasets[[id]])))

  # Pass 2 — spatial reserve assignment for datasets configured with reserve_match.
  reserve_hulls <- list()
  for (id in names(datasets)) {
    rm <- datasets[[id]]$meta$geography$reserve_match
    if (is.null(rm) || !identical(rm$strategy, "spatial_hull")) next
    res <- assign_reserves_spatial(locations, id, rm)
    if (is.null(res)) next
    idx <- match(res$cols$location_id, locations$location_id)
    for (col in setdiff(names(res$cols), "location_id")) locations[[col]][idx] <- res$cols[[col]]
    reserve_hulls <- utils::modifyList(reserve_hulls, res$hulls)
  }

  # Pass 3 — anything still without a reserve (e.g. coordless / mobile traps that can't be
  # spatially matched) goes to a placeholder reserve so it still fits the reserve→line→location
  # hierarchy and appears in non-spatial views; map views simply have no point to draw.
  unplaced <- is.na(locations$reserve)
  if (any(unplaced)) {
    locations$reserve[unplaced]               <- GEO_UNPLACED_RESERVE
    locations$within_monitored_area[unplaced] <- FALSE
  }

  list(levels = c("location", "line", "reserve", "global"),
       locations = locations, reserve_hulls = reserve_hulls)
}

#' The shared geography registry.
#' @param ik_data The ik_data container.
#' @return ik_data$app$geography (see build_geography()).
ik_geography <- function(ik_data) {
  ik_data$app$geography
}
