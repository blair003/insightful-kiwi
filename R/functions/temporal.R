# temporal.R — the shared temporal substrate (ik_data$app$temporal): per-reserve, per-day
# sun-window boundaries (civil dawn · sunrise · sunset · civil dusk) from suncalc, plus the
# classifiers that turn an event timestamp into a DIEL PERIOD or a Day/Night label.
#
# Granularity = RESERVE (centroid lat/lon). Locations within a reserve sit within ~km, so
# their sun times differ by sub-minute — below the resolution that could change a diel
# class — while the reserve anchor still separates distant regions correctly. Cameras only:
# trap observations are date-only (no time), so they have no diel.
#
# Diel periods (Holmberg/standard, civil twilight = sun 6° below horizon):
#   Matutinal  civil dawn → sunrise      Vespertine sunset → civil dusk
#   Diurnal    sunrise → sunset          Nocturnal  civil dusk → civil dawn (wraps midnight)
# The Nocturnal wrap needs no cross-day lookup: within a single calendar day, "before this
# day's civil dawn" and "at/after this day's civil dusk" are both Nocturnal.

IK_DIEL_PERIODS <- c("Matutinal", "Diurnal", "Vespertine", "Nocturnal")

# Period palette (dawn = amber · day = blue · dusk = orange · night = slate) — shared by the
# species Behaviour card's stacked bar + legend so the colours stay consistent.
IK_DIEL_COLORS <- c(Matutinal = "#e8a33d", Diurnal = "#2b7bba",
                    Vespertine = "#e0701f", Nocturnal = "#44505e")

# Overall diel-CLASS rules — turn the four per-period effort-normalised rate SHARES (each period's
# rate ÷ total rate) into one activity class. Global (same for every species); a project can override
# via instance/config/project.R `diel <- list(...)` → ik_data$meta$diel (ik_diel_class reads that,
# falling back here). Shares use rates, not raw counts, so unequal period lengths (a long winter
# night) don't masquerade as more activity.
IK_DIEL_CLASS_RULES <- list(
  min_obs     = 30,    # fewer net detections than this → "Insufficient data" (no class)
  low_obs     = 60,    # min_obs .. (low_obs-1) → class shown, but flagged "low confidence"
  dominant    = 0.60,  # Diurnal / Nocturnal: that single period's share ≥ this
  crepuscular = 0.45,  # Crepuscular: (Matutinal + Vespertine) share ≥ this, BOTH represented
  cathemeral  = 0.25   # Cathemeral: BOTH Diurnal and Nocturnal shares ≥ this
)

#' Empty sun table (correct columns/types) for the degenerate cases. @keywords internal
.temporal_empty_sun <- function() {
  data.frame(reserve = character(), date = as.Date(character()),
             civil_dawn = as.POSIXct(character()), sunrise = as.POSIXct(character()),
             sunset = as.POSIXct(character()), civil_dusk = as.POSIXct(character()),
             stringsAsFactors = FALSE)
}

#' Build the temporal substrate: sun-window boundaries per reserve per day.
#'
#' @param datasets  The imported datasets (ik_data$datasets).
#' @param geography The built geography (provides $locations: reserve + lat/lon).
#' @param tzone     The local timezone the app reasons in (default Pacific/Auckland).
#' @return list(sun = <reserve·date·civil_dawn·sunrise·sunset·civil_dusk>,
#'   centroids = <reserve·lat·lon>, tzone). `sun` covers each DISTINCT day a camera
#'   observation falls on (× every reserve) — exactly the lookup the classifiers need, and
#'   robust to a stray sensor-default timestamp that a min→max sequence would balloon into
#'   decades. The species-page deployed-hours-per-period work will widen this to deployment
#'   days when it lands.
build_temporal <- function(datasets, geography, tzone = "Pacific/Auckland") {
  locs <- geography$locations
  rc   <- locs[!is.na(locs$reserve) & !is.na(locs$latitude) & !is.na(locs$longitude), , drop = FALSE]
  if (!nrow(rc)) return(list(sun = .temporal_empty_sun(), centroids = NULL, tzone = tzone))

  centroids <- dplyr::summarise(
    dplyr::group_by(rc, .data$reserve),
    lat = mean(.data$latitude), lon = mean(.data$longitude), .groups = "drop")

  # Distinct local dates from CAMERA observations only (traps are date-only → no diel).
  cam_ids <- names(datasets)[vapply(datasets,
    function(d) identical(d$meta$source_type, "camera"), logical(1))]
  ev <- unlist(lapply(cam_ids, function(id) {
    ds <- datasets[[id]]
    o  <- ik_localize_times(camtrapdp::observations(ds$package), ds$meta, "eventStart")
    as.numeric(o$eventStart)
  }), use.names = FALSE)
  ev <- ev[is.finite(ev)]
  if (!length(ev)) return(list(sun = .temporal_empty_sun(), centroids = centroids, tzone = tzone))

  dates <- sort(unique(as.Date(as.POSIXct(ev, origin = "1970-01-01", tz = tzone), tz = tzone)))

  # suncalc `dawn`/`dusk` are the CIVIL twilight boundaries (sun at -6°) — exactly the
  # Matutinal/Vespertine edges. Times are returned as POSIXct in `tzone`.
  sun <- dplyr::bind_rows(lapply(seq_len(nrow(centroids)), function(i) {
    st <- suncalc::getSunlightTimes(date = dates, lat = centroids$lat[i], lon = centroids$lon[i],
                                    keep = c("dawn", "sunrise", "sunset", "dusk"), tz = tzone)
    data.frame(reserve = centroids$reserve[i], date = st$date,
               civil_dawn = st$dawn, sunrise = st$sunrise,
               sunset = st$sunset, civil_dusk = st$dusk, stringsAsFactors = FALSE)
  }))
  list(sun = sun, centroids = centroids, tzone = tzone)
}

#' The temporal substrate. @keywords internal
ik_temporal <- function(ik_data) ik_data$app$temporal

#' Look up each event's sun-window boundaries by (reserve, local date). Returns a list of
#' four aligned POSIXct vectors (NA where the reserve/date isn't in the table). @keywords internal
.diel_bounds <- function(ik_data, time, reserve) {
  tp    <- ik_temporal(ik_data)
  sun   <- tp$sun %||% .temporal_empty_sun()
  tzone <- tp$tzone %||% "Pacific/Auckland"
  d   <- as.Date(time, tz = tzone)
  idx <- match(paste(reserve, d), paste(sun$reserve, sun$date))
  list(idx = idx, dawn = sun$civil_dawn[idx], sunrise = sun$sunrise[idx],
       sunset = sun$sunset[idx], dusk = sun$civil_dusk[idx])
}

#' Diel period of each event timestamp.
#'
#' @param ik_data The ik_data container.
#' @param time    Event timestamps (POSIXct, local zone — e.g. observation `eventStart`).
#' @param reserve Reserve name(s), aligned with `time` (recycled if length 1).
#' @return Factor with levels IK_DIEL_PERIODS; NA where boundaries are unknown.
ik_diel_period <- function(ik_data, time, reserve) {
  if (length(reserve) == 1L) reserve <- rep(reserve, length(time))
  b   <- .diel_bounds(ik_data, time, reserve)
  ok  <- !is.na(b$idx)
  out <- rep(NA_character_, length(time))
  out[ok & (time <  b$dawn  | time >= b$dusk)] <- "Nocturnal"
  out[ok &  time >= b$dawn  & time <  b$sunrise] <- "Matutinal"
  out[ok &  time >= b$sunrise & time < b$sunset]  <- "Diurnal"
  out[ok &  time >= b$sunset & time <  b$dusk]    <- "Vespertine"
  factor(out, levels = IK_DIEL_PERIODS)
}

#' Mean sun-window boundaries as decimal HOURS-of-day, for shading a 24h activity histogram.
#' Averages civil dawn / sunrise / sunset / civil dusk over the sun table, optionally scoped to a
#' reserve and/or a set of calendar seasons — so a winter-only view gets winter's longer night.
#' @param ik_data The container. @param reserve Optional reserve filter. @param seasons Optional
#' `calendar_season` vector. @return named numeric c(civil_dawn, sunrise, sunset, civil_dusk) in
#' [0,24); NULL when the (filtered) table is empty. @keywords internal
ik_sun_hours <- function(ik_data, reserve = NULL, seasons = NULL) {
  sun <- ik_temporal(ik_data)$sun
  if (is.null(sun) || !nrow(sun)) return(NULL)
  if (!is.null(reserve) && length(reserve)) sun <- sun[sun$reserve %in% reserve, , drop = FALSE]
  if (!is.null(seasons) && length(seasons)) {
    ssea <- ik_assign_season(as.POSIXct(sun$date), as.POSIXct(sun$date))$calendar_season
    sun  <- sun[!is.na(ssea) & ssea %in% seasons, , drop = FALSE]
  }
  if (!nrow(sun)) return(NULL)
  hr <- function(t) { lt <- as.POSIXlt(t); lt$hour + lt$min / 60 + lt$sec / 3600 }
  c(civil_dawn = mean(hr(sun$civil_dawn), na.rm = TRUE), sunrise = mean(hr(sun$sunrise), na.rm = TRUE),
    sunset = mean(hr(sun$sunset), na.rm = TRUE), civil_dusk = mean(hr(sun$civil_dusk), na.rm = TRUE))
}

#' Day/Night label of each event timestamp (sun-up definition: sunrise ≤ t < sunset).
#'
#' @inheritParams ik_diel_period
#' @return Character "Day"/"Night"; NA where boundaries are unknown.
ik_day_night <- function(ik_data, time, reserve) {
  if (length(reserve) == 1L) reserve <- rep(reserve, length(time))
  b  <- .diel_bounds(ik_data, time, reserve)
  ok <- !is.na(b$idx)
  out <- rep(NA_character_, length(time))
  out[ok] <- ifelse(time[ok] >= b$sunrise[ok] & time[ok] < b$sunset[ok], "Day", "Night")
  out
}
