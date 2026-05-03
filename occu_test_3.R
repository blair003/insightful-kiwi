Sys.setenv(R_LIBS_USER="~/.R/library")
.libPaths("~/.R/library")
library(dplyr)
library(tidyr)
library(unmarked)

deps <- data.frame(
  locality = c("Loc1", "Loc1", "Loc2"),
  line = c("L1", "L2", "L1"),
  deploymentID = c("D1", "D2", "D3"),
  camera_hours = c(500, 500, 500),
  deploymentStart = as.POSIXct(c("2023-01-01 00:00:00", "2023-01-01 00:00:00", "2023-01-01 00:00:00"))
)
obs <- data.frame(
  deploymentID = c("D1", "D1", "D2", "D3", "D3"),
  locality = c("Loc1", "Loc1", "Loc1", "Loc2", "Loc2"),
  line = c("L1", "L1", "L2", "L1", "L1"),
  scientificName = rep("Apteryx", 5),
  rai_group = rep("Kiwi", 5),
  timestamp = as.POSIXct(c(
    "2023-01-01 10:00:00",
    "2023-01-03 10:00:00",
    "2023-01-01 15:00:00",
    "2023-01-01 20:00:00",
    "2023-01-01 22:00:00"
  ))
)

calculate_occupancy <- function(obs, deps) {

    obs <- obs %>% mutate(obs_date = as.Date(timestamp))
    deps <- deps %>% mutate(dep_date = as.Date(deploymentStart))

    # join to get start date per observation
    obs <- obs %>% left_join(deps %>% select(deploymentID, dep_date), by="deploymentID")

    # day index is date diff + 1
    obs <- obs %>% mutate(day_index = as.integer(obs_date - dep_date) + 1)

    det_hist <- obs %>%
        group_by(deploymentID, rai_group, day_index) %>%
        summarise(detected = 1, .groups="drop")

    group_obs <- det_hist %>% filter(rai_group == "Kiwi")

    # pivot wider to fill with 0
    wide_hist <- group_obs %>%
        tidyr::pivot_wider(id_cols=deploymentID, names_from=day_index, values_from=detected, names_prefix="day_", values_fill=0)

    # get the columns that start with day_
    y <- as.matrix(wide_hist %>% select(starts_with("day_")))

    umf <- unmarkedFrameOccu(y = y)
    fm <- occu(~1 ~1, umf)
    # estimate occupancy (psi)
    occ_est <- backTransform(fm, "state")@estimate
    occ_se <- SE(backTransform(fm, "state"))

    return(c(psi=occ_est, se=occ_se))
}
print(calculate_occupancy(obs, deps))
