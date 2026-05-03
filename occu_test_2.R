Sys.setenv(R_LIBS_USER="~/.R/library")
.libPaths("~/.R/library")

deps <- data.frame(
  locality = c("Loc1", "Loc1", "Loc2"),
  line = c("L1", "L2", "L1"),
  deploymentID = c("D1", "D2", "D3"),
  camera_hours = c(500, 500, 500)
)
obs <- data.frame(
  deploymentID = c("D1", "D1", "D2", "D3", "D3"),
  locality = c("Loc1", "Loc1", "Loc1", "Loc2", "Loc2"),
  line = c("L1", "L1", "L2", "L1", "L1"),
  scientificName = rep("Apteryx", 5),
  rai_group = rep("Kiwi", 5),
  timestamp = as.POSIXct(c(
    "2023-01-01 10:00:00",
    "2023-01-02 10:00:00",
    "2023-01-01 15:00:00",
    "2023-01-01 20:00:00",
    "2023-01-01 22:00:00"
  ))
)

library(dplyr)

calculate_occupancy <- function(obs, deps) {
    if(!requireNamespace("unmarked", quietly = TRUE)) {
        return(NULL)
    }
    library(unmarked)

    obs <- obs %>% mutate(obs_date = as.Date(timestamp))

    det_hist <- obs %>%
        group_by(deploymentID, rai_group, obs_date) %>%
        summarise(detected = 1, .groups="drop")

    all_deployments <- unique(deps$deploymentID)

    # Normally we do this by group, let's do it for one group as example
    group_obs <- det_hist %>% filter(rai_group == "Kiwi")

    # create N x K matrix where N = deployments, K = days
    # Since deployments might have started on different days, we just
    # order the dates per deployment (day 1, day 2, ..., day K)

    # We should join det_hist to a grid of all days. Let's just create a simple wide matrix.
    wide_hist <- group_obs %>%
        group_by(deploymentID) %>%
        mutate(day = row_number()) %>%
        tidyr::pivot_wider(id_cols=deploymentID, names_from=day, values_from=detected, names_prefix="day_")

    return(wide_hist)
}
print(calculate_occupancy(obs, deps))
