# IF UPDATING the presented name (right), remember to check and update column descriptions
# So the help icon still works

static_column_rename_override <- list(
  locationName = "Location",
#  line = "Ln",
  observationID = "ObID",
  scientificName = "Species",
  `vernacularNames.eng` = "Species",
 # count = "#",
  individuals_count = "Total Individuals",
  net_individuals_count = "Net Count",
  unique_species_count = "Unique Species",
  individuals_count_protected = "Protected Individuals",
  individuals_count_managed = "Managed Individuals",
  individuals_count_watchlist = "Watchlist Individuals",
  individuals_count_monitored = "Monitored Individuals",
  deploymentID = "Deployment ID",
# camera_hours = "Hours",
  mean_detection_interval = "Detection Interval",
  blank_detections_percentage = "Blanks %",
  blank_detections_count = "Blanks",
  unknown_detections_count = "Unknowns",
  possible_duplicates_percentage = "Possible Duplicates %",
  possible_duplicates_count = "Dup Count",
  possible_duplicate = "Dup?",
  RAI_net = "RAI (Net)",
  mRAI_SE = "RAI ± SE",
  mRAI_SE_net = "RAI ± SE (Net)",
  mmRAI_SE = "Network RAI ± SE",
  mmRAI_SE_net = "Network RAI ± SE (Net)",
  encompassed_area_km2 = "Coverage Area (km²)",
  camera_density_km2 = "Camera Density (per km²)"
)
