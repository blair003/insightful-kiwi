# This contains configuration options for your project. Take a backup copy before
# editing. Specify the config to use in global.R 
# Also see environment.R for environment config

config <- list()

config$meta <- list(
  organisation_name = "Whakatane Kiwi Trust",
  organisation_abbrev = "WKT",
  organisation_header_logo = "wkt-logo-small.png",
  
  project_name = "Whakatane & Ohope",
  
  # Map locality code to name (code based on locationName)
  localities_list = list(
    "KP" = "Kohi Point", 
    "MK" = "Mokoroa", 
    "OH" = "Ohope"
  )
)

config$globals <- list(
  # Set logger threshold as required: DEBUG, INFO, SUCCESS, WARN, ERROR, FATAL
  log_threshold = "DEBUG",
  
  # Google analytics code, read in from config/.env
  ga_tag = Sys.getenv("GA_TAG"),
  
  # Determines the species name we output, only works for these two shown
  species_name_type = "vernacularNames.eng",
  #species_name_type = "scientificName",
  
  # Actually don't touch this, even if your timezone is not UTC.
  # If we need to support timezones later, we will need to do timeshifting from UTC?
  timezone = "UTC", 
  
  # Put your real timezone here, for getSunlightTimes() to determine sunrise/sunset
  actual_timezone = "Pacific/Auckland",
  
  # To calculate seasons
  hemisphere = "south",
  
  # We group data into time periods then calculate and summarise for that time period 
  # Only calculated_seasons currently exists as a period_grouping method
  period_grouping = "calculated_seasons",
  
  # Which period_grouping to use as default for primary and comparative 
  # We create core_data$period_groups as a list, with each item having a description, 
  # start and end date. These reference the list item number

  default_primary_period = 1, # Most recent season
  default_comparative_period = 5, # Same season prior year

  
  # Normalisation applied to all RAI calculations
  rai_norm_hours  = 2000,
  
  # in minutes (decimal), used for the possible duplicate logic
  dup_detect_threshold = 30.1,
  
  # Show RAI calculations based on net individuals count (excluding duplicates)
  rai_net_count = TRUE,
  
  # For species that have no observations on a particular line, we create '0' entries 
  # so we can get accurate standard error calculations. The following field determine 
  # whether those entries are removed  for that view after we are finished using them. 
  # If they are not removed, you will see a lot of 0 entries in the line/locality view
  
  # TRUE means remove entries from the view if there were 0 observations of the species
  
  spp_summary_rm_zeros = list(
    line = TRUE,
    locality = TRUE
  ),
  
  # Each marker-pair (on different lines) lower than this threshold (meters) will 
  # be reported as an exception
  min_distance_threshold = 500,
  
  # Size of resized image copies, being width (left to right). Aspect ratio will be 
  # maintained. Recommended 800, 1024, 1200 as we have placeholder images
  image_resize_width_pixels = 1200,
  
  # How far from a location an species icon can be randomly dispersed
  marker_offset_value = 0.0002,
  
  # If the species record count for the selected date range is over this number, 
  # max_markers_per_species_per_location (below) will be applied. Limits output on species map
  max_markers_apply_for_total_counts_over = 1000,
  
  # This setting limits the markers rendered per-location, applied if above is reached
  max_markers_per_species_per_location = 4,
  
  # Definition used to consolidate many species into one.
  spp_consol_defs = list(
    # The named index (Rattus) will become the new scientificName throughout
    "Rattus" = list(
      # Any observations matching any old_scientificName will be overwritten with 
      # the new scientificName being the list index name. Include the new scientificName
      # in this list, otherwise you'll end up with two taxonomic entries
      old_scientificName = c("Rattus norvegicus", "Rattus rattus", "Rattus"),
      new_vernacularNames.eng = "Rats",
      #new_vernacularNames.nld = "Ratten",
      
      # Used in observations and taxonomic
      new_taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/63QK6",
      
      # Taxonomic entries for each old_scientificName will be removed. These details 
      # along with the new scientificName, and new_taxonID above will be used to 
      # create a new entry
      new_taxonRank = "genus",
      new_taxon_order = "Rodentia",
      new_taxon_family = "Muridae"
    ),
    
    "Insecta" = list(
      old_scientificName = c("Fannia", "Insecta"),
      new_vernacularNames.eng = "Insect sp.",
      new_vernacularNames.nld = "Insect sp.",
      
      # Used in observations and taxonomic
      new_taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/H6",
      new_taxonRank = "class",
      new_taxon_order = "",
      new_taxon_family = ""
    ),
    
    # Combine Felis and Felis catus into Felis catus
    # Because the AI detects as Felis, and we only have Felis catus in our area
    "Felis catus" = list(
      old_scientificName = c("Felis", "Felis catus"),
      new_vernacularNames.eng = "domestic cat",
     # new_vernacularNames.nld = "kat",
      
      # Used in observations and taxonomic
      new_taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/3DXV3",
      new_taxonRank = "species",
      new_taxon_order = "Carnivora",
      new_taxon_family = "Felidae"
    )
  ),
  
  # Classify species into classes, starting with the most important class (e.g. 
  # protected) and working down. You can change the species in each class 
  # (use scientific name), rename classes, remove classes, and you can add classes
  # You are limited based on what will fit the default reporting template
  
  spp_classes = list(

    "target" = c(
      "Mustela erminea", 
      "Mustela nivalis",
      "Mustela putorius furo", 
      "Felis catus", 
      "Rattus"
    ),
    "interesting" = c(
      "Apteryx mantelli",
      "Sus scrofa", 
      "Canis lupus familiaris", 
      "Trichosurus vulpecula", 
      "Erinaceus europaeus", 
      "Mus musculus",
      "Gallirallus australis"
    )
  ),
  
  # Any species not in a class will be put into a class with this name
  spp_class_unclassified = "other",
  
  # Determines whether unclassified species are shown in selects
  spp_show_unclassified = FALSE,
  
  # Default height output for all leaflet maps
  leaflet_height = "calc(100vh - 150px)",
  
  # By default, we output column names (exploded by underscore), with capitalisation 
  # applied to each word. The list below lets you completely change the output name. 
  # Note that get_column_descriptions.R tries to match the new name (right side)
  
  # name = "New Name"
  column_renames = list(
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
    possible_duplicates_count = "Dup Count?",
    possible_duplicate = "Dup?",
    RAI_net = "RAI (Net)",
    mRAI_SE = "RAI ± SE",
    mRAI_SE_net = "RAI ± SE (Net)",
    mmRAI_SE = "Network RAI ± SE",
    mmRAI_SE_net = "Network RAI ± SE (Net)",
    encompassed_area_km2 = "Coverage Area (km²)",
    camera_density_km2 = "Camera Density (per km²)"
  )
  
) # End of config list
