# Returns the column description of column_name if specified, all column descriptions if non column_name specified, and
# NULL if a column_name is specified that doesn't exist.
get_column_description <- function(column_name = NULL) {

column_descriptions <- list(
  "ObID" = "
    Observation ID, the unique identifier for the observation record.<br><br>
    
    This identifier traces the observation back to its source data, revealing all details, including 
    the image sequence that generated the observation. Note: May be truncated in some views due to space.
  ",
  
  "Deployment ID" = "
    The unique identifier for the deployment record.<br><br>
    
    This identifier traces a deployment back to its source data, showing all details such as the Location 
    and camera setup.
  ",
  
  
  "Start" = "
    The start date of the deployment, indicated by the first 'setup' images taken at the time of camera deployment.<br><br>
    
    The start date is based on the camera's local time set at the start of each deployment.
  ",
  
  "End" = "
    The end date of the deployment, indicated by the last 'setup' images taken when the camera is retrieved.<br><br>
    
    The end date is based on the camera's local time set at the start of each deployment.
  ",
  
  "Locality" = "
    The general geographical location where cameras are deployed, consisting of multiple predefined 
    camera lines strategically positioned according to the methodology.
  ",
  
  "Line" = "
    A camera line, being a grouping of cameras, spaced at predefined Locations in a line in accordance 
    with the methodology.
  ",
    
  "Lines" = "
    A Line (camera line) is a grouping of cameras, spaced at predefined Locations in a line in accordance 
    with the methodology. Lines (plural) refers to the number of Lines in a Locality.
  ",
    
    "Location" = "
    A predefined point on a Line where a camera is deployed, identified by latitude and longitude coordinates.
  ",
    
  "Locations" = "
    A Location is a predefined point on a Line where a camera is deployed, identified by latitude and longitude coordinates. 
    Locations (plural) refers to the number of Locations in the Locality or on the Line, depending on context.
  ",
    
  "Timestamp" = "
    Usually refers to the date and time of the first camera image in the sequence that contained the observed species.<br><br>
    The timestamp is based on the camera time and time. The camera is set to local time/time at the start of every deployment.
  ",
    
  "Deployments" = "
    A camera installed at a Location for the observation period, in accordance with the methodology.<br><br>
    <em>(The number of Deployments will always equal the number of Locations, unless your deployment selection criteria 
    includes multiple deployments to a Location e.g. 'ALL' deployments).</em>
  ",
    
  "Animal Detections" = "
    The number of times the camera's motion detection was triggered by an animal of any type during the deployment.
  ",
    
  "Count" = "
    The number of individual's of the species observed. Some detections show several individual 
    animals of the same species in a single frame, so the count is not always 1.
  ",
    
  "Species" = "
    The name of the species observed, in scientific or common vernacular, depending on the settings.
  ",
  
  "Scientific Name" = "
    The scientific name of the species observed, with a matching entry on 
    <a href=\"https://www.catalogueoflife.org/\" target=\"_blank\">https://www.catalogueoflife.org</a>.
  ",
    
  "Common Name" = "
    The vernacular name of the species, based on information in the data package. Common names can vary based on language and region.
  ",
    
  "Camera Hours" = "
    The number of hours a camera was deployed in the field for during the deployment selection period.<br><br>
    In the 'Locality' or 'Line' grouping summaries, it is the combined total for all cameras in the Locality or on the Line.
    Rounded to the nearest hour for output.
  ",
    
  "Detection Interval" = "
    The mean number of hours between each animal detection. Calculation: Camera hours / Animal detections.
  ",
  
  "Species Class" = sprintf("
    The classification assigned to a species by the project, used by InsightfulKiwi to ensure data of most 
    interest is highlighted. Species in a named class are collectively referred to as 'important species'. 
    The named classes are:<br><br>
    
    <em>target:</em> Species specifically targetted by the protocol.<br>
    <em>interesting:</em> Non-target species of most interest<br>
    <em>other:</em> Not a target species nor as interesting, but still likely to be useful data.
    <br><br>
    
    Any species not falling into a named class is labelled as %s.
  ", config$globals$spp_class_unclassified),
  
  
  "Total Individuals" = "
    The total number of individuals observed. A single Animal Detection may show many different 
    individuals in frame; this is a count of the total individuals.
  ",
  
  "Net Count" = "
    The total number of individuals of that species observed, minus the possible duplicate observations 
    of the species.
  ",
  
  "Dup Count" = sprintf("
    Short for possible duplicate count. An observation is considered a possible duplicate if it matches the same species, 
    count, and life stage as another observation within the previous %s minutes at the same Location. These are legitimate 
    observations based on the data, but may be candidates for removal from RAI calculations or other analyses, depending on 
    the methodology of the monitoring program.<br><br>
    
    The threshold (%s minutes, decimal) is defined globally in the configuration. It can be calibrated for the project based on the 
    composition of camera lines, camera rest interval, and behaviours of target species. The same threshold is applied to all species.
  ", config$globals$dup_detect_threshold, config$globals$dup_detect_threshold),
  
  # See "Possible Duplicate" as well, at the end. Possible duplicates is at the summary level
  # Possible Duplicate is at the indivudal observation level
  
  "Possible Duplicates %" = sprintf("
    The percentage of the Animal Count (or Total Count if in a species view) that are possible duplicates based 
    on the Possible Duplicates logic.<br><br>
    
    An observation is considered a possible duplicate if it matches the same species, count, and life stage as 
    another observation within the previous %s minutes at the same Location.
  ", config$globals$dup_detect_threshold),


  "Blank Observations" = "
    Blank observations are where camera detection was triggered, but no species was observed. This is usually due
    to wind causing foliage to move, or causing trees to sway and the resulting shadows triggering detection.
    A high level of blank observations could be a reason to reposition a camera.
  ",
    
  "RAI" = sprintf("
    The Relative Abundance Index (RAI) provides a standardised measure of species detections within a Line, 
    facilitating comparisons over time and across Lines. However, it does not directly estimate population size and may 
    not be comparable between species due to differences in detection probability.
    
    This calculation is based on on Individuals Count, divided by the total camera hours for all locations on the line, 
    normalised to %s camera hours.<br><br>

    The RAI figures for each Line are the basis for calculating the standard error of the mean RAI for the Locality.
  ", config$globals$rai_norm_hours),
    
  "RAI (Net)" = sprintf("
    The Relative Abundance Index (RAI) provides a standardised measure of species detections within a Line, 
    facilitating comparisons over time and across Lines. However, it does not directly estimate population size and may 
    not be comparable between species due to differences in detection probability.
    
    This calculation is based on on Net Individuals Count (excluding possible duplicates), divided by the total camera hours
    for all locations on the line, normalised to %s camera hours. <br><br>

    The RAI figures for each Line are the basis for calculating the standard error of the mean RAI for the Locality.
  ", config$globals$rai_norm_hours),
    
  "RAI ± SE" = sprintf("
    RAI ± SE: The Relative Abundance Index (RAI) provides a standardised measure of species detections within a Locality, 
    facilitating comparisons over time and across Localities. However, it does not directly estimate population size and may 
    not be comparable between species due to differences in detection probability.

    The ± SE (standard error) is calculated as the standard deviation of RAI across Lines within a Locality, divided by 
    the square root of the number of Lines. This reflects the variability in detection rates within a Locality.

    RAI values are standardised to %s camera hours to enable more consistent comparisons across deployments.<br><br>

    The accuracy of these metrics in representing species trends depends on the monitoring methodology and its consistent application. 
    Key methodological factors include target species behavior, camera line and camera positioning, deployment duration, and sampling effort.
  ", config$globals$rai_norm_hours),
    
  "Network RAI ± SE" = "
    The Network RAI (Relative Abundance Index) provides a standardised measure of species detections across the entire project, 
    facilitating comparisons over time. However, it does not directly estimate population size and may 
    not be comparable between species due to differences in detection probability.<b,r><br>
    
    The ± SE calculations are based on RAI figures for the species in each Locality.<br><br>
    
    The Network RAI ± SE data is intended to be a quick glance into species detections. The RAI ± SE data for each Locality is 
    more relevant to measure the results of control programs, which are normally organised within the Locality.
  ",

  "Coverage Area (ha)" = "
    This is the area (in hectares) <strong><em>encompassed by the outermost Locations (cameras)</em></strong> 
    within the Locality. It has been measured solely based on latitude and longitude coordinates of each camera Location, 
    factoring in the curvature of Earth. This calculated coverage area does not factor in any of the area beyond the virtual 
    border defined by the outer locations of cameras in the Locality, and that area is likely to be considered part of the 
    catchment area to some extent.
  ",
    
  "Camera Density (per hectare)" = "
    The average number of cameras per hectare in the Locality, based on the Coverage Area. 
    Calculation: Locations divided by Coverage Area. Does not account for the distribution of the cameras within the Locality.
  ",
    
  "Mean Location Pair Spacing" = "
    This is the mean distance (in meters) between locations on different lines. Calculation: From any 
    given location, there is a distance to every other location. We exclude distances to locations on the same line, but calculate 
    all of the others for every line, excluding duplicate pairings. This is the mean (or average) resulting from those calculations.
  ",
    
  "Location Pair Exceptions" = sprintf("
    The number of location-pairs (being locations on different lines) that are within %s meters of each other.
  ", config$globals$min_distance_threshold),
    
  "Camera Rest Interval" = "
    After motion detection triggers, this is the amount of time (in seconds) each camera is configured to disable motion detection for, 
    before being ready to trigger again.
  ",
    
  "Unique Species" = "
    The number of different types of species observed. The aggregate unique species is the number of unique species observed 
    across all localities.
  ",
  
  "Blanks %" = "
    The percentage of total detections that contained no animal observations.<br><br>
    
    Each time detection is triggered, the camera rest interval begins, during which time nothing can be detected, including animals. 
    High levels of blank detections should be investigated and resolved to reduce the chance of missing animal detections.
  ",
    
  "Blanks" = "
    The number of detections that contained no animal observations.<br><br>
    
    Each time detection is triggered, the camera rest interval begins, during which time nothing can be detected, including animals. 
    High levels of blank detections should be investigated and resolved to reduce the chance of missing animal detections.
  ",
  
  "About InsightfulKiwi" = sprintf("
          InsightfulKiwi provides insights into data collected from wildlife camera monitoring
          programs using the Camera Trap Data Packages (Camtrap DP) format.<br><br>
          
          For more information about InsightfulKiwi, contact
         <a href='mailto:blair@aketechnology.co.nz'>blair@aketechnology.co.nz</a>.<br><br>
          
          The data loaded here was collected by and remains property of %s", config$meta$organisation_name)
  
)
  

  # Possible Duplicate references "Possible Duplicates" in the list above
  # Possible Duplicate is the description shown if user is looking at a view showing individual observations

column_descriptions[["Dup?"]] <- paste0(
  column_descriptions[["Dup Count"]],  # Existing description from "Possible Duplicates"
  "<br><br>
  If the data looks incorrect, it is likely because you can't see the life stage, 
  which may only be recorded when obvious, which is infrequently."  # Additional information
)


  if (is.null(column_name)) {
    return(column_descriptions)
  }
  else if (column_name %in% names(column_descriptions)) {
    return(column_descriptions[[column_name]])
  } else {
    return(NULL)
  }

}