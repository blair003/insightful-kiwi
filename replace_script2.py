import re

with open("modules/species_dashboard_module.R", "r") as f:
    content = f.read()

# UI side modifications
content = content.replace(
"""        layout_column_wrap(
          width = "250px",
          uiOutput(ns("overall_total_detections_card")),
          uiOutput(ns("overall_unique_locations_card")),
          uiOutput(ns("overall_rai_card"))
        ),""",
"""        layout_column_wrap(
          width = "250px",
          uiOutput(ns("overall_total_detections_card")),
          uiOutput(ns("overall_unique_locations_card")),
          uiOutput(ns("overall_rai_card")),
          uiOutput(ns("overall_other_metrics_card"))
        ),""")

content = content.replace(
"""        layout_column_wrap(
          width = "250px",
          uiOutput(ns("current_total_detections_card")),
          uiOutput(ns("current_unique_locations_card")),
          uiOutput(ns("current_rai_card"))
        ),""",
"""        layout_column_wrap(
          width = "250px",
          uiOutput(ns("current_total_detections_card")),
          uiOutput(ns("current_unique_locations_card")),
          uiOutput(ns("current_rai_card")),
          uiOutput(ns("current_other_metrics_card"))
        ),""")

content = content.replace(
"""        layout_column_wrap(
          width = "250px",
          uiOutput(ns("prior_total_detections_card")),
          uiOutput(ns("prior_unique_locations_card")),
          uiOutput(ns("prior_rai_card"))
        ),""",
"""        layout_column_wrap(
          width = "250px",
          uiOutput(ns("prior_total_detections_card")),
          uiOutput(ns("prior_unique_locations_card")),
          uiOutput(ns("prior_rai_card")),
          uiOutput(ns("prior_other_metrics_card"))
        ),""")

content = content.replace(
"""        layout_column_wrap(
          width = "250px",
          uiOutput(ns("last_year_total_detections_card")),
          uiOutput(ns("last_year_unique_locations_card")),
          uiOutput(ns("last_year_rai_card"))
        ),""",
"""        layout_column_wrap(
          width = "250px",
          uiOutput(ns("last_year_total_detections_card")),
          uiOutput(ns("last_year_unique_locations_card")),
          uiOutput(ns("last_year_rai_card")),
          uiOutput(ns("last_year_other_metrics_card"))
        ),""")


with open("modules/species_dashboard_module.R", "w") as f:
    f.write(content)
