import re

with open("modules/species_dashboard_module.R", "r") as f:
    content = f.read()

# Add mapping UI for overall
content = content.replace(
"""          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("overall_cooccurrence_ui"))
          )
        )
      ),""",
"""          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("overall_cooccurrence_ui"))
          )
        ),
        br(),
        card(
          class = "dashboard-plot-card",
          card_header(tagList(icon("map"), "Species Density Map")),
          mapping_module_ui(id = ns("species_density_map_overall"), view = "map"),
          full_screen = FALSE
        )
      ),""")

# Add mapping UI for current period
content = content.replace(
"""          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("current_cooccurrence_ui"))
          )
        )
      ),""",
"""          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("current_cooccurrence_ui"))
          )
        ),
        br(),
        card(
          class = "dashboard-plot-card",
          card_header(tagList(icon("map"), "Species Density Map")),
          mapping_module_ui(id = ns("species_density_map_current"), view = "map"),
          full_screen = FALSE
        )
      ),""")

# Add mapping UI for prior period
content = content.replace(
"""          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("prior_cooccurrence_ui"))
          )
        )
      ),""",
"""          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("prior_cooccurrence_ui"))
          )
        ),
        br(),
        card(
          class = "dashboard-plot-card",
          card_header(tagList(icon("map"), "Species Density Map")),
          mapping_module_ui(id = ns("species_density_map_prior"), view = "map"),
          full_screen = FALSE
        )
      ),""")

# Add mapping UI for last year period
content = content.replace(
"""          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("last_year_cooccurrence_ui"))
          )
        )
      )""",
"""          card(
            card_header("Co-occurrence with Kiwi"),
            uiOutput(ns("last_year_cooccurrence_ui"))
          )
        ),
        br(),
        card(
          class = "dashboard-plot-card",
          card_header(tagList(icon("map"), "Species Density Map")),
          mapping_module_ui(id = ns("species_density_map_last_year"), view = "map"),
          full_screen = FALSE
        )
      )""")

with open("modules/species_dashboard_module.R", "w") as f:
    f.write(content)
