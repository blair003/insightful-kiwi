import re

with open("modules/species_dashboard_module.R", "r") as f:
    content = f.read()

content = content.replace(
"""    # Overall RAI plot using plotting_module_server (we map this to overall)
    plotting_module_server(
      id = "overall_rai_plot",
      type = NULL,
      obs = core_data$obs,
      deps = core_data$deps,
      species_override = species_name,
      rai_groups = rai_groups_for_species,
      rai_norm_hours = config$globals$rai_norm_hours,
      use_net = config$globals$rai_net_count
    )""",
"""    # Overall RAI plot using plotting_module_server (we map this to overall)
    plotting_module_server(
      id = "overall_rai_plot",
      type = NULL,
      obs = core_data$obs,
      deps = core_data$deps,
      species_override = species_name,
      rai_groups = rai_groups_for_species,
      rai_norm_hours = config$globals$rai_norm_hours,
      use_net = config$globals$rai_net_count
    )

    # Overall Density Map
    mapping_module_server(
      id = "species_density_map_overall",
      type = "density",
      obs = overall_obs,
      deps = overall_deps,
      species_override = reactive(species_name),
      localities_override = selected_localities
    )""")

content = content.replace(
"""    output$current_cooccurrence_ui <- renderUI({ generate_cooccurrence(current_sobs(), current_obs()) })
    output$current_period_name <- renderText({ current_period_data$period_name() })""",
"""    output$current_cooccurrence_ui <- renderUI({ generate_cooccurrence(current_sobs(), current_obs()) })
    output$current_period_name <- renderText({ current_period_data$period_name() })

    # Current Period Density Map
    mapping_module_server(
      id = "species_density_map_current",
      type = "density",
      obs = current_obs,
      deps = current_deps,
      species_override = reactive(species_name),
      localities_override = selected_localities
    )""")


content = content.replace(
"""    output$prior_cooccurrence_ui <- renderUI({ generate_cooccurrence(prior_sobs(), prior_obs()) })
    output$prior_period_name <- renderText({ prior_period_data$period_name() })""",
"""    output$prior_cooccurrence_ui <- renderUI({ generate_cooccurrence(prior_sobs(), prior_obs()) })
    output$prior_period_name <- renderText({ prior_period_data$period_name() })

    # Prior Period Density Map
    mapping_module_server(
      id = "species_density_map_prior",
      type = "density",
      obs = prior_obs,
      deps = prior_deps,
      species_override = reactive(species_name),
      localities_override = selected_localities
    )""")

content = content.replace(
"""    output$last_year_cooccurrence_ui <- renderUI({ generate_cooccurrence(ly_sobs(), ly_obs()) })
    output$last_year_period_name <- renderText({ last_year_period_data$period_name() })""",
"""    output$last_year_cooccurrence_ui <- renderUI({ generate_cooccurrence(ly_sobs(), ly_obs()) })
    output$last_year_period_name <- renderText({ last_year_period_data$period_name() })

    # Last Year Period Density Map
    mapping_module_server(
      id = "species_density_map_last_year",
      type = "density",
      obs = ly_obs,
      deps = ly_deps,
      species_override = reactive(species_name),
      localities_override = selected_localities
    )""")

with open("modules/species_dashboard_module.R", "w") as f:
    f.write(content)
