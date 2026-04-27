import re

with open("modules/species_dashboard_module.R", "r") as f:
    content = f.read()

# Server side modifications
content = content.replace(
"""    generate_cards <- function(species_obs, deps_data, period_name_label) {
      total_count <- sum(species_obs$count, na.rm = TRUE)
      unique_locs <- length(unique(species_obs$locationName))""",
"""    generate_cards <- function(species_obs, deps_data, period_name_label) {
      total_count <- sum(species_obs$count, na.rm = TRUE)
      unique_locs <- length(unique(species_obs$locationName))
      total_deployments <- length(unique(deps_data$locationName))
      pct_locations <- if (total_deployments > 0) (unique_locs / total_deployments) * 100 else 0
      avg_count <- if (unique_locs > 0) total_count / unique_locs else 0
""")

content = content.replace(
"""      list(
        total = card(card_header("Total Detections"), card_body(h2(total_count), review_action)),
        unique = card(card_header("Unique Locations"), card_body(h2(unique_locs))),
        rai = card(
          card_header(tagList("RAI", rai_calculation_basis_link(period_name_label))),
          card_body(h2(ifelse(is.na(rai), "N/A", sprintf("%.2f", rai))))
        )
      )""",
"""      list(
        total = card(card_header("Total Detections"), card_body(h2(total_count), review_action)),
        unique = card(card_header("Unique Locations"), card_body(h2(unique_locs))),
        rai = card(
          card_header(tagList("RAI", rai_calculation_basis_link(period_name_label))),
          card_body(h2(ifelse(is.na(rai), "N/A", sprintf("%.2f", rai))))
        ),
        other_metrics = card(
          card_header("Other Metrics"),
          card_body(
            p(sprintf("Seen at: %.1f%% of locations", pct_locations)),
            p(sprintf("Avg count per location: %.1f", avg_count))
          )
        )
      )""")

content = content.replace(
"""    output$overall_total_detections_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$total })
    output$overall_unique_locations_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$unique })
    output$overall_rai_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$rai })""",
"""    output$overall_total_detections_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$total })
    output$overall_unique_locations_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$unique })
    output$overall_rai_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$rai })
    output$overall_other_metrics_card <- renderUI({ generate_cards(overall_sobs(), overall_deps(), "ALL")$other_metrics })""")

content = content.replace(
"""    output$current_total_detections_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$total })
    output$current_unique_locations_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$unique })
    output$current_rai_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$rai })""",
"""    output$current_total_detections_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$total })
    output$current_unique_locations_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$unique })
    output$current_rai_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$rai })
    output$current_other_metrics_card <- renderUI({ generate_cards(current_sobs(), current_deps(), current_period_data$period_name())$other_metrics })""")

content = content.replace(
"""    output$prior_total_detections_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$total })
    output$prior_unique_locations_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$unique })
    output$prior_rai_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$rai })""",
"""    output$prior_total_detections_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$total })
    output$prior_unique_locations_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$unique })
    output$prior_rai_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$rai })
    output$prior_other_metrics_card <- renderUI({ generate_cards(prior_sobs(), prior_deps(), prior_period_data$period_name())$other_metrics })""")

content = content.replace(
"""    output$last_year_total_detections_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$total })
    output$last_year_unique_locations_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$unique })
    output$last_year_rai_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$rai })""",
"""    output$last_year_total_detections_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$total })
    output$last_year_unique_locations_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$unique })
    output$last_year_rai_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$rai })
    output$last_year_other_metrics_card <- renderUI({ generate_cards(ly_sobs(), ly_deps(), last_year_period_data$period_name())$other_metrics })""")


with open("modules/species_dashboard_module.R", "w") as f:
    f.write(content)
