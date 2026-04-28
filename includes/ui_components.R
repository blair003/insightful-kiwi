species_dashboard_nav_menu <- function() {
  species_dashboard_panels <- unlist(
    lapply(names(core_data$spp_classes), function(group_name) {
      species_in_group <- core_data$spp_classes[[group_name]]

      lapply(names(species_in_group), function(species_name) {
        sci_name <- species_in_group[[species_name]]
        dashboard_id <- paste0("species_dashboard_", make.names(sci_name))

        nav_panel(
          title = tools::toTitleCase(species_name),
          value = dashboard_id,
          species_dashboard_module_ui(dashboard_id)
        )
      })
    }),
    recursive = FALSE
  )

  if (length(species_dashboard_panels) == 0) {
    return(NULL)
  }

  do.call(
    nav_menu,
    c(
      list(
        title = "Species Dashboards",
        icon = icon("paw")
      ),
      species_dashboard_panels
    )
  )
}

species_dashboard_sidebar_controls <- function() {
  period_defaults <- species_dashboard_period_defaults(core_data)

  species_dashboard_controls <- unlist(
    lapply(core_data$spp_classes, function(species_in_group) {
      lapply(species_in_group, function(sci_name) {
        dashboard_id <- paste0("species_dashboard_", make.names(sci_name))
        dashboard_tab_input <- paste0(dashboard_id, "-dashboard_tabs")

        tagList(
          conditionalPanel(
            condition = sprintf("input.nav === '%s' && input['%s'] === 'current_period'", dashboard_id, dashboard_tab_input),
            period_selection_module_ui(
              id = paste0(dashboard_id, "-current_period"),
              view = "select",
              choices = names(core_data$period_groups),
              selected = period_defaults$current_period,
              label = "Current complete season:"
            )
          ),
          conditionalPanel(
            condition = sprintf("input.nav === '%s' && input['%s'] === 'prior_period'", dashboard_id, dashboard_tab_input),
            period_selection_module_ui(
              id = paste0(dashboard_id, "-prior_period"),
              view = "select",
              choices = names(core_data$period_groups),
              selected = period_defaults$prior_period,
              label = "Prior season:"
            )
          ),
          conditionalPanel(
            condition = sprintf("input.nav === '%s' && input['%s'] === 'last_year_period'", dashboard_id, dashboard_tab_input),
            period_selection_module_ui(
              id = paste0(dashboard_id, "-last_year_period"),
              view = "select",
              choices = names(core_data$period_groups),
              selected = period_defaults$last_year_period,
              label = "Same season last year:"
            )
          ),
          conditionalPanel(
            condition = sprintf("input.nav === '%s'", dashboard_id),
            plotting_module_ui(
              id = paste0(dashboard_id, "-overall_rai_plot"),
              view = "select_localities",
              choices = unique(core_data$deps$locality),
              selected = unique(core_data$deps$locality),
              include_combine_localities = FALSE
            )
          ),
          conditionalPanel(
            condition = sprintf("input.nav === '%s'", dashboard_id),
            plotting_module_ui(
              id = paste0(dashboard_id, "-overall_rai_plot"),
              view = "select_combine_localities"
            )
          )
        )
      })
    }),
    recursive = FALSE
  )

  tagList(species_dashboard_controls)
}
