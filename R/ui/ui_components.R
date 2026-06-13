species_overview_nav_menu <- function() {
  species_overview_panels <- unlist(
    lapply(names(core_data$app$spp_classes), function(group_name) {
      species_in_group <- core_data$app$spp_classes[[group_name]]

      lapply(names(species_in_group), function(species_name) {
        sci_name <- species_in_group[[species_name]]
        overview_id <- paste0("species_overview_", make.names(sci_name))

        nav_panel(
          title = tools::toTitleCase(species_name),
          value = overview_id,
          species_overview_module_ui(overview_id)
        )
      })
    }),
    recursive = FALSE
  )

  if (length(species_overview_panels) == 0) {
    return(NULL)
  }

  do.call(
    nav_menu,
    c(
      list(
        title = "Species",
        icon = icon("paw")
      ),
      species_overview_panels
    )
  )
}

species_overview_sidebar_controls <- function() {
  period_defaults <- species_overview_period_defaults(core_data)

  species_overview_controls <- unlist(
    lapply(core_data$app$spp_classes, function(species_in_group) {
      lapply(species_in_group, function(sci_name) {
        overview_id <- paste0("species_overview_", make.names(sci_name))
        overview_tab_input <- paste0(overview_id, "-overview_tabs")

        tagList(
          conditionalPanel(
            condition = sprintf("input.nav === '%s' && input['%s'] === 'current_period'", overview_id, overview_tab_input),
            period_selection_module_ui(
              id = paste0(overview_id, "-current_period"),
              view = "select",
              choices = period_selection_choices(core_data$period_groups, config = config),
              selected = period_defaults$current_period,
              label = "Current period:"
            )
          ),
          conditionalPanel(
            condition = sprintf("input.nav === '%s' && input['%s'] === 'prior_period'", overview_id, overview_tab_input),
            period_selection_module_ui(
              id = paste0(overview_id, "-prior_period"),
              view = "select",
              choices = period_selection_choices(core_data$period_groups, config = config),
              selected = period_defaults$prior_period,
              label = "Prior period:"
            )
          ),
          conditionalPanel(
            condition = sprintf("input.nav === '%s' && input['%s'] === 'last_year_period'", overview_id, overview_tab_input),
            period_selection_module_ui(
              id = paste0(overview_id, "-last_year_period"),
              view = "select",
              choices = period_selection_choices(core_data$period_groups, config = config),
              selected = period_defaults$last_year_period,
              label = "Same period last year:"
            )
          ),
          conditionalPanel(
            condition = sprintf("input.nav === '%s'", overview_id),
            plotting_module_ui(
              id = paste0(overview_id, "-alltime_rai_plot"),
              view = "select_localities",
              choices = unique(core_data$deps$locality),
              selected = unique(core_data$deps$locality),
              include_combine_localities = FALSE
            )
          ),
          conditionalPanel(
            condition = sprintf("input.nav === '%s'", overview_id),
            plotting_module_ui(
              id = paste0(overview_id, "-alltime_rai_plot"),
              view = "select_combine_localities"
            )
          )
        )
      })
    }),
    recursive = FALSE
  )

  tagList(species_overview_controls)
}
