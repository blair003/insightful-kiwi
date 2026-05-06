  # Handle viewing a sequence
  handle_view_sequence <- function(observation_id, view_mode = NULL) {
    #browser()
    if (is.null(view_mode) || view_mode == "") {
      view_mode <- "modal"
    }
    action_type <- paste0("view_sequence|", view_mode)
    observation_details <- create_observation_viewer_output(observation_id, action_type)

    if (!is.null(observation_details)) {
      image_output <- create_observation_images_ui(observation_details$sequence_media_info,
                                                   observation_id,
                                                   context = view_mode)
      if (view_mode == "modal") {
        show_image_modal(observation_id, image_output$ui_elements)
        session$sendCustomMessage(type = "refreshCarousel", message = list(carouselId = image_output$carousel_id))
      } else if (view_mode == "pageview") {
        output$observation_images_pageview <- renderUI({
          image_output$ui_elements
        })
        updateNavbarPage(session, "main_menu", selected = "observations")
        updateTabsetPanel(session, "observations_tabsetpanel", selected = "Viewer")
      }

      if (!image_output$cache_hit) {
        update_image_cache(observation_details$sequence_media_info)
      }
      shinyjs::click("reset_button")
    }
  }

  # Handle editing a sequence
  handle_edit_sequence <- function(sequence_id) {
    sequence_details <- create_sequence_editor_output(sequence_id)

    if (!is.null(sequence_details)) {
      sequence_ui <- create_sequence_editor_ui(sequence_details, context = "pageview")
      output$sequence_editor_pageview <- renderUI({
        sequence_ui
      })
      updateNavbarPage(session, "main_menu", selected = "sequences")
      updateTabsetPanel(session, "sequences_tabsetpanel", selected = "Editor")
      shinyjs::click("reset_button")
    }
  }

  trap_record_source_id <- function(value) {
    if (is.na(value) || !nzchar(value)) {
      return(NA_character_)
    }

    parts <- strsplit(as.character(value), "\\s*\\|\\s*")[[1]]
    match <- parts[startsWith(parts, "source_trapdata_id:")]
    if (length(match) == 0) {
      return(NA_character_)
    }

    trimws(sub("source_trapdata_id:", "", match[[1]], fixed = TRUE))
  }

  trap_detail_table <- function(values) {
    values <- values[!is.na(values) & nzchar(as.character(values))]
    if (length(values) == 0) {
      return(NULL)
    }

    table_data <- data.frame(
      Field = names(values),
      Value = as.character(values),
      stringsAsFactors = FALSE
    )

    HTML(
      knitr::kable(table_data, format = "html", escape = TRUE) %>%
        kableExtra::kable_styling(
          full_width = TRUE,
          bootstrap_options = c("striped", "hover", "condensed", "bordered"),
          font_size = 12
        )
    )
  }

  read_raw_trap_source_record <- function(source_trapdata_id) {
    if (is.na(source_trapdata_id) || !nzchar(source_trapdata_id) ||
        is.null(config$env$dirs$trap_data_source) ||
        is.null(config$env$trap_data_files$raw_trap_data)) {
      return(NULL)
    }

    raw_path <- file.path(
      config$env$dirs$trap_data_source,
      config$env$trap_data_files$raw_trap_data
    )
    if (!file.exists(raw_path)) {
      return(NULL)
    }

    raw_traps <- utils::read.csv(
      raw_path,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA", "NULL"),
      check.names = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
    raw_traps$trapdata_id <- as.character(raw_traps$trapdata_id)
    raw_traps[raw_traps$trapdata_id == source_trapdata_id, , drop = FALSE]
  }

  show_trap_observation_modal <- function(observation_id) {
    if (is.null(trap_data) ||
        is.null(trap_data$obs) ||
        is.null(trap_data$deps) ||
        !nzchar(observation_id)) {
      showModal(modalDialog(
        title = "Trap Record",
        "Trap data is not available.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    trap_obs <- trap_data$obs %>%
      dplyr::filter(observationID == observation_id)

    if (nrow(trap_obs) == 0) {
      showModal(modalDialog(
        title = "Trap Record",
        "No trap record found for this observation ID.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    trap_obs <- trap_obs[1, , drop = FALSE]
    trap_dep <- trap_data$deps %>%
      dplyr::filter(deploymentID == trap_obs$deploymentID[[1]]) %>%
      dplyr::select(
        deploymentID,
        trap_code = locationName,
        trap_line = deploymentGroups,
        latitude,
        longitude,
        deploymentStart,
        deploymentEnd
      )

    source_trapdata_id <- trap_record_source_id(trap_obs$observationComments[[1]])
    raw_record <- read_raw_trap_source_record(source_trapdata_id)

    converted_values <- c(
      observationID = trap_obs$observationID,
      prior_check_date = as.character(as.Date(trap_obs$prior_check_date)),
      check_date = as.character(as.Date(trap_obs$eventStart)),
      check_interval = as.character(trap_obs$check_interval),
      outcome = if ("observationTags" %in% names(trap_obs)) trap_obs$observationTags else NA_character_,
      scientificName = trap_obs$scientificName,
      count = as.character(trap_obs$count),
      behavior = trap_obs$behavior,
      sex = trap_obs$sex,
      lifeStage = trap_obs$lifeStage,
      classifiedBy = trap_obs$classifiedBy,
      source_trapdata_id = source_trapdata_id
    )

    deployment_values <- if (nrow(trap_dep) > 0) {
      unlist(trap_dep[1, ], use.names = TRUE)
    } else {
      NULL
    }

    raw_values <- if (!is.null(raw_record) && nrow(raw_record) > 0) {
      unlist(raw_record[1, ], use.names = TRUE)
    } else {
      NULL
    }

    showModal(modalDialog(
      title = tagList(
        tags$div("Trap Record"),
        tags$small(sprintf("Obs ID: %s", observation_id))
      ),
      tags$h4("Converted trap observation"),
      trap_detail_table(converted_values),
      if (!is.null(deployment_values)) {
        tagList(tags$h4("Trap location/deployment"), trap_detail_table(deployment_values))
      },
      if (!is.null(raw_values)) {
        tagList(tags$h4("Raw source trap-data record"), trap_detail_table(raw_values))
      },
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }

  observeEvent(input$trap_observation_click, {
    parts <- strsplit(input$trap_observation_click, "\\|")[[1]]
    observation_id <- parts[[1]]
    show_trap_observation_modal(observation_id)
  })

  # Updated create_observation_viewer_output function
  create_observation_viewer_output <- function(observation_id = NULL, action_type) {
   # browser()
    # Parse the action_type to extract the action and view_mode
    action_parts <- strsplit(action_type, "\\|")[[1]]
    action <- action_parts[1]      # "view_sequence" or "edit_sequence"
    view_mode <- action_parts[2]   # "modal" or "pageview" (only relevant for view_sequence)

    # Fetch the observation row
    obs_row <- core_data$obs %>%
      dplyr::filter(observationID == observation_id)

    if (!is.null(obs_row)) {
      media <- core_data$media

      # Filter media for the specified sequenceID
      media_filtered <- media %>%
        dplyr::filter(sequenceID == obs_row$sequenceID)

      sequence_media_info <- get_sequence_media_urls(media_filtered)

      if (action == "view_sequence") {
        # Handle view_sequence actions
        if (view_mode == "modal") {
          # Render for modal
          output$observation_record_table_modal <- renderUI({
            setup_kable_output(
              table_id = "special_observation_viewer",
              data = obs_row,
              table_type = "short",
              column_spec = NULL,
              heading_level = NULL
            )
          })
        } else if (view_mode == "pageview") {
          # Render for pageview
          fields <- c("locality", "line", "locationName", "timestamp", "count", config$globals$species_name_type, "comments")
          selected_data <- filter_fields(obs_row, fields, species_group_definitions = NULL, species_fields = NULL)
          output_data <- format_fieldnames(selected_data)

          output$observation_record_table_pageview <- renderUI({
            HTML(
              kable(output_data, format = "html") %>%
                kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover", "condensed", "bordered"))
            )
          })
        }
      } else if (action == "edit_sequence") {
        # Handle edit_sequence actions (implement as needed)
        output$sequence_editor_table <- renderUI({
          # Example: Render a sequence editor UI (logic needs to be defined)
          setup_kable_output(
            table_id = "sequence_editor_viewer",
            data = media_filtered,
            table_type = "full",
            column_spec = NULL,
            heading_level = NULL
          )
        })
      }

      return(list(
        sequence_id = obs_row$sequenceID,
        sequence_media_info = sequence_media_info
      ))
    }
  }

  show_review_sequences_modal <- function(observation_ids, current_index, initial_slide = 0) {
    total_sequences <- length(observation_ids)

    if (current_index < 1 || current_index > total_sequences) {
      return()
    }

    observation_id <- observation_ids[current_index]

    # Get image UI
    observation_details <- create_observation_viewer_output(observation_id, "view_sequence|modal")

    if (!is.null(observation_details)) {
      image_output <- create_observation_images_ui(observation_details$sequence_media_info,
                                                   observation_id,
                                                   context = "modal", review_nav = list(
                                                     current_index = current_index,
                                                     total_sequences = total_sequences,
                                                     initial_slide = initial_slide
                                                   ))

      nav_footer <- if (total_sequences > 1) {
        tagList(
          if (current_index > 1) {
            actionButton("review_prev", "Previous", icon = icon("arrow-left"),
                         onclick = sprintf("Shiny.setInputValue('review_nav_click', %d, {priority: 'event'});", current_index - 1))
          } else {
            disabled(actionButton("review_prev_disabled", "Previous", icon = icon("arrow-left")))
          },
          tags$span(sprintf(" Sequence %d of %d ", current_index, total_sequences), style = "margin: 0 15px;"),
          if (current_index < total_sequences) {
            actionButton("review_next", "Next", icon = icon("arrow-right"),
                         onclick = sprintf("Shiny.setInputValue('review_nav_click', %d, {priority: 'event'});", current_index + 1))
          } else {
            disabled(actionButton("review_next_disabled", "Next", icon = icon("arrow-right")))
          },
          modalButton("Close")
        )
      } else {
        modalButton("Close")
      }

      share_btn_html <- sprintf(
        "<button class='btn btn-sm btn-outline-secondary' style='margin-left: 10px;' onclick='copyObservationUrl(\"%s\", this)' title='Share this observation'><i class='fa fa-share-nodes'></i> Share</button>",
        observation_id
      )

      # Show the modal
      showModal(modalDialog(
        title = tagList(
          tags$div(
            class = "review-sequence-modal-title",
            tags$div(
              class = "review-sequence-heading-block",
              tags$div(class = "review-sequences-title", sprintf("Review Sequence (%d of %d)", current_index, total_sequences)),
              tags$div(
                class = "review-sequences-obs-id",
                sprintf("Obs ID: %s", observation_id)
              )
            ),
            HTML(share_btn_html)
          )
        ),
        image_output$ui_elements,
        uiOutput("observation_record_table_modal"),
        size = "l",
        easyClose = TRUE,
        footer = nav_footer
      ))

      # Refresh carousel
      session$sendCustomMessage(type = "refreshCarousel", message = list(carouselId = image_output$carousel_id))

      # Update cache if needed
      if (!image_output$cache_hit) {
        update_image_cache(observation_details$sequence_media_info)
      }

      # Store current state for navigation
      session$userData$review_sequences_state <- list(
        observation_ids = observation_ids,
        current_index = current_index
      )
    }
  }
