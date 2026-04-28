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

      # Build navigation footer
      nav_footer <- tagList(
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

      share_btn_html <- sprintf(
        "<button class='btn btn-sm btn-outline-secondary' style='margin-left: 10px;' onclick='copyObservationUrl(\"%s\", this)' title='Share this observation'><i class='fa fa-share-nodes'></i> Share</button>",
        observation_id
      )

      # Show the modal
      showModal(modalDialog(
        title = tagList(
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 20px;",
            tags$div(
              class = "review-sequences-title",
              sprintf("Review Sequences (%d of %d)", current_index, total_sequences)
            ),
            tags$div(
              style = "display: flex; align-items: center;",
              tags$div(
                class = "review-sequences-obs-id",
                sprintf("Obs ID: %s", observation_id)
              ),
              HTML(share_btn_html)
            )
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
