getServer <- function(data, config) {
  # returns a server function with data and config within scope
  function(input, output, session) {

    # Get authentication details
    auth_info <- authServer(input, output, session, config)
    is_authorized <- auth_info$is_authorized
    output$is_authorized <- shiny::reactive({ auth_info$is_authorized() })
    shiny::outputOptions(output, "is_authorized", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$mode, {
      if (input$mode == "counts") {
        shinyjs::disable("onlyHighestCheckbox")
      } else {
        shinyjs::enable("onlyHighestCheckbox")
      }
    })

    default_score_selection <- getDefaultScoreSelection(data)
    shiny::observeEvent(input$resetSlider, {
      shiny::updateSliderInput(session, "scoreInput", value = default_score_selection)
    })

    point_clicked <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(plotly::event_data("plotly_click", source = "plotSource"), {
      print('plotly click observed')
      point_clicked(TRUE)
    })

    shiny::observeEvent(input$resetTableButton, {point_clicked(FALSE)})

    message_parts <- c()
    putMessage <- function (msg, clear = FALSE) {
      if (clear) message_parts <<- c()
      message_parts <<- c(message_parts, msg)
      message <- paste(message_parts, collapse = '. ')
      output$messageBox <- shiny::renderText(message)
    }

    filtered_data <- shiny::reactive({
      shiny::req(input$speciesInput, input$scoreInput)

      modified_data <- data %>%
        dplyr::filter(
          label %in% input$speciesInput,
          score >= input$scoreInput[1],
          score <= input$scoreInput[2],
          timestamp >= input$dateInput[1],
          timestamp <= input$dateInput[2] + lubridate::days(1),
          if (!is.null(input$siteInput) && length(input$siteInput) > 0) site %in% input$siteInput else TRUE
        )

      putMessage(paste(nrow(modified_data), 'of', nrow(data), 'fit critera'), TRUE)

      if (input$mode == "scatter" && input$onlyHighestCheckbox) {
        # Filter for top scoring row per species per interval per site
        modified_data <- modified_data %>%
          dplyr::mutate(interval = lubridate::floor_date(timestamp, unit = input$intervalInput)) %>%
          dplyr::group_by(interval, label, site) %>%
          dplyr::slice_max(score, n = 1, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::select(-interval)

        putMessage(paste('Of these, showing only the highest scoring per', input$intervalInput, 'per site, per class', paste0('(',nrow(modified_data),' points)')))
      }

      modified_data %>%
        dplyr::mutate(row_id = dplyr::row_number()) %>%
        dplyr::arrange(timestamp, label)
    })

    aggregated_data <- shiny::reactive({
      df <- shiny::req(filtered_data())
      modified_data <- getCounts(df, input$intervalInput, input$speciesInput)
      putMessage(paste(
        'Aggregated into', length(unique(modified_data$timestamp)),
        paste0('one-', input$intervalInput), 'time intervals'
      ))
      modified_data
    })

    output$timeSeriesPlot <- plotly::renderPlotly({
      plot_type <- shiny::req(input$mode)

      if(plot_type == 'counts') {
        df <- shiny::req(aggregated_data())
        p <- getDetectionCountPlot(df, input, output)
      } else {
        df <- shiny::req(filtered_data())
        max_detections = 2000
        if (nrow(df) > config$max_scatter_points) {
          putMessage(paste("Too many points to plot, selecting", config$max_scatter_points, "of", nrow(df), "at random"))
          df <- dplyr::slice_sample(df, n = config$max_scatter_points)
        }
        p <- getDetectionScatterPlot(df, input, output)
      }

      return(p)
    })

    output$detailsTable <- DT::renderDT({
      df <- shiny::req(filtered_data())
      getDetailsTable(df, config, output, point_clicked)
    })
  }
}
