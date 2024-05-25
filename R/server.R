getServer <- function(data, config) {
  # returns a server function with data and config within scope
  function(input, output, session) {

    unique_labels <- as.character(unique(data$label))
    unique_sites <- as.character(unique(data$site))

    output$modeRadio <- renderUI({
      radioButtons("mode", "Mode:", choices = c("Counts" = "counts", "Scatter" = "scatter"), selected = "counts")
    })

    output$siteSelector <- renderUI({
      site_list <- as.list(unique_sites)
      names(site_list) <- unique_sites
      selectInput("siteInput", "Select Sites (if none will include all):", choices = site_list, selected = list(), multiple = TRUE)
    })


    output$speciesSelector <- renderUI({
      label_list <- as.list(unique_labels)
      names(label_list) <- mapLabel(unique_labels)
      print(label_list)
      selected_species <- initialSelectedSpecies(unique_labels)
      selectInput("speciesInput", "Select Species:", choices = label_list, selected = label_list[selected_species], multiple = TRUE)
    })


    # Create dynamic score slider based on available data
    output$scoreSlider <- renderUI({
      min_score <- min(data$score, na.rm = TRUE)
      max_score <- max(data$score, na.rm = TRUE)
      sliderInput("scoreInput", "Score Range:",
                  min = min_score,
                  max = max_score,
                  value = c(min_score, max_score))
    })

    # Date range picker setup with constrained selectable dates
    output$dateRangePicker <- renderUI({
      min_date <- min(data$timestamp, na.rm = TRUE)
      max_date <- max(data$timestamp, na.rm = TRUE)

      dateRangeInput("dateInput", "Select Date Range:",
                     start = min_date,
                     end = max_date,
                     min = min_date,
                     max = max_date)
    })

    filtered_data <- reactive({
      req(input$speciesInput, input$scoreInput)

      modified_data <- data %>%
        dplyr::filter(
          label %in% input$speciesInput,
          score >= input$scoreInput[1],
          score <= input$scoreInput[2],
          timestamp >= input$dateInput[1],
          timestamp <= input$dateInput[2],
          if (!is.null(input$siteInput) && length(input$siteInput) > 0) site %in% input$siteInput else TRUE
        ) %>%
        mutate(interval = floor_date(timestamp, unit = input$intervalInput))

      print(head(modified_data))

      if (input$mode == 'counts') {
        modified_data <- modified_data %>%
          group_by(interval, label) %>%
          summarise(count = n(), .groups = 'drop', row_ids = list(row_id))
      }

      print(paste('nrow data', nrow(data)))
      print(paste('nrow modified_data', nrow(modified_data)))
      print(paste('date input 1', input$dateInput[1]))
      print(paste('date input 2', input$dateInput[2]))
      print(paste('input sites', input$siteInput))



      modified_data
    })



    output$timeSeriesPlot <- renderPlotly({
      df <- req(filtered_data())
      plot_type <- input$mode
      if(plot_type == 'counts') {
        p <- getDetectionCountPlot(df, input, output)
      } else {
        p <- getDetectionScatterPlot(df, input, output, config$max_scatter_points)
      }

      return(p)
    })



    output$detailsTable <- renderDT({
      click_data <- event_data("plotly_click", source = "plotSource")
      if (!is.null(click_data) && !is.null(click_data$customdata)) {
        # Retrieve original rows based on row IDs stored in customdata of the clicked point
        row_ids <- unlist(click_data$customdata)
        original_data <- data %>%
          dplyr::filter(row_id %in% row_ids) %>%
          mutate(
            listen = sprintf(
              '<a href="https://%s/listen/%s?start=%s&end=%s" target="_blank">Listen</a>',
              config$web_host, arid, pmax(0, offset_seconds - 1),
              pmin(config$recording_duration_seconds, offset_seconds + 1 + config$clip_duration_seconds)),
            download = sprintf(
              '<a href="https://%s/audio_recordings/%s/media.wav?start_offset=%s&end_offset=%s" target="_blank">Download</a>',
              config$api_host, arid, offset_seconds, offset_seconds + config$clip_duration_seconds),
            species = mapLabel(label),
            date = timestamp  # Assuming 'timestamp' is the datetime column from 'data'
          ) %>%
          select(species, site, date, score, listen, download)  # Select relevant columns for display

        print(head(original_data))

        datatable(original_data, options = list(pageLength = 5), escape = FALSE)  # 'escape = FALSE' allows HTML rendering of URLs
      } else {
        datatable(data.frame(), options = list(pageLength = 5))
      }
    })

  }
}
