getServer <- function(data, config) {
  # returns a server function with data and config within scope
  function(input, output, session) {

    # Get authentication details
    auth_info <- authServer(input, output, session, config)
    is_authorized <- auth_info$is_authorized
    output$is_authorized <- reactive({ auth_info$is_authorized() })
    outputOptions(output, "is_authorized", suspendWhenHidden = FALSE)

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
      selected_species <- initialSelectedSpecies(unique_labels)
      selectInput("speciesInput", "Select Species:", choices = label_list, selected = label_list[selected_species], multiple = TRUE)
    })

    output$scoreSlider <- renderUI({
      min_score <- min(data$score, na.rm = TRUE)
      max_score <- max(data$score, na.rm = TRUE)
      sliderInput("scoreInput", "Score Range:",
                  min = min_score,
                  max = max_score,
                  value = c(min_score, max_score))
    })

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

      if (input$mode == 'counts') {
        modified_data <- modified_data %>%
          group_by(interval, label) %>%
          summarise(count = dplyr::n(), .groups = 'drop', row_ids = list(row_id))
      }

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

    output$detailsTable <- renderDT(getDetailsTable(data, config))

  }
}
