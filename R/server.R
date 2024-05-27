getServer <- function(data, config) {
  # returns a server function with data and config within scope
  function(input, output, session) {

    # Get authentication details
    auth_info <- authServer(input, output, session, config)
    is_authorized <- auth_info$is_authorized
    output$is_authorized <- reactive({ auth_info$is_authorized() })
    outputOptions(output, "is_authorized", suspendWhenHidden = FALSE)

    unique_labels <- sort(as.character(unique(data$label)))
    unique_sites <- sort(as.character(unique(data$site)))

    output$modeRadio <- renderUI({
      radioButtons("mode", "Mode:", choices = c("Counts" = "counts", "Scatter" = "scatter"), selected = "counts", inline = TRUE)
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

    observeEvent(input$mode, {
      if (input$mode == "counts") {
        shinyjs::disable("onlyHighestCheckbox")
      } else {
        shinyjs::enable("onlyHighestCheckbox")
      }
    })


    point_clicked <- reactiveVal(FALSE)
    observeEvent(event_data("plotly_click", source = "plotSource"), {
      print('plotly click observed')
      point_clicked(TRUE)
      })

    observeEvent(input$resetTableButton, {point_clicked(FALSE)})

    message_parts <- c()
    putMessage <- function (msg, clear = FALSE) {
      if (clear) message_parts <<- c()
      message_parts <<- c(message_parts, msg)
      message <- paste(message_parts, collapse = '. ')
      output$messageBox <- renderText(message)
    }


    filtered_data <- reactive({
      req(input$speciesInput, input$scoreInput)

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
          mutate(interval = floor_date(timestamp, unit = input$intervalInput)) %>%
          group_by(interval, label, site) %>%
          slice_max(score, n = 1, with_ties = FALSE) %>%
          ungroup() %>%
          select(-interval)

        putMessage(paste('Of these, showing only the highest scoring per', input$intervalInput, 'per site, per class', paste0('(',nrow(modified_data),' points)')))

      }

      modified_data %>%
        mutate(row_id = row_number()) %>%
        arrange(timestamp, label)
    })

    aggregated_data <- reactive({

      df <- req(filtered_data())

      modified_data <- getCounts(df, input$intervalInput, input$speciesInput)

      putMessage(paste(
        'Aggregated into', length(unique(modified_data$timestamp)),
        paste0('one-', input$intervalInput), 'time intervals'
      ))

      modified_data

    })




    output$timeSeriesPlot <- renderPlotly({

      plot_type <- input$mode
      if(plot_type == 'counts') {
        df <- req(aggregated_data())
        p <- getDetectionCountPlot(df, input, output)
      } else {
        df <- req(filtered_data())
        max_detections = 2000
        if (nrow(df) > config$max_scatter_points) {
          putMessage(paste("Too many points to plot, selecting", config$max_scatter_points, "of", nrow(df), "at random"))
          df <- df %>% slice_sample(n = config$max_scatter_points)
        }
        p <- getDetectionScatterPlot(df, input, output)
      }

      return(p)
    })

    output$detailsTable <- renderDT({
      df <- req(filtered_data())
      getDetailsTable(df, config, output, point_clicked)
    })

  }
}


#' Get Counts of Time-Series Data
#'
#' This function processes time-series data to generate counts of occurrences
#' within specified time intervals. It also ensures that all combinations of
#' intervals and labels are included in the result, filling in zeros where data
#' is missing.
#'
#' @param data A tibble containing time-series data. Must include columns:
#'        `timestamp`, `label`, and `row_id`.
#' @param interval A string representing the unit for floor_date (e.g., "day",
#'        "hour", "minute").
#' @return A tibble with counts for each combination of interval and label.
#' @importFrom dplyr mutate group_by summarise left_join n
#' @importFrom tidyr crossing replace_na
#' @importFrom lubridate floor_date
#' @importFrom stats setNames
#' @export
getCounts <- function (data, interval, labels=NULL) {

  modified_data <- data %>%
    mutate(interval = floor_date(timestamp, unit = interval)) %>%
    group_by(interval, label) %>%
    summarise(count = dplyr::n(), .groups = 'drop', row_ids = list(row_id)) %>%
    droplevels() %>%
    rename(timestamp = interval)

  if (is.null(labels)) {
    labels <- unique(modified_data$label)
  }

  all_period_timestamps <- crossing(
    timestamp = seq.POSIXt(
      from = min(modified_data$timestamp),
      to = max(modified_data$timestamp),
      by = interval
    ),
    label = labels
  )

  completed_data <- all_period_timestamps %>%
    left_join(modified_data, by = c("timestamp", "label")) %>%
    mutate(
      count = replace_na(count, 0),
      row_ids = replace_na(row_ids, list(integer(0)))
    )

  return(completed_data)

}
