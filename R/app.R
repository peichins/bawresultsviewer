library(shiny)
library(lubridate)
library(dplyr)       # If primarily using dplyr from tidyverse
library(plotly)
library(DT)
#source('./plot_results_helpers.R')
options(shiny.reactlog = TRUE)

default_config <- list(
  # where audio is downloaded from
  api_host = "api.acousticobservatory.org",
  # where the listen link points to
  web_host = "data.acousticobservatory.org",
  # used to stop padding going past end of recording.
  # If unknown leave as a really high number and occasionally the link will error
  recording_duration_seconds = 60*60*24,
  clip_duration_seconds = 5,
  # how many of the random species to show when launched
  initial_num_species = 1,
  title = 'Bird Observations Over Time'
)



#' Launch Shiny Server
#'
#' This function launches a Shiny server to serve the Bird Audio Window results viewer.
#'
#' @param data_path A string specifying the path to the RDS file containing the classification results data.
#' @param config A list containing configuration options for the Shiny app (e.g., API and web host URLs, recording duration).
#'
#' @return A Shiny application object.
#'
#' @examples
#' \dontrun{
#' data_path <- system.file("extdata", "minjerribah_birdnet.rds", package = "bawresultsviewer")
#'
#' config <- list(
#'   api_host = "api.acousticobservatory.org",
#'   web_host = "data.acousticobservatory.org"
#' )
#'
#' launchServer(data_path, config)
#' }
#'
#' @importFrom dplyr anti_join arrange bind_rows distinct filter group_by mutate row_number select summarise slice_max ungroup
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom lubridate floor_date ymd_hms
#' @importFrom magrittr %>%
#' @importFrom plotly add_trace event_data layout plot_ly plotlyOutput renderPlotly event_register
#' @importFrom shiny fluidPage mainPanel shinyApp sidebarLayout sidebarPanel selectInput titlePanel uiOutput
#' @importFrom stringr str_detect
#' @importFrom stats end start
#' @importFrom scales brewer_pal
#' @importFrom utils head modifyList

#'
#' @export
launchServer <- function (data_path, config = list(), test_subset = 0.1) {

  # make sure the working directory is set to the directory where this file is
  # to deploy, set the account info provided by shinyapp.io:
  # rsconnect::setAccountInfo(name='app_name',
  #                           token='my_token',
  #                           secret='my_secret')
  # then run
  # rsconnect::deployApp('.')

  # check for json config in working directory



  config <- getConfig(config)

  # Define the UI
  ui <- fluidPage(
    titlePanel(config$title),
    sidebarLayout(
      sidebarPanel(
        uiOutput("scoreSlider"),
        uiOutput("speciesSelector"),
        uiOutput("siteSelector"),
        uiOutput("dateRangePicker"),
        selectInput("intervalInput", "Aggregation Interval:",
                    choices = c("1 hour" = "hour", "1 day" = "day", "1 week" = "week", "1 month" = "month"),
                    selected = "day")
      ),
      mainPanel(
        plotlyOutput("timeSeriesPlot"),  # Changed to plotlyOutput
        DTOutput("detailsTable")  # Add this to display selected data
      )
    )
  )

  if (is.character(data)) {
    data <- readRDS(data_path)
  }

  if (!is.na(test_subset)) {

    num = round(nrow(data) * test_subset)
    data <- data %>% slice_sample(n = num)
  }

  # add a row number so we track rows when aggregating
  data <- data %>% mutate(row_id = row_number())

  unique_names <- data %>%
    select(label) %>%
    distinct()

  # Run the application


  server <- function(input, output, session) {
    print(head(data))

    unique_labels <- as.character(unique(data$label))
    unique_sites <- as.character(unique(data$site))

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
      req(input$speciesInput, input$scoreInput)  # Ensure necessary inputs are present

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

      modified_data <- modified_data %>%
        group_by(interval, label) %>%
        summarise(count = n(), .groups = 'drop', row_ids = list(row_id))

      print(paste('nrow data', nrow(data)))
      print(paste('nrow modified_data', nrow(modified_data)))
      print(paste('date input 1', input$dateInput[1]))
      print(paste('date input 2', input$dateInput[2]))
      print(paste('input sites', input$siteInput))



      modified_data
    })





    output$timeSeriesPlot <- renderPlotly({
      df <- req(filtered_data())
      breaks_info <- determineBreaks(df$interval, input$intervalInput)

      # Print out summaries to debug the subsets
      print(paste("Total data points:", nrow(df)))

      # Create a dynamic color palette
      num_labels <- length(unique(df$label))
      color_palette <- scales::brewer_pal(palette = "Set2")(max(3, num_labels))


      p <- plot_ly(source = "plotSource")

      # Define different line styles
      line_styles <- c("solid", "dash", "dot")

      # Adding traces manually for each combination of species and site type
      for (label_name in unique(df$label)) {
          subset_df <- df %>% dplyr::filter(label == label_name)
          line_style <- "solid"
          p <- p %>% add_trace(data = subset_df,
                               x = ~interval, y = ~count,
                               type = 'scatter', mode = 'lines',
                               line = list(dash = line_style, width = 2),
                               color = ~label,
                               colors = color_palette,
                               name = ~label,
                               text = ~paste(label, format(interval, "%Y-%m-%d %H:%M:%S"), "with", count, "detections"),
                               hoverinfo = 'text',
                               customdata = ~row_ids)
      }

      p <- p %>% layout(
        xaxis = list(title = 'Date'),
        yaxis = list(title = 'Count'),
        hovermode = 'closest',
        legend = list(title = list(text = 'Species')),
        showlegend = TRUE
      ) %>%
        layout(
          hoverlabel = list(bgcolor = "white",
                            font = list(family = "Arial", size = 12, color = "black"))
        )

      p <- p %>% event_register("plotly_click")  # Register the plotly_click event

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


  shinyApp(ui = ui, server = server)



}



#todo: map a label to something like a species name for display
mapLabel <- function (label) {
  return(label)
}

initialSelectedSpecies <- function (label_list, initial_num_species = 3, show_all_if_under = 8) {
  if (length(label_list) < show_all_if_under) {
    seq_along(label_list)
  } else {
    sample(seq_along(label_list), min(initial_num_species, length(values)))
  }
}


getConfig <- function(config1 = list()) {
  config <- modifyList(default_config, list())

  if (file.exists("config.json")) {
    # Read JSON file into a list
    config2 <- fromJSON("config.json")
    cat("Loaded config.json successfully!\n")
    config <- modifyList(config, config2)
  }

  config <- modifyList(config, config1)
  return(config)
}


