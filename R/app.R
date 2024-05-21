library(shiny)
library(lubridate)
library(dplyr)       # If primarily using dplyr from tidyverse
library(plotly)
library(DT)
source('./plot_results_helpers.R')


default_config <- list(
  # where audio is downloaded from
  api_host = "api.acousticobservatory.org",
  # where the listen link points to
  web_host = "data.acousticobservatory.org",
  # used to stop padding going past end of recording.
  # If unknown leave as a really high number and occasionally the link will error
  recording_duration_seconds <- 60*60*24,
  # how many of the random species to show when launched
  initial_num_species = 1
)


launchServer <- function (data_path) {

  # make sure the working directory is set to the directory where this file is
  # to deploy, set the account info provided by shinyapp.io:
  # rsconnect::setAccountInfo(name='app_name',
  #                           token='my_token',
  #                           secret='my_secret')
  # then run
  # rsconnect::deployApp('.')

  # Define the UI
  ui <- fluidPage(
    titlePanel("Minjerribah Bird Call Observations"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("comparisonSelector"),
        uiOutput("confidenceSlider"),
        uiOutput("nameTypeSelector"),  # Selector for name type
        uiOutput("speciesSelector"),   # Updated dynamically based on selection


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


  # loads all_data into global environment
  loadData(data_path)

  # add a row number so we track rows when aggregating
  all_data <- all_data %>% mutate(row_id = row_number())

  # precompute whether it's wet or dry for efficiency
  all_data$is_dry <- str_detect(all_data$site_name, "Dry")

  unique_names <- all_data %>%
    select(scientific_name, common_name) %>%
    distinct()

  # Run the application
  shinyApp(ui = ui, server = server)

}


server <- function(input, output, session) {

  print(head(all_data))


  # Allow user to choose between displaying scientific or common names
  output$nameTypeSelector <- renderUI({
    radioButtons("nameType", "Display Name Type:",
                 choices = c("Scientific Name", "Common Name"),
                 selected = "Common Name")
  })

  # UI for choosing comparison mode
  output$comparisonSelector <- renderUI({
    radioButtons("comparisonType", "Comparison Type:",
                 choices = c("All points", "Wet vs Dry"),
                 selected = "All points")
  })


  # Dynamic species selector based on name type choice
  output$speciesSelector <- renderUI({
    req(input$nameType)
    values <- as.list(unique_names$scientific_name)
    if (input$nameType == "Scientific Name") {
      names(values) <- unique_names$scientific_name
    } else {
      names(values) <- unique_names$common_name
    }
    # Choose random species, or fewer if less than 3 are available
    selected_species <- sample(seq_along(values), min(initial_num_species, length(values)))
    selectInput("speciesInput", "Select Species:", choices = values, selected = values[selected_species], multiple = TRUE)
  })



  # Create dynamic confidence slider based on available data
  output$confidenceSlider <- renderUI({
    min_confidence <- min(all_data$confidence, na.rm = TRUE)
    sliderInput("confidenceInput", "Confidence Level:",
                min = min_confidence, max = 1, value = min_confidence)
  })

  # Date range picker setup with constrained selectable dates
  output$dateRangePicker <- renderUI({
    min_date <- min(all_data$start_date, na.rm = TRUE)
    max_date <- max(all_data$start_date, na.rm = TRUE)

    dateRangeInput("dateInput", "Select Date Range:",
                   start = min_date,
                   end = max_date,
                   min = min_date,
                   max = max_date)
  })

  filtered_data <- reactive({
    req(input$speciesInput, input$confidenceInput)  # Ensure necessary inputs are present

    # Prepare the data with a universal 'site_type' column
    data <- all_data %>%
      filter(scientific_name %in% input$speciesInput,
             confidence >= input$confidenceInput,
             start_date >= input$dateInput[1],
             start_date <= input$dateInput[2]) %>%
      mutate(interval = floor_date(start_date, unit = input$intervalInput),
             species = case_when(
               input$nameType == "Scientific Name" ~ scientific_name,
               TRUE ~ common_name
             ),
             site_type = case_when(
               input$comparisonType == "Wet vs Dry" & !is_dry ~ "Wet",
               input$comparisonType == "Wet vs Dry" & is_dry ~ "Dry",
               TRUE ~ "All"  # Default to 'All' if 'Wet vs Dry' is not selected
             )) %>%
      group_by(interval, site_type, species) %>%
      summarise(count = n(), .groups = 'drop', row_ids = list(row_id))

    data
  })





  output$timeSeriesPlot <- renderPlotly({
    df <- req(filtered_data())
    breaks_info <- determineBreaks(df$interval, input$intervalInput)

    # Print out summaries to debug the subsets
    print(paste("Total data points:", nrow(df)))
    print(table(df$site_type))

    # Create a dynamic color palette
    num_species <- length(unique(df$species))
    color_palette <- scales::brewer_pal(palette = "Set2")(max(3, num_species))


    p <- plot_ly(source = "plotSource")

    # Define different line styles
    line_styles <- c("solid", "dash", "dot")

    # Adding traces manually for each combination of species and site type
    for (species_name in unique(df$species)) {
      for (cur_site_type in unique(df$site_type)) {
        subset_df <- df %>% filter(species == species_name, site_type == cur_site_type)
        line_style <- ifelse(cur_site_type == "Wet", "dash", ifelse(cur_site_type == "Dry", "dot", "solid"))

        p <- p %>% add_trace(data = subset_df,
                             x = ~interval, y = ~count,
                             type = 'scatter', mode = 'lines',
                             line = list(dash = line_style, width = 2),
                             color = ~species,
                             colors = color_palette,
                             name = paste(species_name, cur_site_type),
                             text = ~paste(species, format(interval, "%Y-%m-%d %H:%M:%S"), "with", count, "detections"),
                             hoverinfo = 'text',
                             customdata = ~row_ids)
      }
    }

    p <- p %>% layout(
      title = 'Minjerribah Bird Observations Over Time',
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

    return(p)
  })



  output$detailsTable <- renderDT({
    click_data <- event_data("plotly_click", source = "plotSource")
    if (!is.null(click_data) && !is.null(click_data$customdata)) {
      # Retrieve original rows based on row IDs stored in customdata of the clicked point
      row_ids <- unlist(click_data$customdata)
      original_data <- all_data %>%
        filter(row_id %in% row_ids) %>%
        mutate(
          listen = sprintf('<a href="https://%s/listen/%s?start=%s&end=%s" target="_blank">Listen</a>', web_host, arid, pmax(0, start - 1), pmin(recording_duration_seconds, end + 1)),
          download = sprintf('<a href="https://%s/audio_recordings/%s/media.wav?start_offset=%s&end_offset=%s" target="_blank">Download</a>', api_host, arid, start, end),
          species = if (input$nameType == "Scientific Name") scientific_name else common_name,
          score = confidence,
          date = start_date  # Assuming 'start_date' is the datetime column from 'all_data'
        ) %>%
        select(species, date, score, listen, download)  # Select relevant columns for display

      print(head(original_data))

      datatable(original_data, options = list(pageLength = 5), escape = FALSE)  # 'escape = FALSE' allows HTML rendering of URLs
    } else {
      datatable(data.frame(), options = list(pageLength = 5))
    }
  })


}






