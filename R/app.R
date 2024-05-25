library(shiny)
library(lubridate)
library(dplyr)       # If primarily using dplyr from tidyverse
library(plotly)
library(DT)

#options(shiny.reactlog = TRUE)




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
#' @importFrom shiny fluidPage mainPanel shinyApp sidebarLayout sidebarPanel selectInput textOutput titlePanel uiOutput
#' @importFrom stringr str_detect
#' @importFrom stats end start
#' @importFrom scales brewer_pal
#' @importFrom utils head modifyList

#'
#' @export
launchServer <- function (data_path, config = list(), test_subset = NA) {

  config <- getConfig(config)

  ui <- getUI()

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
  server <- getServer(data, config)

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





