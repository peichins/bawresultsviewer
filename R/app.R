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
#' @param data A data.frame or tibble, or a string specifying the path to the RDS file,
#'             containing the classification results data.
#' @param config A list containing configuration options for the Shiny app (e.g., API and web host URLs, recording duration).
#' @param test_subset A numeric value between 0 and 1 specifying the proportion of the data to use for testing purposes.
#'                    Default is NA, meaning all data will be used.
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
#' @importFrom dplyr anti_join arrange bind_rows distinct filter group_by left_join mutate row_number select summarise slice_max slice_sample tibble ungroup
#' @importFrom DT DTOutput JS renderDT datatable
#' @importFrom jsonlite toJSON
#' @importFrom lubridate days floor_date ymd_hms
#' @importFrom magrittr %>%
#' @importFrom plotly add_trace event_data layout plot_ly plotlyOutput renderPlotly event_register
#' @importFrom shiny actionButton conditionalPanel div fluidPage checkboxInput includeScript mainPanel observeEvent radioButtons shinyApp sidebarLayout sidebarPanel selectInput tags tagList textOutput titlePanel uiOutput
#' @importFrom shinyauthr loginServer logoutServer
#' @importFrom shinyjs disable enable
#' @importFrom stringr str_detect
#' @importFrom stats end start
#' @importFrom scales brewer_pal
#' @importFrom tidyr complete
#' @importFrom utils head modifyList

#'
#' @export
launchServer <- function (data, config = list(), test_subset = NA) {


  if (is.character(data)) {
    data <- readRDS(data)
  }

  if (!is.na(test_subset)) {

    num = round(nrow(data) * test_subset)
    data <- data %>% dplyr::slice_sample(n = num)
  }

  config <- getConfig(config)
  ui <- getUI(data, config)

  # Run the application
  server <- getServer(data, config)

  shiny::shinyApp(ui = ui, server = server)

}






#todo: map a label to something like a species name for display
mapLabel <- function (label) {
  return(label)
}







