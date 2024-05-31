
selectData <- function (data, input) {
  if (is.data.frame(data)) {
    selected_data <- selectDataTibble(data, input)
  } else {
    selected_data <- selectDataSql(data, input)
  }
  return(selected_data)
}



#' Select Data Based on User Inputs
#'
#' Filters the input data tibble based on the criteria specified in the Shiny app's user interface (UI) elements.
#'
#' @param data A tibble containing the raw data to be filtered.
#' @param input A list containing the user's input values from the Shiny app's UI elements.
#'   It should have at least the following components:
#'   - `speciesInput`: A vector of selected species labels.
#'   - `scoreInput`: A numeric vector of length 2 defining the minimum and maximum score range.
#'   - `dateInput`: A vector of Date objects defining the start and end date range.
#'   - `siteInput` (optional): A vector of selected site names. If NULL or empty, all sites are included.
#'
#' @return A filtered tibble containing only the rows that match the user's selection criteria.
#'
#' @examples
#' \dontrun{
#' # Example usage assuming you have a 'data' tibble and a Shiny 'input' object
#' filtered_data <- selectDataTibble(data, input)
#' }
selectDataTibble <- function (data, input) {

  selected_data <- data %>%
    dplyr::filter(
      label %in% input$speciesInput,
      score >= input$scoreInput[1],
      score <= input$scoreInput[2],
      timestamp >= input$dateInput[1],
      timestamp <= input$dateInput[2] + lubridate::days(1),
      if (!is.null(input$siteInput) && length(input$siteInput) > 0) site %in% input$siteInput else TRUE
    )

  return(selected_data)

}

#' Select Top-Scoring Observation per Interval, Label, and Site
#'
#' Groups the input data by time intervals, labels (species), and sites.
#' Within each group, it selects the row with the highest score, discarding any ties.
#'
#' @param data A tibble containing the data to be processed. It must have columns named:
#'   - `timestamp`:  A column of class `POSIXct` representing the timestamps.
#'   - `label`: A column indicating the species label.
#'   - `score`:  A numeric column representing the score.
#'   - `site`: A column indicating the site.
#' @param input A list containing the user input values from the Shiny app's UI elements.
#'    It should contain at least the following component:
#'    - `intervalInput`: A character string specifying the time interval for aggregation (e.g., "hour", "day", "week", "month").
#'
#' @return A tibble containing the top-scoring row for each combination of interval, label, and site.
#'
#' @examples
#' \dontrun{
#' # Example usage assuming you have a 'data' tibble and a Shiny 'input' object
#' top_scoring_data <- selectTopScoringTibble(data, input)
#' }
selectTopScoringTibble <- function (data, input) {
  selected_data <- data %>%
    dplyr::mutate(interval = lubridate::floor_date(timestamp, unit = input$intervalInput)) %>%
    dplyr::group_by(interval, label, site) %>%
    dplyr::slice_max(score, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-interval)

  return(selected_data)

}


selectDataSql <- function (connection, input) {
  query <- getSelectQuery(input)
  selected_data <- DBI::dbGetQuery(connection, query)
  return(selected_data)
x}
