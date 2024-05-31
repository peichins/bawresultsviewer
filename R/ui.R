
getUI <- function (data, config) {

  min_score <- min(data$score, na.rm = TRUE)
  max_score <- max(data$score, na.rm = TRUE)
  default_score_selection <- getDefaultScoreSelection(data)

  unique_labels <- sort(as.character(unique(data$label)))
  unique_sites <- sort(as.character(unique(data$site)))

  label_list <- as.list(unique_labels)
  names(label_list) <- mapLabel(unique_labels)
  selected_species <- initialSelectedSpecies(unique_labels)

  min_date <- min(data$timestamp, na.rm = TRUE)
  max_date <- max(data$timestamp, na.rm = TRUE)

  auth_panel <- shiny::div()
  if (config$require_auth) {
    auth_panel <- shiny::div(
      shinyauthr::loginUI(id = "login")
    )
  }



  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::includeScript(system.file("www/js/table_callbacks.js", package = "bawresultsviewer"))
    ),
    shiny::titlePanel(config$title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        auth_panel,
        shiny::conditionalPanel(
          condition = "output.is_authorized",
          shiny::radioButtons("mode", "Mode:", choices = c("Counts" = "counts", "Scatter" = "scatter"), selected = "counts", inline = TRUE),
          shiny::checkboxInput("onlyHighestCheckbox", "Show only higest scoring", value = FALSE),
          shiny::tagList(
            shiny::tags$label("Score Range:"),
            shiny::tags$a("reset", href = "#", onclick = "Shiny.onInputChange('resetSlider', Math.random())"),
            shiny::sliderInput("scoreInput", label = NULL, min = min_score, max = max_score, value = default_score_selection)
          ),
          shiny::selectInput("siteInput", "Select Sites (if none will include all):",
                             choices = unique_sites, selected = list(), multiple = TRUE),
          shiny::selectInput("speciesInput", "Select Species:",
                             choices = label_list, selected = label_list[selected_species], multiple = TRUE),
          shiny::dateRangeInput("dateInput", "Select Date Range:",
                                start = min_date,
                                end = max_date,
                                min = min_date,
                                max = max_date),
          shiny::selectInput("intervalInput", "Aggregation Interval:",
                             choices = c("1 hour" = "hour", "1 day" = "day", "1 week" = "week", "1 month" = "month"),
                             selected = "day"),
          shiny::textOutput("messageBox"),
          shiny::uiOutput("resetTableButton"),
          # text input to specify user_token for download link
          shiny::textInput("user_token", "Enter user token")
        )
      ),
      shiny::mainPanel(
        shiny::conditionalPanel(
          condition = "output.is_authorized",
          plotly::plotlyOutput("timeSeriesPlot"),
          DT::DTOutput("detailsTable")
        )
      )
    )
  )
}



initialSelectedSpecies <- function (label_list, initial_num_species = 3, show_all_if_under = 8) {
  if (length(label_list) < show_all_if_under) {
    seq_along(label_list)
  } else {
    sample(seq_along(label_list), min(initial_num_species, length(values)))
  }
}

getDefaultScoreSelection <- function (data) {
  # determine input values from data. These are static and will not change
  range <- getRange(data, 'score')
  default_score_selection <- c(max(0, range$min), range$max)
  return(default_score_selection)
}
