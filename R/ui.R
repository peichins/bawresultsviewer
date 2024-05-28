
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


  auth_panel <- div()
  if (config$require_auth) {
    auth_panel <- div(
      shinyauthr::loginUI(id = "login")
    )
  }


  fluidPage(
    shinyjs::useShinyjs(),
    titlePanel(config$title),
    sidebarLayout(
      sidebarPanel(
        auth_panel,
        conditionalPanel(
          condition = "output.is_authorized",
          radioButtons("mode", "Mode:", choices = c("Counts" = "counts", "Scatter" = "scatter"), selected = "counts", inline = TRUE),
          checkboxInput("onlyHighestCheckbox", "Show only higest scoring", value = FALSE),
          tagList(
              tags$label("Score Range:"),
              tags$a("reset", href = "#", onclick = "Shiny.onInputChange('resetSlider', Math.random())"),
              sliderInput("scoreInput", label = NULL, min = min_score, max = max_score, value = default_score_selection)
          ),
          selectInput("siteInput", "Select Sites (if none will include all):",
                      choices = unique_sites, selected = list(), multiple = TRUE),
          selectInput("speciesInput", "Select Species:",
                      choices = label_list, selected = label_list[selected_species], multiple = TRUE),
          dateRangeInput("dateInput", "Select Date Range:",
                         start = min_date,
                         end = max_date,
                         min = min_date,
                         max = max_date),
          selectInput("intervalInput", "Aggregation Interval:",
                      choices = c("1 hour" = "hour", "1 day" = "day", "1 week" = "week", "1 month" = "month"),
                      selected = "day"),
          textOutput("messageBox"),
          uiOutput("resetTableButton")
        )
      ),
      mainPanel(
        conditionalPanel(
          condition = "output.is_authorized",
          plotlyOutput("timeSeriesPlot"),
          DTOutput("detailsTable")
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
  min_score <- min(data$score, na.rm = TRUE)
  max_score <- max(data$score, na.rm = TRUE)
  default_score_selection <- c(max(0, min_score), max_score)
  return(default_score_selection)
}
