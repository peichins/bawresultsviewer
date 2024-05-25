
getUI <- function () {
  fluidPage(
    titlePanel(config$title),
    sidebarLayout(
      sidebarPanel(
        uiOutput("modeRadio"),
        uiOutput("scoreSlider"),
        uiOutput("speciesSelector"),
        uiOutput("siteSelector"),
        uiOutput("dateRangePicker"),
        selectInput("intervalInput", "Aggregation Interval:",
                    choices = c("1 hour" = "hour", "1 day" = "day", "1 week" = "week", "1 month" = "month"),
                    selected = "day"),
        textOutput("messageBox")
      ),
      mainPanel(
        plotlyOutput("timeSeriesPlot"),
        DTOutput("detailsTable")
      )
    )
  )
}

