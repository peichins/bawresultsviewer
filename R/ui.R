
getUI <- function (config) {

  auth_panel <- div()
  if (config$require_auth) {
    auth_panel <- div(
      div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
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
          uiOutput("modeRadio"),
          checkboxInput("onlyHighestCheckbox", "Show only higest scoring", value = FALSE),
          uiOutput("scoreSlider"),
          uiOutput("speciesSelector"),
          uiOutput("siteSelector"),
          uiOutput("dateRangePicker"),
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

