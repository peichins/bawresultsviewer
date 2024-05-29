getDetailsTable <- function (data, config, input, output, point_clicked) {

  click_data <- event_data("plotly_click", source = "plotSource")

  output$resetTableButton <- renderUI({
    if (point_clicked()) {
      actionButton("resetTableButton", "Reset Table")
    }
  })

  head(data)

  if (point_clicked()) {
    # Retrieve rows based on row IDs stored in customdata of the clicked point
    row_ids <- unlist(click_data$customdata)
    data <- data %>%
      dplyr::filter(row_id %in% row_ids)
  }

  if (nrow(data) > config$max_table_rows) {
    #putMessage("Too many rows for table")
    return(datatable(data.frame(), options = list(pageLength = 5)))
  }

  fixed_params <- jsonlite::toJSON(list(
    web_host = config$web_host,
    api_host = config$api_host,
    clip_duration = config$clip_duration,
    clip_duration = config$recording_duration
  ), auto_unbox = TRUE)

  print(fixed_params)

  rownums <- jsonlite::toJSON(list(
    listen = 5, download = 6, arid = 7, offset_seconds = 8
  ))

  dt_options <- list(
    pageLength = 5,
    escape = FALSE,
    rowCallback = JS(paste("
    function(row, data, index, rowId) {
        var fixed_params = ", fixed_params, ";
        var rownums = ", rownums, ";
        updateRow(row, data, index, fixed_params, rownums);
        }")),
    columnDefs = list(list(visible=FALSE, targets=c(7, 8)))
  )

  # 'escape = FALSE' allows HTML rendering of URLs
  data <- data %>% mutate(
    species = mapLabel(label),
    date = timestamp,
    # these are empty now, and will get populated by javascript clientside
    listen = "", download = ""
  ) %>%
    # this order must match the rowCallbackJavascript
    select(species, site, date, score, listen, download, arid, offset_seconds)

  dt <- DT::datatable(data, options = dt_options, escape = FALSE)
  return(dt)
}
