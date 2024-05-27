getDetailsTable <- function (data, config, output, point_clicked) {

  click_data <- event_data("plotly_click", source = "plotSource")

  output$resetTableButton <- renderUI({
    if (point_clicked()) {
      actionButton("resetTableButton", "Reset Table")
    }
  })

  if (point_clicked()) {
    # Retrieve rows based on row IDs stored in customdata of the clicked point
    row_ids <- unlist(click_data$customdata)
    data <- data %>%
      dplyr::filter(row_id %in% row_ids)
  }

  dt_options <- list(
    pageLength = 5,
    escape = FALSE,
    rowCallback = JS(
      "function(row, data, index, rowId) {",
      "  var arid = data[7];",
      "  var offset_seconds = data[8];",
      sprintf("  var web_host = '%s';", config$web_host),
      sprintf("  var api_host = '%s';", config$api_host),
      sprintf("  var clip_duration = '%s';", config$clip_duration_seconds),
      sprintf("  var recording_duration = '%s';", config$recording_duration_seconds),
      "  var listen_link = `<a href='https://${web_host}/listen/${arid}?start=${Math.max(0, offset_seconds - 1)}&end=${Math.min(recording_duration, offset_seconds + 1 + clip_duration)}' target='_blank'>Listen</a>`;",
      "  var download_link = `<a href='https://${api_host}/audio_recordings/${arid}/media.wav?start_offset=${offset_seconds}&end_offset=${offset_seconds + clip_duration}' target='_blank'>Download</a>`;",
      "  $('td:eq(5)', row).html(listen_link);",
      "  $('td:eq(6)', row).html(download_link);",
      "}"
    ),
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

  dt <- datatable(data, options = dt_options, escape = FALSE)
  return(dt)
}
