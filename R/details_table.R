getDetailsTable <- function (data, config) {

  click_data <- event_data("plotly_click", source = "plotSource")

  if (!is.null(click_data) && !is.null(click_data$customdata)) {
    # Retrieve original rows based on row IDs stored in customdata of the clicked point
    row_ids <- unlist(click_data$customdata)
    original_data <- data %>%
      dplyr::filter(row_id %in% row_ids) %>%
      mutate(
        listen = sprintf(
          '<a href="https://%s/listen/%s?start=%s&end=%s" target="_blank">Listen</a>',
          config$web_host, arid, pmax(0, offset_seconds - 1),
          pmin(config$recording_duration_seconds, offset_seconds + 1 + config$clip_duration_seconds)),
        download = sprintf(
          '<a href="https://%s/audio_recordings/%s/media.wav?start_offset=%s&end_offset=%s" target="_blank">Download</a>',
          config$api_host, arid, offset_seconds, offset_seconds + config$clip_duration_seconds),
        species = mapLabel(label),
        date = timestamp  # Assuming 'timestamp' is the datetime column from 'data'
      ) %>%
      select(species, site, date, score, listen, download)  # Select relevant columns for display

    print(head(original_data))

    dt <- datatable(original_data, options = list(pageLength = 5), escape = FALSE)  # 'escape = FALSE' allows HTML rendering of URLs
  } else {
    dt <- datatable(data.frame(), options = list(pageLength = 5))
  }

  return(dt)

}
