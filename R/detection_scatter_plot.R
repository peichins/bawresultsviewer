getDetectionScatterPlot <- function (df, input, output, max_detections = 2000) {

  if (nrow(df) > max_detections) {
    message <- paste("Too many points to plot, selecting", max_detections, "of", nrow(df), "at random")
    print(message)
    output$messageBox <- renderText(message)
    df <- df %>% slice_sample(n = max_detections)
  } else {
    output$messageBox <- renderText("")
  }

  # Print out summaries to debug the subsets
  print(paste("Total data points:", nrow(df)))

  # Create a dynamic color palette
  num_labels <- length(unique(df$label))
  color_palette <- scales::brewer_pal(palette = "Set2")(max(3, num_labels))

  p <- plot_ly(source = "plotSource")

  print(num_labels)

  # Adding traces manually for each combination of species

    p <- p %>% add_trace(
      data = df,
      x = ~timestamp,
      y = ~score,
      type = "scatter",
      mode = "markers",
      color = ~label,  # Color by species
      colors = color_palette,
      text = ~paste0(label, ":", score, ":", timestamp),
      hoverinfo = "text",
      customdata = ~row_id
    ) %>%
      layout(yaxis = list(title = "Score"))


  p <- p %>% layout(
    xaxis = list(title = 'Date'),
    yaxis = list(title = 'Score'),
    hovermode = 'closest',
    legend = list(title = list(text = 'Species')),
    showlegend = TRUE
  ) %>%
    layout(
      hoverlabel = list(bgcolor = "white",
                        font = list(family = "Arial", size = 12, color = "black"))
    )

  p <- p %>% event_register("plotly_click")  # Register the plotly_click event

  return(p)
}
