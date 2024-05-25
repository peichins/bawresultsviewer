getDetectionCountPlot <- function (df, input, output) {

  output$messageBox <- renderText("")

  breaks_info <- determineBreaks(df$interval, input$intervalInput)

  # Print out summaries to debug the subsets
  print(paste("Total data points:", nrow(df)))

  # Create a dynamic color palette
  num_labels <- length(unique(df$label))
  color_palette <- scales::brewer_pal(palette = "Set2")(max(3, num_labels))


  p <- plot_ly(source = "plotSource")

  # Define different line styles
  line_styles <- c("solid", "dash", "dot")

  # Adding traces manually for each combination of species and site type
  for (label_name in unique(df$label)) {
    subset_df <- df %>% dplyr::filter(label == label_name)
    line_style <- "solid"
    p <- p %>% add_trace(data = subset_df,
                         x = ~interval, y = ~count,
                         type = 'scatter', mode = 'lines',
                         line = list(dash = line_style, width = 2),
                         color = ~label,
                         colors = color_palette,
                         name = ~label,
                         text = ~paste(label, format(interval, "%Y-%m-%d %H:%M:%S"), "with", count, "detections"),
                         hoverinfo = 'text',
                         customdata = ~row_ids)
  }

  p <- p %>% layout(
    xaxis = list(title = 'Date'),
    yaxis = list(title = 'Count'),
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
