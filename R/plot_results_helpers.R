
# depending on the total date range and aggregation interval size,
# we can show different breaks in the plot to make things look as good as possible
determineBreaks <- function (interval_list, interval_input) {

  min_date = min(interval_list)
  max_date = max(interval_list)

  # Calculate the range of dates
  total_days <- as.numeric(difftime(max_date, min_date, units = "days"))

  # Determine appropriate scale for date breaks
  date_breaks <- ifelse(total_days > 180, "1 month",
                        ifelse(total_days > 14, "1 week",
                               ifelse(total_days > 1, "1 day", "1 hour")))

  # Ensure date_breaks are not finer than the aggregation interval
  if (interval_input == "month" && date_breaks == "1 week") {
    date_breaks <- "1 month"
  } else if (interval_input == "week" && (date_breaks == "1 day" || date_breaks == "1 hour")) {
    date_breaks <- "1 week"
  } else if (interval_input == "day" && date_breaks == "1 hour") {
    date_breaks <- "1 day"
  }

  return(list(
    min_date = min_date,
    max_date = max_date,
    total_days = total_days,
    date_breaks = date_breaks
  ))


}

