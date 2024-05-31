#' Get Counts of Time-Series Data
#'
#' This function processes time-series data to generate counts of occurrences
#' within specified time intervals. It also ensures that all combinations of
#' intervals and labels are included in the result, filling in zeros where data
#' is missing.
#'
#' @param data A tibble containing time-series data. Must include columns:
#'        `timestamp`, `label`, and `row_id`.
#' @param interval A string representing the unit for floor_date (e.g., "day",
#'        "hour", "minute").
#' @param labels (optional) A character vector of unique labels to include in the result.
#'    If NULL (default), all unique labels in the `data` will be used.
#' @return A tibble with counts for each combination of interval and label.
#' @importFrom dplyr mutate group_by summarise left_join n
#' @importFrom tidyr crossing replace_na
#' @importFrom lubridate floor_date
#' @importFrom stats setNames
#' @export
getCounts <- function (data, interval, labels=NULL) {
  data <- getCountsNoZeros(data, interval)
  data <- fillZeros(data, interval, labels)
  return(data)
}

getCountsNoZeros <- function (data, interval) {

  data <- data %>%
    dplyr::mutate(interval = lubridate::floor_date(timestamp, unit = interval)) %>%
    dplyr::group_by(interval, label) %>%
    dplyr::summarise(count = dplyr::n(), .groups = 'drop', row_ids = list(row_id)) %>%
    droplevels() %>%
    dplyr::rename(timestamp = interval)

  return(data)

}



fillZeros <- function (data, interval, labels=NULL) {


  if (is.null(labels)) {
    labels <- unique(data$label)
  }

  # create a tibble with all possible combinations of timestamps and labels
  all_period_timestamps <- tidyr::crossing(
    timestamp = seq.POSIXt(
      from = min(data$timestamp),
      to = max(data$timestamp),
      by = interval
    ),
    label = labels
  )

  # left join on the data to fill counts where there are some
  # then replace missing with zeros
  data <- all_period_timestamps %>%
    dplyr::left_join(data, by = c("timestamp", "label")) %>%
    dplyr::mutate(
      count = tidyr::replace_na(count, 0),
      row_ids = tidyr::replace_na(row_ids, list(integer(0)))
    )

  return(data)

}


