#' Format Results
#'
#' Extracts site, ARID (audio_recording_id), and timestamp information from
#' a filename column in BAW results data. Assumes the filename follows the
#' canonical format: `[file_start_timestamp]_[site_name]_[arid].[ext]`.
#'
#' @param source The path to the RDS file containing the results data.
#' @param col_map A named list specifying the column names in the input data.
#'   Default: `list(filename = "filename", offset_seconds = "timestamp_s")`.
#' @param random_subset To test quickly, supply a number to reduce the computation time
#'
#' @return A tibble with the original data and new columns:
#'   * `site`: The site name extracted from the filename.
#'   * `arid`: The ARID extracted from the filename.
#'   * `timestamp`: The calculated timestamp, combining file start time and offset.
#'
#' @importFrom dplyr mutate select rename
#' @importFrom lubridate ymd_hms with_tz seconds
#' @importFrom stringr str_extract str_remove str_detect
#' @importFrom readr read_rds
#'
#' @details
#' Generally files analysed from a baw server use a filename containing the start datetime, site and arid. This function assumes the filename column
#' contains filenames in this format.
#'
#'
#' @examples
#' \dontrun{
#' data_path <- system.file("extdata", "example_baw_results.rds", package = "yourpackage")
#' formatted_data <- formatResults(data_path)
#' }
#' @export
formatResults <- function(source, col_map = list(filename = "filename", offset_seconds = "timestamp_s"), random_subset = NA) {

  # Load the data
  data <- readr::read_rds(source)

  if (!is.na(random_subset)) {
    data <- data %>% slice_sample(n = random_subset)
  }

  # rename filename and offset columns to consistent names
  data <- data %>% rename(setNames(col_map$filename, 'filename'))
  data <- data %>% rename(setNames(col_map$offset_seconds, 'offset_seconds'))

  data <- data %>%  mutate(filename= basename(as.character(filename)))
  data <- data %>%  mutate(site = stringr::str_extract(filename, "(?<=_).+(?=_[^_.]+\\.[^_.]+$)"))
  data <- data %>%  mutate(arid = stringr::str_extract(filename, "(?<=_)[^_.]+(?=\\.[^_.]+$)"))
  data <- data %>%  mutate(file_start_timestamp = stringr::str_extract(filename, "^\\d{8}T\\d{6}(Z|[+-]\\d{4})"))
  data <- data %>%  mutate(utc_offset = stringr::str_extract(file_start_timestamp, "Z|[+-]\\d{4}"))
  data <- data %>%  mutate(file_start_timestamp = stringr::str_extract(file_start_timestamp, "^\\d{8}T\\d{6}"))
  data <- data %>%  mutate(timestamp = lubridate::ymd_hms(file_start_timestamp) + lubridate::seconds(offset_seconds))
  data <- data %>%  select(-file_start_timestamp)


  return(data)
}
