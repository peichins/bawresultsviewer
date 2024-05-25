#' Add Download Link
#'
#' Adds a column with a download link for audio files hosted on a specific server.
#'
#' @param df A data frame containing information about audio files.
#' @param host The hostname (e.g., "api.acousticobservatory.org") where the audio files are hosted.
#' @param arid A character or integer vector of audio recording IDs.
#' @param start A numeric vector of start offsets (in seconds) for each audio file.
#' @param duration A single number or numeric vector of durations (in seconds) for each audio file.
#'
#' @return The modified data frame with a new `download` column containing the download links.
#'
#' @export
add_download_link <- function(df, host, arid, start, duration) {
  arid <- as.character(arid)
  start <- as.numeric(start)
  duration <- as.numeric(duration)
  df$download <- sprintf(
    'https://%s/audio_recordings/%s/media.wav?start_offset=%s&end_offset=%s',
    host, arid, start, start + duration
  )
  return(df)
}

#' Add Listen Link
#'
#' Adds a column with a link to listen to audio files hosted on a specific server.
#'
#' @param df A data frame containing information about audio files.
#' @param host The hostname (e.g., "data.acousticobservatory.org") where the audio files are hosted.
#' @param arid A character or integer vector of audio recording IDs.
#' @param start A numeric vector of start offsets (in seconds) for each audio file.
#' @param duration  A single number or numeric vector of durations (in seconds) for each audio file.
#'
#' @return The modified data frame with a new `listen` column containing the listen links.
#'
#' @export
add_listen_link <- function(df, host, arid, start, duration) {
  arid <- as.character(arid)
  start <- as.numeric(start)
  duration <- as.numeric(duration)
  df$listen <- sprintf(
    'https://%s/listen/%s?start=%s&end=%s',
    host, arid, start, start + duration
  )
  return(df)
}
