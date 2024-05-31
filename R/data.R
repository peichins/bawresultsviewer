#' Bird Observation Data
#'
#' A subset of bird detections with label and site names changed
#'
#' @format ## `who`
#' A data frame with 5000 rows and 6 columns:
#' \describe{
#'   \item{site}{site name (factor)}
#'   \item{arid}{int}
#'   \item{offset_seconds}{Offset from start of file in seconds}
#'   \item{timestamp}{datetime of start of detection}
#'   \item{label}{Label (factor)}
#'   \item{score}{}
#'   ...
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
"bird_observations"
