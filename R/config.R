
default_config <- list(

  # where audio is downloaded from
  api_host = "api.acousticobservatory.org",

  # where the listen link points to
  web_host = "data.acousticobservatory.org",

  # used to stop padding going past end of recording.
  # If unknown leave as a really high number and occasionally the link will error
  # todo: ideally we will have access to durations of all audio, so we can also normalize counts by recording coverage
  recording_duration_seconds = 60*60*24,

  # duration of clip to link to in baw
  clip_duration_seconds = 5,

  # how many of the random species to show when launched
  initial_num_species = 1,

  # main title of page
  title = 'Bird Observations Over Time',

  # too many points causes the scatter plot to load very slowly, so
  # take a random subset of this size if the number of points exceeds it
  max_scatter_points = 2000,

  auth = FALSE
)

preprocessConfig <- function (config) {

  if (is.character(config$auth)) {
    config$require_auth <- TRUE
  } else if (is.logical(config$auth) && !config$auth) {
    config$require_auth <- FALSE
  } else {
    stop("Invalid value for config$auth. It should be a filepath to an RDS file or FALSE.")
  }

  return(config)

}

getConfig <- function(config1 = list()) {
  config <- modifyList(default_config, list())

  if (file.exists("config.json")) {
    # Read JSON file into a list
    config2 <- fromJSON("config.json")
    cat("Loaded config.json successfully!\n")
    config <- modifyList(config, config2)
  }

  config <- modifyList(config, config1)
  return(preprocessConfig(config))
}
