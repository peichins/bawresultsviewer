
default_config <- list(

  # where audio is downloaded from
  api_host = "api.acousticobservatory.org",

  # where the listen link points to
  web_host = "data.acousticobservatory.org",

  baw_instance = 'a2o',

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

  max_table_rows = 50000,

  auth = FALSE
)


baw_instances <- list(
  a2o = list(
    api_host = "api.acousticobservatory.org",
    web_host = "data.acousticobservatory.org"
  ),
  ecosounds = list(
    api_host = "api.ecosounds.org",
    web_host = "www.ecosounds.org"
  )
)

preprocessConfig <- function (config) {

  if (is.character(config$auth)) {
    config$require_auth <- TRUE
  } else if (is.logical(config$auth) && !config$auth) {
    config$require_auth <- FALSE
  } else {
    stop("Invalid value for config$auth. It should be a filepath to an RDS file or FALSE.")
  }

  # config is a flat list, and baw config values are part of the flat list
  # if a baw_instance is specified, then we look for the corresponding config values in the baw instance list and
  # then merge them into the flat config list. If api_host or web_host are specified in the flat config list, they
  # will be overwritten by the values in the baw instance list.
  if (is.character(config$baw_instance)) {
    if (config$baw_instance %in% names(baw_instances)) {
      config <- modifyList(config, baw_instances[[config$baw_instance]])
    } else {
      stop("Invalid value for config$baw_instance. It should be one of ", paste(names(baw_instances), collapse = ", "))
    }
  } else {
    stop("Invalid value for config$baw_instance. It should be a string and one of ", paste(names(baw_instances), collapse = ", "))
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
