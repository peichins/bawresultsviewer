authServer <- function(input, output, session, config) {

  credentials <- NULL

  # Determine authentication requirement once at the start
  if (config$require_auth) {
    # if config auth is missing or invalid, we should crash here
    user_base <- readRDS(config$auth)
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = user_base,
      user_col = user,
      pwd_col = password,
      sodium_hashed = TRUE
    )

    is_authorized <- function() {
        return(!is.null(credentials) && credentials()$user_auth)
    }
    logout_init <- shinyauthr::logoutServer(id = "logout", active = reactive(credentials()$user_auth))

  } else {
    is_authorized <- function ()  TRUE
    logout_init <- NULL
  }

  # Expose values
  list(
    is_authorized = is_authorized,
    logout_init = logout_init
    )

}


