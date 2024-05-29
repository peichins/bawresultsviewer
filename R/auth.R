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
    #logout_init <- shinyauthr::logoutServer(id = "logout", active = reactive(credentials()$user_auth))

  } else {
    is_authorized <- function ()  TRUE
    logout_init <- NULL
  }

  # Expose values
  list(
    is_authorized = is_authorized
    )

}



#' Add Users to Authentication Database
#'
#' This function adds new users to an existing or new authentication database (RDS file)
#' for Shiny authentication.
#'
#' @param file The path to the RDS file where the user information will be stored.
#'   If the file doesn't exist, a new one will be created.
#' @param usernames A character vector containing the usernames of the new users.
#' @param passwords A character vector containing the passwords of the new users.
#' @param permissions A character vector specifying the permissions for each user (e.g., "admin", "standard").
#'   Defaults to "standard" for all users.
#' @param names (Optional) A character vector containing the full names of the users.
#'   If not provided, defaults to the usernames.
#'
#' @return A tibble containing the combined user information (existing users from the RDS file,
#'   if any, and the newly added users).
#'
#' @importFrom sodium password_store
#' @importFrom dplyr tibble
#' @importFrom readr read_rds
#'
#' @export
addUsers <- function(file, usernames, passwords, permissions = "standard", names = usernames) {
  # Create a tibble with the new user data
  user_base <- dplyr::tibble(
    user = usernames,
    password = as.character(sapply(passwords, sodium::password_store)),
    permission = permissions,
    name = names
  )

  # Check if the file exists
  if (file.exists(file)) {
    # If it does, read the existing user data and combine with the new data
    existing_users <- readr::read_rds(file)
    user_base <- rbind(existing_users, user_base)
  }

  # Save the combined user data back to the RDS file
  saveRDS(user_base, file)
  return(user_base)
}



