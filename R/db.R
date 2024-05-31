db <- function (db_file) {
  RSQLite::dbConnect(RSQLite::SQLite(), db_file)
}
