#' Save a Tibble of Bird Observations to a SQLite Database
#'
#' Saves a tibble containing bird observation data to a SQLite database.
#' It efficiently creates or updates lookup tables for sites, labels (species), and ARIDs,
#' and then inserts the observation data into a `detections` table using integer IDs for space optimization.
#'
#' @param data A tibble (or data frame) containing the bird observation data.
#' @param db_path The path to the SQLite database file (e.g., "bird_observations.sqlite").
#'
#' @details
#' This function performs the following steps:
#' 1. Creates lookup tables (`sites`, `labels`, `arids`) if they don't exist.
#' 2. Populates the lookup tables with unique values from the `data` tibble.
#' 3. Converts the `site`, `label`, and `arid` columns in `data` to integer IDs based on the lookup tables.
#' 4. Creates a `detections` table if it doesn't exist.
#' 5. Inserts the data into the `detections` table using the integer IDs for space optimization.
#'
#' @note This function uses prepared statements for efficient and secure SQL execution.
#'
#' @export
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbSendQuery dbBind dbClearResult dbAppendTable dbWriteTable
#' @importFrom RSQLite SQLite
#' @importFrom dplyr mutate
saveTibbleToDb <- function(data, db_path) {

  # first, we normalize the data into sites, labels and recordings tables
  # this is mainly to save space

  # create a sites, labels and recordings tibbles with ids
  sites <- tibble(site = unique(data$site)) %>%
    mutate(site_id = row_number()) %>%
    select(site_id, site)

  audio_recordings <- tibble(arid = unique(data$arid)) %>%
    mutate(arid_id = row_number()) %>%
    select(arid_id, arid)

  labels <- tibble(label = unique(data$label)) %>%
    mutate(label_id = row_number()) %>%
    select(label_id, label)

  #replace the site, label and arid columns in data with the ids
  data <- data %>%
    left_join(sites, by = "site") %>%
    left_join(labels, by = "label") %>%
    left_join(audio_recordings, by = "arid") %>%
    select(-site, -label, -arid)



  db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(dbDisconnect(db))  # Ensure connection is closed

  # use dbWriteTable to create the sites, labels and recordings tables and the detections table
  DBI::dbWriteTable(db, "sites", sites, row.names = FALSE, overwrite = TRUE)
  DBI::dbWriteTable(db, "labels", labels, row.names = FALSE, overwrite = TRUE)
  DBI::dbWriteTable(db, "audio_recordings", audio_recordings, row.names = FALSE, overwrite = TRUE)



  # Create detections Table with Foreign Keys
  dbExecute(db, "CREATE TABLE IF NOT EXISTS detections (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 label_id INTEGER REFERENCES labels(label_id),
                 timestamp DATETIME,
                 score REAL,
                 site_id INTEGER REFERENCES sites(site_id),
                 arid_id INTEGER REFERENCES audio_recordings(arid_id),
                 offset_seconds INTEGER
                 );")

  DBI::dbAppendTable(db, "detections", data)


  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_detections_label_id ON detections (label_id);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_detections_site_id ON detections (site_id);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_detections_arid_id ON detections (arid_id);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_detections_timestamp ON detections (timestamp);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_detections_timestamp ON detections (timestamp);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_detections_score ON detections (score);")

}




# saveTibbleToDb <- function(data, db_path) {
#
#   print("one")
#
#
#   db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
#   on.exit(dbDisconnect(db))  # Ensure connection is closed
#
#   dbExecute(db,"CREATE TABLE IF NOT EXISTS sites (site_id INTEGER PRIMARY KEY AUTOINCREMENT, site TEXT UNIQUE);")
#   dbExecute(db,"CREATE TABLE IF NOT EXISTS labels (label_id INTEGER PRIMARY KEY AUTOINCREMENT, label TEXT UNIQUE);")
#   dbExecute(db,"CREATE TABLE IF NOT EXISTS arids (arid_id INTEGER PRIMARY KEY AUTOINCREMENT, arid INTEGER UNIQUE);")
#
#   # Populate Lookup Tables with Prepared Statements
#   site_insert_query <- "INSERT INTO sites (site) VALUES (?);"
#   label_insert_query <- "INSERT INTO labels (label) VALUES (?);"
#   arid_insert_query <- "INSERT INTO arids (arid) VALUES (?);"
#
#
#   site_stmt <- dbSendStatement(db, site_insert_query)
#   label_stmt <- dbSendStatement(db, label_insert_query)
#   arid_stmt <- dbSendStatement(db, arid_insert_query)
#
#   dbBind(site_stmt, list(as.character(unique(data$site))))
#   dbGetRowsAffected(site_stmt)
#   dbClearResult(site_stmt)
#
#
#   for (label_value in unique(data$label)) {
#     dbBind(label_stmt, list(label_value))
#   }
#   for (arid_value in unique(data$arid)) {
#     dbBind(arid_stmt, list(arid_value))
#   }
#
#   dbClearResult(site_stmt)
#   dbClearResult(label_stmt)
#   dbClearResult(arid_stmt)
#
#   # Convert site and label names to IDs
#   data_with_ids <- data %>%
#     mutate(
#       label_id = as.integer(factor(label)),
#       site_id = as.integer(factor(site)),
#       arid_id = as.integer(factor(arid))
#     )
#
#   # Create detections Table (no change)
#   # ... your existing code for table creation ...
#
#   # Insert data into Detections Table with Prepared Statement
#   detections_insert_query <- paste0(
#     "INSERT INTO detections (label_id, score, timestamp, site_id, arid_id) VALUES (",
#     paste(rep("?", 5), collapse = ","),
#     ");"
#   )
#   insert_stmt <- dbSendQuery(db, detections_insert_query)
#   dbBind(insert_stmt, as.data.frame(data_with_ids))
#   dbClearResult(insert_stmt)
# }

#
#
# saveTibbleToDb <- function(data, db_path) {
#   db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
#   on.exit(dbDisconnect(db))
#
#
#   query <- paste("CREATE TABLE IF NOT EXISTS sites (site_id INTEGER PRIMARY KEY AUTOINCREMENT,site TEXT UNIQUE);",
#                  "CREATE TABLE IF NOT EXISTS labels (label_id INTEGER PRIMARY KEY AUTOINCREMENT,label TEXT UNIQUE);",
#                  "CREATE TABLE IF NOT EXISTS arids (arid_id INTEGER PRIMARY KEY AUTOINCREMENT,arid INTEGER UNIQUE);")
#   dbExecute(db, query)
#
#
#   for(site_value in unique(data$site)){
#     query <- paste0("INSERT OR IGNORE INTO sites (site) VALUES('", site_value, "')")
#     dbExecute(db, query)
#   }
#   for(label_value in unique(data$label)){
#     query <- paste0("INSERT OR IGNORE INTO labels (label) VALUES('", label_value, "')")
#     dbExecute(db, query)
#   }
#   for(arid_value in unique(data$arid)){
#     query <- paste0("INSERT OR IGNORE INTO arids (arid) VALUES('", arid_value, "')")
#     dbExecute(db, query)
#   }
#
#   # 2. Convert site and label names to IDs
#   data_with_ids <- data %>%
#     mutate(
#       label_id = as.integer(factor(label)),
#       site_id = as.integer(factor(site)),
#       arid_id = as.integer(factor(arid))
#     )
#
#   # 3. Create detections Table
#   table_name <- "detections"
#   query <- paste0(
#     "CREATE TABLE IF NOT EXISTS ", table_name, " (",
#     "id INTEGER PRIMARY KEY AUTOINCREMENT,",
#     "label_id INTEGER,",
#     "score REAL,",
#     "timestamp DATETIME,",
#     "site_id INTEGER",
#     "arid_id INTEGER",
#     ");"
#   )
#   dbExecute(db, query)
#
#   # 4. Insert data into Detections Table
#   if (nrow(data_with_ids) > 0) {
#     dbAppendTable(db, table_name, data_with_ids)
#   }
#
#   dbDisconnect(db)
# }
#

