getRange <- function (data, col) {

  if (is.data.frame(data)) {
    min <- min(data[[col]], na.rm = TRUE)
    max <- max(data[[col]], na.rm = TRUE)
  } else {
    # data is a dbi connection. Use sql to get the min and max of the columns
    min <- dbGetQuery(data, paste0("SELECT MIN(", col, ") FROM data"))[[1]]
    max <- dbGetQuery(data, paste0("SELECT MAX(", col, ") FROM data"))[[1]]
  }
  return(list(min = min, max = max))

}

