getTinySubset <- function (data, target_num_labels = -1, target_num_sites = -1, target_num_rows = -1) {

  num_labels <- length(unique(data$label))

  if (target_num_labels > 1){
    num_labels <- min(target_num_labels, num_labels)
  } else if (target_num_labels > 0) {
    # if target num labels is below 1, treat it as a proportion of the original number
    print('one')
    num_labels <- round(target_num_labels * num_labels)

  }

  if (num_labels == 0) {
    stop('The target number of labels is too low. Please increase the target number of labels.')
  }

  chosen_labels <- sample(unique(data$label), num_labels)
  print(paste("selecting labels: ", paste(chosen_labels, collapse = ", ")))
  data <- data[data$label %in% chosen_labels,]

  num_sites <- length(unique(data$site))


  if (target_num_sites > 1){
    num_sites <- min(target_num_sites, num_sites)
  } else if (target_num_sites > 0) {
    # if target num sites is below 1, treat it as a proportion of the original number
    num_sites <- round(target_num_sites * num_sites)
  }

  if (num_sites == 0) {
    stop('The target number of sites is too low. Please increase the target number of sites.')
  }

  chosen_sites <- sample(unique(data$site), num_sites)
  print(paste("selecting sites: ", paste(chosen_sites, collapse = ", ")))

  data <- data[data$site %in% chosen_sites,]

  num_rows <- nrow(data)

  if (target_num_rows > 1) {
    num_rows <- min(target_num_rows, num_rows)
  } else if (target_num_rows > 0) {
    # if target num rows is below 1, treat it as a proportion of the original number
    num_rows <- round(target_num_rows * num_rows)
  }

  if (num_rows == 0) {
    stop('The target number of rows is too low. Please increase the target number of rows.')
  }
  print(paste("Selecting random rows: ", num_rows))
  data <- data[sample(nrow(data), num_rows),]

  return(data)


}
