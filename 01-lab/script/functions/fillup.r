sum_if_target_empty <- function(data, target_col, sum_col1, sum_col2) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i, target_col])) {
      data[i, target_col] <- data[i, sum_col1] + data[i, sum_col2]
    }
  }
  
  return(data)
}

multiply_if_target_empty <- function(data, target_col, factor_col1, factor_col2) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i, target_col])) {
      data[i, target_col] <- data[i, factor_col1] * data[i, factor_col2]
    }
  }
  
  return(data)
}

delete_if_target_empty <- function(data, target_col) {
  data <- data[!is.na(data[[target_col]]),]
  return(data)
}

has_empty_values <- function(data, target_col) {
  has_empty = any(is.na(data[[target_col]]))
  return(has_empty)
}


