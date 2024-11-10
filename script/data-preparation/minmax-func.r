library(dplyr)

normalize_data <- function(data, label_column="label") {
  min_max_normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  data_normalized <- data
  
  columns_to_normalize <- setdiff(colnames(data), label_column)
  
  for (col in columns_to_normalize) {
    data_normalized[[col]] <- min_max_normalize(data[[col]])
  }
  
  return(data_normalized)
}