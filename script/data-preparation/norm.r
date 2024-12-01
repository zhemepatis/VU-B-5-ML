library(dplyr)

normalize_data <- function(data, label_colname = "label") {
  columns_to_normalize <- setdiff(colnames(data), label_colname)
  
  for (col in columns_to_normalize) {
    data[[col]] <- min_max_normalize(data[[col]])
  }
  
  return(data)
}

min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

test_set <- normalize_data(test_set)
training_set <- normalize_data(training_set)
validation_set <- normalize_data(validation_set)

# istrinam resursus, kuriu nebenaudosim
rm(normalize_data)
rm(min_max_normalize)