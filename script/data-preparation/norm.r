normalize_data <- function(data, label_colname = "label", custom_min = NULL, custom_max = NULL) {
  columns_to_normalize <- setdiff(colnames(data), label_colname)
  
  min_values <- list()
  max_values <- list()
  
  for (col in columns_to_normalize) {
    column_min <- if (!is.null(custom_min)) custom_min[[col]] else min(data[[col]], na.rm = TRUE)
    column_max <- if (!is.null(custom_max)) custom_max[[col]] else max(data[[col]], na.rm = TRUE)
    
    min_values[[col]] <- column_min
    max_values[[col]] <- column_max
    
    data[[col]] <- min_max_normalize(data[[col]], column_min, column_max)
  }
  
  return(list(data = data, min_values = min_values, max_values = max_values))
}

min_max_normalize <- function(x, custom_min, custom_max) {
  return((x - custom_min) / (custom_max - custom_min))
}
