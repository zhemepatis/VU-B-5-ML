normalize_sets <- function(main_set, secondary_set) {
  normalization_result <- normalize_data(main_set)
  main_set <- normalization_result$data
  custom_min <- normalization_result$min
  custom_max <- normalization_result$max
  
  normalization_result <- normalize_data(secondary_set, custom_min = custom_min, custom_max = custom_max)
  secondary_set <- normalization_result$data
  
  result <- list(
    main_set = main_set,
    secondary_set = secondary_set
  )
  
  return(result)
}


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
