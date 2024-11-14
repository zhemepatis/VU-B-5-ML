library(uwot)

perform_umap <- function(data, n_components = 2, label_col = "label", n_neighbors = 25, min_dist = 0.05, spread = 1.25, metric = "euclidean") {
  min_max_normalize <- function(x) { 
    return((x - min(x)) / (max(x) - min(x))) 
  }
  
  data_normalized <- data
  
  columns_to_normalize <- setdiff(names(data), label_col)
  
  for (col in columns_to_normalize) { 
    data_normalized[[col]] <- min_max_normalize(data_normalized[[col]]) 
  }
  
  set.seed(1000)
  umap_result <- umap(
    data_normalized[, columns_to_normalize], 
    n_neighbors = n_neighbors,
    n_components = n_components,
    min_dist = min_dist, 
    spread = spread,
    metric = metric
  )
  
  umap_df <- as.data.frame(umap_result)
  return(umap_df)
}