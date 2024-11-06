library(uwot)

perform_umap <- function(data, label_col = "label", n_neighbors = 35, min_dist = 0.02, spread = 1.25, metric = "euclidean") {
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
    min_dist = min_dist, 
    spread = spread,
    metric = metric
  )
  
  umap_df <- data.frame(
    UMAP1 = umap_result[, 1],
    UMAP2 = umap_result[, 2],
    label = data[[label_col]]
  )
  
  return(umap_df)
}