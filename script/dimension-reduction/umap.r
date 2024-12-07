library(uwot)

perform_umap <- function(data, n_components = 2, label_col = "label", n_neighbors = 25, min_dist = 0.05, spread = 1.25, metric = "euclidean") {
  target_cols <- setdiff(names(data), label_col)
  
  set.seed(1000)
  umap_result <- umap(
    data[, target_cols], 
    n_neighbors = n_neighbors,
    n_components = n_components,
    min_dist = min_dist, 
    spread = spread,
    metric = metric
  )
  
  umap_df <- as.data.frame(umap_result,  data[[label_col]])
  umap_df[[label_col]] <- data[[label_col]]
  return(umap_df)
}