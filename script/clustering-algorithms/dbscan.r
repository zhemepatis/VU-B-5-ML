library(dbscan)
library(uwot)
library(fpc)

perform_kNN_method <- function(data, distance, neighbors) {
  kNNdistplot(data, neighbors)
  abline(h = distance, lty = 2)
}

perform_dbscan <- function(data, distance, neighbors, plot_kNN = TRUE) {
  if(plot_kNN) {
    perform_kNN_method(data, distance, neighbors)
  }
  
  dbscan_results <- dbscan(data, distance, neighbors)
  return(dbscan_results)
}

set.seed(1000)

ekg_data_norm <- normalize_data(ekg_data) # normuojam duomenis
target_data <- ekg_data_norm[, -32] # paimam visus pozymius, isskyrus "label"


# nagrinejam visa duomenu aibe
ekg_data_6d <- umap(
  target_data,
  n_components = 6,
  n_neighbors = 35,
  min_dist = 0.02,
  spread = 1.25,
  metric = "euclidean"
)
dbscan_results <- perform_dbscan(ekg_data_6d, 0.3, 5)
ekg_data_2d <- umap(
  ekg_data_6d,
  n_components = 2,
  n_neighbors = 35,
  min_dist = 0.02,
  spread = 1.25,
  metric = "euclidean"
)
hullplot(ekg_data_2d, dbscan_results$cluster)


# nagrinejam atrinktus pozymius
target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos")
target_data <- ekg_data_norm[, target_cols]

ekg_data_6d <- umap(
  target_data,
  n_components = 6
)
dbscan_results <- perform_dbscan(ekg_data_6d, 0.3, 5, plot_kNN = FALSE)
ekg_data_2d <- umap(
  ekg_data_6d,
  n_components = 2
)
hullplot(ekg_data_2d, dbscan_results$cluster)


# nagrinejam suspaustus duomenis iki 2 dimensiju
target_data <- ekg_data_norm[, -32] # paimam visus pozymius, isskyrus "label"

ekg_data_2d <- umap(
  target_data,
  n_components = 2
)
dbscan_results <- perform_dbscan(ekg_data_2d, 0.2, 15)
hullplot(ekg_data_2d, dbscan_results$cluster)