library(dbscan)
library(uwot)
library(fpc)

perform_kNN_method <- function(data, distance, neighbors) {
  kNNdistplot(data, neighbors)
  abline(h = distance, lty = 2)
}

perform_dbscan <- function(data, distance, neighbors, metric = "euclidean", plot_kNN = TRUE) {
  if(plot_kNN) {
    perform_kNN_method(data, distance, neighbors)
  }

  dbscan_results <- dbscan(data, distance, neighbors)
  return(dbscan_results)
}


# nagrinejam visa duomenu aibe
target_data <- ekg_data_norm[, -32] # paimam visus pozymius, isskyrus "label"

ekg_data_6d <- perform_umap(target_data, n_components = 6)
dbscan_results <- perform_dbscan(ekg_data_6d, 0.4, 15, plot_kNN = FALSE)
ekg_data_2d <- perform_umap(ekg_data_6d, n_components = 2)
hullplot(
  ekg_data_2d, 
  dbscan_results$cluster,
  main = "DBSCAN grupavimo metodas visai aibei",
  xlab = "UMAP1",
  ylab = "UMAP2"
)


# nagrinejam atrinktus pozymius
target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side")
target_data <- ekg_data_norm[, target_cols]

ekg_data_6d <- perform_umap(target_data, n_components = 6)
dbscan_results <- perform_dbscan(ekg_data_6d, 0.4, 15)
ekg_data_2d <- perform_umap(ekg_data_6d, n_components = 2)
hullplot(
  ekg_data_2d, 
  dbscan_results$cluster,
  main = "DBSCAN grupavimo metodas aibei su apribotais požymiais",
  xlab = "UMAP1",
  ylab = "UMAP2"
)


# nagrinejam suspaustus duomenis iki 2 
target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side")
target_data <- ekg_data[, target_cols]

ekg_data_2d <- perform_umap(target_data, n_components = 2)
dbscan_results <- perform_dbscan(ekg_data_2d, 0.5, 15, plot_kNN = FALSE)
hullplot(
  ekg_data_2d, 
  dbscan_results$cluster,
  main = "DBSCAN grupavimo metodas sumažintos dimensijos aibei",
  xlab = "UMAP1",
  ylab = "UMAP2"
)