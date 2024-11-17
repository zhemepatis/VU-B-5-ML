library(dbscan)
library(fpc)
library(uwot)

perform_kNN_method <- function(data, distance, neighbors, title) {
  par(mar = c(5, 7, 5, 5))
  kNNdistplot(
    data,
    neighbors
  )

  abline(h = distance, lty = 2)

  title(title)
}

perform_dbscan <- function(data, distance, neighbors, metric = "euclidean") {
  dbscan_results <- dbscan(data, distance, neighbors)
  return(dbscan_results)
}

get_label_distribution <- function(data, clusters) {
  return(round(prop.table(table(clusters, data$label), margin = 1) * 100, 2))
}

get_cluster_coverage <- function(clusters) {
  total_points <- length(clusters)
  clustered_points <- sum(clusters != 0)
  
  coverage_percentage <- (clustered_points / total_points) * 100

  return(round(coverage_percentage, 2))
}

save_label_distribution <- function(distribution, filename) {
  distribution_df <- as.data.frame(distribution)
  write.csv(distribution_df, file = filename, row.name = TRUE)
}

plot_results <- function(data_2d, clusters, eps, minPts, title) {
  # parenkam spalvas
  cluster_num <- length(unique(clusters))
  color_palette <- hcl(h = seq(15, 360, length = cluster_num), c = 100, l = 50)
  
  # suvienodinam asis
  x_range <- range(data_2d[, 1])
  y_range <- range(data_2d[, 2])
  combined_range <- range(c(x_range, y_range))
  
  # atvaizduojam rezultatus
  par(mar = c(5, 5, 5, 10))
  hullplot(
    data_2d, 
    cl = clusters,
    main = title,
    xlab = "UMAP1",
    ylab = "UMAP2",
    cex.lab = 1.5,
    col = c("black", color_palette),
    xlim = combined_range,
    ylim = combined_range,
    asp = 1
  )
  
  # pridedam klasteriu numerius
  data_2d_df <- as.data.frame(data_2d)
  data_2d_df$Cluster <- as.factor(clusters)
  cluster_centroids <- aggregate(. ~ Cluster, data = data_2d_df, FUN = mean)
  cluster_centroids <- cluster_centroids[cluster_centroids$Cluster != 0, ]
  
  text(
    x = cluster_centroids$V1,
    y = cluster_centroids$V2,
    labels = cluster_centroids$Cluster,
    col = "black",
    cex = 1.2,
    font = 1
  )
    
  # pridedam legenda
  legend(
    "topright",
    inset = c(-0.25, 0),
    legend = paste("Klasteris", levels(data_2d_df$Cluster)),
    fill = c("black", color_palette),
    title = "Legenda",
    cex = 1.2,
    bty = "n",
    xpd = TRUE
  )

  # pridedam parametrus
  param_text <- paste0("eps = ", eps, ", minPts = ", minPts)
  mtext(param_text, side = 1, line = 4, adj = 0.5, cex = 1.5)
}


# nagrinejam visa duomenu aibe
target_data <- ekg_data[, -32]

ekg_data_6d <- perform_umap(target_data, n_components = 6)
ekg_data_2d <- perform_umap(ekg_data_6d, n_components = 2)

eps = 0.5
neighbours = 5
perform_kNN_method(ekg_data_6d, eps, neighbours, "Kelio metodas visai duomenų aibei")
dbscan_results <- perform_dbscan(ekg_data_6d, eps, neighbours)

plot_results(ekg_data_2d, dbscan_results$cluster, eps, neighbours, "DBSCAN metodas visai duomenų aibei")
label_distribution <- get_label_distribution(ekg_data, dbscan_results$cluster)
save_label_distribution(label_distribution, "output/label_distribution_whole.csv")
coverage <- get_cluster_coverage(dbscan_results$cluster)
print(coverage)


# nagrinejam atrinktus pozymius
target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side")
target_data <- ekg_data[, target_cols]

ekg_data_6d <- perform_umap(target_data, n_components = 6)
ekg_data_2d <- perform_umap(ekg_data_6d, n_components = 2)

eps = 0.45
neighbours = 12
perform_kNN_method(ekg_data_6d, eps, neighbours, "Kelio metodas duomenų aibei su apribotais požymiais")
dbscan_results <- perform_dbscan(ekg_data_6d, eps, neighbours)

plot_results(ekg_data_2d, dbscan_results$cluster, eps, neighbours, "DBSCAN metodas duomenų aibei su apribotais požymiais")
label_distribution <- get_label_distribution(ekg_data, dbscan_results$cluster)
save_label_distribution(label_distribution, "output/label_distribution_limited.csv")
coverage <- get_cluster_coverage(dbscan_results$cluster)
print(coverage)


# nagrinejam atrinktus pozymius
target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side")
target_data <- ekg_data[, target_cols]

ekg_data_2d <- perform_umap(target_data, n_components = 2)

eps = 0.5
neighbours = 10
perform_kNN_method(ekg_data_2d, eps, neighbours, "Kelio metodas duomenų aibei, sumažintai iki 2 dimesnijų")
dbscan_results <- perform_dbscan(ekg_data_2d, eps, neighbours)

plot_results(ekg_data_2d, dbscan_results$cluster, eps, neighbours, "DBSCAN metodas duomenų aibei, sumažintai iki 2 dimesnijų")
label_distribution <- get_label_distribution(ekg_data, dbscan_results$cluster)
save_label_distribution(label_distribution, "output/label_distribution_2d.csv")
coverage <- get_cluster_coverage(dbscan_results$cluster)
print(coverage)