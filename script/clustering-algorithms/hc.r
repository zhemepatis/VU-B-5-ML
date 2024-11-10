library(cluster)

perform_hclustering <- function(data, num_clusters = 4, method = "ward.D", plot = TRUE, metric = "euclidean", return_clusters = FALSE) {
  
  set.seed(6)
  
  hc <- hclust(dist(data[, -which(colnames(data) == "label")], method = metric), method = method)
  y_hc <- cutree(hc, k = num_clusters)

  if (plot) {
    plot(hc,
         main = 'Dendrograma',
         xlab = 'Euklidinis atstumas',
         ylab = 'EKG pūpsniai',
         labels = FALSE,
         sub = ''
    )
  }

  if (plot) {
    cluster_colors <- c(
      "red", "green", "blue", "purple", "orange", "lightblue", "pink", "brown", 
      "cyan", "darkgreen", "magenta", "darkred", "gray", "darkblue", "gold"
    )
    
    cluster_colors_named <- setNames(cluster_colors, sort(unique(y_hc)))
    
    dark_colors <- c(
      "#1B1B3A", "#2E8B57", "#1C3F95", "#8B008B", "#FF4500", "#9932CC", 
      "#8B0000", "#FF8C00", "#556B2F", "#4682B4", "#8B4513", "#2F4F4F",
      "#4B0082", "#483D8B", "#2E2E2E"
    )
    cluster_colors_limit <- setNames(dark_colors, sort(unique(y_hc)))
    
    par(mar = c(6, 4, 4, 8) + 0.1)
    
    clusplot(data[, -which(colnames(data) == "label")],
             y_hc,
             lines = 0,
             shade = TRUE,
             color = TRUE,
             #col.p = point_colors,
             col.p = cluster_colors_named[as.character(y_hc)],
             col.clus = cluster_colors_limit,
             labels = 4,
             plotchar = FALSE,
             span = TRUE,
             main = 'Klasterių vizualizacija',
             xlab = 'DIM1',
             ylab = 'DIM2'
    )
    legend("topright",
           inset = c(-0.275, 0),  # Moves the legend outside the plot
           legend = paste("Klasteris:", 1:num_clusters),
           fill = cluster_colors_named,
           title = "Klasteriai",
           title.cex = 1,
           cex = 0.8,
           border = "black",
           bty = "n",
           xpd = TRUE,
           adj = 0.4,
           title.adj = 0.25
    )
    
    mtext(paste("Parametrai: metodas =", method, ", metrika =", metric, ", klasterių sk. =", num_clusters),
          side = 1, line = 4, cex = 1, adj = 1)
  }
  
  data$cluster <- y_hc
  
  label_distribution <- prop.table(table(data$cluster, data$label), margin = 1) * 100
  return(list(label_distribution = label_distribution, clustered_data = if (return_clusters) data else NULL))
}

## CALLAI
limit_col <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
meth <- "ward.D"
metr <- "euclidean"


# ORIG - full
#ekg_data_hc <- normalize_data(ekg_data)
ekg_data_hc <- ekg_data
ekg_data_hc <- perform_umap(ekg_data_hc, n_components = 6)
perform_hclustering(ekg_data_hc, num_clusters = 3, method = "ward.D", plot = TRUE, metric = "euclidean")

# ORIG - limited
#ekg_data_hc <- normalize_data(ekg_data[, limit_col])
ekg_data_hc <- ekg_data[, limit_col]
ekg_data_hc <- perform_umap(ekg_data_hc, n_components = 6)
perform_hclustering(ekg_data_hc, num_clusters = 8, method = "ward.D", plot = TRUE, metric = "euclidean")

# UMAP - full
ekg_data_umap <- ekg_data
ekg_data_umap <- perform_umap(ekg_data_umap, n_components = 2)
perform_hclustering(ekg_data_umap, num_clusters = 9, method = "ward.D", plot = TRUE, metric = "euclidean")
# empiriskai 10

# UMAP - limited
ekg_data_umap <- ekg_data[, limit_col]
ekg_data_umap <- perform_umap(ekg_data_umap, n_components = 2)
perform_hclustering(ekg_data_umap, num_clusters = 10, method = "ward.D", plot = TRUE, metric = "euclidean")
# empiriskai 14
