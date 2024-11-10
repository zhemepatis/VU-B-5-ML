library(cluster)

perform_hclustering <- function(data, num_clusters = 4, method = "ward.D", plot = TRUE, metric = "euclidean") {
  
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
    unique_labels <- unique(data$label)
    manual_colors <- c("red", "green", "blue", "purple", "orange", "yellow")
    label_colors <- setNames(manual_colors[1:length(unique_labels)], unique_labels)
    point_colors <- label_colors[as.character(data$label)]
    
    clusplot(data[, -which(colnames(data) == "label")],
             y_hc,
             lines = 0,
             shade = TRUE,
             color = TRUE,
             col.p = point_colors,
             labels = 4,
             plotchar = FALSE,
             span = TRUE,
             main = 'Klasterių vizualizacija',
             xlab = 'UMAP1',
             ylab = 'UMAP2'
    )
    legend(
      "bottomright",
      legend = paste(unique_labels),
      col = label_colors,
      pch = 19,
      title = "Pūpsnio\n klasė",
      cex = 0.9,
      pt.cex = 1,
      x.intersp = 0.5,
      y.intersp = 0.5
    )
  }
  
  data$cluster <- y_hc
  
  label_distribution <- prop.table(table(data$label, data$cluster), margin = 1) * 100
  return(label_distribution)
}

## CALLAI
limit_col <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")

# ORIG - full
ekg_data_hc <- normalize_data(ekg_data)
perform_hclustering(ekg_data_hc, num_clusters = 4, method = "ward.D", plot = FALSE, metric = "euclidean")

# ORIG - limited
ekg_data_hc <- ekg_data_hc[, limit_col]
perform_hclustering(ekg_data_hc, num_clusters = 4, method = "ward.D", plot = FALSE, metric = "euclidean")

# UMAP - full
ekg_data_umap <- ekg_data
ekg_data_umap <- perform_umap(ekg_data_umap)
perform_hclustering(ekg_data_umap, num_clusters = 4, method = "ward.D", plot = TRUE, metric = "euclidean")

# UMAP - limited
ekg_data_umap <- ekg_data[, limit_col]
ekg_data_umap <- perform_umap(ekg_data_umap)
perform_hclustering(ekg_data_umap, num_clusters = 14, method = "ward.D", plot = TRUE, metric = "euclidean")

