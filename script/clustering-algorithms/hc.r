library(cluster)

perform_hclustering <- function(data, num_clusters = 4, method = "ward.D", plot = TRUE, metric = "euclidean", return_clusters = FALSE) {
  
  set.seed(1000)
  
  hc <- hclust(dist(data, method = metric), method = method)
  y_hc <- cutree(hc, k = num_clusters)
  
  if (plot) {
    plot(hc,
         main = 'Dendrograma',
         xlab = 'Duomenų aibės objektai',
         ylab = 'Atstumas',
         labels = FALSE,
         sub = ''
    )
    abline(h = hc$height[length(hc$height) - (num_clusters - 1)], col = "red", lwd = 2)
  }

  if (plot) {
    if (ncol(data) > 2) {
      data <- perform_umap(data, n_components = 2)
    }
    
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
    
    x_min <- min(data[, 1])
    x_max <- max(data[, 1])
    y_min <- min(data[, 2])
    y_max <- max(data[, 2])
    plot_min <- min(x_min-4, y_min-4)
    plot_max <- max(x_max+4, y_max+4)
    
    par(pty = "s")
    
    clusplot(data,
             y_hc,
             lines = 0,
             shade = TRUE,
             color = TRUE,
             col.p = cluster_colors_named[as.character(y_hc)],
             col.clus = cluster_colors_limit,
             labels = 4,
             plotchar = FALSE,
             span = TRUE,
             main = 'Klasterių vizualizacija',
             xlab = 'UMAP1',
             ylab = 'UMAP2',
             xlim = c(plot_min, plot_max),
             ylim = c(plot_min, plot_max)
    )
    legend("topright",
           inset = c(-0.6, -0.1),
           legend = paste("Klasteris:", 1:num_clusters),
           fill = cluster_colors_named,
           title = "Klasteriai",
           title.cex = 1,
           cex = 0.7,
           border = "black",
           bty = "n",
           xpd = TRUE,
           adj = 0.4,
           title.adj = 0.25
    )
    
    mtext(paste("Parametrai: metodas =", method, ", metrika =", metric, ", klasterių sk. =", num_clusters),
          side = 1, line = 5, cex = 1, adj = 0)
  }
  
  data$cluster <- y_hc
  
  id_cluster_table <- data.frame(id = seq_len(nrow(data)), cluster = y_hc)
  
  return(list(id_cluster_table = id_cluster_table, clustered_data = if (return_clusters) data else NULL))
}


# Pridedamas klasteris aukštos dimensijos duomenims
add_cluster_column <- function(original_data, id_cluster_table) {
  original_data$id <- seq_len(nrow(original_data))
  merged_data <- merge(original_data, id_cluster_table, by = "id", all.x = TRUE)
  merged_data$id <- NULL
  return(merged_data)
}

# Atspausdinamas kiekvieno pozymio summary pagal klasteri
summarize_clusters <- function(data) {
  numeric_columns <- sapply(data, is.numeric)
  summary_list <- list()
  
  for (col_name in names(data)[numeric_columns]) {
    column_stats <- list()
    for (cluster in unique(data$cluster)) {
      cluster_data <- data[data$cluster == cluster, col_name, drop = TRUE]
      cluster_summary <- c(
        min = round(min(cluster_data, na.rm = TRUE), 4),
        Q1 = round(quantile(cluster_data, 0.25, na.rm = TRUE), 4),
        mean = round(mean(cluster_data, na.rm = TRUE), 4),
        median = round(median(cluster_data, na.rm = TRUE), 4),
        Q3 = round(quantile(cluster_data, 0.75, na.rm = TRUE), 4),
        max = round(max(cluster_data, na.rm = TRUE), 4),
        sd = round(sd(cluster_data, na.rm = TRUE), 4)
      )
      column_stats[[paste("Klast.", cluster)]] <- cluster_summary
    }
    column_summary_df <- do.call(cbind, column_stats)
    column_summary_df <- as.data.frame(column_summary_df)
    column_summary_df <- cbind(Statistika = rownames(column_summary_df), column_summary_df)
    rownames(column_summary_df) <- NULL
    summary_list[[col_name]] <- column_summary_df
  }
  
  label_distribution <- round(prop.table(table(data$cluster, data$label), margin = 1) * 100, 2)
  
  return(list(summary_list = summary_list, label_distribution = label_distribution))
}



### Metodo callas su prop table ir summaries
# Reikia prideti 4 pagrindinius ir su UMAP 2 pereiti visus linkages
# Isbandyt po 2 metrikas

limit_col <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")

ekg_data_temp <- ekg_data[, limit_col]
ekg_data_umap <- ekg_data_temp
ekg_data_umap <- perform_umap(ekg_data_umap, n_components = 2)

# geriausias ward.D2 eksperimentas su 30k objektu
linkage_methods <- c("single", "complete", "average", "ward.D", "ward.D2", "centroid", "median")
metric_methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
for (m in linkage_methods) {
  for (n in metric_methods) {
    clustering_results <- perform_hclustering(ekg_data_umap, num_clusters = 10, method = m, plot = TRUE, metric = n, return_clusters = TRUE)
    
    print(paste(m, "-", n))
    
    ekg_data_with_clusters <- add_cluster_column(ekg_data_temp, clustering_results$id_cluster_table)
    cluster_summaries <- summarize_clusters(ekg_data_with_clusters)
    print(cluster_summaries$label_distribution)
  }
}



############

limit_col <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
meth <- "median"     # "single", "complete", "average", "ward.D", "ward.D2", "centroid", "median", "mcquitty"
metr <- "euclidean"   # "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"


#ekg_data_temp <- read.csv("data/ecg-data.csv", sep = ";", header = TRUE, na.strings = c(""))
#print(nrow(ekg_data_temp))
#ekg_data_temp <- ekg_data_temp[, limit_col]

ekg_data_temp <- ekg_data
ekg_data_temp <- ekg_data[, limit_col]
ekg_data_umap <- perform_umap(ekg_data_temp, n_components = 2)

clustering_results <- perform_hclustering(ekg_data_umap, num_clusters = 10, method = meth, plot = TRUE, metric = metr, return_clusters = TRUE)

ekg_data_with_clusters <- add_cluster_column(ekg_data_temp, clustering_results$id_cluster_table)
cluster_summaries <- summarize_clusters(ekg_data_with_clusters)
#print(cluster_summaries)
print(cluster_summaries$label_distribution)

print(nrow(subset(ekg_data_with_clusters, cluster == 4 & label == 2)))
# 70% dominuoja

############


# ATASKAITOS PAGRINDINES PROJEKCIJOS
## CALLAI
# limit_col <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
# meth <- "ward.D"
# metr <- "euclidean"
# 
# 
# # ORIG - full
# ekg_data_hc <- ekg_data
# ekg_data_hc <- perform_umap(ekg_data_hc, n_components = 6)
# perform_hclustering(ekg_data_hc, num_clusters = 10, method = "ward.D", plot = TRUE, metric = "euclidean")
# 
# # ORIG - limited
# ekg_data_hc <- ekg_data[, limit_col]
# ekg_data_hc <- perform_umap(ekg_data_hc, n_components = 6)
# perform_hclustering(ekg_data_hc, num_clusters = 15, method = "ward.D", plot = TRUE, metric = "euclidean")
# 
# # UMAP - limited
# ekg_data_umap <- ekg_data[, limit_col]
# ekg_data_umap <- perform_umap(ekg_data_umap, n_components = 2)
# perform_hclustering(ekg_data_umap, num_clusters = 10, method = "ward.D", plot = TRUE, metric = "euclidean")
