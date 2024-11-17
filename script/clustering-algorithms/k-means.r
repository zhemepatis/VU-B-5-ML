library(uwot)
library(cluster)

perform_kmeans_with_hulls <- function(data, clusters, labels, dataset_name, centers, iter.max, nstart, algorithm, selected_cols = NULL, plot = TRUE) {
  if (plot) {
    if (ncol(data) > 2) {
      data <- perform_umap(data, n_components = 2)
    }
    
    cluster_colors <- c(
      "red", "orange", "yellow", "green", "cyan", "blue", "purple", "pink", 
      "brown", "gray", "lightblue", "magenta", "darkred", "darkgreen", 
      "gold", "darkblue", "lightgray", "darkcyan", "darkmagenta", "lightgreen"
    )
    cluster_colors_named <- setNames(cluster_colors, sort(unique(clusters)))
    
    par(mar = c(5, 4, 4, 8) + 0.1) # Adjust margins for the legend
    par(pty = "s")
    
    plot(data[, 1], data[, 2], col = cluster_colors_named[as.character(clusters)], pch = 19,
         xlab = "UMAP1", ylab = "UMAP2", main = 'Klasterių vizualizacija (K-means)')
    
    # Add convex hulls with matching border colors
    for (cluster in unique(clusters)) {
      cluster_points <- data[clusters == cluster, ]
      if (nrow(cluster_points) > 2) { # Convex hull requires at least 3 points
        hull <- chull(cluster_points)
        hull <- c(hull, hull[1]) # Close the hull
        polygon(cluster_points[hull, 1], cluster_points[hull, 2], border = cluster_colors_named[as.character(cluster)], 
                col = adjustcolor(cluster_colors_named[as.character(cluster)], alpha.f = 0.2), lwd = 2)
      }
    }
    
    # Add a legend without a border
    par(xpd = TRUE) # Allow drawing outside the plot area
    legend("topright", inset = c(-0.3, 0), legend = paste("Klasteris:", sort(unique(clusters))),
           col = cluster_colors_named, pch = 19, title = "Klasteriai", title.cex = 1, cex = 0.8, bg = "white", bty = "n")
    par(xpd = FALSE)
    
    # Add parameters and dataset name below the plot
    mtext(paste("Dataset:", dataset_name, ", Centers:", centers, ", Iter.max:", iter.max, 
                ", Nstart:", nstart, ", Algorithm:", algorithm),
          side = 1, line = 4, cex = 0.9, adj = 0.5)
    
    # Add selected columns (if provided)
    if (!is.null(selected_cols)) {
      mtext(paste("Selected Columns:", paste(selected_cols, collapse = ", ")),
            side = 1, line = 5, cex = 0.8, adj = 0.5)
    }
  }
}

# Function to summarize cluster statistics and label distribution
summarize_kmeans_clusters <- function(data, clusters, labels) {
  data$cluster <- clusters
  numeric_columns <- sapply(data, is.numeric)
  summary_list <- list()
  influence_list <- list()
  
  for (col_name in names(data)[numeric_columns]) {
    column_stats <- list()
    column_influence <- list()
    
    for (cluster in unique(clusters)) {
      cluster_data <- data[data$cluster == cluster, col_name, drop = TRUE]
      
      # Compute summary statistics
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
      
      # Compute influence: Variance of feature within cluster
      column_influence[[paste("Klast.", cluster)]] <- round(var(cluster_data, na.rm = TRUE), 4)
    }
    
    column_summary_df <- do.call(cbind, column_stats)
    column_summary_df <- as.data.frame(column_summary_df)
    column_summary_df <- cbind(Statistika = rownames(column_summary_df), column_summary_df)
    rownames(column_summary_df) <- NULL
    summary_list[[col_name]] <- column_summary_df
    
    # Add influence results
    influence_list[[col_name]] <- column_influence
  }
  
  label_distribution <- round(prop.table(table(clusters, labels), margin = 1) * 100, 2)
  print(label_distribution)
  
  return(list(summary = summary_list, label_distribution = label_distribution, feature_influence = influence_list))
}
  
# Parameters for k-means clustering
centers <- 6
iter.max <- 25
nstart <- 25
algorithm <- "Hartigan-Wong" 

# 1. Entire dataset
# dataset_name <- "Entire Dataset"
# target_data <- ekg_data[, -32]
ekg_labels <- ekg_data$label
# 
# ekg_data_6d <- perform_umap(target_data, n_components = 6)
# set.seed(6)
# kmeans_results <- kmeans(ekg_data_6d, centers = centers, iter.max = iter.max, nstart = nstart, algorithm = algorithm)
# ekg_data_2d <- perform_umap(ekg_data_6d, n_components = 2)
# perform_kmeans_with_hulls(ekg_data_2d, kmeans_results$cluster, ekg_labels, dataset_name, centers, iter.max, nstart, algorithm, plot = TRUE)
# 
# cluster_summaries <- summarize_kmeans_clusters(target_data, kmeans_results$cluster, ekg_labels)
# print(cluster_summaries$feature_influence)

# 2. Selected columns
dataset_name <- "Selected Columns"
target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side")
target_data <- ekg_data[, target_cols]
# 
# ekg_data_6d <- perform_umap(target_data, n_components = 6)
# set.seed(6)
# kmeans_results <- kmeans(ekg_data_6d, centers = centers, iter.max = iter.max, nstart = nstart, algorithm = algorithm)
# ekg_data_2d <- perform_umap(ekg_data_6d, n_components = 2)
# perform_kmeans_with_hulls(ekg_data_2d, kmeans_results$cluster, ekg_labels, dataset_name, centers, iter.max, nstart, algorithm, selected_cols = target_cols, plot = TRUE)
# 
# cluster_summaries <- summarize_kmeans_clusters(target_data, kmeans_results$cluster, ekg_labels)
# print(cluster_summaries$feature_influence)


# 3. Reduced directly to 2D
dataset_name <- "Reduced Directly to 2D"
ekg_data_2d <- perform_umap(target_data, n_components = 2)
set.seed(6)
kmeans_results <- kmeans(ekg_data_2d, centers = centers, iter.max = iter.max, nstart = nstart, algorithm = algorithm)
perform_kmeans_with_hulls(ekg_data_2d, kmeans_results$cluster, ekg_labels, dataset_name, centers, iter.max, nstart, algorithm, plot = TRUE)

cluster_summaries <- summarize_kmeans_clusters(target_data, kmeans_results$cluster, ekg_labels)
print(cluster_summaries$feature_influence)
