library(cluster)
library(ggplot2)

# Silhouette method function
silhouette_method <- function(data, max_clusters = 15) {
  avg_silhouette <- numeric(max_clusters)
  
  for (k in 2:max_clusters) {
    kmeans_result <- kmeans(data, centers = k, nstart = 10)
    
    sil <- silhouette(kmeans_result$cluster, dist(data))
    avg_silhouette[k] <- mean(sil[, 3])
  }
  
  silhouette_df <- data.frame(Clusters = 2:max_clusters, Avg_Silhouette = avg_silhouette[2:max_clusters])
  
  # Plot the average silhouette score for each number of clusters
  ggplot(silhouette_df, aes(x = Clusters, y = Avg_Silhouette)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "red", size = 2) +
    labs(title = "Silueto metodas",
         x = "Klasterių skaičius",
         y = "Vidutinis silueto plotis") +
    theme_minimal()
}


# Prepas
ekg_data_silhouette <- ekg_data[, c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos")]
ekg_data_silhouette_norm <- ekg_data_silhouette

min_max_normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x))) 
}

for (col in names(ekg_data_silhouette_norm)) { 
  ekg_data_silhouette_norm[[col]] <- min_max_normalize(ekg_data_silhouette_norm[[col]])
}

# Callinamas metodas
silhouette_method(ekg_data_silhouette)
silhouette_method(ekg_data_silhouette_norm)
