library(ggplot2)

elbow_method <- function(data, max_clusters = 15) {
  wss <- numeric(max_clusters)
  
  for (k in 1:max_clusters) {
    kmeans_result <- kmeans(data, centers = k, nstart = 10)
    wss[k] <- kmeans_result$tot.withinss
  }
  
  
  elbow_plot <- data.frame(Clusters = 1:max_clusters, WSS = wss)
  
  ggplot(elbow_plot, aes(x = Clusters, y = WSS)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "red", size = 2) +
    labs(title = "Elbow metodas",
         x = "Klasterių skaičius (k)",
         y = "WCSS") +
    theme_minimal()
}


# Prepas
ekg_data_elbow <- ekg_data[, c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos")]
ekg_data_elbow_norm <- ekg_data_elbow

min_max_normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x))) 
}

for (col in names(ekg_data_elbow_norm)) { 
  ekg_data_elbow_norm[[col]] <- min_max_normalize(ekg_data_elbow_norm[[col]])
}

# Callinamas metodas
elbow_method(ekg_data_elbow)
elbow_method(ekg_data_elbow_norm)
