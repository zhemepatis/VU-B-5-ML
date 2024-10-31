# Libraries
library(ggplot2)  
library(ggrepel)

# Min-Max Normalization Function
min_max_normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x))) 
}

ekg_data_minmax <- ekg_data
columns_to_normalize <- setdiff(names(ekg_data), 'label')
for (col in columns_to_normalize) { 
  ekg_data_minmax[[col]] <- min_max_normalize(ekg_data_minmax[[col]]) 
}

set.seed(1000)

# Chosen columns
selected_columns <- c("P_val", "Q_val", "R_val", "S_val", 
                      "P_pos", "Q_pos", "R_pos", 
                      "T_pos", "signal_std")

# Helper function for MDS and Plotting
plot_mds <- function(data, method, title) {
  data_subset <- data[, selected_columns]
  
  distance_matrix <- dist(data_subset, method = method)
  mds_result <- cmdscale(distance_matrix, k = 2)
  
  mds_df <- data.frame(
    MDS1 = mds_result[, 1],
    MDS2 = mds_result[, 2],
    label = data$label
  )
  
  selected_columns_text <- paste(selected_columns, collapse = ", ")
  
  x_range <- range(mds_df$MDS1)
  y_range <- range(mds_df$MDS2)
  max_range <- max(abs(c(x_range, y_range)))
  
  plot <- ggplot(mds_df, aes(x = MDS1, y = MDS2, col = as.factor(label))) +
    geom_point() +
    theme_minimal(base_size = 9) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      plot.title = element_text(hjust = 0.5),     
      plot.caption = element_text(size = 8, face = "italic", hjust = 0.5)  
    ) +
    coord_fixed(ratio = 1, xlim = c(-max_range, max_range), ylim = c(-max_range, max_range)) +
    labs(
      color = "Klasė", 
      title = title, 
      x = "MDS 1 dimensija", 
      y = "MDS 2 dimensija",
      caption = selected_columns_text
    ) +
    guides(color = guide_legend(override.aes = list(size = 5)))
  
  print(plot)  
}

# Canberra distance on normalized data -----------------
plot_mds(ekg_data_minmax, method = "canberra", 
         title = "MDS vizualizacija normuotai pasirinktų požymių duomenų aibei - canberra")
