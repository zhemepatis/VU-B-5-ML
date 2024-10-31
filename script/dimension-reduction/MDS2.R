library(MASS)
library(ggplot2)  
library(ggrepel)
library(smacof)

# Min-Max Normalization 
min_max_normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x))) 
}

ekg_data_unnormalized <- ekg_data

ekg_data_minmax <- ekg_data
columns_to_normalize <- setdiff(names(ekg_data), 'label')
for (col in columns_to_normalize) { 
  ekg_data_minmax[[col]] <- min_max_normalize(ekg_data_minmax[[col]]) 
}

set.seed(1000)

# Helper function for MDS and Plotting using smacof 
plot_mds_smacof <- function(data, columns, method = "euclidean", title, ndim = 2, eps = 1e-6, itmax = 500) {
  data_subset <- data[, columns]
  
  distance_matrix <- dist(data_subset, method = method)
  
  mds_result <- smacofSym(distance_matrix, ndim = ndim, eps = eps, itmax = itmax)
  
  mds_df <- data.frame(
    MDS1 = mds_result$conf[, 1],
    MDS2 = mds_result$conf[, 2],
    label = data$label
  )
  
  # Plot the MDS result
  x_range <- range(mds_df$MDS1)
  y_range <- range(mds_df$MDS2)
  max_range <- max(abs(c(x_range, y_range)))
  
  ggplot(mds_df, aes(x = MDS1, y = MDS2, col = as.factor(label))) +
    geom_point() +
    scale_color_manual(values = c("0" = "red", "1" = "green", "2" = "blue")) + 
    theme_minimal() +
    coord_fixed(ratio = 1, xlim = c(-max_range, max_range), ylim = c(-max_range, max_range)) +  
    theme(
      axis.text = element_text(size = 9), 
      axis.title = element_text(size = 10),
    ) +
    labs(color = "Klasė", title = title, x = "MDS 1 dimensija", y = "MDS 2 dimensija") + 
    guides(color = guide_legend(override.aes = list(size = 5)))  
}

# MDS for Unnormalized Data using smacof ----------------------

#plot_mds_smacof(ekg_data_unnormalized, columns = c("RR_l_0", "RR_l_0.RR_l_1", "RR_l_1", "RR_l_1.RR_l_2", 
#                                                   "RR_l_2", "RR_l_2.RR_l_3", "RR_l_3", "RR_l_3.RR_l_4", 
#                                                   "RR_r_0", "RR_r_0.RR_r_1", "RR_r_1", "RR_r_1.RR_r_2", 
#                                                   "RR_r_2", "RR_r_2.RR_r_3", "RR_r_3", "RR_r_3.RR_r_4",
#                                                   "seq_size", "signal_mean", "signal_std", "wl_side", 
#                                                   "wr_side", "P_val", "Q_val", "R_val", "S_val", "T_val",
#                                                   "P_pos", "Q_pos", "R_pos", "S_pos", "T_pos"), 
#                method = "euclidean", title = "MDS vizualizacija nenormuotai visų požymių duomenų aibei", ndim = 2, eps = 1e-6, itmax = 1000)


# MDS for Min-Max Normalized Data using smacof ------------------------

#plot_mds_smacof(ekg_data_minmax, columns = c("RR_l_0", "RR_l_0.RR_l_1", "RR_l_1", "RR_l_1.RR_l_2", 
#                                                   "RR_l_2", "RR_l_2.RR_l_3", "RR_l_3", "RR_l_3.RR_l_4", 
#                                                   "RR_r_0", "RR_r_0.RR_r_1", "RR_r_1", "RR_r_1.RR_r_2", 
#                                                   "RR_r_2", "RR_r_2.RR_r_3", "RR_r_3", "RR_r_3.RR_r_4",
#                                                   "seq_size", "signal_mean", "signal_std", "wl_side", 
#                                                   "wr_side", "P_val", "Q_val", "R_val", "S_val", "T_val",
#                                                   "P_pos", "Q_pos", "R_pos", "S_pos", "T_pos"), 
#                method = "euclidean", title = "MDS vizualizacija normuotai visų požymių duomenų aibei", ndim = 2, eps = 1e-6, itmax = 1000)


#plot_mds_smacof(ekg_data_minmax, columns = c("P_val", "T_val", "R_val", "seq_size", "wr_side", "wl_side", "signal_std"), 
#                method = "euclidean", title = "MDS vizualizacija normuotai pasirinktų požymių duomenų aibei", ndim = 2, eps = 1e-6, itmax = 1000)

#plot_mds_smacof(ekg_data_minmax, columns = c("signal_mean", "signal_std", "seq_size", "wl_side", "wr_side"), 
#                method = "euclidean", title = "MDS (euclidean) signal_mean, signal_std", ndim = 2, eps = 1e-6, itmax = 1000)