library(factoextra)


# Prepas
ekg_data_optsim <- ekg_data
ekg_data_optsim <- ekg_data[, c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")]
ekg_data_optsim_umap <- perform_umap(ekg_data_optsim, n_components = 6)


# Elbow
set.seed(6)
wcss = vector()
for (i in 1:15) wcss[i] = sum(kmeans(ekg_data_optsim_umap, i)$withinss)
plot(1:15,
     wcss,
     type = 'b',
     main = paste(' Elbow metodas'),
     xlab = 'Klasterių skaičius',
     ylab = 'WCSS',
     cex.lab = 1.3,
     cex.axis = 1.2,
     cex.main = 1.5,
     cex.sub = 1.3)


# Silhouette
# reiketu pakeist default asiu pavadinimus

fviz_nbclust(ekg_data_optsim_umap,
             kmeans,
             k.max = 15,
             method = "silhouette") + 
  labs(title = "Silueto metodas", 
       x = "Klasterių skaičius", 
       y = "Vidutinė silueto reikšmė") + 
  geom_point(color = "red", size = 2) + 
  geom_line(color = "blue", size = 1) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)   
  )

