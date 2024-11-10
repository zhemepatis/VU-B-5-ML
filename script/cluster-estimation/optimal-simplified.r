library(factoextra)


# Prepas
ekg_data_optsim <- ekg_data
#ekg_data_optsim <- ekg_data[, c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")]
ekg_data_optsim_norm <- normalize_data(ekg_data_optsim)
ekg_data_optsim_umap <- perform_umap(ekg_data_optsim, n_components = 6)


# Elbow
set.seed(6)
wcss = vector()
for (i in 1:15) wcss[i] = sum(kmeans(ekg_data_optsim_norm, i)$withinss)
plot(1:15,
     wcss,
     type = 'b',
     main = paste(' Elbow metodas'),
     xlab = 'Klasterių skaičius',
     ylab = 'WCSS')

set.seed(6)
wcss = vector()
for (i in 1:15) wcss[i] = sum(kmeans(ekg_data_optsim_umap, i)$withinss)
plot(1:15,
     wcss,
     type = 'b',
     main = paste(' Elbow metodas'),
     xlab = 'Klasterių skaičius',
     ylab = 'WCSS')


# Silhouette
# reiketu pakeist default asiu pavadinimus

fviz_nbclust(ekg_data_optsim_norm,
             kmeans,
             k.max = 15,
             method = "silhouette") + 
  labs(title = "Silueto metodas") +
  geom_point(color = "red", size = 2) +
  geom_line(color = "blue", size = 1)

fviz_nbclust(ekg_data_optsim_umap,
             kmeans,
             k.max = 15,
             method = "silhouette") + 
  labs(title = "Silueto metodas") +
  geom_point(color = "red", size = 2) +
  geom_line(color = "blue", size = 1)

