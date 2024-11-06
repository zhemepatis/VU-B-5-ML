library(factoextra)


# Prepas
ekg_data_optsim <- ekg_data[, c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos")]
ekg_data_optsim_norm <- ekg_data_optsim

min_max_normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x))) 
}

for (col in names(ekg_data_optsim_norm)) { 
  ekg_data_optsim_norm[[col]] <- min_max_normalize(ekg_data_optsim_norm[[col]])
}


# Elbow
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(ekg_data_optsim, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste(' Elbow metodas'),
     xlab = 'Klasterių skaičius',
     ylab = 'WCSS')

set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(ekg_data_optsim_norm, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste(' Elbow metodas'),
     xlab = 'Klasterių skaičius',
     ylab = 'WCSS')


# Silhouette
# reiketu pakeist default asiu pavadinimus

fviz_nbclust(ekg_data_optsim,  
             kmeans,  
             method = "silhouette") + 
  labs(title = "Silueto metodas") +
  geom_point(color = "red", size = 2) +
  geom_line(color = "blue", size = 1)

fviz_nbclust(ekg_data_optsim_norm,
             kmeans,  
             method = "silhouette") + 
  labs(title = "Silueto metodas") +
  geom_point(color = "red", size = 2) +
  geom_line(color = "blue", size = 1)