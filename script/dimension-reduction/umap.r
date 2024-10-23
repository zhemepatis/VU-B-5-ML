library(uwot)
library(ggplot2)
library(ggrepel)

min_max_normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x))) 
}

ekg_data_minmax <- ekg_data
columns_to_normalize <- setdiff(names(ekg_data), 'label')

for (col in columns_to_normalize) { 
  ekg_data_minmax[[col]] <- min_max_normalize(ekg_data_minmax[[col]]) 
}


# n_neighbors – parametras kuris nurodo globalios ir lokalios struktūros balansą.
# Kuo mažesnis parametras, tuo labiau algoritmas skirs dėmesį lokaliai struktūrai
# ir kuo didesnis tuo labiau atsižvelgs į globalią struktūrą.

# min_dist - parametras kuris nusako koks yra minimalus atstumas
# kuriuo bus atpažįstami taškai “kaimynai”. Šis parametras nusako kaip tankiai 
# sumažintos dimensijos taškai atsispindės projekcijoje

# spread - pasiskirstymas, isbalansuojama min_dist detaliau pateikiami klasteriai

# metric - naudojama atstumo skaiciavimui
#geriausi correlation arba euclidean

# geriausi n20s3 arba n5s1

umap_ekg <- umap(ekg_data[, -which(names(ekg_data) == "label")], 
                 n_neighbors = 20, 
                 min_dist = 0.1, 
                 spread = 3,
                 metric = 'euclidean')

umap_ekg <- data.frame(
  UMAP1 = umap_ekg[, 1],
  UMAP2 = umap_ekg[, 2],
  label = ekg_data$label
)

ggplot(umap_ekg, aes(
  x = UMAP1, y = UMAP2, 
  col = as.factor(label))) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = label), size = 2.5) +
  scale_color_manual(values = c("0" = "red", "1" = "green", "2" = "blue")) +
  theme_minimal() +
  labs(color = "Klasė", 
       title = "EKG nenormuotos aibės UMAP vizualizacija", 
       x = "UMAP1", 
       y = "UMAP2")

###

umap_ekg <- umap(ekg_data_minmax[, -which(names(ekg_data_minmax) == "label")], 
                 n_neighbors = 20, 
                 min_dist = 0.1, 
                 spread = 3,
                 metric = 'euclidean')

umap_ekg <- data.frame(
  UMAP1 = umap_ekg[, 1],
  UMAP2 = umap_ekg[, 2],
  label = ekg_data$label
)

ggplot(umap_ekg, aes(
  x = UMAP1, y = UMAP2, 
  col = as.factor(label))) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = label), size = 2.5) +
  scale_color_manual(values = c("0" = "red", "1" = "green", "2" = "blue")) +
  theme_minimal() +
  labs(color = "Klasė", 
       title = "EKG normuotos aibės UMAP vizualizacija", 
       x = "UMAP1", 
       y = "UMAP2")
