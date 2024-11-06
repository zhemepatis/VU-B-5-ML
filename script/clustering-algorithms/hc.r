library(cluster)
set.seed(1000)

ekg_data_umap <- ekg_data
ekg_data_umap[, c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "label")]
ekg_data_umap <- perform_umap(ekg_data_umap)
head(ekg_data_umap)

hc <- hclust(dist(ekg_data_umap, method = 'euclidean'), method = 'ward.D')

plot(hc,
     main = 'Dendrograma',
     xlab = 'Euklidinis atstumas',
     ylab = 'EKG pūpsniai',
     labels = FALSE,
     sub = '')



num_clusters <- 8
y_hc <- cutree(hc, k = num_clusters)


label_colors <- c("red", "green", "blue")

ekg_data_umap$label <- as.factor(ekg_data_umap$label)
point_colors <- label_colors[as.numeric(ekg_data_umap$label)]

clusplot(ekg_data_umap,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         col.p = point_colors,
         labels = 4,
         plotchar = FALSE,
         span = TRUE,
         main = 'Klasterių vizualizacija UMAP sumažintos dimensijos duomenų aibei',
         xlab = 'UMAP1',
         ylab = 'UMAP2')
