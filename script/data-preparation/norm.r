library(dplyr)

min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

ekg_data_minmax <- ekg_data
columns_to_normalize <- setdiff(colnames(ekg_data), 'label')

for (col in columns_to_normalize) {
  ekg_data_minmax[[col]] <- min_max_normalize(ekg_data_minmax[[col]])
}

# istrinam resursus, kuriu nebenaudosim
rm(min_max_normalize)

rm(columns_to_normalize)
rm(col)