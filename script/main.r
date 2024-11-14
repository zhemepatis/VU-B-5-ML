ekg_data <-
  read.csv("data/ecg-data.csv", sep = ";", header = TRUE, na.strings = c(""))

# data preparation
source("script/data-preparation/type-conversion.r")
source("script/data-preparation/data-fillup.r")
source("script/analysis/outliers.r")
source("script/data-preparation/sampling.r")
source("script/data-preparation/minmax-func.r")
source("script/dimension-reduction/umap.r")
