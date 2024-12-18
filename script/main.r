ekg_data <-
  read.csv("data/ecg-data.csv", sep = ";", header = TRUE, na.strings = c(""))

ekg_data <- ekg_data[ekg_data$label != 1, ]

# data preparation
source("script/data-preparation/type-conversion.r")
source("script/data-preparation/data-fillup.r")
source("script/analysis/outliers.r")
source("script/data-preparation/sampling.r")
