ekg_data <-
  read.csv("data/ecg-data.csv", sep = ";", header = TRUE, na.strings = c(""))

source("script/type-conversion.r")
source("script/sampling.r")
source("script/data-fillup.r")