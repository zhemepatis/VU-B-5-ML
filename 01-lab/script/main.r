setwd("/home/rink/repos/UNI-B-5-ML/01-lab")
ekg_data <- read.csv("data/ecg-data.csv", sep = ";", header = TRUE, na.strings=c(""))

# patikrinam duomenu tipus ir konvertuojam juos
str(ekg_data)
ekg_data$RR_r_0 <- as.numeric(as.character(ekg_data$RR_r_0))
ekg_data$RR_r_0.RR_r_1 <- as.numeric(as.character(ekg_data$RR_r_0.RR_r_1))