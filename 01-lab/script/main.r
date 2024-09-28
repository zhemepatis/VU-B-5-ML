ekg_data <- read.csv("data/ecg-data.csv", sep = ";", header = TRUE, na.strings=c(""))

# patikrinam duomenu tipus ir konvertuojam juos
str(ekg_data)
ekg_data$RR_r_0 <- as.numeric(as.character(ekg_data$RR_r_0))
ekg_data$RR_r_0.RR_r_1 <- as.numeric(as.character(ekg_data$RR_r_0.RR_r_1))

# atsitiktinai pasirenkam duomenis
set.seed(222)

sample_0 <- ekg_data[ekg_data$label == 0.0,]
sample_0 <- sample_0[rowSums(is.na(sample_0)) != ncol(sample_0),]
sample_0 <- sample_0[sample(1:nrow(sample_0), 1000),]

sample_1 <- ekg_data[ekg_data$label == 1.0,]
sample_1 <- sample_1[rowSums(is.na(sample_1)) != ncol(sample_1),]
sample_1 <- sample_1[sample(1:nrow(sample_1), 1000),]

sample_2 <- ekg_data[ekg_data$label == 2.0,]
sample_2 <- sample_2[rowSums(is.na(sample_2)) != ncol(sample_2),]
sample_2 <- sample_2[sample(1:nrow(sample_2), 1000),]