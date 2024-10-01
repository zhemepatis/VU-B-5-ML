source("script/functions/fillup.r") # ikeliam funkcijas duomenu uzpildymui

ekg_data <- read.csv("data/ecg-data.csv", sep = ";", header = TRUE, na.strings=c(""))

# patikrinam duomenu tipus ir konvertuojam juos
str(ekg_data)
ekg_data$RR_r_0 <- as.numeric(as.character(ekg_data$RR_r_0))
ekg_data$RR_r_0.RR_r_1 <- as.numeric(as.character(ekg_data$RR_r_0.RR_r_1))

# uzpildom duomenis isvestinem reiksmem
ekg_data <- multiply_if_target_empty(ekg_data, "RR_l_0", "RR_l_0.RR_l_1", "RR_l_1")
ekg_data <- sum_if_target_empty(ekg_data, "seq_size", "wl_side", "wr_side")

# pratrinam duomenu eilutes, kuriose yra duomenu, kuriu negalim uzpildyti isvestinem reiksmem
unfillable_cols = list("signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")
for (i in 1:length(unfillable_cols)) {
  curr_col_name = unfillable_cols[[i]]
  ekg_data <- delete_if_target_empty(ekg_data, curr_col_name)
}

# atsitiktinai pasirenkam duomenis
set.seed(1000)

sample_0 <- ekg_data[ekg_data$label == 0.0,]
sample_0 <- sample_0[rowSums(is.na(sample_0)) != ncol(sample_0),]
sample_0 <- sample_0[sample(1:nrow(sample_0), 1000),]

sample_1 <- ekg_data[ekg_data$label == 1.0,]
sample_1 <- sample_1[rowSums(is.na(sample_1)) != ncol(sample_1),]
sample_1 <- sample_1[sample(1:nrow(sample_1), 1000),]

sample_2 <- ekg_data[ekg_data$label == 2.0,]
sample_2 <- sample_2[rowSums(is.na(sample_2)) != ncol(sample_2),]
sample_2 <- sample_2[sample(1:nrow(sample_2), 1000),]

ekg_data <- rbind(sample_0, sample_1, sample_2)

# atrenkame 9 pozymius
ekg_data <- ekg_data[, c("RR_l_0", "seq_size", "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")]
sample_0 <- sample_0[, c("RR_l_0", "seq_size", "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")]
sample_1 <- sample_1[, c("RR_l_0", "seq_size", "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")]
sample_2 <- sample_2[, c("RR_l_0", "seq_size", "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")]


# patikrinam, ar yra eiluciu su tusciom reiksmem
any(!complete.cases(ekg_data))