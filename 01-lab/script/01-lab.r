# setwd("/home/rink/repos/UNI-B-5-ML")
# ekg_data <- read.csv("ecg-data.csv", sep = ";", header = TRUE, na.strings=c(""))

# # converting from char to num
# ekg_data$RR_r_0 <- as.numeric(as.character(ekg_data$RR_r_0))
# ekg_data$RR_r_0.RR_r_1 <- as.numeric(as.character(ekg_data$RR_r_0.RR_r_1))
# summary(ekg_data)

# set.seed(222)  # To make sampling reproducible

# sample_0 <- ekg_data[ekg_data$label == 0.0,]
# sample_0 <- sample_0[rowSums(is.na(sample_0)) != ncol(sample_0),]
# sample_0 <- sample_0[sample(1:nrow(sample_0), 1000),]

# sample_1 <- ekg_data[ekg_data$label == 1.0,]
# sample_1 <- sample_1[rowSums(is.na(sample_1)) != ncol(sample_1),]
# sample_1 <- sample_1[sample(1:nrow(sample_1), 1000),]

# sample_2 <- ekg_data[ekg_data$label == 2.0,]
# sample_2 <- sample_2[rowSums(is.na(sample_2)) != ncol(sample_2),]
# sample_2 <- sample_2[sample(1:nrow(sample_2), 1000),]

# Calculating min, max, quantiles, median, mean, NA values
# summary(sample_0)
# summary(sample_1)
# summary(sample_2)

# Sujungiam duomenis i viena kruva
# ekg_data <- rbind(sample_0, sample_1, sample_2)

# # Funkcija uzpildanti santykius
# fill_in_missing_ratio <- function(sample_data, missing_column, counter_column, denominator_column) {
#   sample_data[[missing_column]] <- ifelse(is.na(sample_data[[missing_column]]), 
#                                           sample_data[[counter_column]] / sample_data[[denominator_column]], 
#                                           sample_data[[missing_column]])
#   return(sample_data)
# }

# Uzpildom RR_l_x / RR_l_x+1
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_l_0.RR_l_1", "RR_l_0", "RR_l_1")
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_l_1.RR_l_2", "RR_l_1", "RR_l_2")
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_l_2.RR_l_3", "RR_l_2", "RR_l_3")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(ekg_data$RR_l_0.RR_l_1)) 
any(is.na(ekg_data$RR_l_1.RR_l_2)) 
any(is.na(ekg_data$RR_l_2.RR_l_3))

# Uzpildom RR_r_x / RR_r_x+1
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_r_0.RR_r_1", "RR_r_0", "RR_r_1")
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_r_1.RR_r_2", "RR_r_1", "RR_r_2")
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_r_2.RR_r_3", "RR_r_2", "RR_r_3")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(ekg_data$RR_r_0.RR_r_1)) 
any(is.na(ekg_data$RR_r_1.RR_r_2)) 
any(is.na(ekg_data$RR_r_2.RR_r_3))

# Funkcija uzpildanti RR_l_x, kai turim RR_l_x / RR_r_x+1
fill_in_missing_intervals_with_multiplication <- function(sample_data, missing_column, mult_column1, mult_column2) {
  sample_data[[missing_column]] <- ifelse(is.na(sample_data[[missing_column]]), 
                                          sample_data[[mult_column1]] * sample_data[[mult_column2]], 
                                          sample_data[[missing_column]])
  return(sample_data)
}

fill_in_missing_intervals_with_division <- function(sample_data, missing_column, ratio_column, divisor_column) {
  sample_data[[missing_column]] <- ifelse(is.na(sample_data[[missing_column]]), 
                                          (sample_data[[ratio_column]] / sample_data[[divisor_column]]) ^ (-1), 
                                          sample_data[[missing_column]])
  return(sample_data)
}

# Uzpildom RR_l_x
ekg_data <- fill_in_missing_intervals_with_multiplication(ekg_data, "RR_l_0", "RR_l_0.RR_l_1", "RR_l_1")
ekg_data <- fill_in_missing_intervals_with_multiplication(ekg_data, "RR_l_1", "RR_l_1.RR_l_2", "RR_l_2")
ekg_data <- fill_in_missing_intervals_with_multiplication(ekg_data, "RR_l_2", "RR_l_2.RR_l_3", "RR_l_3")
ekg_data <- fill_in_missing_intervals_with_division(ekg_data, "RR_l_3", "RR_l_2.RR_l_3", "RR_l_2")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(ekg_data$RR_l_0)) 
any(is.na(ekg_data$RR_l_1)) 
any(is.na(ekg_data$RR_l_2)) 
any(is.na(ekg_data$RR_l_3))

# Uzpildom RR_r_x
ekg_data <- fill_in_missing_intervals_with_multiplication(ekg_data, "RR_r_0", "RR_r_0.RR_r_1", "RR_r_1")
ekg_data <- fill_in_missing_intervals_with_multiplication(ekg_data, "RR_r_1", "RR_r_1.RR_r_2", "RR_r_2")
ekg_data <- fill_in_missing_intervals_with_multiplication(ekg_data, "RR_r_2", "RR_r_2.RR_r_3", "RR_r_3")
ekg_data <- fill_in_missing_intervals_with_division(ekg_data, "RR_r_3", "RR_r_2.RR_r_3", "RR_r_2")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(ekg_data$RR_r_0)) 
any(is.na(ekg_data$RR_r_1)) 
any(is.na(ekg_data$RR_r_2)) 
any(is.na(ekg_data$RR_r_3)) 

# Funkcija uzpildanti praleista reiksme naudojant sudeti
# fill_in_missing_values_with_sum <- function(sample_data, missing_column, sum_column1, sum_column2) {
#   sample_data[[missing_column]] <- ifelse(is.na(sample_data[[missing_column]]), 
#                                            sample_data[[sum_column1]] + sample_data[[sum_column2]], 
#                                            sample_data[[missing_column]])
  
#   return(sample_data)
# }

# Funkcija uzpildanti praleista reiksme naudojant atimti
# fill_in_missing_values_with_difference <- function(sample_data, missing_column, diff_column1, diff_column2) {
#   sample_data[[missing_column]] <- ifelse(is.na(sample_data[[missing_column]]), 
#                                           sample_data[[diff_column1]] - sample_data[[diff_column2]], 
#                                           sample_data[[missing_column]])
  
#   return(sample_data)
# }

# Uzpildom seq_size, wl_side, wr_side
ekg_data <- fill_in_missing_values_with_sum(ekg_data, "seq_size", "wl_side", "wr_side")
ekg_data <- fill_in_missing_values_with_difference(ekg_data, "wl_side", "seq_size", "wr_side")
ekg_data <- fill_in_missing_values_with_difference(ekg_data, "wr_side", "seq_size", "wl_side")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(ekg_data$seq_size)) 
any(is.na(ekg_data$wl_side)) 
any(is.na(ekg_data$wr_side))

# Sampling so that missing values would be calculated more accurately
sample_0 <- ekg_data[ekg_data$label == 0.0,]
sample_0 <- sample_0[rowSums(is.na(sample_0)) != ncol(sample_0),]

sample_1 <- ekg_data[ekg_data$label == 1.0,]
sample_1 <- sample_1[rowSums(is.na(sample_1)) != ncol(sample_1),]

sample_2 <- ekg_data[ekg_data$label == 2.0,]
sample_2 <- sample_2[rowSums(is.na(sample_2)) != ncol(sample_2),]

# Funkcija skaiciuoti mediana konkreciam pozymiui
calculate_median <- function(sample_data, col_name) {
  sample_data[[col_name]] <- ifelse(is.na(sample_data[[col_name]]),  
                                    median(sample_data[[col_name]], na.rm = TRUE),  
                                    sample_data[[col_name]])
  
  return(sample_data)
}

# Funkcija skaiciuoti vidurki konkreciam pozymiui
calculate_mean <- function(sample_data, col_name) {
  sample_data[[col_name]] <- ifelse(is.na(sample_data[[col_name]]),  
                                    mean(sample_data[[col_name]], na.rm = TRUE),  
                                    sample_data[[col_name]])
  
  return(sample_data)
}

# Funkcija uzpildanti praleistas reiksmes pozymiam, kuriu negalima buvo apskaiciuoti kitaip
# fill_in_missing_values <- function(sample_data, col_name) {
#   test_result <- shapiro.test(sample_data[[col_name]])

#   if (test_result$p.value < 0.05) {
#     sample_data <- calculate_median(sample_data, col_name)
#   }
#   else {
#     sample_data <- calculate_mean(sample_data, col_name)
#   }

#   return(sample_data)
# }

# Uzpildom RR_l_3 / RR_l_4
sample_0 <- fill_in_missing_values(sample_0, "RR_l_3.RR_l_4")

qqnorm(sample_1$RR_l_3.RR_l_4)
qqline(sample_1$RR_l_3.RR_l_4, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "RR_l_3.RR_l_4")

qqnorm(sample_2$RR_l_3.RR_l_4)
qqline(sample_2$RR_l_3.RR_l_4, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "RR_l_3.RR_l_4")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$RR_l_3.RR_l_4))
any(is.na(sample_1$RR_l_3.RR_l_4))
any(is.na(sample_2$RR_l_3.RR_l_4))

# Uzpildom RR_r_3 / RR_r_4
qqnorm(sample_0$RR_r_3.RR_r_4)
qqline(sample_0$RR_r_3.RR_r_4, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "RR_r_3.RR_r_4")

qqnorm(sample_1$RR_r_3.RR_r_4)
qqline(sample_1$RR_r_3.RR_r_4, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "RR_r_3.RR_r_4")

qqnorm(sample_2$RR_r_3.RR_r_4)
qqline(sample_2$RR_r_3.RR_r_4, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "RR_r_3.RR_r_4")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$RR_r_3.RR_r_4))
any(is.na(sample_1$RR_r_3.RR_r_4))
any(is.na(sample_2$RR_r_3.RR_r_4))

# Uzpildom signal_mean
qqnorm(sample_0$signal_mean)
qqline(sample_0$signal_mean, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "signal_mean")

qqnorm(sample_1$signal_mean)
qqline(sample_1$signal_mean, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "signal_mean")

qqnorm(sample_2$signal_mean)
qqline(sample_2$signal_mean, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "signal_mean")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$signal_mean))
any(is.na(sample_1$signal_mean))
any(is.na(sample_2$signal_mean))

# Uzpildom signal_std
qqnorm(sample_0$signal_std)
qqline(sample_0$signal_std, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "signal_std")

qqnorm(sample_1$signal_std)
qqline(sample_1$signal_std, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "signal_std")

qqnorm(sample_2$signal_std)
qqline(sample_2$signal_std, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "signal_std")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$signal_std))
any(is.na(sample_1$signal_std))
any(is.na(sample_2$signal_std))

# Uzpildom P_val
qqnorm(sample_0$P_val)
qqline(sample_0$P_val, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "P_val")

qqnorm(sample_1$P_val)
qqline(sample_1$P_val, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "P_val")

qqnorm(sample_2$P_val)
qqline(sample_2$P_val, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "P_val")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$P_val))
any(is.na(sample_1$P_val))
any(is.na(sample_2$P_val))

# Uzpildom Q_val
qqnorm(sample_0$Q_val)
qqline(sample_0$Q_val, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "Q_val")

qqnorm(sample_1$Q_val)
qqline(sample_1$Q_val, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "Q_val")

qqnorm(sample_2$Q_val)
qqline(sample_2$Q_val, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "Q_val")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$Q_val))
any(is.na(sample_1$Q_val))
any(is.na(sample_2$Q_val))

# Uzpildom R_val
qqnorm(sample_0$R_val)
qqline(sample_0$R_val, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "R_val")

qqnorm(sample_1$R_val)
qqline(sample_1$R_val, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "R_val")

qqnorm(sample_2$R_val)
qqline(sample_2$R_val, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "R_val")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$R_val))
any(is.na(sample_1$R_val))
any(is.na(sample_2$R_val))

# Uzpildom S_val
qqnorm(sample_0$S_val)
qqline(sample_0$S_val, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "S_val")

qqnorm(sample_1$S_val)
qqline(sample_1$S_val, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "S_val")

qqnorm(sample_2$S_val)
qqline(sample_2$S_val, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "S_val")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$S_val))
any(is.na(sample_1$S_val))
any(is.na(sample_2$S_val))

# Uzpildom T_val
qqnorm(sample_0$T_val)
qqline(sample_0$T_val, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "T_val")

qqnorm(sample_1$T_val)
qqline(sample_1$T_val, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "T_val")

qqnorm(sample_2$T_val)
qqline(sample_2$T_val, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "T_val")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$T_val))
any(is.na(sample_1$T_val))
any(is.na(sample_2$T_val))

# Uzpildom P_pos
qqnorm(sample_0$P_pos)
qqline(sample_0$P_pos, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "P_pos")

qqnorm(sample_1$P_pos)
qqline(sample_1$P_pos, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "P_pos")

qqnorm(sample_2$P_pos)
qqline(sample_2$P_pos, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "P_pos")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$P_pos))
any(is.na(sample_1$P_pos))
any(is.na(sample_2$P_pos))

# Uzpildom Q_pos
qqnorm(sample_0$Q_pos)
qqline(sample_0$Q_pos, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "Q_pos")

qqnorm(sample_1$Q_pos)
qqline(sample_1$Q_pos, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "Q_pos")

qqnorm(sample_2$Q_pos)
qqline(sample_2$Q_pos, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "Q_pos")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$Q_pos))
any(is.na(sample_1$Q_pos))
any(is.na(sample_2$Q_pos))

# Uzpildom R_pos
qqnorm(sample_0$R_pos)
qqline(sample_0$R_pos, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "R_pos")

qqnorm(sample_1$R_pos)
qqline(sample_1$R_pos, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "R_pos")

qqnorm(sample_2$R_pos)
qqline(sample_2$R_pos, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "R_pos")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$R_pos))
any(is.na(sample_1$R_pos))
any(is.na(sample_2$R_pos))

# Uzpildom S_pos
qqnorm(sample_0$S_pos)
qqline(sample_0$S_pos, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "S_pos")

qqnorm(sample_1$S_pos)
qqline(sample_1$S_pos, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "S_pos")

qqnorm(sample_2$S_pos)
qqline(sample_2$S_pos, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "S_pos")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$S_pos))
any(is.na(sample_1$S_pos))
any(is.na(sample_2$S_pos))

# Uzpildom T_pos
qqnorm(sample_0$T_pos)
qqline(sample_0$T_pos, col = "red")
sample_0 <- fill_in_missing_values(sample_0, "T_pos")

qqnorm(sample_1$T_pos)
qqline(sample_1$T_pos, col = "red")
sample_1 <- fill_in_missing_values(sample_1, "T_pos")

qqnorm(sample_2$T_pos)
qqline(sample_2$T_pos, col = "red")
sample_2 <- fill_in_missing_values(sample_2, "T_pos")
# Patikrinam ar dar yra praleistu reiksmiu
any(is.na(sample_0$T_pos))
any(is.na(sample_1$T_pos))
any(is.na(sample_2$T_pos))

# sudedam uzpildytus duomenis i viena aibe
ekg_data <- rbind(sample_0, sample_1, sample_2)
# patikrinam ar dar liko objektu su neuzpildytom reiksmem
any(complete.cases(ekg_data))