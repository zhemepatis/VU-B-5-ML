library(ggplot2)
library(dplyr)
library(tidyr)

min_max_normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x))) 
}

mean_std_normalize <- function(x) { 
  return((x - mean(x)) / sqrt(var(x))) 
}


ekg_data_minmax <- ekg_data
ekg_data_meanstd <- ekg_data

columns_to_normalize <- c("RR_l_0", "seq_size", "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos")

for (col in columns_to_normalize) { 
  ekg_data_minmax[[col]] <- min_max_normalize(ekg_data_minmax[[col]]) 
}

for (col in columns_to_normalize) { 
  ekg_data_meanstd[[col]] <- mean_std_normalize(ekg_data_meanstd[[col]]) 
}


long_data_minmax <- ekg_data_minmax %>%
  pivot_longer(cols = columns_to_normalize, names_to = "attribute", values_to = "norm_value") 

long_data_meanstd <- ekg_data_meanstd %>%
  pivot_longer(cols = columns_to_normalize, names_to = "attribute", values_to = "norm_value") 


ggplot(long_data_minmax, aes(x = factor(label), y = norm_value, fill = attribute)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Pūpsnio klasė", y = "Normuota reikšmė", fill = "Požymis") +
  ggtitle("Normuotos reikšmės pagal pūpsnio klasę (Min-Max)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.background = element_blank())


ggplot(long_data_meanstd, aes(x = factor(label), y = norm_value, fill = attribute)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Pūpsnio klasė", y = "Normuota reikšmė", fill = "Požymis") +
  ggtitle("Normuotos reikšmės pagal pūpsnio klasę (Mean-Std)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.background = element_blank())
