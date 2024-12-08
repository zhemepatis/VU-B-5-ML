library(dplyr)
library(caTools)

split_data_into_sets <- function(data, label_colname = "label", ratio = 0.8) {
  split <- sample.split(data[[label_colname]], SplitRatio = ratio)
  test_set <- subset(data, split == FALSE)
  training_set <- subset(data, split == TRUE)

  return(list(
    bigger_set = test_set,
    smaller_set = training_set
  ))
}

set.seed(1000) # reikalinga atkuriamumui

# suzinom, kurios klases objektu yra maziau
label_count <- count(ekg_data, label)
min_label_count <- min(label_count$n)

# susimazinam aibe
ekg_data <- ekg_data %>%
  group_by(label) %>%
  sample_n(min_label_count, replace = FALSE) %>%
  ungroup()

# skaidom duomenis i mokymosi ir testavimo aibes
result <- split_data_into_sets(ekg_data)
training_set <- result$bigger_set
test_set <- result$smaller_set

target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
training_set_2d <- training_set[, target_cols]
test_set_2d <- test_set[, target_cols]