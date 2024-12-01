library(dplyr)
library(caTools)

split_data_into_sets <- function(data, label_colname = "label") {
  split <- sample.split(data[[label_colname]], SplitRatio = 0.8)
  test_set <- subset(data, split == FALSE)
  temp_set <- subset(data, split == TRUE)

  split <- sample.split(temp_set[[label_colname]], SplitRatio = 0.8)
  training_set <- subset(temp_set, split == TRUE)
  validation_set <- subset(temp_set, split == FALSE)
  
  return(list(
    test_set = test_set,
    training_set = training_set,
    validation_set = validation_set
  ))
}

sample_by_label <- function(data, label, size) {
  result <- data[data$label == label, ]
  result <- result[rowSums(is.na(result)) != ncol(result), ]
  result <- result[sample(seq_len(nrow(result)), size), ]
  
  return(result)
}

set.seed(1000) # reikalinga atkuriamumui

# suzinom, kurios klases objektu yra maziau
label_count <- count(ekg_data, label)
min_label_count <- min(label_count$n)

# susimazinam aibe
data <- data.frame()
for (idx in 1:1:nrow(label_count)) {
  curr_label <- label_count$label[idx]
  curr_sample <- sample_by_label(ekg_data, curr_label, min_label_count)
  data <- rbind(data, curr_sample)
}
ekg_data <- data

# skaidom duomenis i mokymosi, validavimo, testavimo aibes
result <- split_data_into_sets(ekg_data)
test_set <- result[[1]]
training_set <- result[[2]]
validation_set <- result[[3]]

target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
test_set_2d <- test_set[, target_cols]
training_set_2d <- training_set[, target_cols]
validation_set_2d <- validation_set[, target_cols]

# panaikinam nebereikalingus resursus
rm(sample_by_label)
rm(label_count)
rm(data)
rm(curr_label)
rm(curr_sample)
rm(min_label_count)