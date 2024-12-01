library(dplyr)
library(caTools)

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

# skaidom i mokymosi, validavimo, testavimo aibes
split <- sample.split(ekg_data$label, SplitRatio = 0.8)
test_set <- subset(ekg_data, split == FALSE)

temp_set <- subset(ekg_data, split == TRUE)

split <- sample.split(temp_set, SplitRatio = 0.8)
training_set = subset(temp_set, split == TRUE)
validation_set = subset(temp_set, split == FALSE)

# panaikinam nebereikalingus resursus
rm(sample_by_label)
rm(label_count)
rm(data)
rm(idx)
rm(curr_label)
rm(curr_sample)
rm(min_label_count)
rm(split)
rm(temp_set)
