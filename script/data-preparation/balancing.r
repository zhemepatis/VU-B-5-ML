library(dplyr)

sample_by_label <- function(data, label, size) {
  result <- data[data$label == label, ]
  result <- result[rowSums(is.na(result)) != ncol(result), ]
  result <- result[sample(seq_len(nrow(result)), size), ]

  return(result)
}

# suzinom, kurios klases objektu yra maziau
label_count <- count(ekg_data, label)
min_label_count <- min(label_count$n)

# susimazinam aibe
sample_0 <- sample_by_label(ekg_data, 0.0, min_label_count)
sample_1 <- sample_by_label(ekg_data, 1.0, min_label_count)

ekg_data <- rbind(sample_0, sample_1)

# istrinam resursus, kuriu nebenaudosim
rm(sample_by_label)
rm(label_count)
rm(min_label_count)
rm(sample_0)
rm(sample_1)
