sample_by_label <- function(data, label, size) {
    result <- data[data$label == label, ]
    result <- result[rowSums(is.na(result)) != ncol(result), ]
    result <- result[sample(seq_len(nrow(result)), size), ]

    return(result)
}

set.seed(1000)

sample_size <- 1000

sample_0 <- sample_by_label(ekg_data, 0.0, sample_size)
sample_1 <- sample_by_label(ekg_data, 1.0, sample_size)
sample_2 <- sample_by_label(ekg_data, 2.0, sample_size)

ekg_data <- rbind(sample_0, sample_1, sample_2)

# istrinam resursus, kuriu nebenaudosim
rm(sample_by_label)
rm(sample_size)