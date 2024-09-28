sum_if_target_empty <- function(data, target_col, sum_col1, sum_col2) {
    if (is.na(data[[target_col]])) {
        data[[target_col]] <- data[[sum_col1]] + data[[sum_col2]]
    }

    return(data)
}

subtract_if_target_empty <- function(data, target_col, minuend_col, subtrahend_col) {
    if (is.na(data[[target_col]])) {
        data[[target_col]] <- data[[sum_col1]] - data[[sum_col2]]
    }

    return(data)
}

divide_if_target_empty <- function(data, target_col, dividend_col, divisor_col) {
    if (is.na(data[[target_col]])) {
        data[[target_col]] <- data[[dividend_col]] * data[[divisor_col]]
    }

    return(data)
}

