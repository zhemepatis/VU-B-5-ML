sum_up_stats <- function(accuracy, micro_stats_neg, micro_stats_pos, macro_stats) {
  result <- data.frame(
    # accuracy
    accuracy_mean = mean(accuracy),
    accuracy_variance = var(accuracy),
    
    # micro stats
    micro_precision_mean_neg = mean(micro_stats_neg$precision),
    micro_precision_variance_neg = var(micro_stats_neg$precision),
    micro_recall_mean_neg = mean(micro_stats_neg$recall),
    micro_recall_variance_neg = var(micro_stats_neg$recall),
    micro_f1_mean_neg = mean(micro_stats_neg$f1),
    micro_f1_variance_neg = var(micro_stats_neg$f1),
    
    micro_precision_mean_pos = mean(micro_stats_pos$precision),
    micro_precision_variance_pos = var(micro_stats_pos$precision),
    micro_recall_mean_pos = mean(micro_stats_pos$recall),
    micro_recall_variance_pos = var(micro_stats_pos$recall),
    micro_f1_mean_pos = mean(micro_stats_pos$f1),
    micro_f1_variance_pos = var(micro_stats_pos$f1),
    
    # macro stats
    macro_precision_mean = mean(macro_stats$precision),
    macro_precision_variance = var(macro_stats$precision),
    macro_recall_mean = mean(macro_stats$recall),
    macro_recall_variance = var(macro_stats$recall),
    macro_f1_mean = mean(macro_stats$f1),
    macro_f1_variance = var(macro_stats$f1)
  )
  
  return(data.frame(result))
}


