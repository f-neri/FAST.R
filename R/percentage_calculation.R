percentage_calculation <- function(data, pos_neg_pairs, cell_counts) {
  
  for(name in pos_neg_pairs) {
    count_name <- paste0('count_', name)
    perc_name <- paste0('percentage_', name)
    
    if(count_name %in% colnames(data)) {
      data[[perc_name]] <- data[[count_name]] / cell_counts
    }
  }
  return(data)
}