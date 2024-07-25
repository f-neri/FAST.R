counts_percentages <- function(data, pos_neg_pairs) {
  
  # count_col_names <- paste0('count_', pos_neg_pairs)
  # perc_col_names <- paste0('percentage_', pos_neg_pairs)
  # count_perc_names <- c(count_col_names, perc_col_names)
  
  expr_list <- list()
  
  for(name in pos_neg_pairs) {
    parts <- unlist(strsplit(name, "_"))
    count_name <- paste0('count_', name)
    
    # count_perct <- parts[1]
    feature1 <- parts[1]
    sign1 <- parts[2]
    feature2 <- parts[3]
    sign2 <- parts[4]
    
    # if(count_perct == 'count') {
    if(sign1 == "positive") {
      expr1 <- rlang::parse_expr(glue::glue(".data${feature1} > .data${feature1}_threshold"))
    } else { # negative
      expr1 <- rlang::parse_expr(glue::glue(".data${feature1} <= .data${feature1}_threshold"))
    }
    if(sign2 == "positive") {
      expr2 <- rlang::parse_expr(glue::glue(".data${feature2} > .data${feature2}_threshold"))
    } else { # negative
      expr2 <- rlang::parse_expr(glue::glue(".data${feature2} <= .data${feature2}_threshold"))
    }
    # }
    
    final_expr <- rlang::expr(!!expr1 & !!expr2)
    expr_list[[count_name]] <- rlang::expr(sum(!!final_expr, na.rm = TRUE))
  }
  
  summarise_expr <- rlang::exprs(!!!expr_list)
  
  result_df <- data %>%
    dplyr::summarise(!!!summarise_expr)
  
  print("counts")
  print(dim(result_df))
  print(colnames(result_df))
  
  return(result_df)
}