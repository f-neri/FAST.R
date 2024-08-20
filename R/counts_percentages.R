counts_percentages <- function(data, pos_neg_pairs) {
  
  expr_list <- list()
  
  for(name in pos_neg_pairs) {
    parts <- unlist(strsplit(name, "_"))
    count_name <- paste0('count_', name)
    
    feature1 <- parts[1]
    sign1 <- parts[2]
    feature2 <- parts[3]
    sign2 <- parts[4]
    
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
    
    final_expr <- rlang::expr(!!expr1 & !!expr2)
    expr_list[[count_name]] <- rlang::expr(sum(!!final_expr, na.rm = TRUE))
  }
  
  summarise_expr <- rlang::exprs(!!!expr_list)
  
  result_df <- data %>%
    dplyr::summarise(!!!summarise_expr)
  
  return(result_df)
}