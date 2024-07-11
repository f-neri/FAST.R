counts_percentages <- function(data, pos_neg_pairs) {
  count_col_names <- paste0('count_', pos_neg_pairs)
  expr_list <- list()
  for(name in count_col_names) {
    parts <- unlist(strsplit(name, "_"))
    count_perct <- parts[1]
    feature1 <- parts[2]
    sign1 <- parts[3]
    feature2 <- parts[4]
    sign2 <- parts[5]
    
    if(count_perct == 'count') {
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
    } else {
      print("percentage")
    }
    
    final_expr <- rlang::expr(!!expr1 & !!expr2)
    expr_list[[name]] <- rlang::expr(sum(!!final_expr, na.rm = TRUE))
  }
  
  summarise_expr <- rlang::exprs(!!!expr_list)
  
  result_df <- data %>%
    dplyr::summarise(!!!summarise_expr)
  
  return(result_df)
}

# generate_summary_df <- function(df, grouping_vars, feature_pairs, pos_neg_pairs) {
#   
#   # Generate the expressions for summarise(across(...))
#   summarise_expr <- purrr::map_dfc(feature_pairs, function(pair) {
#     feature1 <- pair[1]
#     feature2 <- pair[2]
#     
#     # Generate column names for positive/negative counts
#     col_names <- paste0("counts_", pos_neg_pairs)
#     
#     # Generate summarise expressions for each pair
#     expr <- dplyr::across(all_of(col_names),
#                           ~ sum(.data[[feature1]] > .data[[paste0(feature1, "_threshold")]] &
#                                   .data[[feature2]] > .data[[paste0(feature2, "_threshold")]], na.rm = TRUE))
#     expr
#   })
#   
#   # Combine the expressions with grouping and cell counts
#   summary_df <- df %>%
#     dplyr::group_by(!!!dplyr::syms(grouping_vars)) %>%
#     dplyr::summarise(
#       cell_counts = n(),
#       !!!summarise_expr,
#       .groups = 'drop'
#     )
#   
#   return(summary_df)
# }

# counts_percentages.R?
# # Define a function to calculate counts and percentages based on thresholds across multiple features
# calculate_counts_percentages <- function(data, grouping_var, features) {
#   
#   counts_percentages <- lapply(features, function(feature_name) {
#     threshold_col <- sym(paste0(feature_name, "_threshold"))
#     data %>% 
#       # dplyr::group_by({{ grouping_var }}) %>%
#     dplyr::group_by(!!!dplyr::syms(single_cell_grouping)) %>%
#       dplyr::summarise(
#         counts_positive = sum(.data[[feature_name]] > .data[[threshold_col]], na.rm = TRUE),
#         counts_negative = sum(.data[[feature_name]] <= .data[[threshold_col]], na.rm = TRUE),
#         .groups = 'drop'
#       ) %>%
#       dplyr::mutate(
#         percentage_positive = counts_positive / dplyr::n(),
#         percentage_negative = counts_negative / dplyr::n()
#       ) %>%
#       dplyr::rename_with(~ paste0(feature_name, "_", .x), contains(feature_name))
#   })
#   
#   counts_percentages_df <- bind_cols(counts_percentages)
#   
#   return(counts_percentages_df)
# }
