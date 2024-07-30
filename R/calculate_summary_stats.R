# Define a function to calculate summary statistics for a single feature
calculate_summary_stats <- function(data, single_cell_grouping, feature_name, keep_all_cols = FALSE) {
  
  threshold_col <- sym(paste0(feature_name, "_threshold"))
  summary_stat_colnames <- c(
    paste0(rlang::as_name(feature_name), c('_min', '_25th', '_median', '_75th', '_max')),
    paste0(feature_name, "_threshold"),
    paste0('count_', feature_name, "_positive"),
    paste0('count_', feature_name, "_negative"),
    paste0('percentage_', feature_name, "_positive"),
    paste0('percentage_', feature_name, "_negative")
  )
  cols_to_select <- c(rlang::as_name(feature_name), as.character(threshold_col))

  summary_stats <- 
    data %>%
    dplyr::select(all_of(cols_to_select)) %>%
    dplyr::summarise(
      cell_counts = dplyr::n(),
      across(
        .cols = {{ feature_name }},
        .fns = list(
          min = ~ min(.x, na.rm = TRUE),
          q25 = ~ quantile(.x, 0.25, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          q75 = ~ quantile(.x, 0.75, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          threshold = ~ mean(!!threshold_col, na.rm = TRUE),
          positive_count = ~ sum(.x > !!threshold_col, na.rm = TRUE),
          negative_count = ~ sum(.x <= !!threshold_col, na.rm = TRUE),
          positive_percent =  ~ sum(.x > !!threshold_col, na.rm = TRUE) / .data$cell_counts,
          negative_percent =  ~ sum(.x <= !!threshold_col, na.rm = TRUE) / .data$cell_counts
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = 'drop'
    )
  
  colnames(summary_stats) <- c(single_cell_grouping, 'cell_counts', summary_stat_colnames)
  
  if (!keep_all_cols) {
    summary_stats <- summary_stats %>%
      dplyr::select(all_of(summary_stat_colnames))
  }
  
  # print("summary stats")
  # print(names(summary_stats))
  # 
  # # Take off cell_counts vol
  # summary_stats <- summary_stats %>%
  #   dplyr::select(-cell_counts)
  
  # print("summary stats")
  # print(names(summary_stats))
  
  return(summary_stats)
}