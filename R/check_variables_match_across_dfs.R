check_variables_match_across_dfs <- function(df_list) {
  
  # Function to check if the names match the first dataframe
  names_match_first_df <- function(df, first_df_names) {
    all(sort(base::names(df)) == first_df_names)
  }
  
  # Extract and sort the names of the first data frame in df_list
  first_df_names_sorted <- sort(base::names(df_list[[1]]))
  
  # Identify data frames that do not match
  non_matching_dfs <- base::sapply(df_list, names_match_first_df, first_df_names_sorted)
  
  # Get the indices of the data frames with non-matching names
  non_matching_indices <- base::which(!non_matching_dfs)
  
  # Return the indices of non-matching data frames
  non_matching_indices
}