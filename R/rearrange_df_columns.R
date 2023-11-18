rearrange_df_columns <- function(df, cols_to_move, col_anchor) {
  
  col_anchor_n <- which(names(df) == col_anchor)
  
  df <- df %>%
    dplyr::select( tidyr::all_of(names(df)[1:col_anchor_n]), tidyr::all_of(cols_to_move), everything() )
  
  df
}