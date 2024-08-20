rearrange_df_columns <- function(df, cols_to_move, col_anchor) {

  col_anchor_n <- which(names(df) == col_anchor)

  df <- df %>%
    dplyr::select( dplyr::all_of(names(df)[1:col_anchor_n]), dplyr::all_of(cols_to_move), dplyr::everything() )

  df
}