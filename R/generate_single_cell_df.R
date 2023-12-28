generate_single_cell_df <- function(Input_files) {
  # tidy IAoutput and merge with metadata -----------------------------------
  
  Input_files$tidy_df <- vector(mode = "list", length = nrow(Input_files)) # create emtpy list-column to store tidy data
  
  for (i in seq_len(nrow(Input_files))) {
    
    # tidy IAoutput
    tidied_IAoutput <- tidy_IAoutput(Input_files$IAoutput_df[[i]])
    
    # add metadata to tidied_IAoutput
    df <- dplyr::left_join(tidied_IAoutput, Input_files$metadata_df[[i]])
    
    # add plate identifier (i.e. file name) and rearrange column names
    variable_names <- names(Input_files$metadata_df[[i]])[-(names(Input_files$metadata_df[[i]]) == "well")]
    
    df <- df %>%
      dplyr::mutate(plate = Input_files$IAoutput_name[i]) %>% # add plate name
      dplyr::select(.data$plate, .data$well, .data$cell_ID, dplyr::all_of(variable_names), dplyr::everything()) # rearrange
    
    # return df
    Input_files$tidy_df[[i]] <- df
  }
  
  # merge tidy_dfs into a single df
  single_cell_df <- dplyr::bind_rows(Input_files$tidy_df)
  
  # return df
  single_cell_df
}
