generate_df_thresholds <- function(data,
                                   input_features,
                                   additional_variables) {
  # Take out morphological feature
  stain_inputs <- input_features[-1]
  threshold_features <- paste0(stain_inputs, "_threshold")
  
  data %>%
    dplyr::group_by(.data$plate, !!!dplyr::syms(additional_variables)) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = all_of(threshold_features),
        .fns = list(threshold_average = ~mean(.x)),
        .names = "{.col}_average"
    )) %>% 
    dplyr::ungroup()
}