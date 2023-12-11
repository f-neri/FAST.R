generate_df_thresholds <- function(data,
                                   additional_variables) {
  data %>%
    dplyr::group_by(.data$plate, !!!dplyr::syms(additional_variables)) %>%
    dplyr::summarise(EdU_threshold_average = mean(.data$EdU_threshold),
                     SABGal_threshold_average = mean(.data$SABGal_threshold)) %>%
    dplyr::ungroup()
}