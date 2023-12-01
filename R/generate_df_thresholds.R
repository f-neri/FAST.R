generate_df_thresholds <- function(data,
                                   additional_variables) {
  data %>%
    dplyr::group_by(plate, !!!dplyr::syms(additional_variables)) %>%
    dplyr::summarise(EdU_threshold_average = mean(EdU_threshold),
                     SABGal_threshold_average = mean(SABGal_threshold)) %>%
    dplyr::ungroup()
}