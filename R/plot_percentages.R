plot_percentages <- function(data,
                             additional_variables,
                             size_axis_text) {
  
  # Find the position of the "cell_counts" column
  cell_counts_pos <- which(names(data) == "cell_counts")
  
  # Select columns that come before "cell_counts"
  columns_before_cell_counts <- names(data)[1:(cell_counts_pos - 1)]
  
  # Select columns that contain the word "percentage"
  columns_with_percentage <- grep("percentage", names(data), value = TRUE)
  
  # Combine the selections
  df_percentages <- data %>%
    dplyr::select(dplyr::all_of(c(columns_before_cell_counts, columns_with_percentage)))
  
  # pivot longer 
  cols_to_pivot <- c("percentage_EdU_negative_SABGal_negative",
                     "percentage_EdU_negative_SABGal_positive",
                     "percentage_EdU_positive_SABGal_negative",
                     "percentage_EdU_positive_SABGal_positive")
  
  df_percentages <- df_percentages %>%
    tidyr::pivot_longer(dplyr::all_of(cols_to_pivot), names_to = "Positivity", values_to = "Percentage")
  
  # adjust Positivity string values
  df_percentages$Positivity <- df_percentages$Positivity %>%
    stringr::str_replace_all(c("percentage_" = "", "_positive" = "+", "_negative" = "-", "_" = " "))
  
  # create grouped df
  df_percentages_summary <- df_percentages %>%
    dplyr::group_by(Condition, Positivity, !!!dplyr::syms(additional_variables)) %>%
    dplyr::summarise(Percentage = mean(Percentage)) %>%
    dplyr::ungroup()
  
  
  # generate stacked plot
  ggplot(df_percentages_summary, aes(x = Percentage, y = Condition, fill = Positivity)) +
    geom_col(position = "fill") +
    labs(title = "Overall % of SA-\u03B2-Gal+/- EdU+/- Cells",
         y = "Condition") +
    geom_text(aes(label = ifelse(Percentage <= 0.10, "", scales::percent(Percentage, accuracy = 1.0))),
              position = position_fill(vjust = 0.5),
              size = size_axis_text/3) +
    scale_x_continuous(labels = scales::percent, name = NULL, breaks = NULL) +
    {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
    {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                       rows = vars(!!dplyr::sym(additional_variables[2])))} +
    scale_fill_manual(values = c("gray88", "gray50", "green", "darkgreen")) +
    guides(fill = guide_legend(title = NULL)) +
    theme(legend.key.height = unit(0.5, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm')) #change legend key width
}