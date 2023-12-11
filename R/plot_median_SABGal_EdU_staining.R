plot_median_SABGal_EdU_staining <- function(data,
                                            data_thresholds,
                                            additional_variables,
                                            scale_fill_brewer) {
  ggplot(data = data,
         aes(x = log10(.data$SABGal_median),
             y = log10(.data$EdU_median),
             fill = .data$Condition)
  ) +
    geom_point(size = 3,
               shape = 21,
               color = "black") +
    geom_vline(data = data_thresholds, aes(xintercept = .data$SABGal_threshold_average_log10)) +
    geom_hline(data = data_thresholds, aes(yintercept = .data$EdU_threshold_average_log10)) +
    {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
    {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                       rows = vars(!!dplyr::sym(additional_variables[2]))) } +
    scale_x_continuous(name = expression(log[10]("Integrated SA-\u03B2-Gal OD"))) +
    scale_y_continuous(name = expression(log[10]("Integrated EdU intensity (AU)"))) +
    labs(title = "Median Well Intensity of SA-\u03B2-Gal and EdU Staining plus Staining Thresholds",
         fill = "Condition") +
    scale_fill_brewer +
    guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                    size = 3)))
}