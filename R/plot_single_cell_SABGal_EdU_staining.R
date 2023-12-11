plot_single_cell_SABGal_EdU_staining <- function(data,
                                                 data_thresholds,
                                                 additional_variables,
                                                 scale_color_brewer) {
  ggplot(data,
         aes(.data$SABGal_log10, .data$EdU_log10)) +
    geom_point(
      aes(color = .data$Condition),
      alpha = 1/8
    ) +
    {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
    {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                       rows = vars(!!dplyr::sym(additional_variables[2]))) } +
    geom_vline(data = data_thresholds, aes(xintercept = .data$SABGal_threshold_average_log10)) +
    geom_hline(data = data_thresholds, aes(yintercept = .data$EdU_threshold_average_log10)) +
    scale_x_continuous(name = expression(log[10]("Integrated SA-\u03B2-Gal OD")),
                       limits = c(stats::quantile(data$SABGal_log10, 0.01),
                                  stats::quantile(data$SABGal_log10, 0.99))
    ) +
    scale_y_continuous(name = expression(log[10]("Integrated EdU intensity (AU)")),
                       limits = c(stats::quantile(data$EdU_log10, 0.01),
                                  stats::quantile(data$EdU_log10, 0.99))
    ) +
    labs(title = "Single Cell Intensity of SA-\u03B2-Gal and EdU Staining plus Staining Thresholds",
         color = "Condition") +
    scale_color_brewer +
    guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                    size = 3))) # to make enlarge points in legend
}