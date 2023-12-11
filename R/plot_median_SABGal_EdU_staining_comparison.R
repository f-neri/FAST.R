plot_median_SABGal_EdU_staining_comparison <- function(data,
                                                       add_vars,
                                                       comparison_var,
                                                       other_add_var) {
  ggplot(data = data,
         aes(x = log10(.data$SABGal_median),
             y = log10(.data$EdU_median),
             fill = !!dplyr::sym(comparison_var))
  ) +
    geom_point(size = 3,
               shape = 21,
               color = "black") +
    {if (length(other_add_var) == 0) facet_grid(cols = vars(.data$Condition))} +
    {if (length(other_add_var) == 1) facet_grid(cols = vars(.data$Condition),
                                                rows = vars(!!dplyr::sym(other_add_var)))} +
    scale_x_continuous(name = expression(log[10]("Integrated SA-\u03B2-Gal OD"))) +
    scale_y_continuous(name = expression(log[10]("Integrated EdU intensity (AU)"))) +
    labs(title = paste0("Median Well Intensity of SA-\u03B2-Gal and EdU Staining; Treatment Comparison: ", comparison_var),
         fill = comparison_var) +
    scale_fill_grey(start = 1, end = 0) +
    guides(fill = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                    size = 3)))
}