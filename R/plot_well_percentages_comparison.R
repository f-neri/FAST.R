plot_well_percentages_comparison <- function(data,
                                             add_vars,
                                             comparison_var,
                                             other_add_var) {
  
  ggplot(data,
         aes(percentage_SABGal_positive, percentage_EdU_positive)
         ) +
    geom_point(
      shape = 21,
      color = "black",
      aes(fill = !!dplyr::sym(comparison_var)),
      size = 3) +
    {if (length(other_add_var) == 0) facet_grid(cols = vars(Condition))} +
    {if (length(other_add_var) == 1) facet_grid(cols = vars(Condition),
                                                rows = vars(!!dplyr::sym(other_add_var)))} +
    scale_x_continuous(
      limits = c(0,1),
      breaks = seq(0, 1, by = 0.2),
      labels = scales::percent,
      name = "% SA-\u03B2-Gal+ cells"
      ) +
    scale_y_continuous(
      limits = c(0,1),
      breaks = seq(0, 1, by = 0.2),
      labels = scales::percent,
      name = "% EdU+ cells"
    ) +
    labs(title = paste0("Well % of SA-\u03B2-Gal+ EdU+ Cells; Treatment Comparison: ", comparison_var)) +
    scale_fill_grey(start = 1, end = 0) +
    guides(fill = guide_legend(override.aes = list(alpha = 1,
                                                    size = 3))) # to ensure colors in legend are visible
}