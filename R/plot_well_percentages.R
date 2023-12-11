plot_well_percentages <- function(data,
                                  add_vars,
                                  scale_fill_brewer) {
  
  ggplot2::ggplot(data,
                  ggplot2::aes(.data$percentage_SABGal_positive, .data$percentage_EdU_positive)
                  ) +
    ggplot2::geom_point(
      shape = 21,
      color = "black",
      ggplot2::aes(fill = .data$Condition),
      size = 3) +
    {if (length(add_vars) == 1)
      ggplot2::facet_grid(cols = ggplot2::vars(!!dplyr::sym(add_vars[1]))) } +
    {if (length(add_vars) == 2)
      ggplot2::facet_grid(cols = ggplot2::vars(!!dplyr::sym(add_vars[1])),
                 rows = ggplot2::vars(!!dplyr::sym(add_vars[2]))) } +
    ggplot2::coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
    ggplot2::scale_x_continuous(labels = scales::percent, name = "% SA-\u03B2-Gal+ cells",
                       breaks = seq(0, 1, by = 0.2)) +
    ggplot2::scale_y_continuous(labels = scales::percent, name = "% EdU+ cells",
                       breaks = seq(0, 1, by = 0.2)) +
    ggplot2::labs(title = "Well % of SA-\u03B2-Gal+ EdU+ Cells",
                  fill = "Condition") +
    scale_fill_brewer +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))
}