plot_median_staining_comparison <- function(data,
                                            input_features,
                                            add_vars,
                                            comparison_var,
                                            other_add_var) {
  if(length(input_features[-1]) == 1) {
    x_feature = input_features[-1]
    x_feature_plot = paste0(x_feature, "_median")
    
    plot_well_percentages_plots <- ggplot2::ggplot(data, ggplot2::aes(log10(x = .data[[x_feature_plot]]))) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = !!dplyr::sym(comparison_var)),
        bins = 30, color = "black", alpha = 0.7) +
      {if (length(other_add_var) == 0) facet_grid(cols = vars(.data$Condition))} +
      {if (length(other_add_var) == 1) facet_grid(cols = vars(.data$Condition),
                                                  rows = vars(!!dplyr::sym(other_add_var)))} +
      scale_x_continuous(name = paste("log10 Integrated", x_feature)) +
      labs(title = paste("Median Well Intensity of",  x_feature, "Staining; Treatment Comparison:", comparison_var),
           fill = comparison_var) +
      scale_fill_grey(start = 1, end = 0) +
      guides(fill = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                     size = 3)))
  } else if(length(input_features[-1]) == 2) {
    x_feature = input_features[-1][1]
    x_feature_plot = paste0(x_feature, "_median")
    y_feature = input_features[-1][2]
    y_feature_plot = paste0(y_feature, "_median")
    
    plot_well_percentages_plots <- ggplot2::ggplot(data = data,
           aes(x = log10(.data[[x_feature_plot]]),
               y = log10(.data[[y_feature_plot]]),
               fill = !!dplyr::sym(comparison_var))
    ) +
      geom_point(size = 3,
                 shape = 21,
                 color = "black") +
      {if (length(other_add_var) == 0) facet_grid(cols = vars(.data$Condition))} +
      {if (length(other_add_var) == 1) facet_grid(cols = vars(.data$Condition),
                                                  rows = vars(!!dplyr::sym(other_add_var)))} +
      scale_x_continuous(name = paste("log10 Integrated", x_feature)) +
      scale_y_continuous(name = paste("log10 Integrated", y_feature)) +
      labs(title = paste("Median Well Intensity of",  x_feature, "and", y_feature, "Staining; Treatment Comparison:", comparison_var),
           fill = comparison_var) +
      scale_fill_grey(start = 1, end = 0) +
      guides(fill = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                     size = 3)))
  } else{
    feat_combns <- get_combos(input_features[-1], special = NULL, permute_combn = "combn")
    
    plots <- lapply(feat_combns, function(pair) {
      # Get feature names
      x_feature <- pair[1]
      x_feature_plot = paste0(x_feature, "_median")
      y_feature <- pair[2]
      y_feature_plot = paste0(y_feature, "_median")
      
      ggplot2::ggplot(data = data,
                      aes(x = log10(.data[[x_feature_plot]]),
                          y = log10(.data[[y_feature_plot]]),
                          fill = !!dplyr::sym(comparison_var))
      ) +
        geom_point(size = 3,
                   shape = 21,
                   color = "black") +
        {if (length(other_add_var) == 0) facet_grid(cols = vars(.data$Condition))} +
        {if (length(other_add_var) == 1) facet_grid(cols = vars(.data$Condition),
                                                    rows = vars(!!dplyr::sym(other_add_var)))} +
        scale_x_continuous(name = paste("log10 Integrated", x_feature)) +
        scale_y_continuous(name = paste("log10 Integrated", y_feature)) +
        labs(title = paste("Median Well Intensity of",  x_feature, "and", y_feature, "Staining; Treatment Comparison:", comparison_var),
             fill = comparison_var) +
        scale_fill_grey(start = 1, end = 0) +
        guides(fill = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                       size = 3)))
      
    })
    
    # Combine plots into a single grid
    plot_well_percentages_plots <- patchwork::wrap_plots(plots, ncol = 2)
    
  }
  plot_well_percentages_plots
}