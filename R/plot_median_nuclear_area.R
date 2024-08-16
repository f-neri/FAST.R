plot_median_nuclear_area <- function(data,
                                     morphological_feature,
                                     additional_variables,
                                     scale_fill_brewer) {
  if(length(morphological_feature) == 1) {
    x_feature_plot <- paste0(morphological_feature, "_median")
    
    plot_well_percentages_plots <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_feature_plot]])) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$Condition),
        bins = 30, color = "black", alpha = 0.7) +
      {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
      {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                         rows = vars(!!dplyr::sym(additional_variables[2]))) } +
      scale_x_continuous(name = paste(morphological_feature, "Median")) +
      labs(title = paste("Distribution of Median of", morphological_feature),
           color = "Condition") +
      scale_fill_brewer +
      guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                      size = 3)))
  } else{
    plots <- lapply(morphological_feature, function(feature) {
      
      feature_plot <- paste0(feature, "_median")
      
      plot_well_percentages_plots <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[feature_plot]])) +
        ggplot2::geom_histogram(
          ggplot2::aes(fill = .data$Condition),
          bins = 30, color = "black", alpha = 0.7) +
        {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
        {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                           rows = vars(!!dplyr::sym(additional_variables[2]))) } +
        scale_x_continuous(name = paste(feature, "Median")) +
        labs(title = paste("Distribution of Median of", feature),
             color = "Condition") +
        scale_fill_brewer +
        guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                        size = 3)))
      
    })
    
    # Combine plots into a single grid
    plot_well_percentages_plots <- patchwork::wrap_plots(plots, ncol = 2)
    
  }
  
  plot_well_percentages_plots
}