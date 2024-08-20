plot_median_fold_change_stains <- function(data,
                                           input_features,
                                           additional_variables,
                                           scale_fill_brewer,
                                           scale_color_brewer) {
  
  if(length(input_features[-1]) == 1) {
    x_feature = input_features[-1]
    x_feature_plot <- paste0(input_features, "_median_fold_change")
    
    plot_well_percentages_plots <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_feature_plot]])) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$Condition),
        bins = 30, color = "black", alpha = 0.7) +
      {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
      {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                         rows = vars(!!dplyr::sym(additional_variables[2]))) } +
      scale_x_continuous(name = paste(input_features, "Median Fold Change")) +
      labs(title = paste("Distribution of Median Fold Change of", x_feature),
           color = "Condition") +
      scale_fill_brewer +
      guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                      size = 3)))
  } else if(length(input_features[-1]) == 2) {
    # Get feature names
    x_feature = input_features[-1][1]
    x_feature_plot <- paste0(x_feature, "_median_fold_change")
    
    y_feature = input_features[-1][2]
    y_feature_plot <- paste0(y_feature, "_median_fold_change")
    
    plot_well_percentages_plots <- ggplot(data,
                                          aes(.data[[x_feature_plot]], .data[[y_feature_plot]])) +
      geom_point(
        aes(color = .data$Condition),
        alpha = 1
      ) +
      {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
      {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                         rows = vars(!!dplyr::sym(additional_variables[2]))) } +
      scale_x_continuous(name = paste0("Median Fold Change ", x_feature),
                         limits = c(stats::quantile(data[[x_feature_plot]], 0.01),
                                    stats::quantile(data[[x_feature_plot]], 0.99))
      ) +
      scale_y_continuous(name = paste0("Median Fold Change ", y_feature),
                         limits = c(stats::quantile(data[[y_feature_plot]], 0.01),
                                    stats::quantile(data[[y_feature_plot]], 0.99))
      ) +
      labs(title = paste0("Median Fold Change of ", x_feature, " and ", y_feature),
           color = "Condition")  +
      scale_color_brewer +
      guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                      size = 3))) # to make enlarge points in legend
  } else{
    feat_combns <- get_combos(input_features[-1], special = NULL, permute_combn = "combn")
    
    plots <- lapply(feat_combns, function(pair) {
      # Get feature names
      x_feature <- pair[1]
      x_feature_plot <- paste0(x_feature, "_median_fold_change")
      
      y_feature <- pair[2]
      y_feature_plot <- paste0(y_feature, "_median_fold_change")
      
      ggplot(data,
             aes(.data[[x_feature_plot]], .data[[y_feature_plot]])) +
        geom_point(
          aes(color = .data$Condition),
          alpha = 1
        ) +
        {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
        {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                           rows = vars(!!dplyr::sym(additional_variables[2]))) } +
        scale_x_continuous(name = paste0("Median Fold Change ", x_feature),
                           limits = c(stats::quantile(data[[x_feature_plot]], 0.01),
                                      stats::quantile(data[[x_feature_plot]], 0.99))
        ) +
        scale_y_continuous(name = paste0("Median Fold Change ", y_feature),
                           limits = c(stats::quantile(data[[y_feature_plot]], 0.01),
                                      stats::quantile(data[[y_feature_plot]], 0.99))
        ) +
        labs(title = paste0("Median Fold Change of ", x_feature, " and ", y_feature),
             color = "Condition")  +
        scale_color_brewer +
        guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                        size = 3))) # to make enlarge points in legend
      
    })
    
    # Combine plots into a single grid
    plot_well_percentages_plots <- patchwork::wrap_plots(plots, ncol = 2)
    
  }
}