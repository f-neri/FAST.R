plot_median_staining <- function(data,
                                            input_features,
                                            data_thresholds,
                                            additional_variables,
                                            scale_fill_brewer) {
  if(length(input_features[-1]) == 1) {
    x_feature = input_features[-1]
    x_feature_plot = paste0(x_feature, "_median")
    x_feature_threshold = paste0(x_feature, "_threshold_average_log10")
    
    plot_well_percentages_plots <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_feature_plot]])) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$Condition),
        bins = 30, color = "black", alpha = 0.7) +
      geom_vline(data = data_thresholds, aes(xintercept = .data[[x_feature_threshold]]))  +
      {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
      {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                         rows = vars(!!dplyr::sym(additional_variables[2]))) } +
      scale_x_continuous(name = paste("log10 Integrated", x_feature)) +
      labs(title = paste("Median Well Intensity of", x_feature, "Staining plus Staining Thresholds"),
           color = "Condition") +
      scale_fill_brewer +
      guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                      size = 3)))
  } else if(length(input_features[-1]) == 2) {
    # Get feature names
    x_feature = input_features[-1][1]
    x_feature_plot = paste0(x_feature, "_median")
    x_feature_threshold = paste0(x_feature, "_threshold_average_log10")
    
    y_feature = input_features[-1][2]
    y_feature_plot =  paste0(y_feature, "_median")
    y_feature_threshold = paste0(y_feature, "_threshold_average_log10")
    
    plot_well_percentages_plots <- ggplot(data,
           aes(.data[[x_feature_plot]], .data[[y_feature_plot]])) +
      geom_point(
        aes(color = .data$Condition),
        alpha = 1/8
      ) +
      geom_vline(data = data_thresholds, aes(xintercept = .data[[x_feature_threshold]])) +
      geom_hline(data = data_thresholds, aes(yintercept = .data[[y_feature_threshold]])) +
      {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
      {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                         rows = vars(!!dplyr::sym(additional_variables[2]))) } +
      scale_x_continuous(name = paste("log10 Integrated", x_feature)) +
      scale_y_continuous(name = paste("log10 Integrated", y_feature)) +
      labs(title = paste("Median Well Intensity of", x_feature, "and", y_feature, "Staining plus Staining Thresholds"),
           color = "Condition") +
      scale_fill_brewer +
      guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                      size = 3)))
  } else {
    feat_combns <- get_combos(input_features[-1], special = NULL, permute_combn = "combn")
    
    plots <- lapply(feat_combns, function(pair) {
      # Get feature names
      x_feature <- pair[1]
      x_feature_plot =  paste0(x_feature, "_median")
      x_feature_threshold = paste0(x_feature, "_threshold_average_log10")
      
      y_feature <- pair[2]
      y_feature_plot =  paste0(y_feature, "_median")
      y_feature_threshold = paste0(y_feature, "_threshold_average_log10")
      
      ggplot(data = data,
             aes(x = log10(.data[[x_feature_plot]]),
                 y = log10(.data[[y_feature_plot]]),
                 fill = .data$Condition)
      ) +
        geom_point(size = 3,
                   shape = 21,
                   color = "black") +
        geom_vline(data = data_thresholds, aes(xintercept = .data[[x_feature_threshold]])) +
        geom_hline(data = data_thresholds, aes(yintercept = .data[[y_feature_threshold]])) +
        {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
        {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                           rows = vars(!!dplyr::sym(additional_variables[2]))) } +
        scale_x_continuous(name = paste("log10 Integrated", x_feature)) +
        scale_y_continuous(name = paste("log10 Integrated", y_feature)) +
        labs(title = paste("Median Well Intensity of", x_feature, "and", y_feature, "Staining plus Staining Thresholds"),
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