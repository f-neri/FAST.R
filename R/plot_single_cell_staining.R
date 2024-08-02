plot_single_cell_staining <- function(data,
                                      input_features,
                                      data_thresholds,
                                      additional_variables,
                                      scale_color_brewer) {
  
  if(length(input_features[-1]) == 1) {
    x_feature = input_features[-1]
    x_feature_plot = paste0(x_feature, "_log10")
    x_feature_threshold = paste0(x_feature, "_threshold_average_log10")
    
    plot_well_percentages_plots <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_feature_plot]])) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$Condition),
        bins = 30, color = "black", alpha = 0.7) +
    {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
    {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                       rows = vars(!!dplyr::sym(additional_variables[2]))) } +
    geom_vline(data = data_thresholds, aes(xintercept = .data[[x_feature_threshold]])) +
      scale_x_continuous(name = paste0("log10 Integrated ", x_feature),
                         limits = c(stats::quantile(data$SABGal_log10, 0.01),
                                    stats::quantile(data$SABGal_log10, 0.99))
      ) +
    labs(title = paste0("Single Cell Intensity of ", x_feature, " Staining plus Staining Thresholds"),
         color = "Condition") +
    scale_color_brewer +
    guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                    size = 3))) # to make enlarge points in legend
  } else if(length(input_features[-1]) == 2) {
    # Get feature names
    x_feature = input_features[-1][1]
    x_feature_plot =  paste0(x_feature, "_log10")
    x_feature_threshold = paste0(x_feature, "_threshold_average_log10")
    
    y_feature = input_features[-1][2]
    y_feature_plot =  paste0(y_feature, "_log10")
    y_feature_threshold = paste0(y_feature, "_threshold_average_log10")
    
    plot_well_percentages_plots <- ggplot(data,
           aes(.data[[x_feature_plot]], .data[[y_feature_plot]])) +
      geom_point(
        aes(color = .data$Condition),
        alpha = 1/8
      ) +
      {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
      {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                         rows = vars(!!dplyr::sym(additional_variables[2]))) } +
      geom_vline(data = data_thresholds, aes(xintercept = .data[[x_feature_threshold]])) +
      geom_hline(data = data_thresholds, aes(yintercept = .data[[y_feature_threshold]])) +
      scale_x_continuous(name = paste0("log10 Integrated ", x_feature), #expression(log[10]("Integrated SA-\u03B2-Gal OD")),
                         limits = c(stats::quantile(data[[x_feature_plot]], 0.01),
                                    stats::quantile(data[[x_feature_plot]], 0.99))
      ) +
      scale_y_continuous(name = paste0("log10 Integrated ", y_feature), #expression(log[10]("Integrated EdU intensity (AU)")),
                         limits = c(stats::quantile(data[[y_feature_plot]], 0.01),
                                    stats::quantile(data[[y_feature_plot]], 0.99))
      ) +
      labs(title = paste0("Single Cell Intensity of ", x_feature, " and ", y_feature, " Staining plus Staining Thresholds"),
           color = "Condition") +
      scale_color_brewer +
      guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                      size = 3))) # to make enlarge points in legend
  } else { # 3+ stains
    feat_combns <- get_combos(input_features[-1], special = NULL, permute_combn = "combn")
    
    plots <- lapply(feat_combns, function(pair) {
      # Get feature names
      x_feature <- pair[1]
      x_feature_plot =  paste0(x_feature, "_log10")
      x_feature_threshold = paste0(x_feature, "_threshold_average_log10")
      
      y_feature <- pair[2]
      y_feature_plot =  paste0(y_feature, "_log10")
      y_feature_threshold = paste0(y_feature, "_threshold_average_log10")
      
      ggplot(data,
             aes(.data[[x_feature_plot]], .data[[y_feature_plot]])) +
        geom_point(
          aes(color = .data$Condition),
          alpha = 1/8
        ) +
        {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
        {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                           rows = vars(!!dplyr::sym(additional_variables[2]))) } +
        geom_vline(data = data_thresholds, aes(xintercept = .data[[x_feature_threshold]])) +
        geom_hline(data = data_thresholds, aes(yintercept = .data[[y_feature_threshold]])) +
        scale_x_continuous(name = paste0("log10 Integrated ", x_feature), #expression(log[10]("Integrated SA-\u03B2-Gal OD")),
                           limits = c(stats::quantile(data[[x_feature_plot]], 0.01),
                                      stats::quantile(data[[x_feature_plot]], 0.99))
        ) +
        scale_y_continuous(name = paste0("log10 Integrated ", y_feature), #expression(log[10]("Integrated EdU intensity (AU)")),
                           limits = c(stats::quantile(data[[y_feature_plot]], 0.01),
                                      stats::quantile(data[[y_feature_plot]], 0.99))
        ) +
        labs(title = paste0("Single Cell Intensity of ", x_feature, " and ", y_feature, " Staining plus Staining Thresholds"),
             color = "Condition") +
        scale_color_brewer +
        guides(color = guide_legend(override.aes = list(alpha = 1, # to ensure colors in legend are visible
                                                        size = 3))) # to make enlarge points in legend
      
    })
    
    # Combine plots into a single grid
    plot_well_percentages_plots <- patchwork::wrap_plots(plots, ncol = 2)
    
  }
  plot_well_percentages_plots
}