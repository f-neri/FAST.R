plot_well_percentages <- function(data,
                                  input_features,
                                  add_vars,
                                  scale_fill_brewer) {
  
  # One stain to plot
  if(length(input_features[-1]) == 1) { # plot will only have one feature/stain
    # Get feature name
    x_feature = input_features[-1]
    x_feature_plot = paste0("percentage_", x_feature, "_positive")
    
    plot_well_percentages_plots <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_feature_plot]])) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$Condition),
        bins = 30, color = "black", alpha = 0.7) +
      {if (length(add_vars) == 1) {
        ggplot2::facet_grid(cols = ggplot2::vars(!!dplyr::sym(add_vars[1])))
      } else if (length(add_vars) == 2) {
        ggplot2::facet_grid(cols = ggplot2::vars(!!dplyr::sym(add_vars[1])),
                            rows = ggplot2::vars(!!dplyr::sym(add_vars[2])))
      }} +
      ggplot2::scale_x_continuous(labels = scales::percent, name = paste0("% ", x_feature, "+ Cells"),
                                  breaks = seq(0, 1, by = 0.2)) +
      ggplot2::labs(title = paste0("Distribution of % ", x_feature, "+ Cells"),
                    fill = "Condition") +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::theme_minimal()
    
  } else if (length(input_features[-1]) == 2) { # 2 stains to plot
    # Get feature names
    x_feature = input_features[-1][1]
    x_feature_plot = paste0("percentage_", x_feature, "_positive")
    y_feature = input_features[-1][2]
    y_feature_plot = paste0("percentage_", y_feature, "_positive")
    
    plot_well_percentages_plots <- ggplot2::ggplot(data,
                    ggplot2::aes(.data[[x_feature_plot]], .data[[y_feature_plot]])
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
      ggplot2::scale_x_continuous(labels = scales::percent, name = paste0("% ", x_feature, "+ Cells"),
                                  breaks = seq(0, 1, by = 0.2)) +
      ggplot2::scale_y_continuous(labels = scales::percent, name = paste0("% ", y_feature, "+ Cells"),
                                  breaks = seq(0, 1, by = 0.2)) +
      ggplot2::labs(title = paste0("Well % of", x_feature, "+ ", y_feature, "+ Cells"),
                    fill = "Condition") +
      scale_fill_brewer +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))
    
  } else { # 3+ stains
    feat_combns <- get_combos(input_features[-1], special = NULL, permute_combn = "combn")
    
    plots <- lapply(feat_combns, function(pair) {
      x_feature <- pair[1]
      x_feature_plot <- paste0("percentage_", x_feature, "_positive")
      y_feature <- pair[2]
      y_feature_plot <- paste0("percentage_", y_feature, "_positive")
      
      ggplot2::ggplot(data,
                      ggplot2::aes(x = .data[[x_feature_plot]], y = .data[[y_feature_plot]])) +
        ggplot2::geom_point(
          shape = 21,
          color = "black",
          ggplot2::aes(fill = .data$Condition),
          size = 3) +
        {if (length(add_vars) == 1) {
          ggplot2::facet_grid(cols = ggplot2::vars(!!dplyr::sym(add_vars[1])))
        } else if (length(add_vars) == 2) {
          ggplot2::facet_grid(cols = ggplot2::vars(!!dplyr::sym(add_vars[1])),
                              rows = ggplot2::vars(!!dplyr::sym(add_vars[2])))
        }} +
        ggplot2::coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
        ggplot2::scale_x_continuous(labels = scales::percent, name = paste0("% ", x_feature, "+ Cells"),
                                    breaks = seq(0, 1, by = 0.2)) +
        ggplot2::scale_y_continuous(labels = scales::percent, name = paste0("% ", y_feature, "+ Cells"),
                                    breaks = seq(0, 1, by = 0.2)) +
        ggplot2::labs(title = paste0("Well % of ", x_feature, "+ ", y_feature, "+ Cells"),
                      fill = "Condition") +
        scale_fill_brewer +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))
    })
    
    # Combine plots into a single grid
    plot_well_percentages_plots <- patchwork::wrap_plots(plots, ncol = 2)
    
  }
  
  plot_well_percentages_plots
}