plot_3d_median_fold_change_stains <- function(data,
                                           input_features,
                                           additional_variables,
                                           scale_fill_brewer,
                                           scale_color_brewer) {
  
  if(length(input_features[-1]) == 3) {
    x_feature = input_features[-1][1]
    x_feature_plot <- paste0(x_feature, "_median_fold_change")
    
    y_feature = input_features[-1][2]
    y_feature_plot <- paste0(y_feature, "_median_fold_change")
    
    z_feature = input_features[-1][3]
    z_feature_plot <- paste0(z_feature, "_median_fold_change")
    
    # Generate the plotly 3D scatter plot
    plot_well_percentages_plots <- rgl::plot3d(
      x = data[[x_feature_plot]],
      y = data[[y_feature_plot]],
      z = data[[z_feature_plot]],
      # col = .data$Condition,  # Use colors from Condition
      xlab = paste0("Median Fold Change ", x_feature),
      ylab = paste0("Median Fold Change ", y_feature),
      zlab = paste0("Median Fold Change ", z_feature),
      main = paste0("3D Median Fold Change of ", x_feature, ", ", y_feature, " and ", z_feature),
      type = 's',
      size = 5
    )
    rgl::rglwidget()
  }
  plot_well_percentages_plots
}

