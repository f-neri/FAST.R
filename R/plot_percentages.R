plot_percentages <- function(data,
                             input_features,
                             additional_variables,
                             size_axis_text) {
  
  stain_inputs <- input_features[-1]
  
  # Find the position of the "cell_counts" column
  cell_counts_pos <- which(names(data) == "cell_counts")
  
  # Select columns that come before "cell_counts"
  columns_before_cell_counts <- names(data)[1:(cell_counts_pos - 1)]
  
  # Select columns that contain the word "percentage"
  columns_with_percentage <- grep("percentage", names(data), value = TRUE)
  
  # Combine the selections
  df_percentages <- data %>%
    dplyr::select(dplyr::all_of(c(columns_before_cell_counts, columns_with_percentage)))
  
  # Plot for only 1 stain
  if(length(stain_inputs) == 1) {
    x_feature = stain_inputs[1] # just the percentage pos and neg
    x_feature_perc_pos = paste0("percentage_", x_feature, "_positive")
    x_feature_perc_neg = paste0("percentage_", x_feature, "_negative")
    cols_to_pivot <-  c(x_feature_perc_pos, x_feature_perc_neg)
    
    df_percentages <- df_percentages %>%
      tidyr::pivot_longer(dplyr::all_of(cols_to_pivot), names_to = "Positivity", values_to = "Percentage")
    
    # adjust Positivity string values
    df_percentages$Positivity <- df_percentages$Positivity %>%
      stringr::str_replace_all(c("percentage_" = "", "_positive" = "+", "_negative" = "-", "_" = " "))
    
    # create grouped df
    df_percentages_summary <- df_percentages %>%
      dplyr::group_by(.data$Condition, .data$Positivity, !!!dplyr::syms(additional_variables)) %>%
      dplyr::summarise(Percentage = mean(.data$Percentage)) %>%
      dplyr::ungroup()
    
    # generate stacked plot
    plot_well_percentages_plots <- ggplot2::ggplot(df_percentages_summary, aes(x = .data$Percentage, y = .data$Condition, fill = .data$Positivity)) +
      geom_col(position = "fill") +
      labs(title = paste("Overall % of", x_feature, "+/- Cells"),
           y = "Condition") +
      geom_text(aes(label = ifelse(.data$Percentage <= 0.10, "", scales::percent(.data$Percentage, accuracy = 1.0))),
                position = position_fill(vjust = 0.5),
                size = size_axis_text/3) +
      scale_x_continuous(labels = scales::percent, name = NULL, breaks = NULL) +
      {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
      {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                         rows = vars(!!dplyr::sym(additional_variables[2])))} +
      scale_fill_manual(values = c("green", "gray88")) +
      guides(fill = guide_legend(title = NULL)) +
      theme(legend.key.height = unit(0.5, 'cm'), #change legend key height
            legend.key.width = unit(0.5, 'cm')) #change legend key width
    
  } else {
    # pivot longer 
    cols_to_pivot <- paste0("percentage_", get_combos(stain_inputs, permute_combn = "perc"))
    
    # 2 Stains
    if(length(stain_inputs) == 2) {
      x_feature = stain_inputs[1]
      y_feature = stain_inputs[2]
      
      df_percentages <- df_percentages %>%
        tidyr::pivot_longer(dplyr::all_of(cols_to_pivot), names_to = "Positivity", values_to = "Percentage")
      
      # adjust Positivity string values
      df_percentages$Positivity <- df_percentages$Positivity %>%
        stringr::str_replace_all(c("percentage_" = "", "_positive" = "+", "_negative" = "-", "_" = " "))
      
      # create grouped df
      df_percentages_summary <- df_percentages %>%
        dplyr::group_by(.data$Condition, .data$Positivity, !!!dplyr::syms(additional_variables)) %>%
        dplyr::summarise(Percentage = mean(.data$Percentage)) %>%
        dplyr::ungroup()
      
      # generate stacked plot
      plot_well_percentages_plots <- ggplot2::ggplot(df_percentages_summary, aes(x = .data$Percentage, y = .data$Condition, fill = .data$Positivity)) +
        geom_col(position = "fill") +
        labs(title = paste("Overall % of", x_feature, "+/-", y_feature, "+/- Cells"),
             y = "Condition") +
        geom_text(aes(label = ifelse(.data$Percentage <= 0.10, "", scales::percent(.data$Percentage, accuracy = 1.0))),
                  position = position_fill(vjust = 0.5),
                  size = size_axis_text/3) +
        scale_x_continuous(labels = scales::percent, name = NULL, breaks = NULL) +
        {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
        {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                           rows = vars(!!dplyr::sym(additional_variables[2])))} +
        scale_fill_manual(values = c("gray88", "gray50", "green", "darkgreen")) +
        guides(fill = guide_legend(title = NULL)) +
        theme(legend.key.height = unit(0.5, 'cm'), #change legend key height
              legend.key.width = unit(0.5, 'cm')) #change legend key width
    } else {
      # 3+ Stains
      feat_combns <- get_combos(input_features[-1], special = NULL, permute_combn = "combn")
      
      plots <- lapply(feat_combns, function(pair) {
        # Get feature names
        x_feature = stain_inputs[1]
        y_feature = stain_inputs[2]
        
        df_percentages <- df_percentages %>%
          tidyr::pivot_longer(dplyr::all_of(cols_to_pivot), names_to = "Positivity", values_to = "Percentage")
        
        # adjust Positivity string values
        df_percentages$Positivity <- df_percentages$Positivity %>%
          stringr::str_replace_all(c("percentage_" = "", "_positive" = "+", "_negative" = "-", "_" = " "))
        
        # create grouped df
        df_percentages_summary <- df_percentages %>%
          dplyr::group_by(.data$Condition, .data$Positivity, !!!dplyr::syms(additional_variables)) %>%
          dplyr::summarise(Percentage = mean(.data$Percentage)) %>%
          dplyr::ungroup()
        
        # generate stacked plot
        ggplot(df_percentages_summary, aes(x = .data$Percentage, y = .data$Condition, fill = .data$Positivity)) +
          geom_col(position = "fill") +
          labs(title = paste("Overall % of", x_feature, "+/-", y_feature, "+/- Cells"),
               y = "Condition") +
          geom_text(aes(label = ifelse(.data$Percentage <= 0.10, "", scales::percent(.data$Percentage, accuracy = 1.0))),
                    position = position_fill(vjust = 0.5),
                    size = size_axis_text/3) +
          scale_x_continuous(labels = scales::percent, name = NULL, breaks = NULL) +
          {if (length(additional_variables) == 1) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1]))) } +
          {if (length(additional_variables) == 2) facet_grid(cols = vars(!!dplyr::sym(additional_variables[1])),
                                                             rows = vars(!!dplyr::sym(additional_variables[2])))} +
          scale_fill_manual(values = c("gray88", "gray50", "green", "darkgreen")) +
          guides(fill = guide_legend(title = NULL)) +
          theme(legend.key.height = unit(0.5, 'cm'), #change legend key height
                legend.key.width = unit(0.5, 'cm')) #change legend key width
      })
      # Combine plots into a single grid
      plot_well_percentages_plots <- patchwork::wrap_plots(plots, ncol = 2)
    }
  }
  plot_well_percentages_plots
}