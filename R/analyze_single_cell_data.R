################################################################################
# 07/11 Modified analyze_single_cell_data.R Script
#
# Dependent scripts: calculate_summary_stats.R, generate_permuatations.R,
# counts_percentages.R, rearrange_df_columns.R
#
# Generate Analysis Report for Data Analysis Server
################################################################################

. <- NULL # prevents R CMD note

analyze_single_cell_data <- function(df_single_cell_data, background_threshold,
                                     input_morphological_feature_list,
                                     input_feature_list) {
  
  # Additional variables check and handler ----------------------------------
  feature_list <- c(input_morphological_feature_list, input_feature_list)
  
  # Define the expected variables including 'plate'
  expected_variables <- c("plate", "well", "cell_ID", "Condition", feature_list)
  
  # Identify additional variables
  additional_vars <- setdiff(names(df_single_cell_data), expected_variables)
  
  additional_vars_noMLTraining <- setdiff(names(df_single_cell_data), expected_variables) %>%
    # remove ML cols if present
    .[stringr::str_detect(., "ML_Training|ML_Prediction", negate = TRUE)]
  
  # Initialize variables to store original names
  additional_variable_1 <- NULL
  additional_variable_2 <- NULL
  
  # Initialize variable to store additional_variable_1 and additional_variable_2 for future data transformation
  additional_variables <- NULL
  
  # Check if additional variables are present
  if (length(additional_vars_noMLTraining) > 0) {
    # Sort additional variables based on the number of unique values
    sorted_vars <- additional_vars_noMLTraining[order(-sapply(df_single_cell_data[additional_vars_noMLTraining], function(x) length(unique(x))))]
    
    # Rename additional variables and store original names
    for (i in seq_along(sorted_vars)) {
      original_name <- sorted_vars[i]
      new_name <- paste0("additional_variable_", i)
      additional_variables[i] <- new_name
      if (i == 1) {
        additional_variable_1 <- original_name
      } else if (i == 2) {
        additional_variable_2 <- original_name
      }
      names(df_single_cell_data)[names(df_single_cell_data) == original_name] <- new_name
    }
  }
  
  # Generate signal thresholds & add to single cell data ----------------------------------------------
  
  # Create background df
  df_single_cell_data_background <- df_single_cell_data[grepl(pattern = "_background$", df_single_cell_data$Condition), ]
  
  # calculate thresholds
  grouping <- c("Condition", additional_variables)
  
  thresholds_df <- df_single_cell_data_background %>%
    dplyr::group_by(!!!dplyr::syms(grouping)) %>%
    dplyr::summarise(across(all_of(feature_list),
                            ~ stats::quantile(.x, background_threshold, na.rm = TRUE),
                            .names = "{.col}_threshold"))
  
  # mirror thresholds values for non background Conditions
  thresholds_df_mirror <- thresholds_df
  
  thresholds_df_mirror$Condition <- gsub(pattern = "_background$", replacement = "", thresholds_df_mirror$Condition)
  
  thresholds_df <- dplyr::bind_rows(thresholds_df, thresholds_df_mirror)
  
  df_single_cell_data <- dplyr::left_join(df_single_cell_data, thresholds_df)
  
  # Generate summary table --------------------------------------------------
  
  # Grouping arguments for summarise()
  single_cell_grouping <- c("well", "Condition", additional_variables)
  
  # Get cell_counts
  cell_counts_df <- df_single_cell_data %>%
    dplyr::group_by(!!!dplyr::syms(single_cell_grouping)) %>%
    dplyr::summarise(cell_counts = dplyr::n(), .groups = 'drop')

  # Fill summary data frame
  index <- 1
  summary_list <- purrr::map(feature_list, function(feature_name) {
    # First pass will keep the grouping features
    keep_all <- index == 1
    index <<- index + 1 # increment up
    
    df_single_cell_data %>%
      dplyr::group_by(!!!dplyr::syms(single_cell_grouping)) %>%
      calculate_summary_stats(single_cell_grouping, feature_name, keep_all_cols = keep_all)
  })
  
  # Summary Data Frame
  summary_df_list <- as.data.frame(summary_list)
  
  # Take off any cols with colname with cell_counts
  summary_df_list <- summary_df_list %>% 
    dplyr::select(-contains("cell_counts"))
  
  summary_df_list <- summary_df_list %>%
    dplyr::left_join(cell_counts_df, by = single_cell_grouping)
  
  ### Add Counts and percentages columns for pairwise features
  # Get feature pairs
  feature_pairs <- combn(input_feature_list, 2, simplify = FALSE)
  pos_neg_pairs <- unlist(lapply(feature_pairs, generate_permutations))
  
  summary_df_counts_percentages <- df_single_cell_data %>%
    dplyr::group_by(!!!dplyr::syms(single_cell_grouping)) %>%
    counts_percentages(pos_neg_pairs)
  
  summary_df_counts_percentages <- summary_df_counts_percentages %>% 
    percentage_calculation(pos_neg_pairs, summary_df_list$cell_counts)
  
  # Full Summary Data Frame
  summary_df <- dplyr::left_join(summary_df_list, summary_df_counts_percentages, by = single_cell_grouping)
  
  # add ML_Training to summary
  if(length(additional_vars) > length(additional_vars_noMLTraining)) {
    df_ML_Training <- df_single_cell_data %>%
      dplyr::select(.data$well, .data$ML_Training, .data$ML_Prediction) %>%
      dplyr::group_by(.data$well) %>%
      dplyr::summarize(
        ML_Training = unique(.data$ML_Training),
        cell_counts = dplyr::n(),
        ML_Prediction_percentage_positive = sum(.data$ML_Prediction == "+")/.data$cell_counts,
        ML_Prediction_percentage_negative = sum(.data$ML_Prediction == "-")/.data$cell_counts
      ) %>%
      dplyr::ungroup()
    
    summary_df <- summary_df %>% dplyr::left_join(df_ML_Training) %>%
      dplyr::select(.data$well, .data$Condition,
                    .data$ML_Training, .data$ML_Prediction_percentage_positive, .data$ML_Prediction_percentage_negative,
                    dplyr::everything()
      )
  }
  
  # Add fold change for median values ---------------------------------------
  
  # Generate mean of median values
  median_cols <- paste0(feature_list, "_median")
  reference_signal <- summary_df[!grepl("_background$", summary_df$Condition), ] %>%
    dplyr::group_by(!!!dplyr::syms(c("Condition", additional_variables))) %>%
    dplyr::summarise(
      across(
        .cols = all_of(median_cols),
        .fns = list(median_reference = ~ mean(.x, na.rm = TRUE)),
        .names = "{.col}_reference"
      ),
      .groups = "drop"
    )
  
  # Identify lowest non-background mean value to use as reference
  
  ## Calculate min value
  median_ref_cols <- paste0(median_cols, "_reference")
  
  if (length(additional_variables) > 0) {
    reference_signal_min <- reference_signal %>%
      dplyr::group_by(!!!dplyr::syms(additional_variables)) %>%
      dplyr::summarise(
        across(
          .cols = all_of(median_ref_cols),
          .fns = list(min_reference = ~min(.x, na.rm = TRUE)),
          .names = "{.col}"
        ),
        .groups = "drop"
      )
  } else {
    reference_signal_min <- reference_signal %>%
      dplyr::mutate(
        across(
          .cols = all_of(median_ref_cols),
          .fns = list(min_reference = ~min(.x, na.rm = TRUE)),
          .names = "{.col}"
        )
      )}
  
  ## Set min value as reference for all Conditions
  if (length(additional_variables) > 0) {
    reference_signal_min <- reference_signal %>%
      dplyr::select(!dplyr::all_of(median_ref_cols)) %>%
      dplyr::left_join(reference_signal_min)
  }
  
  ## Set min value as reference for all background Conditions
  reference_signal_min_background <- reference_signal_min %>%
    dplyr::mutate(Condition = paste0(.data$Condition, "_background"))
  
  reference_signal_min <- dplyr::bind_rows(reference_signal_min, reference_signal_min_background)
  
  # Calculate fold change compared to reference
  median_fold_change_cols <- paste0(median_cols, "_fold_change")
  summary_df <- summary_df %>%
    dplyr::left_join(reference_signal_min)
  
  fold_change_cols <- list()
  for (col in median_cols) {
    # Calculate fold change
    fold_change <- summary_df[[col]] / summary_df[[paste0(col, "_reference")]]
    fold_change_cols[[paste0(col, "_fold_change")]] <- fold_change
  }
  # Convert to df
  fold_change_df <- as.data.frame(fold_change_cols)
  
  # Add fold change df to summary df
  summary_df <- cbind(summary_df, fold_change_df)
  summary_df <- summary_df %>%
    dplyr::select(-dplyr::all_of(median_ref_cols))
  
  # Re-add plate column -----------------------------------------------------
  
  # Create small df with plate column
  plate_df <- df_single_cell_data %>%
    dplyr::select(.data$plate, .data$well, .data$Condition, dplyr::all_of(additional_variables)) %>%
    unique()
  
  # Join plate_df with summary df
  summary_df <- dplyr::left_join(summary_df, plate_df) %>%
    dplyr::select(.data$plate, dplyr::everything()) # rearrange plate to be 1st column
  
  # Reduce decimal digits to 2 for all <dbl> columns ----------------------------------------------
  
  # Identify percentage columns
  is_percentage_column <- grepl("percentage|%", names(summary_df))
  
  # Multiply proportion values by 100
  summary_df[ , is_percentage_column] <- summary_df[ , is_percentage_column] * 100
  
  # Identify percentage a double columns
  is_double_column <- sapply(summary_df, is.double)
  
  # Format values to 2 decimal digits
  summary_df[, is_double_column] <- lapply(summary_df[, is_double_column], function(x) format(x, digits = 2, nsmall = 2))
  
  # Adjust column order -----------------------------------------------------
  
  anchor_point <- paste0(feature_list[length(feature_list)], "_max")
  
  summary_df <- summary_df %>%
    dplyr::select(.data$plate, .data$well, .data$Condition, dplyr::all_of(additional_variables), dplyr::everything()) %>%
    rearrange_df_columns(.,
                         cols_to_move = median_fold_change_cols,
                         col_anchor =  anchor_point)
  summary_df <- summary_df %>% 
    dplyr::select("plate", single_cell_grouping, "cell_counts", everything())
  
  # Rename additional_variables with original names -------------------------
  
  if (length(additional_variables) > 0) {
    for (i in seq_along(additional_variables)) {
      if (i == 1) {
        names(summary_df)[grepl(pattern = "additional_variable_1", names(summary_df))] <- additional_variable_1
      } else if (i == 2) {
        names(summary_df)[grepl(pattern = "additional_variable_2", names(summary_df))] <- additional_variable_2
      }
    }
  }
  
  # Return df ---------------------------------------------------------------
  summary_df
}