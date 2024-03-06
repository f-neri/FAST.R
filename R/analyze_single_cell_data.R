analyze_single_cell_data <- function(df_single_cell_data, background_threshold) {
  
  # Additional variables check and handler ----------------------------------
  
  # Define the expected variables including 'plate'
  expected_variables <- c("plate", "well", "cell_ID", "Condition", "Nuclear_Area", "DAPI", "EdU", "SABGal")
  
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
    dplyr::summarise(
      EdU_threshold = stats::quantile(.data$EdU, background_threshold, na.rm = TRUE),
      SABGal_threshold = stats::quantile(.data$SABGal, background_threshold, na.rm = TRUE)
    )
  
  # mirror thresholds values for non background Conditions
  thresholds_df_mirror <- thresholds_df
  
  thresholds_df_mirror$Condition <- gsub(pattern = "_background$", replacement = "", thresholds_df_mirror$Condition)
  
  thresholds_df <- dplyr::bind_rows(thresholds_df, thresholds_df_mirror)
  
  df_single_cell_data <- dplyr::left_join(df_single_cell_data, thresholds_df)
  
  # }
  
  # Generate summary table --------------------------------------------------
  
  # grouping arguments for summarise()
  single_cell_grouping <- c("well", "Condition", additional_variables)
  
  # Generate summary_df
  summary_df <- df_single_cell_data %>%
    dplyr::group_by(!!!dplyr::syms(single_cell_grouping)) %>%
    dplyr::summarise(
      cell_counts = dplyr::n(),
      Nuclear_Area_min = min(.data$Nuclear_Area, na.rm = TRUE),
      Nuclear_Area_25th = stats::quantile(.data$Nuclear_Area, 0.25, na.rm = TRUE),
      Nuclear_Area_median = stats::median(.data$Nuclear_Area, na.rm = TRUE),
      Nuclear_Area_75th = stats::quantile(.data$Nuclear_Area, 0.75, na.rm = TRUE),
      Nuclear_Area_max = max(.data$Nuclear_Area, na.rm = TRUE),
      EdU_min = min(.data$EdU, na.rm = TRUE),
      EdU_25th = stats::quantile(.data$EdU, 0.25, na.rm = TRUE),
      EdU_median = stats::median(.data$EdU, na.rm = TRUE),
      EdU_75th = stats::quantile(.data$EdU, 0.75, na.rm = TRUE),
      EdU_max = max(.data$EdU, na.rm = TRUE),
      SABGal_min = min(.data$SABGal, na.rm = TRUE),
      SABGal_25th = stats::quantile(.data$SABGal, 0.25, na.rm = TRUE),
      SABGal_median = stats::median(.data$SABGal, na.rm = TRUE),
      SABGal_75th = stats::quantile(.data$SABGal, 0.75, na.rm = TRUE),
      SABGal_max = max(.data$SABGal, na.rm = TRUE),
      EdU_threshold = mean(.data$EdU_threshold),
      SABGal_threshold = mean(.data$SABGal_threshold, na.rm = TRUE),
      counts_EdU_positive = sum(.data$EdU > .data$EdU_threshold, na.rm = TRUE),
      counts_SABGal_positive = sum(.data$SABGal > .data$SABGal_threshold, na.rm = TRUE),
      counts_EdU_negative_SABGal_negative = sum(.data$EdU <= .data$EdU_threshold & .data$SABGal <= .data$SABGal_threshold, na.rm = TRUE),
      counts_EdU_negative_SABGal_positive = sum(.data$EdU <= .data$EdU_threshold & .data$SABGal > .data$SABGal_threshold, na.rm = TRUE),
      counts_EdU_positive_SABGal_negative = sum(.data$EdU > .data$EdU_threshold & .data$SABGal <= .data$SABGal_threshold, na.rm = TRUE),
      counts_EdU_positive_SABGal_positive = sum(.data$EdU > .data$EdU_threshold & .data$SABGal > .data$SABGal_threshold, na.rm = TRUE),
      percentage_EdU_positive = .data$counts_EdU_positive / .data$cell_counts,
      percentage_SABGal_positive = .data$counts_SABGal_positive / .data$cell_counts,
      percentage_EdU_negative_SABGal_negative = .data$counts_EdU_negative_SABGal_negative / .data$cell_counts,
      percentage_EdU_negative_SABGal_positive = .data$counts_EdU_negative_SABGal_positive / .data$cell_counts,
      percentage_EdU_positive_SABGal_negative = .data$counts_EdU_positive_SABGal_negative / .data$cell_counts,
      percentage_EdU_positive_SABGal_positive = .data$counts_EdU_positive_SABGal_positive / .data$cell_counts,
      .groups = 'drop'
    )
  
  # add ML_Training to summary
  if(length(additional_vars) > length(additional_vars_noMLTraining)) {
    df_ML_Training <- df_single_cell_data %>%
      dplyr::select(well, ML_Training, ML_Prediction) %>%
      dplyr::group_by(well) %>%
      dplyr::summarize(
        ML_Training = unique(ML_Training),
        cell_count = dplyr::n(),
        `ML_Prediction_% +` = sum(ML_Prediction == "+")/cell_count,
        `ML_Prediction_% -` = sum(ML_Prediction == "-")/cell_count
      ) %>%
      dplyr::ungroup()
    
    summary_df <- summary_df %>% dplyr::left_join(df_ML_Training) %>%
      dplyr::select(well, Condition,
                    ML_Training, `ML_Prediction_% +`, `ML_Prediction_% -`,
                    dplyr::everything()
                    )
  }
  
  # Add fold change for median values ---------------------------------------
  
  # generate mean median values
  reference_signal <- summary_df[!grepl("_background$", summary_df$Condition), ] %>% # remove background wells
    dplyr::group_by(!!!dplyr::syms(c("Condition", additional_variables))) %>%
    dplyr::summarise(
      Nuclear_Area_median_reference = mean(.data$Nuclear_Area_median),
      EdU_median_reference = mean(.data$EdU_median),
      SABGal_median_reference = mean(.data$SABGal_median),
      .groups = "drop"
    )
  
  # identify lowest non-background mean value to use as reference
  
  ## calculate min value
  if (length(additional_variables) > 0) {
    reference_signal_min <- reference_signal %>%
      dplyr::group_by(!!!dplyr::syms(additional_variables)) %>%
      dplyr::summarise(
        Nuclear_Area_median_reference = min(.data$Nuclear_Area_median_reference),
        EdU_median_reference = min(.data$EdU_median_reference),
        SABGal_median_reference = min(.data$SABGal_median_reference),
        .groups = "drop"
      )
  } else {
    reference_signal_min <- reference_signal %>%
      dplyr::mutate(
        Nuclear_Area_median_reference = min(.data$Nuclear_Area_median_reference),
        EdU_median_reference = min(.data$EdU_median_reference),
        SABGal_median_reference = min(.data$SABGal_median_reference)
      )
  }
  
  ## set min value as reference for all Conditions
  if (length(additional_variables) > 0) {
    reference_signal_min <- reference_signal %>%
      dplyr::select(!dplyr::all_of(c("Nuclear_Area_median_reference", "EdU_median_reference", "SABGal_median_reference"))) %>%
      dplyr::left_join(reference_signal_min)
  }
  
  ## set min value as reference for all background Conditions
  reference_signal_min_background <- reference_signal_min %>%
    dplyr::mutate(Condition = paste0(.data$Condition, "_background"))
  
  reference_signal_min <- dplyr::bind_rows(reference_signal_min, reference_signal_min_background)
  
  # calculate fold change compared to reference
  summary_df <- summary_df %>%
    dplyr::left_join(reference_signal_min) %>%
    dplyr::mutate(
      Nuclear_Area_median_fold_change = .data$Nuclear_Area_median / .data$Nuclear_Area_median_reference,
      EdU_median_fold_change = .data$EdU_median / .data$EdU_median_reference,
      SABGal_median_fold_change = .data$SABGal_median / .data$SABGal_median_reference
    ) %>%
    dplyr::select(!dplyr::all_of(c("Nuclear_Area_median_reference", "EdU_median_reference", "SABGal_median_reference")))
  
  # Re-add plate column -----------------------------------------------------
  
  # create small df with plate column
  plate_df <- df_single_cell_data %>%
    dplyr::select(.data$plate, .data$well, .data$Condition, dplyr::all_of(additional_variables)) %>%
    unique()
  
  # join plate_df with summary df
  summary_df <- dplyr::left_join(summary_df, plate_df) %>%
    dplyr::select(.data$plate, dplyr::everything()) # rearrange plate to be 1st column
  
  # reduce decimal digits to 2 for all <dbl> columns ----------------------------------------------
  
  # identify percentage columns
  is_percentage_column <- grepl("percentage|%", names(summary_df))
  
  # multiply proportion values by 100
  summary_df[ , is_percentage_column] <- summary_df[ , is_percentage_column] * 100
  
  # identify percentage a double columns
  is_double_column <- sapply(summary_df, is.double)
  
  # Format values to 2 decimal digits
  summary_df[, is_double_column] <- lapply(summary_df[, is_double_column], function(x) format(x, digits = 2, nsmall = 2))
  
  # Adjust column order -----------------------------------------------------
  summary_df %>%
    dplyr::select(plate, well, Condition, additional_variables, dplyr::everything()) %>%
    rearrange_df_columns(.,
                       cols_to_move = c("Nuclear_Area_median_fold_change", "EdU_median_fold_change", "SABGal_median_fold_change"),
                       col_anchor =  "SABGal_max")
    
  
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