################################################################################
# 07/08 Modified generate_single_cell_df.R Script
#
# Dependent scripts: get_feature_channels.R, tidy_IAoutput.R,
#
# Returns single_cell_df to work with
################################################################################

. <- NULL # prevents R CMD note

generate_single_cell_df <- function(Input_files,
                                    input_morphological_feature_list,
                                    input_feature_list,
                                    input_ML_features,
                                    training_folds = 10,
                                    training_repeats = 3) {
  
  # Add features
  feature_list <- c(input_morphological_feature_list, input_feature_list)
  feature_dictionary <- get_feature_channels(Input_files, input_morphological_feature_list, input_feature_list)
  Input_files <- Input_files %>%
    dplyr::mutate(feature_channels = list(feature_dictionary))
  
  # tidy IAoutput and merge with metadata -----------------------------------
  
  Input_files$tidy_df <- vector(mode = "list", length = nrow(Input_files)) # create empty list-column to store tidy data
  
  for (i in seq_len(nrow(Input_files))) {

    # tidy IAoutput
    tidied_IAoutput <- tidy_IAoutput(Input_files$IAoutput_df[[i]], feature_dictionary)

    # add metadata to tidied_IAoutput
    df <- dplyr::left_join(tidied_IAoutput, Input_files$metadata_df[[i]])

    # add plate identifier (i.e. file name) and rearrange column names
    variable_names <- names(Input_files$metadata_df[[i]])[-(names(Input_files$metadata_df[[i]]) == "well")]

    df <- df %>%
      # add plate name
      dplyr::mutate(plate = Input_files$IAoutput_name[i]) %>%
      # rearrange cols
      dplyr::select(.data$plate, .data$well, .data$cell_ID, dplyr::all_of(variable_names), dplyr::everything())

    # Machine learning --------------------------------------------------------
    
    # check if Training metadata was provided
    if (any(names(df) %in% "ML_Training")) {

      # Train Random Forest model
      ## generate training df
      df_training <- df %>%
        dplyr::filter(
          # remove background wells
          !stringr::str_detect(.data$Condition, pattern = "_background"),
          # filter for wells selected as + or - sample for training
          .data$ML_Training %in% c("+","-")
        ) %>%
        dplyr::mutate(
          # turn ML_Training into a factor col
          ML_Training = factor(.data$ML_Training, levels = c("+", "-"))
        )

      ## Train a Random Forest model w/ the
      ## carete package https://cran.r-project.org/web/packages/caret/vignettes/caret.html

      set.seed(123) # set seed to ensure reproducibility for model training and prediction
      RFmodel <-  caret::train(
        x = df_training %>%
          dplyr::select(all_of(input_ML_features)),
        y = df_training$ML_Training,
        method = "rf",
        preProc = c("center", "scale"),
        trControl = caret::trainControl(
          method = "repeatedcv",
          number = training_folds,
          repeats = training_repeats,
          verboseIter = TRUE
        )
      )

      # TO ADD: error message if the training fails and causes an error

      # predict +/-
      df <- df %>%
        dplyr::mutate(
          # add predictions
          ML_Prediction = dplyr::case_when(
            !stringr::str_detect(.data$Condition, pattern = "_background") ~ stats::predict(RFmodel, newdata = df),
            TRUE ~ NA
          )
        ) %>%
        # rearrange cols
        dplyr::select(1:which(names(.) == "ML_Training"), .data$ML_Prediction, dplyr::everything())
    }

    # return df
    Input_files$tidy_df[[i]] <- df
  }

  # merge tidy_dfs into a single df
  single_cell_df <- dplyr::bind_rows(Input_files$tidy_df)
  Input_files$tidy_df[[i]] <- Input_files$IAoutput_df[[i]]
  # return df
  single_cell_df
}
