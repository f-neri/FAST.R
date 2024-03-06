
# test dataset paths
IAoutput_datapath <- system.file("extdata", "IR_Serum-Starvation", "example_Image_Analyst_Output_File.xlsx", package = "FAST.R")
metadata_datapath <- system.file("extdata", "IR_Serum-Starvation", "example_Image_Analyst_Output_File_metadata.csv", package = "FAST.R")

# expected metadata variables
col_names_metadata <- c("well", "Condition", "Serum", "ML_Training")

# expected col names
col_names_single_cell <- c("plate", "well", "cell_ID", "Condition", "Serum", "ML_Training", "ML_Prediction", "Nuclear_Area", "DAPI", "EdU", "SABGal" )

col_names_analysis_report <- c("plate", "well", "Condition", "ML_Training", "ML_Prediction_% +", "ML_Prediction_% -", "Serum",
                               "cell_counts", "Nuclear_Area_min", "Nuclear_Area_25th", "Nuclear_Area_median", "Nuclear_Area_75th",
                               "Nuclear_Area_max", "EdU_min", "EdU_25th", "EdU_median", "EdU_75th",
                               "EdU_max", "SABGal_min", "SABGal_25th", "SABGal_median", "SABGal_75th",
                               "SABGal_max", "EdU_threshold", "SABGal_threshold", "counts_EdU_positive",
                               "counts_SABGal_positive", "counts_EdU_negative_SABGal_negative",
                               "counts_EdU_negative_SABGal_positive", "counts_EdU_positive_SABGal_negative",
                               "counts_EdU_positive_SABGal_positive", "percentage_EdU_positive",
                               "percentage_SABGal_positive", "percentage_EdU_negative_SABGal_negative",
                               "percentage_EdU_negative_SABGal_positive", "percentage_EdU_positive_SABGal_negative",
                               "percentage_EdU_positive_SABGal_positive", "Nuclear_Area_median_fold_change",
                               "EdU_median_fold_change", "SABGal_median_fold_change" )