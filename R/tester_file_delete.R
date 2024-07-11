# library(devtools)
# devtools::load_all()

# source("R/app.R")
# source("R/data_analysisServer.R")
# source("R/data_analysisUI.R")
# 
# library(dplyr)
# library(purrr)
# 
# source("R/generate_single_cell_df.R")
# source("R/load_input_files.R")
# source("R/tidy_IAoutput.R")
# source("R/get_feature_channels.R")
# 
# IAOutput_file <- '/Users/alicezhang/Downloads/Example_Data/example_Image_Analyst_Output_File.xlsx'
# metadata_file <- '/Users/alicezhang/Downloads/Example_Data/example_Image_Analyst_Output_File_metadata.csv'
# 
# Image_Analyst_output <- readxl::read_xlsx(path = IAOutput_file, skip = 1, na = "NA")
# plate_metadata <- utils::read.csv(metadata_file)
# 
# morpho_list <- c("Nuclear_Area")
# ft_list <- c("DAPI", "EdU", "SABGal")
# 
# Input_files <- load_input_files(Image_Analyst_output, plate_metadata)
# 
# single_cell_df <- generate_single_cell_df(Input_files, morpho_list, ft_list)
# 
# ################## analyze_sg_cell_data.R ##################
# 
# source("R/analyze_single_cell_data.R")
# source("R/calculate_summary_stats.R")
# source("R/generate_permuatations.R")
# source("R/counts_percentages.R")
# source("R/rearrange_df_columns.R")
# 
# df_single_cell_data <- single_cell_df
# background_threshold <- 0.95
# 
# # summary_expressions <- generate_summary_expressions(input_feature_list)
# 
# analysis_table <- analyze_single_cell_data(df_single_cell_data, background_threshold, morpho_list, ft_list)
# # analysis_table <- analyze_single_cell_data(df_single_cell_data, background_threshold)
# View(analysis_table)
