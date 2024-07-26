# # library(devtools)
# # devtools::load_all()
# 
# single_cell_data_df <- read.csv("/Users/alicezhang/Downloads/Example_Data/chondrocyte/ML/Single_Cell_Data_2024-07-26.csv")
# analysis_report_df <- read.csv("/Users/alicezhang/Downloads/Example_Data/chondrocyte/ML/Analysis_Report_2024-07-26.csv")
# 
# cell_counts_column <- grep("cell_counts", names(analysis_report_df)) 
# nums <- cell_counts_column:length(names(analysis_report_df))
# add_vars <- names(analysis_report_df)[-c( nums )]
# add_vars <- add_vars[-c(1:3)]
# 
# df_single_cell <- single_cell_data_df
# df_single_cell <- df_single_cell %>%
#   dplyr::mutate(dplyr::across(dplyr::all_of(add_vars), factor))
# 
# 
# # test function
# 
# df <- tibble::tibble(
#   dim = c("width", "height",
#           "width_comparison", "height_comparison",
#           "width_percentages", "height_percentages"),
#   version = c("72")) %>%
#   dplyr::mutate(value = dplyr::case_when(
#     dim == "width" ~ ifelse(length(additional_variables) > 0,
#                             300 + 300 * length(unique(df[[ additional_variables[1] ]])),
#                             600),
#     dim == "height" ~ ifelse(length(additional_variables) > 1,
#                              100 + 250 * length(unique(df[[ additional_variables[2] ]])),
#                              350),
#     dim == "width_comparison" ~ 300 + 300 * length(unique(df$Condition)),
#     dim == "height_comparison" ~ ifelse(length(other_add_var) > 0,
#                                         100 + 250 * length(unique(df[[other_add_var]])),
#                                         350),
#     dim == "width_percentages" ~ ifelse(length(additional_variables) > 0,
#                                         300 + 300 * length(unique(df[[ additional_variables[1] ]])),
#                                         600),
#     dim == "height_percentages" ~ ifelse(length(other_add_var) > 0,
#                                          60 * length(unique(df$Condition)) * length(unique(df[[ additional_variables[2] ]])),
#                                          60 * length(unique(df$Condition)))
#   ))
# dim(df)
