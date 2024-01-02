test_that("files are correctly loaded and analyzed", {
  testServer(data_analysisServer, expr = {
    # set inputs
    
    ## Image_Analyst_output
    IAoutput_datapath <- system.file("extdata", "IR_Serum-Starvation/IR_sum_2023-0317.xlsx", package = "FAST.R")
    IAoutput_name <- IAoutput_datapath %>% basename()
    
    ## plate_metadata
    metadata_datapath <- system.file("extdata", "IR_Serum-Starvation/IR_sum_2023-0317_metadata.csv", package = "FAST.R")
    metadata_name <- metadata_datapath %>% basename()
    
    # update input$Image_Analyst_output, input$plate_metadata, input$background_threshold
    session$setInputs(Image_Analyst_output = list(datapath = IAoutput_datapath,
                                                  name = IAoutput_name),
                      plate_metadata = list(datapath = metadata_datapath,
                                            name = metadata_name))
    
    # load_input_files
    Input_files <- load_input_files(input$Image_Analyst_output,
                                    input$plate_metadata)
    
    expect_equal(nrow(Input_files), 1)
    
    expect_equal(Input_files$metadata_df[[1]] %>% names(), c("well", "Condition", "Serum"))
    expect_equal(Input_files$metadata_df[[1]] %>% nrow(), 60)
    
    
    # generate_single_cell_df
    single_cell_df <- generate_single_cell_df(Input_files)
    
    expect_equal(single_cell_df %>% names(), c("plate", "well", "cell_ID", "Condition", "Serum", "Nuclear_Area", "DAPI", "EdU", "SABGal" ))
    expect_equal(single_cell_df %>% nrow(), 24119)
    
    # update background_threshold
    session$setInputs(background_threshold = 0.95)
    
    # analyze_single_cell_data
    analysis_report <- analyze_single_cell_data(single_cell_df, input$background_threshold)
    
    expect_equal(analysis_report %>% names(),
                 c("plate", "well", "Condition", "Serum", "cell_counts", "Nuclear_Area_min",
                   "Nuclear_Area_25th", "Nuclear_Area_median", "Nuclear_Area_75th",
                   "Nuclear_Area_max", "EdU_min", "EdU_25th", "EdU_median", "EdU_75th",
                   "EdU_max", "SABGal_min", "SABGal_25th", "SABGal_median", "SABGal_75th",
                   "SABGal_max", "EdU_threshold", "SABGal_threshold", "counts_EdU_positive",
                   "counts_SABGal_positive", "counts_EdU_negative_SABGal_negative",
                   "counts_EdU_negative_SABGal_positive", "counts_EdU_positive_SABGal_negative",
                   "counts_EdU_positive_SABGal_positive", "percentage_EdU_positive",
                   "percentage_SABGal_positive", "percentage_EdU_negative_SABGal_negative",
                   "percentage_EdU_negative_SABGal_positive", "percentage_EdU_positive_SABGal_negative",
                   "percentage_EdU_positive_SABGal_positive", "Nuclear_Area_median_fold_change",
                   "EdU_median_fold_change", "SABGal_median_fold_change" ))
    expect_equal(analysis_report %>% nrow(), 60)
    
  })
})
