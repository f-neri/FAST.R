test_that("files are correctly loaded and analyzed", {
  testServer(data_analysisServer, expr = {
    # set inputs
    
    ## Image_Analyst_output
    IAoutput_name <- IAoutput_datapath %>% basename()
    
    ## plate_metadata
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
    
    expect_equal(Input_files$metadata_df[[1]] %>% names(), col_names_metadata)
    expect_equal(Input_files$metadata_df[[1]] %>% nrow(), 60)
    
    
    # generate_single_cell_df
    single_cell_df <- generate_single_cell_df(Input_files,
                                              training_folds = 3,
                                              training_repeats = 1)
    
    expect_equal(single_cell_df %>% names(), col_names_single_cell)
    expect_equal(single_cell_df %>% nrow(), 24119)
    
    # update background_threshold
    session$setInputs(background_threshold = 0.95)
    
    # analyze_single_cell_data
    analysis_report <- analyze_single_cell_data(single_cell_df, input$background_threshold)
    
    expect_equal(analysis_report %>% names(), col_names_analysis_report)
    expect_equal(analysis_report %>% nrow(), 60)
    
  })
})
