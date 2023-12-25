test_that("file number check works", {
  testServer(data_analysisServer, {
    # length each = 1
    IAoutput_file_names_1 <- metadata_file_names_1 <- reactiveVal(vector(mode = "character", length = 1))
    IAoutput_file_names_2 <- metadata_file_names_2 <- reactiveVal(vector(mode = "character", length = 2))
    
    expect_silent(check_file_numbers_match(IAoutput_file_names_1(), metadata_file_names_1()))
    expect_silent(check_file_numbers_match(IAoutput_file_names_2(), metadata_file_names_2()))
    
    expect_error(check_file_numbers_match(IAoutput_file_names_1(), metadata_file_names_2()), "ERROR: Mismatch in number of files uploaded")
    expect_error(check_file_numbers_match(IAoutput_file_names_2(), metadata_file_names_1()), "ERROR: Mismatch in number of files uploaded")
  })
  })