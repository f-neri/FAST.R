test_that("IAoutput_has_metadata check works", {
  testServer(data_analysisServer, expr = {
    IAoutput_files <- tibble::tibble(IAoutput_name = c("ABT263_2023-0329_no9uM.xlsx",
                                                       "IA-output_10x_prw15_230407.xlsx"))
    plate_metadata_files <- tibble::tibble(metadata_name = c("ABT263_2023-0329_no9uM_metadata.csv",
                                                             "IA-output_10x_prw15_230407_metadata.csv"))
    expect_silent(check_IAoutput_has_metadata(IAoutput_files, plate_metadata_files))
    
    plate_metadata_files$metadata_name <- ""
    expect_error(check_IAoutput_has_metadata(IAoutput_files, plate_metadata_files), "ERROR: Mismatch in file names")
  })
})