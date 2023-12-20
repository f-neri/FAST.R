check_file_numbers_match <- function(IAoutput_file_names,
                                     metadata_file_names) {
  
  # check that an equal number of IAoutput and metadata files have been uploaded
  if (length(IAoutput_file_names) != length(metadata_file_names)) {
    enable_button_analysis()
    validate(paste0(
      "ERROR: Mismatch in number of files uploaded
          
          The number of uploaded Image Analyst output files must equal the number of uploaded adjusted metadata files.
          Verify that each Image Analyst output (.xlsx) file has a corresponding metadata (.csv) file.
          
          uploaded Image Analyst output files: ", length(IAoutput_file_names),"
          uploaded Adjusted metadata files: ", length(metadata_file_names)
    )
    )
  }
  
}