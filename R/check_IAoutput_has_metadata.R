check_IAoutput_has_metadata <- function(IAoutput_files, plate_metadata_files) {
  if ( any(IAoutput_files$IAoutput_name != plate_metadata_files$IAoutput_name) ) {
    enable_button_analysis()
    validate(
      paste0(
        "ERROR: Mismatch in file names
          
          For each Image Analyst output file uploaded, an adjusted plate metadata file with the same name + \"_metadata\" must also be uploaded.
          Verify that each Image_Analyst_output.xlsx file has a corresponding Image_Analyst_output_metadata.csv file.
          
          uploaded Image Analyst output file: ", paste(c(IAoutput_files$IAoutput_name[IAoutput_files$IAoutput_name != plate_metadata_files$IAoutput_name]), collapse=", "),"
          uploaded Adjusted metadata file: ", paste(c(plate_metadata_files$IAoutput_name[IAoutput_files$IAoutput_name != plate_metadata_files$IAoutput_name]), collapse=", ")
      )
    )
  }
}