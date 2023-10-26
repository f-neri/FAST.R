
# merge_plate_metadata ----------------------------------------------------

# function to merge plate map/metadata with a tidied Image Analyst excel output

merge_plate_metadata <- function(
    tidied_IAoutput_df,
    metadata
    ) {
  # importing metadata regarding conditions of each well (IR or CTL, full serum or serum-starved, different drug concentrations etc.)
  
  ## plater
  plate_metadata <- read_plate(
    file = metadata,             # full path to the .csv file
    well_ids_column = "well",    # name to give column of well IDs (optional)
    sep = ","                     # separator used in the csv file (optional)
  )
  
  # change variable names to lower case, removing parenthesis (if present)
  colnames(plate_metadata) <- colnames(plate_metadata) %>%
    tolower() %>%
    str_replace_all(., "[()]", "")
  
  # check that plate-template contains Condition variable
  if (any(colnames(plate_metadata) %in% "condition") == TRUE) {} else {
    beep(1)
    Sys.sleep(2)
    stop(
      "The metadata entered in plate-template must contain the \"Condition\" variable"
    )}
  
  # check that the variables contained in plate-template are limited to
  # condition, and up to 2 additional variable (e.g. serum and/or drug treatment)
  additional_variables <- colnames(plate_metadata) %>%
    .[-grep(pattern = "well|condition", .)]
  
  if (length(additional_variables) > 2) {
    beep(1)
    Sys.sleep(2)
    stop(
      "The metadata entered in plate-template is not acceptable.

The only metadata that can be entered in the plate-template file are
\"Condition\" and up to TWO more variable (e.g. \"Serum\" and/or \"Drug_concentration\")")
  }

  # add plate column
  
  metadata_file_name <- metadata %>%
    sub(pattern = ".*\\/([^/]+)\\.csv$", replacement = "\\1")
  
  plate_metadata <- plate_metadata %>%
    mutate(plate = metadata_file_name)
  
  # add metadata info to tidied_IAoutput file
  
  ## mutating join with left_join()
  
  plate_metadata_variables <- colnames(plate_metadata)[-1]
  
  tidied_IAoutput_df %>%
    left_join(plate_metadata, by = "well") %>%
    select(plate, well, cell_ID, all_of(plate_metadata_variables), everything())
}


# Notes -------------------------------------------------------------------

# these need to be moved the data analysis part of FAST-R

## additional_variables_check <-  if (length(additional_variables) > 0) {TRUE} else {FALSE}
## multiple_additional_variables_check <- if (length(additional_variables) == 2) {TRUE} else {FALSE}
