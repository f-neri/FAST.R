################################################################################
# 07/08 Modified load_input_files.R Script
#
# Load and check IA Output file and meta data - need to update UI
################################################################################

load_input_files <- function(Image_Analyst_output, plate_metadata) {
  
  check_file_numbers_match(Image_Analyst_output$name,
                           plate_metadata$name)
  
  ## create IAoutput and plate_metadata tibbles
  IAoutput_files <- tibble::tibble(
    IAoutput_name = Image_Analyst_output$name,
    IAoutput_datapath = Image_Analyst_output$datapath
  ) %>%
    dplyr::arrange(.data$IAoutput_name)
  
  plate_metadata_files <- tibble::tibble(
    metadata_name = plate_metadata$name,
    metadata_datapath = plate_metadata$datapath
  ) %>%
    dplyr::arrange(.data$metadata_name)
  
  ## check that each IAoutput file has a corresponding plate_metadata file with appropriate name (IAoutput_metadata.csv)
  plate_metadata_files$IAoutput_name <- plate_metadata_files$metadata_name %>%
    gsub(pattern = "_metadata.csv", replacement = ".xlsx")
  
  check_IAoutput_has_metadata(IAoutput_files, plate_metadata_files)
  
  ## create input file table
  Input_files <- dplyr::full_join(IAoutput_files, plate_metadata_files) %>%
    suppressMessages()
  
  ## read and check metadata files
  Input_files$metadata_df <- vector(mode = "list", length = nrow(Input_files)) # empty list-column to store metadata
  
  for (i in seq_len(nrow(Input_files))) {
    
    # read metadata
    Input_files$metadata_df[[i]] <- plater::read_plate(file = Input_files$metadata_datapath[i],
                                                       well_ids_column = "well",    # name to give column of well IDs
                                                       sep = ",") %>%               # separator used in the csv file
      dplyr::select(dplyr::where(~ !all(is.na(.x)))) # remove columns whose values are all NA
    
    # adjust metadata variable names
    names(Input_files$metadata_df[[i]]) <- names(Input_files$metadata_df[[i]]) %>%
      make.names()
    
    # check that metadata has a variable named Condition
    
    names(Input_files$metadata_df[[i]])[grepl("^(?i)(condition)$", names(Input_files$metadata_df[[i]]))] <- "Condition" # set Condition capitalization
    names(Input_files$metadata_df[[i]])[grepl("^(?i)(ML_Training)$", names(Input_files$metadata_df[[i]]))] <- "ML_Training" # set Condition capitalization
    
    if ( !(any(grepl("^Condition$", names(Input_files$metadata_df[[i]])))) ) { # if metadata does NOT have Condition column
      enable_button_analysis()
      validate(
        paste0(
          "ERROR: Plate metadata is incorrect; \"Condition\" is missing
          
          Ensure that each metadata file contains 1 plate template named \"Condition\"
          
          Incorrect metadata file: ", Input_files$metadata_name[i], "
          Plate template names/variables: ",paste(c(names(Input_files$metadata_df[[i]])), collapse=", ")
        )
      )
    }
    
    # check that metadata has max 2 additional variables
    add_var_names_indeces <- names(Input_files$metadata_df[[i]]) %>%
      # remove Condition and ML_Training
      stringr::str_detect("^Condition$|^ML_Training$|well", negate = TRUE)
    
    add_var_names <- names(Input_files$metadata_df[[i]])[add_var_names_indeces]
    
    if ( length(add_var_names) > 2 ) { 
      enable_button_analysis()
      validate(
        paste0(
          "ERROR: Metadata is incorrect; too many microplate templates/variables
          
          Ensure that each metadata file contains no more than 4 microplate templates/variables (\"Condition\", \"ML_Training\" + 2 optional additional variables)
          
          incorrect metadata file: ", Input_files$metadata_name[i], "
          Plate template names/variables: ",paste(c(names(Input_files$metadata_df[[i]])), collapse=", ")
        )
      )
    }
  }
  
  ## check all metadata files have same variables
  
  ### get vector with indices of Input_files$metadata_df whose col names don't match those of the 1st df
  mismatched_indices <- check_variables_match_across_dfs(Input_files$metadata_df)
  
  ### check vector is empty (i.e. all df names match the first one)
  if (length(mismatched_indices) > 0) {
    enable_button_analysis()
    validate(
      paste0(
        "ERROR: Plate metadata files should all have the same variables

          Ensure that all metadata files contain the same plate template names/variables.

          Variables in first metadata file (", Input_files$metadata_name[1], "): ", paste(c(sort(names(Input_files$metadata_df[[1]]))), collapse=", "), "
          Mismatched metadata files: ", paste(c(Input_files$metadata_name[mismatched_indices]), collapse=", ")
      )
    )
  }
  
  ## read and check IAoutput files
  Input_files$IAoutput_df <- vector(mode = "list", length = nrow(Input_files))
  
  for (i in seq_len(nrow(Input_files))) {
    
    # read IAoutput
    Input_files$IAoutput_df[[i]] <- readxl::read_xlsx(path = Input_files$IAoutput_datapath[i], skip = 1, na = "NA")
    
    # check that each IAoutput file and corresponding plate_metadata file have same # of wells/labels
    n_channels <- Input_files$IAoutput_df[[i]]$Channel %>% unique() %>% length()
    
    number_wells_IAoutput <- nrow( Input_files$IAoutput_df[[i]] ) / n_channels
    
    number_wells_metadata <- Input_files$metadata_df[[i]]$well %>% length()
    
    if (number_wells_IAoutput != number_wells_metadata) {
      enable_button_analysis()
      validate(
        paste0(
          "ERROR: Mismatch in well number between Image Analyst output file and adjusted plate metadata
          
          Ensure that each well present in the Image Analyst output file has a corresponding label in the Plate Metadata file
          
          Image Analyst output file: ", Input_files$IAoutput_name[i], "
          Well number: ", number_wells_IAoutput,"
          
          Adjusted plate metadata file: ", Input_files$metadata_name[i], "
          Well number: ", number_wells_metadata
        )
      )
    }  
  }
  
  # return df
  Input_files
}

