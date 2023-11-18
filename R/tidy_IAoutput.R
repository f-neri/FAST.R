# adjust IA output excel file to have tidy format
tidy_IAoutput <- function(data,
                          Nuclear_Area_channel_number = 0,
                          DAPI_channel_number = 1,
                          EdU_channel_number = 2,
                          SABGal_channel_number = 3) {
  
  IAoutput <- data
  
  # set Condition capitalization
  names(IAoutput)[grepl("^(?i)(condition)$", names(IAoutput))] <- "Condition"
  
  # change Plot_Name/Name column name to well
  IAoutput <- if ( any(colnames(IAoutput) %in% "Plot Name") ) {
    IAoutput %>% dplyr::rename(well = "Plot Name")
  } else {
    IAoutput %>% dplyr::rename(well = Name)
  }
  
  # remove extra columns
  
  ## the pattern looks for "well", "Channel", or "OBJ" followed by any number
  ## the regular expression is case insensitive, indicated by (?i)
  pattern_to_keep <- "^(?i)(well|Channel|OBJ[0-9]*)$"
  
  cols_to_keep <- grepl(pattern_to_keep, names(IAoutput))
  
  IAoutput <- IAoutput[, cols_to_keep]
  
  # pivot_longer()
  OBJ_vec <- colnames(IAoutput)[-(1:2)]
  
  tidy_data1 <-  tidyr::pivot_longer(IAoutput, all_of(OBJ_vec),
                                     names_to = "cell_ID",
                                     values_to = "Signal_Intensity")
  
  # adjust Channel column name and values
  
  tidy_data1 <- tidy_data1 %>%
    dplyr::rename(Measured_Parameter = Channel) %>%
    dplyr::mutate(Measured_Parameter = dplyr::case_when(
      Measured_Parameter == 0 ~ "Nuclear_Area",
      Measured_Parameter == 1 ~ "DAPI",
      Measured_Parameter == 2 ~ "EdU",
      Measured_Parameter == 3 ~ "SABGal",
      .default = NA)
    )
  
  # adjust well column values - extract first letter followed by 2 digits
  
  tidy_data1 <- tidy_data1 %>%
    dplyr::mutate(well = stringr::str_extract(well, "[A-Za-z]\\d{2}")) %>%
    dplyr::select(well, cell_ID,everything()) # rearrange column order
  
  # pivot_wider()
  
  tidy_data2 <- tidy_data1 %>%
    tidyr::pivot_wider(names_from = Measured_Parameter,
                       values_from = Signal_Intensity) %>%
    na.omit() # remove cells that had with blank/NA signal intensities in IAoutput file
  
  print("completed tidying IA output")
  
  tidy_data2
}