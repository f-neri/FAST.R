# adjust IA output excel file to have tidy format
tidy_IAoutput <- function(file_path,
         DAPI_label_number = 1,
         EdU_label_number = 2,
         SABGal_label_number = 3,
         morphology_parameter = "Nuclear_Area") {
  
  # add ".xlsx" if omitted in file path
  
  file_path <- if (str_detect(file_path, ".xlsx")) {file_path
  } else {str_c(file_path, ".xlsx")}
  
  # check if file name exists
  
  if (file.exists(file_path)) {
    str_c(file_path, " was found")
  } else {
    beep(1)
    Sys.sleep(1)
    validate(
      HTML(paste0("ERROR<br/>", 
                  file_path, " was not found.<br/>",
                  "Ensure file path entered corresponds to the Image Analyst output .xlsx file"))
      )
  }
  
  # import IA-output file
  IA_output_unadjusted <- read_xlsx(file_path, skip = 1, na = "NA")
  
  # adjust new IA output to match older IA output (removing extra columns, changing "Plot Name" to "Name")
  
  cols_to_remove <- c("Folder Name", "Base Name", "Position Name", "Frame")
  
  cols_to_keep <- colnames(IA_output_unadjusted)[!(colnames(IA_output_unadjusted) %in% cols_to_remove)]
  
  IA_output <- IA_output_unadjusted[,cols_to_keep]
  
  if (any(colnames(IA_output) %in% "Plot Name")) {
    colnames(IA_output)[grep("Plot Name", colnames(IA_output))] <- "Name"
  }
  
  # change column names so they're all "OBJ#"
  
  col_names <- colnames(IA_output)
  
  adjusted_col_names <- col_names[-(1:2)] %>%
    grep(pattern = "[.][.][.]", value = TRUE) %>%
    str_sub(start = 4) %>%
    as.numeric() %>%
    -2 %>%
    str_c("OBJ", .)
  
  final_col_names <- col_names %>%
    grep(pattern = "[.][.][.]", invert = TRUE, value = TRUE) %>%
    c(., adjusted_col_names)
  
  colnames(IA_output) <- final_col_names
  
  # tidy the dataset
  
  ## pivot_longer()
  
  ### creating character vector with all OBJ# present in the data frame header
  
  OBJ_vec <- colnames(IA_output)[-(1:2)]
  
  ### pivoting
  tidy_data1 <-  pivot_longer(IA_output, all_of(OBJ_vec),
                              names_to = "cell_ID",
                              values_to = "Signal_Intensity")
  
  ## splitting "Name" Column into well # and the parameter measured
  tidy_data2 <- mutate(tidy_data1,
                       well = sub(".+ - ([0-9]+ )*", "", tidy_data1$Name),
                       Measured_Parameter = sub(" - .*", "", tidy_data1$Name)
  )
  
  tidy_data2$well <- ifelse(nchar(tidy_data2$well) == 2, sub(pattern = "(.)(.)", replacement = "\\10\\2", tidy_data2$well), tidy_data2$well)
  
  ## eliminating "Name" and "Channel" columns, rearranging tibble
  tidy_data3 <- select(tidy_data2, -(Name:Channel)) %>%
    select(well, cell_ID,everything())
  
  ## pivot_wider()
  tidy_data4 <- tidy_data3 %>%
    pivot_wider(names_from = Measured_Parameter,
                values_from = Signal_Intensity)
  
  ## removing non-existing cells from each well (i.e. where signal intensities = NA)
  tidy_data5 <- tidy_data4 %>% na.omit()
  
  # changing the column names based on the strings entered at the beginning of script (READ ME section)
  
  ## changing current variable/column names to the ones inputted in the READ ME section
  colnames(tidy_data5)[str_detect(colnames(tidy_data5), "Plot of Each")] <- morphology_parameter
  
  latest_value_tidy_IAoutput <- colnames(tidy_data5)
  
  param <- tibble(previous = list(DAPI_label_number, EdU_label_number, SABGal_label_number),
                  new = list("DAPI","EdU","SABGal"))
  
  update_value <- function(previous, new) {
    latest_value_tidy_IAoutput <<- sub(pattern = str_c(".+Label #", previous, ".*"), replacement = new, latest_value_tidy_IAoutput)
  } # function needs "<<-" in order to update the global variable "latest_value_tidy_IAoutput"
  
  map2(param$previous, param$new, update_value)
  
  colnames(tidy_data5) <- latest_value_tidy_IAoutput
  
  "completed tidying IA output"
  
  tidy_data5
}