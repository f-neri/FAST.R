# Indicate the variables to turn into plate templates
write_template_metadata <- function(file_path, variables, file_name = "metadata") {
  
  stopifnot(is.character(variables))
  
  # helper function that creates an empty plate template with 8 rows and 12 columns + extra empty row
  create_empty_plate <- function(var_name) {
    
    plate <- tibble::tribble(
      ~"0" , ~"1", ~"2", ~"3", ~"4", ~"5", ~"6", ~"7", ~"8", ~"9", ~"10", ~"11", ~"12",
      "0" , "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
      "A", "", "", "", "", "", "", "", "", "", "", "", "",
      "B", "", "", "", "", "", "", "", "", "", "", "", "",
      "C", "", "", "", "", "", "", "", "", "", "", "", "",
      "D", "", "", "", "", "", "", "", "", "", "", "", "",
      "E", "", "", "", "", "", "", "", "", "", "", "", "",
      "F", "", "", "", "", "", "", "", "", "", "", "", "",
      "G", "", "", "", "", "", "", "", "", "", "", "", "",
      "H", "", "", "", "", "", "", "", "", "", "", "", "",
      "", "", "", "", "", "", "", "", "", "", "", "", "",
    )
    
    plate[1,1] <- var_name
    
    plate
    
  }
  
  # generate metadata template 
  create_metadata_templates <- function(var_names) {
    
    template_list <- vector(mode = "list", length = length(var_names))
    
    for (i in seq_along(var_names)) {
      template_list[[i]] <- create_empty_plate(var_names[i])
    }
    
    template_list
  }
  
  metadata_template <- create_metadata_templates(variables) %>%
    dplyr::bind_rows()
  
  metadata_template
  
  # Write to TSV
  utils::write.table(metadata_template,
              file.path(file_path, paste0(file_name,".tsv")),
              sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
}
  


