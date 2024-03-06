example_dataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # download Example Data handler
    output$download_example_data <- downloadHandler(
      filename = function() {
        "Example_Data.zip"
      },
      content = function(file) {
        
        # create temporary directory to collect all files for download
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        
        # get example data file names
        path_example_data <- system.file("extdata/IR_Serum-Starvation", package = "FAST.R")
        file_names <- list.files(path_example_data)
        
        files_paths <- tibble::tibble(
          # generate example data file paths
          from = purrr::map_chr(file_names, ~ file.path(path_example_data, .x)),
          # generate corresponding files paths in the temp dir
          to = purrr::map_chr(file_names, ~ file.path(temp_directory, .x))
        )
        
        # copy example data files into temp dir
        purrr::pwalk(files_paths, file.copy)
        
        # zip temp dir for download
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
      },
      contentType = "application/zip"
    )
    
  })
}