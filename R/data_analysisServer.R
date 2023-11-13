data_analysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    

# Load IA output file -----------------------------------------------------
    
    # observe: input IAoutput
    observeEvent(input$Image_Analyst_output, {
      message("\nImage Analyst output file:\n", input$Image_Analyst_output$name, ";\n\nFiles uploaded: ", length(input$Image_Analyst_output$name), "\n")
    })
    
# Download plate metadata template ----------------------------------------
    
    # toggle download_metadata button after data upload
    observe({
      shinyjs::toggleState(id = "download_metadata", condition = input$Image_Analyst_output)
    })
    
    # download plate metadata template
    template_names <- reactive({
      input$Image_Analyst_output$name %>%
        stringr::str_replace_all(., pattern = ".xlsx", replacement = "_metadata.csv")
    })
    
    template_url <- "https://raw.githubusercontent.com/f-neri/FASTR/main/inst/extdata/plate-metadata.csv"
    
    output$download_metadata <- downloadHandler(
      filename = function() if (length(input$Image_Analyst_output$name) == 1) { # single IAoutput and metadata files
        template_names()
        
      } else { # multiple IAoutput and metadata files
        
        paste0("plate_metadata_templates_", Sys.Date(), ".zip")
      },
      
      content = function(file) if (length(input$Image_Analyst_output$name) == 1) { # single IAoutput and metadata files
        utils::download.file(template_url, destfile = file, method = "auto")
        message("Downloaded plate metadata template: ", template_names(),"\n")
        
      } else { # multiple IAoutput and metadata files
        
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        
        file_paths <- vector("character", length = length(input$Image_Analyst_output$name))
        
        for (i in seq_along(input$Image_Analyst_output$name)) {
          file_paths[i] <- file.path(temp_directory, template_names()[i])
          suppressMessages(
            utils::download.file(template_url, destfile = file_paths[i], method = "auto")
          )
          message("Downloaded plate metadata template: ", template_names()[i],"\n")
        }
        
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
        
      },
      contentType = "application/zip"
    )
    

# Upload adjusted metadata ------------------------------------------------

    # observe: input adjusted metadata
    observeEvent(input$plate_metadata, {
      message("\nAdjusted plate metadata file(s):\n", input$plate_metadata$name, ";\n")
      message("File(s) uploaded: ", length(input$plate_metadata$name), "\n")
    })

# Data Analysis -----------------------------------------------------------
    
    # tidy IAouput and merge with metadata
    tidied_IAoutput <- reactive({
      
      # Check input files: START
      
      ## disable button_analysis while computing, and update message
      shinyjs::disable("button_analysis")
      updateActionButton(inputId = "button_analysis", label = "Checking uploaded files...", icon = icon("sync", class = "fa-spin"))
      
      ## check that an equal number of IAoutput and metadata files have been uploaded
      if (length(input$Image_Analyst_output$name) != length(input$plate_metadata$name)) {
        enable_button_analysis()
        validate(paste0(
          "ERROR: Mismatch in number of files uploaded
          
          The number of uploaded Image Analyst output files must equal the number of uploaded adjusted metadata files.
          Verify that each Image Analyst output (.xlsx) file has a corresponding metadata (.csv) file.
          
          uploaded Image Analyst output files: ", length(input$Image_Analyst_output$name),"
          uploaded Adjusted metadata files: ", length(input$plate_metadata$name)
        )
        )
      }
      
      ## check that each IAoutput file has a corresponding plate_metadata file with appropriate name (IAoutput_metadata.csv)
      
      IAoutput_files <- tibble::tibble(
        name = input$Image_Analyst_output$name,
        IAoutput_datapath = input$Image_Analyst_output$datapath
      ) %>%
        dplyr::arrange(name)
      
      plate_metadata_files <- tibble::tibble(
        name = input$plate_metadata$name,
        metadata_datapath = input$plate_metadata$datapath
      ) %>%
        dplyr::arrange(name)
      
      plate_metadata_files$name <- plate_metadata_files$name %>%
        gsub(pattern = "_metadata.csv", replacement = ".xlsx", .)
      
      if ( any(IAoutput_files$name != plate_metadata_files$name) ) {
        enable_button_analysis()
        validate(
          paste0(
          "ERROR: Mismatch in file names
          
          For each Image Analyst output file uploaded, an adjusted plate metadata file with the same name + \"_metadata\" must also be uploaded.
          Verify that each Image_Analyst_output.xlsx file has a corresponding Image_Analyst_output_metadata.csv file.
          
          uploaded Image Analyst output file: ", input$Image_Analyst_output$name[IAoutput_files$name != plate_metadata_files$name],"
          uploaded Adjusted metadata file: ", input$plate_metadata$name[IAoutput_files$name != plate_metadata_files$name]
          )
        )
      }
      
      ## check that each IAoutput file and corresponding plate_metadata file have same # of wells/labels
      
      Input_files <- dplyr::left_join(IAoutput_files, plate_metadata_files) %>%
        dplyr::mutate(IAoutput_df = NA,
                      plate_metadata_df = NA)
      
      for (i in seq_len(nrow(Input_files))) {
        
        
        Input_files$IAoutput_df[i] <- readxl::read_xlsx(Input_files$IAoutput_datapath[i], skip = 1, na = "NA")
        Input_files$plate_metadata_df[i] <- plater::read_plate(Input_files$plate_metadata_df[i],
                                                               well_ids_column = "well",    # name to give column of well IDs
                                                               sep = "\t")                  # separator used in the csv file
        
      }
      
      # Check input files: END
      
      message("\ntidying IAoutput completed")
      
      Input_files
      
    }) %>%
      bindCache(input$Image_Analyst_output$datapath,
                input$plate_metadata$datapath,
                input$background_threshold) %>%
      bindEvent(input$button_analysis)
    
    # perform calculations and generate analysis_report table
    analysis_report <- reactive({
      
      tidied_IAoutput() # start point for analysis report table
      input$background_threshold # needed for % calculations
      
      Sys.sleep(1) # pretend this is long calculation
      
      message("\ndata analysis completed")
      
      on.exit({
        shinyjs::enable("button_analysis")
        updateActionButton(session, "button_analysis", label = "Run Analysis", icon = icon("rocket"))
      })
      
    }) %>%
      bindCache(input$Image_Analyst_output$datapath,
                input$plate_metadata$datapath,
                input$background_threshold) %>%
      bindEvent(input$button_analysis)
    
# Plot tables with analyzed data ------------------------------------------
    
    # Print data analysis message
    output$tidying_IAoutput_message <- renderText({
      tidied_IAoutput()
      c("Single-cell data table generated!!")
    }) %>%
      bindEvent(input$button_analysis)
    
    output$analysis_report_message <- renderText({
      analysis_report()
      c("Analysis report generated!!")
    }) %>%
      bindEvent(input$button_analysis)
    
    # output tidied single data
    output$df_single_cell <- renderDataTable(
      tidied_IAoutput(),
      options = list(pageLength = 5)
    )
    
    # output analysis report
    output$df_analysis_report <- renderDataTable(
      analysis_report(),
      options = list(pageLength = 5)
    )
    
  })
}