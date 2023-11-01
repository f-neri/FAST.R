data_analysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # observe: input IAoutput
    observeEvent(input$Image_Analyst_output, {
      message("\nImage Analyst output file:\n", input$Image_Analyst_output$name, ";\n\nFiles uploaded: ", length(input$Image_Analyst_output$name), "\n")
    })
    
    # toggle download_metadata after data upload
    observe({
      shinyjs::toggleState(id = "download_metadata", condition = input$Image_Analyst_output)
    })
    
    # download plate metadata template
    template_names <- reactive({
      input$Image_Analyst_output$name %>%
        stringr::str_replace_all(., pattern = ".xlsx", replacement = "_metadata.tsv")
    })
    
    template_url <- "https://raw.githubusercontent.com/f-neri/FASTR/main/inst/extdata/plate-metadata.tsv"
    
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
    
    # observe: input adjusted metadata
    observeEvent(input$plate_template_name, {
      message("\nAdjusted plate metadata file(s):\n", input$plate_template_name$name, ";\n")
      message("File(s) uploaded: ", length(input$plate_template_name$name), "\n")
    })
    
    # Run data analysis
    
    withProgress({ # progress bar
      
      
      
      ## tidy IAoutput and merge with metadata
      tidied_IAoutput <- reactive({
        notification <- showNotification("Analysing data...", duration = NULL, closeButton = FALSE) # shows a permanent notification
        on.exit(removeNotification(notification), add = TRUE) # removes notification once expression is complted/terminated
        
        Sys.sleep(1) # pretend this is long calculation
        
        if (length(input$Image_Analyst_output$name) > 1) {
          validate(
            "test error tidied_IAoutput"
          )
        }
      }) %>%
        bindCache(input$Image_Analyst_output$datapath,
                  input$plate_metadata$datapath,
                  input$background_threshold) %>%
        bindEvent(input$button_analysis)
      
      ## perform calculations and generate analysis_report table
      analysis_report <- reactive({
        
        tidied_IAoutput() # start point for analysis report table
        input$background_threshold
        
        Sys.sleep(1) # pretend this is long calculation
        
        if (length(input$Image_Analyst_output$name) < 1) {
          validate(
            "test error analysis_report"
          )
        }
        
      }) %>%
        bindEvent(input$button_analysis)
      
    })
    
    # Print data analysis message
    output$analysis_message <- renderText({
      tidied_IAoutput()
      analysis_report()
      c("Data analysis was successful!!")
    }) %>%
      bindEvent(input$button_analysis)
    
    
  })
}