data_analysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    

# Load IA output file -----------------------------------------------------
    
    # observe: input IAoutput
    observeEvent(input$Image_Analyst_output, {
      message("\nImage Analyst output file:\n", input$Image_Analyst_output$name, ";\n\nFiles uploaded: ", length(input$Image_Analyst_output$name), "\n")
    })
    
    # check file extension is .xlsx
    
    ## ADD CODE
    
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
    
    template_url <- "https://raw.githubusercontent.com/f-neri/FAST.R/main/inst/extdata/plate-metadata.csv"
    
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
    
    # DATA ANALYSIS -----------------------------------------------------------
    
    # check input files +
    # tidy IAouput and merge with metadata
    single_cell_data <- reactive({
      
      req(input$Image_Analyst_output, input$plate_metadata, input$background_threshold)
      
      # Update UI ---------------------------------------------------------------
      
      # disable button_analysis while computing, and update message
      shinyjs::disable("button_analysis")
      updateActionButton(inputId = "button_analysis", label = "Checking uploaded files...", icon = icon("sync", class = "fa-spin")) # not working
      
      # disable table outputs w/ shinyjs
      hide_multiple_ids(c("df_single_cell_title",
                          "df_single_cell",
                          "analysis_report_title",
                          "df_analysis_report"))
      
      # enable button_analysis on exit
      on.exit({ enable_button_analysis() })
      
      # Read and check input files -------------------------------------------------------
      
      # --- MOVE FILE READING AND CHECKING INTO MODULE ---
      
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
        IAoutput_name = input$Image_Analyst_output$name,
        IAoutput_datapath = input$Image_Analyst_output$datapath
      ) %>%
        dplyr::arrange(.data$IAoutput_name)
      
      plate_metadata_files <- tibble::tibble(
        metadata_name = input$plate_metadata$name,
        metadata_datapath = input$plate_metadata$datapath
      ) %>%
        dplyr::arrange(.data$metadata_name)
      
      plate_metadata_files$IAoutput_name <- plate_metadata_files$metadata_name %>%
        gsub(pattern = "_metadata.csv", replacement = ".xlsx", .)
      
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
      
      ## create input file table
      Input_files <- dplyr::full_join(IAoutput_files, plate_metadata_files)
      
      ## read and check metadata files
      Input_files$metadata_df <- vector(mode = "list", length = nrow(Input_files)) # empty list-column to store metadata
      
      for (i in seq_len(nrow(Input_files))) {
        
        # read metadata
        Input_files$metadata_df[[i]] <- plater::read_plate(file = Input_files$metadata_datapath[i],
                                                           well_ids_column = "well",    # name to give column of well IDs
                                                           sep = ",") %>%               # separator used in the csv file
          dplyr::select(dplyr::where(~ !all(is.na(.)))) # remove columns whose values are all NA
        
        # adjust metadata variable names
        names(Input_files$metadata_df[[i]]) <- names(Input_files$metadata_df[[i]]) %>%
          make.names()
        
        # check that metadata has a variable named Condition
        if ( !(any(grepl("^condition$", names(Input_files$metadata_df[[i]]), ignore.case = TRUE))) ) { # if metadata does NOT have Condition column
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
        
        # check that metadata has max 3 variables
        if ( length(names(Input_files$metadata_df[[i]])) > 4 ) { 
          enable_button_analysis()
          validate(
            paste0(
              "ERROR: Plate metadata is incorrect; too many plate templates/variables
          
          Ensure that each metadata file contains no more than 3 plate templates/variables (\"Condition\" + 2 optional additional variables)
          
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
        n_channels <- Input_files$IAoutput_df[[i]] %>% .$Channel %>% unique() %>% length()
        number_wells_IAoutput <- nrow( Input_files$IAoutput_df[[i]] ) / n_channels
        
        number_wells_metadata <- Input_files$metadata_df[[i]] %>% .$well %>% length()
        
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
      
      # tidy IAoutput and merge with metadata -----------------------------------

      Input_files$tidy_df <- vector(mode = "list", length = nrow(Input_files)) # create emtpy list-column to store tidy data
      
      for (i in seq_len(nrow(Input_files))) {
        
        # tidy IAoutput
        tidied_IAoutput <- tidy_IAoutput(Input_files$IAoutput_df[[i]])
        
        # add metadata to tidied_IAoutput
        df <- dplyr::left_join(tidied_IAoutput, Input_files$metadata_df[[i]])
        
        # add plate identifier (i.e. file name) and rearrange column names
        variable_names <- names(Input_files$metadata_df[[i]])[-(names(Input_files$metadata_df[[i]]) == "well")]
        
        df <- df %>%
          dplyr::mutate(plate = Input_files$IAoutput_name[i]) %>% # add plate name
          dplyr::select(.data$plate, .data$well, .data$cell_ID, dplyr::all_of(variable_names), dplyr::everything()) # rearrange
        
        # return df
        Input_files$tidy_df[[i]] <- df
      }
      
      # merge tidy_dfs into a single df
      single_cell_df <- dplyr::bind_rows(Input_files$tidy_df)
      
      # return single cell df
      single_cell_df
      
    }) %>%
      bindCache(input$Image_Analyst_output$datapath,
                input$plate_metadata$datapath,
                input$background_threshold) %>%
      bindEvent(input$button_analysis)
    
    # Generate analysis_report table ------------------------------------------
    analysis_report <- reactive({
      
      # generate analysis report
      analysis_report <- analyze_single_cell_data(single_cell_data(), input$background_threshold)
      
      # return analysis report df
      analysis_report
    }) %>%
      bindCache(input$Image_Analyst_output$datapath,
                input$plate_metadata$datapath,
                input$background_threshold) %>%
      bindEvent(input$button_analysis)
    

# DATA ANALYSIS OUTPUT ----------------------------------------------------
    
    # Print data analysis message
    output$analysis_report_message <- renderText({
      analysis_report() # creates dependency on analysis_report() output
      
      # turn output tables visible w/ shinyjs
      show_multiple_ids(c("df_single_cell_title",
                          "df_single_cell",
                          "analysis_report_title",
                          "df_analysis_report",
                          "download_sc_data"))
      
      # return empty text if all good
      ""
    }) %>%
      bindEvent(input$button_analysis)
    
    # Output tidied single cell data ------------------------------------------
    output$df_single_cell_title <- renderText({
      single_cell_data()
      "Single Cell Data"
      })
    
    output$df_single_cell <- DT::renderDataTable({
      DT::datatable(
        single_cell_data(),
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scroller = TRUE,
                       scrollY = 200,
                       scrollX = 500,
                       deferRender = TRUE,
                       dom = 'lBfrtip',
                       fixedColumns = TRUE,
                       buttons = list(
                         list(extend = 'colvis', targets = 0, visible = FALSE)
                         )
                       ),
        rownames = FALSE)
      })
    
    # download button for single-cell data
    output$download_sc_data <- downloadHandler(
      filename = function() {
        paste0("Single_Cell_Data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(single_cell_data(), file, row.names = FALSE)
      }
    )
    
    # disable download button on app load
    shinyjs::disable("download_sc_data")
    shinyjs::hide("download_sc_data_panel")
    
    # enable download button upon calculation of single_cell_data()
    observe({
      shinyjs::enable("download_sc_data")
      shinyjs::show("download_sc_data_panel")
      
    }) %>%
      bindEvent(single_cell_data())
    
    # Output analysis report --------------------------------------------------
    
    ## table title
    output$analysis_report_title <- renderText({
      analysis_report()
      "Analysis Report"
    })
    
    ## set columns to be visible initially
    cols_to_hide_indices <- reactive({
      # create vector containing additional variables
      additional_variables <- names(analysis_report())[-c(1:3)] # remove plate, well, Condition
      
      pos_cell_counts <- which(additional_variables == "cell_counts") # find index for cell_counts
      
      additional_variables <- additional_variables[-c(pos_cell_counts:length(additional_variables))] # remove all vars after cell_counts, leaving only possible additional vars
      
      # create vector with cols to visualize
      cols_to_vis <- c("plate", "well", "Condition", additional_variables,
                       "cell_counts", "Nuclear_Area_median", "EdU_median", "SABGal_median",
                       "percentage_EdU_positive", "percentage_SABGal_positive"
                       )
      
      # get indices of cols to NOT visualize
      indices <- which(!(names(analysis_report()) %in% cols_to_vis)) %>% -1 # indices in columnDefs calls start from 0, not 1
      
      indices
    })
    
    ## render table
    output$df_analysis_report <- DT::renderDataTable({
      DT::datatable(
        analysis_report(),
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scroller = TRUE,
                       scrollY = 200,
                       scrollX = 500,
                       deferRender = TRUE,
                       dom = 'lBfrtip',
                       fixedColumns = TRUE,
                       buttons = list('colvis'),
                       columnDefs = list(
                         list(visible = FALSE, targets = cols_to_hide_indices())  # Use the vector to hide columns
                       )
                       ),
        rownames = FALSE)
      })
    
    ## download button for analysis report data
    output$download_analysis_report <- downloadHandler(
      filename = function() {
        paste0("Analysis_Report_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(analysis_report(), file, row.names = FALSE)
      }
    )
    
    # disable download button on app load
    shinyjs::disable("download_analysis_report")
    shinyjs::hide("download_analysis_report_panel")
    
    # enable download button upon calculation of single_cell_data()
    observe({
      shinyjs::enable("download_analysis_report")
      shinyjs::show("download_analysis_report_panel")
    }) %>%
      bindEvent(analysis_report())
    
    })
}
