data_analysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Update UI: plate metadata template ----------------------------------------
    
    # toggle download_metadata button after data upload
    observe({
      shinyjs::toggleState(id = "download_metadata", condition = input$Image_Analyst_output)
    })
    
    # Handler download_metadata -----------------------------------------------
    
    # download plate metadata template
    template_names <- reactive({
      input$Image_Analyst_output$name %>%
        stringr::str_replace_all(pattern = ".xlsx", replacement = "_metadata.csv")
    })
    
    # copy template from package directory
    template_path <- system.file("extdata", "plate-metadata.csv", package = "FAST.R")
    
    output$download_metadata <- downloadHandler(
      filename = function() if (length(input$Image_Analyst_output$name) == 1) { # single IAoutput and metadata files
        template_names()
        
      } else { # multiple IAoutput and metadata files
        
        paste0("plate_metadata_templates_", Sys.Date(), ".zip")
      },
      
      content = function(file) if (length(input$Image_Analyst_output$name) == 1) { # single IAoutput and metadata files
        # copy template from package directory
        file.copy(template_path, file)
        
      } else { # multiple IAoutput and metadata files
        
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        
        file_paths <- vector("character", length = length(input$Image_Analyst_output$name))
        
        for (i in seq_along(input$Image_Analyst_output$name)) {
          file_paths[i] <- file.path(temp_directory, template_names()[i])
          suppressMessages(
            # download template from GitHub
            ## utils::download.file(template_url, destfile = file_paths[i], method = "auto")
            
            # copy template from package directory
            file.copy(template_path, file_paths[i])
          )
        }
        
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
        
      },
      contentType = "application/zip"
    )
    
    # ML Features -----------------------------------------------------------
    observeEvent(input$generate_dropdown, {
      selected_features <- c(input$morphological_feature, input$input_feature)
      
      output$dynamic_dropdown_ui <- renderUI({
        selectizeInput(
          inputId = NS(id, "input_ML_features"),
          label = "ML Features",
          choices = selected_features,
          multiple = TRUE,
          options = list(placeholder = 'Select ML features',
                         plugins = list('remove_button', 'drag_drop'))
        )
      })
    })
      
    # DATA ANALYSIS -----------------------------------------------------------
    
    # Load files --------------------------------------------------------------
    Input_files <- reactive({
      # Optional input handling: input$input_ML_features
      ml_features <- if (is.null(input$input_ML_features)) character(0) else input$input_ML_features
      
      req(input$Image_Analyst_output, input$plate_metadata, input$background_threshold, input$morphological_feature, input$input_feature)
      
      # Update UI ---------------------------------------------------------------
      
      # disable button_analysis while computing
      shinyjs::disable("button_analysis")
      
      # disable table outputs w/ shinyjs
      shinyjs::hide("sc_and_analysis_report_panel")
      
      # Load and check input files -------------------------------------------------------
      
      Input_files <- load_input_files(input$Image_Analyst_output,
                                      input$plate_metadata)
      
      # return loaded files
      Input_files
    }) %>%
      bindCache(input$Image_Analyst_output$datapath,
                input$plate_metadata$datapath,
                input$background_threshold,
                input$input_ML_features,
                input$morphological_feature,
                input$feature_list) %>%
      bindEvent(input$button_analysis)
    
    # Generate single_cell_data table -----------------------------------------------------------
    
    # tidy IAouput and merge with metadata
    single_cell_data <- reactive({
      
      # disable button_analysis while computing
      shinyjs::disable("button_analysis")
      
      Input_files <- Input_files()
      
      # Optional input handling: input$input_ML_features
      ml_features <- if (is.null(input$input_ML_features)) character(0) else input$input_ML_features
      
      single_cell_df <- generate_single_cell_df(Input_files, input$morphological_feature, input$input_feature, input$input_ML_features)
      
      # return single cell df
      single_cell_df
      
    }) %>%
      bindCache(input$Image_Analyst_output$datapath,
                input$plate_metadata$datapath,
                input$background_threshold,
                input$input_ML_features,
                input$morphological_feature,
                input$feature_list) %>%
      bindEvent(input$button_analysis)
    
    # Generate analysis_report table ------------------------------------------
    analysis_report <- reactive({
      
      # Optional input handling: input$input_ML_features
      ml_features <- if (is.null(input$input_ML_features)) character(0) else input$input_ML_features
      
      # disable button_analysis while computing
      shinyjs::disable("button_analysis")
      
      # enable button_analysis on exit
      on.exit({ enable_button_analysis() })
      
      # generate analysis report
      analysis_report <- analyze_single_cell_data(
        single_cell_data(),
        input$background_threshold,
        input$morphological_feature,
        input$input_feature
      )
      
      # return analysis report df
      analysis_report
    }) %>%
      bindCache(input$Image_Analyst_output$datapath,
                input$plate_metadata$datapath,
                input$background_threshold,
                input$morphological_feature,
                input$feature_list) %>%
      bindEvent(input$button_analysis)
    
    # OUTPUT ------------------------------------------------------------------
    
    # error message -----------------------------------------------------------
    
    # Print data analysis message
    output$analysis_report_message <- renderText({
      analysis_report() # creates dependency on analysis_report() output
      
      # turn output tables visible w/ shinyjs
      shinyjs::show("sc_and_analysis_report_panel")
      
      # return empty text if all good
      ""
    }) %>%
      bindEvent(input$button_analysis)
    
    # Hide output panel on load
    
    shinyjs::hide("sc_and_analysis_report_panel")
    
    # Show output panel upon calculation of single_cell_data() and analysis_report()
    observe({
      shinyjs::show("sc_and_analysis_report_panel")
    }) %>%
      bindEvent(single_cell_data(),
                analysis_report())
    
    # single cell data ------------------------------------------
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
    
    # analysis report --------------------------------------------------
    
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
      all_feature_list <- c(input$morphological_feature, input$input_feature)
      median_cols <- paste0(all_feature_list, "_median")
      percentage_pos_cols <- paste0("percentage_", all_feature_list, "_positive")
      cols_to_vis <- c("plate", "well", "Condition", additional_variables,
                       "cell_counts", median_cols,
                       percentage_pos_cols
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
    
    
    }) # close moduleServer
} # close data_analysisServer
