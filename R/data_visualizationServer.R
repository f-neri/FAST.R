palette_names <- rownames(RColorBrewer::brewer.pal.info) 

data_visualizationServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # Read and check single-cell and analysis report data -------------------------------
    
    # read single-cell data
    single_cell_data_df <- reactive({
      utils::read.csv(file = input$single_cell_data_df$datapath)
    }) %>%
      bindCache(input$single_cell_data_df$datapath) %>%
      bindEvent(input$single_cell_data_df)
    
    # check single-cell data format
    ## TO ADD
    
    # read analysis report data
    analysis_report_df <- reactive({
      utils::read.csv(file = input$analysis_report_df$datapath)
    }) %>%
      bindCache(input$analysis_report_df$datapath) %>%
      bindEvent(input$analysis_report_df)
    
    # check analysis report data format
    ## TO ADD
    
    # Show upload error messages ----------------------------------------------
    output$upload_message <- renderText({
      
      # check sinlge-cell file matches analysis report file
      if (sort(unique(single_cell_data_df()$plate)) != sort(unique(analysis_report_df()$plate))) {
        validate(
          paste(
            "ERROR: mismatch between Single Cell Data and Analysis Report files
          
          Ensure to load single cell and analysis report files generated from the same plate(s)"
          )
        )
      }
      
      # show graph control widgets and example graphs
      shinyjs::show("graphs_control_widgets") # show after clicking Next
      shinyjs::show("example_graphs")
      
      # return empty text if all good
      ""
    }) %>%
      bindEvent(input$next_button)
    
    # Graphs control widgets handler ----------------------------------------------
    shinyjs::hide("graphs_control_widgets") # hide by default
    shinyjs::hide("example_graphs")
    
    observe({
      shinyjs::hide("graphs_control_widgets") # hide if analysis report or single cell data change
      shinyjs::hide("example_graphs")
    }) %>%
      bindEvent(input$analysis_report_df$datapath,
                input$single_cell_data_df$datapath)
    
    # Get the input features --------------------------------------------------
    
    input_features <- reactive({
      input_features <- c(input$morph_data_input_feature, input$data_input_feature)
      input_features
    })
    
    # Identify additional variables -------------------------------------------
    
    additional_variables <- reactive({
      cell_counts_column <- grep("cell_counts", names(analysis_report_df())) # get column position of cell_counts
            
      add_vars <- names(analysis_report_df())[-c(cell_counts_column:length(names(analysis_report_df())))] # remove all columns from cell_counts on
      
      add_vars <- add_vars[-c(1:3)] # remove first 3 columns (plate, well, Condition)
      
      add_vars
    }) %>%
      bindCache(input$analysis_report_df$datapath) %>%
      bindEvent(input$analysis_report_df)
    
    # Themes & colors ggplot2  ---------------------------------------------------------
    
    # use white background
    ggplot2::theme_set(ggplot2::theme_bw())
    
    # use reactive font size
    observe({
      ggplot2::theme_update(
        legend.title = ggplot2::element_text(size = input$legend_title),
        legend.text = ggplot2::element_text(size = input$legend_text),
        axis.title = element_text(size = input$size_axis_title),
        axis.text = element_text(size = input$size_axis_text),
        strip.text = element_text(size = input$size_facets_text)
      )
    })
    
    ggplot2_theme <- reactive({
      theme(
        legend.title = ggplot2::element_text(size = input$legend_title),
        legend.text = ggplot2::element_text(size = input$legend_text),
        axis.title = element_text(size = input$size_axis_title),
        axis.text = element_text(size = input$size_axis_text),
        strip.text = element_text(size = input$size_facets_text)
      )
    })
    
    # set scale fill and colors based on Conditions
    scale_fill_brewer_conditions <- reactive({
      ggplot2::scale_fill_brewer(palette = input$palette,
                                 limits = sort(unique(analysis_report_df()$Condition)),
                                 direction = ifelse(input$reverse == FALSE, 1, -1))
    })
    
    scale_color_brewer_conditions <- reactive({
      ggplot2::scale_color_brewer(palette = input$palette,
                                  limits = sort(unique(analysis_report_df()$Condition)),
                                  direction = ifelse(input$reverse == FALSE, 1, -1))
    })
    
    # comparison_graphs_panel handler -------------------------------------
    
    # disable panel on load
    shinyjs::disable("comparison_graphs_panel")
    
    # toggle panel based on input$analysis_report_df
    observe({
      if (length(additional_variables()) > 0) { # enable if analysis_report_df has additional variables
        shinyjs::enable("comparison_graphs_panel")
        updateSelectInput(session,
                          inputId = "select_comparison_variable",
                          choices = c(additional_variables()),
                          selected = additional_variables()[1])
      } else { # disable if analysis_report_df has NO additional variables
        shinyjs::disable("comparison_graphs_panel")
        updateCheckboxInput(session,
                            inputId = "generate_comparison_graphs",
                            value = FALSE)
        updateSelectInput(session,
                          inputId = "select_comparison_variable",
                          choices = c("NA"),
                          selected = "NA")
      }
    }) %>%
      bindEvent(input$analysis_report_df)
    
    # check for other_add_var
    other_add_var <- reactive({
      if (input$select_comparison_variable != "NA") {
        additional_variables()[!additional_variables() %in% input$select_comparison_variable]
      }
    })
    
    # Adjust data for plotting -----------------------------------------------------
    
    # single cell data df
    df_single_cell <- reactive({
      
      # Inputs from App
      input_features <- input$data_input_feature
      
      df_single_cell <- if (input$remove_background == TRUE) { # remove background cells/wells
        dplyr::filter(single_cell_data_df(), !stringr::str_detect(.data$Condition, "_background"))
      } else {
        single_cell_data_df()
      }
      
      df_single_cell <- df_single_cell %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(additional_variables()), factor)) %>% # change add_vars into factors
        dplyr::rowwise() %>%
        dplyr::filter(all(dplyr::c_across(dplyr::all_of(input_features)) > 0)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate( # calculate log10 values
          dplyr::across(
            .cols = all_of(input_features),
            .fns = list(log10 = ~log10(.x)),
            .names = "{.col}_log10"
          )
        )
      # return single cell df
      df_single_cell
    }) %>%
      bindCache(input$single_cell_data_df$datapath,
                input$remove_background)
    
    # single cell df
    single_cell_df <- reactive({
      
      # remove background cells/wells
      df <- if (input$remove_background == TRUE) {
        dplyr::filter(single_cell_data_df(), !stringr::str_detect(.data$Condition, "_background"))
      } else {
        single_cell_data_df()
      }
      
      # return df
      df
    }) %>%
      bindCache(input$single_cell_data_df$datapath,
                input$remove_background)
    
    # analysis report df
    df <- reactive({
      
      # remove background cells/wells
      df <- if (input$remove_background == TRUE) {
        dplyr::filter(analysis_report_df(), !stringr::str_detect(.data$Condition, "_background"))
      } else {
        analysis_report_df()
      }
      
      # change % to proportions for scales::percent calls to work
      df <- df %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("percentage"), ~ . / 100),
                      dplyr::across(dplyr::all_of(additional_variables()), factor)) # change add_vars into factors
      
      # return df
      df
    }) %>%
      bindCache(input$analysis_report_df$datapath,
                input$remove_background)
    
    # df_thresholds
    df_thresholds <- reactive({
      # Inputs from App
      input_features <- input$data_input_feature
      input_features_threshold <- paste0(input_features, "_threshold_average")
      
      generate_df_thresholds(data = df(),
                             input_features = input_features(),
                             additional_variables = additional_variables()
      ) %>%
        dplyr::mutate( # calculate log10 threshold values
          dplyr::across(
            .cols = dplyr::all_of(input_features_threshold),
            .fns = list(log10 = ~log10(.x)),
            .names = "{.col}_log10"
          )
        )
    })
    
    # Calculate default width and height for plots -----------------------------------------------------
    
    # turn input$dpi into num value
    dpi <- reactive({
      input$dpi %>% as.numeric()
    })
    
    # calculate widths and heigths
    dims_plot <- reactive({
      
      # create df with all widths and heights and calculate respective values
      df <- tibble::tibble(
        dim = c("width", "height",
                "width_comparison", "height_comparison",
                "width_percentages", "height_percentages"),
        version = c("72"))  %>%
        dplyr::mutate(value = dplyr::case_when(
          dim == "width" ~ ifelse(length(additional_variables()) > 0,
                                  300 + 300 * length(unique(df()[[ additional_variables()[1] ]])),
                                  600),
          dim == "height" ~ ifelse(length(additional_variables()) > 1,
                                   100 + 250 * length(unique(df()[[ additional_variables()[2] ]])),
                                   350),
          dim == "width_comparison" ~ 300 + 300 * length(unique(df()$Condition)),
          dim == "height_comparison" ~ ifelse(length(other_add_var()) > 0,
                                              100 + 250 * length(unique(df()[[other_add_var()]])),
                                              350),
          dim == "width_percentages" ~ ifelse(length(additional_variables()) > 0,
                                              300 + 300 * length(unique(df()[[ additional_variables()[1] ]])),
                                              600),
          dim == "height_percentages" ~ ifelse(length(other_add_var()) > 0,
                                               60 * length(unique(df()$Condition)) * length(unique(df()[[ additional_variables()[2] ]])),
                                               60 * length(unique(df()$Condition)))
        ))

      # create duplicate df with values adjusted based on dpi
      df_duplicate <- df %>%
        dplyr::mutate(version = "dpi_adj",
                      value = .data$value * dpi()/72)
      # merge dfs
      df <- dplyr::bind_rows(df, df_duplicate)
      
      # return df
      df
    }) %>%
      bindEvent(input$analysis_report_df, df(), input$dpi)
    
    # Plot example graphs -----------------------------------------------------
    
    well_percentages <- reactive({
      plot <- plot_well_percentages(data = df(),
                                    input_features = input_features(),
                                    add_vars = additional_variables(),
                                    scale_fill_brewer = scale_fill_brewer_conditions())
      plot +
        ggplot2_theme()
    })

    output$example_graph <- renderPlot({
      well_percentages()
    },
    width = function() {as.numeric(get_dim(dims_plot(), "width", "72"))},
    height = function() {as.numeric(get_dim(dims_plot(), "height", "72"))},
    res = 72)
    # %>% # COULDN'T CACHE
    # Caching forces to use sizePolicy() to controls plot dims, and it does not correctly update plot dims when reactive vars cahnge
    #  bindCache(
    #    sizePolicy = sizeGrowthRatio( # controls width and height in cached plots
    #      width = default_width(), ### causes WARNING ###
    #      height = default_height(), ### causes WARNING ###
    #      growthRate = 1 # growthRate = 1 prevents size changes in plot
    #    ),
    #    input$analysis_report_df$datapath, input$palette, input$reverse, ggplot2_theme(), input$remove_background,
    #    default_width(), default_height() ### prevents WARNING ###
    #  )
    
    # well percentages comparison
    well_percentages_comparison <- reactive({
      plot <- plot_well_percentages_comparison(data = df(),
                                               input_features = input_features(),
                                               add_vars = additional_variables(),
                                               comparison_var = input$select_comparison_variable,
                                               other_add_var = other_add_var())
      plot +
        ggplot2_theme()
    })
    
    output$example_graph_comparison <- renderPlot({
      well_percentages_comparison()
    },
    width = function() {get_dim(dims_plot(), "width_comparison", "72")},
    height = function() {get_dim(dims_plot(), "height_comparison", "72")},
    res = 72
    )
    # %>% # COULDN'T CACHE
    # Caching forces to use sizePolicy() to controls plot dims, and it does not correctly update plot dims when reactive vars cahnge
    #  bindCache( 
    #    sizePolicy = sizeGrowthRatio(width = default_width_comparison(), ### TO FIX ### input$remove_background changes don't update width
    #                                 height = default_height_comparison(),
    #                                 growthRate = 1), # growthRate = 1 prevents size changes in plot
    #    input$analysis_report_df$datapath, ggplot2_theme(), input$remove_background
    #  )
    
    ## hide well_percentages_comparison on load
    shinyjs::hide("example_graph_comparison")
    
    observe({
      if (input$generate_comparison_graphs == TRUE) {
        shinyjs::show("example_graph_comparison")
      } else {
        shinyjs::hide("example_graph_comparison")
      }
    }) %>%
      bindEvent(input$generate_comparison_graphs)
    
    # Generate all graphs -----------------------------------------------------------
    
    graphs <- reactive({
      
      # 2D plots  -----------------------------------------------------------
      
      # single cell staining
      single_cell_staining <- plot_single_cell_staining(data = df_single_cell(),
                                                        input_features = input_features(),
                                                        data_thresholds = df_thresholds(),
                                                        additional_variables = additional_variables(),
                                                        scale_color_brewer = scale_color_brewer_conditions())
      
      # median staining
      median_staining <- plot_median_staining(df(),
                                              input_features = input_features(),
                                              df_thresholds(),
                                              additional_variables = additional_variables(),
                                              scale_fill_brewer = scale_fill_brewer_conditions())
      
      # percentages
      percentages <- plot_percentages(df(),
                                      input_features = input_features(),
                                      additional_variables = additional_variables(),
                                      size_axis_text = input$size_axis_text)
      
      # SABGal+ and EdU+ percentages
      well_percentages <- well_percentages()
      
      # nuclear area distribution
      nuclear_area_distribution <- plot_morphological_feature_distribution(single_cell_df(),
                                                          morphological_feature = input$morph_data_input_feature,
                                                          additional_variables = additional_variables(),
                                                          scale_fill_brewer = scale_fill_brewer_conditions())
      
      # median nuclear area
      median_nuclear_area <- plot_median_nuclear_area(df(),
                                                      morphological_feature = input$morph_data_input_feature,
                                                      additional_variables = additional_variables(),
                                                      scale_fill_brewer = scale_fill_brewer_conditions())
      
      # fold change median SABGal
      
      # fold change median EdU
      
      # fold change median Nuclear Area
      
      # 3D plots  -----------------------------------------------------------
      
      # SABGal+ and EdU+ percentages plus median Nuclear Area
      
      # median fold changes
      
      # Comparison plots  -----------------------------------------------------------
      
      if (input$generate_comparison_graphs == TRUE) {
        # Cell counts
        
        # Cell count percentages
        
        # median staining comparison
        median_staining_comparison <- plot_median_staining_comparison(data = df(),
                                                                      input_features = input_features(),
                                                                      add_vars = additional_variables(),
                                                                      comparison_var = input$select_comparison_variable,
                                                                      other_add_var = other_add_var())
        
        # well percentages comparison
        well_percentages_comparison <- well_percentages_comparison()
        
      }
      
      # Return plot list  -----------------------------------------------------------
      list_plots <- list(single_cell_staining = single_cell_staining,
                         percentages = percentages,
                         median_staining = median_staining,
                         well_percentages = well_percentages,
                         nuclear_area_distribution = nuclear_area_distribution,
                         median_nuclear_area = median_nuclear_area)
      
      if (input$generate_comparison_graphs == TRUE) {
        list_plots_comparison <- list(median_staining_comparison = median_staining_comparison,
                                      well_percentages_comparison = well_percentages_comparison)
        
        list_plots <- c(list_plots, list_plots_comparison)
      }
      
      list_plots
    }) %>%
      bindEvent(input$generate_graphs)
    
    # All graphs handlers -----------------------------------------------------------
    shinyjs::hide("all_graphs") # hide by default
    
    observe({
      shinyjs::show("all_graphs") # show after clicking generate graphs
    }) %>%
      bindEvent(input$generate_graphs)
    
    observe({
      shinyjs::hide("all_graphs") # hide if analysis report or single cell data are changed
    }) %>%
      bindEvent(input$analysis_report_df$datapath, input$single_cell_data_df$datapath)
    
    # Download all graphs button ----------------------------------------------
    
    output$download_all_graphs <- downloadHandler(
      filename = function() {
        paste0("FAST.R_graphs_", Sys.Date(), ".zip")
      },
      
      content = function(file) {
        
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        
        # single cell SABGal EdU
        grDevices::png(file.path(temp_directory, "single_cell_staining.png"),
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$single_cell_staining)
        grDevices::dev.off()
        
        # percentages
        grDevices::png(file.path(temp_directory, "percentages.png"),
                       width = get_dim(dims_plot(), "width_percentages", "dpi_adj"),
                       height = get_dim(dims_plot(), "height_percentages", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$percentages)
        grDevices::dev.off()
        
        # median_staining
        grDevices::png(file.path(temp_directory, "median_staining.png"),
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$median_staining)
        grDevices::dev.off()
        
        # well percentages
        grDevices::png(file.path(temp_directory, "well_percentages.png"),
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$well_percentages)
        grDevices::dev.off()
        
        # nuclear area
        grDevices::png(file.path(temp_directory, "nuclear_area_distribution.png"),
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$nuclear_area_distribution)
        grDevices::dev.off()
        
        # median nuclear area
        grDevices::png(file.path(temp_directory, "median_nuclear_area.png"),
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$median_nuclear_area)
        grDevices::dev.off()
        
        # Comparison plots
        if (input$generate_comparison_graphs == TRUE) {
          
          # median SABGal EdU staining comparison
          grDevices::png(file.path(temp_directory, "median_staining_comparison.png"),
                         width = get_dim(dims_plot(), "width_comparison", "dpi_adj"),
                         height = get_dim(dims_plot(), "height_comparison", "dpi_adj"),
                         res = input$dpi)
          print(graphs()$median_staining_comparison)
          grDevices::dev.off()
          
          # well percentages comparison
          grDevices::png(file.path(temp_directory, "well_percentages_comparison.png"),
                         width = get_dim(dims_plot(), "width_comparison", "dpi_adj"),
                         height = get_dim(dims_plot(), "height_comparison", "dpi_adj"),
                         res = input$dpi)
          print(graphs()$well_percentages_comparison)
          grDevices::dev.off()
          
        } 
        
        
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
        
      },
      contentType = "application/zip"
    )
    
    # Render and download buttons for all graphs -----------------------------------------------------------
    
    # median_nuclear_area
    output$median_nuclear_area <- renderPlot({ # plot
      graphs()$median_nuclear_area
    },
    width = function() {get_dim(dims_plot(), "width", "72")},
    height = function() {get_dim(dims_plot(), "height", "72")},
    res = 72) %>%
      bindEvent(input$generate_graphs)
    
    output$download_median_nuclear_area <- downloadHandler( # download button
      filename = function() {
        paste0("median_nuclear_area", ".png")
      },
      content = function(file) {
        grDevices::png(file,
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$median_nuclear_area)
        grDevices::dev.off()
      }
    )
    
    # nuclear_area_distribution
    output$nuclear_area_distribution <- renderPlot({ # plot
      graphs()$nuclear_area_distribution
    },
    width = function() {get_dim(dims_plot(), "width", "72")},
    height = function() {get_dim(dims_plot(), "height", "72")},
    res = 72) %>%
      bindEvent(input$generate_graphs)
    
    output$download_nuclear_area_distribution <- downloadHandler( # download button
      filename = function() {
        paste0("nuclear_area_distribution", ".png")
      },
      content = function(file) {
        grDevices::png(file,
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$nuclear_area_distribution)
        grDevices::dev.off()
      }
    )
    
    # single cell stains
    output$single_cell_staining <- renderPlot({ # plot
      graphs()$single_cell_staining
    },
    width = function() {get_dim(dims_plot(), "width", "72")},
    height = function() {get_dim(dims_plot(), "height", "72")},
    res = 72) %>%
      bindEvent(input$generate_graphs)
    
    output$download_single_cell_staining <- downloadHandler( # download button
      filename = function() {
        paste0(Sys.Date(), "_single_cell_staining", ".png")
      },
      content = function(file) {
        grDevices::png(file,
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$single_cell_staining)
        grDevices::dev.off()
      }
    )
    
    # percentages
    output$percentages <- renderPlot({ # plot
      graphs()$percentages
    },
    width = function() {get_dim(dims_plot(), "width_percentages", "72")},
    height = function() {get_dim(dims_plot(), "height_percentages", "72")},
    res = 72) %>%
      bindEvent(input$generate_graphs)
    
    output$download_percentages <- downloadHandler( # download button
      filename = function() {
        paste0(Sys.Date(), "_percentages",  ".png")
      },
      content = function(file) {
        grDevices::png(file,
                       width = get_dim(dims_plot(), "width_percentages", "dpi_adj"),
                       height = get_dim(dims_plot(), "height_percentages", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$percentages)
        grDevices::dev.off()
      }
    )
    
    # median_staining
    output$median_staining <- renderPlot({ # plot
      graphs()$median_staining
    },
    width = function() {get_dim(dims_plot(), "width", "72")},
    height = function() {get_dim(dims_plot(), "height", "72")},
    res = 72) %>%
      bindEvent(input$generate_graphs)
    
    output$download_median_staining <- downloadHandler( # download button
      filename = function() {
        paste0(Sys.Date(), "_median_staining",  ".png")
      },
      content = function(file) {
        grDevices::png(file,
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$median_staining)
        grDevices::dev.off()
      }
    )
    
    # well percentages
    output$well_percentages <- renderPlot({ # plot
      graphs()$well_percentages
    },
    width = function() {get_dim(dims_plot(), "width", "72")},
    height = function() {get_dim(dims_plot(), "height", "72")},
    res = 72) %>%
      bindEvent(input$generate_graphs)
    
    output$download_well_percentages <- downloadHandler( # download
      filename = function() {
        paste0(Sys.Date(), "_well_percentages",  ".png")
      },
      content = function(file) {
        grDevices::png(file,
                       width = get_dim(dims_plot(), "width", "dpi_adj"),
                       height = get_dim(dims_plot(), "height", "dpi_adj"),
                       res = input$dpi)
        print(graphs()$well_percentages)
        grDevices::dev.off()
      }
    )
    
    # Comparison plots
    observe({
      if (input$generate_comparison_graphs == TRUE) {
        
        # median staining comparison
        output$median_staining_comparison <- renderPlot({ # plot
          graphs()$median_staining_comparison
        },
        width = function() {get_dim(dims_plot(), "width_comparison", "72")},
        height = function() {get_dim(dims_plot(), "height_comparison", "72")},
        res = 72
        )
        
        output$download_median_staining_comparison <- downloadHandler( # download
          filename = function() {
            paste0(Sys.Date(), "_median_staining_comparison",  ".png")
          },
          content = function(file) {
            grDevices::png(file,
                           width = get_dim(dims_plot(), "width_comparison", "dpi_adj"),
                           height = get_dim(dims_plot(), "height_comparison", "dpi_adj"),
                           res = input$dpi)
            print(graphs()$median_staining_comparison)
            grDevices::dev.off()
          }
        )
        
        # well percentages comparison
        output$well_percentages_comparison <- renderPlot({ # plot
          graphs()$well_percentages_comparison
        },
        width = function() {get_dim(dims_plot(), "width_comparison", "72")},
        height = function() {get_dim(dims_plot(), "height_comparison", "72")},
        res = 72
        )
        
        output$download_well_percentages_comparison <- downloadHandler( # download
          filename = function() {
            paste0(Sys.Date(), "_well_percentages_comparison",  ".png")
          },
          content = function(file) {
            grDevices::png(file,
                           width = get_dim(dims_plot(), "width_comparison", "dpi_adj"),
                           height = get_dim(dims_plot(), "height_comparison", "dpi_adj"),
                           res = input$dpi)
            print(graphs()$well_percentages_comparison)
            grDevices::dev.off()
          }
        )
        
      }
    }) %>%
      bindEvent(input$generate_comparison_graphs)
    
    # Comparison graphs handler -----------------------------------------------------------
    shinyjs::hide("comparison_graphs") # hide by default
    
    observe({
      if (input$generate_comparison_graphs == TRUE) {
        shinyjs::show("comparison_graphs") # show if generate_comparison_graphs is checked
      } else {
        shinyjs::hide("comparison_graphs") # hide if generate_comparison_graphs is unchecked
      }
    }) %>%
      bindEvent(input$generate_graphs)
    
  })
  
}