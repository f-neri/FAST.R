palette_names <- rownames(RColorBrewer::brewer.pal.info) 

data_visualizationServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # Read and check single-cell and analysis report data -------------------------------
    
    # read single-cell data
    single_cell_data_df <- reactive({
      read.csv(file = input$single_cell_data_df$datapath)
    }) %>%
      bindCache(input$single_cell_data_df$datapath) %>%
      bindEvent(input$single_cell_data_df)
    
    # check sinlge-cell data format
    ## TO ADD
    
    # read analysis report data
    analysis_report_df <- reactive({
      analysis_report_df_df <- read.csv(file = input$analysis_report_df$datapath)
    }) %>%
      bindCache(input$analysis_report_df$datapath) %>%
      bindEvent(input$analysis_report_df)
    
    # check analysis report data format
    ## TO ADD
    
    # check sinlge-cell file matches analysis report file
    ## TO ADD
    
    # show upload error messages ----------------------------------------------
    output$upload_message <- renderText({
      # return empty text if all good
      ""
    }) %>%
      bindEvent(input$single_cell_data_df,
                input$analysis_report_df)
    
    
    # Identify additional variables -------------------------------------------
    
    additional_variables <- reactive({
      cell_counts_column <- grep("cell_counts", names(analysis_report_df())) # get column position of cell_counts
      
      add_vars <- names(analysis_report_df())[-c(cell_counts_column:length(names(analysis_report_df())))] # remove all columns from cell_counts_on
      
      add_vars <- add_vars[-c(1:3)] # remove first 3 columns (plate, well, Condition)
      
      add_vars
    }) %>%
      bindCache(input$analysis_report_df$datapath) %>%
      bindEvent(input$analysis_report_df)
    
    # Example graph widgets  ------------------------------------------------------
    
    # show text w/ instructions
    output$example_graph_text <- renderUI({
      HTML("Adjust the <strong>plot settings</strong> below as desired")
    })
    
    # show selected palette --- ISSUES --- works, but creates huge blank gap below the plot
                                          # seems due to default dimensions assigned for plots by R shiny 
    output$palettePlot <- renderPlot({
      # Get the selected palette
      selected_palette <- input$palette
      
      # Set smaller margins to avoid "figure margins too large" error
      # The 'mar' parameter specifies the margins in the order (bottom, left, top, right)
      par(mar = c(0, 0, 0, 0))
      
      # Generate a plot showing the colors of the selected palette
      colors_to_show <- RColorBrewer::brewer.pal(n = RColorBrewer::brewer.pal.info[selected_palette, "maxcolors"], name = selected_palette)
      barplot(rep(1, length(colors_to_show)), 
              col = colors_to_show, 
              border = NA, 
              axes = FALSE, 
              xaxt = 'n', 
              yaxt = 'n')
    }, height = 40, width = 100) # Set the height of the output plot to 150px
    
    # set scale fill and colors based on Conditions
    scale_fill_brewer_conditions <- reactive({
      ggplot2::scale_fill_brewer(palette = input$palette,
                        limits = sort(unique(analysis_report_df()$Condition)),
                        direction = ifelse(input$reverse == FALSE, 1, -1))
    })
    
    # Themes ggplot2  ---------------------------------------------------------
    
    ggplot2::theme_set(ggplot2::theme_bw())
    
    observe({
      ggplot2::theme_update(
        legend.title = ggplot2::element_text(size = input$legend_title),
        legend.text = ggplot2::element_text(size = input$legend_text),
        axis.title = element_text(size = input$size_axis_title),
        axis.text = element_text(size = input$size_axis_text),
        strip.text = element_text(size = input$size_facets_text)
      )
    })
    
    # Plot example graphs -----------------------------------------------------
    
    well_percentages <- reactive({
      plot_well_percentages(data = analysis_report_df(),
                            add_vars = additional_variables(),
                            scale_fill_brewer = scale_fill_brewer_conditions())
    })
    
    output$example_graph <- renderPlot({
      well_percentages() +
        theme(
          legend.title = ggplot2::element_text(size = input$legend_title),
          legend.text = ggplot2::element_text(size = input$legend_text),
          axis.title = element_text(size = input$size_axis_title),
          axis.text = element_text(size = input$size_axis_text),
          strip.text = element_text(size = input$size_facets_text)
        )
    }, res = 96) %>% # res = 96 will make the Shiny plots match what you see in RStudio as closely as possible
      bindCache(
        input$analysis_report_df,
        input$palette,
        input$reverse,
        input$legend_title,
        input$legend_text,
        input$size_axis_title,
        input$size_axis_text,
        input$size_facets_text
      )
    
    # Print message -----------------------------------------------------------
    
    observe({
      print("")
    }) %>% bindEvent(input$analysis_report_df)
    
  })
  
}