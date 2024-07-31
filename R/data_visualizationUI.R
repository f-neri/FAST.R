data_visualizationUI <- function(id) {
  
  tagList(
    
    # Data visualization header -----------------------------------------------
    
    fluidRow(
      column(12,
             br(),
             h2("Data Visualization", align = "center"),
             br()
      )
    ),
    
    fluidRow(
      style = "background-color: #D6EBF2",
      fluidRow(
        column(3, offset = 3,
               h3("Input", align = "center")
        ),
        column(3, offset = 0,
               h3("Instructions", align = "center")
        ),
        column(12,
               br(),
               br())
      ),
      
      # upload single-cell and analysis report data -----------------------------
      
      fluidRow(
        column(3, offset = 3, align = "center",
               br(),
               fileInput(NS(id, "single_cell_data_df"), label = "Single Cell Data", multiple = FALSE, accept = ".csv"),
               fileInput(NS(id, "analysis_report_df"), label = "Analysis Report", multiple = FALSE, accept = ".csv"),
               # 07/21
               selectizeInput(
                 inputId = NS(id, "morph_data_input_feature"),
                 label = "Morphological Data Features",
                 choices = c("Nuclear_Area"),  # Add all possible stain features here
                 selected = c("Nuclear_Area"),  # Initial selected options
                 multiple = TRUE,
                 options = list(placeholder = 'Select morphological features in the dataset',
                                plugins = list('remove_button', 'drag_drop'))
               ),
               selectizeInput(
                 inputId = NS(id, "data_input_feature"),
                 label = "Stains",
                 choices = c("DAPI", "EdU", "SABGal"),  # Add all possible stain features here
                 selected = c("DAPI", "EdU", "SABGal"),  # Initial selected options
                 multiple = TRUE,
                 options = list(placeholder = 'Select features in the dataset',
                                plugins = list('remove_button', 'drag_drop'))
               )
        ),
        column(5, offset = 0,
               br(), br(), br(), br(), br(),
               p("Upload the ", strong("Single Cell Data"), " and ", strong("Analysis Report"), " files
               generated via the FAST-R Data Analysis tab")
        ),
        column(12, offset = 0, align = "center",
               actionButton(NS(id, "next_button"), label = "Next"),
               br(), br())
      ),
      
    ),
    
    # Upload error messages ---------------------------------------------------
    fluidRow(
      column(12, align = "center",
             br(),
             textOutput(NS(id, "upload_message")),
             br()
      )
    ),
    
    # Graphs control widgets -----------
    div(id = NS(id)("graphs_control_widgets"),
        fluidRow(
          style = "background-color: #D6EBF2",
          
          # Example graph text
          column(10, offset = 1, align = "center",
                 br(),
                 p("Adjust ", strong("plot settings"), " and click the ", strong("\"Generate Graphs\""), " button"),
                 br()
          ),
          
          # Palette
          column(2, offset = 1, align = "center",
                 wellPanel(
                   fluidRow(
                     column(12, offset = 0, align = "center",
                            strong("Palette"),
                            br(), br(),
                     )
                   ),
                   fluidRow(
                     column(6, offset = 0, align = "center",
                            selectInput(NS(id, "palette"),
                                        label = NULL,
                                        choices = palette_names,
                                        selected = "Dark2")
                     ),
                     column(6, offset = 0, align = "center",
                            checkboxInput(NS(id, "reverse"), label = "Reverse", value = FALSE)
                     )
                   ),
                   fluidRow(
                     column(12, offset = 0, align = "center",
                            helpText("Check out available palettes at"),
                            helpText("https://r-graph-gallery.com/38-rcolorbrewers-palettes.html")
                     )
                   )
                 )
          ),
          
          # Font sizes
          column(4, offset = 0, align = "center",
                 wellPanel(
                   fluidRow(
                     column(12, offset = 0, align = "center",
                            strong("Font Size"),
                            br(), br()
                     )
                   ),
                   fluidRow(
                     column(4, offset = 0, align = "center",
                            numericInput(NS(id, "legend_title"),
                                         label = "Legend Title",
                                         value = 14,
                                         step = 1),
                            numericInput(NS(id, "legend_text"),
                                         label = "Legend Text",
                                         value = 12,
                                         step = 1)
                     ),
                     column(4, offset = 0, align = "center",
                            numericInput(NS(id, "size_axis_title"),
                                         label = "Axis Title",
                                         value = 14,
                                         step = 1),
                            numericInput(NS(id, "size_axis_text"),
                                         label = "Axis Text",
                                         value = 12,
                                         step = 1)
                     ),
                     column(4, offset = 0, align = "center",
                            br(), br(),
                            numericInput(NS(id, "size_facets_text"),
                                         label = "Facet Text",
                                         value = 14,
                                         step = 1)
                     ),
                   ),
                 )),
          
          # Background cells/wells
          column(2, offset = 0, align = "center",
                 wellPanel(
                   fluidRow(
                     column(12, offset = 0, align = "center",
                            strong("Background Cells/Wells"),
                            br(), br(), br(), br()
                     )
                   ),
                   
                   fluidRow(
                     column(12, offset = 0, align = "center",
                            checkboxInput(NS(id, "remove_background"), label = "Remove", value = FALSE),
                            br(), br(), br()
                     )
                   ),
                   
                 )),
          
          # additional graphs
          column(2, offset = 0, align = "center",
                 div(id = NS(id)("comparison_graphs_panel"),
                     wellPanel(
                       fluidRow(
                         column(12, offset = 0, align = "center",
                                strong("Treatment Comparison Graphs"),
                                br(), br(),
                         )
                       ),
                       
                       fluidRow(
                         column(12, offset = 0, align = "center",
                                checkboxInput(NS(id, "generate_comparison_graphs"), label = "Generate", value = FALSE),
                                br()
                         )
                       ),
                       
                       fluidRow(
                         column(12, offset = 0, align = "center",
                                selectInput(NS(id, "select_comparison_variable"),
                                            label = "Additional Variable",
                                            choices = c("NA"),
                                            selected = "NA")
                         )
                       ),
                       
                     )
                 )
          ),
          # Generate graphs button -----------
          column(10, offset = 1, align = "center",
                 actionButton(NS(id, "generate_graphs"), label = "Generate Graphs"),
                 br(), br()
          )
          
          # graph resolution for download
          
        )
        ),
    
    # Example graphs ----------- 
    div(id = NS(id)("example_graphs"),
        fluidRow(
          column(12, offset = 0, align = "center",
                 br(),br(),
                 plotOutput(NS(id, "example_graph")),
                 plotOutput(NS(id, "example_graph_comparison"))
          )
        )),
    
    # All graphs ----------- 
    div(id = NS(id)("all_graphs"),
        fluidRow(
          # resolution widget and download all button
          column(12, offset = 0, align = "center",
                 style = "background-color: #D6EBF2",
                 br(),
                 column(3, offset = 3, align = "center",
                        selectInput(NS(id, "dpi"),
                                    label = "Graph resolution (dpi)",
                                    choices = c(72, 150, 300, 600),
                                    selected = 150,
                                    width = "180px"),
                        helpText("72: low; 150: medium; 300: high; 600: ultra high"),
                 ),
                 column(3, offset = 0, align = "center",
                        br(),
                        downloadButton(NS(id, "download_all_graphs"), label = "Download all graphs")
                 )
          ),
          
          # graphs and individual download buttons
          column(12, offset = 0, align = "center",
                 br(),
                 # single cell SABGal EdU staining
                 plotOutput(NS(id, "single_cell_SABGal_EdU_staining")),
                 downloadButton(NS(id, "download_single_cell_SABGal_EdU_staining"), label = "Download graph above"),
                 br(), hr(),
                 
                 # percentages
                 plotOutput(NS(id, "percentages")),
                 downloadButton(NS(id, "download_percentages"), label = "Download graph above"),
                 br(), hr(),
                 
                 # median SABGal EdU staining
                 plotOutput(NS(id, "median_SABGal_EdU_staining")),
                 downloadButton(NS(id, "download_median_SABGal_EdU_staining"), label = "Download graph above"),
                 br(), hr(),
                 
                 # well percentages
                 plotOutput(NS(id, "well_percentages")),
                 downloadButton(NS(id, "download_well_percentages"), label = "Download graph above"),
                 br(), hr(),
          ),
          div(id = NS(id)("comparison_graphs"),
              column(12, offset = 0, align = "center",
                     # median SABGal EdU staining Comparison
                     plotOutput(NS(id, "median_SABGal_EdU_staining_comparison")),
                     downloadButton(NS(id, "download_median_SABGal_EdU_staining_comparison"), label = "Download graph above"),
                     br(), hr(),
                     
                     # well percentages comparison
                     plotOutput(NS(id, "well_percentages_comparison")),
                     downloadButton(NS(id, "download_well_percentages_comparison"), label = "Download graph above"),
                     br(), hr(),
              ))
          
        )
        ),
    
    
  )
}