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
      wellPanel(
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
                 br()
          ),
          column(5, offset = 0,
                 br(), br(), br(), br(), br(),
                 p("Upload the ", strong("Single Cell Data"), " and ", strong("Analysis Report"), " files
               generated via the FAST-R Data Analysis tab")
          )
        )
      )),
    
    # Upload error messages ---------------------------------------------------
    fluidRow(
      column(12, align = "center",
             br(),
             textOutput(NS(id, "upload_message")),
             br()
      )
    ),
    
    # Example graph text -----------
    
    fluidRow(
      column(10, offset = 1, align = "center",
             br(),
             uiOutput(NS(id, "example_graph_text")),
             br(),br(),
             )
      ),
    
    # Graph control widgets -----------
    fluidRow(
      
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
    
    # background cells/wells
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
           wellPanel(
             fluidRow(
               column(12, offset = 0, align = "center",
                      strong("Generate Comparison Graphs?"),
                      br(), br(), br(), br()
               )
             ),
             
             fluidRow(
               column(12, offset = 0, align = "center",
                      checkboxInput(NS(id, "additional_graphs"), label = "Generate", value = FALSE),
                      br(), br(), br()
               )
             ),
             
           ))
    
    # downloaded graph resolution
    
    ),
    
    # Example graph ----------- 
    fluidRow(
      column(12, offset = 0, align = "center",
             plotOutput(NS(id, "example_graph")))
    )
    
  )
}