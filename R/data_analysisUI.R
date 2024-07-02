data_analysisUI <- function(id) {
  
  tagList(
    # Data analysis tab title
    fluidRow(
      column(12,
             br(),
             h2("Data Analysis", align = "center"),
             br()
      )
    ),
    
    # Input and Instructions col headers
    fluidRow(
      style = "background-color: #D6EBF2;",
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
      
      # upload IAoutput
      fluidRow(
        # Input
        column(3, offset = 3, align = "center",
               fileInput(NS(id, "Image_Analyst_output"), label = "Image Analyst Output File", multiple = TRUE, accept = ".xlsx")
        ),
        # Instructions
        column(5, offset = 0,
               br(),
               p("Upload the ", strong("Image Analyst Output File"), " containing your single-cell measurements.")
        )
      ),
      
      # download metadata template
      fluidRow(
        # Input
        column(3, offset = 3, align = "center",
               # manually added line brakes to have download button ~ in the middle of instructions block
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               downloadButton(NS(id, "download_metadata"), label = "Metadata Template")
        ),
        # Instructions
        column(5, offset = 0,
               p("Download the ", strong("Metadata Template.csv file"), ", and ", strong("modify it"), " appropriately to include your metadata:"),
               tags$ul(
                 tags$li("In the first microplate template (\"Condition\"), ", strong("add labels"), " to differentiate your experimental conditions (e.g., IR & Mock IR). Then, ", strong("append the tag \"_background\""), " to denote your Background wells for each condition (e.g., IR_background & Mock IR_background)."),
                 tags$li(em("Optional"), ": Labels for up to two additional variables can be added in the remaining microplate templates (e.g., different culturing media or varying concentrations of a drug treatment). Replace the placeholder names (\"Variable1\" and \"Variable2\") with your actual variable names (e.g., \"Medium\" or \"DrugA (nM)\")."),
                 tags$li(em("Optional"), ": A machine learning (ML) approach to classify cells as senescent or non-senescent can be added to the analysis. To do so, modify the ", em("ML_Training"), " template to specify which wells to be used for training:",
                         tags$ul(
                           tags$li("label with ", strong("\"+\""), " a few positive samples (i.e. wells where you are confident the great majority / >90% of cells are senescent);"),
                           tags$li("label with ", strong("\"-\""), " a few negative samples (i.e. wells where you are confident the great majority / >90% of cells are NOT senescent);")
                         ),
                         tags$div(style = "margin-left: 40px;", 
                                  p("The ML model will classify cells in all non-training wells as being senescent or not"),
                                  p(strong("NOTE"), ": this additional ML analysis requires a ", em("significantly longer"), " time to complete (from a few seconds per plate without ML, to a few minutes per plate with ML).")
                         )
                 )
               ),
               br()
        )
      ),
      
      # upload adjusted metadata
      fluidRow(
        # Input
        column(3, offset = 3, align = "center",
               fileInput(NS(id, "plate_metadata"), label = "Adjusted Metadata File",
                         multiple = TRUE,  accept = ".csv"),
        ),
        # Instructions
        column(5, offset = 0,
               br(),
               br(),
               p("Upload the ", strong("Adjusted Metadata File"), "."),
               br()
        )
      ),
      
      # adjust background_threshold percentile value
      fluidRow(
        # Input
        column(3, offset = 3, align = "center",
               numericInput(NS(id, "background_threshold"), label = "Background Threshold",
                            value = 0.95, min = 0.9, max = 1.0, step = 0.01, width = "180px"),
               helpText("Recommended value: 0.95")
        ),
        # Instructions
        column(5, offset = 0,
               br(),
               p(em("Optional"), ": Adjust the ", strong("Background Threshold"), ".
           This is the percentile value (0.90-1.00) used to calculate staining
             thresholds from Background wells."),
               br()
        )
      ),
      
      # run analysis button
      fluidRow(
        column(12, align = "center",
               br(), br(),
               actionButton(NS(id, "button_analysis"), label = "Run Analysis", icon = icon("rocket")),
               br(), br()
        )
      )
    ),
    
    # show upload/data analysis error messages
    fluidRow(
      column(12, align = "center",
             br(),
             textOutput(NS(id, "analysis_report_message")),
             br(),
      )
    ),
    
    # show tidied single cell & analysis report tables
    div(id = NS(id)("sc_and_analysis_report_panel"),
        fluidRow(
          column(10, offset = 1, align = "center",
                 br(), hr(),
                 div(textOutput(NS(id, "df_single_cell_title")),
                     style = "font-size: 16pt"),
                 br(),
                 DT::dataTableOutput(NS(id, "df_single_cell"))
                 ),
          column(10, offset = 1, align = "center",
                 downloadButton(NS(id, "download_sc_data"), label = "Download Single-Cell Data")
                 ),
          column(10, offset = 1, align = "center",
                 br(), hr(),
                 div(textOutput(NS(id, "analysis_report_title")),
                     style = "font-size: 16pt"),
                 br(),
                 DT::dataTableOutput(NS(id, "df_analysis_report"))
                 ),
          column(10, offset = 1, align = "center",
                 downloadButton(NS(id, "download_analysis_report"), label = "Download Analysis Report"),
                 br(), br(), br()
                 )
        ))
  )
}