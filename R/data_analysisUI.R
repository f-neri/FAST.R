data_analysisUI <- function(id) {
  
  tagList(
    # Data analysis header
    fluidRow(
      column(12,
             br(),
             h2("Data Analysis", align = "center"),
             br()
      )
    ),
    
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
        column(3, offset = 3, align = "center",
               fileInput(NS(id, "Image_Analyst_output"), label = "Image Analyst Output File", multiple = TRUE, accept = ".xlsx")
        ),
        column(5, offset = 0,
               br(),
               p("Upload the ", strong("Image Analyst Output File"), " containing your single-cell measurements.")
        )
      ),
      
      # download metadata template
      fluidRow(
        column(3, offset = 3, align = "center",
               br(),
               br(),
               downloadButton(NS(id, "download_metadata"), label = "Plate Metadata Template"),
               br(),
               br()
        ),
        column(5, offset = 0,
               p("Download the ", strong("Plate Metadata Template.csv file"), ", and modify it appropriately to include your metadata:"),
               p("- In the first plate template (\"Condition\"), ", strong("add labels")," to differentiate your experimental conditions
           (e.g., IR & Mock IR). Then, ", strong("append the tag \"_background\"")," to denote your Background wells for each condition
             (e.g., IR_background & Mock IR_background)."),
               p("- ", em("Optional"), ": Labels for up to two additional variables can be added in the remaining plate
             templates (e.g., different culturing media or varying concentrations of a drug treatment). Replace the
             placeholder names (\"Variable1\" and \"Variable2\") with your actual variable names (e.g., \"Medium\" or \"DrugA (nM)\")."),
               br()
        )
      ),
      
      # upload adjusted metadata
      fluidRow(
        column(3, offset = 3, align = "center",
               fileInput(NS(id, "plate_metadata"), label = "Adjusted Metadata File",
                         multiple = TRUE,  accept = ".csv"),
        ),
        column(5, offset = 0,
               br(),
               br(),
               p("Upload the ", strong("adjusted metadata file"), "."),
               br()
        )
      ),
      
      # adjust background_threshold percentile value
      fluidRow(
        column(3, offset = 3, align = "center",
               numericInput(NS(id, "background_threshold"), label = "Background Threshold",
                            value = 0.95, min = 0.9, max = 1.0, step = 0.01, width = "180px"),
               helpText("Recommended value: 0.95")
        ),
        column(5, offset = 0,
               br(),
               p(em("Optional"), ": Adjust the ", strong("Background Threshold"), ".
           This is the percentile value (0.90-1.00) used to calculate the staining
             threshold from Background wells."),
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
             textOutput(NS(id, "analysis_report_message"))
      )
    ),
    
    # show tidied single cell & analysis report tables
    fluidRow(
      column(10, offset = 1, align = "center",
             br(), br(), br(),
             div(textOutput(NS(id, "df_single_cell_title")),
                 style = "font-size: 16pt"),
             br(),
             DT::dataTableOutput(NS(id, "df_single_cell"))
      ),
      div(id = NS(id)("download_sc_data_panel"), # NS call of entire row needed to hide download button
          column(10, offset = 1, align = "center",
                 downloadButton(NS(id, "download_sc_data"), label = "Download Single-Cell Data"))
          ),
      column(10, offset = 1, align = "center",
             br(), br(), br(),
             div(textOutput(NS(id, "analysis_report_title")),
                 style = "font-size: 16pt"),
             br(),
             DT::dataTableOutput(NS(id, "df_analysis_report"))
      ),
      div(id = NS(id)("download_analysis_report_panel"), # NS call of entire row needed to hide download button
          column(10, offset = 1, align = "center",
                 downloadButton(NS(id, "download_analysis_report"), label = "Download Analysis Report"))
      )
    )
    
  )
}