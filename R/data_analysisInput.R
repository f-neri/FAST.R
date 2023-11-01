data_analysisInput <- function(id) {
  
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
             fileInput(NS(id, "Image_Analyst_output"), label = "Image Analyst output file", multiple = TRUE, accept = ".xlsx")
      ),
      column(5, offset = 0,
             br(),
             p("Updaload the ", strong("Image Analyst output file"), " containing your single-cell measurements")
      )
    ),
    
    # download metadata template
    fluidRow(
      column(3, offset = 3, align = "center",
             br(),
             br(),
             downloadButton(NS(id, "download_metadata"), label = "Plate metadata template"),
             br(),
             br()
      ),
      column(5, offset = 0,
             p("Download the ", strong("plate metadata template *.tsv file"), ", and modify it appropriately to add your metadata:"),
             p("- In the 1st plate template (\"Condition\"), ", strong("add labels")," to distinguish your different experimental conditions
           (e.g. IR & Mock IR). Then, ", strong("append the tag \"_background\"")," to denote your Background wells for each condition
             (e.g. IR_background & Mock IR_background)."),
             p("- ", em("Optional"),": labels for up to two additional variables can be entered in the two additional plate
             templates (e.g., different culturing media or varying concentrations of a drug treatment, etc.). Replace the
             placeholder names (\"Variable1\" and \"Variable2\") with your actual variable names (e.g. \"Medium\" or \"DrugA (nM)\")."),
             br()
      )
    ),
    
    # updload adjusted metadata
    fluidRow(
      column(3, offset = 3, align = "center",
             fileInput(NS(id, "plate_metadata"), label = "Adjusted metadata file",
                       multiple = TRUE,  accept = ".tsv"),
      ),
      column(5, offset = 0,
             br(),
             p("Updaload the ", strong("adjusted metadata file")),
             br()
      )
    ),
    
    # adjust background_threhsold percentile value
    fluidRow(
      column(3, offset = 3, align = "center",
             numericInput(NS(id, "background_threshold"), label = "Background threshold",
                          value = 0.95, min = 0.9, max = 1.0, step = 0.01, width = "150px"),
             helpText("Recommended value: 0.95")
      ),
      column(5, offset = 0,
             br(),
             p(em("Optional"), "Adjust the ", strong("Background threshold"), ".
           This is the percentile value to be used when calculating the staining
             threshold in Background wells"),
             br()
      )
    ),
    
    # run analysis button
    fluidRow(
      column(12, align = "center",
             br(),
             br(),
             actionButton(NS(id, "button_analysis"), label = "Run Analysis")
      )
    ),
    
    # show analysis messages
    fluidRow(
      column(12, align = "center",
             br(),
             textOutput(NS(id, "analysis_message"))
      )
    ),
    
    # show tidied single cell & analysis report tables
    fluidRow(
      column(4, offset = 1,
             # add single cell table
      ),
      column(6,
             # add analysis report
      )
    )
  )
  
  
}