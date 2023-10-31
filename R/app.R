
#' FASTR
#'
#' @import shiny
#' @import magrittr
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
FASTR <- function(...) { # app needs to be wrapped in function to be used as package
  
  template_url <- "https://raw.githubusercontent.com/f-neri/FASTR/main/plate-metadata.tsv"
  
# UI ----------------------------------------------------------------------

ui <- fluidPage(
  shinyjs::useShinyjs(), # enables shinyjs functions
  
  titlePanel(
    h1(strong("FAST-R"), align = "center")
    ),
  
  # Title + image
  fluidRow(
    column(12,
           h1(a(href = "link", "FAST"), " Data Analysis & Visualization App", align = "center"),
           br(),
           br()
           )
  ),
  
  # Data analysis header
  fluidRow(
    column(12,
           br(),
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
    column(3, offset = 3,
           fileInput("Image_Analyst_output", label = "Image Analyst output file", multiple = TRUE, accept = ".xlsx")
           ),
    column(5, offset = 0,
           br(),
           p("Updaload the ", strong("Image Analyst output file"), " containing your single-cell measurements")
           )
  ),
  
  # download metadata template
  fluidRow(
    column(3, offset = 3,
           br(),
           br(),
           downloadButton("download_metadata", label = "Plate metadata template"),
           br(),
           br()
           ),
    column(5, offset = 0,
           p("Download the ", strong("plate metadata template *.tsv file"), ", and modify it appropriately to add your metadata:"),
           p("○ In the 1st plate template (\"Condition\"), ", strong("add labels")," to distinguish your different experimental conditions
           (e.g. IR & Mock IR). Then, ", strong("append the tag \"_background\"")," to denote your Background wells for each condition
             (e.g. IR_background & Mock IR_background)."),
           p("○ ", em("Optional"),": labels for up to two additional variables can be entered in the two additional plate
             templates (e.g., different culturing media or varying concentrations of a drug treatment, etc.). Replace the
             placeholder names (\"Variable1\" and \"Variable2\") with your actual variable names (e.g. \"Medium\" or \"DrugA (nM)\")."),
           br()
           )
  ),
  
  # updload adjusted metadata
  fluidRow(
    column(3, offset = 3,
           fileInput("plate_metadata", label = "Adjusted metadata file", multiple = TRUE,  accept = ".tsv"),
    ),
    column(5, offset = 0,
           br(),
           p("Updaload the ", strong("adjusted metadata file"))
    )
  ),
  
  # run analysis button
  fluidRow(
    column(12, align = "center",
           actionButton("button_analysis", label = "Run Analysis")
           )
  ),
  
  # show analysis messages
  fluidRow(
    column(12, align = "center",
           br(),
           textOutput("analysis_message")
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
    
  
# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  
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
  
  output$download_metadata <- downloadHandler(
    filename = function() if (length(input$Image_Analyst_output$name) == 1) { # single IAoutput and metadata files
      template_names()
      
    } else { # multiple IAoutput and metadata files
      
      paste0("plate_metadata_templates_", Sys.Date(), ".zip")
    },
    
    content = function(file) if (length(input$Image_Analyst_output$name) == 1) { # single IAoutput and metadata files
      download.file(template_url, destfile = file, method = "auto")
      message("Downloaded plate metadata template: ", template_names(),"\n")
      
    } else { # multiple IAoutput and metadata files
      
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      file_paths <- vector("character", length = length(input$Image_Analyst_output$name))
      
      for (i in seq_along(input$Image_Analyst_output$name)) {
        file_paths[i] <- file.path(temp_directory, template_names()[i])
        suppressMessages(
          download.file(template_url, destfile = file_paths[i], method = "auto")
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
  
  ## tidy IAoutput and merge with metadata
  tidied_IAoutput <- reactive({
    notification <- showNotification("Analysing data...", duration = NULL, closeButton = FALSE) # shows a permanent notification
    on.exit(removeNotification(notification), add = TRUE)
    
    Sys.sleep(2) # pretend this is long calculation
    
    if (length(input$Image_Analyst_output$name) > 1) {
      validate(
        "test error tidied_IAoutput"
      )
    }
  }) %>%
    bindCache(input$Image_Analyst_output$datapath, input$plate_metadata$datapath) %>%
    bindEvent(input$button_analysis)
  
  ## perform calculations and generate analysis_report table
  analysis_report <- reactive({
    
    tidied_IAoutput() # start point for analysis report table
    
    Sys.sleep(1) # pretend this is long calculation
    
    if (length(input$Image_Analyst_output$name) < 1) {
      validate(
        "test error analysis_report"
      )
    }
    
  }) %>%
    bindEvent(input$button_analysis)
  
  
  
  # Print data analysis message
  output$analysis_message <- renderText({
    tidied_IAoutput()
    analysis_report()
    c("Data analysis was successful!!")
    }) %>%
    bindEvent(input$button_analysis)
}

shinyApp(ui, server) # , options = list(launch.browser = TRUE) -> browser lunch

}