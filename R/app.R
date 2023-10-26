
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
  
# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel(
    h1(strong("FAST-R"), align = "center")
    ),
  
  # Title + image
  fluidRow(
    column(12,
           h1(a(href = "link", "FAST"), " Data Analysis & Visualization App", align = "center"),
           br(),
           br(),
           img(src = "placeholder.png", style = "display: block; margin-left: auto; margin-right: auto;") # style is for centering
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
           fileInput("Image_Analyst_output", label = "Upload Image Analyst output file", multiple = TRUE, accept = ".xlsx")
           ),
    column(5, offset = 0,
           br(),
           p("Updaload the ", strong("Image Analyst output file"))
           )
  ),
  
  # download metadata template
  fluidRow(
    column(3, offset = 3,
           br(),
           br(),
           downloadButton("download_metadata", label = "Download metadata plate template"),
           br(),
           br()
           ),
    column(5, offset = 0,
           p("Download the ", strong("metadata plate template *.tsv file"), ", and modify it appropriately to add your metadata:"),
           p("○ In the 1st plate template (\"Condition\"), ", strong("add labels")," to distinguish senescence-inducing and control
             conditions (e.g. IR & CTL). Then, ", strong("append the tag \"_background\"")," to denote your Background wells for each condition
             (e.g. IR_background & CTL_background)."),
           p("○ ", em("Optional"),": if your experiment involves up to two additional variables, label them in the two additional plate
             templates (e.g., different culturing conditions or varying concentrations of a drug treatment, etc.). Replace the
             placeholder names (\"Variable1\" and \"Variable2\") with your actual variable names."),
           br()
           )
  ),
  
  # updload adjusted metadata
  fluidRow(
    column(3, offset = 3,
           fileInput("plate_template_name", label = "Upload adjusted metadata file", multiple = TRUE,  accept = ".tsv"),
    ),
    column(5, offset = 0,
           br(),
           p("Updaload the ", strong("adjusted metadata file"))
    )
  ),
  
  # run analysis button
  fluidRow(
    column(12, align = "center",
           actionButton("run_button", label = "Run Analysis")
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
    message("Image Analyst output file: ", input$Image_Analyst_output$name, "; length: ", length(input$Image_Analyst_output$name))
  })
  
  # metadata plate template names
  plate_template_names <- reactive({
    
  })
  
  # download metadata template 
  output$download_metadata <- downloadHandler(
    filename = function() {
      name <- if (is.null(input$Image_Analyst_output$name)) {
        "metadata-plate-template"
      } else {
        input$Image_Analyst_output$name %>%
          str_replace(., pattern = ".xlsx", replacement = "_metadata")
      }
      
      paste0(name, ".tsv")
    },
    content = function(file) {
      url <- "https://raw.githubusercontent.com/f-neri/FAST-R_2.0/main/plate-metadata.tsv"
      
      download.file(url, destfile = file, method = "auto")
    }
  )
  
  # observe: input adjusted metadata
  observeEvent(input$plate_template_name, {
    message("adjusted metadata file: ", input$plate_template_name$name)
  })
  
}

shinyApp(ui, server, options = list(launch.browser = TRUE))

}