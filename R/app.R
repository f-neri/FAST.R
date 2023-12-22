
#' FAST.R: Analyze and Visualize FAST-Generated Data
#'
#' R shiny app to perform data analysis and visualization for the
#' Fully Automated Senescence Test (FAST) workflow.
#'
#' @param Browser Logical. If TRUE, FAST.R will open in a browser window
#'
#' @import shiny
#' @import ggplot2
#' @import magrittr
#' @importFrom RColorBrewer brewer.pal.info
#' @return Launches an R shiny app to analyze and visualize data generated with the FAST workflow
#' @export
#'
#' @examples
#' if(interactive()){FAST.R()}

FAST.R <- function(Browser = FALSE) { # app needs to be wrapped in function to be used as package
  
  . <- NULL # workaround to prevent an R CMD note (see https://github.com/Rdatatable/data.table/issues/5436)
  
  # check arguments
  stopifnot(is.logical(Browser))
  
  # Set file upload size limit to 20 MB
  old_options <- options()
  on.exit(options(old_options)) # ensures user options are restored upon exit
  options(shiny.maxRequestSize=20*1024^2)
  
  # UI ----------------------------------------------------------------------
  
  ui <- fluidPage(
    shinyjs::useShinyjs(), # enables shinyjs functions
    waiter::useWaiter(), # enables waiter functions
    shinyFeedback::useShinyFeedback(), # enables ShinyFeedback
    
    waiter::autoWaiter(color = "white",
                       html = tagList(br(),
                                      waiter::spin_hexdots(),
                                      tagAppendAttributes(style = "color:black",
                                                          div(
                                                            br(),br(),
                                                            p("Loading ...")
                                                          ))
                                      )
                       ),
    
    tags$head( # changes CSS style of validate() text
      tags$style(HTML("
        .shiny-output-error-validation {
          color: #ff0000;
          font-weight: bold;
        }
      "))
    ),
    
    titlePanel(
      h1(strong("FAST-R"), align = "center")
      ),
    
    # Title + paper link
    fluidRow(
      column(12,
             h1("FAST Data Analysis & Visualization App", align = "center"), # TO ADD a(href = "link", "FAST")
             br()
             )
    ),
    
    ui <- fluidPage(
      
      tabsetPanel(
        tabPanel("Data Analysis",
                 data_analysisUI("data_analysis")),
        
        tabPanel("Data Visualization",
                 data_visualizationUI("data_visualization"))
      )
    )
  )
      
    
  # Server ------------------------------------------------------------------
  
  server <- function(input, output, session) {
    
    data_analysisServer("data_analysis")
    
    data_visualizationServer("data_visualization")
    
  }
  
  # Launch app --------------------------------------------------------------
  
  if (Browser == FALSE) {
    shinyApp(ui = ui, server = server)
  } else {
    shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
  }
  
}
