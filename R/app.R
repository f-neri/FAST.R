
#' FASTR
#'
#' @import shiny
#' @import magrittr
#' @return
#' @export
#'
#' @examples

FASTR <- function() { # app needs to be wrapped in function to be used as package
  
  . <- NULL # workaround to prevent R CMD note (see https://github.com/Rdatatable/data.table/issues/5436)
  
  function(step_message = "") {
    percentage_progress <- percentage_progress + 1/tot_steps*100 
    incProgress(1,
                detail = str_c("Progress: ", round(percentage_progress, digits = 0), "%\n",step_message))
    percentage_progress
  }
  
# UI ----------------------------------------------------------------------

ui <- fluidPage(
  shinyjs::useShinyjs(), # enables shinyjs functions
  
  titlePanel(
    h1(strong("FAST-R"), align = "center")
    ),
  
  # Title + paper link
  fluidRow(
    column(12,
           h1("FAST Data Analysis & Visualization App", align = "center"), # add a(href = "link", "FAST"), and include link to paper once in place
           br()
           )
  ),
  
  ui <- fluidPage(
    data_analysisInput("data_analysis")
  )
)
    
  
# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  data_analysisServer("data_analysis")
  
  }

shinyApp(ui, server) # , options = list(launch.browser = TRUE) -> browser lunch

}