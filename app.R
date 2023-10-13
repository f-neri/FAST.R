library(shiny)
ui <- fluidPage(
  
  titlePanel(
    h1(strong("FAST-R"), align = "center")
    ),
  
  fluidRow(
    
    column(12,
           h1("FAST Data Analysis & Visualization App", align = "center"),
           br(),
           br()
           )
    
  )
  
)
server <- function(input, output, session) {
}
shinyApp(ui, server, options = list(launch.browser = TRUE))