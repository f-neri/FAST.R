enable_button_analysis <- function() {
  shinyjs::enable("button_analysis")
  updateActionButton(inputId = "button_analysis", label = "Run Analysis", icon = icon("rocket"))
}