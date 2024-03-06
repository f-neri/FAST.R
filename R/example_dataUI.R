example_dataUI <- function(id) {
  
  tagList(
    
    # Example data header -----------------------------------------------
    fluidRow(
      column(12,
             br(),
             h2("Example Data", align = "center"),
             br()
      )
    ),
    
    fluidRow(
      style = "background-color: #D6EBF2",
      fluidRow(
        # download example data
        column(6, offset = 3, align = "center",
               h3("Download", align = "center"),
               br(),
               downloadButton(NS(id, "download_example_data"), label = "Download Example Data"),
               br(),
               br()
        ),
        # description of example data
        column(6, offset = 3,
               h3("Descritpion", align = "center"),
               p("Download the", strong("Example Data files."), " These files can be used to see the FAST.R app in action, or can serve as a benchmark to ensure your own data is structured correctly and ready for analysis."),
               br(),
               p(strong("Included in the download:")),
               p("For Data Analysis:"),
               p(" ○ example_Image_Analyst_Output.xlsx - An Excel file containing image data analyzed with ImageAnalyst MKII."),
               p(" ○ example_Image_Analyst_Output_Metadata.csv - Accompanying metadata in CSV format."),
               p("For Data Visualization:"),
               p(" ○ example_Single_Cell_Data.csv - Single-cell measurements table generated via the Data Analysis tab."),
               p(" ○ example_Analysis_Report.csv - A comprehensive report for each microplate well generated via the Data Analysis tab."),
               br(),
               p(strong("Example Dataset Background:")),
               p("The dataset is derived from an experiment conducted on a standard 96-well microplate. It is divided into two primary conditions:"),
               p(" ○ Ionizing Radiation (IR): Senescent cells induced via ionizing radiation."),
               p(" ○ Control (CTL): Mock-irradiated cells that serve as non-senescent control."),
               p("Additionally, these conditions are further split based on serum levels in the medium when the assay was performed:"),
               p(" ○ Full-Serum Medium (FS): Provides ideal culturing condition for both IR and proliferating CTL cells."),
               p(" ○ Serum-Starved Medium (SS): Allows to compare growth-arrested IR cells to quiescent CTL cells."),
        ),
        column(12,
               br(),
               br())
      )
    )
  )
  
}