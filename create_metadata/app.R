#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(skimr)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Create metadata for Metro's datasets"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Upload your final csv file, formatted according to Metro's data guideline"),

      # Input: select a file
      fileInput("file1", "Choose CSV File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),


      actionButton("choice", "show Data"),

      # horizontal line
      tags$hr(),

      # button

      downloadButton("download_metadata", label = "Download the metadata")
    ),

    # Show data head
    mainPanel(
      # textOutput("metadata"),
      tableOutput("contents")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # load data
  load <- eventReactive(input$choice, {
    req(input$file1)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
      df <- read.csv(input$file1$datapath)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
    )
  })

  # display datasets
  output$contents <- renderTable(
    head(load())
  )

  # # display metadata (fix formatting)
  # output$metadata <- renderPrint(
  #   skimr::skim(load())
  # )

  # create metadata for download
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste0("meta_", input$file1$name)
    },
    content = function(file) {
      write.csv(skimr::skim_to_wide(load()), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
