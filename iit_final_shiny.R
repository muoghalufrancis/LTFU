library(shiny)
library(DT)
library(tidyverse)
iiTModel <- readRDS("Newmodel_iit.rds")

iit_file <- read.csv("df_file3.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("IIT Dataset Predictions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "upload csv file here",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")), 
      
      
      # Button
      downloadButton("downloadData", "Download the Predictions")
    ),
    
    # Show the table with the predictions
    mainPanel(
      DT::dataTableOutput("mytable")
    )
  )
)

# Define server logic required to draw a histogram
options(shiny.maxRequestSize = 50*1024^2)
server <- function(input, output) {
  
  
  reactiveDF<-reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = TRUE)
    
    df$predictions<-predict(iiTModel, newdata = iit_file, type ="response")
    return(df)
    
  })
  
  output$mytable = DT::renderDataTable({
    req(input$file1)
    
    return(DT::datatable(reactiveDF(),  options = list(pageLength = 100), filter = c("top")))
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(reactiveDF(), file, row.names = FALSE)
    }
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

