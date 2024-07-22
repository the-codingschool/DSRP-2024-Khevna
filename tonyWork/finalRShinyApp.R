library(ggplot2)
library(dplyr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(tibble)
library(vroom)

ui <- fluidPage(theme = shinytheme("sandstone"),
    navbarPage(title = "Lung Cancer Data Science - Tony",
         tabPanel("Introduction",
              h2("About the Dataset"),
              p("Realistic python generated dataset on lung cancer patients taken from kaggle."),
              h4("38 variables, 23658 patients"),
              h2("What I did"),
              h2("About Me")
         ),
         navbarMenu("Custom Data",
              tabPanel("Import dataset",
                  h2("Use your own dataset (Not Recommended)"),
                  p("Make sure your dataset has all the same columns as the default dataset!"),
                  p("If your dataset contains different variable names, please define them in the variable declaration tab"),
                  
                  fileInput("inputCSV", "Choose Custom CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                      )
                  )
              ),
              tabPanel("Declare variables",
                  h2("Declare your own variable names."),
                  p("Note that Patient_ID is not necessary")
              )
          )
               
               
               
    )
  
)
server <- function(input,output){
  
  dataInput <- reactive ({
    if(is.null(input$inputCSV)){
      df <- read.csv("lung_cancer_data.csv")
    }
    
    
  })
  
  
}

shinyApp(ui=ui,server=server)