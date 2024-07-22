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
     )
             
             
             
  )
  
)
server <- function(input,output){}

shinyApp(ui=ui,server=server)