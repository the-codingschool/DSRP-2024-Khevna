library(ggplot2)
library(dplyr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(tibble)
library(vroom)
library(fastDummies)
library(caTools)
library(class)
library(paletteer)
library(randomForest)
library(shinyjs)
library(ggcorrplot)
library(utils)
library(reshape2)
library(gridExtra)

set.seed(42)



ui <- fluidPage(theme = shinytheme("sandstone"),
    useShinyjs(),
    navbarPage(title = "Lung Cancer Data Science - Tony",
         tabPanel("Introduction",
              h2("About the Dataset"),
              p("Python generated dataset on lung cancer patients taken from kaggle."),
              h4("38 variables, 23658 patients"),
              h2("What I did"),
              h2("About Me")
         ),
         tabPanel("Analysis",
              navlistPanel(
                  tabPanel("Introductory Exploration", 
                  column(8,
                  h2("Introductory Exploration"),
                  p("In this introductory exploration, we will perform a preliminary analysis of a 
                    lung cancer dataset. This exploration includes loading and inspecting the dataset,
                    summarizing the data, and visualizing various relationships and distributions within the dataset."),
                  
                  
                  p("To begin any meaningful analysis, it is essential to first import the necessary 
                    libraries and the dataset into our analysis environment. This step is crucial as 
                    it allows us to access and manipulate the data necessary for our exploratory analysis.
                    By importing the dataset, we ensure that we have a structured and organized format of
                    the data, typically in CSV or another accessible format, that can be easily read and
                    processed by our analytical tools."),
                  
                         actionButton("show_import_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "import_code_snippet",
                        verbatimTextOutput("importCode")
                        
                    )
                  ),
                  
                  p("\n\n"),
                  p("We should first look at what the dataset contains and some of 
                    the values at the top of the dataset to get a general gist of the variables and their types"),
                  
                  actionButton("show_head_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "head_code_snippet",
                        verbatimTextOutput("headCode")
                    )
                  ),
                  p("\n"),
                  tableOutput("showHead"),
                  
                  p("\n\n"),
                  p("We can also take a peak at the bottom of the dataset as well."),
                  
                  actionButton("show_tail_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "tail_code_snippet",
                        verbatimTextOutput("tailCode")
                    )
                  ),
                  p("\n"),
                  tableOutput("showTail"),
                  
                  p("\n\n"),
                  p("Now, we saw some of the different values. However, we should still find the exact datatype of each column."),
                  
                  actionButton("show_type_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "type_code_snippet",
                        verbatimTextOutput("typeCode")
                    )
                  ),
                  p("\n"),
                  tableOutput("showType"),
                  
                  p("\n\n"),
                  p("We also need to get the number of rows and columns for dataset manipulation
                    purposes. This dataset is already very clean because there are no NAs or different values.
                    This is because it is the result of a computer generation."),
                  
                  actionButton("show_dims_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "dims_code_snippet",
                        verbatimTextOutput("dimsCode")
                    )
                  ),
                  p("\n"),
                  tableOutput("showDims"))),
                  
                  tabPanel("Categorical Exploration",
                  column(8,
                  h2("Categorical Exploration"),
                  
                  p("\n\n"),
                  p("We should find the most common value for each categorical variable. 
                    We excluded the numerical values as their modes won't show anything as the values are all different"),
                  
                  actionButton("show_common_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "common_code_snippet",
                        verbatimTextOutput("commonCode")
                    )
                  ),
                  p("\n"),
                  tableOutput("showCommon"),
                  
                  p("\n\n"),
                  p("We should also find the most unique value for each categorical variable as well. 
                    Again, we excluded the numerical values as they are almost all unique"),
                  
                  actionButton("show_unique_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "unique_code_snippet",
                        verbatimTextOutput("uniqueCode")
                    )
                  ),
                  p("\n"),
                  tableOutput("showUnique"),
                  
                  p("\n\n"),
                  p("We have the most and least common, but we also need the total number of unique values for each categorical variable"),
                  
                  actionButton("show_uniqueNum_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "uniqueNum_code_snippet",
                        verbatimTextOutput("uniqueNumCode")
                    )
                  ),
                  p("\n"),
                  tableOutput("showUniqueNum"),
                  
                  
                  p("\n\n"),
                  p("Now that we have these values, let's find the distribution of the categorical variables"),
                  
                  actionButton("show_catDis_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "catDis_code_snippet",
                        verbatimTextOutput("catDisCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showCatDis")
                  
                  
                  
                  )),
                  
                  tabPanel("Numerical Exploration",
                  column(8,
                  h2("Numerical Exploration"),
                  p("\n\n"),
                  p("Now, we have done some exploration with the categorical values. Let's move onto doing some things with the numerical values now. We can find
                    statistics such as the mean, median, maximum, and minimum."),
                  
                  actionButton("show_MMS_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "MMS_code_snippet",
                        verbatimTextOutput("MMSCode")
                    )
                  ),
                  p("\n"),
                  p("Mean:"),
                  tableOutput("showMean"),
                  p("Median:"),
                  tableOutput("showMedian"),
                  p("Max:"),
                  tableOutput("showMax"),
                  p("Min:"),
                  tableOutput("showMin"),
                  
                  p("\n\n"),
                  p("Let's also find the variances and standard deviations."),
                  
                  actionButton("show_varsd_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "varsd_code_snippet",
                        verbatimTextOutput("varsdCode")
                    )
                  ),
                  p("\n"),
                  p("Variance:"),
                  tableOutput("showVar"),
                  p("Standard Deviation:"),
                  tableOutput("showSd"),
                  
                  p("\n\n"),
                  p("Now that we have all of these statistics, lets see if the numerical
                    values have any correlation with each other. (Please fullscreen for best viewing experience)"),
                  
                  actionButton("show_corr_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "corr_code_snippet",
                        verbatimTextOutput("corrCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showCorr"),
                  
                  p("\n\n"),
                  p("Hmm, that's strange. Let's switch to something else for now: creating boxplots for each numerical value. 
                    This way, we can see if there are patterns such as weighted distribution. (Please fullscreen for best viewing experience)"),
                  
                  actionButton("show_numBoxPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "numBoxPlot_code_snippet",
                        verbatimTextOutput("numBoxPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showNumBoxPlot"),
                  
                  p("\n\n"),
                  p("This time, we saw an even distribution. Let's confirm this with a series of histograms
                    (Please fullscreen for best viewing experience)"),
                  
                  actionButton("show_numHistPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "numHistPlot_code_snippet",
                        verbatimTextOutput("numHistPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showNumHistPlot"),
                  
                  p("\n\n"),
                  p("You can see that almost everything is evenly distributed. Some of them have extra or half on the ends."))
                  ),
                  tabPanel("Combined Exploration",
                           h2("Combined Exploration"),
                           p("Now lets explore how the categorical and numerical variables interact.
                             First, we can check how ethnicity impacts everything else"),
                  actionButton("show_BoxEthPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "boxEthPlot_code_snippet",
                        verbatimTextOutput("boxEthPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showBoxEthPlot"),
                  p("\n\n"),
                  p("Again, it is very evenly distributed even though it is split by ethnicity. All the ethnicities
                    have almost the same boxplot as well."),
                  p("We can also create
                    a scatterplot of tumor size vs age. There should be some kind of pattern, according
                    to online source. Maybe there would even be a pattern that we can see by coloring with ethnicity"),
                  actionButton("show_EthAgeSizePlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "EthAgeSizePlot_code_snippet",
                        verbatimTextOutput("EthAgeSizePlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showEthAgeSizePlot"),
                  
                  p("\n\n"),
                  p("Yep. Nothing, again. It seems like it is all evenly distributed and that the ethnicity
                    has no effect either, just as we found with the boxplots. Let's move onto something else."),
                  h3("Analyzing Stages"),
                  p("It is well known that there are four stages when it comes to cancer, with Stage I
                    being the least severe and Stage IV being the most severe."),
                  p("Thus, let's first comapare the months survived for the cancer patients grouped
                    by their stage of cancer."),
                  actionButton("show_BoxStSMPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "boxStSMPlot_code_snippet",
                        verbatimTextOutput("boxStSMPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showBoxStSMPlot"),
                  
                  p("\n\n"),
                  p("Again, we get an even distribution split evenly among the four stages. This is
                    contrary to other research in the field and overwhelming trends in the real world."),
                  p("Let's give it the benefit of the doubt. It is often observed that 
                  glucose levels may increase as the cancer progresses to more 
                  advanced stages. This could be due to increased metabolic demands
                  of tumor cells, which often exhibit higher glucose uptake (known as the Warburg effect).
                  Thus, let's compare the stages with the glucose levels."),
                  actionButton("show_BoxStGlPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "boxStGlPlot_code_snippet",
                        verbatimTextOutput("boxStGlPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showBoxStGlPlot"),
                  
                  p("\n\n"),
                  p("Now that we know that the glucose levels are evenly distributed, let's look at every other
                  numerical value as well"),
                  actionButton("show_BoxStGridPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "boxStGridPlot_code_snippet",
                        verbatimTextOutput("boxStGridPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showBoxStGridPlot"),
                  
                  p("\n\n"),
                  p("It surely is a trend in this dataset to have everything evenly distributed."),
                  p("Maybe it has something to do with comorbidity. Maybe if we graph some boxplots
                    along with stage, we can see some effect in survival months"),
                  actionButton("show_BoxStCoGridPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "boxStCoGridPlot_code_snippet",
                        verbatimTextOutput("boxStCoGridPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showBoxStCoGridPlot"),
                  
                  p("\n\n"),
                  p("Wow, they are almost all the exact same. Maybe it has something to deal
                    with the number of people with each comorobidity disease. Let's plot
                    this real quick."),
                  actionButton("show_BarCoGridPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "barCoGridPlot_code_snippet",
                        verbatimTextOutput("barCoGridPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showBarCoGridPlot"),
                  
                  p("\n\n"),
                  p("It wasn't that, but this gives us some important information. Even the comorbidity
                    is evenly distributed")),
                  tabPanel("Death and Age",
                  
                  h2("Death and Age"),
                  p("Lung cancer treatment encompasses several methods, each tailored to the specific
                    needs of the patient and the stage of the cancer. Chemotherapy involves using
                    powerful drugs to kill rapidly growing cancer cells, and it can be used before
                    or after surgery, or as a primary treatment for advanced cancer. Radiation therapy 
                    uses high-energy rays to target and destroy cancer cells, either externally or 
                    internally, and can shrink tumors or eliminate remaining cancer cells post-surgery. 
                    Surgery involves the physical removal of cancerous tissue and is often utilized in
                    early-stage lung cancer. Additionally, targeted therapy focuses on the molecular changes
                    in cancer cells, offering a treatment that interferes with specific proteins or genes
                    promoting cancer growth. Each of these treatments has its own set of benefits and side
                    effects, and the choice of treatment depends on various factors, including the type 
                    and stage of lung cancer, and the overall health of the patient."),
                  p("Let us first explore how each treatment type affects how long the patient survives"),
                  actionButton("show_tSMPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "tSMPlot_code_snippet",
                        verbatimTextOutput("tSMPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showtSMPlot"),
                  
                  p("\n\n"),
                  p("Now, how about we split the survival months into three categories:"),
                  markdown("
                    + 0: Death within 1 year
                    + 1: Death within 5 years
                    + 2: Death after 5 years
                  "),
                  p("
                    We can now compare this to age to see if there is any difference between these three major
                    groups using boxplots, which we couldn't do if we only had survival months.
                    "),
                  actionButton("show_ASGPlot_code", "Show Code",class = "btn btn-primary"),
                  hidden(
                    div(id = "ASGPlot_code_snippet",
                        verbatimTextOutput("ASGPlotCode")
                    )
                  ),
                  p("\n"),
                  plotOutput("showASGPlot")
                  
                  )
              )
         ),
         navbarMenu("Custom Data",
              tabPanel("Import dataset",
                  h2("Use your own dataset (Not Recommended)"),
                  p("Make sure your dataset has all the same columns as the default dataset!"),
                  p("If your dataset contains different variable names, please define them in the variable declaration tab"),
                  p("Note: This probably won't work because of maximum file size issues"),
                  
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
                  p("Note that Patient_ID is not necessary"),
                  # Big chunk now
                  column(6,
                      textInput("submitAgeName", "Enter Age Variable Name:", "Age"),
                      textInput("submitGenderName", "Enter Gender Variable Name:", "Gender"),
                      textInput("submitSmoking_HistoryName", "Enter Smoking_History Variable Name:", "Smoking_History"),
                      textInput("submitTumor_Size_mmName", "Enter Tumor_Size_mm Variable Name:", "Tumor_Size_mm"),
                      textInput("submitTumor_LocationName", "Enter Tumor_Location Variable Name:", "Tumor_Location"),
                      textInput("submitStageName", "Enter Stage Variable Name:", "Stage"),
                      textInput("submitTreatmentName", "Enter Treatment Variable Name:", "Treatment"),
                      textInput("submitSurvival_MonthsName", "Enter Survival_Months Variable Name:", "Survival_Months"),
                      textInput("submitEthnicityName", "Enter Ethnicity Variable Name:", "Ethnicity"),
                      textInput("submitInsurance_TypeName", "Enter Insurance_Type Variable Name:", "Insurance_Type"),
                      textInput("submitFamily_HistoryName", "Enter Family_History Variable Name:", "Family_History"),
                      textInput("submitComorbidity_DiabetesName", "Enter Comorbidity_Diabetes Variable Name:", "Comorbidity_Diabetes"),
                      textInput("submitComorbidity_HypertensionName", "Enter Comorbidity_Hypertension Variable Name:", "Comorbidity_Hypertension"),
                      textInput("submitComorbidity_Heart_DiseaseName", "Enter Comorbidity_Heart_Disease Variable Name:", "Comorbidity_Heart_Disease"),
                      textInput("submitComorbidity_Chronic_Lung_DiseaseName", "Enter Comorbidity_Chronic_Lung_Disease Variable Name:", "Comorbidity_Chronic_Lung_Disease"),
                      textInput("submitComorbidity_Kidney_DiseaseName", "Enter Comorbidity_Kidney_Disease Variable Name:", "Comorbidity_Kidney_Disease"),
                      textInput("submitComorbidity_Autoimmune_DiseaseName", "Enter Comorbidity_Autoimmune_Disease Variable Name:", "Comorbidity_Autoimmune_Disease"),
                      textInput("submitComorbidity_OtherName", "Enter Comorbidity_Other Variable Name:", "Comorbidity_Other")
                  ),
                  column(6,
                      textInput("submitPerformance_StatusName", "Enter Performance_Status Variable Name:", "Performance_Status"),
                      textInput("submitBlood_Pressure_SystolicName", "Enter Blood_Pressure_Systolic Variable Name:", "Blood_Pressure_Systolic"),
                      textInput("submitBlood_Pressure_DiastolicName", "Enter Blood_Pressure_Diastolic Variable Name:", "Blood_Pressure_Diastolic"),
                      textInput("submitBlood_Pressure_PulseName", "Enter Blood_Pressure_Pulse Variable Name:", "Blood_Pressure_Pulse"),
                      textInput("submitHemoglobin_LevelName", "Enter Hemoglobin_Level Variable Name:", "Hemoglobin_Level"),
                      textInput("submitWhite_Blood_Cell_CountName", "Enter White_Blood_Cell_Count Variable Name:", "White_Blood_Cell_Count"),
                      textInput("submitPlatelet_CountName", "Enter Platelet_Count Variable Name:", "Platelet_Count"),
                      textInput("submitAlbumin_LevelName", "Enter Albumin_Level Variable Name:", "Albumin_Level"),
                      textInput("submitAlkaline_Phosphatase_LevelName", "Enter Alkaline_Phosphatase_Level Variable Name:", "Alkaline_Phosphatase_Level"),
                      textInput("submitAlanine_Aminotransferase_LevelName", "Enter Alanine_Aminotransferase_Level Variable Name:", "Alanine_Aminotransferase_Level"),
                      textInput("submitAspartate_Aminotransferase_LevelName", "Enter Aspartate_Aminotransferase_Level Variable Name:", "Aspartate_Aminotransferase_Level"),
                      textInput("submitCreatinine_LevelName", "Enter Creatinine_Level Variable Name:", "Creatinine_Level"),
                      textInput("submitLDH_LevelName", "Enter LDH_Level Variable Name:", "LDH_Level"),
                      textInput("submitCalcium_LevelName", "Enter Calcium_Level Variable Name:", "Calcium_Level"),
                      textInput("submitPhosphorus_LevelName", "Enter Phosphorus_Level Variable Name:", "Phosphorus_Level"),
                      textInput("submitGlucose_LevelName", "Enter Glucose_Level Variable Name:", "Glucose_Level"),
                      textInput("submitPotassium_LevelName", "Enter Potassium_Level Variable Name:", "Potassium_Level"),
                      textInput("submitSodium_LevelName", "Enter Sodium_Level Variable Name:", "Sodium_Level")
                  ),
                  column(12,
                      textInput("submitSmoking_Pack_YearsName", "Enter Smoking_Pack_Years Variable Name:", "Smoking_Pack_Years")
                  )
                  
                  
              )
         ),
         
         navbarMenu("Prediction",
             tabPanel("Input Patient Data",
                h2("Input Patient Data"),
                p("Please answer as many as possible"),
                column(6,
                    numericInput("submitAge", "Enter Age (Years):",NULL),
                    selectInput("submitGender", "Select Gender:", choices=c("Male","Female")),
                    selectInput("submitSmoking_History", "Select Smoking_History:", choices=c("Never Smoked","Current Smoker","Former Smoker")),
                    numericInput("submitTumor_Size_mm", "Enter Tumor_Size_mm:",NULL),
                    selectInput("submitTumor_Location", "Select Tumor_Location:",choices=c("Lower Lobe","Middle Lobe","Upper Lobe","Unknown")),
                    selectInput("submitStage","Select Stage:",choices=c("Stage I", "Stage II", "Stage III", "Stage IV", "Unknown")),
                    selectInput("submitEthnicity", "Select Ethnicity:",choices=c("African American","Asian","Caucasian","Hispanic","Other")),
                    selectInput("submitFamily_History", "Select Family_History:",choices=c("Yes","No"),"No"),
                    selectInput("submitComorbidity_Diabetes", "Select Comorbidity_Diabetes:",choices=c("Yes","No"),"No"),
                    selectInput("submitComorbidity_Hypertension", "Select Comorbidity_Hypertension:",choices=c("Yes","No"),"No"),
                    selectInput("submitComorbidity_Heart_Disease", "Select Comorbidity_Heart_Disease:",choices=c("Yes","No"),"No"),
                    selectInput("submitComorbidity_Chronic_Lung_Disease", "Select Comorbidity_Chronic_Lung_Disease:",choices=c("Yes","No"),"No"),
                    selectInput("submitComorbidity_Kidney_Disease", "Select Comorbidity_Kidney_Disease:",choices=c("Yes","No"),"No"),
                    selectInput("submitComorbidity_Autoimmune_Disease", "Select Comorbidity_Autoimmune_Disease:",choices=c("Yes","No"),"No"),
                    selectInput("submitComorbidity_Other", "Select Comorbidity_Other:",choices=c("Yes","No"),"No"),
                    selectInput("submitTreatment","Select Treatment",c("Surgery","Radiation Therapy", "Chemotherapy", "Targeted Therapy")),
                    selectInput("submitPerformance_Status","Select Performance Status",c(1,2,3,4))
                    ),
                column(6,
                     numericInput("submitBlood_Pressure_Systolic", "Enter Blood_Pressure_Systolic:",NULL),
                     numericInput("submitBlood_Pressure_Diastolic", "Enter Blood_Pressure_Diastolic:",NULL),
                     numericInput("submitBlood_Pressure_Pulse", "Enter Blood_Pressure_Pulse:",NULL),
                     numericInput("submitHemoglobin_Level", "Enter Hemoglobin_Level:",NULL),
                     numericInput("submitWhite_Blood_Cell_Count", "Enter White_Blood_Cell_Count:",NULL),
                     numericInput("submitPlatelet_Count", "Enter Platelet_Count:",NULL),
                     numericInput("submitAlbumin_Level", "Enter Albumin_Level:",NULL),
                     numericInput("submitAlkaline_Phosphatase_Level", "Enter Alkaline_Phosphatase_Level:",NULL),
                     numericInput("submitAlanine_Aminotransferase_Level", "Enter Alanine_Aminotransferase_Level:",NULL),
                     numericInput("submitAspartate_Aminotransferase_Level", "Enter Aspartate_Aminotransferase_Level:",NULL),
                     numericInput("submitCreatinine_Level", "Enter Creatinine_Level:",NULL),
                     numericInput("submitLDH_Level", "Enter LDH_Level:",NULL),
                     numericInput("submitCalcium_Level", "Enter Calcium_Level:",NULL),
                     numericInput("submitPhosphorus_Level", "Enter Phosphorus_Level:",NULL),
                     numericInput("submitGlucose_Level", "Enter Glucose_Level:",NULL),
                     numericInput("submitPotassium_Level", "Enter Potassium_Level:",NULL),
                     numericInput("submitSodium_Level", "Enter Sodium_Level:",NULL)
                     
                 )
             ),
             tabPanel("Results",
                h2("Results"),
                navlistPanel(
                  tabPanel(
               "KNN",
                 h2("K-Nearest Neighbors Prediction"),
                 p("The algorithm was trained with the following final outcomes:"),
                 markdown("
                    + 0: Death within 1 year
                    + 1: Death within 5 years
                    + 2: Death after 5 years
                 "),
                 p("Confusion Matrix:"),
                 tableOutput("knnConfusionMatrix"),
                 textOutput("knnAccuracy"),
                 checkboxInput("submitPredict","Predict with patient data? (Will only use provided variables to train)",FALSE),
                 textOutput("knnPrediction")
             ),
             tabPanel("Random Forest",
                      h2("Random Forest Prediction"),
                      p("The algorithm was trained with the following final outcomes:"),
                      markdown("
                    + 0: Death within 1 year
                    + 1: Death within 5 years
                    + 2: Death after 5 years
                 "),
                      p("Confusion Matrix:"),
                      tableOutput("rfConfusionMatrix"),
                      textOutput("rfAccuracy"),
                      checkboxInput("submitPredict","Predict with patient data? (Will only use provided variables to train)",FALSE),
                      textOutput("rfPrediction")
             )))
           
           
         )
               
               
               
    )
  
)
server <- function(input,output){
  
  dataSetConstant <- reactive({
    df <- read.csv("../data/lung_cancer_data.csv")
    return(df)
  })
  
  output$showImport <- renderTable({
    head(dataSetConstant())
  })
  
  output$importCode <- renderText({
    "library(utils)
library(ggcorrplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
# Get csv file
# 1. Read the data from a CSV file and set working directory (This may be different for you)
setwd(\"C:/Users/Tony/OneDrive/Documents/tonyR/DSRP-2024-Khevna/tonyWork\")
data <- read.csv(\"../data/lung_cancer_data.csv\")"
  })
  
  observeEvent(input$show_import_code, {
    toggle(id="import_code_snippet")
  })
  
  output$showHead <- renderTable({
    head(dataSetConstant())
  })
  
  output$headCode <- renderText({
    "head(data)"
  })
  
  observeEvent(input$show_head_code, {
    toggle(id="head_code_snippet")
  })
  
  output$showTail <- renderTable({
    tail(dataSetConstant())
  })
  
  output$tailCode <- renderText({
    "tail(data)"
  })
  
  observeEvent(input$show_tail_code, {
    toggle(id="tail_code_snippet")
  })
  
  output$showType <- renderTable({
    t(sapply(dataSetConstant(), class))
  })
  
  output$typeCode <- renderText({
    "t(sapply(data, class))"
  })
  
  observeEvent(input$show_type_code, {
    toggle(id="type_code_snippet")
  })
  
  output$showDims <- renderTable({
    t(dim(dataSetConstant()))
  })
  
  output$dimsCode <- renderText({
    "t(dim(data))"
  })
  
  observeEvent(input$show_dims_code, {
    toggle(id="dims_code_snippet")
  })
  
  output$showCommon <- renderTable({
    categoricalData=select(dataSetConstant(),c("Gender","Smoking_History","Tumor_Location","Stage","Treatment","Ethnicity","Performance_Status","Insurance_Type"))
    t(sapply(categoricalData, function(col) {
      names(sort(table(col), decreasing = TRUE))[1]  # Most common value
    }))
  })
  
  output$commonCode <- renderText({
    "t(categoricalData=select(data,c(\"Gender\",\"Smoking_History\",\"Tumor_Location\",\"Stage\",\"Treatment\",\"Ethnicity\",\"Performance_Status\",\"Insurance_Type\"))
sapply(categoricalData, function(col) {
  names(sort(table(col), decreasing = TRUE))[1] 
}))"
  })
  
  observeEvent(input$show_common_code, {
    toggle(id="common_code_snippet")
  })
  
  output$showUnique <- renderTable({
    categoricalData=select(dataSetConstant(),c("Gender","Smoking_History","Tumor_Location","Stage","Treatment","Ethnicity","Performance_Status","Insurance_Type"))
    t(sapply(categoricalData, function(col) {
      names(sort(table(col), increasing = TRUE))[1]  # Most common value
    }))
  })
  
  output$uniqueCode <- renderText({
    "t(categoricalData=select(data,c(\"Gender\",\"Smoking_History\",\"Tumor_Location\",\"Stage\",\"Treatment\",\"Ethnicity\",\"Performance_Status\",\"Insurance_Type\"))
sapply(categoricalData, function(col) {
  names(sort(table(col), increasing = TRUE))[1] 
}))"
  })
  
  observeEvent(input$show_unique_code, {
    toggle(id="unique_code_snippet")
  })
  
  observeEvent(input$show_uniqueNum_code, {
    toggle(id="uniqueNum_code_snippet")
  })
  
  output$showUniqueNum <- renderTable({
    categoricalData=select(dataSetConstant(),c("Gender","Smoking_History","Tumor_Location","Stage","Treatment","Ethnicity","Performance_Status","Insurance_Type"))
    t(sapply(categoricalData, function(col) length(unique(col))))
  })
  
  output$uniqueNumCode <- renderText({
    "t(sapply(categoricalData, function(col) length(unique(col))))"
  })
  
  observeEvent(input$show_catDis_code, {
    toggle(id="catDis_code_snippet")
  })
  
  output$showCatDis <- renderPlot({
    df <- dataSetConstant()
    plot_list <- list()
    for (var in c("Gender","Smoking_History","Tumor_Location","Stage", "Insurance_Type")) {
      p <- ggplot(df, aes_string(x = var)) +
        geom_bar() +
        theme_minimal()
      
      plot_list[[var]] <- p
    }
    do.call(grid.arrange, c(plot_list, ncol = 2))
  })
  
  output$catDisCode <- renderText({
    "plot_list <- list()
for (var in c(\"Gender\",\"Smoking_History\",\"Tumor_Location\",\"Stage\", \"Insurance_Type\")) {
  p <- ggplot(data, aes_string(x = var)) +
    geom_bar() +
    theme_minimal()
  
  plot_list[[var]] <- p
}
do.call(grid.arrange, c(plot_list, ncol = 2))"
  })
  
  observeEvent(input$show_MMS_code, {
    toggle(id="MMS_code_snippet")
  })
  
  MMSnumericalData <- reactive({
    numericalData = select(dataSetConstant(),c("Age", "Tumor_Size_mm","Survival_Months","Blood_Pressure_Systolic","Blood_Pressure_Diastolic",
                                  "Blood_Pressure_Pulse","Hemoglobin_Level","White_Blood_Cell_Count","Platelet_Count",
                                  "Albumin_Level","Alkaline_Phosphatase_Level","Alanine_Aminotransferase_Level",
                                  "Aspartate_Aminotransferase_Level","Creatinine_Level","LDH_Level","Calcium_Level",
                                  "Phosphorus_Level","Glucose_Level","Potassium_Level","Sodium_Level","Smoking_Pack_Years"))
    
  })
  
  output$showMean <- renderTable({
    t(sapply(MMSnumericalData(),mean))
  })
  output$showMedian <- renderTable({
    t(sapply(MMSnumericalData(),median))
  })
  output$showMax <- renderTable({
    t(sapply(MMSnumericalData(),max))
  })
  
  output$showMin <- renderTable({
    t(sapply(MMSnumericalData(),min))
  })
  
  output$MMSCode <- renderText({
    "numericalData = select(data,c(\"Age\", \"Tumor_Size_mm\",\"Survival_Months\",\"Blood_Pressure_Systolic\",\"Blood_Pressure_Diastolic\",
                              \"Blood_Pressure_Pulse\",\"Hemoglobin_Level\",\"White_Blood_Cell_Count\",\"Platelet_Count\",
                              \"Albumin_Level\",\"Alkaline_Phosphatase_Level\",\"Alanine_Aminotransferase_Level\",
                              \"Aspartate_Aminotransferase_Level\",\"Creatinine_Level\",\"LDH_Level\",\"Calcium_Level\",
                              \"Phosphorus_Level\",\"Glucose_Level\",\"Potassium_Level\",\"Sodium_Level\",\"Smoking_Pack_Years\"))

t(sapply(numericalData,mean))
t(sapply(numericalData,median))
t(sapply(numericalData,max))
t(sapply(numericalData,min))"
  })
  
  observeEvent(input$show_varsd_code, {
    toggle(id="varsd_code_snippet")
  })
  
  output$showVar <- renderTable({
    t(sapply(numericalData,var))
  })
  output$showSd <- renderTable({
    t(sapply(numericalData,sd))
  })
  
  output$varsdCode <- renderText({
    "t(sapply(numericalData,var))
t(sapply(numericalData,sd))"
  })
  
  output$showCorr <- renderPlot({
        correlation_matrix <- cor(MMSnumericalData(),use = "complete.obs")
    ggcorrplot(correlation_matrix) + labs(title="Numerical Data Correlation Matrix")
  })
  
  output$corrCode <- renderText({
    "correlation_matrix <- cor(numericalData,use = \"complete.obs\")
ggcorrplot(correlation_matrix)"
  })
  
  observeEvent(input$show_corr_code, {
    toggle(id="corr_code_snippet")
  })
  
  output$showNumBoxPlot <- renderPlot({
    par(mfrow = c(3, 7))
    for (i in 1:21) {
      boxplot(MMSnumericalData()[, i], main = colnames(numericalData)[i], col = "lightblue")
    }
    
  })
  
  output$numBoxPlotCode <- renderText({
    "par(mfrow = c(3, 7))
    for (i in 1:21) {
      boxplot(MMSnumericalData()[, i], main = colnames(numericalData)[i], col = \"lightblue\")
    }"
  })
  
  observeEvent(input$show_numBoxPlot_code, {
    toggle(id="numBoxPlot_code_snippet")
  })
  
  output$showNumHistPlot <- renderPlot({
    par(mfrow = c(3, 7))
    for (i in 1:21) {
      hist(numericalData[, i], main = colnames(numericalData)[i], col = paletteer_d("RColorBrewer::Pastel2"))
    }
    
  })
  
  output$numHistPlotCode <- renderText({
    "par(mfrow = c(3, 7))
    for (i in 1:21) {
  hist(numericalData[, i], main = colnames(numericalData)[i], col = paletteer_d(\"RColorBrewer::Pastel2\"))
}"
  })
  
  observeEvent(input$show_numHistPlot_code, {
    toggle(id="numHistPlot_code_snippet")
  })
  
  
  numericalDataEthnicity <- reactive({
    df <- select(dataSetConstant(),c("Ethnicity","Age","Tumor_Size_mm","Survival_Months","Blood_Pressure_Systolic","Blood_Pressure_Diastolic",
                                     "Blood_Pressure_Pulse","Hemoglobin_Level","White_Blood_Cell_Count","Platelet_Count",
                                     "Albumin_Level","Alkaline_Phosphatase_Level","Alanine_Aminotransferase_Level",
                                     "Aspartate_Aminotransferase_Level","Creatinine_Level","LDH_Level","Calcium_Level",
                                     "Phosphorus_Level","Glucose_Level","Potassium_Level","Sodium_Level"))
    return(df)
    
  }) 
  
  output$showBoxEthPlot <- renderPlot({
    par(mfrow = c(3, 7))

    longData <- gather(numericalDataEthnicity(), key = "Variable", value = "Value", -Ethnicity)
    
    # Create boxplots
    ggplot(longData, aes(x = Ethnicity, y = Value)) +
      geom_boxplot() +
      facet_wrap(~Variable, scales = "free_y") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Boxplots of Variables by Ethnicity",
           x = "Ethnicity",
           y = "Value") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$boxEthPlotCode <- renderText({
    "numericalDataEthnicity = select(data,c(\"Ethnicity\",\"Age\",\"Tumor_Size_mm\",\"Survival_Months\",\"Blood_Pressure_Systolic\",\"Blood_Pressure_Diastolic\",
                                       \"Blood_Pressure_Pulse\",\"Hemoglobin_Level\",\"White_Blood_Cell_Count\",\"Platelet_Count\",
                                       \"Albumin_Level\",\"Alkaline_Phosphatase_Level\",\"Alanine_Aminotransferase_Level\",
                                       \"Aspartate_Aminotransferase_Level\",\"Creatinine_Level\",\"LDH_Level\",\"Calcium_Level\",
                                       \"Phosphorus_Level\",\"Glucose_Level\",\"Potassium_Level\",\"Sodium_Level\"))

longData <- gather(numericalDataEthnicity, key = \"Variable\", value = \"Value\", -Ethnicity)

# Create boxplots
ggplot(longData, aes(x = Ethnicity, y = Value)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = \"free_y\") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = \"Boxplots of Variables by Ethnicity\",
       x = \"Ethnicity\",
       y = \"Value\") +
  theme(plot.title = element_text(hjust = 0.5))"
  })
  
  observeEvent(input$show_BoxEthPlot_code, {
    toggle(id="boxEthPlot_code_snippet")
  })
  
  
  
  output$showEthAgeSizePlot <- renderPlot({
    par(mfrow = c(1, 1))
    ggplot(numericalDataEthnicity(), aes(x = Age, y = Tumor_Size_mm, color = Ethnicity)) +
      geom_point() +
      theme_bw() +
      labs(title = paste("Scatter Plot of Age vs Tumor Size Colored by Ethnicity"),
           x = "Age",
           y = "Tumor_Size_mm") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$EthAgeSizePlotCode <- renderText({
    "par(mfrow = c(1, 1))
    ggplot(numericalDataEthnicity, aes(x = Age, y = Tumor_Size_mm, color = Ethnicity)) +
  geom_point() +
  theme_bw() +
  labs(title = paste(\"Scatter Plot of Age vs Tumor Size Colored by Ethnicity\"),
       x = \"Age\",
       y = \"Tumor_Size_mm\") +
  theme(plot.title = element_text(hjust = 0.5))"
  })
  
  observeEvent(input$show_EthAgeSizePlot_code, {
    toggle(id="EthAgeSizePlot_code_snippet")
  })
  
  numericalDataStage <- reactive({
    df <- select(dataSetConstant(),c("Stage","Tumor_Size_mm","Survival_Months","Blood_Pressure_Systolic","Blood_Pressure_Diastolic",
                        "Blood_Pressure_Pulse","Hemoglobin_Level","White_Blood_Cell_Count","Platelet_Count",
                        "Albumin_Level","Alkaline_Phosphatase_Level","Alanine_Aminotransferase_Level",
                        "Aspartate_Aminotransferase_Level","Creatinine_Level","LDH_Level","Calcium_Level",
                        "Phosphorus_Level","Glucose_Level","Potassium_Level","Sodium_Level"))
    return(df)
    
  }) 
  
  output$showBoxStSMPlot <- renderPlot({
    par(mfrow = c(1, 1))
    ggplot(numericalDataStage(),aes(x=Stage,y=Survival_Months)) + geom_boxplot() + 
    labs(title="Boxplots of Survival Months vs Stage") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$boxStSMPlotCode <- renderText({
    "par(mfrow = c(1, 1))
    numericalDataStage = select(data,c(\"Stage\",\"Tumor_Size_mm\",\"Survival_Months\",\"Blood_Pressure_Systolic\",\"Blood_Pressure_Diastolic\",
                                   \"Blood_Pressure_Pulse\",\"Hemoglobin_Level\",\"White_Blood_Cell_Count\",\"Platelet_Count\",
                                   \"Albumin_Level\",\"Alkaline_Phosphatase_Level\",\"Alanine_Aminotransferase_Level\",
                                   \"Aspartate_Aminotransferase_Level\",\"Creatinine_Level\",\"LDH_Level\",\"Calcium_Level\",
                                   \"Phosphorus_Level\",\"Glucose_Level\",\"Potassium_Level\",\"Sodium_Level\"))


ggplot(numericalDataStage,aes(x=Stage,y=Survival_Months)) + geom_boxplot() + 
labs(title=\"Boxplots of Survival Months vs Stage\") +
      theme(plot.title = element_text(hjust = 0.5))"
  })
  
  observeEvent(input$show_BoxStSMPlot_code, {
    toggle(id="boxStSMPlot_code_snippet")
  })
  
  output$showBoxStGlPlot <- renderPlot({
    par(mfrow = c(1, 1))
    ggplot(numericalDataStage(),aes(x=Stage,y=Glucose_Level)) + geom_boxplot() + 
      labs(title="Boxplots of Glucose Level vs Stage")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$boxStGlPlotCode <- renderText({
    "par(mfrow = c(1, 1))
ggplot(numericalDataStage,aes(x=Stage,y=Glucose_Level)) + geom_boxplot() + 
labs(title=\"Boxplots of Glucose Level vs Stage\") +
    theme(plot.title = element_text(hjust = 0.5))"
  })
  
  observeEvent(input$show_BoxStGlPlot_code, {
    toggle(id="boxStGlPlot_code_snippet")
  })
  
  output$showBoxStGridPlot <- renderPlot({
    long_data <- melt(dataSetConstant(), variable.name = "Variable", value.name = "Value")
    
    ggplot(long_data, aes(x = Stage, y = Value)) +
      geom_boxplot() +
      facet_wrap(~ Variable, scales = "free_y", ncol = 5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(hjust = 0.5)) +
      labs(title = "Boxplots of Numerical Variables vs Stage",
           x = "Stage",
           y = "Numerical Variable Value") 
  })
  
  output$boxStGridPlotCode <- renderText({
    "long_data <- melt(data, variable.name = \"Variable\", value.name = \"Value\")

ggplot(long_data, aes(x = Stage, y = Value)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = \"free_y\", ncol = 5) +
theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(hjust = 0.5)) +
labs(title = \"Boxplots of Numerical Variables vs Stage\",
     x = \"Stage\",
     y = \"Numerical Variable Value\") "
  })
  
  observeEvent(input$show_BoxStGridPlot_code, {
    toggle(id="boxStGridPlot_code_snippet")
  })
  
  output$showBoxStCoGridPlot <- renderPlot({
    df <- dataSetConstant()
    plot_list <- list()
    for(comorbidity in c("Comorbidity_Diabetes", "Comorbidity_Hypertension", "Comorbidity_Heart_Disease", "Comorbidity_Chronic_Lung_Disease", "Comorbidity_Kidney_Disease", "Comorbidity_Autoimmune_Disease", "Comorbidity_Other")){
      p <- ggplot(df, aes_string(x = "Stage", y = "Survival_Months", fill = comorbidity)) +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      plot_list[[comorbidity]] <- p
    }
    do.call(grid.arrange, c(plot_list, ncol = 2))
  })
  
  output$boxStCoGridPlotCode <- renderText({
    "plot_list <- list()
for(comorbidity in c(\"Comorbidity_Diabetes\", \"Comorbidity_Hypertension\", \"Comorbidity_Heart_Disease\",
\"Comorbidity_Chronic_Lung_Disease\", \"Comorbidity_Kidney_Disease\", \"Comorbidity_Autoimmune_Disease\",
\"Comorbidity_Other\")){
  p <- ggplot(data, aes_string(x = \"Stage\", y = \"Survival_Months\", fill = comorbidity)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_list[[comorbidity]] <- p
}
do.call(grid.arrange, c(plot_list, ncol = 2))"
  })
  
  observeEvent(input$show_BoxStCoGridPlot_code, {
    toggle(id="boxStCoGridPlot_code_snippet")
  })
  
  output$showBarCoGridPlot <- renderPlot({
    df <- dataSetConstant()
    par(mfrow = c(2, 4))
    for(i in 13:19) {
      counts <- table(df[, i])
      barplot(counts, main = colnames(df)[i])
    }
  })
  
  output$barCoGridPlotCode <- renderText({
    "par(mfrow = c(2, 4))
for(i in 13:19) {
  counts <- table(data[, i])
  barplot(counts, main = colnames(data)[i])
}"
  })
  
  observeEvent(input$show_BarCoGridPlot_code, {
    toggle(id="barCoGridPlot_code_snippet")
  })
  
  output$showtSMPlot <- renderPlot({
    ggplot(dataSetConstant(),aes(x=Treatment,y=Survival_Months)) +
      geom_boxplot() + labs(title="Boxplot of Survival Months vs Treatment") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$tSMPlotCode <- renderText({
    "ggplot(data,aes(x=Treatment,y=Survival_Months)) +
  geom_boxplot() + labs(title=\"Boxplot of Survival Months vs Treatment\") +
  theme(plot.title = element_text(hjust = 0.5))"
  })
  
  observeEvent(input$show_tSMPlot_code, {
    toggle(id="tSMPlot_code_snippet")
  })
  
  output$showASGPlot <- renderPlot({
    data <- dataSetConstant()
    data <- data %>% mutate(Survival=ifelse(Survival_Months>=60,2,ifelse(Survival_Months>=12,1,0)))
    data$Survival <- as.factor(data$Survival)
    ggplot(data,aes(x=Survival,y=Age,group=Survival)) + geom_boxplot() + labs(title = "Boxplot of Age by Survival Group",
                                                                              x = "Survival Group",
                                                                              y = "Age") + theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$ASGPlotCode <- renderText({
    "data <- data %>% mutate(Survival=ifelse(Survival_Months>=60,2,ifelse(Survival_Months>=12,1,0)))
data$Survival <- as.factor(data$Survival)
ggplot(data,aes(x=Survival,y=Age,group=Survival)) + geom_boxplot() + labs(title = \"Boxplot of Age by Survival Group\",
                                                               x = \"Survival Group\",
                                                               y = \"Age\") +
theme(plot.title = element_text(hjust = 0.5))
"
  })
  
  observeEvent(input$show_ASGPlot_code, {
    toggle(id="ASGPlot_code_snippet")
  })
  
  
  
  
  
  data <- reactive ({
    if(is.null(input$inputCSV)){
        df <- read.csv("../data/lung_cancer_data.csv")
    } else {
      ext <- tools::file_ext(input$inputCSV$name)
      df <- switch(ext,
         csv = vroom::vroom(input$inputCSV$datapath, delim = ","),
         tsv = vroom::vroom(input$inputCSV$datapath, delim = "\t"),
         validate("Invalid file; Please upload a .csv or .tsv file")
      )
      
      colnames(df)[which(names(df)==input$submitAgeName)] <- "Age"
      colnames(df)[which(names(df)==input$submitGenderName)] <- "Gender"
      colnames(df)[which(names(df)==input$submitSmoking_HistoryName)] <- "Smoking_History"
      colnames(df)[which(names(df)==input$submitTumor_Size_mmName)] <- "Tumor_Size_mm"
      colnames(df)[which(names(df)==input$submitTumor_LocationName)] <- "Tumor_Location"
      colnames(df)[which(names(df)==input$submitStageName)] <- "Stage"
      colnames(df)[which(names(df)==input$submitTreatmentName)] <- "Treatment"
      colnames(df)[which(names(df)==input$submitSurvival_MonthsName)] <- "Survival_Months"
      colnames(df)[which(names(df)==input$submitEthnicityName)] <- "Ethnicity"
      colnames(df)[which(names(df)==input$submitInsurance_TypeName)] <- "Insurance_Type"
      colnames(df)[which(names(df)==input$submitFamily_HistoryName)] <- "Family_History"
      colnames(df)[which(names(df)==input$submitComorbidity_DiabetesName)] <- "Comorbidity_Diabetes"
      colnames(df)[which(names(df)==input$submitComorbidity_HypertensionName)] <- "Comorbidity_Hypertension"
      colnames(df)[which(names(df)==input$submitComorbidity_Heart_DiseaseName)] <- "Comorbidity_Heart_Disease"
      colnames(df)[which(names(df)==input$submitComorbidity_Chronic_Lung_DiseaseName)] <- "Comorbidity_Chronic_Lung_Disease"
      colnames(df)[which(names(df)==input$submitComorbidity_Kidney_DiseaseName)] <- "Comorbidity_Kidney_Disease"
      colnames(df)[which(names(df)==input$submitComorbidity_Autoimmune_DiseaseName)] <- "Comorbidity_Autoimmune_Disease"
      colnames(df)[which(names(df)==input$submitComorbidity_OtherName)] <- "Comorbidity_Other"
      colnames(df)[which(names(df)==input$submitPerformance_StatusName)] <- "Performance_Status"
      colnames(df)[which(names(df)==input$submitBlood_Pressure_SystolicName)] <- "Blood_Pressure_Systolic"
      colnames(df)[which(names(df)==input$submitBlood_Pressure_DiastolicName)] <- "Blood_Pressure_Diastolic"
      colnames(df)[which(names(df)==input$submitBlood_Pressure_PulseName)] <- "Blood_Pressure_Pulse"
      colnames(df)[which(names(df)==input$submitHemoglobin_LevelName)] <- "Hemoglobin_Level"
      colnames(df)[which(names(df)==input$submitWhite_Blood_Cell_CountName)] <- "White_Blood_Cell_Count"
      colnames(df)[which(names(df)==input$submitPlatelet_CountName)] <- "Platelet_Count"
      colnames(df)[which(names(df)==input$submitAlbumin_LevelName)] <- "Albumin_Level"
      colnames(df)[which(names(df)==input$submitAlkaline_Phosphatase_LevelName)] <- "Alkaline_Phosphatase_Level"
      colnames(df)[which(names(df)==input$submitAlanine_Aminotransferase_LevelName)] <- "Alanine_Aminotransferase_Level"
      colnames(df)[which(names(df)==input$submitAspartate_Aminotransferase_LevelName)] <- "Aspartate_Aminotransferase_Level"
      colnames(df)[which(names(df)==input$submitCreatinine_LevelName)] <- "Creatinine_Level"
      colnames(df)[which(names(df)==input$submitLDH_LevelName)] <- "LDH_Level"
      colnames(df)[which(names(df)==input$submitCalcium_LevelName)] <- "Calcium_Level"
      colnames(df)[which(names(df)==input$submitPhosphorus_LevelName)] <- "Phosphorus_Level"
      colnames(df)[which(names(df)==input$submitGlucose_LevelName)] <- "Glucose_Level"
      colnames(df)[which(names(df)==input$submitPotassium_LevelName)] <- "Potassium_Level"
      colnames(df)[which(names(df)==input$submitSodium_LevelName)] <- "Sodium_Level"
      colnames(df)[which(names(df)==input$submitSmoking_Pack_YearsName)] <- "Smoking_Pack_Years"
    }
    df <- df %>%
      mutate(across(c(Family_History, Comorbidity_Diabetes, Comorbidity_Hypertension, 
                      Comorbidity_Heart_Disease, Comorbidity_Chronic_Lung_Disease, 
                      Comorbidity_Kidney_Disease, Comorbidity_Autoimmune_Disease, 
                      Comorbidity_Other), ~ ifelse(. == "Yes", 1, 0)))
    df_encoded <- dummy_cols(df, select_columns = c("Ethnicity","Smoking_History","Tumor_Location","Stage","Treatment"), remove_first_dummy = FALSE)
    df_encoded <- df_encoded %>% mutate(Survival=ifelse(Survival_Months>=60,2,ifelse(Survival_Months>=12,1,0)))
    return(df_encoded)
  })
  
  patientData <- reactive ({
      patientdf <- data.frame(
          Age = input$submitAge,
          Gender = input$submitGender,
          Smoking_History = input$submitSmoking_History,
          Tumor_Size_mm = input$submitTumor_Size_mm,
          Tumor_Location = input$submitTumor_Location,
          Stage = input$submitStage,
          Treatment = input$submitTreatment,
          Ethnicity = input$submitEthnicity,
          Family_History = input$submitFamily_History,
          Comorbidity_Diabetes = input$submitComorbidity_Diabetes,
          Comorbidity_Hypertension = input$submitComorbidity_Hypertension,
          Comorbidity_Heart_Disease = input$submitComorbidity_Heart_Disease,
          Comorbidity_Chronic_Lung_Disease = input$submitComorbidity_Chronic_Lung_Disease,
          Comorbidity_Kidney_Disease = input$submitComorbidity_Kidney_Disease,
          Comorbidity_Autoimmune_Disease = input$submitComorbidity_Autoimmune_Disease,
          Comorbidity_Other = input$submitComorbidity_Other,
          Perforance_Status=input$submitPerformance_Status,
          Blood_Pressure_Systolic = input$submitBlood_Pressure_Systolic,
          Blood_Pressure_Diastolic = input$submitBlood_Pressure_Diastolic,
          Blood_Pressure_Pulse = input$submitBlood_Pressure_Pulse,
          Hemoglobin_Level = input$submitHemoglobin_Level,
          White_Blood_Cell_Count = input$submitWhite_Blood_Cell_Count,
          Platelet_Count = input$submitPlatelet_Count,
          Albumin_Level = input$submitAlbumin_Level,
          Alkaline_Phosphatase_Level = input$submitAlkaline_Phosphatase_Level,
          Alanine_Aminotransferase_Level = input$submitAlanine_Aminotransferase_Level,
          Aspartate_Aminotransferase_Level = input$submitAspartate_Aminotransferase_Level,
          Creatinine_Level = input$submitCreatinine_Level,
          LDH_Level = input$submitLDH_Level,
          Calcium_Level = input$submitCalcium_Level,
          Phosphorus_Level = input$submitPhosphorus_Level,
          Glucose_Level = input$submitGlucose_Level,
          Potassium_Level = input$submitPotassium_Level,
          Sodium_Level = input$submitSodium_Level
      )
      
      patientdf <- patientdf %>%
        mutate(across(c(Family_History, Comorbidity_Diabetes, Comorbidity_Hypertension, 
                        Comorbidity_Heart_Disease, Comorbidity_Chronic_Lung_Disease, 
                        Comorbidity_Kidney_Disease, Comorbidity_Autoimmune_Disease, 
                        Comorbidity_Other), ~ ifelse(. == "Yes", 1, 0)))
      
      
      patientdf$`Stage_Stage I` <- 0
      patientdf$`Stage_Stage II` <- 0
      patientdf$`Stage_Stage III` <- 0
      patientdf$`Stage_Stage IV` <- 0
      if(patientdf$Stage[1]=="Stage I"){
        patientdf$`Stage_Stage I` = 1
      } else if(patientdf$Stage[1]=="Stage II"){
        patientdf$`Stage_Stage II` = 1
      } else if(patientdf$Stage[1]=="Stage III"){
        patientdf$`Stage_Stage III` = 1
      } else if(patientdf$Stage[1]=="Stage IV"){
        patientdf$`Stage_Stage IV` = 1
      }
      
      patientdf$`Ethnicity_African American` <- 0
      patientdf$`Ethnicity_Asian` <- 0
      patientdf$`Ethnicity_Caucasian` <- 0
      patientdf$`Ethnicity_Hispanic` <- 0
      patientdf$`Ethnicity_Other` <- 0
      if(patientdf$Ethnicity[1]=="African American"){
        patientdf$`Ethnicity_African American` = 1
      } else if(patientdf$Ethnicity[1]=="Asian"){
        patientdf$`Ethnicity_Asian` = 1
      } else if(patientdf$Ethnicity[1]=="Caucasian"){
        patientdf$`Ethnicity_Caucasian` = 1
      } else if(patientdf$Ethnicity[1]=="Hispanic"){
        patientdf$`Ethnicity_Hispanic` = 1
      } else if(patientdf$Ethnicity[1]=="Other"){
        patientdf$`Ethnicity_Other` = 1
      }
      
      patientdf$`Smoking_History_Never Smoked` <- 0
      patientdf$`Smoking_History_Former Smoker` <- 0
      patientdf$`Smoking_History_Current Smoker` <- 0
      if(patientdf$Smoking_History[1]=="Never Smoked"){
        patientdf$`Smoking_History_Never Smoked` = 1
      } else if(patientdf$Smoking_History[1]=="Former Smoker"){
        patientdf$`Smoking_History_Former Smoker` = 1
      } else if(patientdf$Smoking_History[1]=="Current Smoker"){
        patientdf$`Smoking_History_Current Smoker` = 1
      } 
      
      patientdf$`Treatment_Radiation Therapy` <- 0
      patientdf$`Treatment_Surgery` <- 0
      patientdf$`Treatment_Targeted Therapy` <- 0
      patientdf$`Treatment_Chemotherapy` <- 0
      if(patientdf$Smoking_History[1]=="Radiation Therapy"){
        patientdf$`Treatment_Radiation Therapy` = 1
      } else if(patientdf$Treatment[1]=="Treatment_Surgery"){
        patientdf$`Treatment_Surgery` = 1
      } else if(patientdf$Treatment[1]=="Targeted Therapy"){
        patientdf$`Treatment_Targeted Therapy` = 1
      } else if(patientdf$Treatment[1]=="Chemotherapy"){
        patientdf$`Treatment_Chemotherapy` = 1
      } 
      
      patientdf$`Tumor_Location_Upper Lobe` <- 0
      patientdf$`Tumor_Location_Middle Lobe` <- 0
      patientdf$`Tumor_Location_Lower Lobe` <- 0
      if(patientdf$Tumor_Location[1]=="Never Smoked"){
        patientdf$`Tumor_Location_Upper Lobe` = 1
      } else if(patientdf$Tumor_Location[1]=="Former Smoker"){
        patientdf$`Tumor_Location_Middle Lobe` = 1
      } else if(patientdf$Tumor_Location[1]=="Current Smoker"){
        patientdf$`Tumor_Location_Lower Lobe` = 1
      } 
      
        
      patientdf <- patientdf %>% select_if(~ !all(is.na(.)))
      
      return(patientdf)
  })
  
  listToTrainWith <- reactive({
    if(input$submitPredict){
      
      list <- setdiff(names(patientData()), c("Stage","Smoking_Pack_Years","Tumor_Location","Survival","Patient_ID", "Survival_Months","Gender","Ethnicity","Smoking_History","Insurance_Type","Treatment"))
      
    } else{
      list <- setdiff(names(data()), c("Stage","Smoking_Pack_Years","Tumor_Location","Survival","Patient_ID", "Survival_Months","Gender","Ethnicity","Smoking_History","Insurance_Type","Treatment"))
      
    }
    list
    
    
  })
  
  knnresult <- reactive({
    set.seed(42)
    dataset <- data()
    split <- sample.split(dataset$Survival,SplitRatio=0.8)
    training <- subset(dataset,split==TRUE)
    testing <- subset(dataset,split==FALSE)
    
    list1 <- listToTrainWith()
    
    knn_model <- knn(train = training[,list1],
                     test=testing[,list1],
                     cl=training$Survival, k=5
    )
    pred <- knn_model
    list(pred = pred, actual = testing$Survival, training = training)
    
  })
  
  output$knnConfusionMatrix <- renderTable({
    confusion_matrix <- table(Predicted = knnresult()$pred, Actual = knnresult()$actual)
    confusion_matrix
  })
  
  output$knnAccuracy <- renderText({
    confusion_matrix <- table(Predicted = knnresult()$pred, Actual = knnresult()$actual)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    paste0("Accuracy: ", round(accuracy*100, 2), "%")
  })
  
  output$knnPrediction <- reactive({
    set.seed(42)
    if (!input$submitPredict) {
      return(NULL)
    }
    pd <- patientData()[, listToTrainWith()]
    new_pred <- knn(train = knnresult()$training[, listToTrainWith()],
                    test = pd,
                    cl = knnresult()$training$Survival, k = 5)
    result <- "Lives for 5 years or more"
    if(new_pred==0){result = "Dies within 1 year"}
    if(new_pred==1){result = "Dies within 5 years"}
    paste("Prediction:", paste(result))
  })
  
  randForestResult <- reactive({
    set.seed(42)
    dataset <- data()
    split <- sample.split(dataset$Survival,SplitRatio=0.8)
    training <- subset(dataset,split==TRUE)
    testing <- subset(dataset,split==FALSE)
    training$Survival <- as.factor(training$Survival)
    testing$Survival <- as.factor(testing$Survival)
    
    list1 <- listToTrainWith()
    
    rf_model <- randomForest(x = training[,list1],
                     y=training$Survival, ntree=50
    )
    predictions <- predict(rf_model, newdata = testing[,list1])
    
    list(pred = rf_model, predictions = predictions, actual = testing$Survival, training = training)
    
  })
  
  output$rfConfusionMatrix <- renderTable({
    set.seed(42)
    predictions <- randForestResult()$predictions
    actuals <- randForestResult()$actual
    
    confusion_matrix <- table(Predicted = predictions, Actual = actuals)
    confusion_matrix
  })
  
  output$rfAccuracy <- renderText({
    set.seed(42)
    predictions <- randForestResult()$predictions
    actuals <- randForestResult()$actual
    
    confusion_matrix <- table(Predicted = predictions, Actual = actuals)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    paste0("Accuracy: ", round(accuracy*100, 2), "%")
  })
  
  output$rfPrediction <- reactive({
    set.seed(42)
    if (!input$submitPredict) {
      return(NULL)
    }
    pd <- patientData()[, listToTrainWith()]
    predictor = randForestResult()$pred
    new_pred <- predict(predictor, newdata = pd)
    result <- "Lives for 5 years or more"
    if(new_pred==0){result = "Dies within 1 year"}
    if(new_pred==1){result = "Dies within 5 years"}
    paste("Prediction:", paste(result))
  })
  
  
  
  
}

shinyApp(ui=ui,server=server)