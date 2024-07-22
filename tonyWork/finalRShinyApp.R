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
                  ),
                  
                  
              )
          )
               
               
               
    )
  
)
server <- function(input,output){
  
  dataInput <- reactive ({
    if(is.null(input$inputCSV)){
        df <- read.csv("lung_cancer_data.csv")
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
    
    
    
  })
  
  
}

shinyApp(ui=ui,server=server)