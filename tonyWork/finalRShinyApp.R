library(ggplot2)
library(dplyr)
library(tidyverse)
library(shiny)
library(shinythemes)
library(tibble)
library(vroom)
library(fastDummies)

set.seed(42)

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
                    numericInput("submitPerformance_Status", "Select Performance_Status (0 to 4):",NULL),
                    numericInput("submitSmoking_Pack_Years", "Select Smoking_Pack_Years:",NULL)
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
             tabPanel("KNN",
                 checkboxInput("submitPredict","Predict with patient data?",FALSE)
             )
           
           
         )
               
               
               
    )
  
)
server <- function(input,output){
  
  data <- reactive ({
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
    df <- df %>%
      mutate(across(c(Family_History, Comorbidity_Diabetes, Comorbidity_Hypertension, 
                      Comorbidity_Heart_Disease, Comorbidity_Chronic_Lung_Disease, 
                      Comorbidity_Kidney_Disease, Comorbidity_Autoimmune_Disease, 
                      Comorbidity_Other), ~ ifelse(. == "Yes", 1, 0)))
    df_encoded <- dummy_cols(df, select_columns = c("Ethnicity","Gender","Smoking_History","Tumor_Location","Stage"), remove_first_dummy = TRUE)
    
    return(df_encoded)
  })
  
  patientData <- reactive ({
      patientdf <- data.frame(
          Age = submitAge,
          Gender = submitGender,
          Smoking_History = submitSmoking_History,
          Tumor_Size_mm = submitTumor_Size_mm,
          Tumor_Location = submitTumor_Location,
          Stage = submitStage,
          Ethnicity = submitEthnicity,
          Family_History = submitFamily_History,
          Comorbidity_Diabetes = submitComorbidity_Diabetes,
          Comorbidity_Hypertension = submitComorbidity_Hypertension,
          Comorbidity_Heart_Disease = submitComorbidity_Heart_Disease,
          Comorbidity_Chronic_Lung_Disease = submitComorbidity_Chronic_Lung_Disease,
          Comorbidity_Kidney_Disease = submitComorbidity_Kidney_Disease,
          Comorbidity_Autoimmune_Disease = submitComorbidity_Autoimmune_Disease,
          Comorbidity_Other = submitComorbidity_Other,
          Performance_Status = submitPerformance_Status,
          Smoking_Pack_Years = submitSmoking_Pack_Years,
          Blood_Pressure_Systolic = submitBlood_Pressure_Systolic,
          Blood_Pressure_Diastolic = submitBlood_Pressure_Diastolic,
          Blood_Pressure_Pulse = submitBlood_Pressure_Pulse,
          Hemoglobin_Level = submitHemoglobin_Level,
          White_Blood_Cell_Count = submitWhite_Blood_Cell_Count,
          Platelet_Count = submitPlatelet_Count,
          Albumin_Level = submitAlbumin_Level,
          Alkaline_Phosphatase_Level = submitAlkaline_Phosphatase_Level,
          Alanine_Aminotransferase_Level = submitAlanine_Aminotransferase_Level,
          Aspartate_Aminotransferase_Level = submitAspartate_Aminotransferase_Level,
          Creatinine_Level = submitCreatinine_Level,
          LDH_Level = submitLDH_Level,
          Calcium_Level = submitCalcium_Level,
          Phosphorus_Level = submitPhosphorus_Level,
          Glucose_Level = submitGlucose_Level,
          Potassium_Level = submitPotassium_Level,
          Sodium_Level = submitSodium_Level
      )
      patientdf <- patientdf %>%
        mutate(across(c(Family_History, Comorbidity_Diabetes, Comorbidity_Hypertension, 
                        Comorbidity_Heart_Disease, Comorbidity_Chronic_Lung_Disease, 
                        Comorbidity_Kidney_Disease, Comorbidity_Autoimmune_Disease, 
                        Comorbidity_Other), ~ ifelse(. == "Yes", 1, 0)))
      patientdf_encoded <- dummy_cols(patientdf, select_columns = c("Ethnicity","Gender","Smoking_History","Tumor_Location","Stage"), remove_first_dummy = TRUE)
      return(patientdf_encoded)
  })
  
  listToTrainWith <- reactive({
    if(submitPredict){
      colnames(patientData())[sapply(patientData(), function(col) any(!is.na(col)))]
    } else{
      p <- setdiff(names(data()), c("Patient_ID", "Survival_Months"))
      
    }
    
  })
  
  knnresult <- reactive({
    split <- sample.split(data()$Survival_Months,SplitRatio=0.8)
    training <- subset(data(),split==TRUE)
    testing <- subset(data(),split==FALSE)
    knn_model <- knn(train = training[,listToTrainWith()],
                     test=testing[,listToTrainWith()],
                     cl=training$Survival_Months, k=5
    )
    pred <- knn_model
    list(pred = pred, actual = testing$Survival_Months, training = training)
    
  })
  
  output$knnConfusionMatrix <- renderTable({
    confusion_matrix <- table(Predicted = knnresult()$pred, Actual = knnresult()$actual)
    confusion_matrix
  })
  
  output$knnAccuracyText <- renderText({
    accuracy <- sum(diag(knnConfusionMatrix())) / sum(knnConfusionMatrix())
    paste("Accuracy:", round(accuracy, 2))
  })
  
  output$knnPrediction <- reactive({
    new_pred <- knn(train = knnresult$training[, listToTrainWith()],
        test = patientData(),
        cl = knnresult$training$Survival_Months, k = 5)
    paste("Predictions:", paste(new_pred, collapse = ", "))
  })
  
  
  
}

shinyApp(ui=ui,server=server)