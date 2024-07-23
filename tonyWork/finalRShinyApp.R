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

set.seed(42)

align_columns <- function(patient_df, training_df) {
  # Get the columns of the training data
  training_cols <- names(training_df)
  
  # Add missing columns to the patient data with default values
  for (col in setdiff(training_cols, names(patient_df))) {
    patient_df[[col]] <- 0
  }
  
  # Ensure columns are in the same order as in training data
  patient_df <- patient_df[training_cols]
  
  return(patient_df)
}


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
                    selectInput("submitTreatment","Select Treatment",c("Surgery","Radiation Therapy", "Chemotherapy", "Targeted Therapy"))
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
             )
           
           
         )
               
               
               
    )
  
)
server <- function(input,output){
  
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
      
      list <- setdiff(names(patientData()), c("Stage","Performance_Status","Smoking_Pack_Years","Tumor_Location","Survival","Patient_ID", "Survival_Months","Stage","Gender","Ethnicity","Smoking_History","Insurance_Type","Treatment"))
      
    } else{
      list <- setdiff(names(data()), c("Stage","Performance_Status","Smoking_Pack_Years","Tumor_Location","Survival","Patient_ID", "Survival_Months","Stage","Gender","Ethnicity","Smoking_History","Insurance_Type","Treatment"))
      
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
                     y=training$Survival, ntree=100
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