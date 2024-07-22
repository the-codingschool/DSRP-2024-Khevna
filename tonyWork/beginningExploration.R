library(utils)
library(ggcorrplot)
library(dplyr)
# Get csv file

data <- read.csv("lung_cancer_data.csv")

# We want to see which variables are most correlated with each other

numericalData = select(data,c("Age", "Tumor_Size_mm","Survival_Months","Blood_Pressure_Systolic","Blood_Pressure_Diastolic",
                              "Blood_Pressure_Pulse","Hemoglobin_Level","White_Blood_Cell_Count","Platelet_Count",
                              "Albumin_Level","Alkaline_Phosphatase_Level","Alanine_Aminotransferase_Level",
                              "Aspartate_Aminotransferase_Level","Creatinine_Level","LDH_Level","Calcium_Level",
                              "Phosphorus_Level","Glucose_Level","Potassium_Level","Sodium_Level","Smoking_Pack_Years"))
head(numericalData)
correlation_matrix <- cor(numericalData,use = "complete.obs")
print(correlation_matrix)
ggcorrplot(correlation_matrix)