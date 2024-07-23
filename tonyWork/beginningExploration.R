library(utils)
library(ggcorrplot)
library(dplyr)
# Get csv file
# 1. Read the data from a CSV file.
data <- read.csv("lung_cancer_data.csv")

# 2. Print the first few rows of the data.
head(data)

# 3. Print the last few rows of the data.
tail(data)

# 4. Determine the datatype of each column.
sapply(data, class)

# 5. Determine the number of rows and columns in the Data.
dim(data)

# 6. Identify the most common and unique values in categorical columns.
categoricalData=select(data,c("Gender","Smoking_History","Tumor_Location","Stage","Treatment","Ethnicity","Performance_Status","Insurance_Type"))
sapply(categoricalData, function(col) {
  names(sort(table(col), decreasing = TRUE))[1]  # Most common value
})
sapply(categoricalData, function(col) {
  names(sort(table(col), increasing = TRUE))[1]  # Most unique value
})

# 7. Calculate the number of unique values in categorical column.
sapply(categoricalData, function(col) length(unique(col)))

# 8. Explore the mean, median, and sum for numerical columns. These statistics can provide insights into the overall distribution of the data, which can be useful in understanding the characteristics of the dataset and helping to make data-driven decisions.
numericalData = select(data,c("Age", "Tumor_Size_mm","Survival_Months","Blood_Pressure_Systolic","Blood_Pressure_Diastolic",
                              "Blood_Pressure_Pulse","Hemoglobin_Level","White_Blood_Cell_Count","Platelet_Count",
                              "Albumin_Level","Alkaline_Phosphatase_Level","Alanine_Aminotransferase_Level",
                              "Aspartate_Aminotransferase_Level","Creatinine_Level","LDH_Level","Calcium_Level",
                              "Phosphorus_Level","Glucose_Level","Potassium_Level","Sodium_Level","Smoking_Pack_Years"))

means <- sapply(numericalData,mean)
medians <- sapply(numericalData,median)
sums <- sapply(numericalData,sum)

means
medians
sums

# 9. Find the variance and standard deviation
variances <- sapply(numericalData,var)
sds <- sapply(numericalData,sd)

variances
sds

# Other: We want to see which variables are most correlated with each other
correlation_matrix <- cor(numericalData,use = "complete.obs")
ggcorrplot(correlation_matrix)

boxplot(data$Survival_Months)
hist(data$Survival_Months)