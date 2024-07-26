library(utils)
library(ggcorrplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(gridExtra)
library(paletteer)

# Get csv file
# 1. Read the data from a CSV file and set working directory (This may be different for you)
setwd("C:/Users/Tony/OneDrive/Documents/tonyR/DSRP-2024-Khevna/tonyWork")
data <- read.csv("../data/lung_cancer_data.csv")

# 2. Print the first few rows of the data.
head(data)

# 3. Print the last few rows of the data.
tail(data)

# 4. Determine the datatype of each column.
t(sapply(data, class))

# 5. Determine the number of rows and columns in the Data.
dim(data)

# 6. Identify the most common and unique values in categorical columns.
categoricalData=select(data,c("Gender","Smoking_History","Tumor_Location","Stage","Treatment","Ethnicity","Performance_Status","Insurance_Type"))
t(sapply(categoricalData, function(col) {
  names(sort(table(col), decreasing = TRUE))[1]  # Most common value
}))
t(sapply(categoricalData, function(col) {
  names(sort(table(col), increasing = TRUE))[1]  # Most unique value
}))

# 7. Calculate the number of unique values in categorical column.
t(sapply(categoricalData, function(col) length(unique(col))))

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
variances <- t(sapply(numericalData,var))
sds <- t(sapply(numericalData,sd))

variances
sds

# Other: We want to see which variables are most correlated with each other
correlation_matrix <- cor(numericalData,use = "complete.obs")

ggcorrplot(correlation_matrix)

boxplot(data$Survival_Months)
hist(data$Survival_Months)

# Plot each numerical variable as a boxplot to see if it is just boxplot being normal or not

par(mfrow = c(3, 7))  # 3 rows and 7 columns
for (i in 1:21) {
  boxplot(numericalData[, i], main = colnames(numericalData)[i], col = "lightblue")
}

for (i in 1:21) {
  hist(numericalData[, i], main = colnames(numericalData)[i], col = paletteer_d("RColorBrewer::Pastel2"))
}
# You can see that almost everything is evenly distributed. Some of them have extra or half on the ends.
# For example, Blood_Pressure_Pulse has double in the first bin and half in the last.

#Test Ethnicity vs Numerical Values
numericalDataEthnicity = select(data,c("Ethnicity","Age","Tumor_Size_mm","Survival_Months","Blood_Pressure_Systolic","Blood_Pressure_Diastolic",
                                       "Blood_Pressure_Pulse","Hemoglobin_Level","White_Blood_Cell_Count","Platelet_Count",
                                       "Albumin_Level","Alkaline_Phosphatase_Level","Alanine_Aminotransferase_Level",
                                       "Aspartate_Aminotransferase_Level","Creatinine_Level","LDH_Level","Calcium_Level",
                                       "Phosphorus_Level","Glucose_Level","Potassium_Level","Sodium_Level"))

longData <- gather(numericalDataEthnicity, key = "Variable", value = "Value", -Ethnicity)

# Create boxplots
ggplot(longData, aes(x = Ethnicity, y = Value)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of Variables by Ethnicity",
       x = "Ethnicity",
       y = "Value")


# Scatter plots for age vs other values colored by ethnicity

# Create and save individual scatter plots
par(mfrow = c(1, 1))
ggplot(numericalDataEthnicity, aes(x = Age, y = Tumor_Size_mm, color = Ethnicity)) +
  geom_point() +
  theme_bw() +
  labs(title = paste("Scatter Plot of Age vs Tumor Size Colored by Ethnicity"),
       x = "Age",
       y = "Tumor_Size_mm")


numericalDataStage = select(data,c("Stage","Tumor_Size_mm","Survival_Months","Blood_Pressure_Systolic","Blood_Pressure_Diastolic",
                                   "Blood_Pressure_Pulse","Hemoglobin_Level","White_Blood_Cell_Count","Platelet_Count",
                                   "Albumin_Level","Alkaline_Phosphatase_Level","Alanine_Aminotransferase_Level",
                                   "Aspartate_Aminotransferase_Level","Creatinine_Level","LDH_Level","Calcium_Level",
                                   "Phosphorus_Level","Glucose_Level","Potassium_Level","Sodium_Level"))


ggplot(numericalDataStage,aes(x=Stage,y=Glucose_Level)) + geom_boxplot()

long_data <- melt(data, variable.name = "Variable", value.name = "Value")

ggplot(long_data, aes(x = Stage, y = Value)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y", ncol = 5) +
theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(hjust = 0.5)) +
labs(title = "Boxplots of Numerical Variables vs Stage",
     x = "Stage",
     y = "Numerical Variable Value") 

ggplot(data,aes(x=Treatment,y=Survival_Months)) +
  geom_boxplot() + labs(title="Boxplot of Survival Months vs Treatment") +
  theme(plot.title = element_text(hjust = 0.5))

par(mfrow = c(2, 4))
for(i in 13:19) {
  counts <- table(data[, i])
  barplot(counts, main = colnames(data)[i])
}

data <- data %>% mutate(Survival=ifelse(Survival_Months>=60,2,ifelse(Survival_Months>=12,1,0)))
data$Survival <- as.factor(data$Survival)
ggplot(data,aes(x=Survival,y=Age,group=Survival)) + geom_boxplot() + labs(title = "Boxplot of Age by Survival Group",
                                                               x = "Survival Group",
                                                               y = "Age")

plot_list <- list()
for(comorbidity in c("Family_History","Comorbidity_Diabetes", "Comorbidity_Hypertension", "Comorbidity_Heart_Disease", "Comorbidity_Chronic_Lung_Disease", "Comorbidity_Kidney_Disease", "Comorbidity_Autoimmune_Disease", "Comorbidity_Other")){
  p <- ggplot(data, aes_string(x = "Stage", y = "Survival_Months", fill = comorbidity)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_list[[comorbidity]] <- p
}
do.call(grid.arrange, c(plot_list, ncol = 2))

plot_list <- list()
for (var in c("Gender","Smoking_History","Tumor_Location","Stage", "Insurance_Type")) {
  p <- ggplot(data, aes_string(x = var)) +
    geom_bar() +
    theme_minimal()
  
  plot_list[[var]] <- p
}
do.call(grid.arrange, c(plot_list, ncol = 2))

