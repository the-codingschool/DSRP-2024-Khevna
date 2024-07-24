data = read.csv("/Users/labibawajihasiddiquei/Downloads/lung_cancer_data.csv")

library(titanic)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)


data = data.frame(data)
head(data)
data = clean_names(data)
names(data) ### changed the names from upper case to lower case 

str(data) # data.frame':	23658 obs. of  38 variables:

data = na.omit(data) 
str(data) ## 'data.frame':	23658 obs. of  38 variables:

## there are no missig data points 

get_dupes(data) ## no duplicates found either 

checking_age = data %>%
  group_by(treatment) %>%
  summarise(med_age = median(age),
            mean_age = mean(age)) %>%
  mutate(differnece = med_age -  mean_age) 
  
checking_age

data %>%
  group_by(treatment) %>%  
  summarise(med_tumor_size_mm = median(tumor_size_mm),
            mean_tumor_size_mm = mean(tumor_size_mm)) %>%
  mutate(differnece = med_tumor_size_mm -  mean_tumor_size_mm) 


names(data)

boxplot(data$age, data$tumor_size_mm, data$survival_months, 
        data$blood_pressure_systolic, 
        data$blood_pressure_diastolic,
        data$blood_pressure_pulse, 
        data$alkaline_phosphatase_level, 
        data$alanine_aminotransferase_level, 
        data$aspartate_aminotransferase_level, 
        data$ldh_level, 
        data$glucose_level, 
        data$smoking_pack_years,
        names = c("Age", "Tumor Size", "Survival Months", "BP Systolic", "BP Diastolic", 
                  "BP Pulse", "Alkaline Phosphatase", "Alanine Aminotransferase", 
                  "Aspartate Aminotransferase", "LDH Level", "Glucose Level", 
                  "Smoking Pack Years"),
        main = "Boxplot: Checking for Outliers", 
        ylab='Scale',
        las = 2 # Rotate x-axis labels for better readability
        )

boxplot(data$performance_status, 
        data$hemoglobin_level, 
        data$white_blood_cell_count, 
        data$albumin_level, 
        data$creatinine_level,
        data$calcium_level, 
        data$phosphorus_level,
        data$potassium_level, 
        main = "Boxplot: Checking for Outliers", 
        names = c("Performace Status", "Hemoglobin Level", " White Blood Cell Count", 
                  "Albumin Level", "Creatinine Level", "Calcium Level", 
                  "Phosphorus Level", 
                  "Potassium Level"),
        ylab = "Scale", 
        las = 2)

boxplot(data$platelet_count, main = "Boxplot: Checking for Outliers",
       ylab='Scale', xlab='Platelet Count')

boxplot(data$sodium_level,  main = "Boxplot: Checking for Outliers",
        ylab='Scale', xlab='Sodium Level')

##### no anomalies found ##########

#######.  Visualizing the data ##### 

ggplot(data, aes(x = treatment, fill = treatment)) +
  geom_bar() +
  labs(title = "Counts of Patients by Treatment Type",
       x = "Treatment Type",
       y = "Count")

ggplot(data, aes(x = stage, fill = stage)) +
  geom_bar() +
  labs(title = "Counts of Patients by Cancer Stage",
       x = "Cancer Stage",
       y = "Count")

ggplot(data, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Counts of Patients by Gender",
       x = "Gender",
       y = "Count")

names(data)

ggplot(data, aes(x = smoking_history, fill = smoking_history)) +
  geom_bar() +
  labs(title = "Counts of Patients by Smoking History",
       x = "Smoking History Categories",
       y = "Count")

ggplot(data, aes(x = tumor_location, fill = tumor_location)) +
  geom_bar() +
  labs(title = "Counts of Patients by Tumor Location",
       x = "Tumor Location",
       y = "Count")

ggplot(data, aes(x = ethnicity, fill = ethnicity)) +
  geom_bar() +
  labs(title = "Counts of Patients by Ethnicity",
       x = "Ethnicity",
       y = "Count")

ggplot(data, aes(x = ethnicity, fill = ethnicity)) +
  geom_bar() +
  labs(title = "Counts of Patients by Ethnicity",
       x = "Ethnicity",
       y = "Count")

ggplot(data, aes(x = insurance_type, fill = insurance_type)) +
  geom_bar() +
  labs(title = "Counts of Patients by Insurance Type",
       x = "Insurance Type",
       y = "Count")


ggplot(data, aes(x = insurance_type, fill = insurance_type)) +
  geom_bar() +
  labs(title = "Counts of Patients by Insurance Type",
       x = "Insurance Type",
       y = "Count")


ggplot(data, aes(x = family_history, fill = family_history)) +
  geom_bar() +
  labs(title = "Counts of Patients by Family History",
       x = "Family History",
       y = "Count")

ggplot(data, aes(x = comorbidity_diabetes, fill = comorbidity_diabetes)) +
  geom_bar() +
  labs(title = "Counts of Patients with Diabetes",
       x = "Diabetes (yes or no)",
       y = "Count")

ggplot(data, aes(x = comorbidity_hypertension, fill = comorbidity_hypertension)) +
  geom_bar() +
  labs(title = "Counts of Patients with Hypertension",
       x = "Hypertension (yes or no)",
       y = "Count")

ggplot(data, aes(x = comorbidity_heart_disease, fill = comorbidity_heart_disease)) +
  geom_bar() +
  labs(title = "Counts of Patients with Heart Disease",
       x = "Heart Disease (yes or no)",
       y = "Count")
ggplot(data, aes(x = comorbidity_chronic_lung_disease, fill =  comorbidity_chronic_lung_disease)) +
  geom_bar() +
  labs(title = "Counts of Patients with Chronic Lung Disease",
       x = "Chronic Lung Disease (yes or no)",
       y = "Count")

ggplot(data, aes(x = comorbidity_kidney_disease, fill = comorbidity_kidney_disease)) +
  geom_bar() +
  labs(title = "Counts of Patients with Kidney Disease",
       x = "Kidney Disease (yes or no)",
       y = "Count")

ggplot(data, aes(x = comorbidity_autoimmune_disease, fill = comorbidity_autoimmune_disease)) +
  geom_bar() +
  labs(title = "Counts of Patients with Autoimmune Disease",
       x = "Autoimmune Disease (yes or no)",
       y = "Count")

ggplot(data, aes(x = comorbidity_other, fill = comorbidity_other)) +
  geom_bar() +
  labs(title = "Counts of Patients with other Diseases",
       x = "Other Diseases (yes or no)",
       y = "Count")

#### find a easier way to do this ########

ggplot(data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Column", y = "Row", title = "Example Heatmap")

