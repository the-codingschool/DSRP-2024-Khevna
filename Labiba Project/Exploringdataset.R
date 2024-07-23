print("hello")

data = read.csv("/Users/labibawajihasiddiquei/Downloads/lung_cancer_data.csv")
head(data)
str(data)
summary(data)
ncol(data)
nrow(data)
#summary(data$Smoking_Pack_Years)

summary(data[, 20:30])

names(data)

unique(data[,10:19])
table(data[,10:19])
table(data[,10]) #gives names + counts of the obv under the names 
unique(data[,10]) #gives names of the unique columns

## create a new dataset with all the int first - num - char 

table(data$Age) # there's people from age 30 - 79 
# I can create five clusters of age 30 - 39 
# 40 - 49
# 50 - 59
# 60 - 69
# 70 - 79

names(data)

library(titanic)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)

?clean_names

?get_dupes

get_dupes(data,Patient_ID) ## there are no duplicate patients 

?remove_empty
?na.omit

exp_data = remove_empty(data) ## doesnt make any changes 
str(exp_data)

str(data)

exp_data = na.omit(data) ## doesnt make any changes 
str(exp_data)

clean_names(exp_data) ## doesnt make any changes
names(exp_data)

## sw1 = rename(starwars,'load_vehicles'= vehicles) how to rename 

under_fourty = filter(exp_data, Age > 40) ## seperating by age 
str(under_thirty) #data.frame':	18972 obs. of  38 variables:

table(data$Ethnicity)

#African American            Asian        Caucasian         Hispanic            Other 
# 4737                       4734             4765             4761             4661 

## can seperate the data using multiple variables - ethinicity , age clusters 

############# how does lung cancer vary among different ethinicites 

############ how does lung cancer - curability differ by age 

names(data)

table(data$Treatment)

##  Chemotherapy  Radiation Therapy    Surgery    Targeted Therapy 
##    5936              5966              5944         5812 

##### how does curability differ by treatment course - whats the best treamtne t 

## curability would be measured by number of months survived 

#table(data$Survival_Months)

Chemo_stagefour = filter(data, Treatment == 'Chemotherapy', Stage == 'Stage IV')
str(Chemo_stagefour) ##'data.frame':	1503 obs. of  38 variables:

table(data$Stage)

#head(data)

#Stage I  Stage II Stage III  Stage IV 
#5905      5820      5922      6011 

table(data$Treatment)

#Chemotherapy Radiation Therapy           Surgery 
#5936              5966              5944 
#Targeted Therapy 
#5812 

#### there's four stages and four types of treatment 
##-- sixteen sub-sections total

## my program would let the user input --- 
## What stage of cancer do you have - what treatment is your
## doctor following 
## plot these againt the number of survival months 
## the computer can predict for you 

## could use linear pregression or knn model for prediction 

########################knn format############################################

#names(data)

#new_data <- data.frame(
  #mean_radius = c(14.0, 12.5),
  #mean_texture = c(18.0, 12.5),
  #mean_perimeter = c(88.0, 85.0),
  #mean_area = c(550, 500),
  #mean_smoothness = c(0.1, 0.11)
#)

# Predict using the model
#new_predictions <- knn(train = train_data[, c('mean_radius', 'mean_texture', '
#mean_perimeter', 'mean_area', 'mean_smoothness')],
                       #test = new_data,
                       #cl = train_data$diagnosis, k = 5)

# View the predictions
# new_predictions

## 16 subsections - 16 models - all the models take all the variables into 
#account 

## the target variable would be survival months

### Question can knn train on categorical variables too ? 

names(data)

#[1] "Patient_ID"                       "Age"                             
#[3] "Gender"                           "Smoking_History"                 
#[5] "Tumor_Size_mm"                    "Tumor_Location"                  
#[7] "Stage"                            "Treatment"                       
#[9] "Survival_Months"                  "Ethnicity"                       
#[11] "Insurance_Type"                   "Family_History"                  
#[13] "Comorbidity_Diabetes"             "Comorbidity_Hypertension"        
#[15] "Comorbidity_Heart_Disease"        "Comorbidity_Chronic_Lung_Disease"
#[17] "Comorbidity_Kidney_Disease"       "Comorbidity_Autoimmune_Disease"  
#[19] "Comorbidity_Other"                "Performance_Status"              
#[21] "Blood_Pressure_Systolic"          "Blood_Pressure_Diastolic"        
#[23] "Blood_Pressure_Pulse"             "Hemoglobin_Level"                
#[25] "White_Blood_Cell_Count"           "Platelet_Count"                  
#[27] "Albumin_Level"                    "Alkaline_Phosphatase_Level"      
#[29] "Alanine_Aminotransferase_Level"   "Aspartate_Aminotransferase_Level"
#[31] "Creatinine_Level"                 "LDH_Level"                       
#[33] "Calcium_Level"                    "Phosphorus_Level"                
#[35] "Glucose_Level"                    "Potassium_Level"                 
#[37] "Sodium_Level"                     "Smoking_Pack_Years" 



#look for missing data
#standarized 
#making some general plots 

## outliers - summary - outliers - distribution compare the mean and the median

str(data)

#'data.frame':	23658 obs. of  38 variables:
#$ Patient_ID                      : chr  "Patient0000" "Patient0001" "Patient0002" "Patient0003" ...
#$ Age                             : int  68 58 44 72 37 50 68 48 52 40 ...
#$ Gender                          : chr  "Male" "Male" "Male" "Male" ...
#$ Smoking_History                 : chr  "Current Smoker" "Never Smoked" "Former Smoker" "Current Smoker" ...
#$ Tumor_Size_mm                   : num  81.7 78.4 67.7 70.8 87.3 ...
#$ Tumor_Location                  : chr  "Lower Lobe" "Lower Lobe" "Lower Lobe" "Lower Lobe" ...
#$ Stage                           : chr  "Stage III" "Stage I" "Stage I" "Stage III" ...
#$ Treatment                       : chr  "Surgery" "Radiation Therapy" "Chemotherapy" "Chemotherapy" ...
#$ Survival_Months                 : int  44 101 69 95 105 49 63 101 35 19 ...
#$ Ethnicity                       : chr  "Hispanic" "Caucasian" "African American" "African American" ...
#$ Insurance_Type                  : chr  "Medicare" "Private" "Other" "Medicare" ...
#$ Family_History                  : chr  "No" "Yes" "Yes" "Yes" ...
#$ Comorbidity_Diabetes            : chr  "Yes" "Yes" "No" "Yes" ...
#$ Comorbidity_Hypertension        : chr  "Yes" "Yes" "No" "No" ...
#$ Comorbidity_Heart_Disease       : chr  "Yes" "No" "No" "Yes" ...
#$ Comorbidity_Chronic_Lung_Disease: chr  "No" "No" "Yes" "No" ...
#$ Comorbidity_Kidney_Disease      : chr  "Yes" "Yes" "Yes" "Yes" ...
#$ Comorbidity_Autoimmune_Disease  : chr  "Yes" "Yes" "No" "Yes" ...
#$ Comorbidity_Other               : chr  "Yes" "No" "No" "No" ...
#$ Performance_Status              : int  3 4 0 1 0 4 3 3 4 1 ...
#$ Blood_Pressure_Systolic         : int  161 101 109 103 165 109 137 90 157 161 ...
#$ Blood_Pressure_Diastolic        : int  99 91 74 85 69 103 93 101 87 95 ...
#$ Blood_Pressure_Pulse            : int  92 93 81 68 99 68 89 95 67 71 ...
#$ Hemoglobin_Level                : num  13.5 16.8 14.5 17.4 13.5 ...
#$ White_Blood_Cell_Count          : num  9.8 4.38 6.16 6.26 5.2 ...
#$ Platelet_Count                  : num  322 252 393 275 382 ...
#$ Albumin_Level                   : num  3.57 3.7 4.71 4.73 4.61 ...
#$ Alkaline_Phosphatase_Level      : num  49.3 111.4 76.6 82 107.5 ...
#$ Alanine_Aminotransferase_Level  : num  27.99 30.12 5.88 38.91 26.34 ...
#$ Aspartate_Aminotransferase_Level: num  46.8 39.7 32.6 44.3 15.7 ...
#$ Creatinine_Level                : num  1.246 1.463 0.63 0.594 1.478 ...
#$ LDH_Level                       : num  239 234 169 214 118 ...
#$ Calcium_Level                   : num  10.37 10.08 8.66 8.83 9.25 ...
#$ Phosphorus_Level                : num  3.55 2.95 4.64 3.62 4.77 ...
#$ Glucose_Level                   : num  113.9 101.3 78.2 127.9 148.8 ...
#$ Potassium_Level                 : num  4.97 3.9 4.37 4.35 3.67 ...
#$ Sodium_Level                    : num  140 135 143 139 141 ...
#$ Smoking_Pack_Years              : num  17 93.3 70.3 19.8 81 ...

summary(data$Smoking_Pack_Years)

#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0168  25.0268  49.9262  49.9136  74.9246  99.9995 



































## suggested steps 


##8.     Explore the mean, median, and sum for numerical columns. 
#These statistics can provide insights into the overall distribution of the data, 
#which can be useful in understanding the characteristics of the dataset 
#and helping to make data-driven decisions.
#Note: The mean and median can give an idea of the typical or central value of a 
#variable, while the sum can indicate the total amount of a variable across all 
#observations. These statistics can be used to summarize the data, visualize the 
#data in a chart, and for inferential purposes (i.e., testing hypotheses or 
#making predictions).
#9.     Find the variance and standard deviation.





