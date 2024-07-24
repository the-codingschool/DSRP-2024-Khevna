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




## Looking for animalies 
## total 20 num/ int columns 

summary(data$age) # 1

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#30.00   42.00   54.00   54.44   67.00   79.00 
#mean off by =  -0.44 #slightly right skewed 

summary(data$tumor_size_mm)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#10.00   32.97   55.30   55.38   78.19   99.99 
#mean off by = -.38 #slightly right skewed

summary(data$survival_months)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   30.00   60.00   59.86   89.00  119.00 
#median off by = 0.14 left skewed 

#summary(data$performance_status) ??????????

#data$performance_status ## what is this ?????????

summary(data$blood_pressure_systolic)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#90.0   112.0   134.0   134.5   157.0   179.0 
# mean off by  -.5 right skewed 

summary(data$blood_pressure_diastolic)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#60.00   72.00   85.00   84.48   97.00  109.00 
#mean off by .52 left skewed 

summary(data$blood_pressure_pulse)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#60.00   70.00   80.00   79.59   90.00   99.00 
#mean off by 0.1

summary(data$hemoglobin_level)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#10.00   11.99   13.98   14.00   16.00   18.00 
#mean off by - .02

summary(data$white_blood_cell_count)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.501   5.109   6.730   6.736   8.354  10.000 
#mean off by  6.730  - 6.736 =  -0.006

summary(data$platelet_count)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#150.0   224.9   299.9   299.9   375.4   450.0 

#d = 0

summary(data$albumin_level)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.000   3.505   4.000   3.999   4.499   5.000 

#d = .001 


#### easier way summary(data[,25:30])

## input this into a table 

##create my table 

#new_data <- data.frame(
#variable_name = c('age', 
#'tumor_size_mm', 
#'survival_months', 
#'blood_pressure_systolic', 
#'blood_pressure_diastolic',
#'blood_pressure_pulse',
#'hemoglobin_level',
#'white_blood_cell_count',
#'platelet_count',
#'albumin_level',
#'alkaline_phosphatase_level',
#'alanine_aminotransferase_level',
#'aspartate_aminotransferase_level',
#'creatinine_level',
#'ldh_level',
#'calcium_level',
#'phosphorus_level',
#'glucose_level',
#'potassium_level',
#'sodium_level',
#'smoking_pack_years'),
#minimum = c(30.00 , 10.00, 1.00, 90.0 , ),
#first_quartile = c(88.0, 85.0),
#median = c(550, 500),
#mean = c(0.1, 0.11)
#thrid_quartile = c(0.1, 0.11)
#maximum = 
#)

summary(data$age)
summary(data$tumor_size_mm)
summary(data$survival_months)

df = summary(data[, 21:38])

head(df)

xlab='Age  Tumor_Size_mm  Survival_Months  
        blood_pressure_systolic blood_pressure_pulse. alkaline_phosphatase_level'

ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

#ggplot(data$age, aes(x = factor(tumor_size_mm), y = Age)) +
#geom_boxplot()

ggplot(data$age, aes(x=dose, y=len, fill=supp)) +
  geom_boxplot()


# Sample data frame
data <- data.frame(
  Age = c(23, 45, 56),
  Tumor_Size = c(10, 20, 30),
  Survival_Months = c(12, 24, 36)
)

# Loop through each column and plot a bar plot
for (col_name in names(data)) {
  column_data <- data[[col_name]]
  
  # Create a data frame for plotting
  plot_data <- data.frame(Category = col_name, Value = column_data)
  
  # Generate bar plot
  p <- ggplot(plot_data, aes(x = Category, y = Value)) +
    geom_bar(stat = "identity") +
    ggtitle(paste("Bar Plot of", col_name)) +
    xlab(col_name) +
    ylab("Value")
  
  # Print the plot
  print(p)
}

























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





