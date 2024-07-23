data = read.csv("/Users/labibawajihasiddiquei/Downloads/lung_cancer_data.csv")

library(titanic)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)


data = clean_names(data)
names(data) ### changed the names from upper case to lower case 

str(data) # data.frame':	23658 obs. of  38 variables:

data = na.omit(data) 
str(data) ## 'data.frame':	23658 obs. of  38 variables:

## there are no missig data points 

get_dupes(data) ## no duplicates found either 

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

new_data <- data.frame(
  variable_name = c('age', 
                    'tumor_size_mm', 
                    'survival_months', 
                    'blood_pressure_systolic', 
                    'blood_pressure_diastolic',
                    'blood_pressure_pulse',
                    'hemoglobin_level',
                    'white_blood_cell_count',
                    'platelet_count',
                    'albumin_level',
                    'alkaline_phosphatase_level',
                    'alanine_aminotransferase_level',
                    'aspartate_aminotransferase_level',
                    'creatinine_level',
                    'ldh_level',
                    'calcium_level',
                    'phosphorus_level',
                    'glucose_level',
                    'potassium_level',
                    'sodium_level',
                    'smoking_pack_years'),
  minimum = c(30.00 , 10.00, 1.00, 90.0 , ),
  first_quartile = c(88.0, 85.0),
  median = c(550, 500),
  mean = c(0.1, 0.11)
  thrid_quartile = c(0.1, 0.11)
  maximum = 
)

summary(data$age)
summary(data$tumor_size_mm)
summary(data$survival_months)

df = summary(data[, 21:38])

head(df)

summarise(group_by(df, Min, 1st Qu, Median, Mean, 3rd Qu, Max.))




