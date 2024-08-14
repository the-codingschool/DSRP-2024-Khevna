

library(ggplot2)
library(dplyr)
library(janitor)
library(tidyr)
library(titanic)

data = read.csv("/Users/alinaahmed/Documents/DRSP Lung Cancer Research/Alina A. Project 2024/Data Coding for Breast Cancer/breast_cancer_classification_data.csv")
head(data)

col_na_count <- colSums(is.na(data))
print(col_na_count)
# Remove the 'X' column
data <- data %>%
  select(-X)

data = clean_names(data)
names(data)

#569 NA values found
nrow(data)

data = na.omit(data)
str(data) #'data.frame':	0 obs. of  33 variables:
nrow(data)
mean(data$perimeter_mean)
mean(data$radius_mean)
mean(data$area_mean)


# everything is fine, no missing values so far

summary(data)

library(ggplot2)
library(tidyverse)

summary_stats <- data %>%
  summarise(
    mean_perimeter = mean(perimeter_mean),
    sd_perimeter = sd(perimeter_mean),
    n = n(),
    se_perimeter = sd_perimeter / sqrt(n)
  )



# Filter data for malignant and benign diagnoses
malignant_data<- filter(breast_cancer_classification_data, diagnosis == "M")
benign_data <- filter(breast_cancer_classification_data, diagnosis == "B")

# Randomly sample 100 rows from each filtered dataset
malignant_data <- sample_n(malignant_data, 100)
benign_data <- sample_n(benign_data, 100)

# Combine the samples into one dataframe
final_sample <- bind_rows(malignant_data, benign_data)
  
# Check the dimensions of the final sample
dim(data)
# Check the distribution of diagnoses in the final sample
table(data$diagnosis)

breast_cancer_classification_data

ggplot(final_sample, aes(x=diagnosis, y=perimeter_mean))+ 
  geom_bar(stat="summary", fill="lightsalmon", alpha=1)+
  geom_errorbar(stat = "summary", width=0.2,colour="black", size=0.5)+
  theme_minimal() + 
  labs(title="Bar Graph of Average Tumor Perimeter Based on Diagnosis", 
       x="Diagnosis", 
       y="Perimeter Mean") + 
  ylim(0, 150)


max_value <- max(benign_data$perimeter_mean, na.rm = TRUE)
row_with_max_value <- benign_data[benign_data$perimeter_mean == max_value, ]
row_with_max_value

# END OF FIRST GRAPH!!!!

ggplot(final_sample, aes(x=diagnosis, y=radius_mean))+ 
  geom_bar(stat = "summary", fill ="olivedrab2") +
  geom_errorbar(stat = "summary", width=0.2,colour="black", size=0.5)+
  theme_minimal() + 
  labs(title="Bar Graph of Average Tumor Radius Based on Diagnosis", 
       x="Diagnosis", 
       y="Average Radius") + 
  ylim(0, 25)

# END OF SECOND GRAPH!!!!

ggplot(final_sample, aes(x=diagnosis, y=area_mean))+ 
  geom_bar(stat = "summary", fill ="plum1") +
  geom_errorbar(stat = "summary", width=0.2,colour="black", size=0.5)+
  theme_minimal() + 
  labs(title="Bar Graph of Average Tumor Area Based on Diagnosis", 
       x="Diagnosis", 
       y="Area Mean") + 
  ylim(0, 1000)

# END OF THIRD GRAPH!!!

table(final_sample$area_mean)

str(final_sample)

# Plotting Heatmap in R

# adding ggplot2 library for plotting
library(ggplot2)

# Plotting Heatmap in R

# adding ggplot2 library for plotting
library(ggplot2)

view(final_sample)

Diagnosis <- c("Benign", "Malignant")
Dimensions <- c("Perimeter Mean", "Mean Radius", "Mean Area")

final_sample <- expand.grid(X=Diagnosis, Y=Dimensions)
final_sample <- runif(440, 0, 6)

# plotting the heatmap
plt <- ggplot(final_sample,aes( X= diagnosis, Y= Dimensions,fill=count))
plt <- plt + geom_tile()

# further customizing the heatmap by
# applying colors and title
plt <- plt + theme_minimal()

# setting gradient color as red and white
plt <- plt + scale_fill_gradient(low="white", high="red")

# setting the title and subtitles using
# title and subtitle
plt <- plt + labs(title = "Heatmap")
plt <- plt + labs(subtitle = "A simple heatmap using geom_tile()")

# setting x and y labels using labs
plt <- plt + labs(x ="Alphabets", y ="Random column names")

# plotting the Heatmap
plt
(final_sample)
ggplot(final_sample, aes(x=Diagnosis, y=Dimensions, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="steelblue") +
  theme_minimal() +
  labs(x="Columns", y="Rows", fill="Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(breast_cancer_classification_data, "Breast Cancer Dataset")

library(reshape2)
install.packages("corrplot")

library(reshape2)
library(corrplot)

numeric_data <- data[, -c(1, 2)]
correlation_matrix <- cor(numeric_data)
correlation_melted <- melt(correlation_matrix)

data <- read.csv("/Users/alinaahmed/Documents/DRSP Lung Cancer Research/Alina A. Project 2024/Data Coding for Breast Cancer/breast_cancer_classification_data.csv")
head(data)
col_na_count <- colSums(is.na(data))
print(col_na_count)
# Remove the 'X' column
data <- data %>%
  select(-X)

data = clean_names(data)
names(data)

#569 NA values found
nrow(data)

data = na.omit(data)
str(data) #'data.frame':	0 obs. of  33 variables:
nrow(data)

data <- read.csv("/Users/alinaahmed/Documents/DRSP Lung Cancer Research/Alina A. Project 2024/Data Coding for Breast Cancer/breast_cancer_classification_data.csv")
head(data)

numeric_data <- data[, -c("perimeter_mean","radius_mean","area_mean", "perimeter_mean")]
correlation_matrix <- cor(numeric_data)
correlation_melted <- melt(correlation_matrix)

ggplot(correlation_melted, aes(x=Var2, y=Var1, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1)) +
  theme_minimal() +
  labs(x="Features", y="Features", fill="Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

selected_data <- data[ c("texture_mean", "compactness_mean", "radius_mean", "area_mean", )]
correlation_matrix <- cor(selected_data)
correlation_melted <- melt(correlation_matrix)


ggplot(correlation_melted, aes(x=Var1, y=Var1, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="oldlace", high="palegreen4", mid="palegreen", midpoint=0, limit=c(-1,1)) +
  theme_minimal() +
  labs(x="Dimensions of Benign Cells", y="Texture", fill="Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

selected_data <- final_sample[, c("perimeter_mean", "radius_mean", "area_mean","texture_mean", "concavity_mean", "smoothness_mean","compactness_mean", "symmetry_mean")]
correlation_matrix <- cor(selected_data)
correlation_melted <- melt(correlation_matrix)

ggplot(correlation_melted, aes(x=Var2, y=Var1, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="oldlace", high="palegreen4", mid="white", midpoint=0, limit=c(-1,1)) +
  theme_minimal() +
  labs(x="Features", y= "Features", fill="Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

install.packages("chisquare")
# load the MASS package

print(str(selected_data))

selected_data

# Create a data frame from the main data set.
statistical_data1 = data.frame(selected_data$area_mean,selected_data$concavity_mean)

# Create a contingency table with the needed variables.		 
statistical_data1 = table(selected_data$perimeter_mean,selected_data$concavity_mean)

print(statistical_data1)
print(chisq.test(statistical_data1))
### chisq of area + concave = 0.2

statistical_data4 = table(selected_data$perimeter_mean,selected_data$compactness_mean)
print(statistical_data4)
print(chisq.test(statistical_data4))

# applying chisq.test() function
print(chisq.test(statistical_data1))
### X-squared = 288198, df = 288368, p-value = 0.588

statistical_data2 = data.frame(selected_data$concavity_mean,selected_data$compactness_mean)
print(statistical_data2)
print(chisq.test(statistical_data2))

## X-squared = 6.3622, df = 568, p-value = 1 for concavity + compactness
statistical_data3 = final_sample

# loading required packages

# package to perform data manipulation
# and visualization
library(tidyverse)

# package to compute
# cross - validation methods
library(caret)

# installing package to
# import desired dataset
install.packages("datarium")

# loading the dataset
data <- final_sample
data



# loading required packages 

# package to perform data manipulation 
# and visualization 
library(tidyverse) 

# package to compute 
# cross - validation methods 
library(caret) 

# package Used to split the data 
# used during classification into 
# train and test subsets 
library(caTools) 

# loading package to 
# import desired dataset 
library(ISLR)


install.packages("infer")
install.packages("mosaic")
install.packages("Stat2Data")
install.packages("skimr")
 library(tidyverse)
library(infer)
library(mosaic)
library(Stat2Data)
library(skimr)

data(data)
view(data)
set.seed(569)
which_train <- sample(1:569, size = 512, replace = FALSE)

training <- data %>%
  slice(which_train)

testing <- data %>%
  slice(-which_train)
install.packages("caret")
library(caret)

breast_cancer_classification_data
set.seed(100)
library(tidyr)
library(tidyverse)
library(caret)

# Installing the package
install.packages("dplyr")

# Loading package
library(dplyr)

# Summary of dataset in package
summary(data)

# Installing the package

# For Logistic regression
install.packages("caTools") 

# For ROC curve to evaluate model
install.packages("ROCR")	 

# Loading package
library(caTools)
library(ROCR)


# Splitting dataset
split <- sample.split(final_sample, SplitRatio = 0.9)
split

train_reg <- subset(final_sample, split == "TRUE")
test_reg <- subset(data, split == "FALSE")

# Training model
logistic_model <- glm(vs ~ wt + disp,
                      data = train_reg,
                      family = "binomial")
logistic_model

# Summary
summary(logistic_model)



# R program to implement
# K-fold cross-validation
install.packages("caret")
install.packages("lattice")
library(lattice)
library(caret)
library(tidyr)
# setting seed to generate a 
#Load the necessary libraries and data:

library(caret)
library(randomForest)
# Load the dataset
data <-  read.csv("/Users/alinaahmed/Documents/DRSP Lung Cancer Research/Alina A. Project 2024/Data Coding for Breast Cancer/breast_cancer_classification_data.csv")

# Drop column X from the dataset
data <- data[, !(names(data) %in% "X")]

# Convert the diagnosis column to a factor
data$diagnosis <- as.factor(data$diagnosis)

str(data)

# Check for missing values
missing_counts <- sapply(data, function(x) sum(is.na(x)))
print(missing_counts)

# Remove rows with any missing values
data_clean <- na.omit(data)



# Verify that the column has been dropped
print(names(data_clean))

# Verify there are no missing values
print(sum(is.na(data_clean)))

# Set up training control for k-fold cross-validation
k <- 10
train_control <- trainControl(method = "cv", number = k)

# Train the model with cleaned data
model <- train(diagnosis ~ ., data = data_clean, method = "rf", trControl = train_control)

# Print the model results
print(model)


# END OF CROSS VALIDATION TEST!!




# Installing the package
install.packages("dplyr")

# Loading package
library(dplyr)

# Summary of dataset in package
summary(data_clean)

# Installing the package

# For Logistic regression
install.packages("caTools") 

# For ROC curve to evaluate model
install.packages("ROCR")	 

# Loading package
library(caTools)
library(ROCR)

# Splitting dataset
split <- sample.split(data_clean, SplitRatio = 0.8)
split

train_reg <- subset(data_clean, split == "TRUE")
test_reg <- subset(data_clean, split == "FALSE")

# Training model

logistic_model <- glm(diagnosis~ concavity_mean + perimeter_mean,
                      data = train_reg,
                      family = "binomial")
logistic_model

# Summary
summary(logistic_model)

library(tidyverse)
library(caret)
library(ISLR)
# Set up training control for k-fold cross-validation
k <- 10
train_control <- trainControl(method = "cv", number = k)
# Convert the target variable to binary (1 if M, 0 if B)
data_clean$diagnosis <- as.factor(data$diagnosis)
data_clean$diagnosis <- ifelse(data$diagnosis == "M", 1, 0)
# Set a seed for reproducibility
set.seed(123)
# Split the data: 80% training, 20% testing
trainIndex <- createDataPartition(data_clean$diagnosis, p = .8, list = FALSE, times = 1)
cancerTrain <- data_clean[trainIndex, ]
cancerTest <- data_clean[-trainIndex, ]

# Training model
logistic_model <- train(diagnosis ~ ., data = cancerTrain, method = "glm", family =
                          "binomial", trControl = train_control)
# Summary – provides the equation of your model (y = mx + b)
summary(logistic_model)
# Predict on the testing set
predictions <- predict(logistic_model, newdata = cancerTest)
# Convert probabilities to binary predictions
binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
# Confusion matrix
conf_matrix <- confusionMatrix(as.factor(binary_predictions),
                               as.factor(cancerTest$diagnosis))
# Print the confusion matrix
print(conf_matrix)

# Load the dataset
data <- read.csv("/Users/alinaahmed/Documents/DRSP Lung Cancer Research/Alina A. Project 2024/Data Coding for Breast Cancer/breast_cancer_classification_data.csv")

# Drop column X from the dataset
data <- data[, !(names(data) %in% "X")]

# Convert the diagnosis column to a factor
data$diagnosis <- as.factor(data$diagnosis)

str(data)

# Check for missing values
missing_counts <- sapply(data, function(x) sum(is.na(x)))
print(missing_counts)

# Remove rows with any missing values
data_clean <- na.omit(data)

# Verify that the column has been dropped
print(names(data_clean))

# Verify there are no missing values
print(sum(is.na(data_clean)))

# Set up training control for k-fold cross-validation
k <- 10
train_control <- trainControl(method = "cv", number = k)

# Train the model with cleaned data using logistic regression
model <- train(diagnosis ~ ., data = data_clean, method = "glm", family = "binomial", trControl = train_control)

print(model)


# Installing the package
install.packages("dplyr")

# Loading package
library(dplyr)

# Summary of dataset in package
summary(data_clean)

# Installing the package

# For Logistic regression
install.packages("caTools") 

# For ROC curve to evaluate model
install.packages("ROCR")	 

# Loading package
library(caTools)
library(ROCR)

# Splitting dataset
split <- sample.split(data_clean, SplitRatio = 0.8)
split

train_reg <- subset(data_clean$diagnosis, split == "TRUE")
test_reg <- subset(data_clean$diagnosis, split == "FALSE")

# Training model

logistic_model <- glm(formula = diagnosis ~ concavity_mean + area_mean,
                      data = train_reg,
                      family = "binomial")
logistic_model

# Summary
summary(logistic_model)

predict_reg <- predict(logistic_model,
                       test_reg, type = "response")
predict_reg


# Load necessary library
library(tidyverse)  # For data manipulation and visualization
# Load data
data <- read.csv("your_data.csv")

# View the first few rows of the dataset
head(data)
# Convert outcome variable to a factor if it's not already
data$outcome <- as.factor(data$outcome)

# Check the structure of the data
str(data)
# Set seed for reproducibility
set.seed(123)

# Split data (80% training, 20% testing)
train_indices <- sample(seq_len(nrow(data_clean)), size = 0.8 * nrow(data))
train_data <- data_clean[train_indices, ]
test_data <- data_clean[-train_indices, ]
# Fit logistic regression model
model <- glm(diagnosis ~ perimeter_mean + concavity_mean + compactness_mean, 
             data = train_data, 
             family = binomial)

# Summary of the model
summary(model)

# Predict probabilities on test data
predictions <- predict(model, test_data, type = "response")

# Convert probabilities to binary outcome (assuming threshold of 0.5)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$diagnosis)

# Print confusion matrix
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(predicted_classes == test_data$outcome) / nrow(test_data)
print(paste("Accuracy:", accuracy))

# Install pROC package if not already installed
# install.packages("pROC")

library(pROC)

# Compute ROC curve
roc_curve <- roc(test_data$diagnosis, predictions)
plot(roc_curve)

# Print AUC
print(paste("AUC:", auc(roc_curve)))
# Plot ROC curve with additional customization
plot(roc_curve, 
     main = "ROC Curve for Diagnosis Prediction", 
     col = "black", 
     lwd = 2, 
     xlab = "False Positive Rate",  # X-axis label
     ylab = "True Positive Rate")    # Y-axis label

view(data_clean)
