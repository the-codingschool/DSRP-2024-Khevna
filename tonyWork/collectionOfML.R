set.seed(42)
dataset <- data
split <- sample.split(dataset$diagnosis,SplitRatio=0.7)
training <- subset(dataset,split==TRUE)
testing <- subset(dataset,split==FALSE)
training$diagnosis <- as.factor(training$diagnosis)
testing$diagnosis <- as.factor(testing$diagnosis)



trainingSubset <- training %>% select(-c("concavity_mean","concave.points_mean","perimeter_worst","radius_mean","perimeter_mean","perimeter_se","area_worst","area_mean","area_se","texture_mean"))
testingSubset <- testing %>% select(-c("concavity_mean","concave.points_mean","perimeter_worst","radius_mean","perimeter_mean","perimeter_se","area_worst","area_mean","area_se","texture_mean"))



rf_model <- randomForest(x = training[,-1],
                         y = training$diagnosis, ntree=50)

predictions <- predict(rf_model, newdata = testing[,-1])

actuals <- testing$diagnosis

confusion_matrix <- table(Predicted = predictions, Actual = actuals)

accuracyrf1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracyrf1*100, 2), "%")


rf_model <- randomForest(x = trainingSubset[,-1],
                         y = trainingSubset$diagnosis, ntree=50)

predictions <- predict(rf_model, newdata = testingSubset[,-1])

actuals <- testingSubset$diagnosis

confusion_matrix <- table(Predicted = predictions, Actual = actuals)

accuracyrf2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracyrf2*100, 2), "%")

knn_model <- knn(train = training[,-1], test=testing[,-1],cl=training$diagnosis, k=5)

actuals <- testing$diagnosis

confusion_matrix <- table(Predicted = knn_model, Actual = actuals)

accuracyknn1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracyknn1*100, 2), "%")

knn_model <- knn(train = trainingSubset[,-1], test=testingSubset[,-1],cl=training$diagnosis, k=5)

actuals <- testingSubset$diagnosis

confusion_matrix <- table(Predicted = knn_model, Actual = actuals)

accuracyknn2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracyknn2*100, 2), "%")


svm_model <- svm(diagnosis ~ ., data = training, kernel = "radial", cost = 1, scale = TRUE)

predictions <- predict(svm_model, testing)
confusion_matrix <- table(Predicted = predictions, Actual = testing$diagnosis)

accuracysvm1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracysvm1*100, 2), "%")

svm_model <- svm(diagnosis ~ ., data = trainingSubset, kernel = "radial", cost = 1, scale = TRUE)

predictions <- predict(svm_model, testingSubset)
confusion_matrix <- table(Predicted = predictions, Actual = testingSubset$diagnosis)

accuracysvm2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracysvm2*100, 2), "%")

dt_model <- rpart(diagnosis ~ ., data = training, method = "class")
predictions <- predict(dt_model, testing, type = "class")
confusion_matrix <- table(Predicted = predictions, Actual = testing$diagnosis)
accuracydt1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracydt1*100, 2), "%")


dt_model <- rpart(diagnosis ~ ., data = trainingSubset, method = "class")
predictions <- predict(dt_model, testingSubset, type = "class")
confusion_matrix <- table(Predicted = predictions, Actual = testingSubset$diagnosis)
accuracydt2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracydt2*100, 2), "%")


nb_model <- naiveBayes(diagnosis ~ ., data = training)
predictions <- predict(nb_model, testing)
confusion_matrix <- table(Predicted = predictions, Actual = testing$diagnosis)
accuracynb1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracynb1*100, 2), "%")


nb_model <- naiveBayes(diagnosis ~ ., data = trainingSubset)
predictions <- predict(nb_model, testingSubset)
confusion_matrix <- table(Predicted = predictions, Actual = testingSubset$diagnosis)
accuracynb2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracynb2*100, 2), "%")


lr_model <- glm(diagnosis ~ ., data = training, family = binomial)
predictions_prob <- predict(lr_model, testing, type = "response")
predictions <- ifelse(predictions_prob > 0.5, "M", "B")
predictions <- factor(predictions, levels = levels(testing$diagnosis))

confusion_matrix <- table(Predicted = predictions, Actual = testing$diagnosis)
accuracylr1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracylr1*100, 2), "%")


lr_model <- glm(diagnosis ~ ., data = trainingSubset, family = binomial)
predictions_prob <- predict(lr_model, testingSubset, type = "response")
predictions <- ifelse(predictions_prob > 0.5, "M", "B")
predictions <- factor(predictions, levels = levels(testingSubset$diagnosis))

confusion_matrix <- table(Predicted = predictions, Actual = testingSubset$diagnosis)
accuracylr2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracylr2*100, 2), "%")


ab_model <- boosting(diagnosis ~ ., data = training, mfinal = 50)
predictions <- predict(ab_model, newdata = testing)

confusion_matrix <- table(Predicted = predictions$class, Actual = testing$diagnosis)
accuracyab1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracyab1*100, 2), "%")


ab_model <- boosting(diagnosis ~ ., data = trainingSubset, mfinal = 50)
predictions <- predict(ab_model, newdata = testingSubset)

confusion_matrix <- table(Predicted = predictions$class, Actual = testingSubset$diagnosis)
accuracyab2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

confusion_matrix
cat("Accuracy: ", round(accuracyab2*100, 2), "%")


training1 <- training
testing1 <- testing
training1$diagnosis <- as.numeric(training1$diagnosis) - 1
testing1$diagnosis <- as.numeric(testing1$diagnosis) - 1

gb_model <- gbm(
  formula = diagnosis ~ ., 
  data = training1, 
  distribution = "bernoulli", 
  n.trees = 100, 
  interaction.depth = 3, 
  shrinkage = 0.01, 
  n.minobsinnode = 10
)


predictions_prob <- predict(gb_model, newdata = testing1, n.trees = 100, type = "response")
predictions <- ifelse(predictions_prob > 0.5, 1, 0) 

predictions <- factor(predictions, levels = c(0, 1))
actual <- factor(testing1$diagnosis, levels = c(0, 1))

confusion_matrix <- table(Predicted = predictions, Actual = actual)
accuracygb1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


print(confusion_matrix)
cat("Accuracy: ", round(accuracygb1 * 100, 2), "%\n")


gb_model <- gbm(
  formula = diagnosis ~ ., 
  data = training1, 
  distribution = "bernoulli", 
  n.trees = 100, 
  interaction.depth = 3, 
  shrinkage = 0.01, 
  n.minobsinnode = 10
)

predictions_prob <- predict(gb_model, newdata = testing1, n.trees = 100, type = "response")
predictions <- ifelse(predictions_prob > 0.5, 1, 0)  

predictions <- factor(predictions, levels = c(0, 1))
actual <- factor(testing1$diagnosis, levels = c(0, 1))


confusion_matrix <- table(Predicted = predictions, Actual = actual)
accuracygb2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

print(confusion_matrix)
cat("Accuracy: ", round(accuracygb2 * 100, 2), "%\n")


mlp_model <- nnet(
  diagnosis ~ ., 
  data = training1, 
  size = 10,       
  decay = 0.01,       
  maxit = 200,        
  linout = FALSE      
)

predictions_prob <- predict(mlp_model, newdata = testing1, type = "raw")
predictions <- ifelse(predictions_prob > 0.5, 1, 0)  

predictions <- factor(predictions, levels = c(0, 1))
actual <- factor(testing1$diagnosis, levels = c(0, 1))

confusion_matrix <- table(Predicted = predictions, Actual = actual)
accuracymlp1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

print(confusion_matrix)
cat("Accuracy: ", round(accuracymlp1 * 100, 2), "%\n")


mlp_model <- nnet(
  diagnosis ~ ., 
  data = training1, 
  size = 10,       
  decay = 0.01,       
  maxit = 200,        
  linout = FALSE      
)

predictions_prob <- predict(mlp_model, newdata = testing1, type = "raw")
predictions <- ifelse(predictions_prob > 0.5, 1, 0)  

predictions <- factor(predictions, levels = c(0, 1))
actual <- factor(testing1$diagnosis, levels = c(0, 1))

confusion_matrix <- table(Predicted = predictions, Actual = actual)
accuracymlp2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

print(confusion_matrix)
cat("Accuracy: ", round(accuracymlp2 * 100, 2), "%\n")


model1 <- nnet(
  diagnosis ~ ., 
  data = training1, 
  size = 10,          
  decay = 0.01,      
  maxit = 200,        
  linout = FALSE      
)

predictions_prob1 <- predict(model1, newdata = testing1, type = "raw")
testing1$pred1 <- ifelse(predictions_prob1 > 0.5, 1, 0)

train_predictions_prob1 <- predict(model1, newdata = training1, type = "raw")
training1$pred1 <- ifelse(train_predictions_prob1 > 0.5, 1, 0)

model2 <- nnet(
  diagnosis ~ . + pred1, 
  data = training1, 
  size = 10,        
  decay = 0.01,       
  maxit = 200,     
  linout = FALSE      
)

predictions_prob2 <- predict(model2, newdata = testing1, type = "raw")
predictions2 <- ifelse(predictions_prob2 > 0.5, 1, 0)

predictions2 <- factor(predictions2, levels = c(0, 1))
actual <- factor(testing1$diagnosis, levels = c(0, 1))

confusion_matrix <- table(Predicted = predictions2, Actual = actual)
accuracyncc1 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

print(confusion_matrix)
cat("Accuracy: ", round(accuracyncc1 * 100, 2), "%\n")


model1 <- nnet(
  diagnosis ~ ., 
  data = training1, 
  size = 10,          
  decay = 0.01,      
  maxit = 200,        
  linout = FALSE      
)

predictions_prob1 <- predict(model1, newdata = testing1, type = "raw")
testing1$pred1 <- ifelse(predictions_prob1 > 0.5, 1, 0)

train_predictions_prob1 <- predict(model1, newdata = training1, type = "raw")
training1$pred1 <- ifelse(train_predictions_prob1 > 0.5, 1, 0)

model2 <- nnet(
  diagnosis ~ . + pred1, 
  data = training1, 
  size = 10,        
  decay = 0.01,       
  maxit = 200,     
  linout = FALSE      
)

predictions_prob2 <- predict(model2, newdata = testing1, type = "raw")
predictions2 <- ifelse(predictions_prob2 > 0.5, 1, 0)

predictions2 <- factor(predictions2, levels = c(0, 1))
actual <- factor(testing1$diagnosis, levels = c(0, 1))

confusion_matrix <- table(Predicted = predictions2, Actual = actual)
accuracyncc2 <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

print(confusion_matrix)
cat("Accuracy: ", round(accuracyncc2 * 100, 2), "%\n")

accuracies <- tibble(
  Model = c("rf", "knn", "svm", "dt", "nb", "lr", "ab", "gb", "ncc", "mlp"),
  All_Variables = c(round(accuracyrf1 * 100, 2), round(accuracyknn1 * 100, 2), round(accuracysvm1 * 100, 2),
                    round(accuracydt1 * 100, 2), round(accuracynb1 * 100, 2), round(accuracylr1 * 100, 2),
                    round(accuracyab1 * 100, 2), round(accuracygb1 * 100, 2), round(accuracyncc1 * 100, 2),
                    round(accuracymlp1 * 100, 2)),
  Subset = c(round(accuracyrf2 * 100, 2), round(accuracyknn2 * 100, 2), round(accuracysvm2 * 100, 2),
             round(accuracydt2 * 100, 2), round(accuracynb2 * 100, 2), round(accuracylr2 * 100, 2),
             round(accuracyab2 * 100, 2), round(accuracygb2 * 100, 2), round(accuracyncc2 * 100, 2),
             round(accuracymlp2 * 100, 2))
)

accuracies_long <- accuracies %>%
  pivot_longer(cols = c("All_Variables", "Subset"), 
               names_to = "Metric", 
               values_to = "Accuracy") %>%
  mutate(Metric = recode(Metric, "All_Variables" = "All", "Subset" = "Sub"))

rbind(accuracy_dataframe,accuracies_long)
