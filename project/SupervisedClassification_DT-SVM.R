library(ggplot2)
library(lattice)
library(caret) 
library(rpart)
library(rpart.plot)
library(e1071)
library(MLmetrics)

source('tidydata.R')

#Dropping primary key column 'booking_id'
fitdata <- subset(fitdata, select = -booking_id) 

###############################################################################
##                     Split into Training and Testing set                    ##
################################################################################
set.seed(42)

# Specify the proportion of data to be used for testing (0.3 for a 70-30 split)
test_proportion <- 0.2

# Use createDataPartition to create indices for the training set
train_indices <- createDataPartition(fitdata$attended, 
                                     p = 1 - test_proportion, 
                                     list = FALSE)
#Creating Stratified Training And Test Datasets
train_data <- fitdata[train_indices, ]
test_data <- fitdata[-train_indices, ]

# Extract the targets
y_train <- as.factor(train_data$attended)
y_test <- as.factor(test_data$attended)

################################################################################
##                            Decision Tree                                   ##
################################################################################

dt_model <- rpart(attended ~ ., data = train_data, method = "class", minsplit=20, minbucket=7) #Uses Gini Index
print(dt_model)

#Plot the tree
rpart.plot(dt_model) 

#Making Predictions For The Test and Train Dataset
train_pred <- predict(dt_model, train_data, type = "class", levels = levels(y_train))
test_pred <- predict(dt_model, test_data, type = "class", levels = levels(y_test))


#Train Results

#Classification Report
train_summary <- confusionMatrix(train_pred, y_train) 
train_summary

#F-1 Score
train_f1_score <- F1_Score(train_pred,y_train)
cat("Train F1-Score:", train_f1_score, "\n")

#Test Results

#Classification Report
test_summary <- confusionMatrix(test_pred, y_test)
test_summary

#F-1 Score
test_f1_score <- F1_Score(test_pred,y_test)
cat("Test F1-Score:", test_f1_score, "\n")

################################################################################
##                           Linear SVM                                       ##
################################################################################

#Calculating Class Weights
class_counts <- table(train_data$attended)
class_weights <- 1 / prop.table(class_counts)

#Model Building 
SVM_model <- svm(
  formula = attended ~ .,
  data = train_data,
  class.weights = class_weights,			#Default Cost = 1
  kernel = "linear" 			
)

#Making Predictions
train_pred <- predict(SVM_model, train_data)
test_pred <- predict(SVM_model, test_data)

#Train Results

#Classification Report
train_summary <- confusionMatrix(train_pred, y_train)
train_summary

#F-1 Score
train_f1_score <- F1_Score(train_pred,y_train)
cat("Train F1-Score:", train_f1_score, "\n")

#Test Results

#Classification Report
test_summary <- confusionMatrix(test_pred, y_test)
test_summary

#F-1 Score
test_f1_score <- F1_Score(test_pred,y_test)
cat("Test F1-Score:", test_f1_score, "\n")
