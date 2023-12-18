source('tidydata.R')

library(caret)
library(dplyr)
library(MLmetrics)

fitdata_prepared <- fitdata %>%
     select(-booking_id) %>% 
     mutate_at(
          vars(months_as_member,weight,days_before),
          scale) %>%  # Scale numerical variables
     mutate(
          day_of_week = as.numeric(day_of_week),  # Convert factors to numeric
          time = as.numeric(time),
          category = as.numeric(category)
     )

set.seed(42)
training_indices <- createDataPartition(fitdata_prepared$attended, p = 0.8, list = FALSE)

train_data <- fitdata_prepared[training_indices, ]
test_data <- fitdata_prepared[-training_indices, ]

model <- glm(attended ~ ., data = train_data, family = "binomial")

train_pred <- predict(model, train_data, type = "response")
train_pred <- ifelse(train_pred > 0.5, 1, 0) %>% factor()

test_pred <- predict(model, test_data, type = "response")
test_pred <- ifelse(test_pred > 0.5, 1, 0) %>% factor()

y_train <- as.factor(train_data$attended)
y_test <- as.factor(test_data$attended)

# conf_matrix <- confusionMatrix(as.factor(predictions), test_data$attended)
# fourfoldplot(conf_matrix$table, color = c("#CC6666", "#9999CC"), conf.level = 0, margin = 1)

#Classification Report
train_summary <- confusionMatrix(train_pred, y_train)
train_summary

#F-1 Score
train_f1_score <- F1_Score(train_pred, y_train)
cat("Train F1-Score:", train_f1_score, "\n")

#Test Results

#Classification Report
test_summary <- confusionMatrix(test_pred, y_test)
test_summary

#F-1 Score
test_f1_score <- F1_Score(test_pred, y_test)
cat("Test F1-Score:", test_f1_score, "\n")

