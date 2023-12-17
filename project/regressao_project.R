# Load necessary libraries
library(caret)
library(dplyr)

# Step 1: Load the dataset
path = "C:/Users/Darla/Downloads/Acadêmico/Técnico Lisboa/Disciplinas/Análise Multivariada/IST-multivariate-analysis-project/project/fitness_class_2212.csv"
data <- read.csv(path)

# Step 2: Data Preprocessing
# Handling missing values in 'weight'
data$weight[is.na(data$weight)] <- median(data$weight, na.rm = TRUE)

# Convert 'day_of_week' and 'time' to factors
data$day_of_week <- as.factor(data$day_of_week)
data$time <- as.factor(data$time)

# One-hot encoding for 'category'
dummy_model <- dummyVars("~ .", data = data)
data_transformed <- data.frame(predict(dummy_model, newdata = data))

# Ensure 'attended' is a factor
data_transformed$attended <- as.factor(data$attended)

# Step 3: Splitting the Dataset
set.seed(42) # For reproducibility
training_indices <- createDataPartition(data_transformed$attended, p = 0.8, list = FALSE)
train_data <- data_transformed[training_indices, ]
test_data <- data_transformed[-training_indices, ]

# Step 4: Model Training
model <- glm(attended ~ ., data = train_data, family = "binomial")

# Step 5: Model Evaluation
predictions <- predict(model, test_data, type = "response")
predictions <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix(as.factor(predictions), test_data$attended)
