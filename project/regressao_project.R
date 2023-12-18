# Load necessary libraries
library(caret)
library(dplyr)
library(vcd)
library(ggplot2)
library(reshape2)
library(MLmetrics)
library(grid)


install.packages("grid")
install.packages("MLmetrics")

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
conf_matrix <- confusionMatrix(as.factor(predictions), test_data$attended)
fourfoldplot(conf_matrix$table, color = c("#CC6666", "#9999CC"), conf.level = 0, margin = 1)

rain_conf_matrix <- data.frame(
  Reference = factor(c("0", "0", "1", "1"), levels = c("0", "1")),
  Prediction = factor(c("0", "1", "0", "1"), levels = c("0", "1")),
  Count = c(748, 68,166, 192)
)

test_conf_matrix <- data.frame(
  Reference = factor(c("0", "0", "1", "1"), levels = c("0", "1")),
  Prediction = factor(c("0", "1", "0", "1"), levels = c("0", "1")),
  Count = c(192, 48 ,17, 42)
)

# Create the train confusion matrix plot
train_plot <- ggplot(train_conf_matrix, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "black") +
  scale_fill_gradient(low = "#FAEBDD", high = "#94A5D6") +
  labs(title = "Train Confusion Matrix",
       x = "Reference",
       y = "Prediction") +
  theme(plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1)

# Create the test confusion matrix plot
test_plot <- ggplot(test_conf_matrix, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "black") +
  scale_fill_gradient(low = "#FAEBDD", high = "#94A5D6") +
  labs(title = "Test Confusion Matrix",
       x = "Reference",
       y = "Prediction") +
  theme(plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1)

# Arrange the plots side by side
grid.arrange(train_plot, test_plot, nrow=1)

train_summary <- confusionMatrix(train_pred, y_train)
train_summary

#F-1 Score
train_f1_score <- F1_Score(as.factor(predictions), test_data$attended)
cat("Train F1-Score:", train_f1_score, "\n")

test_summary <- confusionMatrix(test_pred, y_test)
test_summary

#F-1 Score
test_f1_score <- F1_Score(test_pred,y_test)
cat("Test F1-Score:", test_f1_score, "\n")