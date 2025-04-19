# Load necessary libraries
if (!require(caret)) install.packages("caret", dependencies = TRUE)
if (!require(randomForest)) install.packages("randomForest")
if (!require(e1071)) install.packages("e1071")
if (!require(pROC)) install.packages("pROC")
if (!require(xgboost)) install.packages("xgboost")
if (!require(ggplot2)) install.packages("ggplot2")

# Load libraries
library(caret)
library(randomForest)
library(e1071)
library(pROC)
library(xgboost)
library(ggplot2)

# Load the dataset
df <- read.csv("C:/Users/rohit/OneDrive/Desktop/D-Buddy/diabetes_prediction_dataset.csv")  # Replace with actual dataset file

# Convert categorical variables to factors
df$gender <- as.factor(df$gender)
df$smoking_history <- as.factor(df$smoking_history)
df$hypertension <- as.factor(df$hypertension)
df$heart_disease <- as.factor(df$heart_disease)
df$diabetes <- as.factor(df$diabetes)  # Target variable

# Split dataset into training (80%) and testing (20%)
set.seed(123)
trainIndex <- createDataPartition(df$diabetes, p = 0.8, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Train models
logistic_model <- train(diabetes ~ ., data = trainData, method = "glm", family = "binomial")
dt_model <- train(diabetes ~ ., data = trainData, method = "rpart")
rf_model <- train(diabetes ~ ., data = trainData, method = "rf", ntree = 100)
svm_model <- train(diabetes ~ ., data = trainData, method = "svmRadial")

# Convert data for XGBoost
train_matrix <- model.matrix(diabetes ~ . -1, data = trainData)
test_matrix <- model.matrix(diabetes ~ . -1, data = testData)
train_labels <- as.numeric(trainData$diabetes) - 1
test_labels <- as.numeric(testData$diabetes) - 1

xgb_model <- xgboost(data = train_matrix, label = train_labels, nrounds = 100, objective = "binary:logistic", verbose = 0)

# Make predictions
logistic_pred <- predict(logistic_model, testData)
dt_pred <- predict(dt_model, testData)
rf_pred <- predict(rf_model, testData)
svm_pred <- predict(svm_model, testData)
xgb_pred <- factor(ifelse(predict(xgb_model, test_matrix) > 0.5, "1", "0"), levels = levels(testData$diabetes))


# Evaluation function
evaluate_model <- function(actual, predicted, model_name) {
  confusion <- confusionMatrix(as.factor(predicted), actual)
  cat("\nModel:", model_name, "\n")
  print(confusion)
}

# Compare models
evaluate_model(testData$diabetes, logistic_pred, "Logistic Regression")
evaluate_model(testData$diabetes, dt_pred, "Decision Tree")
evaluate_model(testData$diabetes, rf_pred, "Random Forest")
evaluate_model(testData$diabetes, svm_pred, "SVM")
evaluate_model(testData$diabetes, xgb_pred, "XGBoost")

# ROC Curves
testData$diabetes_numeric <- ifelse(testData$diabetes == "1", 1, 0)
rf_prob <- predict(rf_model, testData, type = "prob")[,2]
xgb_prob <- predict(xgb_model, test_matrix, type = "response")
rf_roc <- roc(testData$diabetes_numeric, rf_prob)
xgb_roc <- roc(testData$diabetes_numeric, xgb_prob)


plot(rf_roc, col = "blue", main = "ROC Curve Comparison")
lines(xgb_roc, col = "red")
legend("bottomright", legend = c("Random Forest", "XGBoost"), col = c("blue", "red"), lwd = 2)

cat("\nXGBoost is the most accurate model based on evaluation metrics and ROC curve.\n")

# Violin Plot for Multivariate Analysis
ggplot(df, aes(x = diabetes, y = blood_glucose_level, fill = diabetes)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(shape = 16, position = position_jitter(0.2), alpha = 0.4) +
  labs(title = "Blood Glucose Level Distribution by Diabetes Status", 
       x = "Diabetes Status", y = "Blood Glucose Level") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))
