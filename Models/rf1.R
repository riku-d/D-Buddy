library(caret)
library(pROC)
library(dplyr)
library(mice)
library(randomForest)

setwd("C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy")
df1<-read.csv("diabetes_cleaned2.csv")
# Step 1: Select and prepare data
df_log <- df[, c("blood_glucose_level", "HbA1c_level", "BMI", "Age",
                 "hypertension", "heart_disease", "DiabetesPedigreeFunction",
                 "gender", "smoking_history", "diabetes")]

# Convert categorical variables to factors
df_log$gender <- as.factor(df_log$gender)
df_log$smoking_history <- as.factor(df_log$smoking_history)
df_log$diabetes <- as.factor(df_log$diabetes)

set.seed(123)
train_index <- createDataPartition(df_log$diabetes, p = 0.8, list = FALSE)
train_data <- df_log[train_index, ]
test_data <- df_log[-train_index, ]


# Step 2: Prepare the data
# Ensure diabetes is a factor for classification
train_data$diabetes <- as.factor(train_data$diabetes)
test_data$diabetes <- as.factor(test_data$diabetes)

# Step 3: Train the Random Forest model
set.seed(123)  # For reproducibility
rf_model <- randomForest(diabetes ~ ., 
                         data = train_data, 
                         ntree = 500, 
                         mtry = 3, 
                         importance = TRUE)
saveRDS(rf_model, "C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/Models/rf_diabetes_model.rds")
# Step 4: Predict on test data
rf_preds <- predict(rf_model, newdata = test_data)  # Predicted classes
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[,2]  # Probabilities for class "1"

# Step 5: Evaluate with Confusion Matrix
conf_matrix_rf <- confusionMatrix(rf_preds, test_data$diabetes, positive = "1")
print(conf_matrix_rf)

# Step 6: ROC Curve and AUC
roc_rf <- roc(as.numeric(as.character(test_data$diabetes)), rf_probs)
cat("Area under the curve:", auc(roc_rf), "\n")
plot(roc_rf, main = "ROC Curve - Random Forest", col = "darkgreen")

