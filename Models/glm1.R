# Load required libraries
library(caret)
library(pROC)
library(dplyr)
library(mice)

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

# Step 2: Train-test split
set.seed(123)
train_index <- createDataPartition(df_log$diabetes, p = 0.8, list = FALSE)
train_data <- df_log[train_index, ]
test_data <- df_log[-train_index, ]

# Step 3: Train Logistic Regression model
log_model <- glm(diabetes ~ ., data = train_data, family = "binomial")
save(log_model, file = "C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/Models/log_diabetes_model.RData")


# Step 4: Predict on test set
log_probs <- predict(log_model, newdata = test_data, type = "response")

log_preds <- ifelse(log_probs > 0.5, 1, 0)
log_preds <- factor(log_preds, levels = c(0, 1))

# Step 5: Evaluate model
conf_matrix <- confusionMatrix(log_preds, test_data$diabetes)
print(conf_matrix)

# Step 6: ROC and AUC
roc_obj <- roc(as.numeric(as.character(test_data$diabetes)), log_probs)
auc(roc_obj)
plot(roc_obj, main = "ROC Curve - Logistic Regression", col = "blue")
