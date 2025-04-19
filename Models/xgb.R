library(caret)
library(pROC)
library(dplyr)
library(mice)
library(xgboost)
library(Matrix)

setwd("C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy")
df<-read.csv("diabetes_cleaned2.csv")
# Step 1: Select and prepare data
df_log <- df[, c("blood_glucose_level", "HbA1c_level", "BMI", "Age",
                 "hypertension", "heart_disease", "DiabetesPedigreeFunction",
                 "gender", "smoking_history", "diabetes")]

df_log$gender <- as.factor(df_log$gender)
df_log$smoking_history <- as.factor(df_log$smoking_history)
df_log$diabetes <- as.factor(df_log$diabetes)

df_xgb <- model.matrix(diabetes ~ . -1, data = df_log)
labels <- as.numeric(as.character(df_log$diabetes))

# Train-test split
set.seed(123)

train_index <- createDataPartition(labels, p = 0.8, list = FALSE)
train_matrix <- xgb.DMatrix(data = df_xgb[train_index, ], label = labels[train_index])
test_matrix <- xgb.DMatrix(data = df_xgb[-train_index, ], label = labels[-train_index])

# XGBoost model parameters
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train model
xgb_model <- xgb.train(params = params, 
                       data = train_matrix, 
                       nrounds = 100, 
                       watchlist = list(eval = test_matrix, train = train_matrix), 
                       print_every_n = 10, 
                       early_stopping_rounds = 10)
xgb.save(xgb_model, "C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/Models/xgb_diabetes_model.model")

# Predict probabilities
xgb_probs <- predict(xgb_model, newdata = test_matrix)

# ROC and best threshold
roc_xgb <- roc(labels[-train_index], xgb_probs)
cat("Area under the curve:", auc(roc_xgb), "\n")

best_threshold <- as.numeric(coords(roc_xgb, "best", ret = "threshold"))
cat("Best Threshold:", best_threshold, "\n")
saveRDS(best_threshold, file = "C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/Models/xgb_threshold.rds")

# Predict using best threshold
xgb_preds_opt <- ifelse(xgb_probs > best_threshold, 1, 0)
xgb_preds_opt <- factor(xgb_preds_opt, levels = c(0, 1))
true_labels <- factor(labels[-train_index], levels = c(0, 1))

# Confusion matrix
conf_matrix_opt <- confusionMatrix(xgb_preds_opt, true_labels, positive = "1")
print(conf_matrix_opt)

# Plot ROC curve
plot(roc_xgb, main = "ROC Curve - XGBoost", col = "darkred")

