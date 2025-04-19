

# Load required libraries
library(dplyr)

setwd("C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/Datasets")

# Load the dataset
d1 <- read.csv("diabetes.csv")

# Rename columns
colnames(d1)[colnames(d1) == "Glucose"] <- "blood_glucose_level"
colnames(d1)[colnames(d1) == "Outcome"] <- "diabetes"
d1$gender <- "Female"  # adding gender column as Female
d1 <- d1[, !(names(d1) %in% c("Insulin", "SkinThickness","BloodPressure","Pregnancies"))]

# Handle blood_glucose_level
d1$blood_glucose_level[d1$blood_glucose_level == 0] <- median(d1$blood_glucose_level[d1$blood_glucose_level > 0])



# DPF: Replace outliers with median
dpf_q1 <- quantile(d1$DiabetesPedigreeFunction, 0.25)
dpf_q3 <- quantile(d1$DiabetesPedigreeFunction, 0.75)
dpf_iqr <- dpf_q3 - dpf_q1
dpf_upper <- dpf_q3 + 1.5 * dpf_iqr
dpf_lower <- dpf_q1 - 1.5 * dpf_iqr
dpf_median <- median(d1$DiabetesPedigreeFunction)
d1$DiabetesPedigreeFunction[d1$DiabetesPedigreeFunction < dpf_lower | d1$DiabetesPedigreeFunction > dpf_upper] <- dpf_median

# BMI: Replace 0s and outliers with median
d1$BMI[d1$BMI == 0] <- NA
bmi_q1 <- quantile(d1$BMI, 0.25, na.rm = TRUE)
bmi_q3 <- quantile(d1$BMI, 0.75, na.rm = TRUE)
bmi_iqr <- bmi_q3 - bmi_q1
bmi_upper <- bmi_q3 + 1.5 * bmi_iqr
bmi_lower <- bmi_q1 - 1.5 * bmi_iqr
bmi_median <- median(d1$BMI, na.rm = TRUE)
d1$BMI[d1$BMI < bmi_lower | d1$BMI > bmi_upper] <- bmi_median
d1$BMI[is.na(d1$BMI)] <- bmi_median

# Age: Keep between 1 and 80
d1 <- d1 %>% filter(Age > 0 & Age <= 80)


# Save cleaned dataset
write.csv(d1, "C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/diabetes_cleaned.csv", row.names = FALSE)

