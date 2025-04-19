
# Load required libraries
library(dplyr)

# Load your dataset
setwd("C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/Datasets")
d1<-read.csv("diabetes_prediction_dataset.csv")

colnames(d1)[colnames(d1) == "bmi"] <- "BMI"
colnames(d1)[colnames(d1) == "age"] <- "Age"

# Blood Glucose Cleaning
d1$blood_glucose_level[d1$blood_glucose_level == 0] <- median(d1$blood_glucose_level[d1$blood_glucose_level > 0])

# BMI: Replace 0 and outliers with median
d1 <- d1 %>% filter(BMI > 0 & BMI < 50)

# Age: Keep between 1 and 80
d1 <- d1 %>% filter(Age > 0 & Age < 80)


# Save cleaned dataset
write.csv(d1, "C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/diabetes_cleaned1.csv", row.names = FALSE)

