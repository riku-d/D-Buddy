# Load necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# Load dataset
df2 <- read.csv("C:/Users/rohit/OneDrive/Desktop/D-Buddy/diabetes_012_health_indicators_BRFSS2015.csv")

# 1. Basic structure and summary
str(df2)
summary(df2)

# 2. Check for missing values
colSums(is.na(df2))

# 3. Diabetes Distribution (0 = No, 1 = Pre-diabetes, 2 = Diabetes)
ggplot(df2, aes(x = factor(Diabetes), fill = factor(Diabetes))) +
  geom_bar() +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  labs(title = "Diabetes Distribution", x = "Diabetes Type", y = "Count")

# 4. Distribution of BMI
ggplot(df2, aes(x = BMI)) +
  geom_histogram(fill = "purple", bins = 30, color = "black") +
  labs(title = "BMI Distribution", x = "BMI", y = "Count")

# 5. Impact of Smoking on Diabetes
ggplot(df2, aes(x = factor(Smoker), fill = factor(Diabetes))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  labs(title = "Impact of Smoking on Diabetes", x = "Smoker (0 = No, 1 = Yes)", y = "Proportion")

# 6. Correlation Heatmap (Numerical Features)
numeric_cols2 <- df2 %>%
  select_if(is.numeric)

corr_matrix2 <- cor(numeric_cols2)
corrplot(corr_matrix2, method = "color", type = "upper", tl.col = "black", tl.srt = 45)


# Convert Diabetes column to factor for better visualization
df2$Diabetes <- factor(df2$Diabetes, levels = c(0, 1, 2), 
                           labels = c("No Diabetes", "Pre-Diabetes", "Diabetes"))

# 1. Smoking History vs Diabetes
ggplot(df2, aes(x = factor(Smoker), fill = Diabetes)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  labs(title = "Diabetes Count by Smoking History", x = "Smoker (0 = No, 1 = Yes)", y = "Count", fill = "Diabetes Status")

# 2. Physical Activity vs Diabetes
ggplot(df2, aes(x = factor(PhysActivity), fill = Diabetes_012)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  labs(title = "Diabetes Count by Physical Activity", x = "Physical Activity (0 = No, 1 = Yes)", y = "Count", fill = "Diabetes Status")

# 3. General Health Status vs Diabetes
ggplot(df2, aes(x = factor(GenHlth), fill = Diabetes)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  labs(title = "Diabetes Count by General Health Status", x = "General Health Rating (1 = Excellent, 5 = Poor)", y = "Count", fill = "Diabetes Status")


# 5. BMI vs Diabetes (Boxplot)
ggplot(df2, aes(x = Diabetes, y = BMI, fill = Diabetes)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  labs(title = "BMI Distribution by Diabetes Status", x = "Diabetes Status", y = "BMI")

# 6. Age vs Diabetes (Histogram)
ggplot(df2, aes(x = Age, fill = Diabetes)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  labs(title = "Age Distribution by Diabetes Status", x = "Age", y = "Count", fill = "Diabetes Status")

# 7. High Blood Pressure vs Diabetes
ggplot(df2, aes(x = factor(HighBP), fill = Diabetes)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  labs(title = "Diabetes Count by High Blood Pressure", x = "High Blood Pressure (0 = No, 1 = Yes)", y = "Count", fill = "Diabetes Status")

# 8. High Cholesterol vs Diabetes
ggplot(df2, aes(x = factor(HighChol), fill = Diabetes)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("blue", "orange", "red")) +
  labs(title = "Diabetes Count by High Cholesterol", x = "High Cholesterol (0 = No, 1 = Yes)", y = "Count", fill = "Diabetes Status")
