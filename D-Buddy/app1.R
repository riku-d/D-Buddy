library(dplyr)
library(mice)
library(ggplot2)
library(corrplot)
library(GGally)

setwd("C:/Users/rohit/OneDrive/Desktop/D-Buddy/Datasets/diabetes_prediction")
df1<-read.csv("diabetes.csv")
df2<-read.csv("diabetes_prediction_dataset.csv")
str(df1)
str(df2)
colnames(df1)
colnames(df2)

df1$gender<-"Female"
colnames(df1)[colnames(df1) == "Glucose"] <- "blood_glucose_level"
colnames(df1)[colnames(df1) == "Outcome"] <- "diabetes"

colnames(df2)[colnames(df2) == "bmi"] <- "BMI"
colnames(df2)[colnames(df2) == "age"] <- "Age"

# Add missing columns to each dataframe with NA values
for (col in setdiff(colnames(df2), colnames(df1))) {
  df1[[col]] <- NA
}

for (col in setdiff(colnames(df1), colnames(df2))) {
  df2[[col]] <- NA
}


# Bind rows after aligning columns
df <- bind_rows(df1, df2)
colnames(df)


#Data Cleaning
df$Pregnancies[df$gender == "Male"] <- 0

# Check pattern of missing data
md.pattern(df)
imputed_data <- mice(df, method = "pmm", m = 1, seed = 500)
df <- complete(imputed_data)

df$BP_category <- cut(
  df$BloodPressure,
  breaks = c(-Inf, 59, 79, 89, 99, Inf),
  labels = c("Low", "Normal", "Elevated", "High Stage 1", "High Stage 2"),
  right = TRUE
)
table(df$BP_category)


df <- df[, !(colnames(df) %in% c("DiabetesPedigreeFunction","Pregnancies"))]
#df$hypertension[is.na(df$hypertension)] <- 0
#df$heart_disease[is.na(df$heart_disease)] <- 0

df$smoking_history <- as.character(df$smoking_history)
df$smoking_history[df$smoking_history %in% c("No Info") | is.na(df$smoking_history)] <- "Unknown"
df$smoking_history <- as.factor(df$smoking_history)

#df$HbA1c_level[is.na(df$HbA1c_level)] <- median(df$HbA1c_level, na.rm = TRUE)

#EDA
str(df)
summary(df)

table(df$diabetes)
prop.table(table(df$diabetes))

#Univariate analysis
ggplot(df, aes(x = blood_glucose_level)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(
    title = "Blood Glucose Distribution",
    x = "Glucose",
    y = "Frequency"
  ) +
  theme_minimal()
# Boxplot (horizontal)
ggplot(df, aes(x = blood_glucose_level)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  coord_flip() +
  labs(
    title = "Blood Glucose Boxplot",
    x = "",
    y = "Glucose Level"
  ) +
  theme_minimal()


#Bar Graph
ggplot(df, aes(x = BP_category)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Diastolic Blood Pressure Categories", x = "Category", y = "Count")


df$SkinThickness[df$SkinThickness == 0] <- NA
ggplot(df, aes(x = SkinThickness)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Skin Thickness Distribution", x = "Skin Thickness", y = "Count") +
  theme_minimal()
# Boxplot
ggplot(df, aes(y = SkinThickness)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Skin Thickness Boxplot", y = "Skin Thickness") +
  theme_minimal()


df$Insulin[df$Insulin == 0] <- NA
imputed <- mice(df, method = "pmm", m = 1, seed = 123)
df <- complete(imputed)
ggplot(df, aes(x = Insulin)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 50) +
  labs(title = "Insulin Level Distribution", x = "Insulin", y = "Frequency") +
  theme_minimal()
ggplot(df, aes(y = Insulin)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Insulin Level Distribution", y = "Insulin level") +
  theme_minimal()


dfb <- df[df$BMI > 10, ]
ggplot(dfb, aes(x = BMI)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 50) +
  labs(title = "BMI Distribution", x = "Insulin", y = "Frequency") +
  theme_minimal()
ggplot(dfb, aes(y = BMI)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "BMI Distribution", y = "BMI") +
  theme_minimal()

df$Age <- round(df$Age)
df <- df[df$Age >= 0 & df$Age <= 80, ]
df_clean <- df[df$Age != 80, ]
ggplot(df_clean, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Cleaned Age Distribution",
       x = "Age",
       y = "Frequency") +
  theme_minimal()
ggplot(df_clean, aes(y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Cleaned Age Distribution", y = "Age") +
  theme_minimal()

ggplot(df, aes(x = HbA1c_level)) +
  geom_histogram(binwidth = 0.1, fill = "orchid", color = "white") +
  labs(title = "HbA1c Level Distribution", x = "HbA1c (%)", y = "Count") +
  theme_minimal()
ggplot(df, aes(y = HbA1c_level)) +
  geom_boxplot(fill = "orchid") +
  labs(title = "HbA1c Level Distribution", y = "HbA1c (%)") +
  theme_minimal()

df$gender <- as.factor(df$gender)
ggplot(df, aes(x = gender, fill = gender)) +
  geom_bar() +
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#00BFFF", "Other" = "#9370DB")) +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
gender_dist <- df %>%
  count(gender) %>%
  mutate(perc = n / sum(n) * 100,
         label = paste0(gender, "\n", round(perc, 1), "%"))
ggplot(gender_dist, aes(x = "", y = n, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4.5) +
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#00BFFF", "Other" = "#9370DB")) +
  labs(title = "Gender Distribution (Pie Chart)") +
  theme_void()

# Prepare data
htn_data <- df %>%
  count(hypertension) %>%
  mutate(hypertension = factor(hypertension, labels = c("No", "Yes")))
# Bar Chart
ggplot(htn_data, aes(x = hypertension, y = n, fill = hypertension)) +
  geom_bar(stat = "identity") +
  labs(title = "Hypertension Distribution (Bar Chart)", x = "Hypertension", y = "Count") +
  scale_fill_manual(values = c("No" = "#66c2a5", "Yes" = "#fc8d62")) +
  theme_minimal()
# Add percentage and labels
htn_data <- htn_data %>%
  mutate(perc = n / sum(n) * 100,
         label = paste0(hypertension, "\n", round(perc, 1), "%"))
# Pie Chart
ggplot(htn_data, aes(x = "", y = n, fill = hypertension)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("No" = "#66c2a5", "Yes" = "#fc8d62")) +
  labs(title = "Hypertension Distribution (Pie Chart)") +
  theme_void()

# Prepare data
heart_data <- df %>%
  count(heart_disease) %>%
  mutate(heart_disease = factor(heart_disease, labels = c("No", "Yes")))
# Bar Chart
ggplot(heart_data, aes(x = heart_disease, y = n, fill = heart_disease)) +
  geom_bar(stat = "identity") +
  labs(title = "Heart Disease Distribution (Bar Chart)", x = "Heart Disease", y = "Count") +
  scale_fill_manual(values = c("No" = "#8da0cb", "Yes" = "#e78ac3")) +
  theme_minimal()
# Add percentage and labels
heart_data <- heart_data %>%
  mutate(perc = n / sum(n) * 100,
         label = paste0(heart_disease, "\n", round(perc, 1), "%"))
# Pie Chart
ggplot(heart_data, aes(x = "", y = n, fill = heart_disease)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("No" = "#8da0cb", "Yes" = "#e78ac3")) +
  labs(title = "Heart Disease Distribution (Pie Chart)") +
  theme_void()

# Prepare data
smoke_data <- df %>%
  count(smoking_history)
# Bar Chart
ggplot(smoke_data, aes(x = reorder(smoking_history, -n), y = n, fill = smoking_history)) +
  geom_bar(stat = "identity") +
  labs(title = "Smoking History Distribution", x = "Smoking History", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")
# Add percentage and label
smoke_data <- smoke_data %>%
  mutate(perc = n / sum(n) * 100,
         label = paste0(smoking_history, "\n", round(perc, 1), "%"))
# Donut Chart
ggplot(smoke_data, aes(x = 2, y = n, fill = smoking_history)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Smoking History Distribution (Donut Chart)") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")

# Prepare data
diabetes_data <- df %>%
  count(diabetes) %>%
  mutate(diabetes = factor(diabetes, levels = c(0, 1), labels = c("No Diabetes", "Diabetes")))
# Bar chart
ggplot(diabetes_data, aes(x = diabetes, y = n, fill = diabetes)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Diabetes Status Distribution", x = "Diabetes", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62")) +
  geom_text(aes(label = n), vjust = -0.3)
# Pie chart
diabetes_data <- diabetes_data %>%
  mutate(perc = round(n / sum(n) * 100, 1),
         label = paste0(diabetes, "\n", perc, "%"))
ggplot(diabetes_data, aes(x = "", y = n, fill = diabetes)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Diabetes Status Distribution (Pie Chart)") +
  theme_void() +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"))

#Bivariate Analysis
ggplot(df, aes(x = factor(diabetes), y = blood_glucose_level, fill = factor(diabetes))) +
  geom_boxplot() +
  labs(
    title = "Blood Glucose Level by Diabetes Status",
    x = "Diabetes (0 = No, 1 = Yes)",
    y = "Blood Glucose Level"
  ) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  theme_minimal()
ggplot(df, aes(x = factor(diabetes), y = blood_glucose_level, fill = factor(diabetes))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(
    title = "Distribution of Blood Glucose Level by Diabetes Status",
    x = "Diabetes (0 = No, 1 = Yes)",
    y = "Blood Glucose Level"
  ) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  theme_minimal()
t.test(blood_glucose_level ~ diabetes, data = df)

ggplot(df, aes(x = factor(diabetes), y = HbA1c_level, fill = factor(diabetes))) +
  geom_boxplot() +
  labs(
    title = "HbA1c Levels by Diabetes Status",
    x = "Diabetes (0 = No, 1 = Yes)",
    y = "HbA1c Level"
  ) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  theme_minimal()
t.test(HbA1c_level ~ diabetes, data = df)
cor(df$HbA1c_level, df$diabetes)


ggplot(df, aes(x = factor(diabetes), y = Age, fill = factor(diabetes))) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Diabetes Status",
    x = "Diabetes (0 = No, 1 = Yes)",
    y = "Age"
  ) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  theme_minimal()
ggplot(df_clean, aes(x = Age, fill = factor(diabetes))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40) +
  labs(
    title = "Age Distribution for Diabetic and Non-Diabetic Individuals",
    x = "Age",
    y = "Count",
    fill = "Diabetes"
  ) +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  theme_minimal()
df$age_group <- cut(df$Age, 
                    breaks = c(0, 18, 30, 40, 50, 60, 70, 80, Inf),
                    labels = c("<18", "18-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80+"))
age_diabetes_rate <- df %>%
  group_by(age_group) %>%
  summarise(
    total = n(),
    diabetics = sum(diabetes == 1),
    rate = round(100 * diabetics / total, 2)
  )
ggplot(age_diabetes_rate, aes(x = age_group, y = rate)) +
  geom_bar(stat = "identity", fill = "#E69F00") +
  labs(
    title = "Diabetes Rate by Age Group",
    x = "Age Group",
    y = "Diabetes Rate (%)"
  ) +
  theme_minimal()

df %>%
  group_by(gender, diabetes) %>%
  summarise(count = n()) %>%
  group_by(gender) %>%
  mutate(proportion = count / sum(count)) %>%
  ggplot(aes(x = gender, y = proportion, fill = factor(diabetes))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Diabetes Proportion Across Gender",
    x = "Gender",
    y = "Proportion",
    fill = "Diabetes (0 = No, 1 = Yes)"
  ) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  theme_minimal()
# Create contingency table
table_gender <- table(df$gender, df$diabetes)
# Chi-square test
chisq.test(table_gender)
# Cross-tabulation
table_gender <- table(df$gender, df$diabetes)
# Convert to proportions
diabetes_rate_gender <- prop.table(table_gender, margin = 1)[, "1"] * 100
# Print result
diabetes_rate_gender
# Bar plot
barplot(diabetes_rate_gender,
        main = "Diabetes Rate by Gender",
        ylab = "Diabetes Rate (%)",
        xlab = "Gender",
        col = c("skyblue", "pink", "lightgreen"),
        ylim = c(0, max(diabetes_rate_gender) + 5))
# Add values on top of bars
text(x = seq_along(diabetes_rate_gender),
     y = diabetes_rate_gender,
     label = round(diabetes_rate_gender, 2),
     pos = 3, cex = 0.8)

# Contingency table
table_htn <- table(df$hypertension, df$diabetes)
# Convert to proportions (column-wise)
prop_table <- prop.table(table_htn, margin = 2)
# Create the bar plot and save midpoints for label positioning
bar_midpoints <- barplot(prop_table,
                         beside = FALSE,
                         col = c("lightblue", "salmon"),
                         legend = rownames(prop_table),
                         main = "Proportion of Hypertension within Diabetes Status",
                         xlab = "Diabetes (0 = No, 1 = Yes)",
                         ylab = "Proportion",
                         ylim = c(0, 1))  # Ensure full scale
# Add percentage labels on bars
for (i in 1:ncol(prop_table)) {
  total_height <- 0
  for (j in 1:nrow(prop_table)) {
    value <- round(prop_table[j, i] * 100, 1)
    text(x = bar_midpoints[i],
         y = total_height + prop_table[j, i] / 2,
         labels = paste0(value, "%"),
         cex = 0.8)
    total_height <- total_height + prop_table[j, i]
  }
}
# Perform chi-square test
chisq.test(table_htn)


table_htn <- table(df$heart_disease, df$diabetes)
# Convert to proportions (column-wise)
prop_table <- prop.table(table_htn, margin = 2)
# Create the bar plot and save midpoints for label positioning
bar_midpoints <- barplot(prop_table,
                         beside = FALSE,
                         col = c("lightblue", "salmon"),
                         legend = rownames(prop_table),
                         main = "Proportion of Heart Disease within Diabetes Status",
                         xlab = "Diabetes (0 = No, 1 = Yes)",
                         ylab = "Proportion",
                         ylim = c(0, 1))  # Ensure full scale
# Add percentage labels on bars
for (i in 1:ncol(prop_table)) {
  total_height <- 0
  for (j in 1:nrow(prop_table)) {
    value <- round(prop_table[j, i] * 100, 1)
    text(x = bar_midpoints[i],
         y = total_height + prop_table[j, i] / 2,
         labels = paste0(value, "%"),
         cex = 0.8)
    total_height <- total_height + prop_table[j, i]
  }
}
# Perform chi-square test
chisq.test(table_htn)

# Contingency table
table_smoke <- table(df$smoking_history, df$diabetes)
# Perform Chi-square test
chisq.test(table_smoke)
# Grouped bar chart
barplot(table_smoke,
        beside = TRUE,
        col = c("lightblue", "salmon", "orange", "green", "purple", "gray"),
        legend = rownames(table_smoke),
        main = "Diabetes by Smoking History",
        xlab = "Diabetes (0 = No, 1 = Yes)",
        ylab = "Count")
# Proportional version
prop_table <- prop.table(table_smoke, margin = 2)
bp<-barplot(prop_table,
        beside = TRUE,
        col = c("lightblue", "salmon", "orange", "green", "purple", "gray"),
        legend = rownames(prop_table),
        main = "Proportion of Smoking History within Diabetes Groups",
        xlab = "Diabetes (0 = No, 1 = Yes)",
        ylab = "Proportion")
text(x = bp,
     y = prop_table,
     labels = sprintf("%.2f", prop_table),
     pos = 3, cex = 0.7, col = "black")


boxplot(BMI ~ diabetes, data = df,
        main = "BMI Distribution by Diabetes Status",
        xlab = "Diabetes (0 = No, 1 = Yes)",
        ylab = "BMI",
        col = c("lightgreen", "salmon"))
cor.test(df$BMI, df$diabetes)



plot(df$HbA1c_level, df$blood_glucose_level,
     main = "Scatter Plot of HbA1c Level vs. Blood Glucose Level",
     xlab = "HbA1c Level (%)",
     ylab = "Blood Glucose Level (mg/dL)",
     pch = 19,
     col = rgb(0.1, 0.4, 0.8, 0.5))
ggplot(df, aes(x = blood_glucose_level, y = HbA1c_level)) +
     stat_bin_hex(bins = 30) +
     scale_fill_viridis_c() +
     labs(title = "Hexbin Plot: Blood Glucose vs. HbA1c",
          x = "Blood Glucose Level",
          y = "HbA1c Level",
          fill = "Count") +
     theme_minimal()
cor.test(df$HbA1c_level, df$blood_glucose_level)


ggplot(df, aes(x = Age, y = blood_glucose_level)) +
     geom_point(alpha = 0.3, color = "blue") +
     geom_smooth(method = "lm", col = "red") +
     labs(title = "Age vs Blood Glucose Level",
          x = "Age", y = "Blood Glucose Level")

boxplot(Age ~ hypertension, data = df,
        main = "Boxplot: Age by Hypertension Status",
        xlab = "Hypertension (0 = No, 1 = Yes)",
        ylab = "Age",
        col = c("skyblue", "salmon"))
df$age_group <- cut(df$Age, breaks = seq(0, 90, by = 10), right = FALSE)
ggplot(df, aes(x = age_group, fill = factor(hypertension))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Hypertension by Age Group",
       x = "Age Group", y = "Proportion", fill = "Hypertension")

boxplot(Age ~ heart_disease, data = df,
        main = "Boxplot: Age by Heart Disease Status",
        xlab = "Heart Disease (0 = No, 1 = Yes)",
        ylab = "Age",
        col = c("lightgreen", "tomato"))
ggplot(df, aes(x = age_group, fill = factor(heart_disease))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Heart Disease by Age Group",
       x = "Age Group", y = "Proportion", fill = "Heart Disease")

plot(df$Age, df$HbA1c_level,
     main = "Scatter Plot: Age vs HbA1c Level",
     xlab = "Age",
     ylab = "HbA1c Level",
     pch = 19, col = "blue")
abline(lm(HbA1c_level ~ Age, data = df), col = "red", lwd = 2)
df$age_group <- cut(df$Age, breaks = seq(0, 90, by = 10), right = FALSE)
boxplot(HbA1c_level ~ age_group, data = df,
        main = "Boxplot: HbA1c Level by Age Group",
        xlab = "Age Group",
        ylab = "HbA1c Level",
        col = "lightblue", las = 2)



#Multivariate analysis
ggplot(df, aes(x = factor(diabetes), y = BMI, fill = factor(diabetes))) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.1) +
  labs(title = "BMI Distribution by Diabetes Status", x = "Diabetes", y = "BMI")


numeric_vars <- df[, sapply(df, is.numeric)]
corr_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(corr_matrix,
         method = "color",        
         type = "upper",          
         addCoef.col = "black",   
         tl.cex = 0.8,            
         number.cex = 0.7,        
         col = colorRampPalette(c("blue", "white", "red"))(200))

ggpairs(df[, c("blood_glucose_level", "BMI", "Age", "HbA1c_level" , "diabetes")])

