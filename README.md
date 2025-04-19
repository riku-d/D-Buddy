# D-Buddy 🩺💡

**D-Buddy** is an intelligent web application built using **R Shiny** that predicts the likelihood of diabetes based on key medical and lifestyle factors. It also recommends a suitable **3-course meal plan** tailored to the user's diabetes status and food preferences. D-Buddy combines predictive power with personalized nutritional guidance to promote proactive health management.

---

## 🚀 Features

- 🔍 **Diabetes Risk Prediction**: Enter your health data manually and get an instant diabetes likelihood assessment.
- 📊 **Interactive Dashboard**: Built using **R Shiny** for a clean, user-friendly experience.
- 📈 **Descriptive Analysis**: Visual and statistical insights into trends in health data.
- 🧠 **Predictive Modeling**: Multiple models tested (Logistic Regression, Random Forest, and XGBoost). **XGBoost** was selected for its top performance.
- 🍽️ **3-Course Meal Plan Generator**: Personalized breakfast, lunch, and dinner suggestions based on your diabetes status and dietary preference (Veg/Non-Veg).
- ✅ **Manual Data Entry**: Users input values via a form 

---

## 📦 Tech Stack

- **Frontend**: R Shiny
- **Backend**: R, XGBoost
- **Modeling Libraries**: `xgboost`, `caret`, `randomForest`, `glm`
- **Data Manipulation**: `dplyr`, `readr`, `tidyr`
- **Visualization**: `ggplot2`, `plotly`, `shinyWidgets`

---

## 🧬 Input Features

The model uses the following input parameters:

- `Age`
- `Gender`
- `Blood Glucose Level`
- `BMI (Body Mass Index)`
- `HbA1c Level`
- `Hypertension` (Yes/No)
- `Heart Disease` (Yes/No)
- `Smoking History` (Never / Former / Current)

---

## 📊 Analysis Pipeline

### Descriptive Analysis:
- Performed detailed **exploratory data analysis (EDA)** to uncover patterns and relationships.
- Visualized:
  - BMI vs Diabetes Status
  - Glucose Distribution
  - Age vs Risk
  - Smoking Impact

### Predictive Modeling:
- Tried three models:
  - **Logistic Regression** (baseline)
  - **Random Forest** (strong ensemble model)
  - **XGBoost** (best performance, used in deployment)
---

## 🍽️ Personalized Meal Recommendation

After prediction, D-Buddy generates a **3-course meal plan** (breakfast, lunch, dinner) based on:

- **Diabetes status**
- **Blood Glucose Level**
- **HbA1c**
- **Age**
- **Food preference** (Veg/Non-Veg)
