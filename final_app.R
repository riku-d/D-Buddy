library(shiny)
library(shinydashboard)
library(dplyr)
library(mice)
library(ggplot2)
library(corrplot)
library(GGally)
library(DT)
library(xgboost)
library(Matrix)
setwd("/Users/ashmita/Desktop/D-Buddy")

d1<-read.csv("diabetes_cleaned.csv")
d2<-read.csv("diabetes_cleaned1.csv")
df<-read.csv("diabetes_cleaned2.csv")


ui <- dashboardPage(
  dashboardHeader(title = "D-Buddy"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .main-sidebar { 
          position: fixed; 
          height: 100vh; 
          overflow-y: auto; 
        }
        /* Fix the header position */
        .main-header {
          position: fixed;
          width: 100%;
          z-index: 1000;
        }
        /* Ensure content does not overlap with fixed header */
        .content-wrapper {
          margin-top: 50px; /* Adjust this based on header height */
          margin-left: 230px; /* Adjust this based on sidebar width */
        }
        /* Adjust body padding to prevent overlap with fixed header */
        .main-body {
          padding-top: 50px;
        }
        /* Increase sidebar menu font size */
        .sidebar-menu > li > a {
          display: flex;
          align-items: center;
          justify-content: space-between;
          font-size: 24px !important;
          font-weight: bold;
        }
        
        /* Increase sidebar header font size */
        .main-header .logo {
          font-size: 22px !important;
          font-weight: bold;
        }
        
        .sidebar-menu > li > a .fa {
          margin-left: auto;  /* Moves the dropdown icon to the right */
        }
        
      "))
    ),
    sidebarMenu(
      div(class = "sidebar-links",
          menuItem("Home", tabName = "home", icon = icon("home")),
          menuItem("DataSet Description", tabName = "eda", icon = icon("search")),
          menuItem("Descriptive Analysis", icon = icon("chart-bar"), tabName = "descriptive",
                   menuSubItem("Univariate Analysis", tabName = "univariate", icon = icon("chart-pie")),
                   menuSubItem("Bivariate Analysis", tabName = "bivariate", icon = icon("chart-line")),
                   menuSubItem("Multivariate Analysis", tabName = "multivariate", icon = icon("project-diagram"))
          ),
          menuItem("Predictive Analysis", tabName = "predict", icon = icon("line-chart")),
          menuItem("Personalized Healthy Diet Recommendation", tabName = "diet", icon = icon("apple-alt"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Section 2: Descriptive Analysis of Diabetes
      tabItem(tabName = "eda",
              h2("Exploratory Data Analysis"),
              
              fluidRow(
                box(title = "Dataset Structure", status = "primary", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("dataset_structure")),
                
                box(title = "Summary Statistics", status = "info", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("dataset_summary"))
              ),
              
              fluidRow(
                box(title = "Missing Values Overview", status = "danger", solidHeader = TRUE, width = 6,
                    tableOutput("missing_values")),
                
                box(title = "Unique Values per Column", status = "warning", solidHeader = TRUE, width = 6,
                    tableOutput("unique_values"))
              ),
              
              fluidRow(
                box(title = "Column Descriptions", status = "success", solidHeader = TRUE, width = 12,
                    HTML("<ul>
  <li><b>blood_glucose_level</b>: Numeric value indicating the concentration of glucose in the blood (measured in mg/dL). Higher levels often signal hyperglycemia, which is a common symptom of diabetes. Normal fasting levels typically range between 70–100 mg/dL.</li>

  <li><b>HbA1c_level</b>: Hemoglobin A1c percentage, representing the average blood glucose level over the past 2–3 months. It's a reliable long-term marker of blood sugar control. A value above 6.5% usually indicates diabetes.</li>

  <li><b>BMI (Body Mass Index)</b>: A numeric measure of body fat calculated using height and weight. It's used to categorize individuals into underweight, normal weight, overweight, or obese. High BMI is a risk factor for Type 2 diabetes.</li>

  <li><b>Age</b>: The patient’s age in years. Age is an important risk factor, as the likelihood of developing Type 2 diabetes increases with age, especially after 45 years.</li>

  <li><b>gender</b>: The biological sex of the patient (categorical variable with values such as 'Male', 'Female', or 'Other'). Certain risk profiles and health outcomes may differ by gender.</li>

  <li><b>smoking_history</b>: Categorical variable indicating the patient's history of tobacco use. Categories may include 'never', 'former', 'current', 'ever', 'not current', or 'Unknown'. Smoking is associated with an increased risk of insulin resistance and diabetes complications.</li>

  <li><b>BP_category (Blood Pressure Category)</b>: A derived categorical variable based on the patient's diastolic and systolic blood pressure readings. Categories include 'Low', 'Normal', 'Elevated', 'High Stage 1', and 'High Stage 2'. High blood pressure is often comorbid with diabetes and increases cardiovascular risk.</li>

  <li><b>Insulin</b>: Numeric value representing insulin concentration in the blood (measured in µU/mL). Abnormal insulin levels may reflect insulin resistance or pancreatic issues. Often, very high or low values are markers for metabolic syndrome or diabetes.</li>

  <li><b>hypertension</b>: Binary variable (0 = No, 1 = Yes) indicating whether the patient has high blood pressure. Hypertension is a common comorbidity in diabetic individuals and is important for risk stratification.</li>

  <li><b>heart_disease</b>: Binary variable (0 = No, 1 = Yes) showing whether the patient has a history of heart disease. Diabetics have a higher risk of developing cardiovascular diseases, making this a key clinical feature.</li>

  <li><b>diabetes</b>: The target output variable (0 = Non-diabetic, 1 = Diabetic). This indicates whether the patient has been diagnosed with diabetes and is what the prediction model attempts to classify.</li>
</ul>")
                )
              ),
              
              fluidRow(
                box(title = "Full Dataset Preview", status = "primary", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("full_data_table"))
              )
      ),
      
      
      # Section 3: Univariate Analysis
      tabItem(tabName = "univariate",
              h2("Univariate Analysis of the first Dataset (PIMA)"),
              fluidRow(
                box(title = "Glucose Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_univariated1"), textOutput("glucose_conclusiond1")),
                box(title = "Glucose Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_univariated2"), textOutput("glucose_conclusiond2"))
              ),
              
              fluidRow(
                box(title = "BMI Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("bmi_univariated1"), textOutput("bmi_conclusiond1")),
                box(title = "BMI Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("bmi_univariated2"), textOutput("bmi_conclusiond2"))
              ),
              fluidRow(
                box(title = "Age Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_univariated1"), textOutput("age_conclusiond1")),
                box(title = "Age Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_univariated2"), textOutput("age_conclusiond2"))
              ),
              
              fluidRow(
                box(title = "Diabetes Pedigree Function Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("dpf_univariated1"), textOutput("dpf_conclusiond1")),
                box(title = "Diabetes Pedigree Function Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("dpf_univariated2"), textOutput("dpf_conclusiond2"))
              ),
              
              fluidRow(
                box(title = "Diabetes Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("diabetes_univariated1"), textOutput("diabetes_conclusiond1")),
                box(title = "Diabetes Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("diabetes_univariated2"), textOutput("diabetes_conclusiond2"))
              ),
              
              #------------------------------------------------------------------------------------------------------------------------------------
              
              
              h2("Univariate Analysis of the Second Dataset (diabetes prediction dataset)"),
              fluidRow(
                box(title = "Glucose Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_univariatedf1"), textOutput("glucose_conclusiondf1")),
                box(title = "Glucose Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_univariatedf2"), textOutput("glucose_conclusiondf2"))
              ),
              
              fluidRow(
                box(title = "BMI Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("bmi_univariatedf1"), textOutput("bmi_conclusiondf1")),
                box(title = "BMI Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("bmi_univariatedf2"), textOutput("bmi_conclusiondf2"))
              ),
              
              fluidRow(
                box(title = "Gender Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("gender_univariatedf1"), textOutput("gender_conclusiondf1")),
                box(title = "Gender Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("gender_univariatedf2"), textOutput("gender_conclusiondf2"))
              ),
              
              fluidRow(
                box(title = "HbA1c level Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("hba1c_univariatedf1"), textOutput("hba1c_conclusiondf1")),
                box(title = "HbA1c level Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("hba1c_univariatedf2"), textOutput("hba1c_conclusiondf2"))
              ),
              
              fluidRow(
                box(title = "Age Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_univariatedf1"), textOutput("age_conclusiondf1")),
                box(title = "Age Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_univariatedf2"), textOutput("age_conclusiondf2"))
              ),
              
              
              fluidRow(
                box(title = "Hypertension Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("hpt_univariatedf1"), textOutput("hpt_conclusiondf1")),
                box(title = "Hypertension Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("hpt_univariatedf2"), textOutput("hpt_conclusiondf2"))
              ),
              
              fluidRow(
                box(title = "Heart Disease Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("heart_univariatedf1"), textOutput("heart_conclusiondf1")),
                box(title = "Heart Disease Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("heart_univariatedf2"), textOutput("heart_conclusiondf2"))
              ),
              
              fluidRow(
                box(title = "Smoking History Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("smoke_univariatedf1"), textOutput("smoke_conclusiondf1")),
                box(title = "Smoking History Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("smoke_univariatedf2"), textOutput("smoke_conclusiondf2"))
              ),
              
              fluidRow(
                box(title = "Diabetes Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("diabetes_univariatedf1"), textOutput("diabetes_conclusiondf1")),
                box(title = "Diabetes Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("diabetes_univariatedf2"), textOutput("diabetes_conclusiondf2"))
              ),
              
              h2("Univariate Analysis of the Combined Dataset"),
              fluidRow(
                box(title = "Glucose Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_univariate1"), textOutput("glucose_conclusion1")),
                box(title = "Glucose Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_univariate2"), textOutput("glucose_conclusion2"))
              ),
              
              fluidRow(
                box(title = "BMI Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("bmi_univariate1"), textOutput("bmi_conclusion1")),
                box(title = "BMI Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("bmi_univariate2"), textOutput("bmi_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Age Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_univariate1"), textOutput("age_conclusion1")),
                box(title = "Age Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_univariate2"), textOutput("age_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Diabetes Pedigree Function Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("dpf_univariate1"), textOutput("dpf_conclusion1")),
                box(title = "Diabetes Pedigree Function Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("dpf_univariate2"), textOutput("dpf_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Gender Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("gender_univariate1"), textOutput("gender_conclusion1")),
                box(title = "Gender Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("gender_univariate2"), textOutput("gender_conclusion2"))
              ),
              
              fluidRow(
                box(title = "HbA1c Level Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("hba1c_univariate1"), textOutput("hba1c_conclusion1")),
                box(title = "HbA1c Level Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("hba1c_univariate2"), textOutput("hba1c_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Hypertension Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("hpt_univariate1"), textOutput("hpt_conclusion1")),
                box(title = "Hypertension Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("hpt_univariate2"), textOutput("hpt_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Heart Disease Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("heart_univariate1"), textOutput("heart_conclusion1")),
                box(title = "Heart Disease Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("heart_univariate2"), textOutput("heart_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Smoking History Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("smoke_univariate1"), textOutput("smoke_conclusion1")),
                box(title = "Smoking History Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("smoke_univariate2"), textOutput("smoke_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Diabetes Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("diabetes_univariate1"), textOutput("diabetes_conclusion1")),
                box(title = "Diabetes Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("diabetes_univariate2"), textOutput("diabetes_conclusion2"))
              )
              
              
      ),
      
      # Section 4: Bivariate Analysis
      tabItem(tabName = "bivariate",
              h2("Bivariate Analysis"),
              fluidRow(
                box(title = "Blood Glucose vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_bivariate1"), textOutput("glucose_bconclusion1")),
                box(title = "Blood Glucose vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_bivariate2"), textOutput("glucose_bconclusion2"))
              ),
              fluidRow(
                box(title = "HbA1c level vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("hba1c_bivariate1"), textOutput("hba1c_conclusion1")),
                box(title = "Age vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_diabetes2"), textOutput("age_diabetes_conclusion2"))
              ),
              fluidRow(
                box(title = "Age vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_diabetes1"), textOutput("age_diabetes_conclusion1")),
                box(title = "Age vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_diabetes3"), textOutput("age_diabetes_conclusion3"))
              ),
              fluidRow(
                box(title = "Gender vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("gender_diabetes1"), textOutput("gender_diabetes_conclusion1")),
                box(title = "Gender vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("gender_diabetes2"), textOutput("gender_diabetes_conclusion2"))
              ),
              fluidRow(
                box(title = "Hypertension vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("hypertension_diabetes1"), textOutput("hypertension_diabetes_conclusion1")),
                box(title = "Heart Disease vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("heart_diabetes"), textOutput("heart_diabetes_conclusion"))
              ),
              fluidRow(
                box(title = "Smoking History vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("smoke_diabetes"), textOutput("smoke_diabetes_conclusion")),
                box(title = "Smoking History vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("smoke_diabetes1"), textOutput("smoke_diabetes_conclusion1"))
              ),
              fluidRow(
                box(title = "Age vs Hypertension", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_hypertension"), textOutput("age_hypertension_conclusion")),
                box(title = "Age vs Hypertension", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_hypertension1"), textOutput("age_hypertension_conclusion1"))
              ),
              fluidRow(
                box(title = "Age vs Heart Disease", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_heart"), textOutput("age_heart_conclusion")),
                box(title = "Age vs Hypertension", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_heart1"), textOutput("age_heart_conclusion1"))
              ),
              fluidRow(
                box(title = "BMI vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("bmi_diabetes"), textOutput("bmi_diabetes_conclusion")),
              )
      ),
      
      # Section 5: Multivariate Analysis
      tabItem(tabName = "multivariate",
              h2("Multivariate Analysis"),
              fluidRow(
                box(title = "Correlation Heatmap", status = "primary", solidHeader = TRUE, width = 12, height = 600,
                    div(
                      plotOutput("correlation_heatmap"),
                      style = "display: flex; justify-content: center; align-items: center; height: 100%;"
                    ))
              ),
              
              # Pairwise Scatter Plot
              fluidRow(
                box(title = "Pairwise Scatter Plot", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("pairwise_plot"))
              ),
              
      ),
      
      # Section 5: Predictive Analysis of Diabetes
      tabItem(
        tabName = "predict",
        h2("Predictive Analysis of Diabetes"),
        fluidRow(
          box(
            title = "Model Performance Comparison", status = "primary", solidHeader = TRUE, width = 12,
            
            # All tabPanels must be children of tabsetPanel
            tabsetPanel(
              type = "tabs",  # optional, adds tab styling
              tabPanel("Logistic Regression", 
                       h4("Evaluation Metrics"),
                       verbatimTextOutput("lr_metrics"),
                       h4("Confusion Matrix"),
                       tableOutput("lr_confmat"),
                       h4("ROC Curve"),
                       imageOutput("lr_roc_plot", height = "300px")
              ),
              tabPanel("Random Forest",
                       h4("Evaluation Metrics"),
                       verbatimTextOutput("rf_metrics"),
                       h4("Confusion Matrix"),
                       tableOutput("rf_confmat"),
                       h4("ROC Curve"),
                       imageOutput("rf_roc_plot", height = "300px")
              ),
              tabPanel("XGBoost",
                       h4("Evaluation Metrics"),
                       verbatimTextOutput("xgb_metrics"),
                       h4("Confusion Matrix"),
                       tableOutput("xgb_confmat"),
                       h4("ROC Curve"),
                       imageOutput("xgb_roc_plot")
              )
            )
          )
        ),
        
        p("Input patient information to predict diabetes risk using the trained XGBoost model."),
        
        fluidRow(
          box(
            title = "Patient Information", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(6,
                     numericInput("blood_glucose_level", "Blood Glucose Level", value = NULL),
                     numericInput("HbA1c_level", "HbA1c Level", value = NULL),
                     numericInput("BMI", "BMI", value = NULL),
                     numericInput("Age", "Age", value = NULL)
                     
              ),
              column(6,
                     selectInput("gender", "Gender", choices = c("", "Male", "Female")),
                     selectInput("smoking_history", "Smoking History",
                                 choices = c("", "never", "former", "current", "ever", "not current", "Unknown")),
                     selectInput("hypertension", "Hypertension", choices = c("", "No" = 0, "Yes" = 1)),
                     selectInput("heart_disease", "Heart Disease", choices = c("", "No" = 0, "Yes" = 1))
              )
            ),
            br(),
            div(style = "text-align:center",
                actionButton("predict_btn", "Predict", class = "btn btn-primary", icon = icon("heartbeat"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Prediction Result", status = "success", solidHeader = TRUE, width = 12,
            verbatimTextOutput("prediction_result")
          )
        )
      ),
      
      
      
      
      
      
      # Section 6: Personalized Diet & Exercise Recommendation
      tabItem(tabName = "diet",
              h2("Personalized Healthy Diet Recommendation"),
              p("Please provide your details for a personalized 3-course meal plan:"),
              
              fluidRow(
                box(
                  title = "Recommendation Form",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(6,
                           selectInput("diet_prediction", "Prediction:", choices = c("Diabetic", "Non-Diabetic")),
                           numericInput("diet_glucose", "Blood Glucose Level (mg/dL)", value = NULL),
                           numericInput("diet_hba1c", "HbA1c Level (%)", value = NULL)
                    ),
                    column(6,
                           numericInput("diet_age", "Age", value = NULL),
                           selectInput("food_pref", "Food Preference:", choices = c("Veg", "Non-Veg"))
                    )
                  ),
                  
                  br(),
                  div(style = "text-align:center",
                      actionButton("submit_diet", "Get Recommendations", class = "btn btn-success")
                  ),
                  
                  br(),
                  uiOutput("diet_feedback")  # Renders diet plan and suggestions
                )
              )
      ),
      
      
      
      # Section 1: Home
      tabItem(tabName = "home",
              # Diabetes Overview Section
              fluidRow(
                box(title = "What is Diabetes Mellitus?", status = "primary", solidHeader = TRUE, width = 12,
                    p("Diabetes Mellitus is a chronic metabolic condition characterized by elevated levels of blood glucose (sugar), either due to the body's inability to produce insulin (a hormone made by the pancreas), 
              or because the body cannot use insulin effectively."),
                    
                    p("There are three primary types of diabetes:"),
                    tags$ul(
                      tags$li(strong("Type 1 Diabetes:"), " An autoimmune condition where the body attacks insulin-producing cells. Usually diagnosed in children and young adults."),
                      tags$li(strong("Type 2 Diabetes:"), " The most common form, associated with lifestyle and genetic factors. It results in insulin resistance."),
                      tags$li(strong("Gestational Diabetes:"), " Develops during pregnancy and usually resolves after delivery, but increases risk of Type 2 diabetes.")
                    ),
                    
                    p("Long-term complications of uncontrolled diabetes include cardiovascular disease, nerve damage, kidney failure, eye damage (retinopathy), and poor wound healing. Early detection and lifestyle interventions can significantly reduce the risk of complications.")
                )
              ),
              
              # Data-Driven Insights
              fluidRow(
                box(title = "Insights from the Dataset", status = "success", solidHeader = TRUE, width = 12,
                    p("Our diabetes prediction dataset offers key insights into individuals at risk. Here's a summary:"),
                    
                    tags$ul(
                      tags$li("🔢 Total number of diabetic cases recorded: ", strong(textOutput("total_diabetes", inline = TRUE))),
                      tags$li("🎂 Average age of individuals with diabetes: ", strong(textOutput("avg_age_diabetes", inline = TRUE)), " years"),
                      tags$li("⚧️ Gender distribution among diabetic individuals: ", strong(textOutput("gender_distribution", inline = TRUE))),
                      tags$li("🩸 Average blood glucose level for diabetic patients: ", strong(textOutput("avg_glucose_diabetes", inline = TRUE)), " mg/dL")
                    ),
                    
                    p("These findings highlight the importance of regular screening, early diagnosis, and targeted interventions. Lifestyle factors such as BMI, physical activity, and smoking history also play a significant role.")
                )
              ),
              
              # Call to Action Box
              fluidRow(
                box(title = "Stay Aware. Stay Healthy.", status = "warning", solidHeader = TRUE, width = 12,
                    p("The risk of diabetes is rising globally due to urbanization, unhealthy diets, and physical inactivity."),
                    p("Explore the Predictive Analysis tab to assess your risk, and check the Diet tab for personalized nutritional guidance."),
                    tags$blockquote("“The groundwork of all happiness is good health.” – Leigh Hunt", style = "font-style: italic; color: #555;")
                )
              )
      )
      
    )
  )
)
server <- function(input, output,session) {
  output$dataset_structure <- renderPrint({
    str(df)
  })
  
  output$dataset_summary <- renderPrint({
    summary(df)
  })
  
  output$missing_values <- renderTable({
    data.frame(
      Column = colnames(df),
      Missing = sapply(df, function(x) sum(is.na(x)))
    )
  })
  
  output$unique_values <- renderTable({
    data.frame(
      Column = colnames(df),
      Unique_Values = sapply(df, function(x) length(unique(x)))
    )
  })
  
  output$full_data_table <- DT::renderDataTable({
    DT::datatable(df, options = list(pageLength = 10))
  })
  #=============================================================================================================================================================================
  # Load the cleaned dataset
  df_insights <- read.csv("/Users/ashmita/Desktop/D-Buddy/diabetes_cleaned2.csv")
  
  # Ensure correct data types
  df_insights$diabetes <- as.factor(df_insights$diabetes)
  df_insights$gender <- as.factor(df_insights$gender)
  
  # Total diabetic cases
  output$total_diabetes <- renderText({
    sum(df_insights$diabetes == 1)
  })
  
  # Average age of diabetic individuals
  output$avg_age_diabetes <- renderText({
    round(mean(df_insights$Age[df_insights$diabetes == 1]), 1)
  })
  
  # Gender distribution among diabetics
  output$gender_distribution <- renderText({
    dist <- table(df_insights$gender[df_insights$diabetes == 1])
    paste0(names(dist), ": ", dist, collapse = " | ")
  })
  
  # Average blood glucose level among diabetics
  output$avg_glucose_diabetes <- renderText({
    round(mean(df_insights$blood_glucose_level[df_insights$diabetes == 1]), 1)
  })
  
  
  #########################################################################################################################
  # Glucose Distribution(Univariate Analysis)
  output$glucose_univariate1 <- renderPlot({
    ggplot(df, aes(x = blood_glucose_level)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      labs(
        title = "Blood Glucose Distribution",
        x = "Glucose",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  output$glucose_conclusion1 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  output$glucose_univariate2 <- renderPlot({
    ggplot(df, aes(x = blood_glucose_level)) +
      geom_boxplot(fill = "lightblue", color = "black") +
      coord_flip() +
      labs(
        title = "Blood Glucose Boxplot",
        y = "Glucose Level"
      ) +
      theme_minimal()
  })
  output$glucose_conclusion2 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  
  
  output$bmi_univariate1 <- renderPlot({
    dfb <- df[df$BMI > 10, ]
    ggplot(dfb, aes(x = BMI)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 50) +
      labs(title = "BMI Distribution", x = "BMI", y = "Frequency") +
      theme_minimal()
  })
  output$bmi_conclusion1 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  output$bmi_univariate2 <- renderPlot({
    dfb <- df[df$BMI > 10, ]
    ggplot(dfb, aes(y = BMI)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "BMI Distribution", y = "BMI") +
      theme_minimal()
  })
  output$bmi_conclusion2 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  
  
  output$age_univariate1 <- renderPlot({
    df$Age <- round(df$Age)
    df <- df[df$Age >= 0 & df$Age <= 80, ]
    df_clean <- df[df$Age != 80, ]
    ggplot(df_clean, aes(x = Age)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
      labs(title = "Cleaned Age Distribution",
           x = "Age",
           y = "Frequency") +
      theme_minimal()
  })
  output$age_conclusion1 <- renderText({
    "Smoking history varies across the dataset, with a notable percentage of individuals having a history of smoking."
  })
  output$age_univariate2 <- renderPlot({
    df$Age <- round(df$Age)
    df <- df[df$Age >= 0 & df$Age <= 80, ]
    df_clean <- df[df$Age != 80, ]
    ggplot(df_clean, aes(y = Age)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "Cleaned Age Distribution", y = "Age") +
      theme_minimal()
  })
  output$age_conclusion2 <- renderText({
    "Smoking history varies across the dataset, with a notable percentage of individuals having a history of smoking."
  })
  
  output$hba1c_univariate1 <- renderPlot({
    ggplot(df, aes(x = HbA1c_level)) +
      geom_histogram(binwidth = 0.2, fill = "mediumorchid", color = "black") +
      labs(title = "Histogram of HbA1c Levels", x = "HbA1c (%)", y = "Count") +
      theme_minimal()
  })
  output$hba1c_conclusion1 <- renderText({
    "The histogram shows that HbA1c levels cluster around the normal range (~5-6.5%), 
with a noticeable tail toward higher levels, indicating a subset of individuals 
may have elevated diabetes risk."
  })
  
  output$hba1c_univariate2 <- renderPlot({
    ggplot(df, aes(y = HbA1c_level)) +
      geom_boxplot(fill = "orchid") +
      labs(title = "HbA1c Level Distribution", y = "HbA1c (%)") +
      theme_minimal()
  })
  output$hba1c_conclusion2 <- renderText({
    "The box plot shows that most individuals have normal HbA1c levels, but some outliers indicate higher values, suggesting diabetes risk."
  })
  
  
  output$gender_univariate1 <- renderPlot({
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
  })
  output$gender_conclusion1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$gender_univariate2 <- renderPlot({
    df$gender <- as.factor(df$gender)
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
  })
  output$gender_conclusion2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  output$hpt_univariate1 <- renderPlot({
    htn_data <- df %>%
      count(hypertension) %>%
      mutate(hypertension = factor(hypertension, labels = c("No", "Yes")))
    ggplot(htn_data, aes(x = hypertension, y = n, fill = hypertension)) +
      geom_bar(stat = "identity") +
      labs(title = "Hypertension Distribution (Bar Chart)", x = "Hypertension", y = "Count") +
      scale_fill_manual(values = c("No" = "#66c2a5", "Yes" = "#fc8d62")) +
      theme_minimal()
  })
  output$hpt_conclusion1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$hpt_univariate2 <- renderPlot({
    htn_data <- df %>%
      count(hypertension) %>%
      mutate(hypertension = factor(hypertension, labels = c("No", "Yes")))
    htn_data <- htn_data %>%
      mutate(perc = n / sum(n) * 100,
             label = paste0(hypertension, "\n", round(perc, 1), "%"))
    ggplot(htn_data, aes(x = "", y = n, fill = hypertension)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
      scale_fill_manual(values = c("No" = "#66c2a5", "Yes" = "#fc8d62")) +
      labs(title = "Hypertension Distribution (Pie Chart)") +
      theme_void()
  })
  output$hpt_conclusion2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  
  output$heart_univariate1 <- renderPlot({
    heart_data <- df %>%
      count(heart_disease) %>%
      mutate(heart_disease = factor(heart_disease, labels = c("No", "Yes")))
    # Bar Chart
    ggplot(heart_data, aes(x = heart_disease, y = n, fill = heart_disease)) +
      geom_bar(stat = "identity") +
      labs(title = "Heart Disease Distribution (Bar Chart)", x = "Heart Disease", y = "Count") +
      scale_fill_manual(values = c("No" = "#8da0cb", "Yes" = "#e78ac3")) +
      theme_minimal()
  })
  output$heart_conclusion1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$heart_univariate2 <- renderPlot({
    heart_data <- df %>%
      count(heart_disease) %>%
      mutate(heart_disease = factor(heart_disease, labels = c("No", "Yes")))
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
  })
  output$heart_conclusion2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  output$smoke_univariate1 <- renderPlot({
    smoke_data <- df %>%
      count(smoking_history)
    # Bar Chart
    ggplot(smoke_data, aes(x = reorder(smoking_history, -n), y = n, fill = smoking_history)) +
      geom_bar(stat = "identity") +
      labs(title = "Smoking History Distribution", x = "Smoking History", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
  })
  output$smoke_conclusion1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$smoke_univariate2 <- renderPlot({
    smoke_data <- df %>%
      count(smoking_history)
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
  })
  output$smoke_conclusion2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  output$dpf_univariate1 <- renderPlot({
    ggplot(df, aes(x = DiabetesPedigreeFunction)) +
      geom_histogram(binwidth = 0.05, fill = "lightgreen", color = "black") +
      theme_minimal() +
      labs(title = "Histogram of Diabetes Pedigree Function (Cleaned)", 
           x = "DPF", y = "Count")
  })
  
  output$dpf_conclusiond1 <- renderText({
    "The cleaned histogram indicates that most individuals have a DPF below 1, 
with a high concentration between 0.2 and 0.4. Outliers were replaced with the median, 
resulting in a smoother, more interpretable distribution for genetic risk factors."
  })
  
  # Boxplot after outlier handling
  output$dpf_univariate2 <- renderPlot({
    ggplot(df, aes(y = DiabetesPedigreeFunction)) +
      geom_boxplot(fill = "plum", color = "black") +
      theme_minimal() +
      labs(title = "Boxplot of Diabetes Pedigree Function (Cleaned)", y = "DPF")
  })
  
  output$dpf_conclusiond2 <- renderText({
    "The boxplot shows a tight distribution of DPF values, with most below 1. 
Outliers were replaced with the median, which reduces skew and better represents the central trend 
in genetic predisposition to diabetes."
  })
  
  output$diabetes_univariate1 <- renderPlot({
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
  })
  output$diabetes_conclusion1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$diabetes_univariate2 <- renderPlot({
    diabetes_data <- df %>%
      count(diabetes) %>%
      mutate(diabetes = factor(diabetes, levels = c(0, 1), labels = c("No Diabetes", "Diabetes")))
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
  })
  output$diabetes_conclusion2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  #=======================================================================================================================================================
  # Glucose Distribution(Univariate Analysis)
  
  output$glucose_univariated1 <- renderPlot({
    ggplot(d1, aes(x = blood_glucose_level)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      labs(
        title = "Blood Glucose Level Distribution (Cleaned)",
        x = "Blood Glucose Level (mg/dL)",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  output$glucose_conclusiond1 <- renderText({
    "Zero values were removed from the blood glucose data to ensure accuracy. 
The histogram shows that most individuals have blood glucose levels concentrated between 
90 and 150 mg/dL. However, a noticeable number of cases exceed 150 mg/dL, indicating potential hyperglycemia risks."
  })
  
  
  output$glucose_univariated2 <- renderPlot({
    ggplot(d1, aes( x = blood_glucose_level)) +
      geom_boxplot(fill = "lightblue", color = "black") +
      coord_flip() +
      labs(
        title = "Blood Glucose Level - Boxplot (Cleaned)",
        y = "Blood Glucose Level (mg/dL)",
        x = NULL
      ) +
      theme_minimal()
  })
  output$glucose_conclusiond2 <- renderText({
    "Zero values were excluded to clean the data. 
The boxplot reveals that while most blood glucose values fall within a healthy range, 
there are outliers on the higher end, indicating possible cases of elevated glucose levels or diabetes risk."
  })
  
  
  # Histogram after outlier handling
  output$dpf_univariated1 <- renderPlot({
    ggplot(d1, aes(x = DiabetesPedigreeFunction)) +
      geom_histogram(binwidth = 0.05, fill = "lightgreen", color = "black") +
      theme_minimal() +
      labs(title = "Histogram of Diabetes Pedigree Function (Cleaned)", 
           x = "DPF", y = "Count")
  })
  
  output$dpf_conclusiond1 <- renderText({
    "The cleaned histogram indicates that most individuals have a DPF below 1, 
with a high concentration between 0.2 and 0.4. Outliers were replaced with the median, 
resulting in a smoother, more interpretable distribution for genetic risk factors."
  })
  
  # Boxplot after outlier handling
  output$dpf_univariated2 <- renderPlot({
    ggplot(d1, aes(y = DiabetesPedigreeFunction)) +
      geom_boxplot(fill = "plum", color = "black") +
      theme_minimal() +
      labs(title = "Boxplot of Diabetes Pedigree Function (Cleaned)", y = "DPF")
  })
  
  output$dpf_conclusiond2 <- renderText({
    "The boxplot shows a tight distribution of DPF values, with most below 1. 
Outliers were replaced with the median, which reduces skew and better represents the central trend 
in genetic predisposition to diabetes."
  })
  
  
  
  # Histogram
  output$bmi_univariated1 <- renderPlot({
    ggplot(d1, aes(x = BMI)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 50) +
      labs(title = "BMI Distribution (Cleaned)", x = "BMI", y = "Frequency") +
      theme_minimal()
  })
  
  output$bmi_conclusiond1 <- renderText({
    "The histogram shows a cleaned distribution of BMI values, where invalid zeros and extreme outliers 
have been replaced with the median. Most individuals fall in the 25–30 range, indicating a trend 
towards overweight and obesity."
  })
  
  output$bmi_univariated2 <- renderPlot({
    ggplot(d1, aes(y = BMI)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "BMI Boxplot (Cleaned)", y = "BMI") +
      theme_minimal()
  })
  
  output$bmi_conclusiond2 <- renderText({
    "The boxplot illustrates the BMI distribution after replacing outliers and zero values with the median. 
Most values are now centered within a healthy range, with minimal to no extreme values visible."
  })
  
  output$age_univariated1 <- renderPlot({
    ggplot(d1, aes(x = Age)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
      labs(title = "Cleaned Age Distribution",
           x = "Age (Years)",
           y = "Frequency") +
      theme_minimal()
  })
  output$age_conclusiond1 <- renderText({
    "The cleaned histogram shows that most individuals fall between 20 to 60 years of age, with a gradual decline in frequency at older ages. This suggests the dataset mainly focuses on adults and middle-aged populations."
  })
  
  output$age_univariated2 <- renderPlot({
    ggplot(d1, aes(y = Age)) +
      geom_boxplot(fill = "lightblue", color = "black") +
      labs(title = "Boxplot of Cleaned Age Distribution",
           y = "Age (Years)") +
      theme_minimal()
  })
  output$age_conclusiond2 <- renderText({
    "The boxplot reveals that most individuals are aged between 25 to 55 years. Outliers are minimal, and the median falls around the mid-30s, indicating a largely working-age population."
  })
  
  
  output$diabetes_univariated1 <- renderPlot({
    diabetes_data <- d1 %>%
      count(diabetes) %>%
      mutate(diabetes = factor(diabetes, levels = c(0, 1), labels = c("No Diabetes", "Diabetes")))
    # Bar chart
    ggplot(diabetes_data, aes(x = diabetes, y = n, fill = diabetes)) +
      geom_bar(stat = "identity", width = 0.5) +
      labs(title = "Diabetes Status Distribution", x = "Diabetes", y = "Count") +
      theme_minimal() +
      scale_fill_manual(values = c("#66c2a5", "#fc8d62")) +
      geom_text(aes(label = n), vjust = -0.3)
  })
  output$diabetes_conclusion1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$diabetes_univariated2 <- renderPlot({
    diabetes_data <- d1 %>%
      count(diabetes) %>%
      mutate(diabetes = factor(diabetes, levels = c(0, 1), labels = c("No Diabetes", "Diabetes")))
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
  })
  output$diabetes_conclusion2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  output$glucose_univariatedf1 <- renderPlot({
    ggplot(d2, aes(x = blood_glucose_level)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      labs(
        title = "Blood Glucose Distribution",
        x = "Glucose",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  output$glucose_conclusiondf1 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  output$glucose_univariatedf2 <- renderPlot({
    ggplot(d2, aes(x = blood_glucose_level)) +
      geom_boxplot(fill = "lightblue", color = "black") +
      coord_flip() +
      labs(
        title = "Blood Glucose Boxplot",
        y = "Glucose Level"
      ) +
      theme_minimal()
  })
  output$glucose_conclusiondf2 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  
  output$bmi_univariatedf1 <- renderPlot({
    ggplot(d2, aes(x = BMI)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 50) +
      labs(title = "BMI Distribution", x = "BMI", y = "Frequency") +
      theme_minimal()
  })
  output$bmi_conclusiondf1 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  output$bmi_univariatedf2 <- renderPlot({
    ggplot(d2, aes(y = BMI)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "BMI Distribution", y = "BMI") +
      theme_minimal()
  })
  output$bmi_conclusiondf2 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  
  
  output$age_univariatedf1 <- renderPlot({
    ggplot(d2, aes(x = Age)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
      labs(title = "Cleaned Age Distribution",
           x = "Age",
           y = "Frequency") +
      theme_minimal()
  })
  output$age_conclusiondf1 <- renderText({
    "Smoking history varies across the dataset, with a notable percentage of individuals having a history of smoking."
  })
  output$age_univariatedf2 <- renderPlot({
    ggplot(d2, aes(y = Age)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "Cleaned Age Distribution", y = "Age") +
      theme_minimal()
  })
  output$age_conclusiondf2 <- renderText({
    "Smoking history varies across the dataset, with a notable percentage of individuals having a history of smoking."
  })
  
  output$hba1c_univariatedf1 <- renderPlot({
    ggplot(d2, aes(y = HbA1c_level)) +
      geom_boxplot(fill = "orchid") +
      labs(title = "HbA1c Level Distribution", y = "HbA1c (%)") +
      theme_minimal()
  })
  output$hba1c_conclusiondf1 <- renderText({
    "The box plot shows that most individuals have normal HbA1c levels, but some outliers indicate higher values, suggesting diabetes risk."
  })
  
  output$hba1c_univariatedf2 <- renderPlot({
    ggplot(d2, aes(x = HbA1c_level)) +
      geom_histogram(binwidth = 0.2, fill = "mediumorchid", color = "black") +
      labs(title = "Histogram of HbA1c Levels", x = "HbA1c (%)", y = "Count") +
      theme_minimal()
  })
  output$hba1c_conclusiondf2 <- renderText({
    "The histogram shows that HbA1c levels cluster around the normal range (~5-6.5%), 
with a noticeable tail toward higher levels, indicating a subset of individuals 
may have elevated diabetes risk."
  })
  
  
  output$gender_univariatedf1 <- renderPlot({
    d2$gender <- as.factor(d2$gender)
    ggplot(d2, aes(x = gender, fill = gender)) +
      geom_bar() +
      scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#00BFFF", "Other" = "#9370DB")) +
      labs(
        title = "Gender Distribution",
        x = "Gender",
        y = "Count"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  output$gender_conclusiondf1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$gender_univariatedf2 <- renderPlot({
    d2$gender <- as.factor(d2$gender)
    gender_dist <- d2 %>%
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
  })
  output$gender_conclusiondf2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  output$hpt_univariatedf1 <- renderPlot({
    htn_data <- d2 %>%
      count(hypertension) %>%
      mutate(hypertension = factor(hypertension, labels = c("No", "Yes")))
    ggplot(htn_data, aes(x = hypertension, y = n, fill = hypertension)) +
      geom_bar(stat = "identity") +
      labs(title = "Hypertension Distribution (Bar Chart)", x = "Hypertension", y = "Count") +
      scale_fill_manual(values = c("No" = "#66c2a5", "Yes" = "#fc8d62")) +
      theme_minimal()
  })
  output$hpt_conclusiondf1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$hpt_univariatedf2 <- renderPlot({
    htn_data <- d2 %>%
      count(hypertension) %>%
      mutate(hypertension = factor(hypertension, labels = c("No", "Yes")))
    htn_data <- htn_data %>%
      mutate(perc = n / sum(n) * 100,
             label = paste0(hypertension, "\n", round(perc, 1), "%"))
    ggplot(htn_data, aes(x = "", y = n, fill = hypertension)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
      scale_fill_manual(values = c("No" = "#66c2a5", "Yes" = "#fc8d62")) +
      labs(title = "Hypertension Distribution (Pie Chart)") +
      theme_void()
  })
  output$hpt_conclusiondf2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  
  output$heart_univariatedf1 <- renderPlot({
    heart_data <- d2 %>%
      count(heart_disease) %>%
      mutate(heart_disease = factor(heart_disease, labels = c("No", "Yes")))
    # Bar Chart
    ggplot(heart_data, aes(x = heart_disease, y = n, fill = heart_disease)) +
      geom_bar(stat = "identity") +
      labs(title = "Heart Disease Distribution (Bar Chart)", x = "Heart Disease", y = "Count") +
      scale_fill_manual(values = c("No" = "#8da0cb", "Yes" = "#e78ac3")) +
      theme_minimal()
  })
  output$heart_conclusiondf1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$heart_univariatedf2 <- renderPlot({
    heart_data <- d2 %>%
      count(heart_disease) %>%
      mutate(heart_disease = factor(heart_disease, labels = c("No", "Yes")))
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
  })
  output$heart_conclusion2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  output$smoke_univariatedf1 <- renderPlot({
    smoke_data <- d2 %>%
      count(smoking_history)
    # Bar Chart
    ggplot(smoke_data, aes(x = reorder(smoking_history, -n), y = n, fill = smoking_history)) +
      geom_bar(stat = "identity") +
      labs(title = "Smoking History Distribution", x = "Smoking History", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
  })
  output$smoke_conclusiondf1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$smoke_univariatedf2 <- renderPlot({
    smoke_data <- d2 %>%
      count(smoking_history)
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
  })
  output$smoke_conclusiondf2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  output$diabetes_univariatedf1 <- renderPlot({
    diabetes_data <- d2 %>%
      count(diabetes) %>%
      mutate(diabetes = factor(diabetes, levels = c(0, 1), labels = c("No Diabetes", "Diabetes")))
    # Bar chart
    ggplot(diabetes_data, aes(x = diabetes, y = n, fill = diabetes)) +
      geom_bar(stat = "identity", width = 0.5) +
      labs(title = "Diabetes Status Distribution", x = "Diabetes", y = "Count") +
      theme_minimal() +
      scale_fill_manual(values = c("#66c2a5", "#fc8d62")) +
      geom_text(aes(label = n), vjust = -0.3)
  })
  output$diabetes_conclusiondf1 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  output$diabetes_univariatedf2 <- renderPlot({
    diabetes_data <- d2 %>%
      count(diabetes) %>%
      mutate(diabetes = factor(diabetes, levels = c(0, 1), labels = c("No Diabetes", "Diabetes")))
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
  })
  output$diabetes_conclusiondf2 <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  #####################################################################################################################
  #Bivariate analysis
  
  output$glucose_bivariate1 <- renderPlot({
    ggplot(df, aes(x = factor(diabetes), y = blood_glucose_level, fill = factor(diabetes))) +
      geom_boxplot() +
      labs(
        title = "Blood Glucose Level by Diabetes Status",
        x = "Diabetes (0 = No, 1 = Yes)",
        y = "Blood Glucose Level"
      ) +
      scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
      theme_minimal()
  })
  output$glucose_bconclusion1 <- renderText({
    "The boxplot illustrates that individuals with diabetes tend to have significantly higher blood glucose levels compared to those without diabetes.

A Welch Two Sample t-test was performed:
- t = -93.78
- df = 9343.9
- p-value < 2.2e-16
- 95% CI: [-61.00, -58.50]
- Mean (No Diabetes) = 132.73
- Mean (Diabetes) = 192.48

Conclusion: The difference in blood glucose levels is highly statistically significant (p < 0.001). On average, individuals with diabetes have about 59.75 mg/dL higher blood glucose levels than non-diabetic individuals."
  })
  
  output$glucose_bivariate2 <- renderPlot({
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
  })
  output$glucose_bconclusion2 <- renderText({
    "Diabetes is more common in older individuals, with the highest cases observed above 50 years."
  })
  
  
  output$hba1c_bivariate1 <- renderPlot({
    ggplot(df, aes(x = factor(diabetes), y = HbA1c_level, fill = factor(diabetes))) +
      geom_boxplot() +
      labs(
        title = "HbA1c Levels by Diabetes Status",
        x = "Diabetes (0 = No, 1 = Yes)",
        y = "HbA1c Level"
      ) +
      scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
      theme_minimal()
  })
  output$hba1c_conclusion1 <- renderText({
    "The Welch Two Sample t-test shows a statistically significant difference in HbA1c levels 
    between individuals with and without diabetes (t = -128.56, df = 10172, p < 0.001). On average,
    individuals with diabetes have a much higher HbA1c level (mean = 6.93) compared to
    non-diabetic individuals (mean = 5.40). The 95% confidence interval for the
    difference in means is between -1.56 and -1.51, indicating a meaningful and reliable difference.
     The positive correlation (r = 0.40) indicates a moderate association between HbA1c levels and diabetes status."
  })
  
  
  output$age_diabetes1 <- renderPlot({
    ggplot(df, aes(x = factor(diabetes), y = Age, fill = factor(diabetes))) +
      geom_boxplot() +
      labs(
        title = "Age Distribution by Diabetes Status",
        x = "Diabetes (0 = No, 1 = Yes)",
        y = "Age"
      ) +
      scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
      theme_minimal()
  })
  output$age_diabetes_conclusion1 <- renderText({
    "Higher blood glucose levels are strongly associated with diabetes."
  })
  output$age_diabetes2 <- renderPlot({
    df$Age <- round(df$Age)
    df <- df[df$Age >= 0 & df$Age <= 80, ]
    df_clean <- df[df$Age != 80, ]
    ggplot(df_clean, aes(x = Age, fill = factor(diabetes))) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 40) +
      labs(
        title = "Age Distribution for Diabetic and Non-Diabetic Individuals",
        x = "Age",
        y = "Count",
        fill = "Diabetes"
      ) +
      scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
      theme_minimal()
  })
  output$age_diabetes_conclusion2 <- renderText({
    "Higher blood glucose levels are strongly associated with diabetes."
  })
  output$age_diabetes3 <- renderPlot({
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
      geom_bar(stat = "identity", fill = "#F8766D") +
      labs(
        title = "Diabetes Rate by Age Group",
        x = "Age Group",
        y = "Diabetes Rate (%)"
      ) +
      theme_minimal()
  })
  output$age_diabetes_conclusion3 <- renderText({
    "Higher blood glucose levels are strongly associated with diabetes."
  })
  
  output$gender_diabetes1 <- renderPlot({
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
      scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
      theme_minimal()
    #table_gender <- table(df$gender, df$diabetes)
    #chisq.test(table_gender)
  })
  output$gender_diabetes_conclusion1 <- renderText({
    "The bar plot illustrates the proportion of individuals with diabetes across different gender groups. 
  A Chi-square test of independence was conducted to assess the relationship between gender and diabetes status.
  The result (X² = 98.67, df = 2, p < 0.001) indicates a statistically significant association between gender and diabetes.

  This implies that the prevalence of diabetes varies meaningfully across gender groups. 
  Although the test did not return a warning in this case, if certain gender categories have very low frequencies (e.g., 'Other' or 'Unknown'), 
  it's still advisable to interpret with caution or consider alternative methods like Fisher’s Exact Test. 
  Nonetheless, the data strongly suggest that gender plays a role in diabetes distribution."
  })
  
  
  output$gender_diabetes2 <- renderPlot({
    table_gender <- table(df$gender, df$diabetes)
    # Convert to proportions
    diabetes_rate_gender <- prop.table(table_gender, margin = 1)[, "1"] * 100
    barplot(diabetes_rate_gender,
            main = "Diabetes Rate by Gender",
            ylab = "Diabetes Rate (%)",
            xlab = "Gender",
            col = c("#00BFC4", "#F8766D", "lightgreen"),
            ylim = c(0, max(diabetes_rate_gender) + 5))
    
    text(x = seq_along(diabetes_rate_gender),
         y = diabetes_rate_gender,
         label = round(diabetes_rate_gender, 2),
         pos = 3, cex = 0.8)
  })
  output$gender_diabetes_conclusion2 <- renderText({
    "The bar plot displays the diabetes rate (percentage of individuals with diabetes) across different gender groups. 
  It highlights that the proportion of individuals diagnosed with diabetes varies by gender.

  A Chi-square test of independence was conducted to assess the relationship between gender and diabetes status. 
  The test result (X² = 98.67, df = 2, p < 0.001) indicates a statistically significant association between gender and diabetes.

  This suggests that gender is related to diabetes prevalence in the population. While the original test produced a warning about the Chi-squared approximation (likely due to small counts in the 'Other' category), 
  the overall trend remains clear: diabetes rates differ meaningfully across genders, with one or more gender groups showing a higher burden of the condition. 
  Further analysis, like combining small categories or using Fisher’s Exact Test, can help confirm these insights."
  })
  
  
  output$hypertension_diabetes1 <- renderPlot({
    df %>%
      group_by(hypertension, diabetes) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(diabetes) %>%
      mutate(proportion = count / sum(count)) %>%
      ggplot(aes(x = factor(diabetes), y = proportion, fill = factor(hypertension))) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(proportion * 100, 1), "%")),
                position = position_stack(vjust = 0.5), size = 3.5, color = "white") +
      scale_fill_manual(values = c("#00BFC4", "#F8766D"),
                        labels = c("No Hypertension", "Hypertension")) +
      labs(
        title = "Proportion of Hypertension within Diabetes Status",
        x = "Diabetes (0 = No, 1 = Yes)",
        y = "Proportion",
        fill = "Hypertension"
      ) +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  output$hypertension_diabetes_conclusion1 <- renderText({
    "The bar plot shows the proportion of individuals with and without hypertension across diabetes status. 
  A Chi-square test of independence was performed to assess the relationship between hypertension and diabetes status. 
  The result (X² = 3991.7, df = 1, p < 0.001) indicates a statistically significant association between the two variables.

  This strong statistical result suggests that individuals with hypertension are significantly more likely to have diabetes. 
  The difference in proportions is visually apparent in the bar plot and statistically validated by the Chi-square test, 
  supporting the hypothesis that hypertension is associated with increased risk of diabetes."
  })
  
  
  output$heart_diabetes <- renderPlot({
    df %>%
      group_by(heart_disease, diabetes) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(diabetes) %>%
      mutate(proportion = count / sum(count)) %>%
      ggplot(aes(x = factor(diabetes), y = proportion, fill = factor(heart_disease))) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(proportion * 100, 1), "%")),
                position = position_stack(vjust = 0.5), size = 3.5, color = "white") +
      scale_fill_manual(values = c("#00BFC4", "#F8766D"),
                        labels = c("No Heart Disease", "Heart Disease")) +
      labs(
        title = "Proportion of Heart Disease within Diabetes Status",
        x = "Diabetes (0 = No, 1 = Yes)",
        y = "Proportion",
        fill = "Heart Disease"
      ) +
      theme_minimal() +
      theme(legend.position = "right")
  })
  output$heart_diabetes_conclusion <- renderText({
    "The bar plot displays the proportion of individuals with and without heart disease across diabetes status. 
  A Chi-square test of independence was conducted to examine the association between heart disease and diabetes. 
  The test yielded a highly significant result (X² = 2943.1, df = 1, p < 0.001), indicating a strong statistical relationship between the two conditions.

  This suggests that individuals with heart disease are significantly more likely to have diabetes. 
  The visual difference in the bar plot, where a greater proportion of heart disease is observed among diabetic individuals compared to non-diabetics, 
  is confirmed by the Chi-square test, reinforcing the conclusion that heart disease is associated with increased risk of diabetes."
  })
  
  output$smoke_diabetes <- renderPlot({
    df %>%
      group_by(smoking_history, diabetes) %>%
      summarise(count = n(), .groups = "drop") %>%
      ggplot(aes(x = smoking_history, y = count, fill = factor(diabetes))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#0072B2", "#D55E00"), labels = c("No Diabetes", "Diabetes")) +
      labs(
        title = "Diabetes Count by Smoking History",
        x = "Smoking History",
        y = "Count",
        fill = "Diabetes"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$heart_diabetes_conclusion <- renderText({
    "The bar plot displays the distribution of diabetes cases across different smoking history categories. 
A Chi-square test of independence was conducted to assess the association between smoking history and diabetes status. 
The test result (X² = 1683.6, df = 5, p < 0.001) indicates a statistically significant relationship between smoking habits and diabetes.

This finding suggests that the likelihood of having diabetes varies meaningfully across smoking categories. 
For instance, certain smoking history groups (e.g., 'current', 'former') may exhibit a higher prevalence of diabetes, 
as seen in the visual representation. These differences, confirmed by the Chi-square test, highlight the importance of considering smoking behavior in diabetes risk assessment."
  })
  
  output$smoke_diabetes1 <- renderPlot({
    df %>%
      group_by(smoking_history, diabetes) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(diabetes) %>%
      mutate(proportion = count / sum(count)) %>%
      ggplot(aes(x = factor(diabetes), y = proportion, fill = smoking_history)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = sprintf("%.2f", proportion)), 
                position = position_dodge(width = 0.9), 
                vjust = -0.3, size = 3) +
      scale_fill_manual(values = c("lightblue", "salmon", "orange", "green", "purple", "gray")) +
      labs(
        title = "Proportion of Smoking History within Diabetes Groups",
        x = "Diabetes (0 = No, 1 = Yes)",
        y = "Proportion",
        fill = "Smoking History"
      ) +
      ylim(0, 1) +
      theme_minimal()
  })
  output$smoke_diabetes_conclusion1 <- renderText({
    "The plot illustrates the proportion of individuals in each smoking history category within diabetic and non-diabetic groups. 
A Chi-square test of independence was conducted to examine the association between smoking history and diabetes status. 
The test yielded a highly significant result (X² = 1683.6, df = 5, p < 0.001), indicating a strong statistical association between smoking behavior and diabetes.

This suggests that the distribution of smoking history differs meaningfully between individuals with and without diabetes. 
Notably, certain categories such as 'current', 'former', or 'never' smokers may show varying prevalence, which is visually represented by the bar plot and statistically supported by the Chi-square test."
  })
  
  output$bmi_diabetes <- renderPlot({
    ggplot(df, aes(x = factor(diabetes), y = BMI, fill = factor(diabetes))) +
      geom_boxplot() +
      scale_fill_manual(values = c("lightgreen", "salmon")) +
      labs(
        title = "BMI Distribution by Diabetes Status",
        x = "Diabetes (0 = No, 1 = Yes)",
        y = "BMI"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$bmi_diabetes_conclusion <- renderText({
    "The box plot displays the distribution of BMI values across individuals with and without diabetes. 
A Pearson correlation test was conducted to assess the relationship between BMI and diabetes status. 
The result shows a statistically significant positive correlation (r = 0.219, p < 0.001), 
suggesting that individuals with higher BMI are more likely to have diabetes.

This relationship is also visually evident from the plot, where the median and interquartile range of BMI are higher for those with diabetes. 
Thus, BMI appears to be positively associated with diabetes prevalence."
  })
  
  output$age_hypertension <- renderPlot({
    ggplot(df, aes(x = factor(hypertension), y = Age, fill = factor(hypertension))) +
      geom_boxplot() +
      scale_fill_manual(values = c("skyblue", "salmon")) +
      labs(
        title = "Boxplot: Age by Hypertension Status",
        x = "Hypertension (0 = No, 1 = Yes)",
        y = "Age"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  output$age_hypertension_conclusion <- renderText({
    "The boxplot displays the distribution of age across individuals with and without hypertension.
    It is evident that individuals with hypertension tend to be older on average compared to those without hypertension.
    This supports the common clinical understanding that the risk of developing hypertension increases with age."
  })
  
  output$age_hypertension1 <- renderPlot({
    df$age_group <- cut(df$Age, breaks = seq(0, 90, by = 10), right = FALSE)
    ggplot(df, aes(x = age_group, fill = factor(hypertension))) +
      geom_bar(position = "fill") +
      labs(
        title = "Proportion of Hypertension by Age Group",
        x = "Age Group",
        y = "Proportion",
        fill = "Hypertension (0 = No, 1 = Yes)"
      ) +
      scale_fill_manual(values = c("skyblue", "salmon")) +
      theme_minimal()
  })
  output$age_hypertension_conclusion1 <- renderText({
    "The plot illustrates the proportion of individuals with and without hypertension across different age groups. 
  There is a clear trend showing that the proportion of individuals with hypertension increases with age. 
  This suggests that age is a significant factor associated with the prevalence of hypertension, 
  with older age groups exhibiting a higher risk."
  })
  
  output$age_heart <- renderPlot({
    ggplot(df, aes(x = factor(heart_disease), y = Age, fill = factor(heart_disease))) +
      geom_boxplot() +
      scale_fill_manual(values = c("lightgreen", "tomato")) +
      labs(
        title = "Boxplot: Age by Heart Disease Status",
        x = "Heart Disease (0 = No, 1 = Yes)",
        y = "Age",
        fill = "Heart Disease"
      ) +
      theme_minimal()
  })
  output$age_heart_conclusion <- renderText({
    "The boxplot compares the age distribution of individuals with and without heart disease. 
   It is evident that individuals with heart disease tend to be older on average compared to those without heart disease. 
   This suggests a strong association between increasing age and the presence of heart disease."
  })
  
  output$age_heart1 <- renderPlot({
    df$age_group <- cut(df$Age, breaks = seq(0, 90, by = 10), right = FALSE)
    
    ggplot(df, aes(x = age_group, fill = factor(heart_disease))) +
      geom_bar(position = "fill") +
      labs(
        title = "Proportion of Heart Disease by Age Group",
        x = "Age Group",
        y = "Proportion",
        fill = "Heart Disease (0 = No, 1 = Yes)"
      ) +
      scale_fill_manual(values = c("lightgreen", "tomato")) +
      theme_minimal()
  })
  output$age_heart_conclusion1 <- renderText({
    "The bar plot displays the proportion of individuals with heart disease across different age groups. 
   A clear pattern emerges, showing that the proportion of individuals with heart disease increases steadily with age. 
   This highlights that older individuals are significantly more likely to develop heart disease, 
   suggesting age as a major contributing factor to heart health risks."
  })
  
  
  
  ########################################################################################################################
  #Multivariate Analysis
  
  
  
  output$pairwise_plot <- renderPlot({
    ggpairs(df, columns = c("Age", "BMI", "blood_glucose_level", "HbA1c_level", "diabetes"),
            aes(color = factor(diabetes)))
  })
  
  
  
  output$correlation_heatmap <- renderPlot({
    numeric_vars <- df[, sapply(df, is.numeric)]
    corr_matrix <- cor(numeric_vars, use = "complete.obs")
    corrplot(corr_matrix,
             method = "color",        
             type = "upper",          
             addCoef.col = "black",   
             tl.cex = 0.8,            
             number.cex = 0.7,        
             col = colorRampPalette(c("blue", "white", "red"))(200))
  },height=500, width=600)
  
  
  # --- Section 2: XGBoost Prediction ---
  
  # Load trained model
  model_path <- "/Users/ashmita/Desktop/D-Buddy/xgb_diabetes_model.model"
  xgb_model <- xgb.load(model_path)
  
  observeEvent(input$predict_btn, {
    
    # Default value for DPF if not input by user
    avg_dpf <- 0.47  # Replace with actual training mean if available
    
    # Collect input from UI
    user_input <- data.frame(
      blood_glucose_level = input$blood_glucose_level,
      HbA1c_level = input$HbA1c_level,
      BMI = input$BMI,
      Age = input$Age,
      hypertension = as.numeric(input$hypertension),
      heart_disease = as.numeric(input$heart_disease),
      DiabetesPedigreeFunction = avg_dpf,
      gender = input$gender,
      smoking_history = input$smoking_history,
      diabetes = factor(0)  # Dummy for model.matrix
    )
    
    # Ensure correct factor levels
    user_input$gender <- factor(user_input$gender, levels = c("Female", "Male"))
    user_input$smoking_history <- factor(user_input$smoking_history,
                                         levels = c("never", "former", "current", "ever", "not current", "Unknown"))
    
    # One-hot encode
    user_matrix <- model.matrix(diabetes ~ . -1, data = user_input)
    user_dmatrix <- xgb.DMatrix(data = user_matrix)
    
    # Predict
    prob <- predict(xgb_model, user_dmatrix)
    threshold <- readRDS("/Users/ashmita/Desktop/D-Buddy/xgb_threshold.rds")
    #threshold <- 0.099  # Adjust as per your ROC analysis
    pred <- ifelse(prob > threshold, 1, 0)
    
    # Output
    output$prediction_result <- renderText({
      if (pred == 1) {
        return("Prediction: 🩺 Diabetic\nProbability: 100%")
      } else {
        paste0(
          "Prediction: ✅ Non-Diabetic\n",
          "Predicted Probability: ", round(prob * 1000, 2), "%"
        )
      }
    })
  })
  
  
  
  # Logistic Regression
  output$lr_metrics <- renderPrint({
    cat("📊 Logistic Regression Performance Metrics\n")
    cat("-----------------------------------------\n")
    cat("▶ Accuracy            : 0.9622\n")
    cat("▶ Kappa               : 0.7037\n")
    cat("▶ Sensitivity (Recall for Non-Diabetic) : 0.9916\n")
    cat("▶ Specificity (Recall for Diabetic)     : 0.6218\n")
    cat("▶ Precision (Non-Diabetic)              : 0.9681\n")
    cat("▶ F1 Score (Non-Diabetic)               : 0.9797\n")
    cat("▶ Balanced Accuracy                     : 0.8067\n")
    cat("▶ AUC (ROC Curve)                       : 0.9613\n\n")
    
    cat("💡 Note:\n")
    cat("- Class '0' (Non-Diabetic) is treated as the positive class by default.\n")
    cat("- To evaluate for Diabetic as positive class (class '1'):\n")
    cat("  • Sensitivity (Diabetic)  : 0.6218\n")
    cat("  • Precision (Diabetic)    : 0.8649\n")
    cat("  • F1 Score (Diabetic)     : 0.7232\n")
  })
  
  output$lr_confmat <- renderTable({
    matrix(c(17224, 568, 146, 934), nrow = 2, byrow = TRUE,
           dimnames = list("Predicted" = c("0", "1"), "Actual" = c("0", "1")))
  })
  output$lr_roc_plot <- renderImage({
    filename <- file.path("www/logistic_roc.jpg")
    list(src = filename, contentType = 'image/jpeg', width = 400, height = 300)
  }, deleteFile = FALSE)
  
  # Random Forest
  output$rf_metrics <- renderPrint({
    cat("🌲 Random Forest Performance Metrics\n")
    cat("-----------------------------------------\n")
    cat("▶ Accuracy            : 0.9750\n")
    cat("▶ Kappa               : 0.8046\n")
    cat("▶ Sensitivity (Recall for Diabetic)     : 0.7044\n")
    cat("▶ Specificity (Recall for Non-Diabetic) : 0.9984\n")
    cat("▶ Precision (Diabetic)                  : 0.9742\n")
    cat("▶ F1 Score (Diabetic)                   : 0.8182\n")
    cat("▶ Balanced Accuracy                     : 0.8514\n")
    cat("▶ AUC (ROC Curve)                       : 0.9817\n\n")
    
    cat("💡 Note:\n")
    cat("- Class '1' (Diabetic) is treated as the positive class.\n")
    cat("- Sensitivity and Precision focus on the model's ability to correctly detect diabetic cases.\n")
  })
  
  output$rf_confmat <- renderTable({
    matrix(c(18374, 541, 26, 1212), nrow = 2, byrow = TRUE,
           dimnames = list("Predicted" = c("0", "1"), "Actual" = c("0", "1")))
  })
  output$rf_roc_plot <- renderImage({
    filename <- file.path("www/rf_roc.jpg")
    list(src = filename, contentType = 'image/jpeg', width = 400, height = 300)
  }, deleteFile = FALSE)
  
  # XGBoost
  output$xgb_metrics <- renderPrint({
    cat("📊 XGBoost Performance Metrics\n")
    cat("-----------------------------------------\n")
    cat("▶ Accuracy            : 0.9377\n")
    cat("▶ Kappa               : 0.6807\n")
    cat("▶ Sensitivity (Recall for Diabetic) : 0.9361\n")
    cat("▶ Specificity (Recall for Non-Diabetic) : 0.9378\n")
    cat("▶ Precision (Diabetic)              : 0.5764\n")
    cat("▶ F1 Score (Diabetic)               : 0.7086\n")
    cat("▶ Balanced Accuracy                 : 0.9370\n")
    cat("▶ AUC (ROC Curve)                   : 0.9861\n\n")
    
    cat("💡 Notes:\n")
    cat("- The model uses a dynamic threshold of 0.09906 (determined by ROC analysis).\n")
    cat("- Class '1' = Diabetic | Class '0' = Non-Diabetic.\n")
    cat("- Precision is lower due to class imbalance, though recall is high.\n")
  })
  
  output$xgb_confmat <- renderTable({
    matrix(c(16485, 120, 1867, 1681), nrow = 2, byrow = TRUE,
           dimnames = list("Predicted" = c("0", "1"), "Actual" = c("0", "1")))
  })
  output$xgb_roc_plot <- renderImage({
    filename <- file.path("www/xgb_roc.jpg")
    list(src = filename, contentType = 'image/jpeg', width = 400, height = 300)
  }, deleteFile = FALSE)
  
  observeEvent(input$submit_diet, {
    req(input$diet_prediction, input$diet_glucose, input$diet_hba1c, input$diet_age, input$food_pref)
    
    # Load and clean food dataset
    food_df <- read.csv("/Users/ashmita/Desktop/D-Buddy/Food_Cleaned.csv", stringsAsFactors = FALSE)
    food_df$GI <- as.numeric(food_df$GI)
    food_df$GL <- as.numeric(food_df$GL)
    food_df <- food_df[!is.na(food_df$GI) & !is.na(food_df$GL) & food_df$GI > 0 & food_df$GL > 0, ]
    
    # Filter based on food preference
    non_veg_keywords <- c("egg", "chicken", "fish", "meat", "beef", "mutton", "prawn", "turkey")
    if (input$food_pref == "Veg") {
      food_df <- food_df[!grepl(paste(non_veg_keywords, collapse = "|"), tolower(food_df$Food_Item)), ]
    }
    
    # Filter based on diabetic condition
    if (input$diet_prediction == "Diabetic") {
      food_df <- food_df[food_df$GI < ifelse(input$diet_glucose > 180 || input$diet_hba1c > 7, 65, 75), ]
    } else {
      food_df <- food_df[food_df$GI <= 80, ]
    }
    
    # Shuffle for randomness
    food_df <- food_df[sample(nrow(food_df)), ]
    
    # Ensure at least one grains item in breakfast
    grains_pool <- food_df[food_df$Category == "Grains" & food_df$GL < 10, ]
    other_pool <- food_df[food_df$GL < 10 & food_df$Category != "Grains", ]
    
    grain_choice <- if (nrow(grains_pool) >= 1) grains_pool[1, ] else NULL
    other_choices <- head(other_pool[!(other_pool$Food_Item %in% grain_choice$Food_Item), ], 2)
    breakfast_items <- rbind(grain_choice, other_choices)
    
    # Fill lunch and dinner
    lunch_items <- head(food_df[food_df$GL >= 10 & food_df$GL <= 20, ], 3)
    dinner_items <- head(food_df[food_df$GL > 20, ], 3)
    
    # Fallbacks if not enough items
    if (nrow(breakfast_items) < 3) {
      extra <- head(food_df[!(food_df$Food_Item %in% breakfast_items$Food_Item), ], 3 - nrow(breakfast_items))
      breakfast_items <- rbind(breakfast_items, extra)
    }
    if (nrow(lunch_items) < 3) {
      extra <- head(food_df[!(food_df$Food_Item %in% c(breakfast_items$Food_Item, lunch_items$Food_Item)), ], 3 - nrow(lunch_items))
      lunch_items <- rbind(lunch_items, extra)
    }
    if (nrow(dinner_items) < 3) {
      extra <- head(food_df[!(food_df$Food_Item %in% c(breakfast_items$Food_Item, lunch_items$Food_Item, dinner_items$Food_Item)), ], 3 - nrow(dinner_items))
      dinner_items <- rbind(dinner_items, extra)
    }
    
    # Combine into meal plan
    meal_plan <- rbind(
      data.frame(Meal = "Breakfast", breakfast_items[1:3, ]),
      data.frame(Meal = "Lunch", lunch_items[1:3, ]),
      data.frame(Meal = "Dinner", dinner_items[1:3, ])
    )
    
    # Age-based note
    age_note <- if (input$diet_age > 50) {
      "Note: Since you are over 50, consider adding more fiber and reducing sodium."
    } else {
      ""
    }
    
    # Render output
    output$diet_feedback <- renderUI({
      tagList(
        h4("Personalized 3-Course Diet Plan"),
        tableOutput("diet_table"),
        if (age_note != "") tags$p(strong(age_note))
      )
    })
    
    output$diet_table <- renderTable({
      meal_plan[, c("Meal", "Food_Item", "Category", "GI", "GL")]
    })
  })
  }

# Run the application
shinyApp(ui = ui, server = server)
