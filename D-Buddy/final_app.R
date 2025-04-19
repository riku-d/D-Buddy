library(shiny)
library(shinydashboard)
library(dplyr)
library(mice)
library(ggplot2)
library(corrplot)
library(GGally)
library(DT)
setwd("C:/Users/rohit/OneDrive/Desktop/D-Buddy/Datasets/diabetes_prediction")
df1<-read.csv("diabetes.csv")
df2<-read.csv("diabetes_prediction_dataset.csv")
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

df$smoking_history <- as.character(df$smoking_history)
df$smoking_history[df$smoking_history %in% c("No Info") | is.na(df$smoking_history)] <- "Unknown"
df$smoking_history <- as.factor(df$smoking_history)

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
          menuItem("DataSet Description", tabName = "eda", icon = icon("search")),
          menuItem("Descriptive Analysis", icon = icon("chart-bar"), tabName = "descriptive",
                   menuSubItem("Univariate Analysis", tabName = "univariate", icon = icon("chart-pie")),
                   menuSubItem("Bivariate Analysis", tabName = "bivariate", icon = icon("chart-line")),
                   menuSubItem("Multivariate Analysis", tabName = "multivariate", icon = icon("project-diagram"))
          ),
          menuItem("Predictive Analysis", tabName = "predict", icon = icon("line-chart")),
          menuItem("Diet & Exercise Recommendation", tabName = "diet", icon = icon("apple-alt")),
          menuItem("24x7 Chatbot Support", tabName = "chatbot", icon = icon("robot")),
          menuItem("Insights", tabName = "insights", icon = icon("lightbulb"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Section 1: Descriptive Analysis of Diabetes
      tabItem(tabName = "eda",
              h2("Dataset Description"),
              fluidRow(
                box(title = "Dataset Summary", status = "info", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("dataset_summary"))
              ),
              fluidRow(
                box(title = "Filter Data", status = "warning", solidHeader = TRUE, width = 12,
                    selectInput("gender_filter", "Select Gender:", 
                                choices = c("All", unique(df$gender)), selected = "All"),
                    sliderInput("age_filter", "Select Age Range:", 
                                min = min(df$age), max = max(df$age),
                                value = c(min(df$age), max(df$age)))
                )
              ),
              fluidRow(
                box(title = "Filtered Dataset", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("filtered_table"))
              ),
              # Unique Values Section
              fluidRow(
                box(title = "Unique Values in Each Column", status = "primary", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("unique_values"))
              )
      ),
      
      # Section 2: Univariate Analysis
      tabItem(tabName = "univariate",
              h2("Univariate Analysis"),
              fluidRow(
                box(title = "Glucose Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_univariate1"), textOutput("glucose_conclusion1")),
                box(title = "Glucose Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_univariate2"), textOutput("glucose_conclusion2"))
              ),
              fluidRow(
                box(title = "Skin Thickness Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("skin_univariate1"), textOutput("skin_conclusion1")),
                box(title = "Skin Thickness Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("skin_univariate2"), textOutput("skin_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Insulin Level Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("insulin_univariate1"), textOutput("insulin_conclusion1")),
                box(title = "Insulin Level Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("insulin_univariate2"), textOutput("insulin_conclusion2"))
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
                box(title = "Gender Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("gender_univariate1"), textOutput("gender_conclusion1")),
                box(title = "Gender Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("gender_univariate2"), textOutput("gender_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Blood Pressure Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("bp_univariate1"), textOutput("bp_conclusion1")),
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
                box(title = "Gender Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("smoke_univariate2"), textOutput("smoke_conclusion2"))
              ),
              
              fluidRow(
                box(title = "Diabetes Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("diabetes_univariate1"), textOutput("diabetes_conclusion1")),
                box(title = "Diabetes Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("diabetes_univariate2"), textOutput("diabetes_conclusion2"))
              )
              
      
      ),
      
      # Section 3: Bivariate Analysis
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
      tabItem(tabName = "predict",
              h2("Predictive Analysis of Diabetes"),
              p("This section will contain machine learning models for diabetes prediction.")
      ),
      
      # Section 6: Personalized Diet & Exercise Recommendation
      tabItem(tabName = "diet",
              h2("Personalized Diet & Exercise Recommendation"),
              p("This section will provide personalized diet and exercise recommendations.")
      ),
      
      # Section 7: 24x7 Chatbot Support
      tabItem(tabName = "chatbot",
              h2("24x7 Chatbot Support"),
              p("This section will include a chatbot for diabetes-related queries.")
      ),
      
      # Section 8: Insights
      tabItem(tabName = "insights",
              h2("Diabetes Mellitus Insights"),
              
              # Overview of Diabetes
              fluidRow(
                box(title = "Understanding Diabetes Mellitus", status = "primary", solidHeader = TRUE, width = 12,
                    p("Diabetes Mellitus is a chronic medical condition where the body either does not produce enough insulin or cannot effectively use the insulin it produces. 
                Insulin is a hormone responsible for regulating blood sugar levels. Without proper insulin function, blood glucose levels rise, leading to various health complications."),
                    p("There are several types of diabetes, including Type 1, Type 2, and gestational diabetes. Type 2 diabetes is the most common form and is often associated with lifestyle factors such as diet, physical activity, and obesity."),
                    p("According to our dataset, there are a total of ", strong(textOutput("total_diabetes", inline = TRUE)), 
                      " recorded cases of Diabetes Mellitus. The average age of individuals diagnosed with diabetes is ", 
                      strong(textOutput("avg_age_diabetes", inline = TRUE)), " years."),
                    p("Among diabetic individuals, the gender distribution is: ",
                      strong(textOutput("gender_distribution", inline = TRUE)), "."),
                    p("Blood glucose levels play a crucial role in diabetes management. Our dataset reveals that the average blood glucose level for diabetic patients is ",
                      strong(textOutput("avg_glucose_diabetes", inline = TRUE)), " mg/dL.")
                )
              ),
      )
    )
  )
)
server <- function(input, output) {
  # Dataset Summary
  output$dataset_summary <- renderPrint({
    summary(df)
  })
  
  # Filtered dataset based on user input
  filtered_data <- reactive({
    data <- df
    if (input$gender_filter != "All") {
      data <- data[data$gender == input$gender_filter, ]
    }
    data <- data[data$age >= input$age_filter[1] & data$age <= input$age_filter[2], ]
    return(data)
  })
  
  # Render filtered data table
  output$filtered_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$unique_values <- renderPrint({
    unique_values <- lapply(df, function(column) unique(column))
    
    explanations <- list(
      gender = "Gender: 0 = Female, 1 = Male",
      smoking_history = "Smoking History: Never, Former, Current, Unknown",
      hypertension = "Hypertension: 0 = No, 1 = Yes",
      heart_disease = "Heart Disease: 0 = No, 1 = Yes",
      diabetes = "Diabetes Status: 0 = No, 1 = Yes"
    )
    
    cat("Unique values in each column:\n")
    for (col in names(unique_values)) {
      cat("\n", col, ":", unique_values[[col]], "\n")
      if (col %in% names(explanations)) {
        cat("  → ", explanations[[col]], "\n")
      }
    }
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
  
  
  output$skin_univariate1 <- renderPlot({
    df$SkinThickness[df$SkinThickness == 0] <- NA
    ggplot(df, aes(x = SkinThickness)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      labs(title = "Skin Thickness Distribution", x = "Skin Thickness", y = "Count") +
      theme_minimal()
  })
  output$skin_conclusion1 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  output$skin_univariate2 <- renderPlot({
    ggplot(df, aes(y = SkinThickness)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "Skin Thickness Boxplot", y = "Skin Thickness") +
      theme_minimal()
  })
  output$skin_conclusion2 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  
  output$insulin_univariate1 <- renderPlot({
    df$Insulin[df$Insulin == 0] <- NA
    imputed <- mice(df, method = "pmm", m = 1, seed = 123)
    df <- complete(imputed)
    ggplot(df, aes(x = Insulin)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 50) +
      labs(title = "Insulin Level Distribution", x = "Insulin", y = "Frequency") +
      theme_minimal()
  })
  output$insulin_conclusion1 <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  output$insulin_univariate2 <- renderPlot({
    df$Insulin[df$Insulin == 0] <- NA
    imputed <- mice(df, method = "pmm", m = 1, seed = 123)
    df <- complete(imputed)
    ggplot(df, aes(y = Insulin)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "Insulin Level Distribution", y = "Insulin level") +
      theme_minimal()
  })
  output$insulin_conclusion2 <- renderText({
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
  
  output$bp_univariate1 <- renderPlot({
    ggplot(df, aes(x = BP_category)) +
      geom_bar(fill = "steelblue") +
      theme_minimal() +
      labs(title = "Diastolic Blood Pressure Categories", x = "Category", y = "Count")
  })
  output$bp_conclusion1 <- renderText({
    "The box plot shows that most individuals have normal HbA1c levels, but some outliers indicate higher values, suggesting diabetes risk."
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
  
}

# Run the application̥
shinyApp(ui = ui, server = server)
