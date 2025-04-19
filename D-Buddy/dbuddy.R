library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(corrplot)
library(ggpubr)
library(DT)
library(GGally)

# Load dataset
df <- read.csv("C:/Users/rohit/OneDrive/Desktop/D-Buddy/diabetes_prediction_dataset.csv")

# Create age groups for visualization
df$age_group <- cut(df$age, breaks = c(0, 20, 40, 60, 80, 100), 
                    labels = c("0-20", "21-40", "41-60", "61-80", "81+"))

# Define UI
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
                box(title = "BMI Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("bmi_univariate"), textOutput("bmi_conclusion")),
                box(title = "Smoking History", status = "primary", solidHeader = TRUE, 
                    plotOutput("smoking_univariate"), textOutput("smoking_conclusion"))
              ),
              fluidRow(
                box(title = "HbA1c Level Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("hba1c_univariate"), textOutput("hba1c_conclusion")),
                box(title = "Blood Glucose Level Distribution", status = "primary", solidHeader = TRUE, 
                    plotOutput("blood_glucose_univariate"), textOutput("blood_glucose_conclusion"))
              )
      ),
      
      # Section 3: Bivariate Analysis
      tabItem(tabName = "bivariate",
              h2("Bivariate Analysis"),
              fluidRow(
                box(title = "Age vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("age_diabetes_plot"), textOutput("age_diabetes_conclusion")),
                box(title = "BMI vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("bmi_diabetes_plot"), textOutput("bmi_diabetes_conclusion"))
              ),
              fluidRow(
                box(title = "Blood Glucose vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("glucose_diabetes_plot"), textOutput("glucose_diabetes_conclusion")),
                box(title = "HbA1c Level vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("hba1c_diabetes_plot"), textOutput("hba1c_diabetes_conclusion"))
              ),
              fluidRow(
                box(title = "Hypertension vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("hypertension_diabetes_plot"), textOutput("hypertension_diabetes_conclusion")),
                box(title = "Heart Disease vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("heart_disease_diabetes_plot"), textOutput("heart_disease_diabetes_conclusion"))
              ),
              fluidRow(
                box(title = "Smoking History vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("smoking_diabetes_plot"), textOutput("smoking_diabetes_conclusion")),
                box(title = "Gender vs Diabetes", status = "primary", solidHeader = TRUE, 
                    plotOutput("gender_diabetes_plot"), textOutput("gender_diabetes_conclusion"))
              )
      ),
      
      # Section 4: Predictive Analysis of Diabetes
      tabItem(tabName = "multivariate",
              h2("Multivariate Analysis"),
              fluidRow(
                box(title = "Correlation Heatmap", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("correlation_heatmap"))
              ),
              
              # Pairwise Scatter Plot
              fluidRow(
                box(title = "Pairwise Scatter Plot", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("pairwise_plot"))
              ),
              
              # violin plot for BMI, Blood Glucose,HbA1C vs Diabetes
              fluidRow(
                box(title = "Violin plot for BMI, Blood Glucose,HbA1C vs Diabetes", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("multivariate_violin_plot"))
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


# Define Server
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
  
  
  # BMI Distribution
  output$bmi_univariate <- renderPlot({
    ggplot(df, aes(x = bmi)) +
      geom_histogram(fill = "purple", bins = 30, color = "black", alpha = 0.7) +
      labs(title = "BMI Distribution", x = "BMI", y = "Count") +
      theme_minimal()
  })
  output$bmi_conclusion <- renderText({
    "BMI follows a normal distribution, with most values ranging between 25-30, indicating a large proportion of overweight individuals."
  })
  
  
  
  # Smoking History Distribution
  output$smoking_univariate <- renderPlot({
    ggplot(df, aes(x = smoking_history, fill = smoking_history)) +
      geom_bar() +
      labs(title = "Smoking History Distribution", x = "Smoking History", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_minimal()
  })
  output$smoking_conclusion <- renderText({
    "Smoking history varies across the dataset, with a notable percentage of individuals having a history of smoking."
  })
  
  # HbA1c Level Box Plot
  output$hba1c_univariate <- renderPlot({
    ggplot(df, aes(y = HbA1c_level)) +
      geom_boxplot(fill = "orange", color = "black", outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      labs(title = "HbA1c Level Distribution", y = "HbA1c Level") +
      theme_minimal()
  })
  output$hba1c_conclusion <- renderText({
    "The box plot shows that most individuals have normal HbA1c levels, but some outliers indicate higher values, suggesting diabetes risk."
  })
  
  # Blood Glucose Level Box Plot
  output$blood_glucose_univariate <- renderPlot({
    ggplot(df, aes(y = blood_glucose_level)) +
      geom_boxplot(fill = "red", color = "black", outlier.colour = "blue", outlier.shape = 16, outlier.size = 2) +
      labs(title = "Blood Glucose Level Distribution", y = "Blood Glucose Level") +
      theme_minimal()
  })
  output$blood_glucose_conclusion <- renderText({
    "The box plot indicates a wide range of blood glucose levels, with some extreme values that may correspond to diabetic individuals."
  })
  
  output$age_diabetes_plot <- renderPlot({
    ggplot(df, aes(x = age, fill = factor(diabetes))) +
      geom_histogram(bins = 30, position = "dodge") +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Diabetes", "Diabetes")) +
      labs(title = "Age vs Diabetes", x = "Age", y = "Count", fill = "Diabetes Status") +
      theme_minimal()
  })
  output$age_diabetes_conclusion <- renderText({
    "Diabetes is more common in older individuals, with the highest cases observed above 50 years."
  })
  
  # BMI vs Diabetes
  output$bmi_diabetes_plot <- renderPlot({
    ggplot(df, aes(x = factor(diabetes), y = bmi, fill = factor(diabetes))) +
      geom_boxplot() +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Diabetes", "Diabetes")) +
      labs(title = "BMI vs Diabetes", x = "Diabetes Status", y = "BMI") +
      theme_minimal()
  })
  output$bmi_diabetes_conclusion <- renderText({
    "Individuals with diabetes tend to have a higher BMI on average."
  })
  
  # Blood Glucose vs Diabetes
  output$glucose_diabetes_plot <- renderPlot({
    ggplot(df, aes(x = factor(diabetes), y = blood_glucose_level, fill = factor(diabetes))) +
      geom_boxplot() +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Diabetes", "Diabetes")) +
      labs(title = "Blood Glucose Level vs Diabetes", x = "Diabetes Status", y = "Blood Glucose Level") +
      theme_minimal()
  })
  output$glucose_diabetes_conclusion <- renderText({
    "Higher blood glucose levels are strongly associated with diabetes."
  })
  
  # HbA1c Level vs Diabetes
  output$hba1c_diabetes_plot <- renderPlot({
    ggplot(df, aes(x = factor(diabetes), y = HbA1c_level, fill = factor(diabetes))) +
      geom_boxplot() +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Diabetes", "Diabetes")) +
      labs(title = "HbA1c Level vs Diabetes", x = "Diabetes Status", y = "HbA1c Level") +
      theme_minimal()
  })
  output$hba1c_diabetes_conclusion <- renderText({
    "Diabetic individuals have significantly higher HbA1c levels."
  })
  
  # Hypertension vs Diabetes
  output$hypertension_diabetes_plot <- renderPlot({
    ggplot(df, aes(x = factor(hypertension), fill = factor(diabetes))) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Diabetes", "Diabetes")) +
      labs(title = "Hypertension vs Diabetes", x = "Hypertension (0 = No, 1 = Yes)", y = "Count") +
      theme_minimal()
  })
  output$hypertension_diabetes_conclusion <- renderText({
    "Diabetes prevalence is higher among individuals with hypertension."
  })
  
  # Heart Disease vs Diabetes
  output$heart_disease_diabetes_plot <- renderPlot({
    ggplot(df, aes(x = factor(heart_disease), fill = factor(diabetes))) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Diabetes", "Diabetes")) +
      labs(title = "Heart Disease vs Diabetes", x = "Heart Disease (0 = No, 1 = Yes)", y = "Count") +
      theme_minimal()
  })
  output$heart_disease_diabetes_conclusion <- renderText({
    "Individuals with heart disease have a higher chance of having diabetes."
  })
  
  # Smoking History vs Diabetes
  output$smoking_diabetes_plot <- renderPlot({
    ggplot(df, aes(x = smoking_history, fill = factor(diabetes))) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Diabetes", "Diabetes")) +
      labs(title = "Smoking History vs Diabetes", x = "Smoking History", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_minimal()
  })
  output$smoking_diabetes_conclusion <- renderText({
    "Smoking history appears to have a correlation with diabetes prevalence."
  })
  
  # Gender vs Diabetes
  output$gender_diabetes_plot <- renderPlot({
    ggplot(df, aes(x = gender, fill = factor(diabetes))) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Diabetes", "Diabetes")) +
      labs(title = "Gender vs Diabetes", x = "Gender", y = "Count") +
      theme_minimal()
  })
  output$gender_diabetes_conclusion <- renderText({
    "Both genders are affected by diabetes, but the distribution varies slightly."
  })
  
  # Ensure relevant columns are numeric for correlation analysis
  numeric_vars <- df[, sapply(df, is.numeric)]
  
  # ✅ **Fix: Remove zero variance columns**
  numeric_vars <- numeric_vars[, apply(numeric_vars, 2, function(x) sd(x, na.rm = TRUE) > 0)]
  
  # ✅ Remove zero variance columns
  numeric_vars <- numeric_vars[, apply(numeric_vars, 2, function(x) sd(x, na.rm = TRUE) > 0)]
  
  # 1️⃣ Correlation Heatmap
  output$correlation_heatmap <- renderPlot({
    corr_matrix <- cor(numeric_vars, use = "complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black")
  })
  
  # 2️⃣ Pairwise Scatter Plot (Fixed Column Names)
  output$pairwise_plot <- renderPlot({
    ggpairs(df, columns = c("age", "bmi", "blood_glucose_level", "hypertension", "heart_disease", "diabetes"),
            aes(color = factor(diabetes)))
  })
  
  # Multivariate Analysis: Violin Plot for BMI, Blood Glucose, HbA1c vs Diabetes
  output$multivariate_violin_plot <- renderPlot({
    if (!require(tidyverse)) install.packages("tidyverse")
    if (!require(tidyr)) install.packages("tidyr")
    df_long <- df %>%
      pivot_longer(cols = c(bmi, blood_glucose_level, HbA1c_level), 
                   names_to = "Variable", values_to = "Value")
    
    ggplot(df_long, aes(x = factor(diabetes), y = Value, fill = factor(diabetes))) +
      geom_violin(alpha = 0.6, trim = FALSE) +
      geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA) +
      facet_wrap(~Variable, scales = "free") +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Diabetes", "Diabetes")) +
      labs(title = "Multivariate Analysis: Distribution of BMI, Blood Glucose, and HbA1c",
           x = "Diabetes Status", y = "Value") +
      theme_minimal()
  })
  
  # Ensure correct filtering of Diabetes Mellitus cases (only value 2)
  diabetes_df <- reactive({
    df[df$diabetes == 1, ]  # Filtering for Diabetes Mellitus only
  })
  
  # Compute summary statistics
  output$total_diabetes <- renderText({
    nrow(diabetes_df())
  })
  
  output$avg_age_diabetes <- renderText({
    if (nrow(diabetes_df()) > 0) {
      round(mean(diabetes_df()$age, na.rm = TRUE), 1)
    } else {
      "Data Not Available"
    }
  })
  
  output$gender_distribution <- renderText({
    if (nrow(diabetes_df()) > 0) {
      table(diabetes_df()$gender) %>% paste(collapse = ", ")
    } else {
      "Data Not Available"
    }
  })
  
  output$avg_glucose_diabetes <- renderText({
    if (nrow(diabetes_df()) > 0) {
      round(mean(diabetes_df()$blood_glucose_level, na.rm = TRUE), 1)
    } else {
      "Data Not Available"
    }
  })
    
}

# Run the application̥
shinyApp(ui = ui, server = server)