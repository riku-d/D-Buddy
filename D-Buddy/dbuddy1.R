library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

# Load Dataset
data <- read.csv("C:/Users/rohit/OneDrive/Desktop/D-Buddy/diabetes.csv")

# UI
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
          menuItem("DataSet Description", tabName = "dashboard", icon = icon("search")),
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
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Data Summary", width = 6, verbatimTextOutput("summary")),
                box(title = "Filter Data", width = 6,
                    selectInput("filter_var", "Select Variable:", choices = names(data)),
                    uiOutput("filter_ui"))
              ),
              fluidRow(
                box(title = "Dataset", width = 12, DTOutput("data_table"))
              )
      ),
      
      tabItem(tabName = "univariate",
              fluidRow(
                box(width = 6, title = "Histogram", solidHeader = TRUE, status = "info",
                    selectInput("hist_var", "Select Variable:", choices = names(data)),
                    plotOutput("histogram_plot")
                ),
                box(width = 6, title = "Box Plot", solidHeader = TRUE, status = "warning",
                    selectInput("box_var", "Select Variable:", choices = names(data)),
                    plotOutput("boxplot_plot")
                )
              ),
              
              fluidRow(
                box(width = 6, title = "Density Plot", solidHeader = TRUE, status = "success",
                    selectInput("density_var", "Select Variable:", choices = names(data)),
                    plotOutput("density_plot")
                ),
                box(width = 6, title = "Violin Plot", solidHeader = TRUE, status = "danger",
                    selectInput("violin_var", "Select Variable:", choices = names(data)),
                    plotOutput("violin_plot")
                )
              )
              
      ),
      
      tabItem(tabName = "bivariate",
              fluidRow(
                box(title = "Scatter Plot (Grouped by Outcome)", width = 6, solidHeader = TRUE, status = "primary",
                    selectInput("scatter_x", "Select X-axis Variable:", choices = names(data)),
                    selectInput("scatter_y", "Select Y-axis Variable:", choices = names(data)),
                    plotOutput("scatter_plot")
                ),
                box(title = "Box Plot (Grouped by Outcome)", width = 6, solidHeader = TRUE, status = "warning",
                    selectInput("box_y", "Select Variable for Box Plot:", choices = names(data)),
                    plotOutput("boxplot")
                )
              ),
              
              fluidRow(
                box(title = "Density Contour Plot (Grouped by Outcome)", width = 6, solidHeader = TRUE, status = "info",
                    selectInput("density_x", "Select X-axis Variable:", choices = names(data)),
                    selectInput("density_y", "Select Y-axis Variable:", choices = names(data)),
                    plotOutput("density_contour")
                ),
                box(title = "Bivariate Histogram (Grouped by Outcome)", width = 6, solidHeader = TRUE, status = "danger",
                    selectInput("bivar_hist_var", "Select Variable for Histogram:", choices = names(data)),
                    plotOutput("bivariate_histogram")
                )
              )
      )
      
    )
  )
)

# Server
server <- function(input, output, session) {
  output$summary <- renderPrint({ summary(data) })
  
  output$filter_ui <- renderUI({
    if (is.numeric(data[[input$filter_var]])) {
      sliderInput("filter_value", "Filter Range:",
                  min = min(data[[input$filter_var]], na.rm = TRUE),
                  max = max(data[[input$filter_var]], na.rm = TRUE),
                  value = range(data[[input$filter_var]], na.rm = TRUE))
    } else {
      selectInput("filter_value", "Select Category:", choices = unique(data[[input$filter_var]]))
    }
  })
  
  output$data_table <- renderDT({
    filtered_data <- data
    if (is.numeric(data[[input$filter_var]])) {
      filtered_data <- subset(data, data[[input$filter_var]] >= input$filter_value[1] & 
                                data[[input$filter_var]] <= input$filter_value[2])
    } else {
      filtered_data <- subset(data, data[[input$filter_var]] == input$filter_value)
    }
    datatable(filtered_data)
  })
  
  output$histogram_plot <- renderPlot({
    ggplot(data, aes_string(x = input$hist_var)) +
      geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
      theme_minimal()
  })
  
  output$boxplot_plot <- renderPlot({
    ggplot(data, aes_string(y = input$box_var)) +
      geom_boxplot(fill = "orange") +
      theme_minimal()
  })
  
  output$density_plot <- renderPlot({
    ggplot(data, aes_string(x = input$density_var)) +
      geom_density(fill = "purple", alpha = 0.5) +
      theme_minimal()
  })
  
  output$violin_plot <- renderPlot({
    req(input$violin_var)  # Ensure a variable is selected
    
    ggplot(data, aes_string(x = "1", y = input$violin_var)) +  
      geom_violin(fill = "red", alpha = 0.7) +  # Violin plot (density distribution)
      geom_boxplot(width = 0.1, fill = "red", outlier.shape = NA) +  # Boxplot inside
      theme_minimal() +
      labs(x = "", y = input$violin_var, title = "Violin Plot with Boxplot Overlay")
  })
  
  
  # Scatter Plot (Grouped by Outcome)
  output$scatter_plot <- renderPlot({
    req(input$scatter_x, input$scatter_y)
    
    ggplot(data, aes_string(x = input$scatter_x, y = input$scatter_y, color = "as.factor(Outcome)")) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Outcome", labels = c("Non-Diabetic (0)", "Diabetic (1)")) +
      theme_minimal() +
      labs(title = "Scatter Plot Grouped by Outcome", x = input$scatter_x, y = input$scatter_y)
  })
  
  # Box Plot (Grouped by Outcome)
  output$boxplot <- renderPlot({
    req(input$box_x, input$box_y)
    
    ggplot(data, aes_string(x = "as.factor(Outcome)", y = input$box_y, fill = "as.factor(Outcome)")) +
      geom_boxplot() +
      scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Outcome", labels = c("Non-Diabetic (0)", "Diabetic (1)")) +
      theme_minimal() +
      labs(title = "Box Plot Grouped by Outcome", x = "Outcome", y = input$box_y)
  })
  
  # Density Contour Plot (Grouped by Outcome)
  output$density_contour <- renderPlot({
    req(input$density_x, input$density_y)
    
    ggplot(data, aes_string(x = input$density_x, y = input$density_y, color = "as.factor(Outcome)")) +
      geom_density2d() +  
      geom_point(alpha = 0.5) +  
      scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Outcome", labels = c("Non-Diabetic (0)", "Diabetic (1)")) +
      theme_minimal() +
      labs(title = "Density Contour Plot Grouped by Outcome", x = input$density_x, y = input$density_y)
  })
  
  # Bivariate Histogram (Grouped by Outcome)
  output$bivariate_histogram <- renderPlot({
    req(input$bivar_hist_var)
    
    ggplot(data, aes_string(x = input$bivar_hist_var, fill = "as.factor(Outcome)")) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
      scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Outcome", labels = c("Non-Diabetic (0)", "Diabetic (1)")) +
      theme_minimal() +
      labs(title = paste("Bivariate Histogram of", input$bivar_hist_var, "by Outcome"),
           x = input$bivar_hist_var, y = "Count")
  })
  
  
}

shinyApp(ui, server)