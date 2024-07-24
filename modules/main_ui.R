# main_ui.R

source("modules/progress.R")

# Define the user interface
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  uiOutput("title_panel"),
  tabsetPanel(
    id = "main_tabset",
    tabPanel("Theory",
             navlistPanel(
               id = "navlist",
               tabPanel("1. R Fundamentals", 
                        htmlOutput("theory_RFundamentals"),
                        actionButton("exercises_RFundamentals", "Go to Exercises")),
               tabPanel("2. Data Structures", 
                        htmlOutput("theory_DataStructures"),
                        actionButton("exercises_DataStructures", "Go to Exercises")),
               tabPanel("3. Data Visualization", 
                        htmlOutput("theory_DataVisualization"),
                        actionButton("exercises_DataVisualization", "Go to Exercises")),
               tabPanel("4. Data Analysis Types", 
                        htmlOutput("theory_DataAnalysisTypes"),
                        actionButton("exercises_DataAnalysisTypes", "Go to Exercises")),
               tabPanel("5. Descriptive Statistics", 
                        htmlOutput("theory_DescriptiveStatistics"),
                        actionButton("exercises_DescriptiveStatistics", "Go to Exercises")),
               tabPanel("6. Probability Statistics", 
                        htmlOutput("theory_ProbabilityStatistics"),
                        actionButton("exercises_ProbabilityStatistics", "Go to Exercises")),
               tabPanel("7. Inferential Hypothesis", 
                        htmlOutput("theory_InferentialHypothesis"),
                        actionButton("exercises_InferentialHypothesis", "Go to Exercises")),
               tabPanel("8. ANOVA", 
                        htmlOutput("theory_ANOVA"),
                        actionButton("exercises_ANOVA", "Go to Exercises")),
               tabPanel("9. Regression Analysis", 
                        htmlOutput("theory_RegressionAnalysis"),
                        actionButton("exercises_RegressionAnalysis", "Go to Exercises")),
               tabPanel("10. Categorical Data Analysis", 
                        htmlOutput("theory_CategoricalDataAnalysis"),
                        actionButton("exercises_CategoricalDataAnalysis", "Go to Exercises")),
               tabPanel("Code Challenge 1", 
                        htmlOutput("theory_CodeChallenge1"),
                        actionButton("CodeChallenge1", "Go To Challenge")),
               tabPanel("Code Challenge 2", 
                        htmlOutput("theory_CodeChallenge2"),
                        actionButton("CodeChallenge2", "Go To Challenge")),
               tabPanel("Code Challenge 3", 
                        htmlOutput("theory_CodeChallenge3"),
                        actionButton("CodeChallenge3", "Go To Challenge")),
               tabPanel("11. Data Wrangling", 
                        htmlOutput("theory_DataWrangling"),
                        actionButton("exercises_DataWrangling", "Go to Exercises")),
               tabPanel("12. Advanced R Programming", 
                        htmlOutput("theory_AdvancedRProgramming"),
                        actionButton("exercises_AdvancedRProgramming", "Go to Exercises"))
             )
    ),
    tabPanel("Exercises",
             uiOutput("exercise_page")
    ),
    
    tabPanel("Code Challenge",
             uiOutput("challenge_page")
    ),
    
    tabPanel("Student Info",
             progressUI("progress"),
    )
  )
)