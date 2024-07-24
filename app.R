# app.R
library(shiny)
library(shinyAce)

# Load required modules
source("modules/exercise_observers.R")
source("modules/challenge_observers.R")
source("database/db_init.R")
source("database/db_operations.R")
initialize_db("database/database.sqlite")

# Source UI definitions
source("modules/main_ui.R")
source("modules/admin_page.R")

# Define server logic
server <- function(input, output, session) {
  rv <- reactiveValues(user_type = NULL, student_name = NULL, student_id = NULL, progress = NULL)
  
  # Show modal dialog for user login
  showModal(modalDialog(
    textInput("student_name", "Enter your name:"),
    actionButton("submit_name", "Submit"),
    actionButton("admin_login", "Admin Login"),
    easyClose = FALSE,
    footer = NULL
  ))
  
  # Observe event for name submission
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$user_type <- "student"
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Observe event for admin login
  observeEvent(input$admin_login, {
    showModal(modalDialog(
      textInput("admin_username", "Enter Admin Username:"),
      passwordInput("admin_password", "Enter Admin Password:"),
      actionButton("admin_login_submit", "Login"),
      easyClose = FALSE,
      footer = NULL
    ))
  })
  
  # Validate admin login
  observeEvent(input$admin_login_submit, {
    if (validate_admin(input$admin_username, input$admin_password)) {
      rv$user_type <- "admin"
      removeModal()
    } else {
      showModal(modalDialog(
        textInput("admin_username", "Enter Admin Username:"),
        passwordInput("admin_password", "Enter Admin Password:"),
        div(style = "color: red;", "Invalid Username or Password. Please try again."),
        actionButton("admin_login_submit", "Login"),
        easyClose = FALSE,
        footer = NULL
      ))
    }
  })
  
  # Logout functionality
  observeEvent(input$logout, {
    rv$user_type <- NULL
    rv$student_name <- NULL
    rv$student_id <- NULL
    session$reload()
  })
  
  # Conditional UI based on user type
  output$user_interface <- renderUI({
    if (is.null(rv$user_type)) {
      return(NULL)
    } else if (rv$user_type == "student") {
      ui
    } else if (rv$user_type == "admin") {
      adminPageUI("admin")
    }
  })
  
  # Update title panel for student
  observe({
    req(rv$student_name)
    output$title_panel <- renderUI({
      fluidRow(
        column(10, h1(paste("Interactive R Course"))),
        column(10, h3(HTML(paste("Working as ", 
                                 "<span style='color: green;'>", rv$student_name, "</span>", "")))),
        column(2, actionButton("logout", "Logout", style = "float:right; margin-top: 20px;"))
      )
    })
  })
  
  # Topics and file names
  topics <- c("R Fundamentals", "Data Structures", "Data Visualization",
              "Data Analysis Types", "Descriptive Statistics", "Probability Statistics",
              "Inferential Hypothesis", "ANOVA", "Regression Analysis",
              "Categorical Data Analysis", "Code Challenge 1", 
              "Code Challenge 2", "Code Challenge 3", "Data Wrangling", "Advanced R Programming")
  file_names <- c("01-R-Fundamentals.html", "02-DataStructures.html", 
                  "03-DataVisualization.html", "04-DataAnalysisTypes.html", 
                  "05-DescriptiveStatistics.html", "06-Probability-Statistics.html", 
                  "07-Inferential-Hypothesis.html", "08-ANOVA.html", 
                  "09-RegressionAnalysis.html", "10-CategoricalDataAnalysis.html",
                  "11-CodeChallenge-1.html", "12-CodeChallenge-2.html",
                  "13-CodeChallenge-3.html","14-DataWrangling.html", 
                  "15-AdvancedRProgramming.html")
  
  # Render UI for topics
  for (i in seq_along(file_names)) {
    theory_output_id <- paste("theory", gsub(" ", "", gsub("and", "And", topics[i])), sep = "_")
    local({
      output_id <- theory_output_id
      file_name <- file_names[i]
      output[[output_id]] <- renderUI({
        tryCatch({
          tags$iframe(src = file_name, class = "responsive-iframe")
        }, error = function(e) {
          div("Error loading content: ", e$message)
        })
      })
    })
  }
  
  # Initialize observers for student content
  observeEvent(rv$user_type == "student", {
    req(rv$student_name)
    initializeExerciseObservers(input, output, session, rv)
    initializeChallengeObservers(input, output, session, rv)
    progressServer("progress", rv)
  })
  
  # Initialize admin server logic
  observeEvent(rv$user_type == "admin", {
    adminPageServer("admin", rv)
  })
}

# Run the application
shinyApp(ui = fluidPage(uiOutput("user_interface")), server = server)