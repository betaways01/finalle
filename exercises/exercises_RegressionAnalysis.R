# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_RegressionAnalysis, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Regression Analysis Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_ui("1", "Complete the function to perform a simple linear regression in R (Dependent Variable as 'sales', and Independent Variable as 'advertising').",
                      "hint1_Regression", "solution1_Regression", "code_editor1_Regression", "run_code1_Regression", "check_code1_Regression", "result_area1_Regression", 
                      "perform_simple_regression <- function(data) {\n  # Fit a simple linear regression model\n  model <- ___(___ ~ ___, data = data)\n  # Return the summary of the model\n  return(summary(model))\n}"),
          generate_mcq_ui("2", "Choose the correct option: What does the coefficient of an independent variable represent in a regression model?",
                          "mcq1_Regression", list("The change in the dependent variable for a one-unit change in the independent variable" = "The change in the dependent variable for a one-unit change in the independent variable", "The change in the independent variable for a one-unit change in the dependent variable" = "The change in the independent variable for a one-unit change in the dependent variable", "The total variance explained by the model" = "The total variance explained by the model", "The significance level of the model" = "The significance level of the model"), 
                          "mcq_feedback1_Regression"),
          generate_ui("3", "Complete the function to perform a multiple linear regression on 'prices', with 2 Independent Variables 'sqft' and 'bedrooms'.",
                      "hint2_Regression", "solution2_Regression", "code_editor2_Regression", "run_code2_Regression", "check_code2_Regression", "result_area2_Regression",
                      "perform_multiple_regression <- function(data) {\n  # Fit a multiple linear regression model\n  model <- ___(___ ~ ___ + ___, data = data)\n  # Return the summary of the model\n  return(summary(model))\n}"),
          generate_mcq_ui("4", "Choose the correct option: Which diagnostic plot is used to check for heteroscedasticity?",
                          "mcq2_Regression", list("Residuals vs Fitted" = "Residuals vs Fitted", "Normal Q-Q" = "Normal Q-Q", "Scale-Location" = "Scale-Location", "Residuals vs Leverage" = "Residuals vs Leverage"), 
                          "mcq_feedback2_Regression")
        ),
        textOutput("submission_status_Regression")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_Regression = NULL,
    plot_output1_Regression = NULL,
    code_output2_Regression = NULL,
    plot_output2_Regression = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Perform simple linear regression
  run_code("run_code1_Regression", "code_editor1_Regression", "result_area1_Regression", session)
  show_hint("Use the lm() function to fit the simple linear regression model and extract the summary.", "hint1_Regression", session)
  rv$attempts1_Regression <- reactiveVal(0)
  show_solution("Correct Code: perform_simple_regression <- function(data) { model <- lm(sales ~ advertising, data = data); return(summary(model)) }", rv$attempts1_Regression, "solution1_Regression", session)
  check_code("perform_simple_regression <- function(data) { model <- lm(sales ~ advertising, data = data); return(summary(model)) }", "check_code1_Regression", "code_editor1_Regression", "9.1", rv$attempts1_Regression, rv, session)
  
  # Exercise 2: MCQ regression coefficients
  rv$attempts_mcq1_Regression <- reactiveVal(0)
  observeEvent(input$mcq1_Regression, {
    selected_answer <- input$mcq1_Regression
    correct_answer <- "The change in the dependent variable for a one-unit change in the independent variable"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_Regression(rv$attempts_mcq1_Regression() + 1)
      if (rv$attempts_mcq1_Regression() >= 2) {
        showModal(modalDialog(
          div(class = "feedback-incorrect", paste("Incorrect. The correct answer is", correct_answer, ".")),
          footer = NULL,
          easyClose = TRUE,
          fade = TRUE
        ))
      } else {
        showModal(modalDialog(
          div(class = "feedback-incorrect", "Incorrect. Try again."),
          footer = NULL,
          easyClose = TRUE,
          fade = TRUE
        ))
      }
    }
    
    student_id <- rv$student_id
    question_id_2 <- "9.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: Perform multiple linear regression
  run_code("run_code2_Regression", "code_editor2_Regression", "result_area2_Regression", session)
  show_hint("Use the lm() function to fit the multiple linear regression model and extract the summary.", "hint2_Regression", session)
  rv$attempts2_Regression <- reactiveVal(0)
  show_solution("Correct Code: perform_multiple_regression <- function(data) { model <- lm(prices ~ sqft + bedrooms, data = data); return(summary(model)) }", rv$attempts2_Regression, "solution2_Regression", session)
  check_code("perform_multiple_regression <- function(data) { model <- lm(prices ~ sqft + bedrooms, data = data); return(summary(model)) }", "check_code2_Regression", "code_editor2_Regression", "9.3", rv$attempts2_Regression, rv, session)
  
  # Exercise 4: MCQ diagnostic plot
  rv$attempts_mcq2_Regression <- reactiveVal(0)
  observeEvent(input$mcq2_Regression, {
    selected_answer <- input$mcq2_Regression
    correct_answer <- "Residuals vs Fitted"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_Regression(rv$attempts_mcq2_Regression() + 1)
      if (rv$attempts_mcq2_Regression() >= 2) {
        showModal(modalDialog(
          div(class = "feedback-incorrect", paste("Incorrect. The correct answer is", correct_answer, ".")),
          footer = NULL,
          easyClose = TRUE,
          fade = TRUE
        ))
      } else {
        showModal(modalDialog(
          div(class = "feedback-incorrect", "Incorrect. Try again."),
          footer = NULL,
          easyClose = TRUE,
          fade = TRUE
        ))
      }
    }
    
    student_id <- rv$student_id
    question_id_4 <- "9.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_Regression, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_Regression
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "9.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("perform_simple_regression <- function(data) { model <- lm(sales ~ advertising, data = data); return(summary(model)) }", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_Regression
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "9.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "The change in the dependent variable for a one-unit change in the independent variable")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_Regression
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "9.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("perform_multiple_regression <- function(data) { model <- lm(prices ~ sqft + bedrooms, data = data); return(summary(model)) }", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_Regression
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "9.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "Residuals vs Fitted")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    output$submission_status_Regression <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})