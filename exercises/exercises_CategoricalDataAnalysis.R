# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_CategoricalDataAnalysis, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Categorical Data Analysis Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_ui("1", "Complete the function to perform a logistic regression, on Dependent Variable 'y', and two Independent Variables 'x1' and 'x2'.",
                      "hint1_LogisticRegression", "solution1_LogisticRegression", "code_editor1_LogisticRegression", "run_code1_LogisticRegression", "check_code1_LogisticRegression", "result_area1_LogisticRegression", 
                      "perform_logistic_regression <- function(data) {\n  # Fit a logistic regression model\n  model <- ___(___ ~ ___ + ___, data = data, family = binomial)\n  # Return the summary of the model\n  return(summary(model))\n}"),
          generate_mcq_ui("2", "Choose the correct option: What does the exponentiated coefficient (exp(coef)) represent in a logistic regression model?",
                          "mcq1_LogisticRegression", list("The odds ratio" = "The odds ratio", "The log odds" = "The log odds", "The probability" = "The probability", "The residual" = "The residual"), 
                          "mcq_feedback1_LogisticRegression"),
          generate_ui("3", "Complete the function to perform the Hosmer-Lemeshow test in R.",
                      "hint2_LogisticRegression", "solution2_LogisticRegression", "code_editor2_LogisticRegression", "run_code2_LogisticRegression", "check_code2_LogisticRegression", "result_area2_LogisticRegression",
                      "perform_hoslem_test <- function(data, model) {\n  # Perform the Hosmer-Lemeshow test\n  library(ResourceSelection)\n  test_result <- ___(data$y, fitted(model))\n  # Return the test result\n  return(test_result)\n}"),
          generate_mcq_ui("4", "Choose the correct option: What is the purpose of the ROC curve?",
                          "mcq2_LogisticRegression", list("To assess the goodness of fit" = "To assess the goodness of fit", "To compare models" = "To compare models", "To evaluate model performance" = "To evaluate model performance", "To calculate the odds ratio" = "To calculate the odds ratio"), 
                          "mcq_feedback2_LogisticRegression")
        ),
        textOutput("submission_status_LogisticRegression")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_LogisticRegression = NULL,
    plot_output1_LogisticRegression = NULL,
    code_output2_LogisticRegression = NULL,
    plot_output2_LogisticRegression = NULL,
    code_output3_LogisticRegression = NULL,
    plot_output3_LogisticRegression = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Perform logistic regression
  run_code("run_code1_LogisticRegression", "code_editor1_LogisticRegression", "result_area1_LogisticRegression", session)
  show_hint("Use the glm() function to fit the logistic regression model and extract the summary.", "hint1_LogisticRegression", session)
  rv$attempts1_LogisticRegression <- reactiveVal(0)
  show_solution("Correct Code: perform_logistic_regression <- function(data) { model <- glm(y ~ x1 + x2, data = data, family = binomial); return(summary(model)) }", rv$attempts1_LogisticRegression, "solution1_LogisticRegression", session)
  check_code("perform_logistic_regression <- function(data) { model <- glm(y ~ x1 + x2, data = data, family = binomial); return(summary(model)) }", "check_code1_LogisticRegression", "code_editor1_LogisticRegression", "10.1", rv$attempts1_LogisticRegression, rv, session)
  
  # Exercise 2: MCQ logistic regression coefficients
  rv$attempts_mcq1_LogisticRegression <- reactiveVal(0)
  observeEvent(input$mcq1_LogisticRegression, {
    selected_answer <- input$mcq1_LogisticRegression
    correct_answer <- "The odds ratio"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_LogisticRegression(rv$attempts_mcq1_LogisticRegression() + 1)
      if (rv$attempts_mcq1_LogisticRegression() >= 2) {
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
    question_id_2 <- "10.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: Perform Hosmer-Lemeshow test
  run_code("run_code2_LogisticRegression", "code_editor2_LogisticRegression", "result_area2_LogisticRegression", session)
  show_hint("Use the hoslem test function from the ResourceSelection package to perform the test.", "hint2_LogisticRegression", session)
  rv$attempts2_LogisticRegression <- reactiveVal(0)
  show_solution("Correct Code: perform_hoslem_test <- function(data, model) { library(ResourceSelection); test_result <- hoslem.test(data$y, fitted(model)); return(test_result) }", rv$attempts2_LogisticRegression, "solution2_LogisticRegression", session)
  check_code("perform_hoslem_test <- function(data, model) { library(ResourceSelection); test_result <- hoslem.test(data$y, fitted(model)); return(test_result) }", "check_code2_LogisticRegression", "code_editor2_LogisticRegression", "10.3", rv$attempts2_LogisticRegression, rv, session)
  
  # Exercise 4: MCQ ROC curve
  rv$attempts_mcq2_LogisticRegression <- reactiveVal(0)
  observeEvent(input$mcq2_LogisticRegression, {
    selected_answer <- input$mcq2_LogisticRegression
    correct_answer <- "To evaluate model performance"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_LogisticRegression(rv$attempts_mcq2_LogisticRegression() + 1)
      if (rv$attempts_mcq2_LogisticRegression() >= 2) {
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
    question_id_4 <- "10.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_LogisticRegression, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_LogisticRegression
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "10.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("perform_logistic_regression <- function(data) { model <- glm(y ~ x1 + x2, data = data, family = binomial); return(summary(model)) }", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_LogisticRegression
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "10.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "The odds ratio")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_LogisticRegression
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "10.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("perform_hoslem_test <- function(data, model) { library(ResourceSelection); test_result <- hoslem.test(data$y, fitted(model)); return(test_result) }", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_LogisticRegression
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "10.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "To evaluate model performance")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    output$submission_status_LogisticRegression <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})