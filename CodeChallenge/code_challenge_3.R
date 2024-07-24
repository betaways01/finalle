# code_challenge_3.R
source("database/db_operations.R")
source("modules/cc_helpers.R")
source("modules/utils.R")

observeEvent(input$CodeChallenge3, {
  reset_challenge(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Code Challenge")
  
  output$challenge_page <- renderUI({
    fluidPage(
      titlePanel("R Fundamentals Exercises"),
      mainPanel(
        tabsetPanel(
          id = "challenge_tabset",
          generate_challenge_ui("1", "Complete the function to perform a linear regression using lm(), on independent variable x, dependent variable y, and return the model summary.",
                                "code_editor1_CodeChallenge3", "run_code1_CodeChallenge3", "check_code1_CodeChallenge3", "result_area1_CodeChallenge3", 
                                "perform_regression <- function(data) {\n  # input dependent and independent variables\n  model <- __(__ ~ __, data = data)\n  # return model summary\n  return(__)\n}"),
          generate_challenge_mcq_ui("2", "Regression Analysis: Which of the following statements is NOT true about linear regression?",
                                    "mcq1_CodeChallenge3", list("a) It estimates a straight line that best fits the relationship between two continuous variables." = "a", "b) It minimizes the squared residuals (differences between predicted and actual values)." = "b", "c) It can be used for classification tasks like predicting spam emails." = "c", "d) The slope of the regression line indicates the direction and strength of the linear relationship." = "d"), 
                                    "mcq_feedback1_CodeChallenge3"),
          generate_challenge_ui("3", "Complete the function to perform an ANOVA test using aov(), on independent variable x, dependent variable y, and return the ANOVA summary.",
                                "code_editor2_CodeChallenge3", "run_code2_CodeChallenge3", "check_code2_CodeChallenge3", "result_area2_CodeChallenge3",
                                "perform_anova <- function(data) {\n  # input dependent and independent variables\n  model <- __(__ ~ __, data = data)\n  # return ANOVA summary\n  return(__)\n}"),
          generate_challenge_mcq_ui("4", "ANOVA (Analysis of Variance): What is the main purpose of ANOVA?",
                                    "mcq2_CodeChallenge3", list("a) To estimate the strength of a linear relationship between two variables." = "a", "b) To compare the means of two or more groups and assess statistical significance." = "b", "c) To predict the value of a dependent variable based on an independent variable." = "c", "d) To identify clusters or groups within a dataset." = "d"), 
                                    "mcq_feedback2_CodeChallenge3"),
          generate_challenge_ui("5", "Complete the function to perform a logistic regression using glm(), on independent variable x, binary outcome variable y, and return the model summary.",
                                "code_editor3_CodeChallenge3", "run_code3_CodeChallenge3", "check_code3_CodeChallenge3", "result_area3_CodeChallenge3",
                                "perform_logistic_regression <- function(data) {\n  # input dependent and independent variables\n  model <- __(__ ~ __, data = data, family = binomial)\n  # return model summary\n  return(__)\n}"),
          generate_challenge_mcq_ui("6", "Logistic Regression: What is the key difference between logistic regression and linear regression?",
                                    "mcq3_CodeChallenge3", list("a) Logistic regression deals with continuous dependent variables, while linear regression handles categorical ones." = "a", "b) Logistic regression models the probability of a binary outcome (0 or 1), while linear regression predicts continuous values." = "b", "c) Logistic regression is more robust to outliers compared to linear regression." = "c", "d) There's no significant difference; they both handle the same types of problems." = "d"), 
                                    "mcq_feedback3_CodeChallenge3"),
          generate_challenge_ui("7", "Complete the function to perform a summary operation on a given data frame `data` and return the summary.",
                                "code_editor4_CodeChallenge3", "run_code4_CodeChallenge3", "check_code4_CodeChallenge3", "result_area4_CodeChallenge3",
                                "perform_summary <- function(data) {\n  # return summary of data\n  return(__)\n}"),
          generate_challenge_mcq_ui("8", "Data Structures in R: Which data structure in R is best suited to store a collection of items with different data types (e.g., names, ages, addresses)?",
                                    "mcq4_CodeChallenge3", list("a) Vector (all elements must be the same data type)" = "a", "b) Matrix (all elements must be the same data type)" = "b", "c) Data Frame (can hold elements of different data types)" = "c", "d) List (can hold elements of different data types)" = "d"), 
                                    "mcq_feedback4_CodeChallenge3"),
          generate_challenge_ui("9", "Complete the function to generate a summary of a data frame `data` using the summary() function.",
                                "code_editor5_CodeChallenge3", "run_code5_CodeChallenge3", "check_code5_CodeChallenge3", "result_area5_CodeChallenge3",
                                "generate_summary <- function(data) {\n  # generate summary of data\n  summary_data <- __(__)\n  return(__)\n}"),
          generate_challenge_mcq_ui("10", "Data Handling in R: How can you read a comma-separated values (CSV) file into an R data frame?",
                                    "mcq5_CodeChallenge3", list("a) Using the load function" = "a", "b) Using the read.csv function" = "b", "c) Using the attach function (not recommended for most cases)" = "c", "d) Using the source function" = "d"), 
                                    "mcq_feedback5_CodeChallenge3")
        ),
        textOutput("submission_status_CodeChallenge3")
      )
    )
  })
  
  message("Student name when CodeChallenge3 clicked: ", rv$student_name)
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_CodeChallenge3 = NULL,
    plot_output1_CodeChallenge3 = NULL,
    code_output2_CodeChallenge3 = NULL,
    plot_output2_CodeChallenge3 = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Linear Regression function
  run_challenge_code("run_code1_CodeChallenge3", "code_editor1_CodeChallenge3", "result_area1_CodeChallenge3", session)
  rv$attempts1_CodeChallenge3 <- reactiveVal(0)
  check_challenge_code("perform_regression <- function(data) { model <- lm(y ~ x, data = data); return(summary(model)) }", "check_code1_CodeChallenge3", "code_editor1_CodeChallenge3", "cc3.1", rv$attempts1_CodeChallenge3, rv, session)
  
  # Exercise 2: MCQ regression analysis
  rv$attempts_mcq1_CodeChallenge3 <- reactiveVal(0)
  check_challenge_mcq("c", "mcq1_CodeChallenge3", "cc3.2", "mcq_feedback1_CodeChallenge3", rv$attempts_mcq1_CodeChallenge3, rv, session)
  
  # Exercise 3: ANOVA function
  run_challenge_code("run_code2_CodeChallenge3", "code_editor2_CodeChallenge3", "result_area2_CodeChallenge3", session)
  rv$attempts2_CodeChallenge3 <- reactiveVal(0)
  check_challenge_code("perform_anova <- function(data) { model <- aov(y ~ x, data = data); return(summary(model)) }", "check_code2_CodeChallenge3", "code_editor2_CodeChallenge3", "cc3.3", rv$attempts2_CodeChallenge3, rv, session)
  
  # Exercise 4: MCQ ANOVA
  rv$attempts_mcq2_CodeChallenge3 <- reactiveVal(0)
  check_challenge_mcq("b", "mcq2_CodeChallenge3", "cc3.4", "mcq_feedback2_CodeChallenge3", rv$attempts_mcq2_CodeChallenge3, rv, session)
  
  # Exercise 5: Logistic Regression function
  run_challenge_code("run_code3_CodeChallenge3", "code_editor3_CodeChallenge3", "result_area3_CodeChallenge3", session)
  rv$attempts3_CodeChallenge3 <- reactiveVal(0)
  check_challenge_code("perform_logistic_regression <- function(data) { model <- glm(y ~ x, data = data, family = binomial); return(summary(model)) }", "check_code3_CodeChallenge3", "code_editor3_CodeChallenge3", "cc3.5", rv$attempts3_CodeChallenge3, rv, session)
  
  # Exercise 6: MCQ logistic regression
  rv$attempts_mcq3_CodeChallenge3 <- reactiveVal(0)
  check_challenge_mcq("b", "mcq3_CodeChallenge3", "cc3.6", "mcq_feedback3_CodeChallenge3", rv$attempts_mcq3_CodeChallenge3, rv, session)
  
  # Exercise 7: Summary function
  run_challenge_code("run_code4_CodeChallenge3", "code_editor4_CodeChallenge3", "result_area4_CodeChallenge3", session)
  rv$attempts4_CodeChallenge3 <- reactiveVal(0)
  check_challenge_code("perform_summary <- function(data) { return(summary(data)) }", "check_code4_CodeChallenge3", "code_editor4_CodeChallenge3", "cc3.7", rv$attempts4_CodeChallenge3, rv, session)
  
  # Exercise 8: MCQ data structures
  rv$attempts_mcq4_CodeChallenge3 <- reactiveVal(0)
  check_challenge_mcq("d", "mcq4_CodeChallenge3", "cc3.8", "mcq_feedback4_CodeChallenge3", rv$attempts_mcq4_CodeChallenge3, rv, session)
  
  # Exercise 9: Generate summary function
  run_challenge_code("run_code5_CodeChallenge3", "code_editor5_CodeChallenge3", "result_area5_CodeChallenge3", session)
  rv$attempts5_CodeChallenge3 <- reactiveVal(0)
  check_challenge_code("generate_summary <- function(data) { summary_data <- summary(data); return(summary_data) }", "check_code5_CodeChallenge3", "code_editor5_CodeChallenge3", "cc3.9", rv$attempts5_CodeChallenge3, rv, session)
  
  # Exercise 10: MCQ data handling
  rv$attempts_mcq5_CodeChallenge3 <- reactiveVal(0)
  check_challenge_mcq("b", "mcq5_CodeChallenge3", "cc3.10", "mcq_feedback5_CodeChallenge3", rv$attempts_mcq5_CodeChallenge3, rv, session)
  
  observeEvent(input$submit_exercise_CodeChallenge3, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_CodeChallenge3
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "cc3.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("perform_regression <- function(data) { model <- lm(y ~ x, data = data); return(summary(model)) }", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_CodeChallenge3
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "cc3.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "c")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_CodeChallenge3
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "cc3.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("perform_anova <- function(data) { model <- aov(y ~ x, data = data); return(summary(model)) }", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_CodeChallenge3
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "cc3.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "b")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    # Question 5
    answer_5 <- session$input$code_editor3_CodeChallenge3
    if (!is.null(answer_5) && length(answer_5) > 0) {
      question_id_5 <- "cc3.5"
      attempt_count_5 <- get_attempt_count(student_id, question_id_5) + 1
      is_correct_5 <- gradethis::grade_code("perform_logistic_regression <- function(data) { model <- glm(y ~ x, data = data, family = binomial); return(summary(model)) }", answer_5)$correct
      add_submission(student_id, question_id_5, answer_5, is_correct_5, attempt_count_5)
      if (is_correct_5) score <- score + 1
    }
    
    # Question 6
    answer_6 <- session$input$mcq3_CodeChallenge3
    if (!is.null(answer_6) && length(answer_6) > 0) {
      question_id_6 <- "cc3.6"
      attempt_count_6 <- get_attempt_count(student_id, question_id_6) + 1
      is_correct_6 <- (answer_6 == "b")
      add_submission(student_id, question_id_6, answer_6, is_correct_6, attempt_count_6)
      if (is_correct_6) score <- score + 1
    }
    
    # Question 7
    answer_7 <- session$input$code_editor4_CodeChallenge3
    if (!is.null(answer_7) && length(answer_7) > 0) {
      question_id_7 <- "cc3.7"
      attempt_count_7 <- get_attempt_count(student_id, question_id_7) + 1
      is_correct_7 <- gradethis::grade_code("perform_summary <- function(data) { return(summary(data)) }", answer_7)$correct
      add_submission(student_id, question_id_7, answer_7, is_correct_7, attempt_count_7)
      if (is_correct_7) score <- score + 1
    }
    
    # Question 8
    answer_8 <- session$input$mcq4_CodeChallenge3
    if (!is.null(answer_8) && length(answer_8) > 0) {
      question_id_8 <- "cc3.8"
      attempt_count_8 <- get_attempt_count(student_id, question_id_8) + 1
      is_correct_8 <- (answer_8 == "d")
      add_submission(student_id, question_id_8, answer_8, is_correct_8, attempt_count_8)
      if (is_correct_8) score <- score + 1
    }
    
    # Question 9
    answer_9 <- session$input$code_editor5_CodeChallenge3
    if (!is.null(answer_9) && length(answer_9) > 0) {
      question_id_9 <- "cc3.9"
      attempt_count_9 <- get_attempt_count(student_id, question_id_9) + 1
      is_correct_9 <- gradethis::grade_code("generate_summary <- function(data) { summary_data <- summary(data); return(summary_data) }", answer_9)$correct
      add_submission(student_id, question_id_9, answer_9, is_correct_9, attempt_count_9)
      if (is_correct_9) score <- score + 1
    }
    
    # Question 10
    answer_10 <- session$input$mcq5_CodeChallenge3
    if (!is.null(answer_10) && length(answer_10) > 0) {
      question_id_10 <- "cc3.10"
      attempt_count_10 <- get_attempt_count(student_id, question_id_10) + 1
      is_correct_10 <- (answer_10 == "b")
      add_submission(student_id, question_id_10, answer_10, is_correct_10, attempt_count_10)
      if (is_correct_10) score <- score + 1
    }
    
    update_overall_score(student_id)
    session$output$submission_status_CodeChallenge3 <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/10"))
  })
})