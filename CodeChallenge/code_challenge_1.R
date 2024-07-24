# code_challenge_1.R
source("database/db_operations.R")
source("modules/cc_helpers.R")
source("modules/utils.R")

observeEvent(input$CodeChallenge1, {
  reset_challenge(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Code Challenge")
  
  output$challenge_page <- renderUI({
    fluidPage(
      titlePanel("R Fundamentals Exercises"),
      mainPanel(
        tabsetPanel(
          id = "challenge_tabset",
          generate_challenge_ui("1", "Complete the function to return the cube of a number.",
                                "code_editor1_CodeChallenge1", "run_code1_CodeChallenge1", "check_code1_CodeChallenge1", "result_area1_CodeChallenge1", 
                                "cube <- function(x) {\n  # Complete the code here\n  return(___)\n}"),
          generate_challenge_mcq_ui("2", "Data Structures: In R, which data type can hold elements of different types?",
                                    "mcq1_CodeChallenge1", list("Vector" = "Vector", "List" = "List", "Matrix" = "Matrix", "Array" = "Array"), 
                                    "mcq_feedback1_CodeChallenge1"),
          generate_challenge_ui("3", "Complete the function to check if a number is a prime number.",
                                "code_editor2_CodeChallenge1", "run_code2_CodeChallenge1", "check_code2_CodeChallenge1", "result_area2_CodeChallenge1",
                                "is_prime <- function(n) {\n  if (n <= 1) return(FALSE)\n  for (i in 2:__(n)) {\n    if (n %% i == 0) return(FALSE)\n  }\n  return(TRUE)\n}"),
          generate_challenge_mcq_ui("4", "Basic R Commands: Which function is used to create a sequence of numbers in R?",
                                    "mcq2_CodeChallenge1", list("seq()" = "seq()", "rep()" = "rep()", "c()" = "c()", "paste()" = "paste()"), 
                                    "mcq_feedback2_CodeChallenge1"),
          generate_challenge_ui("5", "Complete the function to return the factorial of a number.",
                                "code_editor3_CodeChallenge1", "run_code3_CodeChallenge1", "check_code3_CodeChallenge1", "result_area3_CodeChallenge1",
                                "factorial <- function(x) {\n  if(x == 0) return(1)\n  # Complete the code here\n  return(__* factorial(__-1))\n}"),
          generate_challenge_mcq_ui("6", "Functions: What is the main purpose of defining a function in R?",
                                    "mcq3_CodeChallenge1", list("To perform repetitive tasks without rewriting code." = "To perform repetitive tasks without rewriting code.", "To access data stored on your computer." = "To access data stored on your computer.", "To connect R to external software." = "To connect R to external software.", "To change the appearance of the R console." = "To change the appearance of the R console."), 
                                    "mcq_feedback3_CodeChallenge1"),
          generate_challenge_ui("7", "Complete the function to calculate the sum of elements in a numeric vector.",
                                "code_editor4_CodeChallenge1", "run_code4_CodeChallenge1", "check_code4_CodeChallenge1", "result_area4_CodeChallenge1",
                                "sum_vector <- function(vec) {\n  sum <- 0\n  for(i in __:__) {\n    sum <- sum + vec[i]\n  }\n  return(sum)\n}"),
          generate_challenge_mcq_ui("8", "Data Manipulation: The c() function in R allows you to:",
                                    "mcq4_CodeChallenge1", list("Combine multiple elements into a single vector." = "Combine multiple elements into a single vector.", "Sort a vector in ascending order." = "Sort a vector in ascending order.", "Calculate the average of a vector." = "Calculate the average of a vector.", "Remove duplicate values from a vector." = "Remove duplicate values from a vector."), 
                                    "mcq_feedback4_CodeChallenge1"),
          generate_challenge_ui("9", "Complete the function to find the maximum value in a numeric vector.",
                                "code_editor5_CodeChallenge1", "run_code5_CodeChallenge1", "check_code5_CodeChallenge1", "result_area5_CodeChallenge1",
                                "max_value <- function(vec) {\n  max <- vec[1]\n  for(i in 2:__) {\n    if(vec[i] > max) max <- vec[i]\n  }\n  return(max)\n}"),
          generate_challenge_mcq_ui("10", "Numbers: How do you write the number 3.14 in R?",
                                    "mcq5_CodeChallenge1", list("\"3.14\" (as a string)" = "\"3.14\" (as a string)", "'3.14' (as a string)" = "'3.14' (as a string)", "3.14 (directly)" = "3.14 (directly)", "It cannot be done in R." = "It cannot be done in R."), 
                                    "mcq_feedback5_CodeChallenge1")
        ),
        textOutput("submission_status_CodeChallenge1")
      )
    )
  })
  
  message("Student name when CodeChallenge1 clicked: ", rv$student_name)
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_CodeChallenge1 = NULL,
    plot_output1_CodeChallenge1 = NULL,
    code_output2_CodeChallenge1 = NULL,
    plot_output2_CodeChallenge1 = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Cube function
  run_challenge_code("run_code1_CodeChallenge1", "code_editor1_CodeChallenge1", "result_area1_CodeChallenge1", session)
  rv$attempts1_CodeChallenge1 <- reactiveVal(0)
  check_challenge_code("cube <- function(x) { return(x^3) }", "check_code1_CodeChallenge1", "code_editor1_CodeChallenge1", "cc1.1", rv$attempts1_CodeChallenge1, rv, session)
  
  # Exercise 2: MCQ data type
  rv$attempts_mcq1_CodeChallenge1 <- reactiveVal(0)
  check_challenge_mcq("List", "mcq1_CodeChallenge1", "cc1.2", "mcq_feedback1_CodeChallenge1", rv$attempts_mcq1_CodeChallenge1, rv, session)
  
  # Exercise 3: Prime number function
  run_challenge_code("run_code2_CodeChallenge1", "code_editor2_CodeChallenge1", "result_area2_CodeChallenge1", session)
  rv$attempts2_CodeChallenge1 <- reactiveVal(0)
  check_challenge_code("is_prime <- function(n) { if (n <= 1) return(FALSE); for (i in 2:sqrt(n)) { if (n %% i == 0) return(FALSE) }; return(TRUE) }", "check_code2_CodeChallenge1", "code_editor2_CodeChallenge1", "cc1.3", rv$attempts2_CodeChallenge1, rv, session)
  
  # Exercise 4: MCQ sequence function
  rv$attempts_mcq2_CodeChallenge1 <- reactiveVal(0)
  check_challenge_mcq("seq()", "mcq2_CodeChallenge1", "cc1.4", "mcq_feedback2_CodeChallenge1", rv$attempts_mcq2_CodeChallenge1, rv, session)
  
  # Exercise 5: Factorial function
  run_challenge_code("run_code3_CodeChallenge1", "code_editor3_CodeChallenge1", "result_area3_CodeChallenge1", session)
  rv$attempts3_CodeChallenge1 <- reactiveVal(0)
  check_challenge_code("factorial <- function(x) { if(x == 0) return(1); return(x * factorial(x-1)) }", "check_code3_CodeChallenge1", "code_editor3_CodeChallenge1", "cc1.5", rv$attempts3_CodeChallenge1, rv, session)
  
  # Exercise 6: MCQ function purpose
  rv$attempts_mcq3_CodeChallenge1 <- reactiveVal(0)
  check_challenge_mcq("To perform repetitive tasks without rewriting code.", "mcq3_CodeChallenge1", "cc1.6", "mcq_feedback3_CodeChallenge1", rv$attempts_mcq3_CodeChallenge1, rv, session)
  
  # Exercise 7: Sum of vector elements
  run_challenge_code("run_code4_CodeChallenge1", "code_editor4_CodeChallenge1", "result_area4_CodeChallenge1", session)
  rv$attempts4_CodeChallenge1 <- reactiveVal(0)
  check_challenge_code("sum_vector <- function(vec) { sum <- 0; for(i in 1:length(vec)) { sum <- sum + vec[i] }; return(sum) }", "check_code4_CodeChallenge1", "code_editor4_CodeChallenge1", "cc1.7", rv$attempts4_CodeChallenge1, rv, session)
  
  # Exercise 8: MCQ c() function
  rv$attempts_mcq4_CodeChallenge1 <- reactiveVal(0)
  check_challenge_mcq("Combine multiple elements into a single vector.", "mcq4_CodeChallenge1", "cc1.8", "mcq_feedback4_CodeChallenge1", rv$attempts_mcq4_CodeChallenge1, rv, session)
  
  # Exercise 9: Maximum value in a vector
  run_challenge_code("run_code5_CodeChallenge1", "code_editor5_CodeChallenge1", "result_area5_CodeChallenge1", session)
  rv$attempts5_CodeChallenge1 <- reactiveVal(0)
  check_challenge_code("max_value <- function(vec) { max <- vec[1]; for(i in 2:length(vec)) { if(vec[i] > max) max <- vec[i] }; return(max) }", "check_code5_CodeChallenge1", "code_editor5_CodeChallenge1", "cc1.9", rv$attempts5_CodeChallenge1, rv, session)
  
  # Exercise 10: MCQ number representation
  rv$attempts_mcq5_CodeChallenge1 <- reactiveVal(0)
  check_challenge_mcq("3.14 (directly)", "mcq5_CodeChallenge1", "cc1.10", "mcq_feedback5_CodeChallenge1", rv$attempts_mcq5_CodeChallenge1, rv, session)
  
  observeEvent(input$submit_exercise_CodeChallenge1, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_CodeChallenge1
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "cc1.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("cube <- function(x) { return(x^3) }", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_CodeChallenge1
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "cc1.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "List")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_CodeChallenge1
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "cc1.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("is_prime <- function(n) { if (n <= 1) return(FALSE); for (i in 2:sqrt(n)) { if (n %% i == 0) return(FALSE) }; return(TRUE) }", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_CodeChallenge1
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "cc1.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "seq()")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    # Question 5
    answer_5 <- session$input$code_editor3_CodeChallenge1
    if (!is.null(answer_5) && length(answer_5) > 0) {
      question_id_5 <- "cc1.5"
      attempt_count_5 <- get_attempt_count(student_id, question_id_5) + 1
      is_correct_5 <- gradethis::grade_code("factorial <- function(x) { if(x == 0) return(1); return(x * factorial(x-1)) }", answer_5)$correct
      add_submission(student_id, question_id_5, answer_5, is_correct_5, attempt_count_5)
      if (is_correct_5) score <- score + 1
    }
    
    # Question 6
    answer_6 <- session$input$mcq3_CodeChallenge1
    if (!is.null(answer_6) && length(answer_6) > 0) {
      question_id_6 <- "cc1.6"
      attempt_count_6 <- get_attempt_count(student_id, question_id_6) + 1
      is_correct_6 <- (answer_6 == "To perform repetitive tasks without rewriting code.")
      add_submission(student_id, question_id_6, answer_6, is_correct_6, attempt_count_6)
      if (is_correct_6) score <- score + 1
    }
    
    # Question 7
    answer_7 <- session$input$code_editor4_CodeChallenge1
    if (!is.null(answer_7) && length(answer_7) > 0) {
      question_id_7 <- "cc1.7"
      attempt_count_7 <- get_attempt_count(student_id, question_id_7) + 1
      is_correct_7 <- gradethis::grade_code("sum_vector <- function(vec) { sum <- 0; for(i in 1:length(vec)) { sum <- sum + vec[i] }; return(sum) }", answer_7)$correct
      add_submission(student_id, question_id_7, answer_7, is_correct_7, attempt_count_7)
      if (is_correct_7) score <- score + 1
    }
    
    # Question 8
    answer_8 <- session$input$mcq4_CodeChallenge1
    if (!is.null(answer_8) && length(answer_8) > 0) {
      question_id_8 <- "cc1.8"
      attempt_count_8 <- get_attempt_count(student_id, question_id_8) + 1
      is_correct_8 <- (answer_8 == "Combine multiple elements into a single vector.")
      add_submission(student_id, question_id_8, answer_8, is_correct_8, attempt_count_8)
      if (is_correct_8) score <- score + 1
    }
    
    # Question 9
    answer_9 <- session$input$code_editor5_CodeChallenge1
    if (!is.null(answer_9) && length(answer_9) > 0) {
      question_id_9 <- "cc1.9"
      attempt_count_9 <- get_attempt_count(student_id, question_id_9) + 1
      is_correct_9 <- gradethis::grade_code("max_value <- function(vec) { max <- vec[1]; for(i in 2:length(vec)) { if(vec[i] > max) max <- vec[i] }; return(max) }", answer_9)$correct
      add_submission(student_id, question_id_9, answer_9, is_correct_9, attempt_count_9)
      if (is_correct_9) score <- score + 1
    }
    
    # Question 10
    answer_10 <- session$input$mcq5_CodeChallenge1
    if (!is.null(answer_10) && length(answer_10) > 0) {
      question_id_10 <- "cc1.10"
      attempt_count_10 <- get_attempt_count(student_id, question_id_10) + 1
      is_correct_10 <- (answer_10 == "3.14 (directly)")
      add_submission(student_id, question_id_10, answer_10, is_correct_10, attempt_count_10)
      if (is_correct_10) score <- score + 1
    }
    
    update_overall_score(student_id)
    session$output$submission_status_CodeChallenge1 <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/10"))
  })
})