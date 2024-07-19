# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_RFundamentals, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("R Fundamentals Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_ui("1", "Complete the function to return the square of a number.",
                      "hint1_RFundamentals", "solution1_RFundamentals", "code_editor1_RFundamentals", "run_code1_RFundamentals", "check_code1_RFundamentals", "result_area1_RFundamentals", 
                      "square <- function(x) {\n  # Complete the code here\n}"),
          generate_mcq_ui("2", "Choose the correct option: What is the output of `sum(1, 2, 3)` in R?",
                          "mcq1_RFundamentals", list("5" = "5", "6" = "6", "7" = "7", "None of the Above" = "None of the Above"), 
                          "mcq_feedback1_RFundamentals"),
          generate_ui("3", "Complete the function to check if a number is even.",
                      "hint2_RFundamentals", "solution2_RFundamentals", "code_editor2_RFundamentals", "run_code2_RFundamentals", "check_code2_RFundamentals", "result_area2_RFundamentals",
                      "is_even <- function(x) {\n  # Complete the code here\n}"),
          generate_mcq_ui("4", "Choose the correct option: Which function is used to calculate the mean in R?",
                          "mcq2_RFundamentals", list("mean()" = "mean()", "median()" = "median()", "sum()" = "sum()", "None of the Above" = "None of the Above"), 
                          "mcq_feedback2_RFundamentals")
        ),
        textOutput("submission_status_RFundamentals")
      )
    )
  })
  
  message("Student name when exercises_RFundamentals clicked: ", rv$student_name)
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_RFundamentals = NULL,
    plot_output1_RFundamentals = NULL,
    code_output2_RFundamentals = NULL,
    plot_output2_RFundamentals = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Square function
  run_code("run_code1_RFundamentals", "code_editor1_RFundamentals", "result_area1_RFundamentals", session)
  show_hint("Try using the `^` operator to find the square of a number.", "hint1_RFundamentals", session)
  rv$attempts1_RFundamentals <- reactiveVal(0)
  show_solution("Correct Code: square <- function(x) { return(x^2) }", rv$attempts1_RFundamentals, "solution1_RFundamentals", session)
  check_code("square <- function(x) { return(x^2) }", "check_code1_RFundamentals", "code_editor1_RFundamentals", "1.1", rv$attempts1_RFundamentals, rv, session)
  
  # Exercise 2: MCQ sum(1, 2, 3)
  rv$attempts_mcq1_RFundamentals <- reactiveVal(0)
  observeEvent(input$mcq1_RFundamentals, {
    selected_answer <- input$mcq1_RFundamentals
    correct_answer <- "6"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_RFundamentals(rv$attempts_mcq1_RFundamentals() + 1)
      if (rv$attempts_mcq1_RFundamentals() >= 2) {
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
    question_id_2 <- "1.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: Even function
  run_code("run_code2_RFundamentals", "code_editor2_RFundamentals", "result_area2_RFundamentals", session)
  show_hint("Use the modulus operator `%%` to check for even numbers.", "hint2_RFundamentals", session)
  rv$attempts2_RFundamentals <- reactiveVal(0)
  show_solution("Correct Code: is_even <- function(x) { return(x %% 2 == 0) }", rv$attempts2_RFundamentals, "solution2_RFundamentals", session)
  check_code("is_even <- function(x) { return(x %% 2 == 0) }", "check_code2_RFundamentals", "code_editor2_RFundamentals", "1.3", rv$attempts2_RFundamentals, rv, session)
  
  # Exercise 4: MCQ mean() function
  rv$attempts_mcq2_RFundamentals <- reactiveVal(0)
  observeEvent(input$mcq2_RFundamentals, {
    selected_answer <- input$mcq2_RFundamentals
    correct_answer <- "mean()"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_RFundamentals(rv$attempts_mcq2_RFundamentals() + 1)
      if (rv$attempts_mcq2_RFundamentals() >= 2) {
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
    question_id_4 <- "1.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_RFundamentals, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_RFundamentals
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "1.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("square <- function(x) { return(x^2) }", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_RFundamentals
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "1.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "6")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_RFundamentals
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "1.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("is_even <- function(x) { return(x %% 2 == 0) }", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_RFundamentals
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "1.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "mean()")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    session$output$submission_status_RFundamentals <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})