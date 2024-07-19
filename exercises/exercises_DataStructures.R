# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_DataStructures, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Data Structures and Data Handling Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_ui("1", "Complete the function to create a numeric vector and return the sum of its elements.",
                      "hint1_DataStructures", "solution1_DataStructures", "code_editor1_DataStructures", "run_code1_DataStructures", "check_code1_DataStructures", "result_area1_DataStructures", 
                      "sum_vector <- function() {\n  # Create a numeric vector with values 1 to 5\n  numbers <-___ \n  # Return the sum of the elements\n  return(___)\n}"),
          generate_mcq_ui("2", "Choose the correct option: What is the output of `matrix(1:6, nrow=2)` in R?",
                          "mcq1_DataStructures", list("2x3 matrix" = "2x3 matrix", "3x2 matrix" = "3x2 matrix", "1x6 matrix" = "1x6 matrix", "6x1 matrix" = "6x1 matrix"), 
                          "mcq_feedback1_DataStructures"),
          generate_ui("3", "Complete the function to create a list containing a character vector and a numeric vector.",
                      "hint2_DataStructures", "solution2_DataStructures", "code_editor2_DataStructures", "run_code2_DataStructures", "check_code2_DataStructures", "result_area2_DataStructures",
                      "create_list <- function() {\n  # Create a character vector with elements 'a', 'b', 'c'\n  char_vector <-___ \n  # Create a numeric vector with values 1, 2, 3\n  num_vector <-___ \n  # Combine into a list\n  my_list <- list(char_vector, num_vector)\n  return(my_list)\n}"),
          generate_mcq_ui("4", "Choose the correct option: Which function is used to access elements of a data frame in R?",
                          "mcq2_DataStructures", list("subset()" = "subset()", "filter()" = "filter()", "select()" = "select()", "dplyr::filter()" = "dplyr::filter()"), 
                          "mcq_feedback2_DataStructures")
        ),
        textOutput("submission_status_DataStructures")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_DataStructures = NULL,
    plot_output1_DataStructures = NULL,
    code_output2_DataStructures = NULL,
    plot_output2_DataStructures = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Sum vector
  run_code("run_code1_DataStructures", "code_editor1_DataStructures", "result_area1_DataStructures", session)
  show_hint("Use the c() function to create the vector and sum() to find the sum.", "hint1_DataStructures", session)
  rv$attempts1_DataStructures <- reactiveVal(0)
  show_solution("Correct Code: sum_vector <- function() { numbers <- c(1, 2, 3, 4, 5); return(sum(numbers)) }", rv$attempts1_DataStructures, "solution1_DataStructures", session)
  check_code("sum_vector <- function() { numbers <- c(1, 2, 3, 4, 5); return(sum(numbers)) }", "check_code1_DataStructures", "code_editor1_DataStructures", "2.1", rv$attempts1_DataStructures, rv, session)
  
  # Exercise 2: MCQ matrix(1:6, nrow=2)
  rv$attempts_mcq1_DataStructures <- reactiveVal(0)
  observeEvent(input$mcq1_DataStructures, {
    selected_answer <- input$mcq1_DataStructures
    correct_answer <- "2x3 matrix"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_DataStructures(rv$attempts_mcq1_DataStructures() + 1)
      if (rv$attempts_mcq1_DataStructures() >= 2) {
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
    question_id_2 <- "2.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: Create list
  run_code("run_code2_DataStructures", "code_editor2_DataStructures", "result_area2_DataStructures", session)
  show_hint("Use the list() function to create the list with character and numeric vectors.", "hint2_DataStructures", session)
  rv$attempts2_DataStructures <- reactiveVal(0)
  show_solution("Correct Code: create_list <- function() { char_vector <- c('a', 'b', 'c'); num_vector <- c(1, 2, 3); my_list <- list(char_vector, num_vector); return(my_list) }", rv$attempts2_DataStructures, "solution2_DataStructures", session)
  check_code("create_list <- function() { char_vector <- c('a', 'b', 'c'); num_vector <- c(1, 2, 3); my_list <- list(char_vector, num_vector); return(my_list) }", "check_code2_DataStructures", "code_editor2_DataStructures", "2.3", rv$attempts2_DataStructures, rv, session)
  
  # Exercise 4: MCQ data frame access
  rv$attempts_mcq2_DataStructures <- reactiveVal(0)
  observeEvent(input$mcq2_DataStructures, {
    selected_answer <- input$mcq2_DataStructures
    correct_answer <- "dplyr::filter()"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_DataStructures(rv$attempts_mcq2_DataStructures() + 1)
      if (rv$attempts_mcq2_DataStructures() >= 2) {
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
    question_id_4 <- "2.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_DataStructures, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_DataStructures
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "2.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("sum_vector <- function() { numbers <- c(1, 2, 3, 4, 5); return(sum(numbers)) }", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_DataStructures
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "2.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "2x3 matrix")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_DataStructures
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "2.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("create_list <- function() { char_vector <- c('a', 'b', 'c'); num_vector <- c(1, 2, 3); my_list <- list(char_vector, num_vector); return(my_list) }", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_DataStructures
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "2.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "dplyr::filter()")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    session$output$submission_status_DataStructures <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})