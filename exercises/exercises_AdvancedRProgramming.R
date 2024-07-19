# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_AdvancedRProgramming, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Advanced R Programming Techniques Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_mcq_ui("1", "Choose the correct statement: Which of the following is a correct example of using a default value for a function argument?",
                          "mcq1_AdvancedRProgramming", list("my_function <- function(x, y) { result <- x * y; return(result) = 2 }" = "my_function <- function(x, y) { result <- x * y; return(result) = 2 }",
                                                            "my_function <- function(x = 2, y) { result <- x * y; return(result) }" = "my_function <- function(x = 2, y) { result <- x * y; return(result) }",
                                                            "my_function <- function(x, y) { result <- x * y; return(result) }" = "my_function <- function(x, y) { result <- x * y; return(result) }",
                                                            "my_function <- function(x, y = 2) { result <- x * y; return(result) }" = "my_function <- function(x, y = 2) { result <- x * y; return(result) }"), 
                          "mcq_feedback1_AdvancedRProgramming"),
          generate_mcq_ui("2", "Choose the correct statement: Which looping construct will continue to execute as long as a condition is true?",
                          "mcq2_AdvancedRProgramming", list("for loop" = "for loop", "apply function" = "apply function", "while loop" = "while loop", "sapply function" = "sapply function"), 
                          "mcq_feedback2_AdvancedRProgramming"),
          generate_mcq_ui("3", "Choose the correct function: Which function from the apply family applies a function over elements of a list or vector?",
                          "mcq3_AdvancedRProgramming", list("for loop" = "for loop", "while loop" = "while loop", "sapply" = "sapply", "lapply" = "lapply"), 
                          "mcq_feedback3_AdvancedRProgramming"),
          generate_mcq_ui("4", "Choose the correct method: Which method is used to perform parallel processing in R?",
                          "mcq4_AdvancedRProgramming", list("parallel package" = "parallel package", "dplyr package" = "dplyr package", "tidyr package" = "tidyr package", "apply family" = "apply family"), 
                          "mcq_feedback4_AdvancedRProgramming")
        ),
        textOutput("submission_status_AdvancedRProgramming")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    mcq_output1_AdvancedRProgramming = NULL,
    mcq_output2_AdvancedRProgramming = NULL,
    mcq_output3_AdvancedRProgramming = NULL,
    mcq_output4_AdvancedRProgramming = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: MCQ function arguments
  rv$attempts_mcq1_AdvancedRProgramming <- reactiveVal(0)
  observeEvent(input$mcq1_AdvancedRProgramming, {
    selected_answer <- input$mcq1_AdvancedRProgramming
    correct_answer <- "my_function <- function(x, y = 2) { result <- x * y; return(result) }"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_AdvancedRProgramming(rv$attempts_mcq1_AdvancedRProgramming() + 1)
      if (rv$attempts_mcq1_AdvancedRProgramming() >= 2) {
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
    question_id_1 <- "12.1"
    attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
    add_submission(student_id, question_id_1, selected_answer, is_correct, attempt_count_1)
    update_overall_score(student_id)
  })
  
  # Exercise 2: MCQ looping constructs
  rv$attempts_mcq2_AdvancedRProgramming <- reactiveVal(0)
  observeEvent(input$mcq2_AdvancedRProgramming, {
    selected_answer <- input$mcq2_AdvancedRProgramming
    correct_answer <- "while loop"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_AdvancedRProgramming(rv$attempts_mcq2_AdvancedRProgramming() + 1)
      if (rv$attempts_mcq2_AdvancedRProgramming() >= 2) {
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
    question_id_2 <- "12.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: MCQ apply family functions
  rv$attempts_mcq3_AdvancedRProgramming <- reactiveVal(0)
  observeEvent(input$mcq3_AdvancedRProgramming, {
    selected_answer <- input$mcq3_AdvancedRProgramming
    correct_answer <- "sapply"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq3_AdvancedRProgramming(rv$attempts_mcq3_AdvancedRProgramming() + 1)
      if (rv$attempts_mcq3_AdvancedRProgramming() >= 2) {
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
    question_id_3 <- "12.3"
    attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
    add_submission(student_id, question_id_3, selected_answer, is_correct, attempt_count_3)
    update_overall_score(student_id)
  })
  
  # Exercise 4: MCQ parallel processing
  rv$attempts_mcq4_AdvancedRProgramming <- reactiveVal(0)
  observeEvent(input$mcq4_AdvancedRProgramming, {
    selected_answer <- input$mcq4_AdvancedRProgramming
    correct_answer <- "parallel package"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq4_AdvancedRProgramming(rv$attempts_mcq4_AdvancedRProgramming() + 1)
      if (rv$attempts_mcq4_AdvancedRProgramming() >= 2) {
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
    question_id_4 <- "12.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_AdvancedRProgramming, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$mcq1_AdvancedRProgramming
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "12.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- (answer_1 == "my_function <- function(x, y = 2) { result <- x * y; return(result) }")
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq2_AdvancedRProgramming
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "12.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "while loop")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$mcq3_AdvancedRProgramming
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "12.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- (answer_3 == "sapply")
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq4_AdvancedRProgramming
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "12.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "parallel package")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    session$output$submission_status_AdvancedRProgramming <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})