# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_DescriptiveStatistics, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Descriptive Statistics Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_mcq_ui("1", "Choose the correct option: Which measure of central tendency is affected by outliers the most?",
                          "mcq1_DescriptiveStats", list("Mean" = "Mean", "Median" = "Median", "Mode" = "Mode"), 
                          "mcq_feedback1_DescriptiveStats"),
          generate_mcq_ui("2", "Choose the correct option: What does the standard deviation measure?",
                          "mcq2_DescriptiveStats", list("The spread of the data" = "The spread of the data", "The central value of the data" = "The central value of the data", "The most frequent value" = "The most frequent value"), 
                          "mcq_feedback2_DescriptiveStats"),
          generate_ui("3", "Complete the function to normalize a vector.",
                      "hint3_DescriptiveStats", "solution3_DescriptiveStats", "code_editor3_DescriptiveStats", "run_code3_DescriptiveStats", "check_code3_DescriptiveStats", "result_area3_DescriptiveStats",
                      "normalize <- function(x) {\n  # Normalize the vector x to a range of 0 to 1\n  normalized_x <- (x - min(x)) / (___(x) - ___(x))\n  return(normalized_x) \n  return(normalized_x)\n}\n"),
          generate_mcq_ui("4", "Choose the correct option: Which function in R calculates the variance of a data set?",
                          "mcq4_DescriptiveStats", list("var()" = "var()", "mean()" = "mean()", "sd()" = "sd()", "range()" = "range()"), 
                          "mcq_feedback4_DescriptiveStats")
        ),
        textOutput("submission_status_DescriptiveStats")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output3_DescriptiveStats = NULL,
    plot_output3_DescriptiveStats = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: MCQ central tendency
  rv$attempts_mcq1_DescriptiveStats <- reactiveVal(0)
  observeEvent(input$mcq1_DescriptiveStats, {
    selected_answer <- input$mcq1_DescriptiveStats
    correct_answer <- "Mean"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_DescriptiveStats(rv$attempts_mcq1_DescriptiveStats() + 1)
      if (rv$attempts_mcq1_DescriptiveStats() >= 2) {
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
    question_id_1 <- "5.1"
    attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
    add_submission(student_id, question_id_1, selected_answer, is_correct, attempt_count_1)
    update_overall_score(student_id)
  })
  
  # Exercise 2: MCQ standard deviation
  rv$attempts_mcq2_DescriptiveStats <- reactiveVal(0)
  observeEvent(input$mcq2_DescriptiveStats, {
    selected_answer <- input$mcq2_DescriptiveStats
    correct_answer <- "The spread of the data"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_DescriptiveStats(rv$attempts_mcq2_DescriptiveStats() + 1)
      if (rv$attempts_mcq2_DescriptiveStats() >= 2) {
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
    question_id_2 <- "5.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: Normalize a vector
  run_code("run_code3_DescriptiveStats", "code_editor3_DescriptiveStats", "result_area3_DescriptiveStats", session)
  show_hint("Use the formula to scale the values between 0 and 1.", "hint3_DescriptiveStats", session)
  rv$attempts3_DescriptiveStats <- reactiveVal(0)
  show_solution("normalize <- function(x) {\n  normalized_x <- (x - min(x)) / (max(x) - min(x))\n  return(normalized_x)\n}\n\n", rv$attempts3_DescriptiveStats, "solution3_DescriptiveStats", session)
  check_code("normalize <- function(x) {\n  normalized_x <- (x - min(x)) / (max(x) - min(x))\n  return(normalized_x)\n}\n\n", "check_code3_DescriptiveStats", "code_editor3_DescriptiveStats", "5.3", rv$attempts3_DescriptiveStats, rv, session)
  
  # Exercise 4: MCQ variance function
  rv$attempts_mcq4_DescriptiveStats <- reactiveVal(0)
  observeEvent(input$mcq4_DescriptiveStats, {
    selected_answer <- input$mcq4_DescriptiveStats
    correct_answer <- "var()"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq4_DescriptiveStats(rv$attempts_mcq4_DescriptiveStats() + 1)
      if (rv$attempts_mcq4_DescriptiveStats() >= 2) {
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
    question_id_4 <- "5.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_DescriptiveStats, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$mcq1_DescriptiveStats
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "5.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- (answer_1 == "Mean")
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq2_DescriptiveStats
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "5.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "The spread of the data")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor3_DescriptiveStats
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "5.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("normalize <- function(x) {\n  normalized_x <- (x - min(x)) / (max(x) - min(x))\n  return(normalized_x)\n}\n", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq4_DescriptiveStats
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "5.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "var()")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    output$submission_status_DescriptiveStats <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})