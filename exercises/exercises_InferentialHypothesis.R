# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_InferentialHypothesis, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Inferential Statistics and Hypothesis Testing Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_ui("1", "Complete the function to perform a one-sample t-test in R.",
                      "hint1_InferentialStatistics", "solution1_InferentialStatistics", "code_editor1_InferentialStatistics", "run_code1_InferentialStatistics", "check_code1_InferentialStatistics", "result_area1_InferentialStatistics", 
                      "perform_t_test <- function(sample_data) {\n  # Perform a one-sample t-test\n  test_result <- ___(sample_data, mu = 75)\n  # Return the p-value\n  return(test_result$___)\n}"),
          generate_mcq_ui("2", "Choose the correct option: What does a p-value of less than 0.05 indicate?",
                          "mcq1_InferentialStatistics", list("Strong evidence against the null hypothesis" = "Strong evidence against the null hypothesis", "Weak evidence against the null hypothesis" = "Weak evidence against the null hypothesis", "No evidence against the null hypothesis" = "No evidence against the null hypothesis", "Evidence supporting the null hypothesis" = "Evidence supporting the null hypothesis"), 
                          "mcq_feedback1_InferentialStatistics"),
          generate_ui("3", "Complete the function to create a matrix and perform a chi-square test in R.",
                      "hint2_InferentialStatistics", "solution2_InferentialStatistics", "code_editor2_InferentialStatistics", "run_code2_InferentialStatistics", "check_code2_InferentialStatistics", "result_area2_InferentialStatistics",
                      "perform_chi_square_test <- function(data_matrix) {\n  # Perform a chi-square test\n  test_result <- ___(data_matrix)\n  # Return the test statistic\n  return(test_result$___)\n}"),
          generate_mcq_ui("4", "Choose the correct option: Which error occurs when the null hypothesis is incorrectly rejected?",
                          "mcq2_InferentialStatistics", list("Type I Error" = "Type I Error", "Type II Error" = "Type II Error", "Type III Error" = "Type III Error", "Type IV Error" = "Type IV Error"), 
                          "mcq_feedback2_InferentialStatistics")
        ),
        textOutput("submission_status_InferentialStatistics")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_InferentialStatistics = NULL,
    plot_output1_InferentialStatistics = NULL,
    code_output2_InferentialStatistics = NULL,
    plot_output2_InferentialStatistics = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Perform t-test
  run_code("run_code1_InferentialStatistics", "code_editor1_InferentialStatistics", "result_area1_InferentialStatistics", session)
  show_hint("Use the t.test() function to perform the t-test and extract the p-value.", "hint1_InferentialStatistics", session)
  rv$attempts1_InferentialStatistics <- reactiveVal(0)
  show_solution("Correct Code: perform_t_test <- function(sample_data) { test_result <- t.test(sample_data, mu = 75); return(test_result$p.value) }", rv$attempts1_InferentialStatistics, "solution1_InferentialStatistics", session)
  check_code("perform_t_test <- function(sample_data) { test_result <- t.test(sample_data, mu = 75); return(test_result$p.value) }", "check_code1_InferentialStatistics", "code_editor1_InferentialStatistics", "7.1", rv$attempts1_InferentialStatistics, rv, session)
  
  # Exercise 2: MCQ p-value
  rv$attempts_mcq1_InferentialStatistics <- reactiveVal(0)
  observeEvent(input$mcq1_InferentialStatistics, {
    selected_answer <- input$mcq1_InferentialStatistics
    correct_answer <- "Strong evidence against the null hypothesis"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_InferentialStatistics(rv$attempts_mcq1_InferentialStatistics() + 1)
      if (rv$attempts_mcq1_InferentialStatistics() >= 2) {
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
    question_id_2 <- "7.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: Perform chi-square test
  run_code("run_code2_InferentialStatistics", "code_editor2_InferentialStatistics", "result_area2_InferentialStatistics", session)
  show_hint("Use the chisq.test() function to perform the chi-square test and extract the test statistic.", "hint2_InferentialStatistics", session)
  rv$attempts2_InferentialStatistics <- reactiveVal(0)
  show_solution("Correct Code: perform_chi_square_test <- function(data_matrix) { test_result <- chisq.test(data_matrix); return(test_result$statistic) }", rv$attempts2_InferentialStatistics, "solution2_InferentialStatistics", session)
  check_code("perform_chi_square_test <- function(data_matrix) { test_result <- chisq.test(data_matrix); return(test_result$statistic) }", "check_code2_InferentialStatistics", "code_editor2_InferentialStatistics", "7.3", rv$attempts2_InferentialStatistics, rv, session)
  
  # Exercise 4: MCQ Type I Error
  rv$attempts_mcq2_InferentialStatistics <- reactiveVal(0)
  observeEvent(input$mcq2_InferentialStatistics, {
    selected_answer <- input$mcq2_InferentialStatistics
    correct_answer <- "Type I Error"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_InferentialStatistics(rv$attempts_mcq2_InferentialStatistics() + 1)
      if (rv$attempts_mcq2_InferentialStatistics() >= 2) {
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
    question_id_4 <- "7.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_InferentialStatistics, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_InferentialStatistics
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "7.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("perform_t_test <- function(sample_data) { test_result <- t.test(sample_data, mu = 75); return(test_result$p.value) }", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_InferentialStatistics
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "7.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "Strong evidence against the null hypothesis")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_InferentialStatistics
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "7.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("perform_chi_square_test <- function(data_matrix) { test_result <- chisq.test(data_matrix); return(test_result$statistic) }", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_InferentialStatistics
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "7.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "Type I Error")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    output$submission_status_InferentialStatistics <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})