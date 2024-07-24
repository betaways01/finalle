# code_challenge_2.R
source("database/db_operations.R")
source("modules/cc_helpers.R")
source("modules/utils.R")

observeEvent(input$CodeChallenge2, {
  reset_challenge(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Code Challenge")
  
  output$challenge_page <- renderUI({
    fluidPage(
      titlePanel("R Statistical Analysis Exercises"),
      mainPanel(
        tabsetPanel(
          id = "challenge_tabset",
          generate_challenge_ui("1", "Calculate Standard Deviation: Write a function called calculate_sd that takes a numeric vector as input and returns the standard deviation of the elements.",
                                "code_editor1_CodeChallenge2", "run_code1_CodeChallenge2", "check_code1_CodeChallenge2", "result_area1_CodeChallenge2", 
                                "calculate_sd <- function(vec) {\n  return(sd(__))\n}"),
          generate_challenge_mcq_ui("2", "Measures of Central Tendency: Which of the following is NOT a measure of central tendency?",
                                    "mcq1_CodeChallenge2", list("Mean" = "Mean", "Median" = "Median", "Mode" = "Mode", "Range" = "Range"), 
                                    "mcq_feedback1_CodeChallenge2"),
          generate_challenge_ui("3", "Generate Normal Distribution: Write a function called generate_normal that takes three arguments: the number of samples (n), the mean, and the standard deviation, and returns a vector of samples from a normal distribution.",
                                "code_editor2_CodeChallenge2", "run_code2_CodeChallenge2", "check_code2_CodeChallenge2", "result_area2_CodeChallenge2",
                                "generate_normal <- function(n, mean, sd) {\n  return(rnorm(n, mean = __, sd = __))\n}"),
          generate_challenge_mcq_ui("4", "Measures of Variability: Standard deviation is calculated as the:",
                                    "mcq2_CodeChallenge2", list("Average of the data points" = "Average of the data points", "Square root of the variance" = "Square root of the variance", "Difference between the highest and lowest values" = "Difference between the highest and lowest values", "Most frequently occurring value" = "Most frequently occurring value"), 
                                    "mcq_feedback2_CodeChallenge2"),
          generate_challenge_ui("5", "Box Plot Helper: Create a function named iqr that takes a numeric vector as input and returns the interquartile range (IQR).",
                                "code_editor3_CodeChallenge2", "run_code3_CodeChallenge2", "check_code3_CodeChallenge2", "result_area3_CodeChallenge2",
                                "iqr <- function(vec) {\n  q1 <- quantile(vec, 0.25)\n  q3 <- quantile(vec, 0.75)\n  return(__ - __)\n}"),
          generate_challenge_mcq_ui("6", "Data Visualization: Histograms are best suited to visualize:",
                                    "mcq3_CodeChallenge2", list("Relationships between two variables" = "Relationships between two variables", "The distribution of a single variable" = "The distribution of a single variable", "Changes over time" = "Changes over time", "All of the above" = "All of the above"), 
                                    "mcq_feedback3_CodeChallenge2"),
          generate_challenge_ui("7", "Descriptive Statistics Summary: Write a function called describe that takes a numeric vector as input and returns a data frame containing the mean, median, and IQR of the elements.",
                                "code_editor4_CodeChallenge2", "run_code4_CodeChallenge2", "check_code4_CodeChallenge2", "result_area4_CodeChallenge2",
                                "describe <- function(vec) {\n  mean_value <- __(vec)\n  median_value <- __(vec)\n  iqr_value <- __(vec)\n  return(data.frame(Mean = mean_value, Median = median_value, IQR = iqr_value))\n}"),
          generate_challenge_mcq_ui("8", "Normalization and Standardization: What happens to the data values after normalization (scaling to 0-1)?",
                                    "mcq4_CodeChallenge2", list("They are centered around the mean." = "They are centered around the mean.", "Their variance remains unchanged." = "Their variance remains unchanged.", "They all fall between 0 and 1." = "They all fall between 0 and 1.", "Both a and c." = "Both a and c."), 
                                    "mcq_feedback4_CodeChallenge2"),
          generate_challenge_ui("9", "Simulate Coin Flips: Write a function called simulate_flips that takes two arguments: the number of flips (n_flips) and the probability of heads (p_heads).",
                                "code_editor5_CodeChallenge2", "run_code5_CodeChallenge2", "check_code5_CodeChallenge2", "result_area5_CodeChallenge2",
                                "simulate_flips <- function(n_flips) {\n  return(rbinom(n = n_flips, size = 1, prob = __))\n}"),
          generate_challenge_mcq_ui("10", "Box Plots: What information can be obtained from a box plot?",
                                    "mcq5_CodeChallenge2", list("The mean and standard deviation of the data" = "The mean and standard deviation of the data", "The quartiles (Q1, Q2, Q3) and potential outliers" = "The quartiles (Q1, Q2, Q3) and potential outliers", "The presence of a linear relationship" = "The presence of a linear relationship", "All of the above" = "All of the above"), 
                                    "mcq_feedback5_CodeChallenge2")
        ),
        textOutput("submission_status_CodeChallenge2")
      )
    )
  })
  
  message("Student name when CodeChallenge2 clicked: ", rv$student_name)
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_CodeChallenge2 = NULL,
    plot_output1_CodeChallenge2 = NULL,
    code_output2_CodeChallenge2 = NULL,
    plot_output2_CodeChallenge2 = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Standard Deviation function
  run_challenge_code("run_code1_CodeChallenge2", "code_editor1_CodeChallenge2", "result_area1_CodeChallenge2", session)
  rv$attempts1_CodeChallenge2 <- reactiveVal(0)
  check_challenge_code("calculate_sd <- function(vec) { return(sd(vec)) }", "check_code1_CodeChallenge2", "code_editor1_CodeChallenge2", "cc2.1", rv$attempts1_CodeChallenge2, rv, session)
  
  # Exercise 2: MCQ measures of central tendency
  rv$attempts_mcq1_CodeChallenge2 <- reactiveVal(0)
  check_challenge_mcq("Range", "mcq1_CodeChallenge2", "cc2.2", "mcq_feedback1_CodeChallenge2", rv$attempts_mcq1_CodeChallenge2, rv, session)
  
  # Exercise 3: Generate Normal Distribution
  run_challenge_code("run_code2_CodeChallenge2", "code_editor2_CodeChallenge2", "result_area2_CodeChallenge2", session)
  rv$attempts2_CodeChallenge2 <- reactiveVal(0)
  check_challenge_code("generate_normal <- function(n, mean, sd) { return(rnorm(n, mean = mean, sd = sd)) }", "check_code2_CodeChallenge2", "code_editor2_CodeChallenge2", "cc2.3", rv$attempts2_CodeChallenge2, rv, session)
  
  # Exercise 4: MCQ measures of variability
  rv$attempts_mcq2_CodeChallenge2 <- reactiveVal(0)
  check_challenge_mcq("Square root of the variance", "mcq2_CodeChallenge2", "cc2.4", "mcq_feedback2_CodeChallenge2", rv$attempts_mcq2_CodeChallenge2, rv, session)
  
  # Exercise 5: Box Plot Helper (IQR)
  run_challenge_code("run_code3_CodeChallenge2", "code_editor3_CodeChallenge2", "result_area3_CodeChallenge2", session)
  rv$attempts3_CodeChallenge2 <- reactiveVal(0)
  check_challenge_code("iqr <- function(vec) { q1 <- quantile(vec, 0.25); q3 <- quantile(vec, 0.75); return(q3 - q1) }", "check_code3_CodeChallenge2", "code_editor3_CodeChallenge2", "cc2.5", rv$attempts3_CodeChallenge2, rv, session)
  
  # Exercise 6: MCQ data visualization
  rv$attempts_mcq3_CodeChallenge2 <- reactiveVal(0)
  check_challenge_mcq("The distribution of a single variable", "mcq3_CodeChallenge2", "cc2.6", "mcq_feedback3_CodeChallenge2", rv$attempts_mcq3_CodeChallenge2, rv, session)
  
  # Exercise 7: Descriptive Statistics Summary
  run_challenge_code("run_code4_CodeChallenge2", "code_editor4_CodeChallenge2", "result_area4_CodeChallenge2", session)
  rv$attempts4_CodeChallenge2 <- reactiveVal(0)
  check_challenge_code("describe <- function(vec) { mean_value <- mean(vec); median_value <- median(vec); iqr_value <- iqr(vec); return(data.frame(Mean = mean_value, Median = median_value, IQR = iqr_value)) }", "check_code4_CodeChallenge2", "code_editor4_CodeChallenge2", "cc2.7", rv$attempts4_CodeChallenge2, rv, session)
  
  # Exercise 8: MCQ normalization and standardization
  rv$attempts_mcq4_CodeChallenge2 <- reactiveVal(0)
  check_challenge_mcq("They all fall between 0 and 1.", "mcq4_CodeChallenge2", "cc2.8", "mcq_feedback4_CodeChallenge2", rv$attempts_mcq4_CodeChallenge2, rv, session)
  
  # Exercise 9: Simulate Coin Flips
  run_challenge_code("run_code5_CodeChallenge2", "code_editor5_CodeChallenge2", "result_area5_CodeChallenge2", session)
  rv$attempts5_CodeChallenge2 <- reactiveVal(0)
  check_challenge_code("simulate_flips <- function(n_flips) { return(rbinom(n = n_flips, size = 1, prob = 0.5)) }", "check_code5_CodeChallenge2", "code_editor5_CodeChallenge2", "cc2.9", rv$attempts5_CodeChallenge2, rv, session)
  
  # Exercise 10: MCQ box plots
  rv$attempts_mcq5_CodeChallenge2 <- reactiveVal(0)
  check_challenge_mcq("The quartiles (Q1, Q2, Q3) and potential outliers", "mcq5_CodeChallenge2", "cc2.10", "mcq_feedback5_CodeChallenge2", rv$attempts_mcq5_CodeChallenge2, rv, session)
  
  observeEvent(input$submit_exercise_CodeChallenge2, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_CodeChallenge2
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "cc2.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("calculate_sd <- function(vec) { return(sd(vec)) }", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_CodeChallenge2
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "cc2.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "Range")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_CodeChallenge2
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "cc2.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("generate_normal <- function(n, mean, sd) { return(rnorm(n, mean = mean, sd = sd)) }", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_CodeChallenge2
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "cc2.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "Square root of the variance")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    # Question 5
    answer_5 <- session$input$code_editor3_CodeChallenge2
    if (!is.null(answer_5) && length(answer_5) > 0) {
      question_id_5 <- "cc2.5"
      attempt_count_5 <- get_attempt_count(student_id, question_id_5) + 1
      is_correct_5 <- gradethis::grade_code("iqr <- function(vec) { q1 <- quantile(vec, 0.25); q3 <- quantile(vec, 0.75); return(q3 - q1) }", answer_5)$correct
      add_submission(student_id, question_id_5, answer_5, is_correct_5, attempt_count_5)
      if (is_correct_5) score <- score + 1
    }
    
    # Question 6
    answer_6 <- session$input$mcq3_CodeChallenge2
    if (!is.null(answer_6) && length(answer_6) > 0) {
      question_id_6 <- "cc2.6"
      attempt_count_6 <- get_attempt_count(student_id, question_id_6) + 1
      is_correct_6 <- (answer_6 == "The distribution of a single variable")
      add_submission(student_id, question_id_6, answer_6, is_correct_6, attempt_count_6)
      if (is_correct_6) score <- score + 1
    }
    
    # Question 7
    answer_7 <- session$input$code_editor4_CodeChallenge2
    if (!is.null(answer_7) && length(answer_7) > 0) {
      question_id_7 <- "cc2.7"
      attempt_count_7 <- get_attempt_count(student_id, question_id_7) + 1
      is_correct_7 <- gradethis::grade_code("describe <- function(vec) { mean_value <- mean(vec); median_value <- median(vec); iqr_value <- iqr(vec); return(data.frame(Mean = mean_value, Median = median_value, IQR = iqr_value)) }", answer_7)$correct
      add_submission(student_id, question_id_7, answer_7, is_correct_7, attempt_count_7)
      if (is_correct_7) score <- score + 1
    }
    
    # Question 8
    answer_8 <- session$input$mcq4_CodeChallenge2
    if (!is.null(answer_8) && length(answer_8) > 0) {
      question_id_8 <- "cc2.8"
      attempt_count_8 <- get_attempt_count(student_id, question_id_8) + 1
      is_correct_8 <- (answer_8 == "They all fall between 0 and 1.")
      add_submission(student_id, question_id_8, answer_8, is_correct_8, attempt_count_8)
      if (is_correct_8) score <- score + 1
    }
    
    # Question 9
    answer_9 <- session$input$code_editor5_CodeChallenge2
    if (!is.null(answer_9) && length(answer_9) > 0) {
      question_id_9 <- "cc2.9"
      attempt_count_9 <- get_attempt_count(student_id, question_id_9) + 1
      is_correct_9 <- gradethis::grade_code("simulate_flips <- function(n_flips) { return(rbinom(n = n_flips, size = 1, prob = 0.5)) }", answer_9)$correct
      add_submission(student_id, question_id_9, answer_9, is_correct_9, attempt_count_9)
      if (is_correct_9) score <- score + 1
    }
    
    # Question 10
    answer_10 <- session$input$mcq5_CodeChallenge2
    if (!is.null(answer_10) && length(answer_10) > 0) {
      question_id_10 <- "cc2.10"
      attempt_count_10 <- get_attempt_count(student_id, question_id_10) + 1
      is_correct_10 <- (answer_10 == "The quartiles (Q1, Q2, Q3) and potential outliers")
      add_submission(student_id, question_id_10, answer_10, is_correct_10, attempt_count_10)
      if (is_correct_10) score <- score + 1
    }
    
    update_overall_score(student_id)
    session$output$submission_status_CodeChallenge2 <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/10"))
  })
})
      