# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_ProbabilityStatistics, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Probability Distributions and Statistical Concepts Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_ui("1", "Compute Probability Using Binomial Distribution",
                      "hint1_ProbabilityDistributions", "solution1_ProbabilityDistributions", "code_editor1_ProbabilityDistributions", "run_code1_ProbabilityDistributions", "check_code1_ProbabilityDistributions", "result_area1_ProbabilityDistributions", 
                      "compute_probability <- function() {\n  # Complete the function to compute the probability\n  # of getting exactly 4 heads in 8 coin tosses with a probability of 0.5\n  prob <- dbinom(___, size=___, prob=___)\n  return(prob)\n}\n"),
          generate_mcq_ui("2", "Choose the correct option: Which distribution is used to model the number of events in a fixed interval of time?",
                          "mcq1_ProbabilityDistributions", list("Binomial Distribution" = "Binomial Distribution", "Poisson Distribution" = "Poisson Distribution", "Normal Distribution" = "Normal Distribution", "Exponential Distribution" = "Exponential Distribution"), 
                          "mcq_feedback1_ProbabilityDistributions"),
          generate_ui("3", "Simulate the Central Limit Theorem",
                      "hint2_ProbabilityDistributions", "solution2_ProbabilityDistributions", "code_editor2_ProbabilityDistributions", "run_code2_ProbabilityDistributions", "check_code2_ProbabilityDistributions", "result_area2_ProbabilityDistributions",
                      "simulate_clt <- function() {\n  set.seed(123)\n  # Simulate sample means\n  sample_means <- replicate(1000, mean(runif(50, min=___, max=___)))\n  return(sample_means)\n}\n"),
          generate_mcq_ui("4", "Choose the correct option: What does the Central Limit Theorem state?",
                          "mcq2_ProbabilityDistributions", list("The sample mean approximates a normal distribution as the sample size becomes large" = "The sample mean approximates a normal distribution as the sample size becomes large", "The sample mean approximates a binomial distribution as the sample size becomes large" = "The sample mean approximates a binomial distribution as the sample size becomes large", "The sample mean approximates a Poisson distribution as the sample size becomes large" = "The sample mean approximates a Poisson distribution as the sample size becomes large", "The sample mean approximates an exponential distribution as the sample size becomes large" = "The sample mean approximates an exponential distribution as the sample size becomes large"), 
                          "mcq_feedback2_ProbabilityDistributions")
        ),
        textOutput("submission_status_ProbabilityDistributions")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_ProbabilityDistributions = NULL,
    plot_output1_ProbabilityDistributions = NULL,
    code_output2_ProbabilityDistributions = NULL,
    plot_output2_ProbabilityDistributions = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Compute Probability Using Binomial Distribution
  run_code("run_code1_ProbabilityDistributions", "code_editor1_ProbabilityDistributions", "result_area1_ProbabilityDistributions", session)
  show_hint("Use dbinom(x, size=n, prob=p) to compute the probability of x successes in n trials.", "hint1_ProbabilityDistributions", session)
  rv$attempts1_ProbabilityDistributions <- reactiveVal(0)
  show_solution("compute_probability <- function() {\n  prob <- dbinom(4, size=8, prob=0.5)\n  return(prob)\n}\n", rv$attempts1_ProbabilityDistributions, "solution1_ProbabilityDistributions", session)
  check_code("compute_probability <- function() {\n  prob <- dbinom(4, size=8, prob=0.5)\n  return(prob)\n}\n", "check_code1_ProbabilityDistributions", "code_editor1_ProbabilityDistributions", "6.1", rv$attempts1_ProbabilityDistributions, rv, session)
  
  # Exercise 2: MCQ on distribution modeling
  rv$attempts_mcq1_ProbabilityDistributions <- reactiveVal(0)
  observeEvent(input$mcq1_ProbabilityDistributions, {
    selected_answer <- input$mcq1_ProbabilityDistributions
    correct_answer <- "Poisson Distribution"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_ProbabilityDistributions(rv$attempts_mcq1_ProbabilityDistributions() + 1)
      if (rv$attempts_mcq1_ProbabilityDistributions() >= 2) {
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
    question_id_2 <- "6.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: Simulate the Central Limit Theorem
  run_code("run_code2_ProbabilityDistributions", "code_editor2_ProbabilityDistributions", "result_area2_ProbabilityDistributions", session)
  show_hint("Use replicate(n, mean(runif(m, min, max))) to simulate sample means.", "hint2_ProbabilityDistributions", session)
  rv$attempts2_ProbabilityDistributions <- reactiveVal(0)
  show_solution("simulate_clt <- function() {\n  set.seed(123)\n  sample_means <- replicate(1000, mean(runif(50, min=0, max=1)))\n  return(sample_means)\n}\n", rv$attempts2_ProbabilityDistributions, "solution2_ProbabilityDistributions", session)
  check_code("simulate_clt <- function() {\n  set.seed(123)\n  sample_means <- replicate(1000, mean(runif(50, min=0, max=1)))\n  return(sample_means)\n}\n", "check_code2_ProbabilityDistributions", "code_editor2_ProbabilityDistributions", "6.3", rv$attempts2_ProbabilityDistributions, rv, session)
  
  # Exercise 4: MCQ on Central Limit Theorem
  rv$attempts_mcq2_ProbabilityDistributions <- reactiveVal(0)
  observeEvent(input$mcq2_ProbabilityDistributions, {
    selected_answer <- input$mcq2_ProbabilityDistributions
    correct_answer <- "The sample mean approximates a normal distribution as the sample size becomes large"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_ProbabilityDistributions(rv$attempts_mcq2_ProbabilityDistributions() + 1)
      if (rv$attempts_mcq2_ProbabilityDistributions() >= 2) {
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
    question_id_4 <- "6.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_ProbabilityDistributions, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_ProbabilityDistributions
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "6.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("compute_probability <- function() {\n  prob <- dbinom(4, size=8, prob=0.5)\n  return(prob)\n}\n", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_ProbabilityDistributions
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "6.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "Poisson Distribution")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_ProbabilityDistributions
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "6.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("simulate_clt <- function() {\n  set.seed(123)\n  sample_means <- replicate(1000, mean(runif(50, min=0, max=1)))\n  return(sample_means)\n}\n", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_ProbabilityDistributions
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "6.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "The sample mean approximates a normal distribution as the sample size becomes large")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    output$submission_status_ProbabilityDistributions <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})