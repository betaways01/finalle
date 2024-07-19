# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_ANOVA, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("ANOVA and Post Hoc Testing Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_ui("1", "Complete the function to perform a one-way ANOVA in R.",
                      "hint1_ANOVA", "solution1_ANOVA", "code_editor1_ANOVA", "run_code1_ANOVA", "check_code1_ANOVA", "result_area1_ANOVA", 
                      "perform_anova <- function(data) {\n  # Perform a one-way ANOVA\n  anova_result <- ___(value ~ group, data = data)\n  # Return the summary of the ANOVA\n  return(summary(___))\n}"),
          generate_mcq_ui("2", "Choose the correct option: Which assumption is NOT required for ANOVA?",
                          "mcq1_ANOVA", list("Normality" = "Normality", "Homogeneity of variances" = "Homogeneity of variances", "Independence" = "Independence", "Equal sample sizes" = "Equal sample sizes"), 
                          "mcq_feedback1_ANOVA"),
          generate_ui("3", "Complete the function to perform a two-way ANOVA in R.",
                      "hint2_ANOVA", "solution2_ANOVA", "code_editor2_ANOVA", "run_code2_ANOVA", "check_code2_ANOVA", "result_area2_ANOVA",
                      "perform_two_way_anova <- function(data) {\n  # Perform a two-way ANOVA\n  anova_result <- ___(value ~ factor1 * factor2, data = data)\n  # Return the summary of the ANOVA\n  return(summary(___))\n}"),
          generate_mcq_ui("4", "Choose the correct option: Which post hoc test is commonly used after ANOVA?",
                          "mcq2_ANOVA", list("Tukey's HSD" = "Tukey's HSD", "Bonferroni" = "Bonferroni", "Scheffé" = "Scheffé", "All of the above" = "All of the above"), 
                          "mcq_feedback2_ANOVA")
        ),
        textOutput("submission_status_ANOVA")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1_ANOVA = NULL,
    plot_output1_ANOVA = NULL,
    code_output2_ANOVA = NULL,
    plot_output2_ANOVA = NULL,
    code_output3_ANOVA = NULL,
    plot_output3_ANOVA = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Perform one-way ANOVA
  run_code("run_code1_ANOVA", "code_editor1_ANOVA", "result_area1_ANOVA", session)
  show_hint("Use the aov() function to perform the one-way ANOVA and extract the summary.", "hint1_ANOVA", session)
  rv$attempts1_ANOVA <- reactiveVal(0)
  show_solution("Correct Code: perform_anova <- function(data) { anova_result <- aov(value ~ group, data = data); return(summary(anova_result)) }", rv$attempts1_ANOVA, "solution1_ANOVA", session)
  check_code("perform_anova <- function(data) { anova_result <- aov(value ~ group, data = data); return(summary(anova_result)) }", "check_code1_ANOVA", "code_editor1_ANOVA", "8.1", rv$attempts1_ANOVA, rv, session)
  
  # Exercise 2: MCQ ANOVA assumptions
  rv$attempts_mcq1_ANOVA <- reactiveVal(0)
  observeEvent(input$mcq1_ANOVA, {
    selected_answer <- input$mcq1_ANOVA
    correct_answer <- "Equal sample sizes"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_ANOVA(rv$attempts_mcq1_ANOVA() + 1)
      if (rv$attempts_mcq1_ANOVA() >= 2) {
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
    question_id_2 <- "8.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: Perform two-way ANOVA
  run_code("run_code2_ANOVA", "code_editor2_ANOVA", "result_area2_ANOVA", session)
  show_hint("Use the aov() function to perform the two-way ANOVA and extract the summary.", "hint2_ANOVA", session)
  rv$attempts2_ANOVA <- reactiveVal(0)
  show_solution("Correct Code: perform_two_way_anova <- function(data) { anova_result <- aov(value ~ factor1 * factor2, data = data); return(summary(anova_result)) }", rv$attempts2_ANOVA, "solution2_ANOVA", session)
  check_code("perform_two_way_anova <- function(data) { anova_result <- aov(value ~ factor1 * factor2, data = data); return(summary(anova_result)) }", "check_code2_ANOVA", "code_editor2_ANOVA", "8.3", rv$attempts2_ANOVA, rv, session)
  
  # Exercise 4: MCQ post hoc tests
  rv$attempts_mcq2_ANOVA <- reactiveVal(0)
  observeEvent(input$mcq2_ANOVA, {
    selected_answer <- input$mcq2_ANOVA
    correct_answer <- "All of the above"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_ANOVA(rv$attempts_mcq2_ANOVA() + 1)
      if (rv$attempts_mcq2_ANOVA() >= 2) {
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
    question_id_4 <- "8.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_ANOVA, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1_ANOVA
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "8.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("perform_anova <- function(data) { anova_result <- aov(value ~ group, data = data); return(summary(anova_result)) }", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1_ANOVA
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "8.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "Equal sample sizes")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2_ANOVA
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "8.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("perform_two_way_anova <- function(data) { anova_result <- aov(value ~ factor1 * factor2, data = data); return(summary(anova_result)) }", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2_ANOVA
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "8.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "All of the above")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    session$output$submission_status_ANOVA <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})