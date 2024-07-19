# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_DataVisualization, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Data Visualization Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_ui("1", "Complete the function to create a bar plot of cylinder counts.",
                      "hint1", "solution1", "code_editor1", "run_code1", "check_code1", "result_area1", 
                      "create_bar_plot <- function() {\n  library(ggplot2)\n  mtcars$cyl <- as.factor(mtcars$cyl)\n  ggplot(mtcars, aes(x = cyl)) + \n    # Add bar plot layer\n ___    \n}"),
          generate_mcq_ui("2", "Choose the correct option: Which function is used to create a histogram in ggplot2?",
                          "mcq1", list("geom_histogram()" = "geom_histogram()", "geom_bar()" = "geom_bar()", "geom_point()" = "geom_point()", "geom_line()" = "geom_line()"), 
                          "mcq_feedback1"),
          generate_ui("3", "Complete the function to create a scatter plot of mpg vs wt.",
                      "hint2", "solution2", "code_editor2", "run_code2", "check_code2", "result_area2",
                      "create_scatter_plot <- function() {\n  library(ggplot2)\n  ggplot(mtcars, aes(x = wt, y = mpg)) + \n    # Add scatter plot layer\n___   \n}"),
          generate_mcq_ui("4", "Choose the correct option: Which function is used to add a smooth line in a scatter plot in ggplot2?",
                          "mcq2", list("geom_smooth()" = "geom_smooth()", "geom_line()" = "geom_line()", "geom_path()" = "geom_path()", "geom_area()" = "geom_area()"), 
                          "mcq_feedback2")
        ),
        textOutput("submission_status")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    code_output1 = NULL,
    plot_output1 = NULL,
    code_output2 = NULL,
    plot_output2 = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # Exercise 1: Bar plot
  run_code("run_code1", "code_editor1", "result_area1", session)
  show_hint("Use the geom_bar() function to create the bar plot.", "hint1", session)
  rv$attempts1 <- reactiveVal(0)
  show_solution("Correct Code: create_bar_plot <- function() {\n  library(ggplot2)\n  mtcars$cyl <- as.factor(mtcars$cyl)\n  ggplot(mtcars, aes(x = cyl)) + \n    geom_bar()\n}", rv$attempts1, "solution1", session)
  check_code("create_bar_plot <- function() {\n  library(ggplot2)\n  mtcars$cyl <- as.factor(mtcars$cyl)\n  ggplot(mtcars, aes(x = cyl)) + \n    geom_bar()\n}", "check_code1", "code_editor1", "3.1", rv$attempts1, rv, session)
  
  # Exercise 2: MCQ histogram function
  rv$attempts_mcq1 <- reactiveVal(0)
  observeEvent(input$mcq1, {
    selected_answer <- input$mcq1
    correct_answer <- "geom_histogram()"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1(rv$attempts_mcq1() + 1)
      if (rv$attempts_mcq1() >= 2) {
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
    question_id_2 <- "3.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # Exercise 3: Scatter plot
  run_code("run_code2", "code_editor2", "result_area2", session)
  show_hint("Use the geom_point() function to create the scatter plot.", "hint2", session)
  rv$attempts2 <- reactiveVal(0)
  show_solution("Correct Code: create_scatter_plot <- function() {\n  library(ggplot2)\n  ggplot(mtcars, aes(x = wt, y = mpg)) + \n    geom_point()\n}", rv$attempts2, "solution2", session)
  check_code("create_scatter_plot <- function() {\n  library(ggplot2)\n  ggplot(mtcars, aes(x = wt, y = mpg)) + \n    geom_point()\n}", "check_code2", "code_editor2", "3.3", rv$attempts2, rv, session)
  
  # Exercise 4: MCQ smooth line function
  rv$attempts_mcq2 <- reactiveVal(0)
  observeEvent(input$mcq2, {
    selected_answer <- input$mcq2
    correct_answer <- "geom_smooth()"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2(rv$attempts_mcq2() + 1)
      if (rv$attempts_mcq2() >= 2) {
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
    question_id_4 <- "3.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$code_editor1
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "3.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- gradethis::grade_code("create_bar_plot <- function() {\n  library(ggplot2)\n  mtcars$cyl <- as.factor(mtcars$cyl)\n  ggplot(mtcars, aes(x = cyl)) + \n    geom_bar()\n}", answer_1)$correct
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq1
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "3.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "geom_histogram()")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$code_editor2
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "3.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- gradethis::grade_code("create_scatter_plot <- function() {\n  library(ggplot2)\n  ggplot(mtcars, aes(x = wt, y = mpg)) + \n    geom_point()\n}", answer_3)$correct
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq2
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "3.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "geom_smooth()")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    session$output$submission_status <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})