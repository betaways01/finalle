# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_DataWrangling, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Advanced Data Manipulation and Transformation Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_mcq_ui("1", "Choose the correct function: Which function in dplyr is used to select specific columns?",
                          "mcq1_DataManipulation", list("select()" = "select()", "filter()" = "filter()", "mutate()" = "mutate()", "summarise()" = "summarise()"), 
                          "mcq_feedback1_DataManipulation"),
          generate_mcq_ui("2", "Choose the correct function: Which function in dplyr is used to filter rows based on conditions?",
                          "mcq2_DataManipulation", list("select()" = "select()", "filter()" = "filter()", "mutate()" = "mutate()", "group_by()" = "group_by()"), 
                          "mcq_feedback2_DataManipulation"),
          generate_mcq_ui("3", "Choose the correct function: Which function from the stringr package is used to convert text to upper case?",
                          "mcq3_DataManipulation", list("str_to_upper()" = "str_to_upper()", "str_length()" = "str_length()", "str_to_lower()" = "str_to_lower()", "str_sub()" = "str_sub()"), 
                          "mcq_feedback3_DataManipulation"),
          generate_mcq_ui("4", "Choose the correct function: Which function from the lubridate package is used to extract the year from a date?",
                          "mcq4_DataManipulation", list("year()" = "year()", "month()" = "month()", "day()" = "day()", "ymd()" = "ymd()"), 
                          "mcq_feedback4_DataManipulation")
        ),
        textOutput("submission_status_DataManipulation")
      )
    )
  })
  
  # Reactive values to store outputs
  rv <- reactiveValues(
    
    student_name = NULL
  )
  
  observeEvent(input$submit_name, {
    req(input$student_name)
    rv$student_name <- input$student_name
    rv$student_id <- get_or_create_student_id(input$student_name)
    removeModal()
  })
  
  # MCQ 1: dplyr select function
  rv$attempts_mcq1_DataManipulation <- reactiveVal(0)
  observeEvent(input$mcq1_DataManipulation, {
    selected_answer <- input$mcq1_DataManipulation
    correct_answer <- "select()"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_DataManipulation(rv$attempts_mcq1_DataManipulation() + 1)
      if (rv$attempts_mcq1_DataManipulation() >= 2) {
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
    question_id_1 <- "11.1"
    attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
    add_submission(student_id, question_id_1, selected_answer, is_correct, attempt_count_1)
    update_overall_score(student_id)
  })
  
  # MCQ 2: dplyr filter function
  rv$attempts_mcq2_DataManipulation <- reactiveVal(0)
  observeEvent(input$mcq2_DataManipulation, {
    selected_answer <- input$mcq2_DataManipulation
    correct_answer <- "filter()"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_DataManipulation(rv$attempts_mcq2_DataManipulation() + 1)
      if (rv$attempts_mcq2_DataManipulation() >= 2) {
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
    question_id_2 <- "11.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # MCQ 3: stringr function for upper case
  rv$attempts_mcq3_DataManipulation <- reactiveVal(0)
  observeEvent(input$mcq3_DataManipulation, {
    selected_answer <- input$mcq3_DataManipulation
    correct_answer <- "str_to_upper()"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq3_DataManipulation(rv$attempts_mcq3_DataManipulation() + 1)
      if (rv$attempts_mcq3_DataManipulation() >= 2) {
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
    question_id_3 <- "11.3"
    attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
    add_submission(student_id, question_id_3, selected_answer, is_correct, attempt_count_3)
    update_overall_score(student_id)
  })
  
  # MCQ 4: lubridate function for extracting year
  rv$attempts_mcq4_DataManipulation <- reactiveVal(0)
  observeEvent(input$mcq4_DataManipulation, {
    selected_answer <- input$mcq4_DataManipulation
    correct_answer <- "year()"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq4_DataManipulation(rv$attempts_mcq4_DataManipulation() + 1)
      if (rv$attempts_mcq4_DataManipulation() >= 2) {
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
    question_id_4 <- "11.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_DataManipulation, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$mcq1_DataManipulation
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "11.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- (answer_1 == "select()")
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq2_DataManipulation
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "11.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "filter()")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$mcq3_DataManipulation
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "11.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- (answer_3 == "str_to_upper()")
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq4_DataManipulation
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "11.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "year()")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    session$output$submission_status_DataManipulation <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})