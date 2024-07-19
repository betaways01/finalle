# Source the necessary files
source("database/db_operations.R")
source("modules/helpers.R")
source("modules/utils.R")

observeEvent(input$exercises_DataAnalysisTypes, {
  reset_exercise(rv, output, session)
  updateTabsetPanel(session, "main_tabset", selected = "Exercises")
  
  output$exercise_page <- renderUI({
    fluidPage(
      titlePanel("Working with Different Types of Data Exercises"),
      mainPanel(
        tabsetPanel(
          id = "exercise_tabset",
          generate_mcq_ui("1", "Identify the type of data: 'The color of a car (red, blue, green)'",
                          "mcq1_DataTypes", list("Qualitative - Categorical" = "Qualitative - Categorical", "Quantitative - Discrete" = "Quantitative - Discrete", "Quantitative - Continuous" = "Quantitative - Continuous", "None of the above" = "None of the above"), 
                          "mcq_feedback1_DataTypes"),
          generate_mcq_ui("2", "Choose the correct option: 'The number of cars in a parking lot'",
                          "mcq2_DataTypes", list("Qualitative - Categorical" = "Qualitative - Categorical", "Quantitative - Discrete" = "Quantitative - Discrete", "Quantitative - Continuous" = "Quantitative - Continuous", "None of the above" = "None of the above"), 
                          "mcq_feedback2_DataTypes"),
          generate_mcq_ui("3", "Identify the type of data: 'The height of a person'",
                          "mcq3_DataTypes", list("Qualitative - Categorical" = "Qualitative - Categorical", "Quantitative - Discrete" = "Quantitative - Discrete", "Quantitative - Continuous" = "Quantitative - Continuous", "None of the above" = "None of the above"), 
                          "mcq_feedback3_DataTypes"),
          generate_mcq_ui("4", "Choose the correct option: 'The temperature of a room'",
                          "mcq4_DataTypes", list("Qualitative - Categorical" = "Qualitative - Categorical", "Quantitative - Discrete" = "Quantitative - Discrete", "Quantitative - Continuous" = "Quantitative - Continuous", "None of the above" = "None of the above"), 
                          "mcq_feedback4_DataTypes")
        ),
        textOutput("submission_status_DataTypes")
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
  
  # MCQ 1: Identify the type of data
  rv$attempts_mcq1_DataTypes <- reactiveVal(0)
  observeEvent(input$mcq1_DataTypes, {
    selected_answer <- input$mcq1_DataTypes
    correct_answer <- "Qualitative - Categorical"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq1_DataTypes(rv$attempts_mcq1_DataTypes() + 1)
      if (rv$attempts_mcq1_DataTypes() >= 2) {
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
    question_id_1 <- "4.1"
    attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
    add_submission(student_id, question_id_1, selected_answer, is_correct, attempt_count_1)
    update_overall_score(student_id)
  })
  
  # MCQ 2: Identify the type of data
  rv$attempts_mcq2_DataTypes <- reactiveVal(0)
  observeEvent(input$mcq2_DataTypes, {
    selected_answer <- input$mcq2_DataTypes
    correct_answer <- "Quantitative - Discrete"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq2_DataTypes(rv$attempts_mcq2_DataTypes() + 1)
      if (rv$attempts_mcq2_DataTypes() >= 2) {
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
    question_id_2 <- "4.2"
    attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
    add_submission(student_id, question_id_2, selected_answer, is_correct, attempt_count_2)
    update_overall_score(student_id)
  })
  
  # MCQ 3: Identify the type of data
  rv$attempts_mcq3_DataTypes <- reactiveVal(0)
  observeEvent(input$mcq3_DataTypes, {
    selected_answer <- input$mcq3_DataTypes
    correct_answer <- "Quantitative - Continuous"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq3_DataTypes(rv$attempts_mcq3_DataTypes() + 1)
      if (rv$attempts_mcq3_DataTypes() >= 2) {
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
    question_id_3 <- "4.3"
    attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
    add_submission(student_id, question_id_3, selected_answer, is_correct, attempt_count_3)
    update_overall_score(student_id)
  })
  
  # MCQ 4: Identify the type of data
  rv$attempts_mcq4_DataTypes <- reactiveVal(0)
  observeEvent(input$mcq4_DataTypes, {
    selected_answer <- input$mcq4_DataTypes
    correct_answer <- "Quantitative - Continuous"
    is_correct <- (selected_answer == correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! You’re all done with this Exercise"),
        footer = NULL,
        easyClose = TRUE,
        fade = TRUE
      ))
    } else {
      rv$attempts_mcq4_DataTypes(rv$attempts_mcq4_DataTypes() + 1)
      if (rv$attempts_mcq4_DataTypes() >= 2) {
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
    question_id_4 <- "4.4"
    attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
    add_submission(student_id, question_id_4, selected_answer, is_correct, attempt_count_4)
    update_overall_score(student_id)
  })
  
  observeEvent(input$submit_exercise_DataTypes, {
    # Get student information
    student_id <- rv$student_id
    if (is.null(student_id) || length(student_id) == 0) {
      stop("Student ID is missing")
    }
    score <- 0
    
    # Question 1
    answer_1 <- session$input$mcq1_DataTypes
    if (!is.null(answer_1) && length(answer_1) > 0) {
      question_id_1 <- "4.1"
      attempt_count_1 <- get_attempt_count(student_id, question_id_1) + 1
      is_correct_1 <- (answer_1 == "Qualitative - Categorical")
      add_submission(student_id, question_id_1, answer_1, is_correct_1, attempt_count_1)
      if (is_correct_1) score <- score + 1
    }
    
    # Question 2
    answer_2 <- session$input$mcq2_DataTypes
    if (!is.null(answer_2) && length(answer_2) > 0) {
      question_id_2 <- "4.2"
      attempt_count_2 <- get_attempt_count(student_id, question_id_2) + 1
      is_correct_2 <- (answer_2 == "Quantitative - Discrete")
      add_submission(student_id, question_id_2, answer_2, is_correct_2, attempt_count_2)
      if (is_correct_2) score <- score + 1
    }
    
    # Question 3
    answer_3 <- session$input$mcq3_DataTypes
    if (!is.null(answer_3) && length(answer_3) > 0) {
      question_id_3 <- "4.3"
      attempt_count_3 <- get_attempt_count(student_id, question_id_3) + 1
      is_correct_3 <- (answer_3 == "Quantitative - Continuous")
      add_submission(student_id, question_id_3, answer_3, is_correct_3, attempt_count_3)
      if (is_correct_3) score <- score + 1
    }
    
    # Question 4
    answer_4 <- session$input$mcq4_DataTypes
    if (!is.null(answer_4) && length(answer_4) > 0) {
      question_id_4 <- "4.4"
      attempt_count_4 <- get_attempt_count(student_id, question_id_4) + 1
      is_correct_4 <- (answer_4 == "Quantitative - Continuous")
      add_submission(student_id, question_id_4, answer_4, is_correct_4, attempt_count_4)
      if (is_correct_4) score <- score + 1
    }
    
    update_overall_score(student_id)
    output$submission_status_DataTypes <- renderText(paste("Exercise submitted successfully! Your score is:", score, "/4"))
  })
})