# modules/progress.R
library(dplyr)
library(DBI)
library(RSQLite)

progressUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Your Progress"),
    tableOutput(ns("progress_table")),
    actionButton(ns("refresh_progress"), "Refresh Progress"),
    actionButton(ns("view_code_challenge_results"), "View Code Challenge Results", class = "btn-success")
  )
}
progressServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    updateProgressTable <- function() {
      #message("Updating progress table...")  # Debug statement
      
      # Connect to the database
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      # Query to get the progress data
      progress_data <- dbGetQuery(con, "
        SELECT 
          sub.question_id,
          sub.is_correct
        FROM 
          Submissions sub
        WHERE 
          sub.student_id = ?
      ", params = list(rv$student_id))
      #message("Progress data: ", toString(progress_data))  # Debug statement
      
      # Query to get the exercise scores
      exercise_scores <- dbGetQuery(con, "
        SELECT 
          exercise_id, 
          score 
        FROM 
          ExerciseScores 
        WHERE 
          student_id = ?
      ", params = list(rv$student_id))
      #message("Exercise scores: ", toString(exercise_scores))  # Debug statement
      
      # Query to get the total score
      total_score <- dbGetQuery(con, "
        SELECT 
          total_score 
        FROM 
          Students 
        WHERE 
          student_id = ?
      ", params = list(rv$student_id))
      #message("Total score: ", toString(total_score))  # Debug statement
      
      # Initialize progress data
      progress_data_wide <- data.frame(
        Exercise = 1:12,
        Q1 = rep(0, 12),
        Q2 = rep(0, 12),
        Q3 = rep(0, 12),
        Q4 = rep(0, 12),
        Total = rep(0, 12)
      )
      
      if (!is.null(progress_data) && nrow(progress_data) > 0) {
        # Map questions to Q1, Q2, Q3, Q4 based on question_id
        progress_data$exercise_id <- as.integer(sapply(strsplit(as.character(progress_data$question_id), "\\."), `[`, 1))
        progress_data$question_num <- as.integer(sapply(strsplit(as.character(progress_data$question_id), "\\."), `[`, 2))
        
        for (i in 1:nrow(progress_data)) {
          exercise_id <- progress_data$exercise_id[i]
          question_num <- progress_data$question_num[i]
          is_correct <- progress_data$is_correct[i]
          
          if (question_num == 1) {
            progress_data_wide$Q1[exercise_id] <- is_correct
          } else if (question_num == 2) {
            progress_data_wide$Q2[exercise_id] <- is_correct
          } else if (question_num == 3) {
            progress_data_wide$Q3[exercise_id] <- is_correct
          } else if (question_num == 4) {
            progress_data_wide$Q4[exercise_id] <- is_correct
          }
        }
        
        # Calculate totals for each exercise
        progress_data_wide$Total <- rowSums(progress_data_wide[, c("Q1", "Q2", "Q3", "Q4")])
      }
      
      # Merge exercise scores with progress data
      if (!is.null(exercise_scores) && nrow(exercise_scores) > 0) {
        for (i in 1:nrow(exercise_scores)) {
          exercise_id <- exercise_scores$exercise_id[i]
          score <- exercise_scores$score[i]
          progress_data_wide$Total[exercise_id] <- score
        }
      }
      
      # Add overall total score
      total_row <- data.frame(
        Exercise = NA,
        Q1 = NA,
        Q2 = NA,
        Q3 = NA,
        Q4 = NA,
        Total = ifelse(!is.null(total_score) && nrow(total_score) > 0, total_score$total_score[1], 0)
      )
      
      progress_data_wide <- rbind(progress_data_wide, total_row)
      rownames(progress_data_wide)[13] <- "Total"
      
      # Ensure scores are discrete integers
      progress_data_wide <- progress_data_wide %>%
        mutate(across(Q1:Total, as.integer))
      
      rv$progress <- progress_data_wide
      #message("Updated progress data: ", toString(progress_data_wide))  # Debug statement
    }
    
    observeEvent(rv$student_name, {
      if (!is.null(rv$student_name) && rv$student_name != "") {
        # Retrieve or create student ID from the database
        rv$student_id <- get_or_create_student_id(rv$student_name)
        updateProgressTable()
      }
    })
    
    observeEvent(input$refresh_progress, {
      message("Refresh button clicked...")  # Debug statement
      if (!is.null(rv$student_id)) {
        updateProgressTable()
        output$progress_table <- renderTable({
          rv$progress
        }, striped = TRUE, hover = TRUE, bordered = TRUE)
      }
    })
    
    observeEvent(input$upload_code_challenge, {
      req(input$upload_code_challenge)
      
      # Determine which code challenge is being uploaded
      code_challenge <- input$code_challenge_select
      student_id <- rv$student_id
      
      # Process the uploaded file
      file_path <- input$upload_code_challenge$datapath
      code_content <- readLines(file_path)
      
      # Store the code in the database
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      dbExecute(con, "
        INSERT OR REPLACE INTO CodeChallenges (student_id, code_challenge, code_content, upload_time) 
        VALUES (?, ?, ?, datetime('now'))
      ", params = list(student_id, code_challenge, paste(code_content, collapse = "\n")))
      
      # Auto-grade the submission (this is a placeholder for the actual grading logic)
      score <- auto_grade_code(code_content)
      
      # Store the grade in the database
      dbExecute(con, "
        INSERT OR REPLACE INTO CodeChallengeScores (student_id, code_challenge, score, submission_time) 
        VALUES (?, ?, ?, datetime('now'))
      ", params = list(student_id, code_challenge, score))
      
      showModal(modalDialog(
        title = "Code Challenge Result",
        paste("Your code challenge has been graded. You scored", score, "/ 10."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    observeEvent(input$view_code_challenge_scores, {
      student_id <- rv$student_id
      
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      scores <- dbGetQuery(con, "
        SELECT code_challenge, score, submission_time 
        FROM CodeChallengeScores 
        WHERE student_id = ?
      ", params = list(student_id))
      
      showModal(modalDialog(
        title = "Code Challenge Scores",
        renderTable(scores),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    output$progress_table <- renderTable({
      progress_data <- rv$progress
      if (is.null(progress_data)) return(NULL)
      
      # Render the table
      progress_data
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}

# Placeholder function for auto-grading code
auto_grade_code <- function(code_content) {
  # Implement your auto-grading logic here
  # For now, it returns a random score out of 10
  sample(0:10, 1)
}