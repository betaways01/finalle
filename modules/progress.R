# modules/progress.R
library(dplyr)
library(DBI)
library(RSQLite)

progressUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Your Progress"),
    tableOutput(ns("progress_table")),
    h3("Your Code Challenge Progress"),
    tableOutput(ns("code_challenge_progress_table")),
    actionButton(ns("refresh_progress"), "Refresh Progress")
  )
}

progressServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    updateProgressTable <- function() {
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
      
      # Query to get the total score
      total_score <- dbGetQuery(con, "
        SELECT 
          total_score 
        FROM 
          Students 
        WHERE 
          student_id = ?
      ", params = list(rv$student_id))
      
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
    }
    
    updateCodeChallengeProgressTable <- function() {
      # Connect to the database
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      # Query to get the code challenge progress data
      progress_data <- dbGetQuery(con, "
        SELECT 
          sub.question_id,
          sub.is_correct
        FROM 
          CcSubmissions sub
        WHERE 
          sub.student_id = ?
      ", params = list(rv$student_id))
      
      # Initialize progress data
      progress_data_wide <- data.frame(
        Challenge = c("Challenge 1", "Challenge 2", "Challenge 3"),
        Q1 = rep(0, 3),
        Q2 = rep(0, 3),
        Q3 = rep(0, 3),
        Q4 = rep(0, 3),
        Q5 = rep(0, 3),
        Q6 = rep(0, 3),
        Q7 = rep(0, 3),
        Q8 = rep(0, 3),
        Q9 = rep(0, 3),
        Q10 = rep(0, 3),
        Total = rep(0, 3)
      )
      
      if (!is.null(progress_data) && nrow(progress_data) > 0) {
        # Map questions to Q1, Q2, ..., Q10 based on question_id
        progress_data$challenge_id <- as.integer(sapply(strsplit(sub("cc", "", as.character(progress_data$question_id)), "\\."), `[`, 1))
        progress_data$question_num <- as.integer(sapply(strsplit(as.character(progress_data$question_id), "\\."), `[`, 2))
        
        for (i in 1:nrow(progress_data)) {
          challenge_id <- progress_data$challenge_id[i]
          question_num <- progress_data$question_num[i]
          is_correct <- progress_data$is_correct[i]
          
          if (!is.na(question_num) && question_num >= 1 && question_num <= 10) {
            progress_data_wide[challenge_id, paste0("Q", question_num)] <- is_correct
          }
        }
        
        # Calculate totals for each challenge
        progress_data_wide$Total <- rowSums(progress_data_wide[, paste0("Q", 1:10)])
      }
      
      # Ensure scores are discrete integers
      progress_data_wide <- progress_data_wide %>%
        mutate(across(Q1:Total, as.integer))
      
      rv$code_challenge_progress <- progress_data_wide
    }
    
    observeEvent(rv$student_name, {
      if (!is.null(rv$student_name) && rv$student_name != "") {
        # Retrieve or create student ID from the database
        rv$student_id <- get_or_create_student_id(rv$student_name)
        updateProgressTable()
        updateCodeChallengeProgressTable()
      }
    })
    
    observeEvent(input$refresh_progress, {
      if (!is.null(rv$student_id)) {
        updateProgressTable()
        updateCodeChallengeProgressTable()
        output$progress_table <- renderTable({
          rv$progress
        }, striped = TRUE, hover = TRUE, bordered = TRUE)
        
        output$code_challenge_progress_table <- renderTable({
          rv$code_challenge_progress
        }, striped = TRUE, hover = TRUE, bordered = TRUE)
      }
    })
    
    output$progress_table <- renderTable({
      progress_data <- rv$progress
      if (is.null(progress_data)) return(NULL)
      
      # Render the table
      progress_data
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$code_challenge_progress_table <- renderTable({
      progress_data <- rv$code_challenge_progress
      if (is.null(progress_data)) return(NULL)
      
      # Render the table
      progress_data
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}