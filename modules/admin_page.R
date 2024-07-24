# modules/admin_page.R

library(shiny)
library(DT)
library(dplyr)

adminPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Admin Dashboard"),
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("refresh"), "Refresh Data"),
        actionButton(ns("manage_students"), "Manage Students", class = "btn-primary")
      ),
      mainPanel(
        DT::dataTableOutput(ns("students_table")),
        actionButton(ns("logout"), "Logout", class = "btn-danger")
      )
    ),
    uiOutput(ns("student_details_popup"))
  )
}

adminPageServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Function to update the students table
    updateStudentsTable <- function() {
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      students_data <- dbGetQuery(con, "
        SELECT name AS 'Name', 
               total_score AS 'Exercise Total Score', 
               cc_total_score AS 'Challenge Total Score'
        FROM Students
      ")
      
      rv$students_data <- students_data
    }
    
    # Observe refresh button to update the table
    observeEvent(input$refresh, {
      updateStudentsTable()
      output$students_table <- DT::renderDataTable({
        DT::datatable(rv$students_data, selection = 'single', options = list(pageLength = 5))
      })
    })
    
    # Observe logout button
    observeEvent(input$logout, {
      rv$user_type <- NULL
      session$reload()
    })
    
    # Initial table load
    updateStudentsTable()
    output$students_table <- DT::renderDataTable({
      DT::datatable(rv$students_data, selection = 'single', options = list(pageLength = 5))
    })
    
    # Function to get detailed scores for a student
    getStudentDetails <- function(student_name) {
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      progress_data <- dbGetQuery(con, "
        SELECT 
          sub.question_id,
          sub.is_correct
        FROM 
          Submissions sub
        JOIN Students st ON sub.student_id = st.student_id
        WHERE st.name = ?
      ", params = list(student_name))
      
      exercise_scores <- dbGetQuery(con, "
        SELECT 
          exercise_id, 
          score 
        FROM 
          ExerciseScores 
        JOIN Students st ON ExerciseScores.student_id = st.student_id
        WHERE st.name = ?
      ", params = list(student_name))
      
      total_score <- dbGetQuery(con, "
        SELECT 
          total_score 
        FROM 
          Students 
        WHERE name = ?
      ", params = list(student_name))
      
      progress_data_wide <- data.frame(
        Exercise = 1:12,
        Q1 = rep(0, 12),
        Q2 = rep(0, 12),
        Q3 = rep(0, 12),
        Q4 = rep(0, 12),
        Total = rep(0, 12)
      )
      
      if (!is.null(progress_data) && nrow(progress_data) > 0) {
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
        
        progress_data_wide$Total <- rowSums(progress_data_wide[, c("Q1", "Q2", "Q3", "Q4")])
      }
      
      if (!is.null(exercise_scores) && nrow(exercise_scores) > 0) {
        for (i in 1:nrow(exercise_scores)) {
          exercise_id <- exercise_scores$exercise_id[i]
          score <- exercise_scores$score[i]
          progress_data_wide$Total[exercise_id] <- score
        }
      }
      
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
      
      progress_data_wide <- progress_data_wide %>%
        mutate(across(Q1:Total, as.integer))
      
      challenge_progress_data <- dbGetQuery(con, "
        SELECT 
          sub.question_id,
          sub.is_correct
        FROM 
          CcSubmissions sub
        JOIN Students st ON sub.student_id = st.student_id
        WHERE st.name = ?
      ", params = list(student_name))
      
      challenge_progress_data_wide <- data.frame(
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
      
      if (!is.null(challenge_progress_data) && nrow(challenge_progress_data) > 0) {
        challenge_progress_data$challenge_id <- as.integer(sapply(strsplit(sub("cc", "", as.character(challenge_progress_data$question_id)), "\\."), `[`, 1))
        challenge_progress_data$question_num <- as.integer(sapply(strsplit(as.character(challenge_progress_data$question_id), "\\."), `[`, 2))
        
        for (i in 1:nrow(challenge_progress_data)) {
          challenge_id <- challenge_progress_data$challenge_id[i]
          question_num <- challenge_progress_data$question_num[i]
          is_correct <- challenge_progress_data$is_correct[i]
          
          if (!is.na(question_num) && question_num >= 1 && question_num <= 10) {
            challenge_progress_data_wide[challenge_id, paste0("Q", question_num)] <- is_correct
          }
        }
        
        challenge_progress_data_wide$Total <- rowSums(challenge_progress_data_wide[, paste0("Q", 1:10)])
      }
      
      challenge_progress_data_wide <- challenge_progress_data_wide %>%
        mutate(across(Q1:Total, as.integer))
      
      list(exercise_scores = progress_data_wide, challenge_scores = challenge_progress_data_wide)
    }
    
    # Observe table row click to show student details
    observeEvent(input$students_table_rows_selected, {
      selected_row <- input$students_table_rows_selected
      if (!is.null(selected_row)) {
        selected_student <- rv$students_data[selected_row, "Name"]
        student_details <- getStudentDetails(selected_student)
        
        showModal(modalDialog(
          title = paste("Details for", selected_student),
          tags$style(HTML("
            .modal-dialog {
              width: 90%;
              max-width: none;
            }
            .modal-body {
              font-size: 80%;
              padding: 15px;
            }
            .table-container {
              display: flex;
              justify-content: space-between;
            }
            .table-container > div {
              width: 45%;
            }
          ")),
          div(class = "table-container",
              div(
                h3("Exercise Scores"), 
                tableOutput(ns("exercise_scores_table"))
              ),
              div(
                h3("Challenge Scores"), 
                tableOutput(ns("challenge_scores_table"))
              )
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
        output$exercise_scores_table <- renderTable({
          student_details$exercise_scores
        }, rownames = TRUE, colnames = TRUE)
        
        output$challenge_scores_table <- renderTable({
          student_details$challenge_scores
        }, rownames = TRUE, colnames = TRUE)
      }
    })
    
    # Observe manage students button
    observeEvent(input$manage_students, {
      showModal(modalDialog(
        title = "Manage Students",
        selectInput(ns("select_student"), "Select Student to Delete", choices = rv$students_data$Name),
        actionButton(ns("delete_student"), "Delete", class = "btn-danger"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # Observe delete student button
    observeEvent(input$delete_student, {
      req(input$select_student)
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      student_id <- dbGetQuery(con, "SELECT student_id FROM Students WHERE name = ?", params = list(input$select_student))$student_id
      
      dbExecute(con, "DELETE FROM Submissions WHERE student_id = ?", params = list(student_id))
      dbExecute(con, "DELETE FROM CcSubmissions WHERE student_id = ?", params = list(student_id))
      dbExecute(con, "DELETE FROM ExerciseScores WHERE student_id = ?", params = list(student_id))
      dbExecute(con, "DELETE FROM ChallengeScores WHERE student_id = ?", params = list(student_id))
      dbExecute(con, "DELETE FROM Students WHERE student_id = ?", params = list(student_id))
      
      # Update the students table
      updateStudentsTable()
      output$students_table <- DT::renderDataTable({
        DT::datatable(rv$students_data, selection = 'single', options = list(pageLength = 5))
      })
      
      removeModal()
      showModal(modalDialog(
        title = "Success",
        paste("Student", input$select_student, "and all their details have been deleted."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
  })
}