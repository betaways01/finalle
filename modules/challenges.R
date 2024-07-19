# modules/challenges.R
source("database/db_operations.R")

challengesUI <- function(id) {
  ns <- NS(id)
  navlistPanel(
    id = ns("navlist_challenges"),
    tabPanel("Code Challenge 1",
             fileInput(ns("upload_code_challenge_1"), "Upload Code Challenge 1", accept = ".Rmd"),
             actionButton(ns("view_results_1"), "View Test Results"),
             actionButton(ns("clear_1"), "Clear"),
             tableOutput(ns("results_1"))
    ),
    tabPanel("Code Challenge 2",
             fileInput(ns("upload_code_challenge_2"), "Upload Code Challenge 2", accept = ".Rmd"),
             actionButton(ns("view_results_2"), "View Test Results"),
             actionButton(ns("clear_2"), "Clear"),
             tableOutput(ns("results_2"))
    ),
    tabPanel("Code Challenge 3",
             fileInput(ns("upload_code_challenge_3"), "Upload Code Challenge 3", accept = ".Rmd"),
             actionButton(ns("view_results_3"), "View Test Results"),
             actionButton(ns("clear_3"), "Clear"),
             tableOutput(ns("results_3"))
    )
  )
}

challengesServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    process_upload <- function(input_id, challenge_name, results_var) {
      observeEvent(input[[input_id]], {
        req(input[[input_id]])
        student_id <- rv$student_id
        file_path <- input[[input_id]]$datapath
        code_content <- paste(readLines(file_path), collapse = "\n")
        
        if (nchar(code_content) == 0) {
          showModal(modalDialog(
            title = "Error",
            "Cannot submit an empty file.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return(NULL)
        }
        
        con <- get_db_connection()
        on.exit(dbDisconnect(con))
        
        dbExecute(con, "
          INSERT OR REPLACE INTO CodeChallenges (student_id, challenge_name, code_content, submission_date) 
          VALUES (?, ?, ?, datetime('now'))
        ", params = list(student_id, challenge_name, code_content))
        
        score <- auto_grade_code(code_content)
        
        dbExecute(con, "
          INSERT OR REPLACE INTO CodeChallenges (student_id, challenge_name, score, submission_time) 
          VALUES (?, ?, ?, datetime('now'))
        ", params = list(student_id, challenge_name, score))
        
        rv[[results_var]] <- score
      })
    }
    
    view_results <- function(view_button, challenge_name, output_id) {
      observeEvent(input[[view_button]], {
        con <- get_db_connection()
        on.exit(dbDisconnect(con))
        
        results <- dbGetQuery(con, "
          SELECT code_content, score
          FROM CodeChallenges
          WHERE student_id = ? AND challenge_name = ?
          ORDER BY submission_date DESC
          LIMIT 1
        ", params = list(rv$student_id, challenge_name))
        
        if (nrow(results) == 0) {
          showModal(modalDialog(
            title = "Test Results",
            "Nothing to show here.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        } else {
          showModal(modalDialog(
            title = "Test Results",
            renderTable({
              data.frame(
                Test = "Test Results",
                Passed = results$code_content,
                Score = results$score
              )
            }),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }
      })
    }
    
    clear_results <- function(clear_button, input_id, results_var, output_id) {
      observeEvent(input[[clear_button]], {
        updateTextInput(session, input_id, value = "")
        rv[[results_var]] <- NULL
        output[[output_id]] <- renderTable(NULL)
      })
    }
    
    process_upload("upload_code_challenge_1", "Code Challenge 1", "results_1")
    view_results("view_results_1", "Code Challenge 1", "results_1")
    clear_results("clear_1", "upload_code_challenge_1", "results_1", "results_1")
    
    process_upload("upload_code_challenge_2", "Code Challenge 2", "results_2")
    view_results("view_results_2", "Code Challenge 2", "results_2")
    clear_results("clear_2", "upload_code_challenge_2", "results_2", "results_2")
    
    process_upload("upload_code_challenge_3", "Code Challenge 3", "results_3")
    view_results("view_results_3", "Code Challenge 3", "results_3")
    clear_results("clear_3", "upload_code_challenge_3", "results_3", "results_3")
  })
}