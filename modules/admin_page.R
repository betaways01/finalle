# modules/admin_page.R

library(shiny)

adminPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Admin Dashboard"),
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("refresh"), "Refresh Data")
      ),
      mainPanel(
        tableOutput(ns("students_table")),
        actionButton(ns("logout"), "Logout", class = "btn-danger")
      )
    )
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
        SELECT name, total_score, cc_total_score
        FROM Students
      ")
      
      rv$students_data <- students_data
    }
    
    # Observe refresh button to update the table
    observeEvent(input$refresh, {
      updateStudentsTable()
      output$students_table <- renderTable({
        rv$students_data
      })
    })
    
    # Observe logout button
    observeEvent(input$logout, {
      rv$user_type <- NULL
      session$reload()
    })
    
    # Initial table load
    updateStudentsTable()
    output$students_table <- renderTable({
      rv$students_data
    })
  })
}