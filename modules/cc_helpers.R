# modules/cc_helpers.R
library(ggplot2)

reset_challenge <- function(rv, output, session) {
  # Reset reactive values for challenges
  rv$challenge_code_output1 <- NULL
  rv$challenge_plot_output1 <- NULL
  rv$challenge_code_output2 <- NULL
  rv$challenge_plot_output2 <- NULL
  
  # Reset UI outputs for challenges
  output$challenge_page <- renderUI({})
  output$challenge_submission_status <- renderText("")
}

# Function to evaluate R code for challenges and capture outputs, including plots
evaluate_challenge_code <- function(code) {
  result <- tryCatch({
    output <- eval(parse(text = code))
    
    # Handle ggplot2 plots
    if (inherits(output, "ggplot")) {
      return(list(type = "plot", content = output))
    } else if (length(dev.list()) > 0) {  # Handle base R plots
      plot <- recordPlot()
      dev.off()  # Turn off the device to capture the plot
      return(list(type = "plot", content = plot))
    } else {
      return(list(type = "text", content = capture.output(print(output))))
    }
  }, error = function(e) {
    return(list(type = "text", content = e$message))
  })
}

# run_code function for challenges
run_challenge_code <- function(input_id, editor_id, output_id, session) {
  observeEvent(session$input[[input_id]], {
    # Clear previous outputs for challenges
    session$output[[paste0(output_id, "_code")]] <- renderPrint({ NULL })
    session$output[[paste0(output_id, "_plot")]] <- renderPlot({ plot.new() })
    
    # Get the code from the editor for challenges
    code <- session$input[[editor_id]]
    
    # Evaluate the code for challenges
    code_output <- evaluate_challenge_code(code)
    
    # Render the outputs for challenges
    if (code_output$type == "plot") {
      session$output[[paste0(output_id, "_plot")]] <- renderPlot({
        code_output$content
      })
    } else {
      session$output[[paste0(output_id, "_code")]] <- renderPrint({ code_output$content })
    }
  })
}

check_challenge_code <- function(correct_code, input_id, editor_id, question_id, attempts, rv, session) {
  observeEvent(session$input[[input_id]], {
    req(session$input[[input_id]])
    student_code <- session$input[[editor_id]]
    correct_result <- tryCatch({
      eval(parse(text = correct_code))
    }, error = function(e) {
      NULL
    })
    
    student_result <- tryCatch({
      eval(parse(text = student_code))
    }, error = function(e) {
      NULL
    })
    
    is_correct <- identical(student_result, correct_result)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
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
    
    student_id <- rv$student_id
    answer <- session$input[[editor_id]]
    attempt_count <- get_attempt_count(student_id, question_id) + 1
    add_submission(student_id, question_id, answer, is_correct, attempt_count)
    update_overall_score(student_id)
  })
}

check_challenge_mcq <- function(correct_answer, input_id, question_id, feedback_id, attempts, rv, session) {
  observeEvent(session$input[[input_id]], {
    req(session$input[[input_id]])
    selected_answer <- session$input[[input_id]]
    is_correct <- identical(selected_answer, correct_answer)
    
    if (is_correct) {
      showModal(modalDialog(
        div(class = "feedback-correct", "Correct!!! Proceed to the Next Question"),
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
    
    student_id <- rv$student_id
    answer <- session$input[[input_id]]
    attempt_count <- get_attempt_count(student_id, question_id) + 1
    add_submission(student_id, question_id, answer, is_correct, attempt_count)
    update_overall_score(student_id)
  })
}

generate_challenge_ui <- function(question_number, question_text, code_editor_id, run_button_id, check_button_id, output_id, default_code) {
  tabPanel(paste("Question", question_number),
           h3(question_text),
           aceEditor(code_editor_id, mode = "r", theme = "github", height = "200px", value = default_code),
           actionButton(run_button_id, "Run", class = "btn btn-run"),
           actionButton(check_button_id, "Check", class = "btn btn-check"),
           div(id = output_id,
               verbatimTextOutput(paste0(output_id, "_code")),
               plotOutput(paste0(output_id, "_plot"))
           )
  )
}

generate_challenge_mcq_ui <- function(question_number, question_text, mcq_id, choices, feedback_id) {
  tabPanel(paste("Question", question_number),
           h3(question_text),
           radioButtons(mcq_id, "Choose one:", choices = choices, selected = character(0)),
           textOutput(feedback_id)
  )
}