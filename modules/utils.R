# modules/utils.R

# Ensure ggplot2 is loaded
library(ggplot2)

# Function to evaluate R code
evaluate_code <- function(code) {
  result <- tryCatch({
    output <- eval(parse(text = code))
    if (inherits(output, "ggplot")) {
      list(type = "plot", content = output)
    } else {
      list(type = "text", content = capture.output(print(output)))
    }
  }, error = function(e) {
    list(type = "text", content = e$message)
  })
  result
}

# Function to grade the code using gradethis
grade_code <- function(correct_code, student_code) {
  result <- tryCatch({
    correct_result <- eval(parse(text = correct_code))
    student_result <- eval(parse(text = student_code))
    identical(correct_result, student_result)
  }, error = function(e) {
    FALSE
  })
  result
}