# exercise_observers.R

initializeExerciseObservers <- function(input, output, session, rv) {
  exercise_files <- list.files("exercises", full.names = TRUE)
  
  for (file in exercise_files) {
    tryCatch({
      source(file, local = TRUE)
    }, error = function(e) {
      cat("Error sourcing", file, ":", e$message, "\n")
    })
  }
}
