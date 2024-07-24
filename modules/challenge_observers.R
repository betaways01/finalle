# modules/challenge_observers
initializeChallengeObservers <- function(input, output, session, rv) {
  challenge_files <- list.files("CodeChallenge", full.names = TRUE)
  
  for (file in challenge_files) {
    tryCatch({
      source(file, local = TRUE)
    }, error = function(e) {
      cat("Error sourcing", file, ":", e$message, "\n")
    })
  }
}