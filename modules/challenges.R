# modules/challenges.R
source("database/db_operations.R")
source("CodeChallenge/code_challenge_1.R")
source("CodeChallenge/code_challenge_2.R")
source("CodeChallenge/code_challenge_3.R")

challengesUI <- function(id) {
  ns <- NS(id)
  navlistPanel(
    id = ns("navlist_challenges"),
    tabPanel("Code Challenge 1",
             codeChallenge1UI(ns("code_challenge_1"))
    )
  )
}

challengesServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize code challenge 1
    codeChallenge1Server(ns("code_challenge_1"), rv)
    
    # Initialize code challenge 2
    #codeChallenge2Server(ns("code_challenge_2"), rv)
    
    # Initialize code challenge 3
    #codeChallenge3Server(ns("code_challenge_3"), rv)
  })
}
