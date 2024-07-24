library(DBI)
library(RSQLite)

db_path <- "database/database.sqlite"

# Function to connect to the database
get_db_connection <- function() {
  dbConnect(RSQLite::SQLite(), db_path)
}

# Function to get the attempt count for a specific question and student
get_attempt_count <- function(student_id, question_id) {
  if (is.null(student_id) || student_id == "" || is.null(question_id) || question_id == "") {
    stop("Student ID or Question ID is missing")
  }
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  table <- ifelse(grepl("^cc", question_id), "CcSubmissions", "Submissions")
  
  result <- dbGetQuery(con, paste0("SELECT attempt_count FROM ", table, " WHERE student_id = ? AND question_id = ?"), params = list(student_id, question_id))
  
  if (nrow(result) == 0) {
    return(0)
  } else {
    return(result$attempt_count[1])
  }
}

# Function to add or update a submission
add_submission <- function(student_id, question_id, answer, is_correct, attempt_count) {
  if (is.null(student_id) || student_id == "" || 
      is.null(question_id) || question_id == "" || 
      is.null(answer) || length(answer) == 0 || 
      is.null(is_correct) || length(is_correct) == 0 || 
      is.null(attempt_count) || length(attempt_count) == 0) {
    stop("Missing parameters for add_submission")
  }
  
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  table <- ifelse(grepl("^cc", question_id), "CcSubmissions", "Submissions")
  
  dbExecute(con, paste0("INSERT OR REPLACE INTO ", table, " (student_id, question_id, answer, is_correct, attempt_count) VALUES (?, ?, ?, ?, ?)"), 
            params = list(student_id, question_id, answer, is_correct, attempt_count))
  
  if (table == "CcSubmissions") {
    # Update the challenge score
    update_challenge_score(student_id, question_id)
  } else {
    # Update the exercise score
    update_exercise_score(student_id, question_id)
  }
  
  # Update the overall score
  update_overall_score(student_id)
}

# Function to update the exercise score for a student
update_exercise_score <- function(student_id, question_id) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  # Extract the exercise_id from the question_id
  exercise_id <- as.integer(sub("\\..*", "", question_id))
  total_score <- dbGetQuery(con, "SELECT SUM(is_correct) as score FROM Submissions WHERE student_id = ? AND question_id LIKE ?", 
                            params = list(student_id, paste0(exercise_id, ".%")))$score
  dbExecute(con, "INSERT OR REPLACE INTO ExerciseScores (student_id, exercise_id, score) VALUES (?, ?, ?)", 
            params = list(student_id, exercise_id, total_score))
}

# Function to update the challenge score for a student
update_challenge_score <- function(student_id, question_id) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  total_score <- dbGetQuery(con, "SELECT SUM(is_correct) as score FROM CcSubmissions WHERE student_id = ? AND question_id = ?", 
                            params = list(student_id, question_id))$score
  dbExecute(con, "INSERT OR REPLACE INTO ChallengeScores (student_id, challenge_id, score) VALUES (?, ?, ?)", 
            params = list(student_id, question_id, total_score))
}

# Function to update the overall score for a student
update_overall_score <- function(student_id) {
  if (is.null(student_id) || student_id == "") {
    stop("Student ID is missing")
  }
  
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  # Update exercise total score
  ex_total_score <- dbGetQuery(con, "SELECT SUM(score) as total_score FROM ExerciseScores WHERE student_id = ?", params = list(student_id))$total_score
  dbExecute(con, "UPDATE Students SET total_score = ? WHERE student_id = ?", params = list(ex_total_score, student_id))
  
  # Update code challenge total score
  cc_total_score <- dbGetQuery(con, "SELECT SUM(score) as total_score FROM ChallengeScores WHERE student_id = ?", params = list(student_id))$total_score
  dbExecute(con, "UPDATE Students SET cc_total_score = ? WHERE student_id = ?", params = list(cc_total_score, student_id))
}

# Function to get or create student ID
get_or_create_student_id <- function(student_name) {
  if (is.null(student_name) || student_name == "") {
    stop("Student name is missing")
  }
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  # Check if student already exists
  student <- dbGetQuery(con, "SELECT student_id FROM Students WHERE name = ?", params = list(student_name))
  
  if (nrow(student) > 0) {
    return(student$student_id[1])
  } else {
    dbExecute(con, "INSERT INTO Students (name) VALUES (?)", params = list(student_name))
    student_id <- dbGetQuery(con, "SELECT last_insert_rowid() as student_id")$student_id[1]
    return(student_id)
  }
}

# Function to get exercise scores
get_exercise_scores <- function(student_id) {
  if (is.null(student_id) || student_id == "") {
    stop("Student ID is missing")
  }
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  exercise_scores <- dbGetQuery(con, "
    SELECT 
      exercise_id, 
      score 
    FROM 
      ExerciseScores 
    WHERE 
      student_id = ?", 
                                params = list(student_id))
  
  return(exercise_scores)
}

# Function to get challenge scores
get_challenge_scores <- function(student_id) {
  if (is.null(student_id) || student_id == "") {
    stop("Student ID is missing")
  }
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  challenge_scores <- dbGetQuery(con, "
    SELECT 
      challenge_id, 
      score 
    FROM 
      ChallengeScores 
    WHERE 
      student_id = ?", 
                                 params = list(student_id))
  
  return(challenge_scores)
}

# Function to get total score
get_total_score <- function(student_id) {
  if (is.null(student_id) || student_id == "") {
    stop("Student ID is missing")
  }
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  total_scores <- dbGetQuery(con, "
    SELECT 
      total_score,
      cc_total_score
    FROM 
      Students 
    WHERE 
      student_id = ?", 
                             params = list(student_id))
  
  if (nrow(total_scores) == 0) {
    return(list(total_score = 0, cc_total_score = 0))
  }
  
  return(list(total_score = total_scores$total_score[1], cc_total_score = total_scores$cc_total_score[1]))
}

# New Function: Get the latest progress of a student
get_latest_student_progress <- function(student_id) {
  if (is.null(student_id) || student_id == "") {
    stop("Student ID is missing")
  }
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  progress <- dbGetQuery(con, "
    SELECT 
      q.exercise_id,
      q.id AS question_id, 
      sub.is_correct
    FROM 
      Submissions sub
    JOIN 
      Questions q ON sub.question_id = q.id
    WHERE 
      sub.student_id = ?
    ORDER BY 
      q.exercise_id, q.id
  ", params = list(student_id))
  
  if (nrow(progress) == 0) {
    return(NULL)
  } else {
    return(progress)
  }
}

# Add or update a code challenge submission
add_code_challenge_submission <- function(student_id, question_id, answer, is_correct, attempt_count) {
  if (is.null(student_id) || student_id == "" || 
      is.null(question_id) || question_id == "" || 
      is.null(answer) || length(answer) == 0 || 
      is.null(is_correct) || length(is_correct) == 0 || 
      is.null(attempt_count) || length(attempt_count) == 0) {
    stop("Missing parameters for add_code_challenge_submission")
  }
  
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  dbExecute(con, "INSERT OR REPLACE INTO CcSubmissions (student_id, question_id, answer, is_correct, attempt_count) VALUES (?, ?, ?, ?, ?)", 
            params = list(student_id, question_id, answer, is_correct, attempt_count))
  
  # Update the challenge score
  update_challenge_score(student_id, question_id)
  
  # Update the overall score
  update_overall_score(student_id)
}

# Function to get code challenge scores for a student
get_code_challenge_scores <- function(student_id) {
  if (is.null(student_id) || student_id == "") {
    stop("Student ID is missing")
  }
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  challenge_scores <- dbGetQuery(con, "
    SELECT 
      challenge_id, 
      score 
    FROM 
      ChallengeScores 
    WHERE 
      student_id = ?", 
                                 params = list(student_id))
  
  return(challenge_scores)
}