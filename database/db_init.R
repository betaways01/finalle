library(DBI)
library(RSQLite)

initialize_db <- function(db_path) {
  retries <- 5
  while (retries > 0) {
    con <- try(dbConnect(RSQLite::SQLite(), db_path), silent = TRUE)
    if (inherits(con, "SQLiteConnection")) {
      on.exit(dbDisconnect(con))
      
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS Students (
          student_id INTEGER PRIMARY KEY AUTOINCREMENT,
          name TEXT NOT NULL,
          total_score INTEGER DEFAULT 0,
          cc_total_score INTEGER DEFAULT 0
        );
      ")
      
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS Questions (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          exercise_id INTEGER,
          question_text TEXT NOT NULL,
          question_type TEXT NOT NULL,
          correct_answer TEXT,
          hint TEXT,
          solution TEXT
        );
      ")
      
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS Submissions (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          student_id INTEGER,
          question_id INTEGER,
          answer TEXT,
          is_correct INTEGER,
          attempt_count INTEGER,
          submission_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          UNIQUE(student_id, question_id),
          FOREIGN KEY (student_id) REFERENCES Students(student_id),
          FOREIGN KEY (question_id) REFERENCES Questions(id)
        );
      ")
      
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS Admins (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          username TEXT NOT NULL,
          password TEXT NOT NULL
        );
      ")
      
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS CcSubmissions (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          student_id INTEGER,
          question_id TEXT,
          answer TEXT,
          is_correct INTEGER,
          attempt_count INTEGER,
          submission_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          UNIQUE(student_id, question_id),
          FOREIGN KEY (student_id) REFERENCES Students(student_id)
        );
      ")
      
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ExerciseScores (
          student_id INTEGER,
          exercise_id INTEGER,
          score INTEGER DEFAULT 0,
          PRIMARY KEY (student_id, exercise_id),
          FOREIGN KEY (student_id) REFERENCES Students(student_id),
          FOREIGN KEY (exercise_id) REFERENCES Questions(exercise_id)
        );
      ")
      
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ChallengeScores (
          student_id INTEGER,
          challenge_id TEXT,
          score INTEGER DEFAULT 0,
          PRIMARY KEY (student_id, challenge_id),
          FOREIGN KEY (student_id) REFERENCES Students(student_id)
        );
      ")
      
      return(invisible(TRUE))
    }
    retries <- retries - 1
    Sys.sleep(0.1)  # Wait for 100 ms before retrying
  }
  stop("Failed to initialize the database after multiple attempts.")
}