---
output:
  html_document: default
  pdf_document: default
---
### Full Workflow and Features for Interactive Exercises

#### 1. Designing the Exercises
Each of the 15 topics will have its own exercise section, and each exercise will contain up to 4 questions, which can be a mix of coding questions and multiple-choice questions (MCQs).

**Exercise Structure:**
- Topic 1 Exercise
  - Question 1: Coding
  - Question 2: MCQ
  - Question 3: Coding
  - Question 4: MCQ
- Topic 2 Exercise
  - (Similarly structured)



#### 2. User Interface (UI) Design
**Main Features:**
- **Topic Navigation:** Navigation buttons are already defined at the end of each HTML to go to the specific exercise page.
- **Student Information Input:** A text input field for entering the student’s name before starting the exercises.
- **Question Flow:** Questions should flow from top to bottom within each exercise to maintain a logical progression and story-like structure.
- **Editor and Input Areas:**
  - **Coding Questions:** Use an `aceEditor` for students to write and run their R code.
  - **MCQ Questions:** Use `radioButtons` for selecting answers.
- **Buttons:** 
  - **Run:** To execute the code written in the `aceEditor` and display the results.
  - **Hint:** To provide hints for the question if needed.
  - **Solution:** To show the solution after a predefined number of attempts.
  - **Submit:** To submit the answers, record them in the database, and grade the submissions.

#### 3. Server Logic
**Handling Code Execution and Display:**
- Evaluate the code input in a secure environment and capture output or error messages to display to the student.

**Hints and Solutions:**
- Track the number of attempts for each question.
- Display hints and solutions conditionally based on the number of attempts.

**Submission Logic:**
- Collect answers for all questions (coding and MCQs).
- Connect to the database to record the submissions.
- Validate inputs and handle errors gracefully.

#### 4. Database Design
**Tables:**
- **Students:** Store student information and overall progress.
- **Questions:** Store individual questions, including type (coding or MCQ).
- **Submissions:** Store each student’s answer to each question, along with attempt count and timestamps.
- **Include others as necessary**

**Fields:**
- Students: `id`, `name`, `email`, `overall_score`
- Exercises: `id`, `topic`, `description`
- Questions: `id`, `exercise_id`, `question_text`, `question_type`, `correct_answer`, `hint`, `solution`
- Submissions: `id`, `student_id`, `question_id`, `answer`, `is_correct`, `attempt_count`, `submission_date`

#### 5. Grading and Feedback
**Grading Logic:**
- Automatically grade MCQs based on the selected answer.
- Automatically grade coding questions based on output comparison or specific criteria.
- Update the score in the `Submissions` table.

**Feedback:**
- Provide immediate feedback for each submission.
- Update the UI to reflect correct answers, hints, and solutions as needed.

#### 6. Workflow
1. **Student logs in and selects a topic.**
2. **Student navigates to the exercise section of the selected topic.**
3. **Student answers each question within the exercise:**
   - Enters code or selects an MCQ answer.
   - Clicks "Run" to test code and see results.
   - Uses "Hint" if needed.
   - Clicks "Submit" to record the answer.
4. **System checks the answer:**
   - For coding questions, the code is executed and output is compared to the expected result.
   - For MCQs, the selected answer is compared to the correct answer.
5. **If the answer is correct, it is recorded with a score; otherwise, feedback is given and attempts are tracked.**
6. **Hints and solutions are displayed based on the number of attempts.**
7. **All submissions are recorded in the database with timestamps.**
8. **Student can see their progress and scores for each exercise.**
