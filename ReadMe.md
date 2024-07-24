# Interactive R Course - Documentation

## Project Structure

The project is structured as follows:

```
finalle fh$ tree
.
├── CodeChallenge
│   ├── code_challenge_1.R
│   ├── code_challenge_2.R
│   └── code_challenge_3.R
├── ReadMe.md
├── app.R
├── database
│   ├── database.sqlite
│   ├── db_init.R
│   └── db_operations.R
├── exercises
│   ├── exercises_ANOVA.R
│   ├── exercises_AdvancedRProgramming.R
│   ├── exercises_CategoricalDataAnalysis.R
│   ├── exercises_DataAnalysisTypes.R
│   ├── exercises_DataStructures.R
│   ├── exercises_DataVisualization.R
│   ├── exercises_DataWrangling.R
│   ├── exercises_DescriptiveStatistics.R
│   ├── exercises_InferentialHypothesis.R
│   ├── exercises_ProbabilityStatistics.R
│   ├── exercises_RFundamentals.R
│   └── exercises_RegressionAnalysis.R
├── finalle.Rproj
├── manifest.json
├── modules
│   ├── admin_page.R
│   ├── cc_helpers.R
│   ├── challenge_observers.R
│   ├── challenges.R
│   ├── exercise_observers.R
│   ├── helpers.R
│   ├── main_ui.R
│   ├── progress.R
│   └── utils.R
├── rsconnect
│   └── shinyapps.io
│       └── 1n7mwy-0-ways
│           └── finalle.dcf
└── www
    ├── 01-R-Fundamentals.Rmd
    ├── 01-R-Fundamentals.html
    ├── 02-DataStructures.Rmd
    ├── 02-DataStructures.html
    ├── 03-DataVisualization.Rmd
    ├── 03-DataVisualization.html
    ├── 04-DataAnalysisTypes.Rmd
    ├── 04-DataAnalysisTypes.html
    ├── 05-DescriptiveStatistics.Rmd
    ├── 05-DescriptiveStatistics.html
    ├── 06-Probability-Statistics.Rmd
    ├── 06-Probability-Statistics.html
    ├── 07-Inferential-Hypothesis.Rmd
    ├── 07-Inferential-Hypothesis.html
    ├── 08-ANOVA.Rmd
    ├── 08-ANOVA.html
    ├── 09-RegressionAnalysis.Rmd
    ├── 09-RegressionAnalysis.html
    ├── 10-CategoricalDataAnalysis.Rmd
    ├── 10-CategoricalDataAnalysis.html
    ├── 11-CodeChallenge-1.Rmd
    ├── 11-CodeChallenge-1.html
    ├── 12-CodeChallenge-2.Rmd
    ├── 12-CodeChallenge-2.html
    ├── 13-CodeChallenge-3.Rmd
    ├── 13-CodeChallenge-3.html
    ├── 14-DataWrangling.Rmd
    ├── 14-DataWrangling.html
    ├── 15-AdvancedRProgramming.Rmd
    ├── 15-AdvancedRProgramming.html
    ├── Rprof.out
    ├── script.js
    └── styles.css
```

## Interaction and Modification

### Theory HTMLs
1. **Location**: All theory content HTML files are located in the `www` directory.
2. **Modification**:
   - To update the theory content, edit the respective `.Rmd` file and render it to HTML using RStudio or any R environment.
   - Place the updated HTML file in the `www` directory with the same filename to ensure the app loads the updated content.

### Exercises
1. **Location**: Exercise logic is located in the `exercises` directory.
2. **Modification**:
   - Each exercise file corresponds to a topic. For instance, `exercises_RFundamentals.R` handles exercises for the R Fundamentals topic.
   - Modify these R files to change the exercises.
   - Ensure the exercise IDs and corresponding logic align with the database structure.

### Code Challenges
1. **Location**: Code challenge logic is located in the `CodeChallenge` directory.
2. **Modification**:
   - Each code challenge file corresponds to a specific challenge. For example, `code_challenge_1.R` handles the first code challenge.
   - Update these R files to change the code challenges.
   - Ensure the challenge IDs and corresponding logic align with the database structure.

## Project Structure Explanation
1. **app.R**: The main application file which initializes the app and includes the UI and server logic for both students and admins.
2. **database**: Contains the SQLite database and scripts for initializing and interacting with the database.
   - `database.sqlite`: The SQLite database file.
   - `db_init.R`: Script to initialize the database.
   - `db_operations.R`: Contains functions to interact with the database.
3. **modules**: Contains modular components used in the app.
   - `admin_page.R`: Logic for the admin dashboard.
   - `cc_helpers.R`, `challenge_observers.R`, `challenges.R`, `exercise_observers.R`, `helpers.R`, `main_ui.R`, `progress.R`, `utils.R`: Various helper functions and modules for different parts of the app.
4. **www**: Static files like HTML content for theory, JavaScript, and CSS files.
5. **rsconnect**: Configuration for deploying the app using RStudio Connect.

## Workflow

### Instructor Workflow
1. **Update Theory Content**:
   - Modify the `.Rmd` file in the `www` directory.
   - Render the updated `.Rmd` file to HTML and save it in the `www` directory.
2. **Update Exercises**:
   - Edit the corresponding file in the `exercises` directory.
   - Ensure the IDs and logic match with the database structure.
3. **Update Code Challenges**:
   - Edit the corresponding file in the `CodeChallenge` directory.
   - Ensure the IDs and logic match with the database structure.

### User Workflow
1. **Student**:
   - Log in by entering the student name.
   - Access theory, exercises, and code challenges.
   - Progress is saved and displayed in the progress tables.
2. **Admin**:
   - Log in using the admin credentials.
   - View and manage student progress.
   - Delete student records if necessary.

## Important Notes
- **Consistency**: Ensure the IDs in the exercise and code challenge files match the database IDs.
- **Database Backup**: Regularly back up the SQLite database to prevent data loss.
- **Security**: Keep admin credentials secure and update them periodically.

## Precautions
- **Data Integrity**: When deleting student records, ensure that all related data (exercises and challenges) are also removed to maintain database integrity.
- **Testing**: Thoroughly test any changes in a development environment before deploying them to production.
- **Version Control**: Use version control (e.g., Git) to track changes and revert to previous versions if necessary.

This documentation provides an overview of the project structure, instructions for making changes, and guidelines for maintaining the app.
