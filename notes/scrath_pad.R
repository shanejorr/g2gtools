# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.

devtools::load_all()

# cleaning teacher survey ---------------
teacher_survey_pre_url <- "https://docs.google.com/spreadsheets/d/1-sjUC4BWH-Ad-TQVK2dACb3WAzlnJRsf9Yxbm2v2KKo/edit#gid=35865763"

df <- googlesheets4::read_sheet(teacher_survey_pre_url)

grouping_columns <- c(3)
question_columns <- 8:30

test_data <- tidy_teacher_survey(df, grouping_columns, question_columns)

full_question_column <- test_data$full_question
devtools::load_all()

