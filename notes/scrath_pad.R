# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)

devtools::load_all()

# cleaning teacher survey ---------------
# teacher_survey_pre_url <- "https://docs.google.com/spreadsheets/d/1-sjUC4BWH-Ad-TQVK2dACb3WAzlnJRsf9Yxbm2v2KKo/edit#gid=35865763"
#df <- googlesheets4::read_sheet(teacher_survey_pre_url)

df <- teacher_pre_survey

grouping_columns <- c(3)
question_columns <- 8:30

he_questions <- teacher_pre_survey %>%
  g2gtools::tidy_teacher_survey(grouping_columns, question_columns) %>%
  dplyr::filter(stringr::str_detect(question_stem, "statements about your state standards")) %>%
  dplyr::pull(response_option)

# test that everything works without an error
metric_responses <- teacher_survey_add_he_metric_colnames(he_questions)

testthat::expect_equal(metric_responses[1], "exp_fairtomaster")
testthat::expect_equal(metric_responses[2], "exp_oneyearenough")
testthat::expect_equal(metric_responses[3], "exp_allstudents")
testthat::expect_equal(metric_responses[4], "exp_appropriate")

# test that there is an error when there are not four metric responses
three_metric_responses <- he_questions[metric_responses != "exp_fairtomaster"]

testthat::expect_error(teacher_survey_add_he_metric_colnames(three_metric_responses), "You failed.*exp_fairtomaster.*")

# test that there is an error when there are too many questions
additional_question <- c(he_questions, "One year is enough sdaffdsafa master these standards.")

testthat::expect_error(teacher_survey_add_he_metric_colnames(additional_question), "You have 5.*")



a <- he %>%
  filter(!metric == 'exp_fairtomaster')


teacher_survey_add_he_metric_colnames(b)

full_question_column <- test_data$full_question
devtools::load_all()

he_questions <- teacher_pre_survey$response
