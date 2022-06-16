# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)
library(g2gtools)

# devtools::load_all()

# cleaning teacher survey ---------------

# pre-survey columns

question_columns <- 8:30

col_to_test <- 'I get feedback on the content of my lesson'

expected_responses <- tibble::tibble(
  response = c('Agree', 'Disagree', 'Somewhat Agree', 'Somewhat Disagree', 'Strongly Agree'),
  .n_response = c(11, 1, 2, 1, 2)
)

total_num_responses <- 17

pre_survey <- teacher_pre_survey %>%
  tidy_teacher_survey(question_columns) |>
  teacher_survey_calc_percentages() |>
  dplyr::filter(stringr::str_detect(response_option, col_to_test)) |>
  dplyr::select(response, .n_response, .n_question, .percent) |>
  dplyr::arrange(response)

testthat::expect_equal(sum(pre_survey$.percent), 1)
testthat::expect_equal(sum(pre_survey$.n_response), total_num_responses)
testthat::expect_equal(max(pre_survey$.n_question), total_num_responses)
testthat::expect_equal(select(pre_survey, response, .n_response), expected_responses)

single_col <- teacher_pre_survey |>
  select(contains(col_to_test)) |>
  rename(test_col = 1) |>
  count(test_col)

# HE questions ----------------------------

he_questions <- teacher_pre_survey %>%
  tidy_teacher_survey(grouping_columns, question_columns) %>%
  teacher_survey_calc_high_expectations()

