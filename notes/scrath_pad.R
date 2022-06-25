# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)
library(g2gtools)

# devtools::load_all()

# classroom observations ------------------

classroom_observations_math |>
  tidy_forms_survey(8:ncol(obs), c(3,4,5)) |>
  classroom_obs_add_ca()

grouping_vars <- c('')

a <- tidy_forms_survey(obs, 8:ncol(obs), c(3,4,5))

qs <- a %>%
  distinct(question_stem, response_option)



test_obs <- data.frame(
  question_stem = c(
    "test", 'Core Action 1 Overall:', 'Core Action 1 Note:',
    "Are all students engaged in the work of the lesson from start to finish?",
    "Overall, did this lesson reflect the demands of the standards and/or the instructional shifts the standards require?",
    "minor domain", "minor domain"
    ),
  response_option = c('test', rep(NA_character_, 4), "asdf (1a)", "dsaf (2b)")
)
# "asdf (1a)", "dsaf (2b)"
classroom_obs_add_ca(test_obs)

ca_main <- c(NA_character_, '1', NA_character_, 'Culture of Learning', 'Demands of the Standards', '1', '2')
ca_minor <- c(NA_character_, 'Overall', NA_character_, 'Culture of Learning', 'Demands of the Standards', 'a', 'b')
classroom_obs_add_ca(test_obs[1])

# "(?<=Core Action )[0-9]"

# cleaning teacher survey ---------------

# pre-survey columns

question_columns <- 8:30

pre_survey <- teacher_pre_survey |>
  tidy_forms_survey(question_columns) |>
  forms_survey_calc_percentages()

agree_disagree_question <- unique(pre_survey$question_stem)[2]

# each question stem should be a single plot, faceted by response option
single_question <- pre_survey %>%
  filter(question_stem == !!agree_disagree_question)

list_of_scales <- list(
   agree_disagree = c('Strongly Agree', 'Agree', 'Somewhat Agree', 'Somewhat Disagree', 'Disagree', 'Strongly Disagree'),
   knowledge = c('Excellent knowledge', 'Good knowledge', 'Some knowledge', 'A little knowledge', 'No knowledge')
)
column_of_scales <- c('Somewhat Disagree', 'Disagree', 'Agree')
scale_to_factor(column_of_scales, list_of_scales)
setdiff(c('a', 'b', 'c', 'z'), c('a', 'b', 'c', 'd'))

intersect(a$unique_values, scale_order('agree_disagree'), )
# HE questions ----------------------------

he_questions <- teacher_pre_survey %>%
  tidy_forms_survey(grouping_columns, question_columns) %>%
  teacher_survey_calc_high_expectations()

