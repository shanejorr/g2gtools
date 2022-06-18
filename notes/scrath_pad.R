# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)
library(g2gtools)

# devtools::load_all()

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
column_of_scales <- c('Somewhat Disagree', 'Disagree', 'Agree)
scale_to_factor(column_of_scales, list_of_scales)
setdiff(c('a', 'b', 'c', 'z'), c('a', 'b', 'c', 'd'))

intersect(a$unique_values, scale_order('agree_disagree'), )
# HE questions ----------------------------

he_questions <- teacher_pre_survey %>%
  tidy_forms_survey(grouping_columns, question_columns) %>%
  teacher_survey_calc_high_expectations()

