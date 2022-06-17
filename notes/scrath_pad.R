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

# each question stem should be a single plot, faceted by response option


# HE questions ----------------------------

he_questions <- teacher_pre_survey %>%
  tidy_forms_survey(grouping_columns, question_columns) %>%
  teacher_survey_calc_high_expectations()

