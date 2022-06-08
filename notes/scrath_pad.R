# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)

devtools::load_all()

# cleaning teacher survey ---------------

grouping_columns <- c(3)
question_columns <- 8:30

he_questions <- teacher_pre_survey %>%
  tidy_teacher_survey(grouping_columns, question_columns) %>%
  teacher_survey_calculate_high_expectations()


