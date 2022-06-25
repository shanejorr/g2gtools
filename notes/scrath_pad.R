# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)
library(g2gtools)
tidy_forms_survey(classroom_observations_math, 8:ncol(classroom_observations_math), c(3, 6)) |>
  classroom_obs_add_ca() |>
  classroom_obs_add_tntpmetrics(grade_column = 'grade', subject_name = 'Math',
                                id_cols = c('.id', 'when_did_the_observation_occur', 'Math'))

# devtools::load_all()
