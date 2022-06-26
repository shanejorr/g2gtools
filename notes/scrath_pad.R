# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)
library(g2gtools)

# devtools::load_all()

# teacher survey --------------------------

teacher_pre <- read_csv('notes/garden_city-summer_2022-teacher-pre.csv') %>%
  mutate(term = 'Pre-Training Survey')

teacher_post <- read_csv('notes/garden_city-summer_2022-teacher-post.csv') %>%
  mutate(term = 'Post-Training Survey')

pre_results <- teacher_pre |>
  tidy_forms_survey(c(9:19, 22:47), c(21, 48))

post_results <- teacher_post |>
  tidy_forms_survey(c(4:9, 11:15, 17:42), c(16, 43))

# label whether question is in pre and post training
common_stems <- intersect(unique(pre_results$question_stem), unique(post_results$question_stem))

pre_grouping_cols <- c("are_you_participating_in_the_math_or_literacy_sessions_this_summer", "term")

all_teacher_results <- pre_results |>
  bind_rows(post_results) |>
  forms_survey_calc_percentages(grouping_columns = pre_grouping_cols) |>
  mutate(in_survey = ifelse(question_stem %in% !!common_stems, "Pre and Post Survey", term)) |>
  # fix misspelling
  mutate(response = str_replace(response, 'Dsagree', 'Disagree'))

## pre and post comparisons -----------------------

pre_and_post <- all_teacher_results |>
  filter(in_survey == "Pre and Post Survey")

unique_stems <- unique(pre_and_post$question_stem)

q_stem <- unique_stems[1]

q <- pre_and_post |>
  filter(question_stem == !!q_stem)

response_col_factor <- scale_to_factor(q$response, scale_order())

column_levels <- levels(response_col_factor)

get_palette()

# obs ------------------------------

obs <- tidy_forms_survey(classroom_observations_math, 8:ncol(classroom_observations_math), c(3, 6)) |>
  classroom_obs_add_ca() |>
  classroom_obs_add_tntpmetrics(grade_column = 'grade', subject_name = 'Math',
                                id_cols = c('.id', 'when_did_the_observation_occur', 'Math'))
