library(tidyverse)

devtools::load_all()

teacher_pre <- teacher_pre_survey |>
  tidy_forms_survey(8:30, 3) |>
  mutate(assessment = 'Pre-training')

teacher_post <- teacher_pre_survey |>
  tidy_forms_survey(8:30, 3) |>
  mutate(assessment = 'Post-training')

teacher <- bind_rows(teacher_pre, teacher_post) |>
  forms_survey_calc_percentages('assessment')

results <- teacher_pre_survey |>
  tidy_forms_survey(8:30, 3) |>
  mutate(assessment = 'Pre-survey') |>
  forms_survey_calc_percentages('assessment') |>
  # the function should only be used on a data set with common scales
  # so, filter to only keep a single question stem since we know it will have the same scales
  dplyr::filter(stringr::str_detect(.data[['question_stem']], 'To what extent do you agree'))

g2g_aggregate_positive_responses(results, c('Agree', 'Strongly Agree'), 'assessment')

# tests -----------------------

df <- data.frame(
  assessment = 'Pre-training',
  question_stem = "What are you up to?",
  response_option = rep(c('a', 'b'), each = 4),
  response = rep(c('bad', 'very bad', 'good', 'very good'), 2),
  .percent = rep(c(.2, .25, .2, .35), 2)
)

df <- df |>
  dplyr::bind_rows(df |> dplyr::mutate(assessment = 'Post-training'))

test_results <- g2g_aggregate_positive_responses(
  df, c('good', 'very good'), 'assessment'
)

needed_results <- df |>
  dplyr::mutate(
    .scale_strength = rep(c('Weak response', 'Weak response', 'Strong response', 'Strong response'), 4),
    .strong_response_percent = rep(c(.45, .45, .55, .55), 4)
  ) |>
  tibble::as_tibble()

expect_equal(test_results, needed_results)


unique_stems <- unique(teacher$question_stem)

stem <- unique_stems[4]

## work with a single stem ----------------------

single_stem <- teacher |>
  filter(question_stem == !!stem)

# find the scale and hex color codes, but you need to flip the values and names for plots
scales_to_use <- g2g_find_scale(single_stem$response)
hex_colors <- names(scales_to_use) |>
  set_names(scales_to_use)

a <- single_stem |>
  g2g_aggregate_positive_responses(scales_to_use[1:2], c('assessment', 'question_stem', 'response_option'), '.percent')

g2g_to_title <- function(column_to_convert) {

  words_not_to_convert <- c(
    'and', 'a', 'the', 'as', 'but', 'for', 'if', 'nor', 'or', 'so', 'yet',
    'as', 'at', 'by', 'for', 'in', 'of', 'off', 'on', 'per', 'to', 'up', 'via'
  )

  words_not_to_convert <- str_c(" ", words_not_to_convert, " ")

  replacement_strings <- stringr::str_to_title(words_not_to_convert) |>
    purrr::set_names(words_not_to_convert)

  replacement_strings <- words_not_to_convert |>
    purrr::set_names(stringr::str_to_title(words_not_to_convert))

  convert_to_title <- column_to_convert |>
    stringr::str_to_lower() |>
    stringr::str_to_title()

  convert_to_title <- str_c(convert_to_title, " ") |>
    stringr::str_replace_all(replacement_strings) |>
    stringr::str_trim(side = 'right')

  return(convert_to_title)

}
