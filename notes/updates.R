library(tidyverse)

devtools::load_all()

teacher_pre <- teacher_pre_survey |>
  g2g_tidy_forms_survey(8:30, 3) |>
  mutate(assessment = 'Pre-training')

teacher_post <- teacher_pre_survey |>
  g2g_tidy_forms_survey(8:30, 3) |>
  mutate(assessment = 'Post-training')

results <- bind_rows(teacher_pre, teacher_post) |>
  g2g_forms_survey_calc_percentages('assessment')

## work with a single stem ----------------------

unique_stems <- unique(teacher$question_stem)

stem <- unique_stems[4]

single_stem <- results |>
  filter(question_stem == !!stem)

# find the scale and hex color codes, but you need to flip the values and names for plots
scales_to_use <- g2g_find_scale(single_stem$response)
hex_colors <- names(scales_to_use) |> set_names(scales_to_use)

single_stem_clean <- single_stem |>
  g2g_aggregate_positive_responses(scales_to_use[c(2, 1)], 'assessment', TRUE) |>
  mutate(response = factor(response, levels = rev(scales_to_use)))


# visualization ----------------

question_name <- unique(single_stem_clean$question_stem)

g2g_viz_stacked_bar_percent(single_stem_clean, '.percent', 'assessment', 'response', '.strong_response_percent', hex_colors) +
  facet_wrap(vars(response_option), ncol = 1)


