# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)
library(googlesheets4)
library(g2gtools)

# devtools::load_all()

# teacher survey --------------------------

teacher_pre <- read_csv('notes/garden_city-summer_2022-teacher-pre.csv') %>%
  mutate(term = 'Pre-Training')

teacher_post <- read_csv('notes/garden_city-summer_2022-teacher-post.csv') %>%
  mutate(term = 'Post-Training')

pre_results <- teacher_pre |>
  tidy_forms_survey(c(9:19, 22:47), c(21, 48))

post_results <- teacher_post |>
  tidy_forms_survey(c(4:9, 11:15, 17:42), c(16, 43))

# label whether question is in pre and post training
common_stems <- intersect(unique(pre_results$question_stem), unique(post_results$question_stem))

pre_grouping_cols <- c("subject", "term")

all_teacher_results <- pre_results |>
  bind_rows(post_results) |>
  # change long column name
  rename(subject = are_you_participating_in_the_math_or_literacy_sessions_this_summer) |>
  # fix misspelling
  mutate(response = str_replace(response, 'Dsagree', 'Disagree')) |>
  # combine agree and strongly agree into one group
  #mutate(response = ifelse(str_detect(response, "^Agree$|^Strongly Agree$"), 'Agree / Strongly Agree', response)) |>
  forms_survey_calc_percentages(grouping_columns = pre_grouping_cols) |>
  mutate(in_survey = ifelse(question_stem %in% !!common_stems, "Pre and Post Survey", term))

## pre and post comparisons -----------------------

pre_and_post <- all_teacher_results |>
  filter(in_survey == "Pre and Post Survey")

unique_stems <- unique(pre_and_post$question_stem)

q_stem <- unique_stems[1]

ind_stem <- pre_and_post |>
  filter(question_stem == !!q_stem)

# we only want to keep agree and strongly agree, so remove other items
scales_to_remove <- c('Somewhat Agree', 'Somewhat Disagree', 'Disagree', 'Strongly Disagree')

ind_stem <- ind_stem |>
  filter(!response %in% !!scales_to_remove)


# find the scale and hex color codes, but you need to flip the values and names for plots
scale_order <- find_scale(ind_stem$response, FALSE)
scale_order <- names(scale_order) |> purrr::set_names(scale_order)

# plot

ind_stem |>
  filter(subject == 'ELA') |>
  mutate(response = factor(response, levels = names(scale_order))) |>
  viz_pre_post_scales (scale_order) +
  labs(title = 'This is a test')

# obs ------------------------------

pers_prep_link <- 'https://docs.google.com/spreadsheets/d/12YrXBV-PSeZWQeQMPx2AC--34ENQSBFMzJzaDOJfehs/edit#gid=2077396197'

pers_prep_obs <- read_sheet(pers_prep_link) |>
  mutate(Grade = as.character(Grade))

pers_prep_tidy <- tidy_forms_survey(classroom_observations_math, 8:ncol(classroom_observations_math), c(3, 4, 5, 6)) |>
  rename(term = when_did_the_observation_occur) |>
  classroom_obs_add_ca()

classroom_obs_add_tntpmetrics(pers_prep_tidy, grade_column = 'grade', subject_name = 'Math', id_cols = c('.id', 'term'))

pers_prep_viz_df <- pers_prep_tidy |>
  drop_na(core_action_main) |>
  forms_survey_calc_percentages(grouping_columns = c('term', 'core_action_main', 'core_action_minor')) |>
  mutate(
    # add space before Overall, so core action number can be combined
    core_action_minor = ifelse(core_action_minor == 'Overall', " Overall", core_action_minor),
    core_action = ifelse(
      str_detect(core_action_main, "[^0-9]"),
      core_action_main,
      glue::glue("CA {core_action_main}{core_action_minor}")
      )
  )

count(pers_prep_viz_df, core_action_main, core_action)
unique(pers_prep_viz_df$core_action)
## visualizations --------------

scale_yes_notyet <- create_color_scales('obs_yes_notyet')

ca_scales <- list(
  '1' = scale_yes_notyet[c(1, 4)],
  '2' = scale_yes_notyet,
  '3' = scale_yes_notyet,
  'Culture of Learning' = scale_yes_notyet,
  'Demands of the Standards' = create_color_scales('obs_yesbut')
)

for (ca in unique(pers_prep_viz_df$core_action_main)) {

  color_pal <- ca_scales[[ca]]

  plt_title <-  if (str_detect(ca, "[0-9]")) glue::glue("Core Action {ca} Observation Results") else glue::glue("{ca} Observation Results")

  plt <- pers_prep_viz_df |>
    filter(core_action_main == !!ca) |>
    mutate(response = factor(response, levels = names(color_pal))) |>
    # create plot
    viz_fill_barchart(color_pal, 'core_action', '.n_response', 'response') +
      geom_text(ggplot2::aes(label = .n_response), position = position_stack(vjust = .5)) +
      labs(
        x = NULL,
        y = 'Number of responses',
        fill = NULL,
        title = plt_title
      )

  slide_title <- 'Teacher Observations'



}

ca_one <- pers_prep_viz_df |>
  filter(core_action_main == '2') |>
  mutate(response = factor(response, levels = names(ca_scale)))





pers_prep_tidy |>
  classroom_obs_add_tntpmetrics(grade_column = 'grade', subject_name = 'Math',
                                id_cols = c('.id', 'term')) |>
  tntpmetrics::make_metric(metric = "ipg")
