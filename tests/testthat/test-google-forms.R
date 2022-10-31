test_that("Raw google forms data properly converts to tidy dataset", {

  grouping_columns <- c(3)
  question_columns <- 8:30

  # convert teacher pre survey to long / tidy form
  tidy_survey <- g2g_tidy_forms_survey(teacher_pre_survey, question_columns, grouping_columns)

  # total rows / unique respondents in raw data
  n_rows <- nrow(teacher_pre_survey)

  # total unique respondents in cleaned data
  n_unique_respondents <- dplyr::n_distinct(tidy_survey$.id, na.rm = TRUE)

  # total unique emails (grouping variable), should be same as total unique respondents in our test data set
  n_unique_emails <- dplyr::n_distinct(tidy_survey$what_is_your_school_email_address, na.rm = TRUE)

  # ensure number of unique responses is correct
  testthat::expect_equal(n_unique_respondents, n_rows)
  testthat::expect_equal(n_unique_emails, n_rows)

  # Ensure the number of responses for each response column is correct for a single column
  single_response_option <- tidy_survey |>
    dplyr::filter(stringr::str_detect(response_option, "^Leaders at my school")) |>
    dplyr::count(response)

  agree_responses <- single_response_option$n[single_response_option$response == 'Agree']
  disagree_responses <- single_response_option$n[single_response_option$response == 'Disagree']
  somewhat_agree_responses <- single_response_option$n[single_response_option$response == 'Somewhat Agree']
  strongly_agree_responses <- single_response_option$n[single_response_option$response == 'Strongly Agree']

  testthat::expect_equal(agree_responses, 10)
  testthat::expect_equal(disagree_responses, 1)
  testthat::expect_equal(somewhat_agree_responses, 3)
  testthat::expect_equal(strongly_agree_responses, 3)

})

test_that("Properly match high expectation questions to their TNTP metrics column names.", {

  grouping_columns <- c(3)
  question_columns <- 8:30

  he_questions <- teacher_pre_survey  |>
    g2g_tidy_forms_survey(question_columns, grouping_columns) |>
    dplyr::filter(stringr::str_detect(question_stem, "statements about your state standards")) |>
    dplyr::pull(response_option)

  # test that everything works without an error
  metric_responses <- g2g_teacher_survey_add_he_metric_colnames(he_questions)

  testthat::expect_equal(metric_responses[1], "exp_fairtomaster")
  testthat::expect_equal(metric_responses[2], "exp_oneyearenough")
  testthat::expect_equal(metric_responses[3], "exp_allstudents")
  testthat::expect_equal(metric_responses[4], "exp_appropriate")

  # test that there is an error when there are not four metric responses
  three_metric_responses <- he_questions[metric_responses != "exp_fairtomaster"]

  testthat::expect_error(g2g_teacher_survey_add_he_metric_colnames(three_metric_responses), "You failed.*exp_fairtomaster.*")

  # test that there is an error when there are too many questions
  additional_question <- c(he_questions, "One year is enough sdaffdsafa master these standards.")

  testthat::expect_error(g2g_teacher_survey_add_he_metric_colnames(additional_question), "You have 5.*")

})

test_that("Create high expectations data set for tntpmetrics.", {

  grouping_columns <- c(3)
  question_columns <- 8:30

  # extract high expectations questions
  he_questions <- teacher_pre_survey |>
    g2g_tidy_forms_survey(question_columns, grouping_columns) |>
    dplyr::filter(stringr::str_detect(.data$question_stem, "statements about your state standards"))

  # create tntpmetrics olumns and extract one respondent to test
  test_metrics <- he_questions |>
    g2g_calc_high_expectations() |>
    dplyr::filter(.data$.id == 10)

  testthat::expect_equal(test_metrics$exp_fairtomaster, 2)
  testthat::expect_equal(test_metrics$exp_oneyearenough, 2)
  testthat::expect_equal(test_metrics$exp_allstudents, 0)
  testthat::expect_equal(test_metrics$exp_appropriate, 1)

  # test that we get an error when a text response (Agree') does not match an integer value
  bad_response_value <- he_questions |>
    dplyr::mutate(response = stringr::str_replace(.data$response, "^Agree$", "Agreee"))

  testthat::expect_error(g2g_calc_high_expectations(bad_response_value), "^The.*after filtering.*")

})

test_that("Ensure HE scores are calculated correctly.", {

  teacher_survey <- teacher_pre_survey |>
     g2g_tidy_forms_survey(8:30, 3)

  he_scores <- teacher_survey |>
    g2g_calc_high_expectations_averages()

  testthat::expect_equal(round(he_scores$cm_expectations[1], 2), 12.82)
  testthat::expect_equal(round(he_scores$cm_binary_expectations[1], 2), 0.76)
  testthat::expect_equal(nrow(he_scores), 1)

  he_scores_groups <- teacher_survey |>
    dplyr::mutate(term = 'a') |>
    dplyr::bind_rows(teacher_survey |> dplyr::mutate(term = 'b')) |>
    g2g_calc_high_expectations_averages(grouping_term = 'term')

  testthat::expect_equal(round(he_scores_groups$cm_expectations, 2), rep(12.82, 2))
  testthat::expect_equal(round(he_scores_groups$cm_binary_expectations, 2), rep(0.76, 2))
  testthat::expect_equal(nrow(he_scores_groups), 2)

})

test_that("Ensure instructional practices scores are properly calculated.", {

  inst_scales <- g2g_list_of_scales()[['how_often']]

  df <- data.frame(
    term = rep(c('Pre', 'Post'), each = length(inst_scales)),
    responses = rep(inst_scales, 2)
  )

  inst_prac_score <- g2g_calc_inst_practices(df, 'responses', 'term')

  testthat::expect_equal(inst_prac_score$inst_practice_score, c(3, 3))

  df_error <- df |>
    dplyr::bind_rows(data.frame(term = 'pre', responses = 'test'))

  testthat::expect_error(g2g_calc_inst_practices(df_error, 'responses', 'term'), regexp = "One of your responses")

})

test_that("Ensure percentages are properly calculated for surveys.", {

  question_columns <- 8:30

  col_to_test <- 'I get feedback on the content of my lesson'

  expected_responses <- tibble::tibble(
    response = c('Agree', 'Disagree', 'Somewhat Agree', 'Somewhat Disagree', 'Strongly Agree'),
    .n_response = c(11, 1, 2, 1, 2)
  )

  total_num_responses <- 17

  pre_survey <- teacher_pre_survey %>%
    g2g_tidy_forms_survey(question_columns) |>
    g2g_forms_survey_calc_percentages() |>
    dplyr::filter(stringr::str_detect(response_option, col_to_test)) |>
    dplyr::select(response, .n_response, .n_question, .percent) |>
    dplyr::arrange(response)

  testthat::expect_equal(sum(pre_survey$.percent), 1)
  testthat::expect_equal(sum(pre_survey$.n_response), total_num_responses)
  testthat::expect_equal(max(pre_survey$.n_question), total_num_responses)
  testthat::expect_equal(dplyr::select(pre_survey, response, .n_response), expected_responses)

})
