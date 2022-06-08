# Test cleaning functions ------------------------------
test_that("Raw data properly converts to tidy dataset", {

  grouping_columns <- c(3)
  question_columns <- 8:30

  # convert teacher pre survey to long / tidy form
  tidy_survey <- tidy_teacher_survey(teacher_pre_survey, grouping_columns, question_columns)

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

test_that("Throws error when raw data has column names with multiple open or closed brackets", {

  error_one <- c("a_dsf[dfs]", "b_fds[fds[fds]")
  testthat::expect_error(test_full_question_brackets(error_one), "Question.*b_fds.*")

  error_two <- c("a_dsf[dfs", "b_fds[fdsfds]")
  testthat::expect_error(test_full_question_brackets(error_two), "Question.*a_dsf.*")

  error_three <- c("a_d[sf[df]s]", "b_fdsfdsfds]")
  testthat::expect_error(test_full_question_brackets(error_three), "Question.*a_d.*b_fdsf.*")

  no_error_one <- c("a_dsf[df]s", "b_fds[fdsfds]")
  testthat::expect_error(test_full_question_brackets(no_error_one), NA)

  no_error_two <- c("a_dsfdfs", "b_fdsfdsfds")
  testthat::expect_error(test_full_question_brackets(no_error_one), NA)

})

test_that("Properly match high expectation questions to their TNTP metrics column names.", {

  grouping_columns <- c(3)
  question_columns <- 8:30

  he_questions <- teacher_pre_survey  |>
    tidy_teacher_survey(grouping_columns, question_columns) |>
    dplyr::filter(stringr::str_detect(question_stem, "statements about your state standards")) |>
    dplyr::pull(response_option)

  # test that everything works without an error
  metric_responses <- teacher_survey_add_he_metric_colnames(he_questions)

  testthat::expect_equal(metric_responses[1], "exp_fairtomaster")
  testthat::expect_equal(metric_responses[2], "exp_oneyearenough")
  testthat::expect_equal(metric_responses[3], "exp_allstudents")
  testthat::expect_equal(metric_responses[4], "exp_appropriate")

  # test that there is an error when there are not four metric responses
  three_metric_responses <- he_questions[metric_responses != "exp_fairtomaster"]

  testthat::expect_error(teacher_survey_add_he_metric_colnames(three_metric_responses), "You failed.*exp_fairtomaster.*")

  # test that there is an error when there are too many questions
  additional_question <- c(he_questions, "One year is enough sdaffdsafa master these standards.")

  testthat::expect_error(teacher_survey_add_he_metric_colnames(additional_question), "You have 5.*")

})

test_that("Create high expectations data set for tntpmetrics.", {

  grouping_columns <- c(3)
  question_columns <- 8:30

  # extract high expectations questions
  he_questions <- teacher_pre_survey |>
    tidy_teacher_survey(grouping_columns, question_columns) |>
    dplyr::filter(stringr::str_detect(.data$question_stem, "statements about your state standards"))

  # create tntpmetrics olumns and extract one respondent to test
  test_metrics <- he_questions |>
    teacher_survey_calculate_high_expectations() |>
    dplyr::filter(.data$.id == 10)

  testthat::expect_equal(test_metrics$exp_fairtomaster, 2)
  testthat::expect_equal(test_metrics$exp_oneyearenough, 2)
  testthat::expect_equal(test_metrics$exp_allstudents, 0)
  testthat::expect_equal(test_metrics$exp_appropriate, 1)

  # test that we get an error when a text response (Agree') does not match an integer value
  bad_response_value <- he_questions |>
    dplyr::mutate(response = stringr::str_replace(.data$response, "^Agree$", "Agreee"))

  testthat::expect_error(teacher_survey_calculate_high_expectations(bad_response_value), "^The.*after filtering.*")

})
