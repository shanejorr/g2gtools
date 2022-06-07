# Test cleaning functions ------------------------------
test_that("Raw data properly converts to tidy dataset", {

  grouping_columns <- c(3)
  question_columns <- 8:30

  # convert teacher pre survey to long / tidy form
  tidy_survey <- tidy_teacher_survey(teacher_pre_survey, grouping_columns, question_columns)

  # total rows / unique respondents in raw data
  n_rows <- nrow(teacher_pre_survey)

  # total unique respondents in cleaned data
  n_unique_respondents <- length(unique(tidy_survey$.id))

  # total unique emails (grouping variable), should be same as total unique respondents in our test data set
  n_unique_emails <- length(unique(tidy_survey$what_is_your_school_email_address))

  # ensure number of unique responses is correct
  testthat::expect_equal(n_unique_respondents, n_rows)
  testthat::expect_equal(n_unique_emails, n_rows)

  # Ensure the number of responses for each response column is correct for a single column
  single_response_option <- tidy_survey %>%
    dplyr::filter(stringr::str_detect(response_option, "^Leaders at my school")) %>%
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
