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

test_that("Survey responses properly convert to factors", {

  # scales to test
  test_scale <- c('Strongly Agree', 'Agree', 'Somewhat Agree')

  # function delivers the correct output when the input parameters are correct
  expect_equal(scale_to_factor(test_scale),
               factor(test_scale, levels = c(test_scale, "Somewhat Disagree", "Disagree", "Strongly Disagree")))

  # get a warning when the input data scale is not in the list of scales to use
  # and return the original data
  expect_message(scale_to_factor(c(test_scale, 'Not on scale')), regexp = "None of.*matched the scales.*")

})
