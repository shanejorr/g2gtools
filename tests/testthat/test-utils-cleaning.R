test_that("Throws error when raw data has column names with multiple open or closed brackets", {

  error_one <- c("a_dsf[dfs]", "b_fds[fds[fds]")
  expect_error(test_full_question_brackets(error_one), "Question.*b_fds.*")

  error_two <- c("a_dsf[dfs", "b_fds[fdsfds]")
  expect_error(test_full_question_brackets(error_two), "Question.*a_dsf.*")

  error_three <- c("a_d[sf[df]s]", "b_fdsfdsfds]")
  expect_error(test_full_question_brackets(error_three), "Question.*a_d.*b_fdsf.*")

  no_error_one <- c("a_dsf[df]s", "b_fds[fdsfds]")
  expect_error(test_full_question_brackets(no_error_one), NA)

  no_error_two <- c("a_dsfdfs", "b_fdsfdsfds")
  expect_error(test_full_question_brackets(no_error_one), NA)

})

test_that("Survey responses properly convert to factors", {

  # scales to test
  test_scale <- c('Strongly Agree', 'Agree', 'Somewhat Agree', NA_character_)

  # function delivers the correct output when the input parameters are correct
  expect_equal(find_scale(test_scale, TRUE) |> unname(),
               c(test_scale[!is.na(test_scale)], "Somewhat Disagree", "Disagree", "Strongly Disagree"))

  # get an error when the input data scale is not in the list of scales to use
  # and return the original data
  expect_error(find_scale(c(test_scale, 'Not on scale'), TRUE), regexp = "None of.*matched the scales.*")

})

test_that("Identify observations in pre and post datasets", {

  pre_post_data <- data.frame(
    id = c('a', 'a', 'b', 'b', 'c', 'd'),
    term = c('pre', 'post', 'pre', 'post', 'pre', 'post')
  )

  # test g2g_compare_names()
  compare_pre_post <- g2g_compare_names(pre_post_data, 'id', 'term')

  expect_equal(compare_pre_post$id, pre_post_data$id)

  expect_equal(compare_pre_post$.group_id, c(1,1,2,2,3,4))

  # test g2g_id_pre_post()
  in_pre_post <- g2g_id_pre_post(pre_post_data, 'id', 'term')

  expect_equal(in_pre_post, c('a', 'b'))

})
