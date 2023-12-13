test_that("Throws error when raw data has column names with multiple open or closed brackets", {

  error_one <- c("a_dsf[dfs]", "b_fds[fds[fds]")
  expect_error(g2g_test_full_question_brackets(error_one), "Question.*b_fds.*")

  error_two <- c("a_dsf[dfs", "b_fds[fdsfds]")
  expect_error(g2g_test_full_question_brackets(error_two), "Question.*a_dsf.*")

  error_three <- c("a_d[sf[df]s]", "b_fdsfdsfds]")
  expect_error(g2g_test_full_question_brackets(error_three), "Question.*a_d.*b_fdsf.*")

  no_error_one <- c("a_dsf[df]s", "b_fds[fdsfds]")
  expect_error(g2g_test_full_question_brackets(no_error_one), NA)

  no_error_two <- c("a_dsfdfs", "b_fdsfdsfds")
  expect_error(g2g_test_full_question_brackets(no_error_one), NA)

})

test_that("Survey responses properly convert to factors", {

  # scales to test
  test_scale <- c('Strongly Agree', 'Agree', 'Somewhat Agree', NA_character_)

  # function delivers the correct output when the input parameters are correct
  expect_equal(g2g_find_scale(test_scale) |> unname(),
               c(test_scale[!is.na(test_scale)], "Somewhat Disagree", "Disagree", "Strongly Disagree"))

  # get an error when the input data scale is not in the list of scales to use
  # and return the original data
  expect_error(g2g_find_scale(c(test_scale, 'Not on scale')), regexp = "None of.*matched the scales.*")

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

test_that("Ensure you get the right scale and color output", {

  proper_scales <- c('In All or Most Lessons', 'Often', 'Sometimes', 'Rarely', 'Never') |>
    # purrr::set_names(c("#1D4935", "#317D5C", "#C7C7C5", "#9E9E9C", "#747473"))
    purrr::set_names(c("#1D4935", "#317D5C", "#F1F1EE", "#C7C7C5", "#9E9E9C"))

  tested_scales <- g2g_scale_order('how_often')

  expect_equal(tested_scales, proper_scales)

  expect_error(g2g_scale_order('how_oftena'), regexp = "Scale names must be.*")

})

test_that("Correctly aggregate positive responses", {

  scales <- c('Strong 1', 'Strong 2', 'Weak 1', 'Weak 2')

  test_data1 <- tibble::tibble(
    response = scales,
    .percent = rep(.25, 4),
    question_stem = 'stem',
    response_option = 'response option',
    grouping = 'a'
  )

  test_data2 <- test_data1 |>
    dplyr::mutate(grouping = 'b')

  test_data <- dplyr::bind_rows(test_data1, test_data2) |>
    dplyr::bind_rows(
      test_data1 |>
        dplyr::filter(response %in% scales[3:4]) |>
        dplyr::mutate(grouping = 'c')
    ) |>
    dplyr::filter(!(grouping == 'a' & response == 'Strong 1'))

  aggregate_responses <- test_data |>
    g2g_aggregate_positive_responses(scales[1:2], 'grouping', only_keep_first_response = TRUE) |>
    dplyr::arrange(.data$grouping, .data$.scale_strength, .data$response) |>
    dplyr::pull(.data$.strong_response_percent)

  expected_results <- c(.25, NA_real_, NA_real_, .5, NA_real_, NA_real_, NA_real_, 0, NA_real_)

  expect_equal(aggregate_responses, expected_results)

})

test_that("Properly create data set for g2g_viz_likert_centered", {

  color_pal <- c(Good = "#317D5C", OK = "#C1C2C4", Bad = "#F2CF13")

  x_var <- 'percentage'
  y_var <- 'question'
  fill_var <- 'response'

  df <- data.frame(
    question = rep(c('Question 1', 'Question 2'), 3),
    response = rep(c('Good', 'OK', 'Bad'), each = 2) |> factor(levels = names(color_pal)),
    percentage = rep(.33333, 6)
  )

  # find positive, negative, and neutral scales
  number_of_scales <- length(color_pal)

  has_neutral <- !(number_of_scales %% 2 == 0)

  # clean the data and extract the positive, negative, and neutral scales
  data_and_scales <- g2g_helper_clean_viz_likert_centered(df, x_var, y_var, fill_var, color_pal, has_neutral, number_of_scales)

  expectation_df <- tibble::tribble(
    ~question, ~response, ~percentage, ~x_intercet, ~response_category, ~category_cumulative,
    "Question 1",    "Good",     0.33333,           0,         "Positive",              0.33333,
    "Question 2",    "Good",     0.33333,           0,         "Positive",              0.33333,
    "Question 1",      "OK",     0.33333,          NA,          "Neutral",              0.33333,
    "Question 2",      "OK",     0.33333,          NA,          "Neutral",              0.33333,
    "Question 1",     "Bad",    -0.33333,           0,         "Negative",             -0.33333,
    "Question 2",     "Bad",    -0.33333,           0,         "Negative",             -0.33333
  ) |>
    dplyr::mutate(response = factor(response,levels = c("Bad","Good","OK")))

  expect_equal(data_and_scales$df, expectation_df)
  expect_equal(data_and_scales$scales$positive, "Good")
  expect_equal(data_and_scales$scales$negative, "Bad")
  expect_equal(data_and_scales$scales$neutral, "OK")

})

test_that("Properly identify scales and add hex colors", {

  scales <- sample(c("Strongly Disagree", "Somewhat Disagree", "Agree"), 20, replace = TRUE)

  pos_scales <- g2g_find_scale(scales, reverse_coded = FALSE)
  neg_scales <- g2g_find_scale(scales, reverse_coded = TRUE)

  neg_scales

  expected_results_pos <- c(
    "#1D4935" = "Strongly Agree", "#317D5C" = "Agree", "#F1F1EE" = "Somewhat Agree",
    "#C7C7C5" = "Somewhat Disagree", "#9E9E9C" = "Disagree", "#747473" = "Strongly Disagree"
  )

  expected_results_neg <- c(
    "#F2CF13" = "Strongly Disagree", "#FDE57B" = "Disagree", "#F1F1EE" = "Somewhat Disagree",
    "#C7C7C5" = "Somewhat Agree", "#9E9E9C" = "Agree", "#747473" = "Strongly Agree"
  )

  expect_equal(pos_scales, expected_results_pos)
  expect_equal(neg_scales, expected_results_neg)

})


