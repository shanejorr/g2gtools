test_that("Determine major and minor core actions for classroom observations.", {

  test_obs <- data.frame(
    question_stem = c(
      "test", 'Core Action 1 Overall:', 'Core Action 1 Note:',
      "Are all students engaged in the work of the lesson from start to finish?",
      "Overall, did this lesson reflect the demands of the standards and/or the instructional shifts the standards require?",
      "minor domain", "minor domain"
    ),
    response_option = c('test', rep(NA_character_, 4), "asdf (1a)", "dsaf (2b)")
  )

  class_obs <- g2g_classroom_obs_add_ca(test_obs)

  ca_main <- c(NA_character_, '1', NA_character_, 'Culture of Learning', 'Demands of the Standards', '1', '2')
  ca_minor <- c(NA_character_, 'Overall', NA_character_, 'Culture of Learning', 'Demands of the Standards', 'a', 'b')

  expect_equal(class_obs$core_action_main, ca_main)
  expect_equal(class_obs$core_action_minor, ca_minor)

  expect_error(g2g_classroom_obs_add_ca(test_obs[1]), regexp = ".*You are missing one.*")


})

test_that("Ensure we properly convert observations to TNTP Metrics dataset.", {

  n_rows <- 12

  obs_data <- data.frame(
    .id = 1,
    grade = rep(5, n_rows),
    core_action_main = c('1', '1', '1', '1', '2', '2', '3', '3', 'Culture of Learning', NA_character_, 'Reading Foundational Skills', 'Reading Foundational Skills'),
    core_action_minor = c('a', 'b', 'c', 'Overall', 'a', 'Overall', 'a', 'Overall', 'Culture of Learning', NA_character_, 'Overall', '1'),
    response = c('Yes', 'Not Yet', 'Yes', 'Not Yet',
                 'Mostly', 'Somewhat', 'Not Yet', 'Not Yet', 'Yes', 'Maybe something',
                 'Yes', 'Mostly')
  )

  obs_metrics <- g2g_classroom_obs_add_tntpmetrics(obs_data, grade_column = 'grade', subject_name = 'Math',
                                               id_cols = '.id')

  # ensure column names are equal
  expect_equal(
    colnames(obs_metrics),
    c('.id', 'grade_level', 'form', 'ca1_a', 'ca1_b', 'ca1_c', 'ca2_overall', 'ca3_overall', 'col', 'rfs_overall')
  )

  # ensure data values are equal
  actual_vals <- obs_metrics[1, ] |> unlist() |>  unname()

  expect_equal(
    obs_metrics[1, ] |> unlist() |>  unname(),
    c("1", "5", "Math","1","0","1","2","1","4", "4")
  )

})

test_that("Ensure we properly convert observations to TNTP Metrics dataset.", {

  n_rows <- 12

  obs_data <- data.frame(
    .id = 1,
    grade = rep(5, n_rows),
    core_action_main = c('1', '1', '1', '1', '2', '2', '3', '3', 'Culture of Learning', NA_character_, 'Reading Foundational Skills', 'Reading Foundational Skills'),
    core_action_minor = c('a', 'b', 'c', 'Overall', 'a', 'Overall', 'a', 'Overall', 'Culture of Learning', NA_character_, 'Overall', '1'),
    response = c('Yes', 'Not Yet', 'Yes', 'Not Yet',
                 'Mostly', 'Somewhat', 'Not Yet', 'Not Yet', 'Yes', 'Maybe something',
                 'Yes', 'Mostly')
  )

  obs_metrics <- g2g_classroom_obs_add_tntpmetrics(obs_data, grade_column = 'grade', subject_name = 'Math',
                                                   id_cols = '.id')

  # ensure column names are equal
  expect_equal(
    colnames(obs_metrics),
    c('.id', 'grade_level', 'form', 'ca1_a', 'ca1_b', 'ca1_c', 'ca2_overall', 'ca3_overall', 'col', 'rfs_overall')
  )

  # ensure data values are equal
  actual_vals <- obs_metrics[1, ] |> unlist() |>  unname()

  expect_equal(
    obs_metrics[1, ] |> unlist() |>  unname(),
    c("1", "5", "Math","1","0","1","2","1","4", "4")
  )

})

test_that("Properly calculate average IPG scores.", {

  ipg_scores <- g2g_tidy_forms_survey(classroom_observations_math, 8:ncol(classroom_observations_math), c(3,6)) |>
    g2g_classroom_obs_add_ca() |>
    g2g_calc_avg_ipg(grade_column = 'grade', subject_name = c('Math'), grouping_terms = 'when_did_the_observation_occur') |>
    dplyr::mutate(across(where(is.numeric), ~round(.x, 2)))

  correct_results <- tibble::tibble(
    when_did_the_observation_occur = c('Post Training', 'Pre Training'),
    cm_ipg = c(1.46, 1.46),
    cm_binary_ipg = c(0.29, 0.29)
  )

  expect_equal(ipg_scores, correct_results)

})
