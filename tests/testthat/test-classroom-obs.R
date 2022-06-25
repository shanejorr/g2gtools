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

  class_obs <- classroom_obs_add_ca(test_obs)

  ca_main <- c(NA_character_, '1', NA_character_, 'Culture of Learning', 'Demands of the Standards', '1', '2')
  ca_minor <- c(NA_character_, 'Overall', NA_character_, 'Culture of Learning', 'Demands of the Standards', 'a', 'b')

  expect_equal(class_obs$core_action_main, ca_main)
  expect_equal(class_obs$core_action_minor, ca_minor)

  expect_error(classroom_obs_add_ca(test_obs[1]), regexp = ".*You are missing one.*")


})
