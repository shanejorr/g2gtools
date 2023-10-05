test_that("Reverse code question responses.", {

  reverse_code_question <- "Having repeated practice with reading strategies significantly improves things for the reader"

  df <- data.frame(
    question_stem = c("reverse coded", "not reverse coded"),
    response_option = c(reverse_code_question, "Note a reverse coded question")
  )

  df_actual <- g2g_teacher_reverse_coded(df)

  expect_equal(df_actual$reverse_coded, c(TRUE, FALSE))
  expect_equal(df_actual$question_stem, c("reverse coded*", "not reverse coded"))


})

test_that("Combine pre and post-training data sets.", {

  pre_training <- data.frame(
    teacher_survey_email = c("pre_only@a.com", "pre_post@b.com", "pre_post_long@c.com", 'pre_only_email@a.com'),
    question_stem = c("Pre stem", "Pre post stem", "Pre post long stem", "Pre post long stem"),
    response_option = c("Pre response option", "Pre post response option", 'pre post long response option', 'pre post long response option'),
    response = "Great",
    term = "Pre training"
  )

  post_training <- data.frame(
    teacher_survey_email = c("post_only@a.com", "pre_post@b.com", "pre_post_long@c.com", "pre_post_long@c.com", "post_long@c.com", "post_long@c.com", 'long@a.com', 'long_only_email@q.com'),
    question_stem = c("Post stem", "Pre post stem", "Pre post long stem", "Pre post long stem", "Post long stem", "Post long stem", "Long stem", "Pre post long stem"),
    response_option = c(
      "Post response option", "Pre post response option", 'pre post long response option', 'pre post long response option',
      'Post long response option', 'Post long response option', 'Long response option', 'pre post long response option'
    ),
    response = "Good",
    term = c("Post training", "Post training", 'Post training', 'Long term', 'Post training', 'Long term', 'Long term', 'Long term')
  )

  df_actual <- g2g_teacher_combine_pre_post(pre_training, post_training) |>
    g2g_number_times_teacher_answered()

  expected_in_survey <- c(
    # pre-training
    "Pre training","Pre training | Post training","Pre training | Post training | Long term","Pre training | Post training | Long term",
    "Post training","Pre training | Post training","Pre training | Post training | Long term",
    "Pre training | Post training | Long term","Post training | Long term",
    "Post training | Long term","Long term","Pre training | Post training | Long term"
  )

  expected_n_answers <- c(1,2,3,1,1,2,3,3,2,2,1,1)

  expect_equal(df_actual$in_survey, expected_in_survey)
  expect_equal(df_actual$n_question_answers, expected_n_answers)

})
