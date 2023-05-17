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
    email = c("pre_only@a.com", "pre_post@b.com"),
    question_stem = c("Pre stem", "Pre post stem"),
    response_option = c("Pre response option", "Pre post response_option"),
    response = "Great",
    term = "Pre training"
  )

  post_training <- data.frame(
    email = c("post_only@a.com", "pre_post@b.com"),
    question_stem = c("Post stem", "Pre post stem"),
    response_option = c("Post response option", "Pre post response_option"),
    response = "Good",
    term = "Post training"
  )

  df_actual <- g2g_teacher_combine_pre_post(pre_training, post_training)

  expected_in_survey <- c("Pre training", "Pre and Post Survey", "Post training", "Pre and Post Survey")

  expect_equal(df_actual$in_survey, expected_in_survey)

})
