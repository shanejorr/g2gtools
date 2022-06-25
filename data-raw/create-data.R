# pre and post training teacher surveys -------------------------

# an example of teacher pre survey data ------------------------------------

pre_survey_url <- "https://docs.google.com/spreadsheets/d/1-sjUC4BWH-Ad-TQVK2dACb3WAzlnJRsf9Yxbm2v2KKo/edit#gid=35865763"

teacher_pre_survey <- googlesheets4::read_sheet(pre_survey_url)

usethis::use_data(teacher_pre_survey, overwrite = TRUE)

# an example of teacher post training survey data ------------------------

# classroom observations -------------------

classroom_observations_math <- readr::read_csv('data-raw/classroom_observations.csv')

usethis::use_data(classroom_observations_math, overwrite = TRUE)
