#' Transform the pre and psot training teacher survey into tody format (long form)
#'
#' The teacher surveys (pre and post training) are conducted in Google Forms. As a result, their
#' results are in Google Sheets. In raw form, each question is a column. The function converst the surveys
#' into a tidy format where each row is a user's response to a specific question.
#'
#' @param survey_url A string. The url to the Google Sheet containing the teacher survey.
#' @param grouping_columns string or vector of strings.
#' @param question_columns A vector of integer positions representing column numbers ffor columns contain question answers
#'     that we want to include in the analysis.
#'
#' @return A tibble in tidy format where each row represents a single person's response to a single question.
teacher_survey_pre_url <- "https://docs.google.com/spreadsheets/d/1-sjUC4BWH-Ad-TQVK2dACb3WAzlnJRsf9Yxbm2v2KKo/edit#gid=35865763"

.data <- googlesheets4::read_sheet(teacher_survey_pre_url)

grouping_columns <- c(3)
question_columns <- 8:30

##########

grouping_var_names <- colnames(.data)[grouping_variables] |>
  paste0(collapse = "\n     ")

message(paste0("`grouping_variables` will use the following columns:\n     ", grouping_var_names))

message

.data |>
  dplyr::mutate(.id = dplyr::row_number()) |>
  dplyr::select(id, !!grouping_columns, !!question_columns) |>
  select(.id, grouping_variables, matches(question_stem))  |>
  pivot_longer(cols = matches(question_stem), names_to = 'full_question', values_to = 'response') |>
  mutate(
    question_stem = str_extract(full_question, !!question_stem),
    response_option = str_remove(full_question, question_stem) |> str_trim(),
    response_option = str_remove_all(response_option, "^\\[|\\]$")
  ) |>
  select(.id, matches(group_cols_re), question_stem, response_option, response)
