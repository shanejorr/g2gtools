#' Clean up column names from data sets imported from Google Forms surveys.
#'
#' @param column_names A string or vector of strings representing the column names we want to clean up.
#'
#' @keywords internal
clean_column_names <- function(column_names) {

  column_names |>
    # replace spaces with underscores
    stringr::str_replace_all(" ", "_") |>
    # remove periods unless the period is the first character
    stringr::str_remove_all("(?<=.)\\.") |>
    # remove question marks
    stringr::str_remove_all("[?]") |>
    # convert to lower
    stringr::str_to_lower()

}

#' Ensure full question column only contains one set of open and closed brackets
#'
#' 'tidy_teacher_survey' identifies the question stem and response options by looking for open and closed
#'      brackets. The text within the brackets is the response option. To work properly,
#'      there can only be one set of open and closed brackets. This function tests to make sure that
#'      there is only one set of brackets, if there are any at all.
#'
#' @param full_question_column Vector representing the column to test.
#'
#' @keywords internal
test_full_question_brackets <- function(full_question_column) {

  # TRUE means a passed test and a problem
  number_brackets_open <- stringr::str_count(full_question_column, "\\[")
  number_brackets_closed <- stringr::str_count(full_question_column, "\\]")
  open_equals_closed_brackets <- number_brackets_open == number_brackets_closed

  # vector showing if any of the three tests failed for a row
  pass_all_tests <- number_brackets_open <= 1 & number_brackets_closed  <= 1 & open_equals_closed_brackets

  # throw an error if any tests did not pass for any rows
  if (any(!pass_all_tests)) {

    row_failed_test <- which(!pass_all_tests)

    failed_questions <- unique(full_question_column[row_failed_test])
    failed_questions <- paste0("     '", failed_questions, "'")

    # error message
    error_text <- c(
      "Question stems are seperated from response options by identifying open and closed bracket (`[` and `]`) in column headers.",
      "The following column names in the raw data have more than one open bracket or more than one closed bracket:",
      failed_questions,
      "Because of this, we cannot properly seperate the question stems from the response option.",
      "Please ensure that the column headers in your raw data only contain open and closed brackets around the response options."
    ) |>
      paste0(collapse = "\n")

    stop(error_text, .call = FALSE)

  }

  return(NULL)

}

# full_question_column <- c('sd[fa]', 'sdfa[fds]dfs')
#
# test_full_question_brackets(full_question_column)
