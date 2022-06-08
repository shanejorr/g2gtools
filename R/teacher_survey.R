#' Transform the pre and post training teacher survey into tidy format (long form)
#'
#' The teacher surveys (pre and post training) are conducted in Google Forms. As a result, their
#' results are in Google Sheets. In raw form, each question is a column. The function converst the surveys
#' into a tidy format where each row is a user's response to a specific question.
#'
#' @param .data The name of an R data frame containing raw teacher survey data.
#' @param grouping_columns An integer or vector of integers representing column numbers of the columns
#'      that provide distinguishing characteristics of respondents that you might want to group on.
#'      This could be a teacher's email address, subjects taught, or demographic information about the teacher.
#' @param question_columns An integer or vector of integers representing column numbers for columns contain question answers
#'     that we want to include in the analysis.
#'
#' @return A tibble in tidy format where each row represents a single person's response to a single question.
#'
#' @export
tidy_teacher_survey <- function(.data, grouping_columns, question_columns) {

  # regular expression that identifies the response options
  question_stem_re <- " \\[.*\\]$"

  grouping_var_names <- colnames(.data)[grouping_columns]

  # print the column names we are grouping by, so user can ensure the integers that were inputted
  # align with what was expected
  message(paste0("`grouping_variables` will use the following columns:\n     ", paste0(grouping_var_names, collapse = "\n     ")))

  # column names of columns that are not in the final data
  # these are columns that are neither in grouping_columns nor question_columns
  colnames_remove <- colnames(.data)[-c(grouping_columns, question_columns)]

  # print message contains column names of columns we will not use, so user can check agains expectations
  message(paste0("\nThe following columns will not be included in the data:\n     ", paste0(colnames_remove, collapse = "\n     ")))

  message("\nPlease ensure this is correct.\nThis is not an error. Just a manual check :)")

  tidy_survey_results <- .data |>
    # add a column that uniquely identifies each respondent
    # needed so when we convert to long form we can still identify individual respondents
    dplyr::mutate(.id = dplyr::row_number()) |>
    dplyr::select(.id, !!grouping_columns, !!question_columns) |>
    # convert to long form with the questions and responses as the columns we will make distinct
    tidyr::pivot_longer(cols = -c(.id, dplyr::all_of(grouping_var_names)), names_to = 'full_question', values_to = 'response') |>
    dplyr::rename_with(~clean_column_names(.x))

  # ensure there is no more than one set of open and closed brackets. We cannot seperate the question stem
  # from the response option if there is more than one set.

  test_full_question_brackets(tidy_survey_results$full_question)

  tidy_survey_results <- tidy_survey_results |>
  #create seperate columns for the question stem and the response option
  dplyr::mutate(
    question_stem = stringr::str_remove(full_question, !!question_stem_re) |> stringr::str_trim(),
    response_option = stringr::str_extract(full_question, !!question_stem_re) |> stringr::str_trim(),
    response_option = stringr::str_remove_all(response_option, "^\\[|\\]$")
  ) |>
  dplyr::select(.id, clean_column_names(dplyr::all_of(grouping_var_names)), question_stem, response_option, response)

  return(tidy_survey_results)

}

#' Add question tntpmetrics column names to survey questions
#'
#' @keywords internal
teacher_survey_add_metric_colnames <- function(response_option) {

  string_did_not_match <- 'Did not match'

  # unique TNTP metrics values
  all_metrics <- c('exp_fairtomaster', 'exp_oneyearenough', 'exp_allstudents', 'exp_appropriate')

  tntp_metric_colnames <- case_when(
    str_detect(response_option, "^It.s fair to expect.*end of the year.$") ~ all_metrics[1],
    str_detect(response_option, "^One year is enough.*master these standards.$") ~ all_metrics[2],
    str_detect(response_option, "^All students in my class.*end of the year.$") ~ all_metrics[3],
    str_detect(response_option, "^The standards are appropriate.*students in this class.$") ~ all_metrics[4],
    TRUE ~ string_did_not_match
  )

  # determine if any rows did not match and deliver warning if they did not
  num_didnt_match <- any(tntp_metric_colnames == string_did_not_match)

  if (num_didnt_match) {
    warning(
      c("Some of your rows did not match with a tntpmetrics value. The non-matches return '", string_did_not_match, "'.\n",
        "If you expected all your rows to match, please examined the returned values for '", string_did_not_match, "'.\n") |>
      paste0(collapse = ""),
      call. = FALSE
    )
  }

  # throw error if there are not exactly four distinct questions
  matched_response_options <- response_option[tntp_metric_colnames != string_did_not_match]
  num_unique_questions <- dplyr::n_distinct(matched_response_options, na.rm = TRUE)

  if (num_unique_questions != 4) {
    stop(
      c("You have ", num_unique_questions, " distinct high expectations questions that we found matches for.\n",
        "There should only be four. Please recheck the wording of your high expectations questions."),
      call. = FALSE
    )
  }

  # throw an error if all four tntp metrics column names do not show up
  matched_unique_metrics <- tntp_metric_colnames[tntp_metric_colnames != string_did_not_match]

  missing_metrics <- setdiff(all_metrics, matched_unique_metrics)

  if (length(missing_metrics) != 0) {

    missing_metrics <- paste0(missing_metrics)

    stop(
      c("You failed to match any question to the following TNTP metrics: ", missing_metrics, ".\n",
        "Please review the spelling in the columns containing the high expectations question text."),
      call. = FALSE
    )
  }
  return(tntp_metric_colnames)

}
