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
#' @param question_option_re A regular expression that identifies the question option. Defaults to a regular
#'     expression that works with Google Forms `(" \\[.*\\]$")`
#'
#' @examples
#' teacher_pre_survey |>
#'    g2g_tidy_forms_survey(8:30, 3)
#'
#' @return A tibble in tidy format where each row represents a single person's response to a single question.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_tidy_forms_survey <- function(.data, question_columns, grouping_columns = NULL, question_option_re = " \\[.*\\]$") {

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
    dplyr::select(.data$.id, !!grouping_columns, !!question_columns) |>
    # convert to long form with the questions and responses as the columns we will make distinct
    tidyr::pivot_longer(cols = -c(.data$.id, dplyr::all_of(grouping_var_names)), names_to = 'full_question', values_to = 'response') |>
    dplyr::rename_with(~g2g_clean_column_names(.x))

  # ensure there is no more than one set of open and closed brackets. We cannot seperate the question stem
  # from the response option if there is more than one set.

  g2g_test_full_question_brackets(tidy_survey_results$full_question)

  tidy_survey_results <- tidy_survey_results |>
  #create seperate columns for the question stem and the response option
  dplyr::mutate(
    question_stem = stringr::str_remove(.data$full_question, !!question_option_re) |> stringr::str_trim(),
    response_option = stringr::str_extract(.data$full_question, !!question_option_re) |> stringr::str_trim(),
    response_option = stringr::str_remove_all(.data$response_option, "^\\[|\\]$")
  ) |>
  dplyr::select(.data$.id, g2g_clean_column_names(dplyr::all_of(grouping_var_names)), .data$question_stem, .data$response_option, .data$response)

  return(tidy_survey_results)

}

#' Create tntpmetrics high expectations column name from question.
#'
#' Calculating high expectations with the tntpmetrics package requires that each question have a distinct
#' and pre-specified column name. This function creates the column name based on the high expectations question.
#'
#' @param response_option Vector of high expectations questions. This will be the 'response_option' column
#'      if 'g2g_tidy_forms_survey()' is used.
#'
#' @importFrom rlang .data
#'
#' @keywords internal
g2g_teacher_survey_add_he_metric_colnames <- function(response_option) {

  string_did_not_match <- 'Did not match'

  # unique TNTP metrics values
  all_metrics <- c('exp_fairtomaster', 'exp_oneyearenough', 'exp_allstudents', 'exp_appropriate')

  tntp_metric_colnames <- dplyr::case_when(
    stringr::str_detect(response_option, "^It.s fair to expect.*end of the year.$") ~ all_metrics[1],
    stringr::str_detect(response_option, "^One year is enough.*master these standards.$") ~ all_metrics[2],
    stringr::str_detect(response_option, "^All students in my class.*end of the year.$") ~ all_metrics[3],
    stringr::str_detect(response_option, "^The standards are appropriate.*students in this class.$") ~ all_metrics[4],
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

  # throw an error if all four tntp metrics column names do not show up
  matched_unique_metrics <- tntp_metric_colnames[tntp_metric_colnames != string_did_not_match]

  missing_metrics <- setdiff(all_metrics, matched_unique_metrics)

  if (length(missing_metrics) != 0) {

    missing_metrics <- paste0(missing_metrics)

    stop(
      c("You failed to match any question to the following TNTP metrics: ", missing_metrics, ".\n",
        "Your data must contain all four questions to calculate high expectations.\n",
        "Please review the spelling in the columns containing the high expectations question text."),
      call. = FALSE
    )
  }

  # throw error if there are not exactly four distinct questions
  matched_response_options <- response_option[tntp_metric_colnames != string_did_not_match]
  num_unique_questions <- dplyr::n_distinct(matched_response_options, na.rm = TRUE)

  if (num_unique_questions != 4) {
    stop(
      c("You have ", num_unique_questions, " distinct high expectations questions that we found matches for.\n",
        "There should be four. Please recheck the wording of your high expectations questions."),
      call. = FALSE
    )
  }

  return(tntp_metric_colnames)

}

#' Create a data set that can be used by TNTP metrics to calculate high expectations.
#'
#' The \code{tntpmetrics} package can be used to calculate high expectations metrics based on the common measures.
#' This function converts the data from the pre and post teacher surveys into the format required by \code{tntpmetrics}.
#' The input data must be created with \code{g2g_tidy_forms_survey}, or maintain the same form.
#'
#' @param .data A data frame containing the survey responses, created with \code{g2g_tidy_forms_survey}.
#'
#' @returns A data frame that is in the correct format to be used by \code{tntpmetrics}.
#'
#' @examples
#' teacher_pre_survey |>
#'    g2g_tidy_forms_survey(8:30, 3) |>
#'    g2g_calc_high_expectations()
#'
#' @importFrom rlang .data
#'
#' @export
g2g_calc_high_expectations <- function(.data) {

  # identify the grouping variables from the input data frame
  id_cols <- setdiff(colnames(.data), c('question_stem', 'response_option', 'response'))

  # relatinship between text response and number required for tntpmetrics
  map_response_to_integer <- c(
    "Strongly Disagree" = 0, "Disagree" = 1, "Somewhat Disagree" = 2,
    "Somewhat Agree" = 3, "Agree" = 4, "Strongly Agree" = 5
  )

  # use this number if a text response fails to match
  # we will then look for match failures by looking for this number
  default_recode <- 99

  he <- .data |>
    # only keep high expectations questions, which have the stem showin in the text
    dplyr::filter(stringr::str_detect(.data$question_stem, "statements about your state standards")) |>
    dplyr::mutate(
      # add the column name required for tntpmetrics
      tntp_metrics = g2g_teacher_survey_add_he_metric_colnames(.data$response_option),
      # convert the text response to an integer
      response = dplyr::recode(.data$response, !!!map_response_to_integer, .default = default_recode, .missing = NA_real_)
    )

  # make sure all questions were matched and throw an error if they were not
  if (any(he$response == default_recode)) {

    stop(
      c("The 'response' column that is created by 'g2g_tidy_forms_survey()' produced odd responses after filtering for high expectations questions.\n",
        "Plese double check the 'response' column. It should contain the strings of the responses ('Agree', 'Disagree', ect.)"),
      call. = FALSE
    )

  }

  # convert to wide form where each question is a column
  he |>
    tidyr::pivot_wider(id_cols= dplyr::all_of(id_cols), names_from = 'tntp_metrics', values_from = 'response')

}

#' Calculate the percentage and number of responses for each question.
#'
#' The teacher survey contains questions that are largely on the 6 point Likert scale. This function
#' calculates the percentage and number of responses for each question and response option. The data,
#' \code{.data}, for this function should be created with \code{g2g_tidy_forms_survey}.
#'
#' @param .data A data frame containing the survey responses, created with \code{g2g_tidy_forms_survey}.
#' @param grouping_columns A string or vector with the column names of columns that you want to group
#'      the results by. This could include a column containing school names or demographic information.
#' @param add_n One of 'none', '.percent_pretty', 'response_option Adds the number of responses. With '.percent_pretty' it adds
#'      adds the number of responses for a given question and scale to percentage number provided in
#'      \code{.percent_pretty} (75% (n=10)). With 'response_option' the number of responses for the question are
#'      added to \code{response_option}.
#'
#' @returns A data frame that contains the percentage and number of responses for each response option
#'      and question. Four columns are added, all starting with a period (.):
#'
#'  - \code{.n_responses}: The number of responses for a given question and response option.
#'  - \code{.n_total}: The total number of responses for the question.
#'  - \code{.percent}: The percentage respondents that provided the given response for the question.
#'  - \code{.percent_pretty}: A string of the percentage with the percentage sign added and the
#'        number of respondents added if \code{add_n = '.percent_pretty'}.
#'
#' @examples
#' teacher_pre_survey |>
#'    g2g_tidy_forms_survey(8:30, 3) |>
#'    g2g_forms_survey_calc_percentages()
#'
#' @importFrom rlang .data
#'
#' @export
g2g_forms_survey_calc_percentages <- function(.data, grouping_columns = NULL, add_n = 'none') {

  # column names from g2g_tidy_forms_survey function that contain question stems, questions, and response
  questions_responses <- c('question_stem', 'response_option', 'response')

  add_n_options <- c('none', 'response_option', '.percent_pretty')

  if (!add_n %in% add_n_options) stop(paste0("`add_n` must be one of '", paste0(add_n_options, collapse= "', "), "'"))

  df <- .data |>
    tidyr::drop_na(.data$response) |>
    # calculate the number of responses for each response option
    dplyr::group_by_at(c(grouping_columns, questions_responses)) |>
    dplyr::count(name = '.n_response') |>
    # calculate the number of responses for each question
    dplyr::group_by_at(c(grouping_columns, questions_responses[-3])) |>
    dplyr::mutate(.n_question = sum(.data$.n_response)) |>
    dplyr::ungroup() |>
    # calculate percentages
    dplyr::mutate(
      # calculate percentages
      .percent = .data$.n_response / .data$.n_question,
      # make a column that is text of the percent for plotting
      .percent_pretty = scales::percent(.data$.percent, accuracy = 1)
    )

  # add n as well, if needed
  if (add_n == '.percent_pretty') {
    df$.percent_pretty <- glue::glue("{df$.percent_pretty}\n(n={df$.n_response})")
  } else if (add_n == 'response_option') {
    df$response_option <- glue::glue("{df$response_option} (n={df$.n_question})")
  }

  return(df)


}
