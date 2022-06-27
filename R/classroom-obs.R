#' Add Core Action (major and minor) identifiers
#'
#' Classroom observations are based on the IPG and contain components called core actions.
#' There are major core actions, which are numbers, and minor core actions, which are letters.
#' The IPG also contains Culture of Learning (CoL) and summary ratings. This function add two columns
#' to the data set of classroom observations identifying the major and minor core action.
#'
#' The column \code{core_action_main} contains the major core action and \code{core_action_minor}
#' contains the minor core action.
#'
#' The input data set should contain the classroom observations survey that has been placed into long
#' form with the function \code{tidy_forms_survey}. It must contain the \code{response_option} and
#' \code{question_stem} columns.
#'
#' @param .data The name of an R data frame containing classroom observations and placed into long form
#'      with \code{tidy_forms_survey}.
#'
#' @examples
#' tidy_forms_survey(classroom_observations_math, 8:ncol(classroom_observations_math), c(3,4,5)) |>
#'  classroom_obs_add_ca()
#'
#' @return A data frame containing all the original columns and rows, along with two additional columns:
#'      \code{core_action_main} and \code{core_action_minor}.
#'
#' @importFrom rlang .data
#'
#' @export
classroom_obs_add_ca <- function(.data) {

  # ensure required columns are present
  col_names <- colnames(.data)

  req_cols <- c('response_option', 'question_stem')

  all_req_cols_present <- length(setdiff(req_cols, col_names))

  if (!all_req_cols_present == 0) {
    req_col_string <- paste0(req_cols, collapse = ", ")
    stop(paste0("You are missing one of the two required columns: ", req_col_string,
                "\nPlease use `tidy_forms_survey()` to create the data set with the proper columns."),
         call. = FALSE)
  }

  .data |>
    dplyr::mutate(core_action_main = dplyr::case_when(
      stringr::str_detect(.data$response_option, "[(][0-9][a-z][)]$") ~ stringr::str_extract(.data$response_option, "[(][0-9][a-z][)]$") |>
        stringr::str_remove_all("[(]|[)]"),
      stringr::str_detect(.data$question_stem, "Core Action [0-9] Overall") ~ stringr::str_extract(.data$question_stem, "(?<=Core Action )[0-9]"),
      stringr::str_detect(.data$question_stem, "^Overall, did this lesson reflect the demands") ~ "Demands of the Standards",
      stringr::str_detect(.data$question_stem, "^Are all students engaged in the work of the lesson fr") ~ "Culture of Learning",
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(core_action_minor = dplyr::case_when(
      stringr::str_detect(.data$core_action_main, "^[0-9][a-z]$") ~ stringr::str_extract(.data$core_action_main, "[a-z]$"),
      stringr::str_detect(.data$core_action_main, "^[0-9]$") ~ "Overall",
      stringr::str_detect(.data$core_action_main, "^Demands of the|^Culture") ~ .data$core_action_main,
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(core_action_main = ifelse(stringr::str_detect(.data$core_action_main, "^[0-9][a-z]$"), stringr::str_remove(.data$core_action_main, "[a-z]$"), .data$core_action_main))

}

#' Convert classroom observation data to a format usable by \code{tntpmetrics}
#'
#' With the \code{tntpmetrics} package you can calculate IPG scores. This function puts observation
#' data in the proper format to be used by \code{tntpmetrics}. To use the function, take the raw
#' classroom observation data and first transform it with the \code{tidy_forms_survey()} function
#' and the \code{classroom_obs_add_ca()} function.
#'
#' @param .data The name of an R data frame containing classroom observations and placed into long form
#'      with \code{tidy_forms_survey}, and containing the columns created by \code{classroom_obs_add_ca()}.
#' @param grade_column A string containing the column name containing the grade. For the grade column,
#'      use 'K' or '0' for kindergarten. Use the whole number, as a string, for other grades.
#' @param subject_name The name of the subject. Must be one of: "Math", "Literacy", "Science", or "Social Studies".
#' @param id_cols A vector of strings signifying the columns to keep when transforming the dataset to wide-form. This correlates to the
#'      \code{id_cols} parameter of \code{pivot_wider()}. \code{.id} will be added if it is not included.
#'
#' @examples
#' tidy_forms_survey(classroom_observations_math, 8:ncol(classroom_observations_math), c(3,6)) |>
#'  classroom_obs_add_ca() |>
#'  classroom_obs_add_tntpmetrics(grade_column = 'grade', subject_name = 'Math',
#'                                id_cols = c('.id', 'when_did_the_observation_occur', 'Math'))
#'
#' @return A data frame that can be used by \code{tntpmetrics} to calcualte IPG scores.
#'
#' @importFrom rlang .data
#'
#' @export
classroom_obs_add_tntpmetrics <- function(.data, grade_column, subject_name, id_cols) {

  # make sure data contains required columns
  required_cols <- c('core_action_main', 'core_action_minor', 'response', grade_column, '.id')
  col_names <- colnames(.data)

  missing_cols <- setdiff(required_cols, col_names)

  if (length(missing_cols) > 0) {
    stop(paste0("You are missing the following required column/s: ", paste0(missing_cols, collapse = ", "),
                "\nPlease ensure you run `classroom_obs_add_ca()` prior to running this function."),
         call. = FALSE)
  }

  # recoding mapping of responses to match tntpmetrics requirements
  recode_responses_ca_one <- c('Not Yet' = 0, 'Yes' = 1)
  recode_responses_ca_others <- c(
    "Not Yet" = 1, "Somewhat" = 2, "Mostly" = 3, "Yes" = 4
  )

  .data <- .data |>
    dplyr::mutate(tntp_metric = dplyr::case_when(
      .data$core_action_main == '1' & .data$core_action_minor != 'Overall' ~ glue::glue("ca1_{.data$core_action_minor}"),
      .data$core_action_main %in% c('2', '3') & .data$core_action_minor == 'Overall' ~ glue::glue("ca{.data$core_action_main}_overall"),
      .data$core_action_main == 'Culture of Learning' ~ 'col'
    )) |>
    # only need rows pertaining to items needed for TNTP metrics
    tidyr::drop_na(.data$tntp_metric)

  # make sure the responses are proper
  all_responses <- unique(c(names(recode_responses_ca_one), names(recode_responses_ca_others)))

  bad_responses <- setdiff(.data$response, all_responses)

  if (length(bad_responses) > 0 | any(is.na(.data$response))) {
    stop(paste0("The following response of yours cannot be converted to a number: ", paste0(bad_responses, collapse = ", "),
                "\n Please ensure there are no mising values in the `response` column and only use the following responses: ",
                paste0(all_responses, collapse = ", ")),
         call. = FALSE)
  }

  # convert responses to numbers
  .data$tntp_metric_response <- dplyr::case_when(
    .data$core_action_main == '1' ~ dplyr::recode(.data$response, !!!recode_responses_ca_one, .default = 99),
    .data$core_action_main != '1' ~ dplyr::recode(.data$response, !!!recode_responses_ca_others, .default = 99),
    TRUE ~ 99
  )

  # ensure grade contains the proper values and convert K to 0
  grade_values <- unique(.data[[grade_column]])
  req_values <- c('K', as.character(seq(0, 12)))

  bad_grades <- setdiff(grade_values, req_values)

  if (length(bad_grades) > 0 | any(is.na(.data[[grade_column]]))) {
    stop(paste0("The you either have missing values in the grade column or following grade values of yours are not recognized: ", paste0(bad_grades, collapse = ", "),
                "\n Please ensure there are no mising values in the grade column and that it contains one of the following: ",
                paste0(req_values, collapse = ", ")),
         call. = FALSE)
  }

  .data$grade_level <- ifelse(.data[[grade_column]] == 'K', '0', .data[[grade_column]]) |> as.numeric()

  # convert to wide form and add class

  # we want to ensure '.id' is included as an ID column
  id_cols <- unique(c('.id'), id_cols)

  .data <- .data |>
    tidyr::pivot_wider(id_cols = dplyr::all_of(id_cols), names_from = 'tntp_metric', values_from = 'tntp_metric_response') |>
    dplyr::mutate(form = !!subject_name) |>
    dplyr::select(dplyr::all_of(id_cols), .data$form, dplyr::everything())

  return(.data)

}
