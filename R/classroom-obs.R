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
#' form with the function \code{g2g_tidy_forms_survey}. It must contain the \code{response_option} and
#' \code{question_stem} columns.
#'
#' @param .data The name of an R data frame containing classroom observations and placed into long form
#'      with \code{g2g_tidy_forms_survey}.
#'
#' @examples
#' g2g_tidy_forms_survey(classroom_observations_math, 8:ncol(classroom_observations_math), c(3,4,5)) |>
#'  g2g_classroom_obs_add_ca()
#'
#' @returns A data frame containing all the original columns and rows, along with two additional columns:
#'      \code{core_action_main} and \code{core_action_minor}.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_classroom_obs_add_ca <- function(.data) {

  # ensure required columns are present
  col_names <- colnames(.data)

  req_cols <- c('response_option', 'question_stem')

  all_req_cols_present <- length(setdiff(req_cols, col_names))

  if (!all_req_cols_present == 0) {
    req_col_string <- paste0(req_cols, collapse = ", ")
    stop(paste0("You are missing one of the two required columns: ", req_col_string,
                "\nPlease use `g2g_tidy_forms_survey()` to create the data set with the proper columns."),
         call. = FALSE)
  }

  .data |>
    dplyr::mutate(core_action_main = dplyr::case_when(
      stringr::str_detect(.data$response_option, "[(][0-9][a-z][)]$") ~ stringr::str_extract(.data$response_option, "[(][0-9][a-z][)]$") |>
        stringr::str_remove_all("[(]|[)]"),
      stringr::str_detect(.data$question_stem, "^Overall.*CA [0-9] overall[)]$") ~ stringr::str_extract(.data$question_stem, "[0-9] overall[)]$") |>
        stringr::str_extract("[0-9]"),
      stringr::str_detect(.data$question_stem, "Core Action [0-9] Overall") ~ stringr::str_extract(.data$question_stem, "(?<=Core Action )[0-9]"),
      stringr::str_detect(.data$question_stem, "^Overall, did this lesson reflect the demands") ~ "Demands of the Standards",
      stringr::str_detect(.data$question_stem, "^Are all students engaged in the work of the lesson fr") ~ "Culture of Learning",
      stringr::str_detect(.data$question_stem, "systematically provide all students with the opportunity to master foundational skills") ~ "Reading Foundational Skills",
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(core_action_minor = dplyr::case_when(
      stringr::str_detect(.data$core_action_main, "^[0-9][a-z]$") ~ stringr::str_extract(.data$core_action_main, "[a-z]$"),
      stringr::str_detect(.data$core_action_main, "^[0-9]$") ~ "Overall",
      stringr::str_detect(.data$core_action_main, "^Demands of the|^Culture") ~ .data$core_action_main,
      stringr::str_detect(.data$core_action_main, "^Reading Foundational Skills$") ~ stringr::str_extract(.data$response_option, "[a-z][)]$") |>
        stringr::str_remove_all("[(]|[)]"),
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(
      core_action_main = ifelse(stringr::str_detect(.data$core_action_main, "^[0-9][a-z]$"), stringr::str_remove(.data$core_action_main, "[a-z]$"), .data$core_action_main),
      response_option = ifelse(is.na(.data$response_option), .data$question_stem, .data$response_option)
    ) |>
    dplyr::mutate(core_action_minor = dplyr::case_when(
      stringr::str_detect(.data$core_action_main, "^Reading Foundational Skills$") & stringr::str_detect(.data$question_stem, "^Overall") ~ 'Overall',
      TRUE ~ .data$core_action_minor
    ))

}


#' Relabel a scale that is in math observations to shorten it and so that it matches other scales.
#'
#' The scales to relabel go from yes to no, with the ones we are changing starting with 'Yes, but'
#' and 'Not really'.
#'
#' @param response_column Column with scales as responses. Will be \code{response} if \code{g2g_tidy_forms_survey()}
#'      is used.
#'
#' @returns A character vector of the same length, with the scales transformed.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_relabel_yesbut_notreally <- function(response_column) {

  g2g_to_title(response_column) |>
    stringr::str_replace("^Yes, but Only.*", "Yes, But Only in Some Areas") |>
    stringr::str_replace("^Not Really.*", "Not Really") |>
    stringr::str_remove("[.]$")

}

#' Convert classroom observation data to a format usable by \code{tntpmetrics}
#'
#' With the \code{tntpmetrics} package you can calculate IPG scores. This function puts observation
#' data in the proper format to be used by \code{tntpmetrics}. To use the function, take the raw
#' classroom observation data and first transform it with the \code{g2g_tidy_forms_survey()} function
#' and the \code{g2g_classroom_obs_add_ca()} function.
#'
#' @param .data The name of an R data frame containing classroom observations and placed into long form
#'      with \code{g2g_tidy_forms_survey}, and containing the columns created by \code{g2g_classroom_obs_add_ca()}.
#' @param grade_column A string containing the column name containing the grade. For the grade column,
#'      use 'K' or '0' for kindergarten. Use the whole number, as a string, for other grades.
#' @param subject_name The name of the subject. Must be one of: "Math", "Literacy", "Science", or "Social Studies".
#' @param id_cols A vector of strings signifying the columns to keep when transforming the dataset to wide-form. This correlates to the
#'      \code{id_cols} parameter of \code{pivot_wider()}. \code{.id} will be added if it is not included.
#'
#' @examples
#' g2g_tidy_forms_survey(classroom_observations_math, 8:ncol(classroom_observations_math), c(3,6)) |>
#'  g2g_classroom_obs_add_ca() |>
#'  g2g_classroom_obs_add_tntpmetrics(grade_column = 'grade', subject_name = 'Math',
#'                                    id_cols = c('.id', 'when_did_the_observation_occur'))
#'
#' @returns A data frame that can be used by \code{tntpmetrics} to calculate IPG scores.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_classroom_obs_add_tntpmetrics <- function(.data, grade_column, subject_name, id_cols) {

  # make sure data contains required columns
  required_cols <- c('core_action_main', 'core_action_minor', 'response', grade_column, '.id')
  col_names <- colnames(.data)

  missing_cols <- setdiff(required_cols, col_names)

  if (length(missing_cols) > 0) {
    stop(paste0("You are missing the following required column/s: ", paste0(missing_cols, collapse = ", "),
                "\nPlease ensure you run `g2g_classroom_obs_add_ca()` prior to running this function."),
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
      .data$core_action_main == 'Culture of Learning' ~ 'col',
      .data$core_action_main == 'Reading Foundational Skills' & .data$core_action_minor == 'Overall' ~ 'rfs_overall',
      TRUE ~'no match'
    ))

  # print main and minor core actions that were not matched, to ensure there are no issues
  actions_not_matches <- .data |>
    dplyr::filter(.data$tntp_metric == 'no match') |>
    dplyr::distinct(.data$core_action_main, .data$core_action_minor)

  message("The following major and minor core action combinations were not matched. Make sure none of these items are required to calculate IPG scores.")
  print(actions_not_matches)

  # remove items that did not match, as they are not required for TNTP metrics
  .data <- .data |>
    dplyr::filter(.data$tntp_metric != 'no match')

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
  id_cols <- unique(c('.id', id_cols, 'grade_level'))

  .data <- .data |>
    tidyr::pivot_wider(id_cols = dplyr::all_of(id_cols), names_from = 'tntp_metric', values_from = 'tntp_metric_response') |>
    dplyr::mutate(form = !!subject_name) |>
    dplyr::select(dplyr::all_of(id_cols), 'form', dplyr::everything())

  return(.data)

}

#' Add a column to the Core Actions that combines the major and minor core actions
#'
#' Classroom observations have a major and minor core action. This functions adds a column to the
#' classroom observation dataset that combines descriptions of major and minor core actions into one column.
#' This is useful for plots. This function should be used on data created with \code{g2g_forms_survey_calc_percentages()}.
#'
#' @param .data Data set created with \code{g2g_forms_survey_calc_percentages()}.
#'
#' @returns A data set that is the same as \code{.data}, but an additional column is added called \code{core_action}.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_obs_combine_ca <- function(.data) {

  .data |>
    dplyr::mutate(
      # add space before Overall, so core action number can be combined
      core_action_minor = ifelse(.data[['core_action_minor']] == 'Overall', " Overall", .data[['core_action_minor']]),
      core_action = dplyr::case_when(
        stringr::str_detect(.data[['core_action_main']], "^[0-9]$") ~ glue::glue("CA {.data[['core_action_main']]}{core_action_minor}"),
        stringr::str_detect(.data[['core_action_main']], "^Reading") ~ glue::glue("RFS {.data[['core_action_minor']]}"),
        stringr::str_detect(.data[['core_action_main']], "^Culture ") ~ 'Culture of Learning',
        stringr::str_detect(.data[['core_action_main']], "^Demands") ~ 'Demands of the Standards',
        TRUE ~ 'Fail to match'
      )
  )

}

#' Find the first and last observations for a person, based on the date.
#'
#' @param .data Data frame containing observation data.
#' @param grouping_columns Columns to group by when finding the first and last observations, as
#'      a vector of strings. Can be names or other unique identifiers.
#' @param date_column Column containing dates that will be used to determine the first and last.
#'
#' @returns A vector of strings with the same length as the number of rows in \code{.data} containing
#'      whether the observation is the first, last, or in the middle of the dates.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_first_or_last <- function(.data, grouping_columns, date_column) {

  # check to ensure columns are in data set
  diff_columns <-setdiff(c(grouping_columns, date_column), colnames(.data))

  if (length(diff_columns != 0)) {

    stop(paste0("The following column that you used as a parameter is not in your data: ", paste0(diff_columns, collapse = ", ")), call.= FALSE)

  }

  # test #
  # check toensure column are in data set
  df <- .data |>
    dplyr::group_by_at(grouping_columns) |>
    dplyr::mutate(.timing = dplyr::case_when(
      .data[[date_column]] == max(.data[[date_column]]) ~ 'Last Observation',
      .data[[date_column]] == min(.data[[date_column]]) ~ 'First Observation',
      TRUE ~ 'During Program'
    ))

  # find the total number of distinct values of grouping column
  distinct_counts <- df |>
    dplyr::group_by_at(grouping_columns) |>
    dplyr::select(dplyr::all_of(c(grouping_columns, date_column))) |>
    dplyr::distinct() |>
    dplyr::count(name = '.num_group')

  df |>
    dplyr::left_join(distinct_counts, by = grouping_columns)|>
    dplyr::ungroup()

}
