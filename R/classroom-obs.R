#' Add Core Action (major and minor) identifiers
#'
#' Classroom observations are based on the IPG and contain components called core actions.
#' There are major core actions, which are numbers, and minor core actions, which are letters.
#' THe IPG also contains Culture of Learning (CoL) and summary ratings. This function add two columns
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
#' classroom_observations_math |>
#'  tidy_forms_survey(8:ncol(obs), c(3,4,5)) |>
#'  classroom_obs_add_ca()
#'
#' @return A data frame containg all the original columns and rows, along with two additional columns:
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
      stringr::str_detect(response_option, "[(][0-9][a-z][)]$") ~ stringr::str_extract(response_option, "[(][0-9][a-z][)]$") |>
        stringr::str_remove_all("[(]|[)]"),
      stringr::str_detect(question_stem, "Core Action [0-9] Overall") ~ stringr::str_extract(question_stem, "(?<=Core Action )[0-9]"),
      stringr::str_detect(question_stem, "^Overall, did this lesson reflect the demands") ~ "Demands of the Standards",
      stringr::str_detect(question_stem, "^Are all students engaged in the work of the lesson fr") ~ "Culture of Learning",
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(core_action_minor = dplyr::case_when(
      stringr::str_detect(core_action_main, "^[0-9][a-z]$") ~ stringr::str_extract(core_action_main, "[a-z]$"),
      stringr::str_detect(core_action_main, "^[0-9]$") ~ "Overall",
      stringr::str_detect(core_action_main, "^Demands of the|^Culture") ~ core_action_main,
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(core_action_main = ifelse(stringr::str_detect(core_action_main, "^[0-9][a-z]$"), stringr::str_remove(core_action_main, "[a-z]$"), core_action_main))

}
