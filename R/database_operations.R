# functions to interact with the Good to Great database

#' Add site information to `sites` table
#'
#' Creates an additional row in the `sites` table that adds site information.
#'
#' @param con Database connection to Good to Great database. Should be created with
#'      `DBI::dbConnect(RSQLite::SQLite(), dbname = "good_to_great.db")`.
#' @param site_info Site information list created with `g2g_site_information()`.
#'
#' @returns
#' Message indicating success.
#'
#' @export
g2g_db_add_site_info <- function(con, site_info) {

  site_tbl_name <- "sites"

  site_table <- data.frame(
    name = site_info$site_name_title,
    year = site_info$year,
    semester = site_info$semester_title,
    subject = site_info$subject_title
  )

  DBI::dbWriteTable(con, site_tbl_name, site_table, append = TRUE, row.names = FALSE)

  return(cli::cli_alert_success("Added row with site information to table {.emph {site_tbl_name}}."))

}

#' Add observations to `observations` table
#'
#' Add a dataframe of observations to the `observations` table.
#'
#' @param con Database connection to Good to Great database. Should be created with
#'      `DBI::dbConnect(RSQLite::SQLite(), dbname = "good_to_great.db")`.
#' @param .data Dataframe of observations. Should be in long-form and must contain all required columns and no more.
#'
#' @returns
#' Message indicating success.
#'
#' @export
g2g_db_add_obs <- function(con, .data) {

  tbl_name <- "observations"

  # dataframe should only have these columns
  req_columns <- c(
    'teacher_id', 'site_id', 'obs_number', 'timing',
    'grade', 'date_of_observation', 'question_stem',
    'response_option', 'response', 'core_action_main', 'core_action_minor'
  )

  cols_equal <- length(req_columns) == length(colnames(.data)) & all(sort(colnames(.data)) == sort(req_columns))

  if (!cols_equal) cli::cli_abort("Your column names are incorrect. Please ensure you have all required columns, and no more.")

  # timing should only be one of three values
  timing_values <- c('First Observation', 'Last Observation', 'During Program')

  actual_values <- unique(.data$timing)

  additional_values_in_data <- setdiff(actual_values, timing_values)

  if (length(additional_values_in_data) > 0) cli::cli_abort("The `timing` column should only contain the values: {timing_values}. Your data contains additional values")

  DBI::dbWriteTable(con, tbl_name, .data, append = TRUE, row.names = FALSE)

  n_rows <- nrow(.data)

  n_obs <- dplyr::n_distinct(.data$obs_number)

  return(cli::cli_alert_success("Added {n_rows} rows of observation data from {n_obs} observations to {.emph {tbl_name}}."))

}

#' Add teacher survey responses to `teacher_survey` table
#'
#' Add a dataframe of survey responses to the `teacher_survey` table.
#'
#' @param con Database connection to Good to Great database. Should be created with
#'      `DBI::dbConnect(RSQLite::SQLite(), dbname = "good_to_great.db")`.
#' @param .data Dataframe of teacher survey responses Should be in long-form and must contain all required columns and no more.
#'
#' @returns
#' Message indicating success.
#'
#' @export
g2g_db_add_teacher_survey <- function(con, .data) {

  tbl_name <- "teacher_survey"

  # dataframe should only have these columns
  req_columns <- c(
    'submission_date', 'teacher_id','site_id', 'term', 'in_survey', 'n_times_administered',
    'question_type', 'question_stem', 'response_option', 'response'
  )

  cols_equal <- length(req_columns) == length(colnames(.data)) & all(sort(colnames(.data)) == sort(req_columns))

  if (!cols_equal) cli::cli_abort("Your column names are incorrect. Please ensure you have all required columns, and no more.")

  # timing should only be one of three values
  term_values <- c('Pre-Training', 'Post-Training', 'Follow-Up')

  actual_values <- unique(.data$term)

  additional_values_in_data <- setdiff(actual_values, term_values)

  if (length(additional_values_in_data) > 0) cli::cli_abort("The `term` column should only contain the values: {term_values}. Your data contains additional values")

  DBI::dbWriteTable(con, tbl_name, .data, append = TRUE, row.names = FALSE)

  n_rows <- nrow(.data)

  n_teachers <- dplyr::n_distinct(.data$teacher_id)

  return(cli::cli_alert_success("Added {n_rows} rows of survey responses from {n_teachers} teachers to {.emph {tbl_name}}."))

}

#' Add student survey responses to `student_survey` table
#'
#' Add a dataframe of survey responses to the `student_survey` table.
#'
#' @param con Database connection to Good to Great database. Should be created with
#'      `DBI::dbConnect(RSQLite::SQLite(), dbname = "good_to_great.db")`.
#' @param .data Dataframe of teacher survey responses Should be in long-form and must contain all required columns and no more.
#'
#' @returns
#' Message indicating success.
#'
#' @export
g2g_db_add_student_survey <- function(con, .data) {

  tbl_name <- "student_survey"

  # dataframe should only have these columns
  req_columns <- c(
    'submission_date', 'student_id', 'teacher_id','site_id', 'current_grade', 'term',
    'question_stem', 'response_option', 'response'
  )

  cols_equal <- length(req_columns) == length(colnames(.data)) & all(sort(colnames(.data)) == sort(req_columns))

  if (!cols_equal) cli::cli_abort("Your column names are incorrect. Please ensure you have all required columns, and no more.")

  # timing should only be one of three values
  term_values <- c('Pre-Training', 'Post-Training', 'Follow-Up')

  actual_values <- unique(.data$term)

  additional_values_in_data <- setdiff(actual_values, term_values)

  if (length(additional_values_in_data) > 0) cli::cli_abort("The `term` column should only contain the values: {term_values}. Your data contains additional values")

  DBI::dbWriteTable(con, tbl_name, .data, append = TRUE, row.names = FALSE)

  n_rows <- nrow(.data)

  n_responses <- dplyr::n_distinct(.data$student_id)

  return(cli::cli_alert_success("Added {n_rows} rows of survey response data from {n_responses} student responses to {.emph {tbl_name}}."))

}

#' Add teachers to `teacher_information` table
#'
#' Add a dataframe of teachers to the `teacher_information` table.
#'
#' @param con Database connection to Good to Great database. Should be created with
#'      `DBI::dbConnect(RSQLite::SQLite(), dbname = "good_to_great.db")`.
#' @param .data Dataframe of observations. Should be in long-form and must contain all required columns and no more.
#'
#' @returns
#' Message indicating success.
#'
#' @export
g2g_db_add_teacher <- function(con, .data) {

  tbl_name <- "teacher_information"

  # dataframe should only have these columns
  req_columns <- c(
    'site_id', 'teacher_survey_email', 'obs_name',
    'student_survey_name', 'years_teaching', 'notes'
  )

  cols_equal <- length(req_columns) == length(colnames(.data)) & all(sort(colnames(.data)) == sort(req_columns))

  if (!cols_equal) cli::cli_abort("Your column names are incorrect. Please ensure you have all required columns, and no more.")

  DBI::dbWriteTable(con, tbl_name, .data, append = TRUE, row.names = FALSE)

  n_rows <- nrow(.data)

  return(cli::cli_alert_success("Added {n_rows} rows of observations to {.emph {tbl_name}}."))

}



#' Get `site_id` (primary key for site) from `sites` table
#'
#' Get the primary key value from a site so that it can be used when adding data.
#'
#' @param con Database connection to Good to Great database. Should be created with
#'      `DBI::dbConnect(RSQLite::SQLite(), dbname = "good_to_great.db")`.
#' @param site_info Site information list created with `g2g_site_information()`.
#'
#' @returns
#' Single integer representing primary key value for site.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_db_get_site_info_key <- function(con, site_info) {

  # get primary key ID for site, to be used in other data sets
  site_id <- con |>
    dplyr::tbl('sites') |>
    dplyr::filter(
      .data$name == !!site_info$site_name_title,
      .data$year == !!site_info$year,
      .data$semester == !!site_info$semester_title,
      .data$subject == !!site_info$subject_title
    ) |>
    dplyr::select(dplyr::all_of('site_id')) |>
    dplyr::collect() |>
    dplyr::pull(.data$site_id)

  # make sure there is only one value
  if (length(site_id) == 0) {
    cli::cli_abort("No site swere found in the `sites` table matching the information yoy provided in the `site_info` parameter")
  } else if (length(site_id) > 1) {
    cli::cli_abort("More than one site was found in the `sites` table matching the information yoy provided in the `site_info` parameter")
  }

  return(site_id)

}
