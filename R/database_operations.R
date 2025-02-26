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

  cli::cli_alert_success("Added row with site information to table {.emph {site_tbl_name}}.")

  invisible(NULL)

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
    'teacher_id', 'site_id', 'obs_number', 'term', 'subject',
    'grade', 'date_of_observation', 'question_stem',
    'response_option', 'response', 'core_action_main', 'core_action_minor'
  )

  cols_equal <- length(req_columns) == length(colnames(.data)) & all(sort(colnames(.data)) == sort(req_columns))

  if (!cols_equal) cli::cli_abort("Your column names are incorrect. Please ensure you have all required columns, and no more.")

  DBI::dbWriteTable(con, tbl_name, .data, append = TRUE, row.names = FALSE)

  n_rows <- nrow(.data)

  n_obs <- dplyr::n_distinct(.data$obs_number)

  cli::cli_alert_success("Added {n_rows} rows of observation data from {n_obs} observations to {.emph {tbl_name}}.")

  invisible(NULL)

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
    'submission_date', 'teacher_id','site_id', 'term',
    'question_type', 'question_stem', 'response_option', 'response'
  )

  cols_equal <- length(req_columns) == length(colnames(.data)) & all(sort(colnames(.data)) == sort(req_columns))

  if (!cols_equal) cli::cli_abort("Your column names are incorrect. Please ensure you have all required columns, and no more.")

  # timing should only be one of these values
  term_values <- c('Pre-Training', 'Post-Training', "Mid-Training", 'Follow-Up')

  actual_values <- unique(.data$term)

  additional_values_in_data <- setdiff(actual_values, term_values)

  if (length(additional_values_in_data) > 0) cli::cli_abort("The `term` column should only contain the values: {term_values}. Your data contains additional values")

  DBI::dbWriteTable(con, tbl_name, .data, append = TRUE, row.names = FALSE)

  n_rows <- nrow(.data)

  n_teachers <- dplyr::n_distinct(.data$teacher_id)

  cli::cli_alert_success("Added {n_rows} rows of survey responses from {n_teachers} teachers to {.emph {tbl_name}}.")

  invisible(NULL)

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

  cli::cli_alert_success("Added {n_rows} rows of survey response data from {n_responses} student responses to {.emph {tbl_name}}.")

  invisible(NULL)

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
    'site_id', 'subject', 'district_school', 'teacher_survey_email', 'obs_name',
    'student_survey_name', 'years_teaching', 'notes'
  )

  cols_equal <- length(req_columns) == length(colnames(.data)) & all(sort(colnames(.data)) == sort(req_columns))

  if (!cols_equal) cli::cli_abort("Your column names are incorrect. Please ensure you have all required columns, and no more.")

  DBI::dbWriteTable(con, tbl_name, .data, append = TRUE, row.names = FALSE)

  n_rows <- nrow(.data)

  cli::cli_alert_success("Added {n_rows} rows of observations to {.emph {tbl_name}}.")

  invisible(NULL)

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

#' Drop all site data from a single table
#'
#' Drops all data from a single site within a single table. Operation is irreversible
#' and it is recommended that you make a copy of your data base prior to dropping site data.
#'
#' To drop data from the `teacher_information` table you must first drop the data from
#' the `observations`, `teacher_survey`, and `student_survey` tables. This is because
#' these tables contain foreign keys that rely on the `teacher_information` table.
#'
#' @param con Database connection to Good to Great database. Should be created with
#'      `DBI::dbConnect(RSQLite::SQLite(), dbname = "good_to_great.db")`.
#' @param site_id An integer. The value in the `site_id` column from the `sites` table.
#'      Can be found with `g2g_site_information()`.
#' @param db_table String identifying which table we want to delete data from.
#'      One of 'teacher_information', 'observations', 'teacher_survey', or 'student_survey'.
#'
#' @export
g2g_db_drop_site_data_from_table <- function(con, site_id, db_table) {

  db_tables_to_use <- c('teacher_information', 'observations', 'teacher_survey', 'student_survey')

  if (!db_table %in% db_tables_to_use) stop(cli::cli_abort("`db_table` must be one of: {db_tables_to_use}"), call. = FALSE)

  # ensure the site_id exists in the data base
  check_id_query <- glue::glue_sql(
    "SELECT
      CASE
          WHEN EXISTS (SELECT 1 FROM sites WHERE site_id = {site_id}) THEN 'T'
          ELSE 'F'
      END AS does_exist;",
    .con = con
  )

  site_id_exists <- DBI::dbGetQuery(con, check_id_query)

  if (site_id_exists == 'F') stop('The `site_id` that you entered does not exist in the `sites` table. Use `g2g_db_get_site_info_key()` to get the `site_id`.', call. = FALSE)

  # ensure person really wants to delete data by asking prompts in the command line
  while (TRUE) {
    answer <- readline(glue::glue("This operation will permenantly delete data from the {db_table} table.\nYou should back up your database prior to deleting data.\n- Are you sure you want to delete your data? (yes/no): "))

    # Convert the answer to lowercase
    answer <- tolower(answer)

    if (answer == "yes") {
      # Proceed with the operation
      cat("Deleting data.\n")
      break
    } else if (answer == "no") {
      # Abort or give a message
      cat("Operation aborted.\n")
      invisible(NULL)
      break
    } else {
      cat("\nInvalid response. Please answer 'yes' or 'no'.\n")
    }
  }

  delete_data_query <- glue::glue_sql(
    "DELETE FROM {db_table}
     WHERE site_id = {site_id};",
    .con = con
  )

  # deletes rows and returns an integer showing the number of rows deleted
  n_rows_deleted <- DBI::dbExecute(con, delete_data_query)

  cli::cli_alert_success("Deleted {.emph {n_rows_deleted}} rows from {.emph {db_table}}.")

  invisible(NULL)

}
