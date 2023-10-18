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
      stringr::str_detect(.data$question_stem, "^Overall.*CA [0-9] [O|o]verall[)]$") ~ stringr::str_extract(.data$question_stem, "[0-9] [O|o]verall[)]$") |>
        stringr::str_extract("[0-9]"),
      stringr::str_detect(.data$question_stem, "Core Action [0-9] [O|o]verall") ~ stringr::str_extract(.data$question_stem, "(?<=Core Action )[0-9]"),
      stringr::str_detect(.data$question_stem, "^Overall, did this lesson reflect the demands") ~ "Demands of the Standards",
      stringr::str_detect(.data$question_stem, "^Are all students engaged in the work of the lesson fr") ~ "Culture of Learning",
      stringr::str_detect(.data$question_stem, "systematically provide all students with the opportunity to master foundational skills") ~ "Reading Foundational Skills",
      stringr::str_detect(.data$response_option, "[(]AC[0-9][)]$") ~ "Aligned Content",
      stringr::str_detect(.data$response_option, "[(]SP[0-9][)]$") ~ "Student Practice",
      stringr::str_detect(.data$response_option, "[(]TD[0-9][)]$") ~ "Teacher-Directed Instruction",
      stringr::str_detect(.data$response_option, "[(]AD[0-9][)]$") ~ "Assessment & Differentiation",
      stringr::str_detect(.data$response_option, "[(]HQIM..[)]$") ~ "HQIM",
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(core_action_minor = dplyr::case_when(
      stringr::str_detect(.data$core_action_main, "^[0-9][a-z]$") ~ stringr::str_extract(.data$core_action_main, "[a-z]$"),
      stringr::str_detect(.data$core_action_main, "^[0-9]$") ~ "Overall",
      stringr::str_detect(.data$core_action_main, "^Demands of the|^Culture") ~ .data$core_action_main,
      stringr::str_detect(.data$core_action_main, "^Reading Foundational Skills$") ~ stringr::str_extract(.data$response_option, "[a-z][)]$") |>
        stringr::str_remove_all("[(]|[)]"),
      stringr::str_detect(.data$response_option, "[(]AC[0-9][)]$|[(]SP[0-9][)]$|[(]TD[0-9][)]$|[(]AD[0-9][)]$") ~ stringr::str_extract(.data$response_option, "[0-9][)]$") |>
        stringr::str_remove_all("[(]|[)]"),
      .data$core_action_main == 'HQIM' & stringr::str_detect(.data$response_option, "teacher is utilizing the materials") ~ "Teacher utilizes materials",
      .data$core_action_main == 'HQIM' & stringr::str_detect(.data$response_option, "teacher utilizes the embedded instructional moves") ~ "Teacher utilizes the embedded instructional moves",
      TRUE ~ NA_character_
    )) |>
    dplyr::mutate(
      core_action_main = ifelse(stringr::str_detect(.data$core_action_main, "^[0-9][a-z]$"), stringr::str_remove(.data$core_action_main, "[a-z]$"), .data$core_action_main),
      response_option = ifelse(is.na(.data$response_option), .data$question_stem, .data$response_option)
    ) |>
    dplyr::mutate(core_action_minor = dplyr::case_when(
      stringr::str_detect(.data$core_action_main, "^Reading Foundational Skills$") & stringr::str_detect(.data$question_stem, "^[O|o]verall") ~ 'Overall',
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

  # can only have one subject
  if (length(subject_name) != 1) stop("There can only be one `subject_name`. You had more than one.", call. = FALSE)

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

#' Calculate average IPG scores
#'
#' Calculates average IPG scores for identified groups. Data must be created with `g2g_tidy_forms_survey`
#' and `g2g_classroom_obs_add_ca`
#'
#' @param .data Data set created with `g2g_tidy_forms_survey` and `g2g_classroom_obs_add_ca`
#' @param grade_column The column name, as a string, that contains the grade.
#' @param subject_name A single value, as a string, representing the subject for the observations.
#'      Cannot have multiple subjects.
#' @param grouping_terms Column name or names, as a string or vector of strings, containing categories we want to group by when averaging
#'      IPG scores. Each unique value in this column will be a distinct row in the final output.
#'
#' @examples
#' g2g_tidy_forms_survey(classroom_observations_math, 8:ncol(classroom_observations_math), c(3,6)) |>
#'     g2g_classroom_obs_add_ca() |>
#'     g2g_calc_avg_ipg(grade_column = 'grade', subject_name = 'Math',
#'     grouping_terms = 'when_did_the_observation_occur')
#'
#' @returns A data frame with average IPG scores. Each row is a different group identified in `grouping_terms`.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_calc_avg_ipg <- function(.data, grade_column, subject_name, grouping_terms = NULL) {

  id_cols <- c(".id", grouping_terms)

  df <- .data |>
    g2g_classroom_obs_add_tntpmetrics(
      grade_column = grade_column, subject_name = subject_name, id_cols = id_cols
    ) |>
    # drop if not all values have numbers
    tidyr::drop_na()

  message(glue::glue("{nrow(df)} observations were used in calculating the IPG score."))

  df |>
    tntpmetrics::make_metric('ipg') |>
    dplyr::group_by_at(grouping_terms) |>
    dplyr::summarize(dplyr::across(dplyr::starts_with('cm_'), ~mean(.x, na.rm = TRUE)))

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
        stringr::str_detect(.data[['core_action_main']], "HQIM") ~ .data$core_action_minor,
        stringr::str_detect(.data[['response_option']], "[(]AC[0-9][)]$|[(]SP[0-9][)]$|[(]TD[0-9][)]$|[(]AD[0-9][)]$") ~ glue::glue("{stringr::str_sub(.data[['response_option']], -4,-3)} {.data[['core_action_minor']]}"),
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
  # check to ensure column are in data set
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

#' Find the scales for observations based on the Core Action
#'
#' @param core_action The core action whose scale is needed. String.
#'      Options are '1', '2', '3', 'RFS', 'Demands of the Standards', 'Culture of Learning',
#'      'Aligned Content', 'Assessment & Differentiation', 'Student Practice', 'Teacher-Directed Instruction'.
#'
#' @returns A vector with the scales for the core action, in order
#'
#' @importFrom rlang .data
#'
#' @export
g2g_obs_map_scales <- function(core_action) {

  lower_case_ca <- stringr::str_to_lower(core_action)

  useable_core_actions <- c(
    '1', '2', '3', 'RFS', 'Reading Foundational Skills', 'Demands of the Standards', 'Culture of Learning', 'CoL',
    'Aligned Content', 'Assessment & Differentiation', 'Student Practice', 'Teacher-Directed Instruction','HQIM'
  )

  foundation_skills_re <- "^aligned|^assessment|^student.*pract|^teacher.*direct"

  lower_case_ca <- dplyr::case_when(
    stringr::str_detect(lower_case_ca, "^reading") ~ 'rfs',
    stringr::str_detect(lower_case_ca, "^culture") ~ 'col',
    TRUE ~ lower_case_ca
  )

  if (!lower_case_ca %in% stringr::str_to_lower(useable_core_actions)) stop(paste0("`core_action` must be one of: ", paste0(useable_core_actions, collapse = ", ")), call. = FALSE)

  scale_order <- dplyr::case_when(
    lower_case_ca == 'demands of the standards' ~ list(g2g_scale_order('yes_but')),
    lower_case_ca == '1' ~ list(g2g_scale_order('yes_notyet')),
    lower_case_ca %in% c('2', '3', 'col', 'rfs', 'hqim') ~ list(g2g_scale_order('yes_mostly_somewhat_notyet')),
    stringr::str_detect(lower_case_ca, "^aligned|^assessment|^teacher.*direct") ~ list(g2g_scale_order('always_rarely')),
    stringr::str_detect(lower_case_ca, "^student.*pract") ~ list(g2g_scale_order('all_few')),
    TRUE ~ list('Failed to match')
  ) |>
    unlist()

  if ('Failed to match' %in% scale_order) stop("There was an issue finding your scales. Please check the spelling of `core_action`.", call. = FALSE)

  return(scale_order)

}

#' Place observation data into long form and add columns identifying the core action
#'
#' @param .data The raw observation data, as pulled from Google Sheets
#' @param ... for `g2g_tidy_forms_survey`. Should include the `question_columns` and
#'      `grouping_columns` parameters
#'
#' @returns A tibble of the observation data in long form, where each row is a teacher / core action combination.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_obs_long_form <- function(.data, ...) {

  .data |>
    g2g_tidy_forms_survey(...) |>
    dplyr::rename(term = .data[['when_did_the_observation_occur']]) |>
    g2g_classroom_obs_add_ca() |>
    dplyr::mutate(
      response = g2g_to_title(.data[['response']]),
      # relabel response for one scale
      response = g2g_relabel_yesbut_notreally(.data[['response']])
    ) |>
    # make all names lower case for better matching
    dplyr::mutate(dplyr::across(dplyr::ends_with('_name'), stringr::str_to_lower))

}

#' Calculate the response percentages of each core action by pre and post training
#'
#' @param .data Observation data that is already in long form, created by `g2g_obs_long_form()`.
#' @param grouping_columns Columns to group by when calculating percentages. The following
#'       columns are automatically added for grouping: '.timing', 'core_action_main', 'core_action_minor'.
#'
#' @returns
#' A tibble with aggregated observation result percentages for each rating. Aggregated by core action and timing (pre or post)
#'
#' @importFrom rlang .data
#'
#' @export
g2g_obs_calc_perc <- function(.data, grouping_columns = NULL) {

  perc_grouping_cols <- c(c('.timing', 'core_action_main', 'core_action_minor'), grouping_columns)

  col_names <- colnames(.data)

  if (!all(perc_grouping_cols %in% col_names)) stop(stringr::str_c("`.data` must contain the following columns: ", paste0(perc_grouping_cols, collapse = ", ")))

  .data |>
    tidyr::drop_na(.data[['core_action_main']], .data[['response']]) |>
    g2g_forms_survey_calc_percentages(grouping_columns = perc_grouping_cols, add_n = 'none') |>
    # add a column combining major and minor core actions
    g2g_obs_combine_ca() |>
    dplyr::mutate(
      response_option = stringr::str_wrap(.data[['response_option']], 40),
      core_action = stringr::str_wrap(.data[['core_action']], 25)
    )

}

#' Create a dataset containing aggregate responses for a single Core Action.
#'
#' Creates a dataset of aggregate responses for a single core action. Filters for the specific core action
#' and adds a column with the percengate of positive responses. This dataset is used for plotting
#'
#' @param .data Observation data in long form, with the following columns:
#'    '.timing', 'core_action_main', 'core_action_minor'. Data should already be aggregated and show
#'    percentages for each core action/ response combination. Can create data with the `g2g_obs_calc_perc()` function.
#' @param core_action The core action that we want to get data for. A string that mirrors the spelling in the
#'      `core_action_main` column.
#' @param scale_order The order of the response scale. Can find with the function `g2g_obs_map_scales()`
#' @param first_obs_factor String representing the first observation category in `.timing`, as a string.
#'      `.timing` will be changed to a factor with this string being the first level. Useful for ordering plots.
#'      'Defaults to 'First Observation'
#'
#' @returns
#' A tibble with aggregate data for a single core action. It contains an additional column with the
#' aggregate of positive responses.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_obs_get_ca_data <- function(.data, core_action, scale_order, first_obs_factor = 'First Observation') {

  perc_grouping_cols <- c('.timing', 'core_action_main', 'core_action_minor')

  col_names <- colnames(.data)

  if (!all(perc_grouping_cols %in% col_names)) stop(stringr::str_c("`.data` must contain the following columns: ", paste0(perc_grouping_cols, collapse = ", ")))

  # if there are 3 or fewer scale responses, only the first response is positive
  # otherwise, the first two responses are positive
  positive_responses <- if (length(scale_order) <= 3) c(1) else c(2,1)

  .data |>
    dplyr::filter(.data[['core_action_main']] == !!core_action) |>
    # aggregate positive responses for plotting
    g2g_aggregate_positive_responses(scale_order[positive_responses], perc_grouping_cols, only_keep_first_response = TRUE) |>
    dplyr::mutate(
      response = factor(.data[['response']], levels = rev(scale_order)),
      core_action = forcats::fct_rev(.data[['core_action']]),
      .timing = forcats::fct_relevel(.data[['.timing']], first_obs_factor)
    )

}

#' Create a vertical visualization of observation data for a single Core Action.
#'
#' Creates a vertical bar plot for a single Core Action. Chart is faceted by .timing.
#' X axis labels are the core action abbreviations (example: CA 1 Overall)
#'
#' @param .data Data created with `g2g_obs_get_ca_data()`
#' @param core_action The core action that we want to get data for. A string that mirrors the spelling in the
#'      `core_action_main` column.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_obs_create_viz_ca <- function(.data, core_action) {

  # make sure we have the required columns
  required_cols <- c('.percent', 'core_action', 'response', '.strong_response_percent', '.timing')

  col_names <- colnames(.data)

  if (!all(required_cols %in% col_names)) stop(stringr::str_c("`.data` must contain the following columns: ", paste0(required_cols, collapse = ", ")))

  # make sure all percentages are between 0 and 1
  if (!all(dplyr::between(.data$.percent, 0, 1))) stop("All expected percentages did nto fall between 0 and 1. Please re-check your input data (`.data`)", call. = FALSE)

  # find out whether we are working with a Core Action
  # title will depend on this
  is_ca <- stringr::str_detect(core_action, "[0-9]")

  if (is_ca) {
    plt_title <- glue::glue("Core Action {core_action} Observation Results")
  } else {
    plt_title <- glue::glue("{core_action} Observation Results")
  }

  scale_order <- g2g_obs_map_scales(core_action)

  pal <- names(scale_order) |> purrr::set_names(scale_order)

  # number of rows and columns of facet depends on number of facet panels
  n_facet_panels <- dplyr::n_distinct(.data[['core_action']])

  n_facet_rows <- if (n_facet_panels <= 4) 1 else 2

  plt <- .data |>
    # dplyr::mutate(core_action = stringr::str_wrap(.data[['core_action']], 30)) |>
    g2g_viz_stacked_bar_percent_vertical(
      x_var = '.timing',
      y_var = '.percent',
      fill_var = 'response',
      text_var = '.strong_response_percent',
      color_pal = pal
    ) +
    ggplot2::labs(
      x = NULL,
      y = 'Percentage of observations',
      fill = NULL,
      title = plt_title
    ) +
    ggplot2::facet_wrap(ggplot2::vars(forcats::fct_rev(core_action)), nrow = n_facet_rows)

  return(plt)

}

#' Add an observation visualization to a PPT slide

#' @param doc The PPT document object. Initially created with `g2g_create_deck_ppt()`.
#' @param plt The plot to add to the PPT. Typically created with `g2g_obs_create_viz()`.
#' @param ca_descriptions A description of all core actions. Can be created with `.data |> distinct(core_action_main, core_action_minor, core_action, response_option)`
#' @param core_action The core action that we want to get data for. A string that mirrors the spelling in the
#'      `core_action_main` column.
#' @param plt_height The height of the plot in the PPT. Can be calculated with `g2g_ppt_calculate_plot_height()`.
#'      Defaults to 6.
#' @param plt_width The width of the plot. Defaults to 9.
#'
#' @returns A `doc` object that can be reused when adding additional slides to the PPT.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_obs_add_viz_ppt <- function(doc, plt, ca_descriptions, core_action, plt_height = 6, plt_width = 9) {

  is_ca <- stringr::str_detect(core_action, "[0-9]")

  if (is_ca) {
    slide_title <- glue::glue('Teacher Observations\nCore Action {core_action}')
  } else {
    slide_title <- glue::glue("Teacher Observations\n{core_action}")
  }

  ca_notes <- ca_descriptions |>
    dplyr::filter(.data[['core_action_main']] == !!core_action) |>
    dplyr::mutate(ca_description = glue::glue("- {.data[['core_action']]}: {.data[['response_option']]}")) |>
    dplyr::pull(.data[['ca_description']]) |>
    stringr::str_wrap(400, exdent = 5) |>
    stringr::str_c(collapse = "\n")

  ca_notes <- if (is_ca) stringr::str_c("Core Action Definitions:\n", ca_notes) else stringr::str_c(core_action, " Definitions:\n", ca_notes)

  doc <- g2g_add_viz_ppt(
    doc, plt,
    slide_title, plt_width = plt_width,
    plt_height = plt_height,
    notes_text = ca_notes
  )

  return(doc)

}


#' Test to ensure the aggregated data matches the  raw data
#'
#' @param raw_data The raw data, as imported from Google Sheets
#' @param long_form_data The long form data of each individual response, created with `g2g_obs_long_form()`
#' @param aggregated_data Aggregated data showing the percentage of respondents for each question / Core Action,
#'      created with `g2g_obs_calc_perc()`.
#' @param core_action_to_test The Core Action to test. Will be the string in the `core_action` column of
#'      the long form data. But, leave off "CA ".
#'
#' @returns
#' Returns NULL if test is passed. otherwise, returns `testthat` output.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_obs_test_results <- function(raw_data, long_form_data, aggregated_data, core_action_to_test) {

  # identify the exact row numbers that are used in the cleaned data and whether the row number
  # is the first or last observation
  # needed so we can pull out the final data from the raw data
  distinct_obs <- long_form_data |>
    dplyr::distinct(.data[['.id']], .data[['.timing']])

  # extract the needed rows from the raw data and label whether they are the first or last observation
  obs_check <- raw_data |>
    dplyr::mutate(.id = dplyr::row_number()) |>
    dplyr::left_join(distinct_obs, by = '.id') |>
    tidyr::drop_na(.data[['.timing']]) |>
    dplyr::select(dplyr::all_of(c('.id', '.timing')), dplyr::contains(core_action_to_test))

  colnames(obs_check) <- c('.id', '.timing', 'response')

  # calculated the percentages from the raw data
  expected_result <- obs_check |>
    tidyr::drop_na('response') |>
    dplyr::group_by_at(c('.timing', 'response')) |>
    dplyr::summarize(.n_response = dplyr::n()) |>
    dplyr::mutate(
      .n_question = sum(.data[['.n_response']]),
      .percent = .data[['.n_response']] / .data[['.n_question']],
      response = stringr::str_to_lower(.data[['response']])
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data[['.timing']], .data[['response']])

  # place the actual percentage data into a format that matches the calculated percentages from the raw data
  actual_result <- aggregated_data |>
    dplyr::filter(stringr::str_detect(.data[['core_action']], core_action_to_test)) |>
    dplyr::mutate(response = stringr::str_to_lower(.data[['response']])) |>
    dplyr::select(dplyr::all_of(c('.timing', 'response', '.n_response', '.n_question', '.percent'))) |>
    dplyr::arrange(.data[['.timing']], .data[['response']])

  # test result
  testthat::expect_equal(actual_result, expected_result)

  return('TEST PASSED!!')

}

#' Create single plot that contains all overall observation scores
#'
#' The input dataset `.data` must be transformed to where the values in the `core_action_main` column
#' represent unique scales. For example, Core Action 1 has different scales than Core Actions 2 and 3.
#' Core Actions 2 and 3 have the same scales. Therefore, `core_action_main` must be transformed to
#' where Core Action 1 has a value (ex: '1') and Core Actions 2 and 3 have the same value (ex: '2').
#' This value should represent the core action value needed for the scales. See `examples` for more information.
#'
#' @param .data Dataset containg aggregate overall Core Action values. Created by first calculating
#'      aggregates with `g2g_obs_calc_perc()` and then filtering to only keep overall scores.
#' @param height_relationships Relationship in proportions between plots with different scales, as a numeric vector.
#'      Defaults to c(1, 3).
#' @param first_obs_factor String representing the first observation category in `.timing`, as a string.
#'      `.timing` will be changed to a factor with this string being the first level. Useful for ordering plots.
#'      Defaults to 'First Observation'.
#'
#' @examples
#' \dontrun{
#'
#' # regular expression to identify the overall items in the core_action column
#' overall_items <- c("Overall|Culture|Demands|RFS")
#'
#' overall_scores_ca <- obs_percent |>
#'   # only keep the overall core action items
#'   filter(str_detect(core_action, !!overall_items)) |>
#'   # change `core_action_main` values so that core actions with the same scales have the same values,
#'   # and these values mirror the scale
#'   mutate(core_action_main = ifelse(core_action_main == "1", '1', "2"))
#'
#' # create plot
#' g2g_obs_viz_overall(overall_scores_ca)
#'
#' }
#'
#' @returns a ggplot plot
#'
#' @importFrom rlang .data
#'
#' @export
g2g_obs_viz_overall <- function(.data, height_relationships = c(1, 3), first_obs_factor = 'First Observation') {

  unique_core_actions <- unique(.data$core_action_main)

  plt <- purrr::map(unique_core_actions, function(core_action) {

    scale_order <- g2g_obs_map_scales(core_action)

    plt <- .data |>
      g2g_obs_get_ca_data(core_action, scale_order, first_obs_factor) |>
      g2g_obs_create_viz_ca(core_action) +
      ggplot2::ggtitle(NULL)

  })

  plt[[1]] <- plt[[1]] +
    ggplot2::labs(x = NULL)

  plt[[2]] <- plt[[2]] +
    ggplot2::theme(
      # strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank()
    )

  ca_plts <- patchwork::wrap_plots(plt[[1]], plt[[2]],ncol = 1, heights = height_relationships) +
    patchwork::plot_annotation(
      title = 'All Overall Observation Results',
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 13, face = "bold"))
    )

  return(ca_plts)

}

# TODO
# identify whether teacher has pre and post for a given item
# compare_cols <- c('teachers_last_name', 'core_action_main', 'core_action_minor')
#
# pre_post_compare <- obs_long |>
#   group_by_at(compare_cols) |>
#   tally(name = '.n_pre_post') |>
#   ungroup()
#
# # add this information to the long-form data set so that we know what to remove
# obs_long <- obs_long |>
#   left_join(pre_post_compare, by = compare_cols)
#
# # all values should either be 1 or 2, check
# if (!all(obs_long$.n_pre_post %in% c(1, 2))) stop("There was an error in checking whether each teacher has pre and post scores for an item. Check data.", call. = FALSE)
#
# # remove items where there is only one response
# obs_long <- obs_long |>
#   filter(!.n_pre_post == 1) |>
#   select(-.n_pre_post)
