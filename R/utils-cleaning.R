#' Clean up column names from data sets imported from Google Forms surveys.
#'
#' @param column_names A string or vector of strings representing the column names we want to clean up.
#'
#' @importFrom rlang .data
#'
#' @keywords internal
g2g_clean_column_names <- function(column_names) {

  column_names |>
    # replace spaces with underscores
    stringr::str_replace_all(" |,", "_") |>
    # remove periods unless the period is the first character
    stringr::str_remove_all("(?<=.)\\.") |>
    # remove the following
    stringr::str_remove_all("[?]|[']|[)]|[(]|[:]") |>
    # convert to lower
    stringr::str_to_lower()

}

#' Ensure full question column only contains one set of open and closed brackets
#'
#' 'g2g_tidy_forms_survey' identifies the question stem and response options by looking for open and closed
#'      brackets. The text within the brackets is the response option. To work properly,
#'      there can only be one set of open and closed brackets. This function tests to make sure that
#'      there is only one set of brackets, if there are any at all.
#'
#' @param full_question_column Vector representing the column to test.
#'
#' @importFrom rlang .data
#'
#' @keywords internal
g2g_test_full_question_brackets <- function(full_question_column) {

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

    stop(error_text, call. = FALSE)

  }

  return(NULL)

}

#' Named list of scales used in Good to Great
#'
#' @keywords internal
g2g_list_of_scales <- function() {

  list(
    agree_disagree = c('Strongly Agree', 'Agree', 'Somewhat Agree', 'Somewhat Disagree', 'Disagree', 'Strongly Disagree'),
    acuerdo_desacuerdo = c('Muy de Acuerdo', 'De Acuerdo', 'Algo de Acuerdo', 'Algo en Desacuerdo', 'En Desacuerdo', 'Muy en Desacuerdo'),
    knowledge = c('Excellent Knowledge', 'Good Knowledge', 'Some Knowledge', 'A Little Knowledge', 'No Knowledge'),
    how_often = c('In All or Most Lessons', 'Often', 'Sometimes', 'Rarely', 'Never'),
    yes_notyet = c('Yes', 'Not Yet'),
    yes_mostly_somewhat_notyet = c('Yes', 'Mostly', 'Somewhat', 'Not Yet'),
    yes_but = c('Yes', 'Yes, But Only in Some Areas', 'Not Really', 'No'),
    almost = c('Almost Always', 'Often',  'Sometimes', 'Once in a While', 'Almost Never'),
    true = c('Very True', 'Mostly True', 'A Little True', 'Not True'),
    high = c('Extremely High', 'High', 'Somewhat High', 'Slightly High', 'Not High at All'),
    always_rarely = c('Always', 'Often', 'Sometimes', 'Rarely or Never'),
    all_few = c('All', 'Most', 'Some', 'Few or None')

  )

}

#' Return a vector of scales in the proper order with colors
#'
#' @param scale_name The name of the scale, as a string.
#' @param reverse_coded Logical, whether you want the revere coded version of the scale.
#'
#' @section Scale options:
#'
#' -  'agree_disagree': Strongly Agree, Agree, Somewhat Agree, Somewhat Disagree, Disagree, Strongly Disagree
#' -  'acuerdo_desacuerdo': Muy de Acuerdo, de Acuerdo, Algo de Acuerdo, Algo en Desacuerdo, en Desacuerd', Muy en Desacuerdo
#' -  'knowledge': Excellent Knowledge, Good Knowledge, Some Knowledge, A Little Knowledge, No Knowledge
#' -  'how_often': In All or Most Lessons', Often, Sometimes, Rarely, Never
#' -  'yes_notyet': Yes, Not Yet
#' -  'yes_mostly_somewhat_notyet': Yes, Mostly, Somewhat, Not Yet
#' -  'yes_but': Yes, 'Yes, But Only in Some Areas', Not Really, No
#' -  'almost': Almost Always, Often,  Sometimes, Once in a While, Almost Never
#' -  'true': Very True, Mostly True, A Little True, Not True
#' -  'high': Extremely High, High, Somewhat High, Slightly High, Not High at All
#' -  'always_rarely': Always', Often', Sometimes, Rarely or Never
#' -  'all_few': All, Most, Some, Few or None
#'
#'
#' @return A a named vector where the values are the scales in the proper order and the names are the
#' hex codes for colors.
#'
#' @examples
#' g2g_scale_order('agree_disagree')
#'
#' @importFrom rlang .data
#'
#' @export
g2g_scale_order <- function(scale_name, reverse_coded = FALSE) {

  # can only enter one scale name
  scale_length <- length(scale_name)
  if (length(scale_name) > 1) stop("You can only enter one value in `scale_name`", call. = FALSE)

  # NOTE: If you add scales, you need to add the name to 'g2g_find_scale
  scales <- g2g_list_of_scales()

  # make sure entered scale is one of the options
  scale_names <- names(scales)

  if (!scale_name %in% scale_names) {

    stop(paste0("Scale names must be one of: '", paste0(scale_names, collapse = "', '"), "'"), call. = FALSE)

  }

  single_scale <- scales[[scale_name]]

  scale_length <- length(single_scale)

  # palettes
  # for all palettes, the two highest values will be blue, the others will be gray
  gray_colors <- c("#eeeeee", "#cccccc", "#aaaaaa", "#888888")
  blue_colors <- c("#00A4C7", "#81D2EB")
  orange_colors <- c("#EA8835", "#FFC723")

  gray_length <- length(gray_colors)

  num_pos <- if (scale_length == 2) 1 else 2

  num_grays <- scale_length - num_pos

  if (!reverse_coded) {
    pal <- c(blue_colors[1:num_pos], gray_colors[(gray_length-(num_grays-1)):gray_length])

    final_scale  <- single_scale |>
      purrr::set_names(pal)

  } else if (reverse_coded) {
    pal <- c(gray_colors[(gray_length-(num_grays-1)):gray_length], orange_colors[1:num_pos])

    final_scale <- single_scale |>
      purrr::set_names(pal) |>
      rev()
  }

  return(final_scale)

}

#' Convert scale to a factor with levels in the proper order for plotting
#'
#' Given a vector of scales in your data and a list of possible scales, where each element in the
#' list is a different scale, the function will find the appropriate scale and convert the scale column
#' in your data to a factor with the levels in the proper order.
#'
#' @param scale_column A vector with the responses in your data containing scales that you want to
#'      convert to a factor.
#' @param reverse_coded Logical, whether you want the revere coded version of the scale.
#'
#' @return A vector with the same values as the input vector, \code{scale_column}, but converted to a
#' factor with the levels in the proper order
#'
#' @importFrom rlang .data
#'
#' @export
g2g_find_scale <- function(scale_column, reverse_coded = FALSE) {

  # set iterator because if we either have no matches (i == 0) or
  # multiple matches (i > 1) there is a problem
  matches <- 0

  scale_names <- names(g2g_list_of_scales())

  scales <- purrr::map(scale_names, g2g_scale_order, reverse_coded = reverse_coded) |>
    purrr::set_names(scale_names)

  for (i in seq.int(scales)) {

    # determine whether all the values in the scale column are in the list of scales
    # if all the values are in the list of scales, then nothing will be returned and length will be 0
    diff_length <- setdiff(scale_column[!is.na(scale_column)], scales[[i]]) |> length()

    # the current scale in the list of scales is not the right one if all the values in the scale column
    # are not in the list of scales, therefore, move to the next scale.
    if (diff_length != 0) next

    # we have the right scale if we are at this point
    # so save the scale as an object to return
    scale <- scales[[i]]
    matches <- matches + 1

  }

  # if none of the scales match, make the returned column the same as the input column
  # i.e. do not convet to a factor
  if (matches == 0) {

    stop(paste0(
      "None of the scales in `g2g_scale_order()` matched the scales in the data from `scale_column`.\n",
      "The scales in the data were: ", paste0(unique(scale_column), collapse = ", "), ".\n"
    ))

  }

  # it is a problem if we find more than one scale in scales_order() matching the scales in scale_column
  # throw error if this occurs
  if (matches > 1) stop("Your scales matched more than one option from `scales_order()`. Please ensure they only match one option", call. = FALSE)

  return(scale)

}

#' Identify participants who have values in pre and post tools
#'
#' Analysis often require comparing pre and post metrics, only using participants in both the
#' pre and post datasets. For example, we might calculate changes in high expectations scores
#' between pre and post, only using teachers who took pre and post surveys. This function identifies
#' participants with pre and post observations and returns a vector with their names / emails / id numbers.
#'
#' @param .data A single data set containing pre and post data, in long form where pre and post are
#'      in different rows.
#' @param participants The column name, as a string, that identifies unique participants. This could
#'      be names, emails or other identifiers.
#' @param pre_post_col The column name, as a string, that identifies whether a row is a pre or post
#'      training observations.
#'
#' @returns A vector of values from the \code{participants} column that includes participants in the
#'      pre and post data sets.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_id_pre_post <- function(.data, participants, pre_post_col) {

  .data |>
    g2g_compare_names(participants, pre_post_col) |>
    dplyr::filter(.data[['.n_obs_total']] != 1) |>
    dplyr::pull(.data[[participants]]) |>
    unique()

}

#' Check participant names for consistency
#'
#' Analysis often require matching pre and post participants. An antecedent step is ensuring unique
#' identifiers such as names or email addresses are consistent across pre and post data sets.
#' This function returns a data set with unique pre and post participants, so that users can easily
#' manually check whether unique identifiers are consistent.
#'
#' @param .data A single data set containing pre and post data, in long form where pre and post are
#'      in different rows.
#' @param participants The column name or names, as a string or vector of strings, that identify unique participants. This could
#'      be names, emails or other identifiers.
#' @param pre_post_col The column name, as a string, that identifies whether a row is a pre or post
#'      training observations.
#'
#' @return A dataframe containing unique pre and post participants, sorted so that users can manually
#'      check for consistent spelling.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_compare_names <- function(.data, participants, pre_post_col) {

  .data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(participants)), .data[[pre_post_col]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(participants))) |>
    dplyr::mutate(
      .n_obs_total = dplyr::n(),
      .obs_num = dplyr::row_number(),
      .group_id = dplyr::cur_group_id()
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(participants)), .data[['.obs_num']], .data[[pre_post_col]])

}

#' Sum percentage totals of positive responses
#'
#' We often want to sum the percentage of respondents answering favorably on a survey, for example
#' answering 'Agree' or 'Strongly Agree'. This function calculates these values and returns them as
#' a column in the original data. The returned data frame will have the same number of rows as the
#' input data frame, but it will have an additional column calls \code{.strong_response_percent}.
#'
#' @param .data Input data frame. It must contain the aggregate percentage for each response option in long form.
#'      For example, each row must be a question and response option combination (question 1, strongly agree).
#'      It cannot be the raw data where each row is a response. The data set must be created with \code{g2g_forms_survey_calc_percentages()}.
#' @param positive_responses A string vector of positive responses, for example \code{c('Agree', 'Strongly Agree')}
#' @param grouping_terms Any columns that you want to group by, such as years, demographics, or pre or post training. Represented
#'      as a vector of strings. Defaults to `NULL`, which means there are no groupings.
#' @param only_keep_first_response Boolean on whether the aggregate percentages should show up in all rows
#'     or only the rows represented by the first response in \code{positive_responses}. Setting this to
#'     \code{TRUE} is useful in stacked bar charts where you only want to show the percentages once and
#'     have it mapped to one of the positive response options. Defaults to \code{FALSE}.
#'
#' @return The original data frame with an additional column titles \code{.strong_response_percent} that shows the
#'      percentage of respondents who answered favorably (if the row represents a favorable response)
#'      or unfavorable (if the row represents an unfavorable response).
#'
#' @examples
#' results <- teacher_pre_survey |>
#'   g2g_tidy_forms_survey(8:30, 3) |>
#'   dplyr::mutate(assessment = 'Pre-survey') |>
#'   g2g_forms_survey_calc_percentages('assessment') |>
#'   # the function should only be used on a data set with common scales
#'   # so, filter to only keep a single question stem since we know it will have the same scales
#'   dplyr::filter(stringr::str_detect(.data[['question_stem']], 'To what extent do you agree'))
#'
#' g2g_aggregate_positive_responses(results, c('Agree', 'Strongly Agree'), 'assessment')
#'
#' @importFrom rlang .data
#'
#' @export
g2g_aggregate_positive_responses <- function(.data, positive_responses, grouping_terms = NULL, only_keep_first_response = FALSE) {

  all_grouping_terms <- c('question_stem', 'response_option', grouping_terms, '.scale_strength')

  .data <- .data |>
    dplyr::ungroup() |>
    dplyr::mutate(.scale_strength = ifelse(.data[['response']] %in% !!positive_responses, 'Strong response', 'Weak response')) |>
    dplyr::group_by_at(all_grouping_terms) |>
    dplyr::mutate(.strong_response_percent = sum(.data[['.percent']])) |>
    dplyr::ungroup()

  if (only_keep_first_response) {

    .data <- .data |>
      dplyr::group_by_at(all_grouping_terms[-length(all_grouping_terms)]) |>
      dplyr::mutate(
        .group_id = dplyr::row_number(),
        .has_strong_response = 'Strong response' %in% .data[['.scale_strength']]
      ) |>
      dplyr::group_by_at(all_grouping_terms) |>
      dplyr::mutate(
        .strong_response_percent = dplyr::case_when(
          .data$.has_strong_response & .data$.scale_strength == 'Strong response' & .data$.group_id == min(.data$.group_id) ~ .data$.strong_response_percent,
          !.data$.has_strong_response & .data$.scale_strength == 'Weak response' & .data$.group_id == min(.data$.group_id) ~ 0,
          TRUE ~ NA_real_
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-c(".group_id", ".has_strong_response"))

  }

  return(.data)

}

#' Convert to title case but leave common words uncapitalized
#'
#' When converting to title case, most R function fail to properly account for words that should
#' not be capitalized in title case such as 'and' and 'the'. \code{g2g_to_title} converts to title
#' case, but does not convert common words. Among other purposes, the function is useful for
#' placing scales in the case that is required by most g2g tools.
#'
#' @param x Vector that you want converted to title case.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_to_title <- function(x) {

  words_not_to_convert <- c(
    'and', 'a', 'the', 'as', 'for', 'if', 'nor', 'or', 'so', 'de', 'en',
    'as', 'at', 'by', 'for', 'in', 'of', 'off', 'on', 'per', 'to', 'up', 'via'
  )

  words_not_to_convert <- stringr::str_c(" ", words_not_to_convert, " ")

  replacement_strings <- stringr::str_to_title(words_not_to_convert) |>
    purrr::set_names(words_not_to_convert)

  replacement_strings <- words_not_to_convert |>
    purrr::set_names(stringr::str_to_title(words_not_to_convert))

  convert_to_title <- x |>
    stringr::str_to_lower() |>
    stringr::str_to_title()

  convert_to_title <- stringr::str_c(convert_to_title, " ") |>
    stringr::str_replace_all(replacement_strings) |>
    stringr::str_trim(side = 'right')

  return(convert_to_title)

}

#' Create key-value pairs of site information
#'
#' Site information is injected into an analysis at multiple points. This function takes the site name,
#' semester, year, and subject as parameters and returns a named list with standard site information
#' that can be refered to in a common format.
#'
#' @param site_name The name of the site, as a string, and in a style that it would appear in a
#'      presentation. For example, this means that the site name will likely be in title case.
#' @param semester The semester, as a string, in title case.
#' @param year The year as a integer.
#' @param subject The subject in title case.
#'
#' @returns
#' A named list that contains containing items such as site names and file paths, to be used throughout a single Good to Great.
#' Use `names(object_name)` to see list of items.
#'
#' @examples
#' g2g_site_information('Bethune', 'Spring', '2022', 'ELA')
#'
#' @importFrom rlang .data
#'
#' @export
g2g_site_information <- function(site_name, semester, year, subject) {

  site_name_lower <- stringr::str_replace_all(site_name, " ", "_") |>
    stringr::str_to_lower()

  semester_lower <- stringr::str_replace_all(semester, " ", "_") |>
    stringr::str_to_lower()

  subject_lower <- stringr::str_replace_all(subject, " ", "_") |>
    stringr::str_to_lower()

  site_info <- list(
    site_name_title = site_name,
    site_name_lower = site_name_lower,
    semester_title = semester,
    semester_lower = semester_lower,
    year = year,
    subject_title = subject,
    subject_lower = subject_lower,
    ppt_title = glue::glue("{site_name} {semester} {year} {subject} G2G")
  )

  site_path <- here::here(glue::glue("{site_info$semester_lower}-{site_info$year}"), site_info$site_name_lower)
  ppt_obs_filename <- glue::glue("Observations - {site_info$ppt_title} - {lubridate::today()}.pptx")
  ppt_teacher_filename <- glue::glue("Teacher Suvey - {site_info$ppt_title} - {lubridate::today()}.pptx")
  ppt_student_survey_filename <- glue::glue("Student Survey - {site_info$ppt_title} - {lubridate::today()}.pptx")
  ppt_all_filename <- glue::glue("{site_info$ppt_title} - Analysis - {lubridate::today()}.pptx")
  landscape_filename <- glue::glue("landscape-{site_name_lower}-{site_info$semester_lower}-{site_info$year}.html")
  data_path <- here::here(site_path, 'data')

  site_info$file_paths <- list(
    site_path = site_path,
    obs_data_filename = here::here(data_path, glue::glue('{site_info$site_name_lower}-{site_info$semester_lower}_{site_info$year}-obs.csv')),
    pre_teacher_filename = here::here(data_path, glue::glue('{site_info$site_name_lower}-{site_info$semester_lower}_{site_info$year}-teacher-pre.csv')),
    post_teacher_filename = here::here(data_path, glue::glue('{site_info$site_name_lower}-{site_info$semester_lower}_{site_info$year}-teacher-post.csv')),
    student_survey_filename = here::here(data_path, glue::glue('{site_info$site_name_lower}-{site_info$semester_lower}_{site_info$year}-student_survey.csv')),
    ppt_obs_filename = here::here(site_path, 'ppt', ppt_obs_filename),
    ppt_teacher_filename = here::here(site_path, 'ppt', ppt_teacher_filename),
    ppt_student_survey_filename = here::here(site_path, 'ppt', ppt_student_survey_filename),
    ppt_all_filename = here::here(site_path, 'ppt', ppt_all_filename),
    landscape_filepath = here::here(site_path, 'landscape', landscape_filename)

  )

  return(site_info)

}

#' Pull data from Google Sheets and add descriptive columns
#'
#' Pulls data from Google Forms and adds columns to the data signifying the site, semester, subject, and tool
#'
#' @param url The url to the Google sheet
#' @param tool_name The name of the tool ('Observations', 'Post-Training')
#' @param site_info A list created by `g2g_site_information()` containing site information.
#'
#' @returns
#' A tibble with the data from the Google Sheet and additional columns called: `year`, `site_name`, `semester`,
#' `subject`, and `tool`
#'
#' @importFrom rlang .data
#'
#' @export
g2g_pull_data <- function(url, tool_name, site_info) {

  googlesheets4::read_sheet(url) |>
    dplyr::mutate(
      year = !!site_info[['year']],
      site_name = !!site_info[['site_name_title']],
      semester = !!site_info[['semester_year']],
      subject = !!site_info[['subject_title']],
      tool = !!tool_name
    )

}

#' Check for required columns
#'
#' @param .data The data set that we want to check for the required columns
#' @param required_columns A vector of strings representing required columns names
#'
#' @returns
#' Returns NULL if all columns are present. Otherwise, produces an error.
g2g_check_required_columns <- function(.data, required_columns) {

  # create a logical vector the same length as required_columns showing whether the column is present
  cols_present <- rlang::has_name(.data, required_columns)

  if (!all(cols_present)) {

    missing_columns <- required_columns[!cols_present]
    error_message <- stringr::str_c("Your data is missing the following required columns: '", stringr::str_c(missing_columns, collapse = "','"), "'")
    stop(error_message, call. = TRUE)

  }

  return(NULL)

}
