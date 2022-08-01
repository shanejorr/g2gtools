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

    knowledge = c('Excellent knowledge', 'Good Knowledge', 'Some Knowledge', 'A Little Knowledge', 'No Knowledge'),

    how_often = c('In All or Most Lessons', 'Often', 'Sometimes', 'Rarely', 'Never'),

    yes_notyet = c('Yes', 'Mostly', 'Somewhat', 'Not Yet'),

    yes_but = c('Yes', 'Yes, But Only in Some Areas', 'Not Really', 'No')
  )

}

#' Return a vector of scales in the proper order with colors
#'
#' @param scale_name The name of the scale, as a string.
#'
#' @section Scale options:
#'
#' -  'agree_disagree': Strongly Agree, Agree, Somewhat Agree, Somewhat Disagree, Disagree, Strongly Disagree
#' -  'knowledge': Excellent Knowledge, Good Knowledge, Some Knowledge, A Little Knowledge, No Knowledge
#' -  'how_often': In All or Most Lessons', Often, Sometimes, Rarely, Never
#' -  'yes_notyet': Yes, Mostly, Somewhat, Not yet
#' -  'yes_but': Yes, 'Yes, But Only in Some Areas', Not Really, No)
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
g2g_scale_order <- function(scale_name) {

  # note: need test

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
  gray_colors <- c("#E2E2E2", "#C2C2C2", "#8A8A8A", "#474747")
  blue_colors <- c("#00A4C7", "#81D2EB")

  gray_length <- length(gray_colors)

  num_grays <- scale_length - 2

  pal <- c(blue_colors, gray_colors[(gray_length-(num_grays-1)):gray_length])

  single_scale |>
    purrr::set_names(pal)

}

#' Convert scale to a factor with levels in the proper order for plotting
#'
#' Given a vector of scales in your data and a list of possible scales, where each element in the
#' list is a different scale, the function will find the appropriate scale and convert the scale column
#' in your data to a factor with the levels in the proper order.
#'
#' @param scale_column A vector with the responses in your data containing scales that you want to
#'      convert to a factor.
#'
#' @return A vector with the same values as the input vector, \code{scale_column}, but converted to a
#' factor with the levels in the proper order
#'
#' @importFrom rlang .data
#'
#' @export
g2g_find_scale <- function(scale_column) {

  # set iterator because if we either have no matches (i == 0) or
  # multiple matches (i > 1) there is a problem
  matches <- 0

  scale_names <- names(g2g_list_of_scales())

  scales <- purrr::map(scale_names, g2g_scale_order) |>
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
    dplyr::filter(.data[['.n']] != 1) |>
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
#' @param participants The column name, as a string, that identifies unique participants. This could
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
    dplyr::distinct(.data[[participants]], .data[[pre_post_col]]) |>
    dplyr::group_by(.data[[participants]]) |>
    dplyr::mutate(
      .n = dplyr::n(),
      .id = dplyr::row_number(),
      .group_id = dplyr::cur_group_id()
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data[[participants]], .data[['.id']], .data[[pre_post_col]], )

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
#'      as a vector of strings.
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
g2g_aggregate_positive_responses <- function(.data, positive_responses, grouping_terms, only_keep_first_response = FALSE) {

  all_grouping_terms <- c('question_stem', 'response_option', grouping_terms, '.scale_strength')

  .data <- .data |>
    dplyr::ungroup() |>
    dplyr::mutate(.scale_strength = ifelse(.data[['response']] %in% !!positive_responses, 'Strong response', 'Weak response')) |>
    dplyr::group_by_at(all_grouping_terms) |>
    dplyr::mutate(.strong_response_percent = sum(.data[['.percent']])) |>
    dplyr::ungroup()

  if (only_keep_first_response) {

    .data <- .data |>
      dplyr::mutate(.strong_response_percent = ifelse(.data[['response']] == positive_responses[1], .data[['.strong_response_percent']], NA_real_))

  }

  return(.data)

}

#' Convert to title case but leave common words uncapitalized
#'
#' When converting to title case, most R function fail to proeprly account for words that should
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
    'and', 'a', 'the', 'as', 'but', 'for', 'if', 'nor', 'or', 'so', 'yet',
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
