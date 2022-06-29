#' Clean up column names from data sets imported from Google Forms surveys.
#'
#' @param column_names A string or vector of strings representing the column names we want to clean up.
#'
#' @importFrom rlang .data
#'
#' @keywords internal
clean_column_names <- function(column_names) {

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
#' 'tidy_forms_survey' identifies the question stem and response options by looking for open and closed
#'      brackets. The text within the brackets is the response option. To work properly,
#'      there can only be one set of open and closed brackets. This function tests to make sure that
#'      there is only one set of brackets, if there are any at all.
#'
#' @param full_question_column Vector representing the column to test.
#'
#' @importFrom rlang .data
#'
#' @keywords internal
test_full_question_brackets <- function(full_question_column) {

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

#' A list containing the order of scales in surveys and observations
#'
#' Returns a named list where each element is a different scale in the proper order. Useful for setting
#' the order of scales for plotting.
#'
#' @param scale_name The name of the scale, as a string, or a vector of scale names. Use 'get scale names'
#'     to retrieve a list of possible names.
#'
#' @return A named list where each element contains scales in the proper order. A single vector
#'      of scales can be returned by subsetting on the name of the scale: \code{scale_order()['agree_disagree']}
#'
#' @examples
#' scale_order('agree_disagree')
#'
#' @importFrom rlang .data
#'
#' @export
scale_order <- function(scale_name) {

  # note: need test

  # palettes
  likert_6_agree <- rev(tntpr::colors_tntp[c('likert_1', 'likert_2', 'likert_3', 'likert_5', 'likert_6', 'likert_7')])
  likert_5 <- tntpr::colors_tntp[c('likert_7', 'likert_6', 'likert_5', 'likert_4', 'default_7')]

  # can only enter one scale name
  if (length(scale_name) > 1) stop("You can only enter one value in `scale_name`", call. = FALSE)

  scales <- list(
    agree_disagree = c('Strongly Agree', 'Agree', 'Somewhat Agree', 'Somewhat Disagree', 'Disagree', 'Strongly Disagree') |>
      purrr::set_names(likert_6_agree),

    strongly_agree = c('Strongly Agree', 'Agree') |>
      purrr::set_names(likert_6_agree[1:2]),

    only_agree_and_strongly = c('Agree / Strongly Agree') |>
      purrr::set_names(likert_6_agree[2]),

    knowledge = c('Excellent knowledge', 'Good knowledge', 'Some knowledge', 'A little knowledge', 'No knowledge') |>
      purrr::set_names(likert_5),

    how_often = c('In all or most lessons', 'Often', 'Sometimes', 'Rarely', 'Never') |>
      purrr::set_names(likert_5),

    obs_yes_notyet = c('Yes', 'Mostly', 'Somewhat', 'Not Yet') |>
      purrr::set_names(likert_5[1:4]),

    obs_yesbut = c('Yes', 'Yes, But Only In Some Areas', 'Not Really', 'No') |>
      purrr::set_names(likert_5[1:4])
  )

  get_scale <- 'get scale names'
  scale_options <- c(names(scales), get_scale)

  if (any(!scale_name %in% scale_options)) {
    stop(glue::glue("`scale_name` must be one of: '", paste0(scale_options, collapse = "', '"), "'."), call. = FALSE)
  }

  if (any(scale_name == get_scale)) {
    return_scale <- names(scales)
  } else {
    return_scale <- scales[[scale_name]]
  }

  return(return_scale)

}

#' Create a named vector from scales where the values are colors and names are the scales
#'
#' To create manual fills in ggplot, you need a named vector with the colors as values and the scale names
#' as the names of the vector. This function takes a named vector where the scales values are values
#' colors are names, and switches names and values. The returned vector can then be used in ggplot
#'
#' @param scale_name name from \code{scale_order()}.
#'
#' @examples
#' create_color_scales('obs_yes_notyet')
#'
#' @returns A named vector with the scales as names and hex values as values.
#'
#' @importFrom rlang .data
#'
#' @export
create_color_scales <- function(scale_name) {

  scales <- scale_order(scale_name)

  names(scales) |>
    purrr::set_names(scales)

}

#' Convert scale to a factor with levels in the proper order for plotting
#'
#' Given a vector of scales in your data and a list of possible scales, where each element in the
#' list is a different scale, the function will find the appropriate scale and convert the scale column
#' in your data to a factor with the levels in the proper order.
#' Find scale of common G2G questions
#'
#' @param scale_column A vector with the responses in your data containing scales that you want to
#'      convert to a factor.
#' @param use_agree_disagree For the strongly agree to strongly disagree scale, whether to use all
#'      six points on the Liekrt scale or only agree and strongly agree.
#'
#' @return A vector with the same values as the input vector, \code{scale_column}, but converted to a
#' factor with the levels in the proper order
#'
#' @importFrom rlang .data
#'
#' @export
find_scale <- function(scale_column, use_agree_disagree) {

  # set iterator because if we either have no matches (i == 0) or
  # multiple matches (i > 1) there is a problem
  matches <- 0

  scale_names <- c('agree_disagree', 'strongly_agree', 'only_agree_and_strongly', 'knowledge', 'how_often')

  scales <- purrr::map(scale_names, scale_order) |>
    purrr::set_names(scale_names)

  # iterate through each scale in the list of scales
  package_scales <- if (use_agree_disagree) scales[-2] else scales[-1]

  for (i in seq.int(package_scales)) {

    # determine whether all the values in the scale column are in the list of scales
    # if all the values are in the list of scales, then nothing will be returned and length will be 0
    diff_length <- setdiff(scale_column[!is.na(scale_column)], package_scales[[i]]) |> length()

    # the current scale in the list of scales is not the right one if all the values in the scale column
    # are not in the list of scales, therefore, move to the next scale.
    if (diff_length != 0) next

    # we have the right scale if we are at this point
    # so save the scale as an object to return
    scale <- package_scales[[i]]
    matches <- matches + 1

  }

  # if none of the scales match, make the returned column the same as the input column
  # i.e. do not convet to a factor
  if (matches == 0) {

    stop(paste0(
      "None of the scales in `scale_order()` matched the scales in the data from `scale_column`.\n",
      "The scales in the data were: ", paste0(unique(scale_column), collapse = ", "), ".\n"
    ))

  }

  # it is a problem if we find more than one scale in scales_order() matching the scales in scale_column
  # throw error if this occurs
  if (matches > 1) stop("Your scales matched more than one option from `scales_order()`. Please ensure they only match one option", call. = FALSE)

  return(scale)

}
