#' Clean up column names from data sets imported from Google Forms surveys.
#'
#' @param column_names A string or vector of strings representing the column names we want to clean up.
#'
#' @keywords internal
clean_column_names <- function(column_names) {

  column_names |>
    # replace spaces with underscores
    stringr::str_replace_all(" ", "_") |>
    # remove periods unless the period is the first character
    stringr::str_remove_all("(?<=.)\\.") |>
    # remove question marks
    stringr::str_remove_all("[?]") |>
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
#' Included scales are:
#'  - 'agree_disagree': 6 point likert scale from strongly agree to strongly disagree.
#'  - 'knowledge': 5 point scale from excellent knowledge to no knowledge.
#'  - 'how_often': 5 point scale from in all or most lessons to never to.
#'
#' @return A named list where each element contains scales in the proper order. A single vector
#'      of scales can be returned by subsetting on the name of the scale: \code{scale_order()['agree_disagree']}
#'
#' @examples
#' scale_order() # for a named list with all scales
#' scale_order()['agree_disagree'] # for a vector containg a single scale
#'
#'@export
scale_order <- function() {

  list(
    agree_disagree = c('Strongly Agree', 'Agree', 'Somewhat Agree', 'Somewhat Disagree', 'Disagree', 'Strongly Disagree'),
    knowledge = c('Excellent knowledge', 'Good knowledge', 'Some knowledge', 'A little knowledge', 'No knowledge'),
    how_often = c('In all or most lessons', 'Often', 'Sometimes', 'Rarely', 'Never')
  )

}

#' Convert scale to a factor with levels in the proper order for plotting
#'
#' Given a vector of scales in your data and a list of possible scales, where each element in the
#' list is a different scale, the function will find the appropriate scale and convert the scale column
#' in your data to a factor with the levels in the proper order.
#'
#' @param scale_column A vector with the responses in your data containing scales that you want to
#'      convert to a factor.
#' @param list_of_scales A list where each element is a vector of ordered scales.
#'
#' @examples
#' list_of_scales <- list(
#'     agree_disagree = c('Strongly Agree', 'Agree', 'Somewhat Agree',
#'                        'Somewhat Disagree', 'Disagree', 'Strongly Disagree'),
#'     knowledge = c('Excellent knowledge', 'Good knowledge', 'Some knowledge',
#'                   'A little knowledge', 'No knowledge')
#' )
#' column_of_scales <- c('Somewhat Disagree', 'Disagree', 'Agree')
#' scale_to_factor(column_of_scales, list_of_scales)
#'
#' @return A vector with the same values as the input vector, \code{scale_column}, but converted to a
#' factor with the levels in the proper order.
#'
#' @export
scale_to_factor <- function(scale_column, list_of_scales) {

  # set iterator because if we either have no matches (i == 0) or
  # multiple matches (i > 1) there is a problem
  i <- 0

  # iterate through each scale in the list of scales
  for (single_scale in list_of_scales) {

    # determine whether all the values in the scale column are in the list of scales
    # if all the values are in the list of scales, then nothing will be returned and length will be 0
    diff_length <- setdiff(scale_column, single_scale) |> length()

    # the current scale in the list of scales is not the right one if all the values in the scale column
    # are not in the list of scales, therefore, move to the next scale.
    if (diff_length != 0) next

    # we have the right scale if we are at this point
    # so convert to factor with the current scale in the iteratin
    factor_column <- factor(scale_column, levels = single_scale)

    i <- i + 1

  }

  # if none of the scales match, make the returned column the same as the input column
  # i.e. do not convet to a factor
  if (i == 0) {
    factor_column <- scale_column
    message(paste0(
      "None of the scales in `list_of_scales` matched the scales in the data from `scale_column`.\n",
      "The scales in the data were: ", paste0(unique(scale_column), collapse = ", "), ".\n",
      "Therefore, returning the original data in the same format and as the same data type."
    ))
  }
  # it is a problem if we find more than one scale in scales_order() matching the scales in scale_column
  # throw error if this occurs
  if (i > 1) stop("Your scales matched more than one option from `scales_order()`. Please ensure they only match one option", call. = FALSE)

  return(factor_column)

}
