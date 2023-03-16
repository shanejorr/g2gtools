# Functions specific to the teacher survey

#' Combine pre and post-training surveys and conduct light cleaning
#'
#' Combine the pre and post-training surveys. Then identify question stems in the pre and post training surveys,
#' and clean up emails and response options.
#'
#' @param pre_training_survey The pre-training survey, after running `g2g_tidy_forms_survey()`
#' @param post_training_survey The post-training survey, after running `g2g_tidy_forms_survey()`
#'
#' @returns
#' A single data frame with the pre and post-training surveys combined. An additional column is added to
#' the data called `in_survey`. The column will contain "Pre and Post Survey" if the question is in the
#' pre and post surveys.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_teacher_combine_pre_post <- function(pre_training_survey, post_training_survey) {

  # make sure we have the required column names
  required_columns <- c('email', 'response_option')

  g2g_check_required_columns(pre_training_survey, required_columns)
  g2g_check_required_columns(post_training_survey, required_columns)

  # identify question stems in the pre and post-training data
  common_stems <- intersect(unique(pre_training_survey$question_stem), unique(post_training_survey$question_stem))

  dplyr::bind_rows(pre_training_survey, post_training_survey) |>
    dplyr::mutate(
      # shorten text so that it plots better
      response_option = stringr::str_remove(.data[['response_option']], ", such as deep.*cycle"),
      in_survey = ifelse(.data[['question_stem']] %in% !!common_stems, "Pre and Post Survey", .data[['term']]),
      email = stringr::str_to_lower(.data[['email']]) |> stringr::str_extract("[a-z]*(?=[@]?)"),
      response_option = stringr::str_replace(.data[['response_option']], "[.][.]", "."),
      response_option = stringr::str_replace(.data[['response_option']], " [.]", "."),
      response = g2g_to_title(.data[['response']])
    )

}

#' Plot the results of survey items that are only on one survey
#'
#' Items only on one survey will not have pre and post-training comparisons.
#'
#' @param .data A data set that contains a single question stem from either the pre or post-training teacher survey.
#' @param response_wrap An integer representing the length (number of characters) in each line of the y axis labels.
#'      These will be the questions.
#' @param title_wrap An integer representing the length (number of characters) of the title. This is the question stem.
#'
#' @returns A ggplot object.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_teacher_viz_single_survey <- function(.data, response_wrap, title_wrap) {

  # make sure we have the required column names
  required_columns <- c('response', 'in_survey', 'response_option', '.percent', 'question_stem')

  g2g_check_required_columns(.data, required_columns)

  scales_to_use <- g2g_find_scale(.data$response)

  hex_colors <- names(scales_to_use) |>
    purrr::set_names(scales_to_use)

  plt_height <- g2g_ppt_calculate_plot_height(.data$response_option)

  plt_title <- stringr::str_wrap(unique(.data[['question_stem']]), title_wrap)

  plt <- .data |>
    # aggregate positive responses for plotting
    g2g_aggregate_positive_responses(scales_to_use[c(2,1)], NULL, only_keep_first_response = TRUE) |>
    dplyr::mutate(
      response_option = stringr::str_wrap(.data[['response_option']], response_wrap),
      response_option = tidyr::replace_na(.data[['response_option']], ' '),
      response = factor(.data[['response']], levels = rev(scales_to_use))
    ) |>
    g2g_viz_stacked_bar_percent(
      x_var = '.percent', y_var = 'response_option',
      text_var = '.strong_response_percent', fill_var = 'response', color_pal = hex_colors
    ) +
    ggplot2::labs(
      x = 'Percentage of respondents',
      y = NULL,
      fill = NULL,
      title = plt_title
    )

  return(plt)

}
