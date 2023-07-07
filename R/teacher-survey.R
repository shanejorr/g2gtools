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
#' pre and post surveys. An additional column called `n_times_administered` is also added. This column
#' is an integer showing how many different surveys a question was administered in. For example, a
#' question will have a 2 if it was administered in the pre and post-training surveys.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_teacher_combine_pre_post <- function(pre_training_survey, post_training_survey) {

  # make sure we have the required column names
  required_columns <- c('email', 'question_stem', 'response_option', 'response', 'term')

  g2g_check_required_columns(pre_training_survey, required_columns)
  g2g_check_required_columns(post_training_survey, required_columns)

  all_results <- dplyr::bind_rows(pre_training_survey, post_training_survey) |>
    dplyr::mutate(
      # shorten text so that it plots better
      response_option = stringr::str_remove(.data[['response_option']], ", such as deep.*cycle"),
      email = stringr::str_to_lower(.data[['email']]) |> stringr::str_extract("^[^@]+"),
      response_option = stringr::str_replace(.data[['response_option']], "[.][.]", "."),
      response_option = stringr::str_replace(.data[['response_option']], " [.]", "."),
      response = g2g_to_title(.data[['response']])
    )

  # find questions common to each survey and label
  distinct_questions <- all_results |>
    dplyr::distinct(.data[['term']], .data[['question_stem']], .data[['response_option']]) |>
    dplyr::group_by_at(c('question_stem', 'response_option')) |>
    dplyr::summarise(
      in_survey = paste(unique(.data[['term']]), collapse = " | "),
      n_times_administered = dplyr::n_distinct(.data[['term']]),
      .groups = 'drop'
    )

  all_results |>
    dplyr::left_join(distinct_questions, by = c('question_stem', 'response_option')) |>
    dplyr::ungroup()

}

#' Identify how many times a teacher answered a question
#'
#' Helps identify how many times the teacher answered a question, which aids in identifying teachers
#' who did not answer a pre and post-training question on both the pre and post-training surveys.
#' Teachers are identfied by their email address in the `email` column.
#'
#' @param .data Data frame with pre and post-training data. created from `g2g_teacher_combine_pre_post()`
#'
#' @returns
#' the same data frame, with an additional column called `n_question_answers` that is an integer telling
#' how many times a teacher answered a question in total.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_number_times_teacher_answered <- function(.data) {

  # make sure we have the required column names
  required_columns <- c('email', 'question_stem', 'response_option')

  g2g_check_required_columns(.data, required_columns)

  .data |>
    dplyr::group_by_at(c('email', 'question_stem', 'response_option')) |>
    dplyr::mutate(n_question_answers = dplyr::n())

}

#' Plot the results of survey items that are only on one survey
#'
#' Items only on one survey will not have pre and post-training comparisons.
#'
#' @param .data A data set that contains a single question stem from either the pre or post-training teacher survey.
#' @param response_wrap An integer representing the length (number of characters) in each line of the y axis labels.
#'      These will be the questions.
#' @param title_wrap An integer representing the length (number of characters) of the title. This is the question stem.
#' @param pre_post_comparison The column name to use for pre and post comparisons. Use NULL if there is no pre or post comparison.
#' @param reverse_coded Boolean whether scales are reverse coded. Defaults to FALSE.
#'
#' @returns A ggplot object.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_teacher_viz_single_survey <- function(.data, response_wrap, title_wrap, pre_post_comparison = NULL, reverse_coded = FALSE) {

  # make sure we have the required column names
  required_columns <- c('response', 'response_option', '.percent', 'question_stem', 'term')

  g2g_check_required_columns(.data, required_columns)

  scales_to_use <- g2g_find_scale(.data$response, reverse_coded = reverse_coded)

  hex_colors <- names(scales_to_use) |>
    purrr::set_names(scales_to_use)

  plt_title <- stringr::str_wrap(unique(.data[['question_stem']]), title_wrap)

  plt <- .data |>
    # aggregate positive responses for plotting
    g2g_aggregate_positive_responses(scales_to_use[c(2,1)], pre_post_comparison, only_keep_first_response = TRUE) |>
    dplyr::mutate(
      response_option = stringr::str_wrap(.data[['response_option']], response_wrap),
      response_option = tidyr::replace_na(.data[['response_option']], ' '),
      response = factor(.data[['response']], levels = rev(scales_to_use))
    ) |>
    g2g_viz_stacked_bar_percent_horizontal(
      perc_value_var = '.percent',
      question_var= 'response_option',
      fill_var= 'response',
      text_var= '.strong_response_percent',
      color_pal = hex_colors,
      comparison_var = pre_post_comparison# ,
      # plot_title_position = 'plot'
    ) +
    ggplot2::labs(
      x = 'Percentage of respondents',
      y = NULL,
      fill = NULL,
      title = plt_title
    )

  return(plt)

}

#' Identify reverse coded teacher survey questions
#'
#' Adds a column to your data set showing whether a question is reverse coded (disagree is good).
#' Should be ran prior to aggregating data.
#'
#' @param .data Teacher survey data set
#'
#' @returns
#' The same data set with two modifications. First, there is an additional logical column added
#' to the data set called reverse_coded that is TRUE if the question is reverse coded, FALSE otherwise.
#' Second, an asterisk is placed at the end of the question stem
#'
#'
#' @importFrom rlang .data
#'
#' @export
g2g_teacher_reverse_coded <- function(.data) {

  # make sure we have the required column names
  required_columns <- c('question_stem', 'response_option')

  g2g_check_required_columns(.data, required_columns)

  reverse_coded_re <- c(
    # Please consider what you believe to be true about how students learn how to read,
    # and rate your agreement with the following statements
    "^Having repeated practice with reading strategies significantly improves",
    "^Students should primarily engage with texts only on their individual",
    "^Text complexity is solely determined by a quantitative measure",
    "^Each reading comprehension lesson should primarily",

    # To what extent do you agree with the following statements about students as readers
    "^Most students will learn to read on their own if given",
    "^Skilled readers rely on context clues and visual cues such as pictures",

    # Consider what you believe about how students improve as writers and rate your agreement with the following statements.
    "^The purpose of giving students a writing prompt is for",
    "^Grammar and syntax skills are best taught in isolation",
    "^The best approach to writing instruction is to",

    # Please consider what you believe to be true about how students learn STEM subjects, and rate your agreement with the following statements.
    "^Students should build fluency before building conceptual understanding",
    "^Student discourse should be focused on the how to get the right answer"
  ) |>
    stringr::str_c(collapse = "|")

  df <- .data |>
    dplyr::mutate(
      reverse_coded = ifelse(stringr::str_detect(.data[['response_option']], reverse_coded_re), TRUE, FALSE),
      question_stem = ifelse(stringr::str_detect(.data[['response_option']], reverse_coded_re), glue::glue("{.data[['question_stem']]}*"), .data[['question_stem']])
    )

  # output message showing reverse coded items
  reverse_code_message <- "The following questions were reverse coded:\n   "

  reverse_code_questions <- df |>
    dplyr::filter(stringr::str_detect(.data[['response_option']], reverse_coded_re)) |>
    dplyr::pull(.data[["response_option"]]) |>
    unique() |>
    stringr::str_c(collapse = "\n    ")

  message(stringr::str_c(reverse_code_message, reverse_code_questions))

  return(df)

}

#' Shorten survey questions by removing parenthesis
#'
#' Shorten survey question so that they plot easier. Do this by removing text within parenthesis
#' and shortening long questions.
#'
#' @param .data Teacher survey data set
#'
#' @returns
#' A vector with the questions shortened.
#'
#'
#' @importFrom rlang .data
#'
#' @export
g2g_teacher_shorten_questions <- function(.data) {

  .data |>
    dplyr::mutate(
      # remove sections of questions wrapped in parenthesis "(" or ")" to shorten questions for plotting
      response_option = stringr::str_remove(.data[["response_option"]], " [(].*[)]"),
      # shorten long questions
      response_option = dplyr::case_when(
        stringr::str_detect(.data[["response_option"]], "^When planning writing instruction") ~ stringr::str_remove(.data[["response_option"]], " for my grade level"),
        .default = .data[["response_option"]]
        )
      )

}

# Shorten full questions for x-axis.
#
# Summarize survey questions into a couple words so that they can be used for the x-axis on plots.
#
# @param question_col The column name, as a string, of the column containing the full questions.
#
# @returns
# A vector with the shortened questions.
#
# @importFrom rlang .data
#
# @export
# g2g_teacher_shorten_questions <- function(.data) {
#
#   dplyr::case_when(
#
#     # high expectations
#     stringr::str_detect(question_col, "fair to expect students in this class to master") ~ "Can master the standards by end of year",
#     stringr::str_detect(question_col, "One year is enough time") ~ "One year is enough time to master",
#     stringr::str_detect(question_col, "All students in my class can master the") ~ "All students can master the standards",
#     stringr::str_detect(question_col, "The standards are appropriate for the students") ~ "The standards are appropriate",
#
#     # ELA beliefs
#     # Please consider what you believe to be true about how students learn how to read, and rate
#     # your agreement with the following statements.
#     stringr::str_detect(question_col, "In ELA classes, a core responsibility of mine") ~ "A responsibility of mine is to build knowledge",
#     stringr::str_detect(question_col, "Having knowledge about a topic significantly improves a reader's comprehension ") ~ "Having kowledge about a topic",
#     stringr::str_detect(question_col, "Having repeated practice with reading strategies significantly") ~ "Having repeated practice with reading strategies",
#     stringr::str_detect(question_col, "Each reading comprehension lesson should") ~ "Focus on a single standard",
#     stringr::str_detect(question_col, "All students, regardless of level, should engage with the same anchor text") ~ "Engage with the same anchor text",
#     stringr::str_detect(question_col, "Questions about texts should focus on") ~ "Text-specific questions",
#     stringr::str_detect(question_col, "Students should primarily engage with texts only") ~ "Only engage in texts at reading level",
#     stringr::str_detect(question_col, "Text complexity is solely determined by") ~ "Text complexity is quantitatively determeined"
#
#   )
#
# }
