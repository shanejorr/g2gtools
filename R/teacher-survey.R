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
  required_columns <- c('email', 'response_option', 'response', 'term')

  g2g_check_required_columns(pre_training_survey, required_columns)
  g2g_check_required_columns(post_training_survey, required_columns)

  # identify question stems in the pre and post-training data
  common_stems <- intersect(unique(pre_training_survey$question_stem), unique(post_training_survey$question_stem))

  dplyr::bind_rows(pre_training_survey, post_training_survey) |>
    dplyr::mutate(
      # shorten text so that it plots better
      response_option = stringr::str_remove(.data[['response_option']], ", such as deep.*cycle"),
      in_survey = ifelse(.data[['question_stem']] %in% !!common_stems, "Pre and Post Survey", .data[['term']]),
      email = stringr::str_to_lower(.data[['email']]) |> stringr::str_extract("^[^@]+"),
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
  required_columns <- c('response', 'response_option', '.percent', 'question_stem')

  g2g_check_required_columns(.data, required_columns)

  scales_to_use <- g2g_find_scale(.data$response)

  hex_colors <- names(scales_to_use) |>
    purrr::set_names(scales_to_use)

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

#' Plot pre and post-training comparisons of survey items
#'
#' The data plotted should have pre and post-training comparisons with a column called `term`
#' identifying pre and post-training.Reverse coded questions should contain different question stems
#' than non-reverse coded questions.
#'
#' @param .data A data set that contains a single question stem from either the pre or post-training teacher survey.
#' @param response_wrap An integer representing the length (number of characters) in each line of the y axis labels.
#'      These will be the questions.
#' @param title_wrap An integer representing the length (number of characters) of the title. This is the question stem.
#' @param reverse_coded Logical, indicating whether question stem contains reverse coded questions. Defaults to TRUE.
#'
#' @returns A ggplot object.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_teacher_viz_pre_post <- function(.data, response_wrap, title_wrap, reverse_coded = FALSE) {

  # make sure we have the required column names
  required_columns <- c('response', 'response_option', '.percent', 'question_stem', 'term')

  g2g_check_required_columns(.data, required_columns)

  scales_to_use <- g2g_find_scale(.data$response, reverse_coded = reverse_coded)

  hex_colors <- names(scales_to_use) |>
    purrr::set_names(scales_to_use)

  plt_title <- stringr::str_wrap(unique(.data[['question_stem']]), title_wrap)

  plt <- .data |>
    # aggregate positive responses for plotting
    g2g_aggregate_positive_responses(scales_to_use[c(2,1)], 'term', only_keep_first_response = TRUE) |>
    dplyr::mutate(
      response_option = stringr::str_wrap(.data[['response_option']], response_wrap),
      response_option = tidyr::replace_na(.data[['response_option']], ' '),
      response = factor(.data[['response']], levels = rev(scales_to_use))
    ) |>
    g2g_viz_stacked_bar_percent(
      x_var = '.percent', y_var = 'response_option',
      text_var = '.strong_response_percent', fill_var = 'response', color_pal = hex_colors
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data[['term']]), ncol = 2) +
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
    "^The best approach to writing instruction is to"
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

#' Shorten full questions for x-axis.
#'
#' Summarize survey questions into a couple words so that they can be used for the x-axis on plots.
#'
#' @param question_col The column name, as a string, of the column containing the full questions.
#'
#' @returns
#' A vector with the shortened questions.
#'
#'
#' @importFrom rlang .data
#'
#' @export
g2g_teacher_shorten_questions <- function(.data) {

  dplyr::case_when(

    # high expectations
    stringr::str_detect(question_col, "fair to expect students in this class to master") ~ "Can master the standards by end of year",
    stringr::str_detect(question_col, "One year is enough time") ~ "One year is enough time to master",
    stringr::str_detect(question_col, "All students in my class can master the") ~ "All students can master the standards",
    stringr::str_detect(question_col, "The standards are appropriate for the students") ~ "The standards are appropriate",

    # math last unit taught
    # Think about the last unit you taught. How often did you do the following?
    stringr::str_detect(question_col, "Build on prior skills and knowledge") ~ "Build on prior skills and knowledge",
    stringr::str_detect(question_col, "Ground procedures and formulas in conceptual") ~ "Ground procedures and formulas in conceptual understanding",
    stringr::str_detect(question_col, "Use repeated practice to improve") ~ "Use repeated practice",
    stringr::str_detect(question_col, "Use questions and problems that are from") ~ "Use items from the textbook/curriculum",
    stringr::str_detect(question_col, "Use materials that I have found or created") ~ "Use found or created materials",
    stringr::str_detect(question_col, "Provide feedback to help students revise") ~ "Provide feedback to help students revise work",
    stringr::str_detect(question_col, "Emphasize one solution method") ~ "Emphasize one solution method",
    stringr::str_detect(question_col, "Check for understanding throughout the lesson") ~ "Check for understanding",
    stringr::str_detect(question_col, "Summarize the lesson with references") ~ "Summarize the lesson",
    stringr::str_detect(question_col, "mathematical language by modeling proper use of relevant forms") ~ "Develop students’ mathematical language",

    # ELA beliefs
    # Please consider what you believe to be true about how students learn how to read, and rate
    # your agreement with the following statements.
    stringr::str_detect(question_col, "In ELA classes, a core responsibility of mine") ~ "A responsibility of mine is to build knowledge",
    stringr::str_detect(question_col, "Having knowledge about a topic significantly improves a reader's comprehension ") ~ "Having kowledge about a topic",
    stringr::str_detect(question_col, "Having repeated practice with reading strategies significantly") ~ "Having repeated practice with reading strategies",
    stringr::str_detect(question_col, "Each reading comprehension lesson should") ~ "Focus on a single standard",
    stringr::str_detect(question_col, "All students, regardless of level, should engage with the same anchor text") ~ "Engage with the same anchor text",
    stringr::str_detect(question_col, "Questions about texts should focus on") ~ "Text-specific questions",
    stringr::str_detect(question_col, "Students should primarily engage with texts only") ~ "Only engage in texts at reading level",
    stringr::str_detect(question_col, "Text complexity is solely determined by") ~ "Text complexity is quantitatively determeined"

  )

}
