#' Basic theme setting fonts
#'
#' @importFrom rlang .data
#'
#' @keywords internal
g2g_plt_base_theme <- function() {

  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size=12),
      strip.text = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(size=0.5, color = 'gray')
    )

}

#' G2G ggplot theme that contains no lines
#'
#' @importFrom rlang .data
#'
#' @export
g2g_plt_theme_no_lines <- function() {

  g2g_plt_base_theme() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
    )

}

#' Stacked horizontal bar chart showing Likert item responses
#'
#' Creates a stacked horizontal bar chart showing Likert items and responses. Data must be percentages.
#'
#' @param .data The data set to visualize. It must be aggregated results in tidy format. Each row is a
#'     single question and response option ('Agree'), and the aggregate percentage of respondents
#'     - as a decimal (.75) - answering with the given response option.
#' @param x_var The x variable name, as a string. This should be numeric and as a decimal between 0 and 1.
#'       It represents the percentage of respondents for the given question and response option.
#' @param y_var The x variable name, as a string. This could be questions or a column signifying
#'       pre or post training, with a facet added after this function signifying questions.
#' @param fill_var The variable name, as a string, representing the response scales ('Agree').
#' @param text_var The variable name, as a string, representing the text to plot over the chart.
#'       This should be numeric and as a decimal between 0 and 1.
#' @param color_pal Custom color palette to use. This should be a vector with the values being
#'       the hex codes for the colors and the names being the unique scales from \code{fill_var}
#'
#' @importFrom rlang .data
#'
#' @export
g2g_viz_stacked_bar_percent <- function(.data, x_var, y_var, fill_var, text_var, color_pal) {

  # make sure all numbers are between 0 and 1
  if (!all(dplyr::between(.data[[x_var]][!is.na(.data[[x_var]])], 0, 1))) {
    stop("All 'x_var' values must be decimals between 0 and 1.", call. = FALSE)
  }

  # the fill column must be a factor
  if (!is.factor(.data[[fill_var]])) stop("`fill_var` must be a factor")

  # the legend should have two rows if there are more than four options and one row otherwise
  num_legend_items <- length(levels(.data[[fill_var]]))
  num_legend_rows <- if (num_legend_items > 4) 2 else 1

  # make sure all column are present
  col_names <- colnames(.data)

  viz_cols <- c(x_var, y_var, fill_var, text_var)

  viz_in_colnames <- viz_cols %in% col_names

  if(!all(viz_in_colnames)) {
    stop(
      paste0(
        "The following column names are not in your data (.data): '",
        paste0(viz_cols[!viz_in_colnames], collapse = ", '"),
        "'"),
      call. = FALSE
      )
  }

  text_offset <- ifelse(.data[[text_var]] < .07, .04, .data[[text_var]]-.05)

  ggplot2::ggplot(.data, ggplot2::aes(.data[[x_var]], .data[[y_var]], fill = .data[[fill_var]])) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(.data[[text_var]], accuracy = 1), x = text_offset),
      color = 'white', fontface='bold'
    ) +
    ggplot2::scale_fill_manual(values = color_pal) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    g2g_plt_theme_no_lines() +
    ggplot2::theme(legend.position = 'bottom') +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=num_legend_rows, byrow=TRUE))

}

#' Create different question stems for stems with more than x number of questions
#'
#' When creating faceted bar charts that are faceted by question, sometimes there are too many
#' questions for a single question stem to place on one visualization. This function lets you choose
#' how many questions should be plotted on one visualization. For questions over this number, it adds the
#' word '(continue)' to the question stem. Then, the questions will plot to different visualization when
#' you iterate through stems, creating faceted charts.
#'
#' The function works with data created by \code{gwg_forms_survey_calc_percentages()}.
#'
#' @param .data Input data frame made with \code{gwg_forms_survey_calc_percentages}.
#' @param number_questions An integer, the number of questions per plot (per facet)
#' @param grouping_columns Columns, as a string vector, that you want to group by when determining whether
#'      the number of questions within the question stem is over the value set by \code{number_questions}.
#'      Defaults to `NULL`, which is no groupings.
#'
#' @returns The same data frame as before, but question stems containing questions over the value
#'      set by \code{number_questions} contain the word '(continued)' at the start of them.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_split_question_stems <- function(.data, number_questions, grouping_columns = NULL) {

  # test #

  if (!is.numeric(number_questions)) stop("`number_questions` must be an integer", call. = FALSE)

  unique_questions <- .data |>
    dplyr::select(dplyr::all_of(grouping_columns), .data$question_stem, .data$response_option) |>
    dplyr::distinct() |>
    dplyr::group_by_at(c(grouping_columns, 'question_stem')) |>
    dplyr::mutate(n = dplyr::row_number()) |>
    dplyr::mutate(cont = ifelse(.data$n > !!number_questions, TRUE, FALSE)) |>
    dplyr::select(-.data$n)

  .data |>
    dplyr::left_join(unique_questions, by = c(grouping_columns, 'question_stem', 'response_option')) |>
    dplyr::mutate(question_stem = ifelse(.data$cont, glue::glue("(continued) {.data$question_stem}"), .data$question_stem)) |>
    dplyr::select(-.data$cont)

}
