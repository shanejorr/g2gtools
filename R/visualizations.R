#' Custom Good to Great Plot Theme
#'
#' Custom theme for Good to Great that relies on the `tntpr::tntp_style()` theme.
#' Adds check to ensure the 'Halyard Display' font it loaded, and if not loads the font.
#'
#' @param font_size The is the `base_size` parameter in `tntpr::tntp_style()`. Defaults to 24.
#' @param ... Parameters for `theme()`
#'
#' @export
g2g_plt_theme <- function(font_size = 16, ...) {

  if (.Platform$OS.type == "unix") {

    halyard_fonts <- c("Halyard Display", "Halyard Display Bold", "Halyard Display Italic", "Halyard Display Bold Italic")

    if (!("Halyard Display" %in% names(grDevices::quartzFonts()))) {

      grDevices::quartzFonts(`Halyard Display` = grDevices::quartzFont(halyard_fonts))
    }
  }

  # reduce title font size a bit
  title_font_size = font_size * .70

  # increase strip text font size
  strip_text_size <- font_size * .6

  # tntpr::tntp_style(
  #   # base_size = font_size,
  #   show_axis_titles = TRUE,
  #   ...
  # ) +
  ggplot2::theme_minimal(base_size = font_size) +
  ggplot2::theme(
      panel.grid = element_blank(),
      legend.position="bottom",
      panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      plot.title = element_text(
        size = rel(0.9),  # 90% of the base size
        face = "bold"
      ),
      plot.subtitle = element_text(size = rel(0.95)),
      ...
  )

}

#' Create a basic vertical bar chart
#'
#' Creates a vertical bar chart. This function can be used as-is, but is primarily used to build
#' more complex plots. For example, \code{g2g_viz_high_expectations()} relies on this function
#' to create the underlying bar charts.
#'
#' @param .data The data set to visualize.
#' @param x_var The column name, as a string, of the variable (categorical) on the x axis.
#' @param y_var The column name, as a string, of the variable (numeric) on the y axis.
#' @param text_var The column name, as a string, of the variable to show as text. This will be the same
#'      number as the variable in `y_var`, but users may want to round it or convert it to a percentage.
#' @param text_offset Defined how many units the text should offset horizontally from the y variable.
#'      Use negative numbers to show the text within the bar and positive numbers to place the text
#'      above the bar. Defaults to 0, which is the `y_var` value.
#' @param fill_color The color, as a string or hex number, of the bars for the bar chart.
#' @param text_color The color of the text on the bar chart. Default is 'black'.
#' @param text_size The size of the text on the bar chart. Default is 4.21.
#' @param font_face The font face of the text numbers that show up in the bars.
#'          One of "plain", "bold", "italic", "bold.italic". Defaults to "plain"
#' @param ... Parameters for `g2g_plt_theme()`.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_viz_basic_bar <- function(.data, x_var, y_var, text_var, text_offset = 0, fill_color = 'gray', text_color = 'black', text_size = 4.21, font_face = "plain", ...) {

  ggplot2::ggplot(.data, ggplot2::aes(.data[[x_var]], .data[[y_var]])) +
    ggplot2::geom_col(fill = fill_color, show.legend = TRUE) +
    ggplot2::scale_x_discrete(drop=FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data[[text_var]], y = .data[[y_var]] + text_offset),
      color = text_color, size = text_size, fontface = font_face, family = "sans"
    )  +
    g2g_plt_theme(...)

}

#' Create a vertical bar chart with grouping variables for colors. Groups are dodged.
#'
#' @param .data The data set to visualize.
#' @param x_var The column name, as a string, of the variable (categorical) on the x axis.
#' @param y_var The column name, as a string, of the variable (numeric) on the y axis.
#' @param text_var The column name, as a string, of the variable to show as text. This will be the same
#'      number as the variable in `y_var`, but users may want to round it or convert it to a percentage.
#' @param color_pal Named vector of the custom color palette to use.
#' @param text_offset Defined how many units the text should offset horizontally from the y variable.
#'      Use negative numbers to show the text within the bar and positive numbers to place the text
#'      above the bar. Defaults to 0, which is the `y_var` value.
#' @param fill_var The color, as a string or hex number, of the bars for the bar chart.
#' @param text_color The color of the text on the bar chart. Default is 'black'.
#' @param text_size The size of the text on the bar chart. Default is 4.21.
#' @param font_face The font face of the text numbers that show up in the bars.
#'          One of "plain", "bold", "italic", "bold.italic". Defaults to "plain"
#' @param add_vertical_lines Boolean, whether to add a vertical line between each fill group. Default is FALSE.
#' @param ... Parameters for `g2g_plt_theme()`.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_viz_basic_dodged_bar <- function(.data, x_var, y_var, fill_var, text_var, color_pal, text_offset = 0, text_color = 'black', text_size = 4.21, font_face = "plain", add_vertical_lines = FALSE, ...) {

  plt<- ggplot2::ggplot(.data, ggplot2::aes(.data[[x_var]], .data[[y_var]], fill = .data[[fill_var]])) +
    ggplot2::geom_col(width = .75, show.legend = TRUE, position = ggplot2::position_dodge2(width = .75, preserve = "single")) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data[[text_var]], y = .data[[y_var]] + text_offset, group = .data[[fill_var]]),
      color = text_color, size = text_size, fontface = font_face,
      position = ggplot2::position_dodge(width=.75)
    ) +
    ggplot2::scale_x_discrete(drop=FALSE) +
    ggplot2::scale_fill_manual(values = color_pal, drop = FALSE) +
    g2g_plt_theme(...)

  if (add_vertical_lines) {

    num_groups <- dplyr::n_distinct(.data[[x_var]])
    x_intercepts <- seq(1.5, num_groups-.5, 1)

    plt <- plt +
      ggplot2::geom_vline(xintercept = x_intercepts, color = 'gray')

  }

  return(plt)

}

#' Stacked horizontal bar chart showing Likert item responses
#'
#' Creates a stacked horizontal bar chart showing Likert items and responses. Data must be percentages.
#'
#' @param .data The data set to visualize. It must be aggregated results in tidy format. Each row is a
#'     single question and response option ('Agree'), and the aggregate percentage of respondents
#'     - as a decimal (.75) - answering with the given response option.
#' @param perc_value_var The column name, as string, of the column containing the percentage values.
#'       It represents the percentage of respondents for the given question and response option. This will be the x-axis and
#'       should be numeric and as a decimal between 0 and 1.
#' @param question_var The column name, as string, of the column containing the survey questions. This will be the y-value labels.
#' @param fill_var The column name,, as a string, representing the response scales ('Agree').
#' @param text_var The column name,, as a string, representing the text to plot over the chart.
#'       This should be numeric and as a decimal between 0 and 1.
#' @param color_pal Custom color palette to use. This should be a vector with the values being
#'       the hex codes for the colors and the names being the unique scales from \code{fill_var}
#' @param comparison_var Optional and defaults to NULL. The  column name, as a string, of the column containing
#'       time periods, to be used for comparison. Example: 'Pre-training' and 'Post-training'.
#' @param text_size Size of the text that represents the numbers within the bar chart. Defaults to 4.586111, which is 13 point font size.
#'       Font size can be converted to `text_size` with this formula: `font size / (14/5)`.
#' @param text_location The variable name, as a string, of the location of the text on the x axis, between 0 and 1. If `NULL`, the default,
#'       the location will be the same as `text_var`, but right under the bar.
#' @param sort_by_value Logical. If TRUE, the y-axis will be sorted by the values in `text_var`. If FALSE, no sorting will be applied. Defaults to FALSE
#' @param ... Parameters for `g2g_plt_theme()`
#'
#' @importFrom rlang .data
#'
#' @export
g2g_viz_stacked_bar_percent_horizontal <- function(.data, perc_value_var, question_var, fill_var, text_var, color_pal, comparison_var = NULL, text_size = 4.586111, text_location = NULL, sort_by_value = FALSE, ...) {

  if (!is.null(comparison_var)) {
    # plot values for plots that are comparing multiple terms
    y_var <- comparison_var
    wrap_var <- question_var

    if (!comparison_var %in% names(.data)) stop("'comparison_var' is not in the data.", call. = FALSE)

  } else {
    # plot for values where there are no comparisons of terms
    y_var <- question_var
  }

  # ensure entered parameters are correct
  g2g_viz_checks(.data, perc_value_var, y_var, fill_var, text_var)

  # the legend should have two rows if there are more than four options and one row otherwise
  num_legend_items <- length(levels(.data[[fill_var]]))
  num_legend_rows <- if (num_legend_items > 4) 2 else 1

  if (is.null(text_location)) {
    text_x_position <- dplyr::case_when(
      .data[[text_var]] < .09 ~ .05,
      .data[[text_var]] > .09 & .data[[text_var]] < 1 ~ .data[[text_var]] - .06,
      .data[[text_var]] == 1 ~ .data[[text_var]] - .08,
      TRUE ~ .06
    )

    text_label <- scales::percent(.data[[text_var]], accuracy = 1)

  } else {
    text_x_position <- .data[[text_location]]
    text_label <- .data[[text_var]]
  }

  # Conditionally apply sorting based on sort_by_value parameter
  if (sort_by_value) {
    plt <- ggplot2::ggplot(.data, ggplot2::aes(.data[[perc_value_var]], forcats::fct_reorder(.data[[y_var]], .data[[text_var]], na.rm = TRUE), fill = .data[[fill_var]]))
  } else {
    plt <- ggplot2::ggplot(.data, ggplot2::aes(.data[[perc_value_var]], .data[[y_var]], fill = .data[[fill_var]]))
  }

  plt <- plt + ggplot2::geom_col(show.legend = TRUE) +
    ggplot2::geom_text(
      ggplot2::aes(label = text_label, x = text_x_position),
      color = 'white',
      fontface='bold', size = text_size
    ) +
    ggplot2::scale_fill_manual(values = color_pal, drop = FALSE) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    g2g_plt_theme(...) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=num_legend_rows, byrow=TRUE, reverse = TRUE))

  if (!is.null(comparison_var)) {
    plt <- plt +
      ggplot2::facet_wrap(ggplot2::vars(.data[[question_var]]),  strip.position = "left", ncol = 1) +
      ggplot2::scale_y_discrete(position = "right", guide = ggplot2::guide_axis(angle=-0)) +
      ggplot2::theme(
          strip.text.y.left = ggplot2::element_text(angle=0, hjust = 1, colour = "#444444"),
          axis.title.y = ggplot2::element_blank()
        )
  }

  return(plt)

}

#' Stacked vertical bar chart showing Likert item responses
#'
#' Creates a stacked vertical bar chart showing Likert items and responses. Data must be percentages.
#'
#' @param .data The data set to visualize. It must be aggregated results in tidy format. Each row is a
#'     single question and response option ('Agree'), and the aggregate percentage of respondents
#'     - as a decimal (.75) - answering with the given response option.
#' @param x_var The x variable name, as a string. This should be numeric and as a decimal between 0 and 1.
#'       It represents the percentage of respondents for the given question and response option.
#' @param y_var The y variable name, as a string. This could be questions or a column signifying
#'       pre or post training, with a facet added after this function signifying questions.
#' @param fill_var The variable name, as a string, representing the response scales ('Agree').
#' @param text_var The variable name, as a string, representing the text to plot over the chart.
#'       This should be numeric and as a decimal between 0 and 1.
#' @param color_pal Custom color palette to use. This should be a vector with the values being
#'       the hex codes for the colors and the names being the unique scales from \code{fill_var}
#' @param text_size Size of the text that represents the numbers within the bar chart. Defaults to 4.586111, which is 13 point font size.
#'       Font size can be converted to `text_size` with this formula: `font size / (14/5)`.
#' @param ... Parameters for `g2g_plt_theme()`
#'
#' @importFrom rlang .data
#'
#' @export
g2g_viz_stacked_bar_percent_vertical <- function(.data, x_var, y_var, fill_var, text_var, color_pal, text_size = 4.586111, ...) {

  # ensure entered parameters are correct
  g2g_viz_checks(.data, y_var, x_var, fill_var, text_var)

  # the legend should have two rows if there are more than four options and one row otherwise
  num_legend_items <- length(levels(.data[[fill_var]]))
  num_legend_rows <- if (num_legend_items > 4) 2 else 1

  text_values <- .data[[text_var]]

  # location of the numeric texts over or under the bars
  # put text over the bars for low numbers and under the bars for anything over 0.03
  text_location <- dplyr::case_when(
    is.na(text_values) ~ NA,
    text_values < 0.05 ~ text_values + 0.05,
    dplyr::between(text_values, .05, .1) ~ text_values - 0.05,
    text_values > .1 ~ text_values - 0.075,
    .default = text_values
  )

  ggplot2::ggplot(.data, ggplot2::aes(.data[[x_var]], .data[[y_var]], fill = .data[[fill_var]])) +
    ggplot2::geom_col(show.legend = TRUE) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(.data[[text_var]], accuracy = 1), y = text_location),
      color = ifelse(.data[[text_var]] > 0.05, "white", "black"),
      fontface='bold', size = text_size
    ) +
    ggplot2::scale_fill_manual(values = color_pal, drop = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    g2g_plt_theme(...) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=num_legend_rows, byrow=TRUE, reverse = TRUE))

}

#' Create different question stems for stems with more than x number of questions
#'
#' When creating faceted bar charts that are faceted by question, sometimes there are too many
#' questions for a single question stem to place on one visualization. This function lets you choose
#' how many questions should be plotted on one visualization. For questions over this number, it adds the
#' word '(continue)' to the question stem. Then, the questions will plot to different visualization when
#' you iterate through stems, creating faceted charts.
#'
#' The function works with data created by \code{g2g_forms_survey_calc_percentages()}.
#'
#' @param .data Input data frame made with \code{g2g_forms_survey_calc_percentages}.
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
    dplyr::select(dplyr::all_of(grouping_columns), "question_stem", "response_option") |>
    dplyr::distinct() |>
    dplyr::group_by_at(c(grouping_columns, 'question_stem')) |>
    dplyr::mutate(n = dplyr::row_number())

  .data |>
    dplyr::left_join(unique_questions, by = c(grouping_columns, 'question_stem', 'response_option')) |>
    dplyr::mutate(question_stem = dplyr::case_when(
      .data$n <= !!number_questions ~ .data$question_stem,
      dplyr::between(.data[['n']], !!number_questions + 1, !!number_questions * 2) ~ glue::glue("(continued) {.data$question_stem}"),
      dplyr::between(.data[['n']], (!!number_questions * 2) + 1, !!number_questions * 3) ~ glue::glue("(continued)  {.data$question_stem}"),
      .data$n > !!number_questions * 3 ~ glue::glue("(continued)   {.data$question_stem}"),
      .default = glue::glue("Failed to count number of questions in question stem for the following stem: {.data$question_stem}")
    )
    ) |>
    dplyr::select(-"n")

}

#' Bar chart of high expectations score and the percentage of teachers with high expectations
#'
#' Create two bar chart visualizations in one plot. One containing average high expectations scores and the other
#' showing the percentage of teachers with high expectations. The data used in the bar charts must
#' come from \code{g2g_calc_high_expectations_averages()}.
#'
#' @param .data Input data frame made with \code{g2g_forms_survey_calc_percentages()}.
#' @param x_axis Column name, as a string vector, containing the categories you want to compare.
#'      These will be the x-axis in the plot.
#' @param plots_to_return String identifying which type of data to return. 'percentages' returns a single plot
#'      showing the percentage of teachers with high expectations, 'scores' returns a single plot showing high
#'      expectations scores, while 'both' returns a single plot containing two visualizations, one with
#'      scores and one with percentages.
#' @param space_between_plots The amount of space,in points ('pt') between plots. Defaults to 40.
#' @param text_size The size of the text on the bar chart. Default is 4.21.
#' @param he_scale The scale in which high expectations scores were calculated.
#'          string, either 'sum' or 'average'. 'sum' is when all HE questions for a teacher
#'          are summed and will fall between 0 and 20. 'average' is when the questions are averaged
#'          and will fall between 0 and 5. Defaults to 'sum'.
#' @param ... Parameters for `g2g_plt_theme()`.
#'
#' @returns A single plot containing two bar chart plots.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_viz_high_expectations <- function(.data, x_axis, plots_to_return = 'both', space_between_plots = 50, text_size = 4.21, he_scale = 'sum', ...) {

  # ensure the two cm columns are present
  col_names <- colnames(.data)

  req_cols <- c('cm_expectations', 'cm_binary_expectations', x_axis)

  if (!all(req_cols %in% col_names)) {

    stop(
      paste(
        "Your data (.data) is missing one of the following required columns: ", paste(req_cols, collapse = ", "),
        "\nEnsure you used `g2g_forms_survey_calc_percentages()` to create your data."
      ), call. = FALSE
    )

  }

  if (!he_scale %in% c('sum', 'average')) stop("`he_scale` must be either 'sum' or 'average'.")

  # set parameters for plots based on scale
  y_lim <- if (he_scale == 'sum') 20 else 5
  score_text_space <- if (he_scale == 'sum') -1.35 else -1.35/4

  he_perc_fill_color <- if (plots_to_return == 'percentages') tntpr::tntp_colors('dark_green') else tntpr::tntp_colors('moss')

  # expectations score
  plt_he_scores <- .data |>
    dplyr::mutate(cm_expectations_text = round(.data[['cm_expectations']], 1)) |>
    g2g_viz_basic_bar(x_axis, 'cm_expectations', 'cm_expectations_text', score_text_space, text_color = 'white', font_face = "bold", fill_color = tntpr::tntp_colors('dark_green'), text_size = text_size, ...) +
    ggplot2::ylim(c(0, y_lim))

  plt_he_perc <- .data |>
    dplyr::mutate(cm_binary_expectations_text = scales::percent(.data[['cm_binary_expectations']], accuracy = 1)) |>
    g2g_viz_basic_bar(x_axis, 'cm_binary_expectations', 'cm_binary_expectations_text', -.075, text_color = 'white', font_face = "bold", fill_color = he_perc_fill_color, text_size = text_size, ...) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1))

  if (plots_to_return == 'percentages') {

    plt_he_perc <- plt_he_perc +
      ggplot2::ylab('Perc. of teachers with high expectations') +
      ggplot2::xlab(NULL)

    return(plt_he_perc)

  } else if (plots_to_return == 'scores') {

    plt_he_scores <- plt_he_scores +
      ggplot2::ylab('Avg. high expectations score') +
      ggplot2::xlab(NULL)

    return(plt_he_scores)

  } else if (plots_to_return == 'both') {

    plt_he_scores <- plt_he_scores +
      ggplot2::labs(
        title = 'Avg. high expectations score',
        x = NULL,
        y = NULL
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.margin = ggplot2::margin(t = 0, r = space_between_plots, b = 0, l = 0, unit = "pt")
      )

    plt_he_perc <- plt_he_perc +
      ggplot2::labs(
        title = "Percentage of teachers\nwith high expectations",
        x = NULL,
        y = NULL
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    plts <- patchwork::wrap_plots(plt_he_scores, plt_he_perc) +
      patchwork::plot_annotation(
        theme = ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
      )

    return(plts)

  } else {
    stop('`plots_to_return` must be either `percentages`, `scores`, or `both`.', call. = FALSE)
  }

}

#' Bar chart of instructional practice scores
#'
#' Creates a bar chart of instructional practice scores. The data used in the bar charts must
#' come from \code{g2g_calc_inst_practices()}. These scores are not from the IPG. They are from
#' the teacher survey and are the 'think about the last unit you taught' questions.
#'
#' @param .data Input data frame made with \code{g2g_calc_inst_practices()}.
#' @param x_axis Column name, as a string vector, containing the categories you want to compare.
#'      These will be the x-axis in the plot
#'
#' @returns A single plot containing instructional practice scores.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_viz_inst_practice <- function(.data, x_axis) {

  col_names <- colnames(.data)

  req_cols <- c('inst_practice_score')

  if (!all(req_cols %in% col_names)) {

    stop(
      paste(
        "Your data (.data) is missing the following required columns: ", paste(req_cols, collapse = ", "),
        "\nEnsure you used `g2g_calc_inst_practices()` to create your data."
      ), call. = FALSE
    )

  }

  .data |>
    dplyr::mutate(inst_practice_score_text = round(.data[['inst_practice_score']], 1)) |>
    g2g_viz_basic_bar(x_axis, 'inst_practice_score', 'inst_practice_score_text', - .3, fill_color = "#00A4C7") +
    ggplot2::labs(
      title = 'Average Instructional Practice Score',
      x = NULL,
      y = NULL
    ) +
    ggplot2::ylim(c(0, 5)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face='bold'))

}

#' Bar charts of IPG scores
#'
#' Create two bar chart visualizations in one plot. One containing average IPG scores and the other
#' showing the percentage of teachers with strong instruction. The data used in the bar charts must
#' come from \code{g2g_calc_inst_practices()}.
#'
#' @param .data Input data frame made with \code{g2g_forms_survey_calc_percentages()}.
#' @param x_axis Column name, as a string vector, containing the categories you want to compare.
#'      These will be the x-axis in the plot.
#' @param space_between_plots The amount of space,in points ('pt') between plots. Defaults to 40.
#'
#' @returns A single plot containing two bar chart plots.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_viz_ipg <- function(.data, x_axis, space_between_plots = 50.) {

  # ensure the two cm columns are present
  col_names <- colnames(.data)

  req_cols <- c('cm_ipg', 'cm_binary_ipg', x_axis)

  if (!all(req_cols %in% col_names)) {

    stop(
      paste(
        "Your data (.data) is missing one of the following required columns: ", paste(req_cols, collapse = ", "),
        "\nEnsure you used `g2g_forms_survey_calc_percentages()` to create your data."
      ), call. = FALSE
    )

  }

  # expectations score
  plt_ipg_scores <- .data |>
    dplyr::mutate(cm_ipg_text = round(.data[['cm_ipg']], 1)) |>
    g2g_viz_basic_bar(x_axis, 'cm_ipg', 'cm_ipg_text', 0.1, fill_color = "#00A4C7") +
    ggplot2::labs(
      title = 'Average Observation Score\n  ',
      x = NULL,
      y = NULL
    ) +
    ggplot2::scale_y_continuous(limits= c(0,3.1),breaks=seq(0,3, 1)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face='bold'),
      plot.margin = ggplot2::margin(t = 0, r = space_between_plots, b = 0, l = 0, unit = "pt")
    )

  plt_ipg_perc <- .data |>
    dplyr::mutate(cm_binary_ipg_text = scales::percent(.data[['cm_binary_ipg']], accuracy = 1)) |>
    g2g_viz_basic_bar(x_axis, 'cm_binary_ipg', 'cm_binary_ipg_text', 0.04, fill_color = "#EA8835") +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1.1),breaks = seq(0,1,.25)) +
    ggplot2::labs(
      title = "Percentage of Observations\nWith Strong Instruction",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face='bold'))

  plts <- patchwork::wrap_plots(plt_ipg_scores, plt_ipg_perc) +
    patchwork::plot_annotation(
      theme = ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 12))
    )

  return(plts)

}

#' Perform checks for visualizations to ensure entered parameters are correct
#'
#' @importFrom rlang .data
#'
#' @keywords internal
g2g_viz_checks <- function(.data, numeric_var, qual_var, fill_var, text_var) {

  # make sure all numbers are between 0 and 1
  if (!all(dplyr::between(.data[[numeric_var]][!is.na(.data[[numeric_var]])], 0, 1))) {
    stop("All 'numeric_var' values must be decimals between 0 and 1.", call. = FALSE)
  }

  # the fill column must be a factor
  if (!is.factor(.data[[fill_var]])) stop("`fill_var` must be a factor")

  # make sure all column are present
  col_names <- colnames(.data)

  viz_cols <- c(numeric_var, qual_var, fill_var, text_var)

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

  return(NULL)

}

#' Plot all Likert scale items centered on 0
#'
#' Creates horizontal bar chart with scales centered on 0. Positive scales extend to the right of 0,
#' while negative scales extend to the left of 0. Neutral scale items are placed in their own plot to
#' the right of the plto containing positive and negative scales.
#'
#' @param .data The data set to visualize. It must be aggregated results in tidy format. Each row is a
#'     single question and response option ('Agree'), and the aggregate percentage of respondents
#'     - as a decimal (.75) - answering with the given response option.
#' @param x_var The x variable name, as a string. This should be numeric and as a decimal between 0 and 1.
#'       It represents the percentage of respondents for the given question and response option.
#' @param y_var The x variable name, as a string. This could be questions or a column signifying
#'       pre or post training, with a facet added after this function signifying questions.
#' @param fill_var The variable name, as a string, representing the response scales ('Agree').
#' @param color_pal Custom color palette to use. This should be a vector with the values being
#'       the hex codes for the colors and the names being the unique scales from \code{fill_var}
#'
#' @importFrom rlang .data
#'
#' @export
g2g_viz_likert_centered <- function(.data, x_var, y_var, fill_var, color_pal) {

  g2g_viz_checks(.data, x_var, y_var, fill_var, text_var = NULL)

  # find positive, negative, and neutral scales
  number_of_scales <- length(color_pal)

  has_neutral <- !(number_of_scales %% 2 == 0)

  # clean the data and extract the positive, negative, and neutral scales
  data_and_scales <- g2g_helper_clean_viz_likert_centered(.data, x_var, y_var, fill_var, color_pal, has_neutral, number_of_scales)
  df <- data_and_scales$df
  neutral_scales <- data_and_scales$scales$neutral

  pos_neg_response_label <- 'Positive / Negative\nResponses'
  neutral_response_lavel <- 'Neutral\nResponses'

  # used to get rid of 'no visible bindings' message
  intercept <- neutral_response <- NULL

  if (has_neutral) {
    df$neutral_response <- ifelse(df[[fill_var]] == neutral_scales, 'Neutral\nResponses', 'Positive / Negative\nResponses') |>
      forcats::fct_relevel('Positive / Negative\nResponses')

    neutral_responses <- df[[x_var]][df$neutral_response == 'Neutral\nResponses']

    # return .10 if there are no neutral response,
    # if there are neutral responses, return the max
    max_neutral <- if (is.null(neutral_responses) | length(neutral_responses) == 0) .10 else max(neutral_responses)

    axis_limit_neutral <- dplyr::case_when(
      max_neutral < .25 ~ .25,
      max_neutral < .5 ~ .5,
      max_neutral < .75 ~ .75,
      max_neutral < .25 ~ .25,
      TRUE ~ 1
    )

    x_intercepts <- data.frame(neutral_response = c('Positive / Negative\nResponses', 'Neutral\nResponses'), intercept = c(0, NA_integer_))
    x_intercepts$neutral_response <- forcats::fct_relevel(x_intercepts$neutral_response, 'Positive / Negative\nResponses')

  } else {

    x_intercepts <- data.frame(neutral_response = c('Positive / Negative\nResponses'), intercept = 0)

  }

  axis_label_percent <- function(x) scales::percent(abs(x), accuracy = 1)

  text_offset <- .1

  legend_order <- c(rev(data_and_scales$scales$negative), rev(data_and_scales$scales$positive), data_and_scales$scales$neutral)

  plt <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
    ggplot2::geom_col(show.legend = TRUE) +
    ggplot2::scale_fill_manual(
      values = color_pal, drop = FALSE,
      breaks = legend_order,
      labels = legend_order
    ) +
    ggplot2::geom_vline(data = x_intercepts, ggplot2::aes(xintercept = .data[['intercept']]), linetype = 1, linewidth = 1.2, alpha = .7) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = ifelse(.data[['category_cumulative']] < 0, .data[['category_cumulative']] - text_offset, .data[['category_cumulative']] + text_offset),
        label = scales::percent(abs(.data[['category_cumulative']]), accuracy = 1)
      ), size = 4.586111
    ) +
    g2g_plt_theme()


  if (has_neutral) {

    plt <- plt +
      ggplot2::facet_wrap(ggplot2::vars(neutral_response), ncol = 2, scales = "free_x") +
      ggh4x::scale_x_facet(neutral_response == 'Positive / Negative\nResponses', limits = c(-1.1, 1.1), breaks = seq(-1, 1, .25), labels = axis_label_percent) +
      ggh4x::scale_x_facet(neutral_response == 'Neutral\nResponses', limits = c(0, axis_limit_neutral+.1), breaks = seq(0, axis_limit_neutral, .25), labels = axis_label_percent) +
      ggh4x::force_panelsizes(cols = c(1, .2))

  } else {

    plt <- plt +
      ggplot2::scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25), labels = axis_label_percent)

  }

  return(plt)

}

#' Clean data prior to creating plot for `g2g_viz_likert_centered`
#'
#' @inheritParams g2g_viz_likert_centered
#' @param has_neutral Boolean. Whether data has a neutral category
#' @param number_of_scales The total number of scales. Must be a single integer.
#'
#' @returns data.frame that is used in `g2g_viz_likert_centered`.
#'
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @keywords internal
g2g_helper_clean_viz_likert_centered <- function(.data, x_var, y_var, fill_var, color_pal, has_neutral, number_of_scales) {

  number_scales_each_category <- floor(number_of_scales/2)

  positive_scales_int <- 1:number_scales_each_category
  negative_scales_int <- (number_of_scales-number_scales_each_category+1):number_of_scales
  neutral_scales_int <- if (has_neutral) ceiling(number_of_scales/2) else NULL

  positive_scales <- names(color_pal)[positive_scales_int]
  negative_scales <- names(color_pal)[negative_scales_int]
  neutral_scales <- if (has_neutral) names(color_pal)[neutral_scales_int] else NULL

  df <- .data |>
    tidyr::drop_na(dplyr::all_of(fill_var)) |>
    dplyr::mutate(
      !!fill_var := factor(.data[[fill_var]], levels = c(negative_scales, positive_scales, neutral_scales)),
      !!x_var := ifelse(.data[[fill_var]] %in% negative_scales, .data[[x_var]] * -1, .data[[x_var]]),
      x_intercet = ifelse(.data[[fill_var]] %in% neutral_scales, NA_integer_, 0),
      response_category = dplyr::case_when(
        .data[[fill_var]] %in% positive_scales ~ 'Positive',
        .data[[fill_var]] %in% negative_scales ~ 'Negative',
        .data[[fill_var]] %in% neutral_scales ~ 'Neutral',
        TRUE ~ 'Failed to match response'
      )
    ) |>
    dplyr::group_by(.data[[y_var]], .data[['response_category']]) |>
    dplyr::mutate(category_cumulative = sum(.data[[x_var]])) |>
    dplyr::ungroup()

  if (any(df$response_category == 'Failed to match response')) stop("We had a problem determining whether your scales were positive or negative. Please recheck your data and parameters.", call. = FALSE)

  list(
    df = df,
    scales = list(
      positive = positive_scales,
      negative = negative_scales,
      neutral = neutral_scales
    ),
    scales_int = list(
      positive = positive_scales_int,
      negative = negative_scales_int,
      neutral = neutral_scales_int
    )
  )

}
