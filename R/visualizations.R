#' Add visualizations to Power Point slides
#'
#' Add ggplot plots to PPT slides as vector graphics. You must initialize the PPT deck with
#' \code{doc <- officer::read_pptx()}. After adding plots to the deck with this function, you can
#' write out the deck with \code{print(doc, target = 'file_name_of_deck.pptx')}
#'
#' @param doc Document object. Created with \code{officer::read_pptx()}.
#' @param slide_plot A ggplot object containing the plot we want in the PPT slide.
#' @param slide_header The slide header, as a string.
#' @param plt_width The width of the plot, in inches, when it is in the PPT presentation.
#' @param plt_height The height of the plot, in inches, when it is in the PPT presentation.
#'
#' @importFrom rlang .data
#'
#' @export
add_slides_ppt <- function(doc, slide_plot, slide_header, plt_width, plt_height) {

  # create new slide with default template
  doc <- officer::add_slide(doc, "Title and Content", 'Office Theme')

  # create header text for ppt slide
  # header text will contain the major question
  fpt <- officer::fp_text(font.size = 18, font.family = "Segoe UI Semibold")
  header_text <- officer::fpar(officer::ftext(slide_header, fpt))

  # add header text to slide
  doc <- officer::ph_with(doc, value = header_text, location = officer::ph_location_type(type = "title"))

  #convert plot to vector graphic
  vec_plt <- rvg::dml(ggobj = slide_plot)

  # add plot to slide
  slide_loc <- officer::ph_location(left = .5, top = 1.5, width = plt_width, height = plt_height, newlabel = "hello")

  doc <- officer::ph_with(doc, value = vec_plt, location = slide_loc)

  return(doc)

}

#' Basic theme setting fonts
#'
#' @importFrom rlang .data
#'
#' @keywords internal
g2g_plt_base_theme <- function() {

  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size=12),
      strip.text = ggplot2::element_text(size = 10),
      panel.background = ggplot2::element_rect(size=0.5, color = 'gray')#,
      #text = ggplot2::element_text(family="Segoe UI")
      #plot.title = element_text(family = "Segoe UI")
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
#' @param color_pal Custom color palette to use. THis should be a vector with the values being
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

  # make sure all numbers are between 0 and 1
  if (!all(dplyr::between(.data[[text_var]][!is.na(.data[[text_var]])], 0, 1))) {
    stop("All 'text_var' values must be decimals between 0 and 1.", call. = FALSE)
  }

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

  ggplot2::ggplot(.data, ggplot2::aes(.data[[x_var]], .data[[y_var]], fill = .data[[fill_var]])) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(.data[[text_var]], accuracy = 1), x = .data[[text_var]] - .03),
      color = 'white', fontface='bold' # nudge_x = .03,
    ) +
    ggplot2::scale_fill_manual(values = color_pal) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    g2g_plt_theme_no_lines() +
    ggplot2::theme(legend.position = 'bottom') +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))

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
#'
#' @returns The same data frame as before, but question stems containing questions over the value
#'      set by \code{number_questions} contain the word '(continued)' at the start of them.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_split_question_stems <- function(.data, number_questions, grouping_columns) {

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
