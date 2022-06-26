#' Add visualizations to Power Point slides
#'
#' @keywords internal
cf_add_slides_ppt <- function(doc, slide_header, slide_plot, plt_width, plt_height) {

  # adds a power point slide with a plot to a power point file
  # parameters:
  #   doc: power point document, created with read_pptx()
  #   slide_header: header of slide with plot
  #   slide_plot: the plot to include in the slide

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
#' @keywords internal
plt_theme <- function() {

  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size=12),
      strip.text = ggplot2::element_text(size = 10),
      panel.background = ggplot2::element_rect(size=0.5, color = 'gray'),
      text = ggplot2::element_text(family="Segoe UI")
      #plot.title = element_text(family = "Segoe UI")
    )

}

#' Theme without any lines
#'
#' @keywords internal
plt_no_lines_theme <- function() {

  plt_theme() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
    )

}


#' Plot comparing pre and post training data for data with scales.
#'
#' A horizontal bar chart comparing pre and post training data. The x axis is the percentage responses
#' and the y axis is pre and post training. The plot is faceted by the question. The scale (response column)
#' is represented by the fill.
#'
#' Use the function \code{forms_survey_calc_percentages()} to ensure the data is in the proper format.
#' All default parameter values are designed to with with \code{forms_survey_calc_percentages()}.
#'
#' @param .data The dataset used to make the visualization.
#' @param color_pal The color palette for the scales. It should be a named vector with the names matching
#'      the scale.
#' @param x_var A string representing the column name for the x variable. Will be percent or term (pre / post).
#'    Default is '.percent'.
#' @param y_var A string representing the column name for the y variable. Will be percent or term (pre / post).
#'    Default is 'term'.
#' @param fill_var A string representing the column name for the fill variable. Will be column with scale.
#'    Default is 'response'.
#'
#' @returns A ggplot visualization.
#'
#' @keywords internal
viz_fill_barchart <- function(.data, color_pal, x_var, y_var, fill_var) {

  # number of characters until line break in facet wrap
  facet_text_wrap <- 75

  plt <- ggplot2::ggplot(.data, aes(.data[[x_var]], .data[[y_var]], fill = .data[[fill_var]])) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = color_pal) +
    plt_no_lines_theme() +
    ggplot2::theme(legend.position = 'bottom')

}

#' Bar chart comparing pre and post with scales.
#'
#' Creates a bar chart comparing pre and post surveys with scales. Since scales are used,
#' the numeric axis is a percentage. Function is designed to work with data produced by
#' \code{forms_survey_calc_percentages()}, but it can work with other forms of data as well.
#' All defaults are designed for \code{forms_survey_calc_percentages()}.
#'
#' @param .data The dataset used to make the visualization.
#' @param color_pal The color palette for the scales. It should be a named vector with the names matching
#'      the scale.
#' @param x_var A string representing the column name for the x variable. Will be percent or term (pre / post).
#'    Default is '.percent'.
#' @param y_var A string representing the column name for the y variable. Will be percent or term (pre / post).
#'    Default is 'term'.
#' @param fill_var A string representing the column name for the fill variable. Will be column with scale.
#'    Default is 'response'.
#' @param facet_var A string representing the column name for the fill variable. Will be question text.
#'    Default is 'response_option'. Use NULL to avoid any facets.
#' @param facet_str_wrap The number of characters in the \code{facet_var} until a new line is created.
#'    Default is 65.
#' @param facet_col The number of columns used in the facet. Defautls to 2.
#'
#' @returns A ggplot visualization.
#'
#' @export
viz_pre_post_scales <- function(.data, color_pal, x_var = '.percent', y_var = 'term', text_var = '.percent_pretty', fill_var = 'response',
                                 facet_var = 'response_option', facet_str_wrap = 65, facet_col = 2) {

  plt <- viz_fill_barchart(.data, color_pal, x_var, y_var, fill_var) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, .25)) +
    ggplot2::geom_text(aes(label = .data[[text_var]]), position = position_stack(vjust = .5))

  if (!is.null(facet_var)) {
    plt <- plt +
      ggplot2::facet_wrap(vars(stringr::str_wrap(.data[[facet_var]], facet_str_wrap)), ncol = facet_col)
  }

  return(plt)

}

