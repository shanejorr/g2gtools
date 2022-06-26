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
plt_theme <- function(strip_panel_text_size = 11) {

  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size=12,face="bold"),
      strip.text = ggplot2::element_text(size = strip_panel_text_size),
      panel.background = ggplot2::element_rect(size=0.5, color = 'gray')#,
      #plot.title = element_text(face = "bold")
    )

}

#' Theme without any lines
#'
#' @keywords internal
plt_no_lines_theme <- function(strip_panel_text_size = 11) {

  plt_theme(strip_panel_text_size= strip_panel_text_size) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
    )

}


#' Plot comparing pre and post training data
#'
#' A horizontal bar chart comparing pre and post training data. The x axis is the percentage responses
#' and the y axis is pre and post training. The plot is faceted by the question.
#'
#' @param .data The dataset used to make the visualization
#' @param color_pal The color palette for the scales.
#'
#' @export
viz_pre_post <- function(.data, color_pal) {

  ggplot2::ggplot(.data, aes(.percent, term, fill = response)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = color_pal) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, .25)) +
    ggplot2::geom_text(aes(label = scales::percent(.data[['.percent']], accuracy = 1)), position = position_stack(vjust = .5)) +
    ggplot2::facet_wrap(vars(response_option), ncol = 2) +
    ggplot2::labs(
      x = 'Percentage Response',
      y = NULL,
      fill = NULL,
      title = NULL
    ) +
    plt_no_lines_theme(strip_panel_text_size = 10) +
    ggplot2::theme(legend.position = 'bottom')

}
