#' Create PPT deck with the TNTP template
#'
#' Initialize a PPT deck with the TNTP template. This function also creates the title page.
#'
#' @param title The title that goes on the title page as a string.
#' @param subtitle The subtitle that goes on the title page as a string.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_create_deck_ppt <- function(title, subtitle) {

  # create today's month and year, to add to title slide
  today_date <- lubridate::today()
  today_month <- lubridate::month(today_date, label = TRUE, abbr = FALSE)
  today_year <- lubridate::year(today_date)
  month_year <- paste0(today_month, " ", today_year)

  # read in TNTP template deck for formatting
  doc <- officer::read_pptx(path = system.file("extdata", 'tntp_template.pptx', package = 'g2gtools'))

  # create title slide
  doc <- officer::add_slide(doc, "Title Slide", 'TNTP Template 2013')

  # add title, subtitle, and date
  doc <- officer::ph_with(doc, value = title, location = officer::ph_location_type(type = "ctrTitle"))
  doc <- officer::ph_with(doc, value = subtitle, location = officer::ph_location_type(type = "subTitle"))
  doc <- officer::ph_with(doc, value = month_year, location = officer::ph_location_label(ph_label = "Text Placeholder 5", newlabel = 'siteName'))

  return(doc)

}

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
#' @param notes_text Text, as a string, of notes to add to slide. Entire note must be one string.
#'      Use `\n` within the string to add line breaks. Defaults to `NULL`, or no notes.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_add_viz_ppt <- function(doc, slide_plot, slide_header, plt_width, plt_height, notes_text = NULL) {

  # create new slide with default template
  doc <- officer::add_slide(doc, "Title Only", 'TNTP Template 2013')

  # determine width so that plot is centered
  s_s <- officer::slide_size(doc)
  s_w <- s_s$width # width of slides
  left <- (s_w/2) - (plt_width/2)

  # add header text to slide
  doc <- officer::ph_with(doc, value = slide_header, location = officer::ph_location_type(type = "title"))

  #convert plot to vector graphic
  vec_plt <- rvg::dml(ggobj = slide_plot)

  # add plot to slide
  slide_loc <- officer::ph_location(left = left, top = 1.25, width = plt_width, height = plt_height, newlabel = "plot")

  doc <- officer::ph_with(doc, value = vec_plt, location = slide_loc)

  # add notes, if needed
  if (!is.null(notes_text)) {

    doc <- officer::set_notes(doc, value = notes_text, location = officer::notes_location_type("body"))

  }

  return(doc)

}

#' Add tables to Power Point slides
#'
#' Add tables to PPT slides. You must initialize the PPT deck with
#' \code{doc <- officer::read_pptx()}. After adding tables to the deck with this function, you can
#' write out the deck with \code{print(doc, target = 'file_name_of_deck.pptx')}
#'
#' @param doc Document object. Created with \code{officer::read_pptx()}.
#' @param .data A data frame containing the table that will be added to a PPT slide.
#' @param slide_header The slide header, as a string.
#' @param col_lengths A numeric vector containing the length in inches of each column.
#'      The vector's length should equal the number of columns.
#' @param fontsize The font size of column names and table text, as an integer. Defaults to 12.
#' @param notes_text Text, as a string, of notes to add to slide. Entire note must be one string.
#'      Use `\n` within the string to add line breaks. Defaults to `NULL`, or no notes.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_add_table_ppt <- function(doc, .data, slide_header, col_lengths, fontsize = 12, notes_text = NULL) {

  n_cols <- ncol(.data)

  if (n_cols != length(col_lengths)) {
    stop("The length of `col_length` should equal the number of columns in `.data`.", call. = FALSE)
  }

  # create new slide with default template
  doc <- officer::add_slide(doc, "Title Only", 'TNTP Template 2013')

  # determine width so that plot is centered
  s_s <- officer::slide_size(doc)
  s_w <- s_s$width # width of slides
  left <- (s_w/2) - (sum(col_lengths)/2)

  cols <- seq(1, n_cols)

  small_border = officer::fp_border(color="gray", width = 1)

  flex_table <- .data |>
    flextable::flextable() |>
    flextable::width(j = cols, width = col_lengths, unit = "in") |>
    flextable::border_inner_h(part="all", border = small_border) |>
    flextable::border_inner_v(part="all", border = small_border) |>
    flextable::font(fontname = "Segoe UI", part = "all") |>
    flextable::fontsize(size = fontsize, part = "all") |>
    flextable::bold(bold = TRUE, part = "header") |>
    flextable::align(align = "left", part = "all")

  slide_loc <- officer::ph_location(left = left, top = 1.25, newlabel = "table")

  doc <- doc |>
    officer::ph_with(
      flex_table,
      location = slide_loc
    ) |>
    officer::ph_with(value = slide_header, location = officer::ph_location_type(type = "title"))

  # add notes, if needed
  if (!is.null(notes_text)) {

    doc <- officer::set_notes(doc, value = notes_text, location = officer::notes_location_type("body"))

  }

  return(doc)

}

#' Add custom notes to Power Point slides
#'
#' Predefined custom notes that can be added to Power Point slide notes to help explain the data.
#'
#' @param note_name The names of the predefined note. One of "HE score", "IPG score", or "Inst Practice score".
#'
#' @returns A string with the note, which can be added to a slide with the `notes_text` parameter of
#'      `g2g_add_viz_ppt` or `g2g_add_table_ppt`.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_slide_notes <- function(note_name) {

  he_score <- c(
    "High expectations scores are single scores that incorporate all four HE questions.",
    "They are calculated as follows:",
    "- A HE score is first calculated for each teacher.",
    "   - To calculate the score, each teacher's response is converted to a number, with Strongly Disagree being a 0 and Strongly Agree being a 5.",
    "   - These four number values are then summed for each teacher. This is the teacher's HE score.",
    "- The overall HE score that you see in the table is the average of all teacher HE scores.",
    "- A teacher has high expectations if the teacher's HE score is greater than or equal to 12.",
    "- Therefore, the percentage of teachers with HE is the percentage of teachers with HE scores greater than or equal to 12."
  ) |>
    stringr::str_c(collapse = '\n')

  ipg_score <- c(
    "IPG scores are single scores that incorporate the following Core Actions.",
    "- Core Action 1A",
    "- Core Action 1B",
    "- Core Action 1C",
    "- Core Action 2 Overall",
    "- Core Action 3 Overall",
    "- Culture of Learning",
    "For Core Action 1 items, teachers receive 1 point for a Yes a 0 points for a No.",
    "Other items are assigned points as follows: 1 (Not Yet), 2 (Somewhat), 3 (Mostly), and 4 (Yes).",
    "All points are summed for each teacher to arrive at the teacher's IPG score.",
    "The overall score is the average of all teacher scores."
  ) |>
    stringr::str_c(collapse = '\n')

  instr_practice_score <- c(
    "The instructional practice score is an aggregate sore for all instructional practices questions.",
    "The score is calculated by first converting the scale responses to numbers.",
    "'Never' is scored a 1 and 'In All or Most Lessons' is scored a 5'.",
    "The final instructional practice score is the average of all these numeric responses."
  ) |>
    stringr::str_c(collapse = '\n')

  list_of_notes <- list(
    'HE score' = he_score,
    "IPG score" = ipg_score,
    'Inst Practice score' = instr_practice_score
  )

  list_of_notes[[note_name]]

}
