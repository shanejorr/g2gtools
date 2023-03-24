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
#' @param text_box Text to add under a graph in a text box. Default is NULL, which is no text.
#' @param vector_plt Should the visualization render as a vector graphic in power point. TRUE for yes,
#'      FALSE for no. Defaults to TRUE.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_add_viz_ppt <- function(doc, slide_plot, slide_header, plt_width = 9.5, plt_height = 5, notes_text = NULL, text_box = NULL, vector_plt = TRUE) {

  if (!vector_plt %in% c(TRUE, FALSE)) stop("`vector_plt` must be either TRUE or FALSE", call. = FALSE)

  # create new slide with default template
  doc <- officer::add_slide(doc, "Title Only", 'TNTP Template 2013')

  # determine width so that plot is centered
  s_s <- officer::slide_size(doc)
  s_w <- s_s$width # width of slides
  left <- (s_w/2) - (plt_width/2)

  # determine height so plot is centered
  default_height <- 1.25
  footer_height <- .4
  s_h <- s_s$height
  h_middle <- ((s_h - default_height)/2) - (plt_height / 2)
  top <- h_middle + default_height - footer_height
  top <- if (top < default_height) default_height else top

  # location of text to add under plot
  text_box_height <- .3
  text_adj_height <- .25
  text_location <- top + plt_height + text_box_height - text_adj_height

  # adjust plot height if there is a text box
  if (!is.null(text_box)) {
    top <- top + text_box_height - text_adj_height - .1
  }

  # add header text to slide
  doc <- officer::ph_with(doc, value = slide_header, location = officer::ph_location_type(type = "title"))

  #convert plot to vector graphic, if needed
  if (vector_plt) {
    slide_plot <- rvg::dml(ggobj = slide_plot)
  }

  # add plot to slide
  slide_loc <- officer::ph_location(left = left, top = top, width = plt_width, height = plt_height, newlabel = "plot")

  doc <- officer::ph_with(doc, value = slide_plot, location = slide_loc)

  # add text box, if needed
  if (!is.null(text_box)) {
    text_format <- officer::fpar(
      officer::ftext(
        text_box,
        officer::fp_text(font.size = 14, bold = TRUE, font.family = "Segoe UI", italic = TRUE, color = "#00A4C7")
      ),
      fp_p = officer::fp_par(text.align = "center")
    )
    text_loc <- officer::ph_location(left = .25, top = text_location, width = s_w - .5, height = .75)
    doc <- officer::ph_with(doc, value = text_format, location = text_loc)
  }

  # add notes, if needed
  if (!is.null(notes_text)) {

    doc <- officer::set_notes(doc, value = notes_text, location = officer::notes_location_type("body"))

  }

  return(doc)

}

#' Calculate optimal height for a visualizations that is being inserted into a PPT.
#'
#' This function is useful for horizontal bar charts. It calculates an optimal height for the plot
#' when it is placed into the PPT based on the number of rows in the horizontal bar chart.
#'
#' @param column_with_rows The column, as a vector, of your data that will be the rows in the
#'      horizontal bar chart. The number of rows should equal the number of unique items in this column.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_ppt_calculate_plot_height <- function(column_with_rows) {

  plt_height_multiplier <- .77

  # find number of questions to determine plot height
  n_questions <- length(unique(column_with_rows))

  plt_height <- plt_height_multiplier*n_questions+2

  plt_height <- if (n_questions == 2) plt_height * 1.1 else plt_height

  return(plt_height)

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
#' @param note_name The names of the predefined note. One of: "HE score", HE score" summary,
#'      "IPG score", "IPG score summary", "Inst Practice score", "Inst Practice score summary".
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
    "Scores fall between 0 and 20. A teacher has high expectations if the teacher's HE score is greater than or equal to 12.",
    "HE scores are calculated as follows:",
    "- A HE score is first calculated for each teacher.",
    "   - To calculate the score, each teacher's response is converted to a number, with Strongly Disagree being a 0 and Strongly Agree being a 5.",
    "   - These four number values are then summed for each teacher. This is the teacher's HE score.",
    "- The overall HE score that you see in the table is the average of all teacher HE scores."
  ) |>
    stringr::str_c(collapse = '\n')

  he_score_summary <- "High expectations scores fall between 0 and 20. A score of 12 or higher represents high expectations."

  ipg_score <- c(
    "IPG scores (observation scores) are single scores that incorporate the following Core Actions.",
    "They fall between 0 and 3, and 2 or higher represents strong instruction.",
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

  ipg_score_summary <- "Observation scores fall between 0 and 3. A score of 2 or higher represents strong instruction."

  instr_practice_score <- c(
    "The instructional practice score is an aggregate sore for all instructional practices questions.",
    "The score is calculated by first converting the scale responses to numbers.",
    "'Never' is scored a 1 and 'In All or Most Lessons' is scored a 5'.",
    "The final instructional practice score is the average of all these numeric responses and falls between 1 and 5."
  ) |>
    stringr::str_c(collapse = '\n')

  instr_practice_score_summary <- "Instructional practice scores fall between 1 and 5."

  list_of_notes <- list(
    'HE score' = he_score,
    'HE score summary' = he_score_summary,
    "IPG score" = ipg_score,
    'IPG score summary' = ipg_score_summary,
    'Inst Practice score' = instr_practice_score,
    'Inst Practice score summary' = instr_practice_score_summary
  )

  if (!note_name %in% names(list_of_notes)) {

    all_names <- paste0(names(list_of_notes), collapse = ", '")

    stop(
      paste0("`note_names` must be one of: ", "'", all_names, "'.")
    )
  }

  list_of_notes[[note_name]]

}

#' Create notes based on items in your data
#'
#' Your data might contain information that is useful for notes. For example, one column might contain
#' categories (CA1a, CA1b, etc.) and a different column might contain descriptions of the category.
#' This function creates notes from the categories and descriptions. it is of the format, "category: description".
#'
#' @param .data A data frame containing columns with the categories and descriptions.
#' @param note_category Column name, as a string, of the column containing the category (CA1a, CA1b, etc.)
#' @param note_description Column name, as a string, of the column containing the description of the category.
#' @param note_title A string that will be placed on the top of the note.
#'
#' @returns A string with the note, which can be added to a slide with the `notes_text` parameter of
#'      `g2g_add_viz_ppt` or `g2g_add_table_ppt`.
#'
#' @importFrom rlang .data
#'
#' @export
g2g_notes_from_data <- function(.data, note_category, note_description, note_title = NULL) {

  custom_note <- .data |>
    dplyr::mutate(full_description = glue::glue("- {.data[[note_category]]}: {.data[[note_description]]}")) |>
    dplyr::distinct(.data[['full_description']]) |>
    dplyr::pull(.data[['full_description']]) |>
    stringr::str_wrap(400, exdent = 5) |>
    stringr::str_c(collapse = "\n")

  stringr::str_c(note_title, "\n", custom_note)

}
