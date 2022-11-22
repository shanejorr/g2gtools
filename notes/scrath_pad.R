# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(g2gtools)

devtools::load_all()


color_pal <- c(Good = "#00A4C7", OK = "#C1C2C4", Bad = "#EA8835")

x_var <- 'percentage'
y_var <- 'question'
fill_var <- 'response'

dfa <- data.frame(
  question = rep(c('Question 1', 'Question 2'), 3),
  response = rep(c('Good', 'OK', 'Bad'), each = 2) |> factor(levels = names(color_pal)),
  percentage = rep(.33333, 6)
)

g2g_viz_likert_centered(dfa, x_var, y_var, fill_var, color_pal)

number_of_scales <- length(color_pal)
number_scales_each_category <- floor(number_of_scales/2)

has_neutral <- !(number_of_scales %% 2 == 0)

positive_scales_int <- 1:number_scales_each_category
negative_scales_int <- (number_of_scales-number_scales_each_category+1):number_of_scales
neutral_scales_int <- if (has_neutral) ceiling(number_of_scales/2) else NULL

positive_scales <- names(color_pal)[positive_scales_int]
negative_scales <- names(color_pal)[negative_scales_int]
neutral_scales <- if (has_neutral) names(color_pal)[neutral_scales_int] else NULL

.data <- .data |>
  tidyr::drop_na(.data[[fill_var]], .data[[fill_var]]) |>
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
  dplyr::group_by(.data[[y_var]],response_category) |>
  dplyr::mutate(category_cumulative = sum(.data[[x_var]])) |>
  dplyr::ungroup()

if (has_neutral) {
  .data$neutral_response <- ifelse(.data[[fill_var]] == neutral_scales, 'Neutral Responses', 'Positive / Negative Responses') |>
    forcats::fct_relevel('Positive / Negative Responses')

  max_neutral <- max(.data[[x_var]][.data$neutral_response == 'Neutral Responses'])

  axis_limit_neutral <- dplyr::case_when(
    max_neutral < .25 ~ .25,
    max_neutral < .5 ~ .5,
    max_neutral < .75 ~ .75,
    max_neutral < .25 ~ .25,
    TRUE ~ 1
  )

  x_intercepts <- data.frame(neutral_response = c('Positive / Negative Responses','Neutral Responses'), intercept = c(0, NA_integer_))
  x_intercepts$neutral_response <- forcats::fct_relevel(x_intercepts$neutral_response, 'Positive / Negative Responses')

} else {

  x_intercepts <- data.frame(neutral_response = c('Positive / Negative Responses'), intercept = 0)

}

axis_label_percent <- function(x) scales::percent(abs(x), accuracy = 1)

text_offset <- .05

plt <- ggplot2::ggplot(.data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = color_pal) +
  ggplot2::geom_vline(data = x_intercepts, ggplot2::aes(xintercept = intercept), linetype = 2) +
  ggplot2::geom_text(
    ggplot2::aes(
      x = ifelse(.data[['category_cumulative']] < 0, .data[['category_cumulative']] - text_offset, .data[['category_cumulative']] + text_offset),
      label = scales::percent(abs(.data[['category_cumulative']]), accuracy = 1)
    ), size = 4.586111
  ) +
  g2g_plt_base_theme(horizontal_barchart = TRUE)


if (has_neutral) {

  plt <- plt +
    ggplot2::facet_wrap(ggplot2::vars(neutral_response), ncol = 2, scales = "free_x") +
    ggh4x::scale_x_facet(neutral_response == 'Positive / Negative Responses', limits = c(-1, 1), breaks = seq(-1, 1, .25), labels = axis_label_percent) +
    ggh4x::scale_x_facet(neutral_response == 'Neutral Responses', limits = c(0, axis_limit_neutral), breaks = seq(0, axis_limit_neutral, .25), labels = axis_label_percent) +
    ggh4x::force_panelsizes(cols = c(1, .2))

} else {

  plt <- plt +
    ggplot2::scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25), labels = axis_label_percent)

}

plt

