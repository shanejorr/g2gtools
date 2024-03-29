% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualizations.R
\name{g2g_viz_basic_dodged_bar}
\alias{g2g_viz_basic_dodged_bar}
\title{Create a vertical bar chart with grouping variables for colors. Groups are dodged.}
\usage{
g2g_viz_basic_dodged_bar(
  .data,
  x_var,
  y_var,
  fill_var,
  text_var,
  color_pal,
  text_offset = 0,
  text_color = "black",
  text_size = 4.21,
  font_face = "plain",
  add_vertical_lines = FALSE,
  ...
)
}
\arguments{
\item{.data}{The data set to visualize.}

\item{x_var}{The column name, as a string, of the variable (categorical) on the x axis.}

\item{y_var}{The column name, as a string, of the variable (numeric) on the y axis.}

\item{fill_var}{The color, as a string or hex number, of the bars for the bar chart.}

\item{text_var}{The column name, as a string, of the variable to show as text. This will be the same
number as the variable in \code{y_var}, but users may want to round it or convert it to a percentage.}

\item{color_pal}{Named vector of the custom color palette to use.}

\item{text_offset}{Defined how many units the text should offset horizontally from the y variable.
Use negative numbers to show the text within the bar and positive numbers to place the text
above the bar. Defaults to 0, which is the \code{y_var} value.}

\item{text_color}{The color of the text on the bar chart. Default is 'black'.}

\item{text_size}{The size of the text on the bar chart. Default is 4.21.}

\item{font_face}{The font face of the text numbers that show up in the bars.
One of "plain", "bold", "italic", "bold.italic". Defaults to "plain"}

\item{add_vertical_lines}{Boolean, whether to add a vertical line between each fill group. Default is FALSE.}

\item{...}{Parameters for \code{g2g_plt_theme()}.}
}
\description{
Create a vertical bar chart with grouping variables for colors. Groups are dodged.
}
