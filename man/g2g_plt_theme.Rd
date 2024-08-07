% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualizations.R
\name{g2g_plt_theme}
\alias{g2g_plt_theme}
\title{Custom Good to Great Plot Theme}
\usage{
g2g_plt_theme(font_size = 24, plot_position = "plot", ...)
}
\arguments{
\item{font_size}{The is the \code{base_size} parameter in \code{tntpr::tntp_style()}. Defaults to 24.}

\item{plot_position}{The is the \code{plot.title.position} parameter. Defaults to 'plot'. Can also be 'panel'.}

\item{...}{Parameters for \code{tntpr::tntp_style()}}
}
\description{
Custom theme for Good to Great that relies on the \code{tntpr::tntp_style()} theme.
Adds check to ensure the 'Halyard Display' font it loaded, and if not loads the font.
}
