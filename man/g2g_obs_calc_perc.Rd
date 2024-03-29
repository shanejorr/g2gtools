% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom-obs.R
\name{g2g_obs_calc_perc}
\alias{g2g_obs_calc_perc}
\title{Calculate the response percentages of each core action by pre and post training}
\usage{
g2g_obs_calc_perc(.data, grouping_columns = NULL)
}
\arguments{
\item{.data}{Observation data that is already in long form, created by \code{g2g_obs_long_form()}.}

\item{grouping_columns}{Columns to group by when calculating percentages. The following
columns are automatically added for grouping: 'timing', 'core_action_main', 'core_action_minor'.}
}
\value{
A tibble with aggregated observation result percentages for each rating. Aggregated by core action and timing (pre or post)
}
\description{
Calculate the response percentages of each core action by pre and post training
}
