% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom-obs.R
\name{g2g_obs_get_ca_data}
\alias{g2g_obs_get_ca_data}
\title{Create a dataset containing aggregate responses for a single Core Action.}
\usage{
g2g_obs_get_ca_data(
  .data,
  core_action,
  scale_order,
  first_obs_factor = "First Observation",
  grouping_columns = NULL
)
}
\arguments{
\item{.data}{Observation data in long form, with the following columns:
'timing', 'core_action_main', 'core_action_minor'. Data should already be aggregated and show
percentages for each core action/ response combination. Can create data with the \code{g2g_obs_calc_perc()} function.}

\item{core_action}{The core action that we want to get data for. A string that mirrors the spelling in the
\code{core_action_main} column.}

\item{scale_order}{The order of the response scale. Can find with the function \code{g2g_obs_map_scales()}}

\item{first_obs_factor}{String representing the first observation category in \code{timing}, as a string.
\code{timing} will be changed to a factor with this string being the first level. Useful for ordering plots.
'Defaults to 'First Observation'}

\item{grouping_columns}{Columns to group by when calculating aggregate of positive responses. Defaults to NULL.
The default grouping columns of 'timing', 'core_action_main', 'core_action_minor' are always used.}
}
\value{
A tibble with aggregate data for a single core action. It contains an additional column with the
aggregate of positive responses.
}
\description{
Creates a dataset of aggregate responses for a single core action. Filters for the specific core action
and adds a column with the percengate of positive responses. This dataset is used for plotting
}
