% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classroom-obs.R
\name{g2g_obs_create_viz_ca}
\alias{g2g_obs_create_viz_ca}
\title{Create a vertical visualization of observation data for a single Core Action.}
\usage{
g2g_obs_create_viz_ca(.data, core_action, scales_to_use = NULL)
}
\arguments{
\item{.data}{Data created with \code{g2g_obs_get_ca_data()}}

\item{core_action}{The core action that we want to get data for. A string that mirrors the spelling in the
\code{core_action_main} column.}

\item{scales_to_use}{The order of the response scale. Can find with the function \code{g2g_obs_map_scales()}.
Default is to use the scales from the \code{g2g_obs_map_scales(core_action)} function.}
}
\description{
Creates a vertical bar plot for a single Core Action. Chart is faceted by timing.
X axis labels are the core action abbreviations (example: CA 1 Overall)
}
