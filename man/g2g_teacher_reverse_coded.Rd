% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teacher-survey.R
\name{g2g_teacher_reverse_coded}
\alias{g2g_teacher_reverse_coded}
\title{Identify reverse coded teacher survey questions}
\usage{
g2g_teacher_reverse_coded(.data)
}
\arguments{
\item{.data}{Teacher survey data set}
}
\value{
The same data set with two modifications. First, there is an additional logical column added
to the data set called reverse_coded that is TRUE if the question is reverse coded, FALSE otherwise.
Second, an asterisk is placed at the end of the question stem
}
\description{
Adds a column to your data set showing whether a question is reverse coded (disagree is good).
Should be ran prior to aggregating data.
}
