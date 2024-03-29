% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_operations.R
\name{g2g_db_add_teacher}
\alias{g2g_db_add_teacher}
\title{Add teachers to \code{teacher_information} table}
\usage{
g2g_db_add_teacher(con, .data)
}
\arguments{
\item{con}{Database connection to Good to Great database. Should be created with
\code{DBI::dbConnect(RSQLite::SQLite(), dbname = "good_to_great.db")}.}

\item{.data}{Dataframe of observations. Should be in long-form and must contain all required columns and no more.}
}
\value{
Message indicating success.
}
\description{
Add a dataframe of teachers to the \code{teacher_information} table.
}
