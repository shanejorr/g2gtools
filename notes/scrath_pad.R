# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.

library(tidyverse)
library(googlesheets4)

devtools::load_all()

googlesheets4::gs4_auth("shane.orr@tntp.org")
googledrive::drive_auth("shane.orr@tntp.org")

response_rate_parameters <- readr::read_csv('notes/response-rate-parameters.csv') |>
  filter(site_name == "Redesign G2G")

g2g_create_googlesheet_response_dashboards(response_rate_parameters)

full_url <- "https://docs.google.com/spreadsheets/d/1_S3tNpAbs3xZViQiQO1BtiKuJTkjEfWx1OLnCRkBO08/edit?resourcekey#gid=523023352"
empty_url <- "https://docs.google.com/spreadsheets/d/1Pz2Aq5Ljx4x-iTyRLdWp0EVUnRtWXsVqeftxkxgILNY/edit?resourcekey#gid=396244635"

empty <- read_sheet(empty_url, col_types = 'c')

full <- full |>
  dplyr::mutate(Timestamp = lubridate::mdy_hms(Timestamp)) |>
  dplyr::rename('response_date' = 'Timestamp') |>
  select(teacher = "What is your school email address?", response_date) |>
  mutate(tool = 'Post')

test <- empty |>
  bind_rows(data.frame(teacher = 'No responses', tool = 'Post', response_date = lubridate::now()))

student <- qualtRics::fetch_survey("SV_bBZNvyZpUsmC2BE", col_types = cols(.default = col_character())) |>
  dplyr::select('response_date' = 'StartDate', teacher = teacher_name) |>
  dplyr::mutate(response_date = lubridate::ymd_hms(response_date)) |>
  mutate(tool = 'student')

teacher_responses <- tibble(
  response_date = rep(lubridate::now(), 3),
  teacher = c('No responses', rep('a', 2)),
  tool = c('Post', rep('Pre', 2))
)

updated_responses <- teacher_responses |>
  dplyr::mutate(tool = forcats::as_factor(.data$tool)) |>
  dplyr::filter(.data$teacher != 'No responses')

updated_responses |>
    dplyr::summarize(
      n_responses = dplyr::n(),
      most_recent_response = max(.data$response_date),
      .by = 'tool'
    ) |>
  tidyr::complete(.data$tool, fill = list(n_responses = 0))
