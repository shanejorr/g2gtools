# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
# library(tidyverse)

googlesheets4::gs4_auth("shane.orr@tntp.org")
googledrive::drive_auth("shane.orr@tntp.org")

devtools::load_all()

pre_teacher_url <- "https://docs.google.com/spreadsheets/d/1_S3tNpAbs3xZViQiQO1BtiKuJTkjEfWx1OLnCRkBO08/edit?resourcekey#gid=523023352"

obs_ela_url <- "https://docs.google.com/spreadsheets/d/1WqHxDzMN4dFadMVWm0_w_ojZ-jKINGsgGwK6RklpqSA/edit?resourcekey#gid=1182466783"

student_survey_id <- "SV_1ENUpq2oW6SNTIG"

site_name <- "Aldine G2G"

list_of_tools <- list(
  list(
    tool_name = 'Teacher Pre-Training Survey',
    format = 'Google Forms',
    address = pre_teacher_url,
    name_column = "What is your school email address?",
    only_keep_distinct = FALSE
  ),
  list(
    tool_name = 'ELA Observations',
    format = 'Google Forms',
    address = obs_ela_url,
    name_column = "Teacher's Last Name",
    only_keep_distinct = FALSE
  ),
  list(
    tool_name = 'Student Survey',
    format = 'Qualtrics',
    address = student_survey_id,
    name_column = "teacher_name",
    only_keep_distinct = TRUE
  )
)

# googledrive::drive_trash(glue::glue("{site_name} response dashboard"))
# Last Update: 2024-01-03 15:03 EST
# dashboard_title <- glue::glue("{site_name} response dashboard")
#
# found_sheets <- googlesheets4::gs4_find(dashboard_title)
# sheet <- googlesheets4::gs4_get(found_sheets)
# sheet$spreadsheet_url
#
# my_sheet <- googlesheets4::gs4_get(glue::glue("{site_name} response dashboard"))
create_googlesheet_of_responses(list_of_tools, site_name)

# https://docs.google.com/spreadsheets/d/1hAEzOjQOYFqEqSq4IqO6xR-LMx7TUqxIi5WyDZqc_6k/edit#gid=2032117741
# https://docs.google.com/spreadsheets/d/1hAEzOjQOYFqEqSq4IqO6xR-LMx7TUqxIi5WyDZqc_6k/edit#gid=2032117741
# https://docs.google.com/spreadsheets/d/1hAEzOjQOYFqEqSq4IqO6xR-LMx7TUqxIi5WyDZqc_6k/edit#gid=2032117741
