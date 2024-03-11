library(tidyverse)

devtools::load_all()

googlesheets4::gs4_auth("shane.orr@tntp.org")
googledrive::drive_auth("shane.orr@tntp.org")

df_of_parameters <- readr::read_csv('notes/response-rate-parameters.csv'
                                    #, col_types = cols(time_filter = col_date(format = "%m/%d/%Y"))
  )

g2g_create_googlesheet_response_dashboards(df_of_parameters, add_note_at_bottom = 'TEST')

as.Date(df_of_parameters$time_filter[1])

# filter by time

teacher_survey <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1z8fkiC77roE7MqeiA4XOUiFgeiLza1BGtL7zV1mZM2Q/edit#gid=89856682", col_types = 'c')

date <- "2024-04-01"

is_date('sdf')

as.Date('dsf')

teacher_survey1 <- teacher_survey |>
  dplyr::rename('response_date' = 'Timestamp') |>
  dplyr::mutate(response_date = lubridate::mdy_hms(.data$response_date)) |>
  dplyr::filter(.data[['response_date']] > as.Date("2023-04-01"))
