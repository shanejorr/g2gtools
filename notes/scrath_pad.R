# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
# library(tidyverse)

googlesheets4::gs4_auth("shane.orr@tntp.org")
googledrive::drive_auth("shane.orr@tntp.org")

devtools::load_all()

response_rate_parameters <- readr::read_csv('response-rate-parameters.csv')

g2g_create_googlesheet_response_dashboards(response_rate_parameters)

create_googlesheet_response_dashboards <- function(df_of_parameters) {

  list_of_parameters_all_sites <- df_of_parameters |>
    dplyr::group_by(.data$site_name) |>
    dplyr::group_split() |>
    purrr::map(convert_df_to_list_single_site)

  for (site in list_of_parameters_all_sites) {

    cli::cli_h1(site$site_name)

    create_googlesheet_of_responses_from_list_single_site(
      list_of_tools = site$tools,
      site_name = site$site_name,
      folder_url = site$folder_url
    )

  }

  invisible(NULL)

}

create_googlesheet_response_dashboards(response_rate_parameters)
