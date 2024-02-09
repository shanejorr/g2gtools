#' Convert a data frame of parameters for a single site into a list
#'
#' Converts a data frame into a list of parameters for a single site that can be used by
#' functions to create response dashboards.
#'
#' @param single_df_parameters Data frame with parameters for a single site.
#'      Must include the following columns: "site_name", "tool_name", "format",
#'      "address", "name_column", "only_keep_distinct", "folder_url"
#'
#' @returns A list where each row is an element in the list, names are column names,
#'      and values are the values in the column for the given row.
#'
#' @keywords internal
convert_df_to_list_single_site <- function(single_df_parameters) {

  # ensure all columns are present
  required_cols <- c("site_name", "tool_name", "format", "address", "name_column", "only_keep_distinct", "folder_url")
  df_cols <- colnames(single_df_parameters)
  has_required_cols <- length(setdiff(required_cols, df_cols))

  if (has_required_cols != 0) cli::cli_abort("Your data frame of parameters does not have all the required columns. It must contains the following columns: {required_cols}")

  site_name <- unique(single_df_parameters$site_name)
  folder_url <- unique(single_df_parameters$folder_url)

  if (length(folder_url) != 1) cli::cli_abort("The site '{site_name}' contains more than one `folder_url` values. All values for `folder_url` must be the same per site.")

  single_list_parameters <- single_df_parameters |>
    dplyr::select(!dplyr::all_of(c('site_name', 'folder_url'))) |>
    as.list() |>
    purrr::list_transpose()

  single_list_parameters <- list(
    site_name = site_name,
    folder_url = folder_url,
    tools = single_list_parameters
  )

  return(single_list_parameters)

}


#' Create data frame of responses for a single tool
#'
#' Returns a data frame where each row is a response. Columns are the name of the
#' respondent and the name of the tool.
#'
#' @keywords internal
g2g_get_teacher_names_single_tool <- function(single_tool_list) {

  # make sure the list has all the proper items
  list_names <- sort(names(single_tool_list))

  required_list_names <- sort(c("tool_name", "format", "address", "name_column", "only_keep_distinct"))

  if (!all(required_list_names == list_names)) stop(stringr::str_c("Your list does not contain all the properly named elements. It should contain: '", stringr::str_flatten_comma(required_list_names), "'."))

  if (single_tool_list$format == 'Qualtrics') {

    tool_data <- qualtRics::fetch_survey(single_tool_list$address, col_types = readr::cols(.default = readr::col_character()), verbose = FALSE) |>
      dplyr::filter(.data$Status != "Survey Preview") |>
      dplyr::rename('response_date' = 'StartDate') |>
      dplyr::mutate(response_date = lubridate::ymd_hms(.data$response_date))

  } else if (single_tool_list$format == 'Google Forms') {

    tool_data <- googlesheets4::read_sheet(single_tool_list$address, col_types = 'c') |>
      dplyr::rename('response_date' = 'Timestamp') |>
      dplyr::mutate(response_date = lubridate::mdy_hms(.data$response_date)) |>
      dplyr::mutate(dplyr::across(-.data$response_date, as.character))

  } else {
    stop("`format` item in list must be either 'Google Forms' or 'Qualtrics'")
  }

  tool_data <- tool_data |>
    dplyr::select(dplyr::all_of(single_tool_list$name_column), 'response_date') |>
    tidyr::drop_na() |>
    dplyr::mutate(tool = !!single_tool_list$tool_name) |>
    dplyr::rename(teacher = dplyr::all_of(single_tool_list$name_column))

  if (single_tool_list$only_keep_distinct) {

    tool_data <- tool_data |>
      dplyr::arrange(dplyr::desc(.data$response_date)) |>
      dplyr::distinct(.data$teacher, .keep_all = TRUE)

  }

  # if there are no responses, add a line saying there are no responses
  # we need to do this so that the tool appears on the individual sheets of tools
  # and so that it appears on the total responses with a 0
  if (nrow(tool_data) == 0) {

    tool_data <- tool_data |>
      dplyr::bind_rows(data.frame(teacher = 'No responses', tool = single_tool_list$tool_name, response_date = lubridate::now()))

  }

  tool_data <- tool_data |>
    dplyr::arrange(.data$response_date)

  return(tool_data)

}

#' Create data frame of responses for all tools
#'
#' Returns a data frame where each row is a response. Columns are the name of the
#' respondent and the name of the tool.
#'
#' @param list_of_tools A list of all data tools. Each element in the list is a
#'      separate tool. Each tool should also be a list with the following named values:
#'      - 'tool_name': name of the tool ('Teacher Pre-Training Survey')
#'      - 'format': Either 'Google Forms' or 'Qualtrics'
#'      - 'address': For Google Forms, the web address to the Google Sheet containing
#'           the results. For Qualtrics, the survey ID.
#'      - 'name_column': The column name, as a string, where teacher names are located.
#'      Here is an example of the list format:
#'      list(
#'          list(
#'            tool_name = 'ELA Observations',
#'            format = 'Google Forms',
#'            address = "https://googlesheetsurl.com,
#'            name_column = "Teacher's Last Name"
#'          ),
#'          list(
#'            tool_name = 'Student Survey',
#'            format = 'Qualtrics',
#'            address = "qualtricssurveyID",
#'            name_column = "teacher_name"
#'          )
#'        )
#'
#'  @returns Data frame of all tools, where each row is a respondent. Columns show
#'       the teacher respondent name and the tool.
#'
#' @keywords internal
g2g_get_teacher_names_all_tools <- function(list_of_tools) {

  purrr::map(list_of_tools, g2g_get_teacher_names_single_tool) |>
    purrr::list_rbind()

}

#' Get the total number of responses for all tools
#'
#' Return data frame with the total number of responses for all tools. Data frame
#' also has a column showing the date / time of the most recent response
#'
#' @param teacher_responses A data frame created with `g2g_get_teacher_names_all_tools()`
#'      listing the tool and all teachers who have entries with the tool.
#'
#' @keywords internal
g2g_total_responses <- function(teacher_responses) {

  # remove values that signify no responses so they are not included in counts
  # but make factor first so we can still show these level in overall counts and show 0
  updated_responses <- teacher_responses |>
    dplyr::mutate(tool = forcats::as_factor(.data$tool)) |>
    dplyr::filter(.data$teacher != 'No responses')

  updated_responses |>
    dplyr::summarize(
      n_responses = dplyr::n(),
      most_recent_response = max(.data$response_date),
      .by = 'tool'
    ) |>
    # show 0 for any tool that does not have any responses (does not appear in data)
    tidyr::complete(.data$tool, fill = list(n_responses = 0))

}

#' Return a Google Sheet or create a new one based on the Sheet's title
#'
#' Given a Google Sheet's title, either return the sheet if it already exists
#' or create a new sheet with the given title if it does not exist. If more than
#' one sheet with the given title exists, return an error.
#'
#' @returns Google Sheet that can be edited.
#'
#' @keywords internal
g2g_create_or_return_sheet <- function(dashboard_title, folder_id) {

  # look for the specified folder and return informative error if it doesn't exist
  found_sheets <- tryCatch({
    # Code that might cause an error
    googledrive::drive_ls(path = folder_id, pattern = dashboard_title)
  },
    error = function(e) {
      # Code to run in case of an error
      stop("Could not find the folder specified in `folder_url`.", call. = FALSE)
  }
  )

  n_sheets_found <- nrow(found_sheets)

  if (n_sheets_found > 1) {

    cli::cli_abort("There is more than one sheet already existing with the name {dashboard_title}. Please correct this.")

  } else if (n_sheets_found == 1) {

    sheet <- googlesheets4::gs4_get(found_sheets)

    cli::cli_alert_success("{.field {dashboard_title}} already exists. Writing over it.")

  } else if (n_sheets_found == 0) {

    sheet <- googlesheets4::gs4_create(dashboard_title, sheets = 'Total Responses')

    suppressMessages({
      # move sheet to proper folder
      googledrive::drive_mv(file = sheet, path = folder_id, name = dashboard_title, overwrite = TRUE)

      # need to pull in sheet with this function to get URL
      sheet <- googlesheets4::gs4_get(sheet)
    })

  } else {

    stop(c(
      "There was an error in determining whether the Google Sheet already exists.\n",
      glue::glue("Please run `googlesheets4::gs4_find({site_name} response dashboard)` and check the results.")
    ), call. = FALSE
    )

  }

  return(sheet)

}

#' Create dashboard on Google Sheets with responses
#'
#' Create a dashboard in Google Sheets with the number of responses for data tools.
#' Dashboard contains one sheet with total number of responses for each tool.
#' Additional sheets are also added that show which teachers responded for each tool.
#' Overwrites currently existing dashboards with the same name.
#'
#' @param list_of_tools A list of all data tools. Each element in the list is a
#'      separate tool. Each tool should also be a list with the following named values:
#'      - 'tool_name': name of the tool ('Teacher Pre-Training Survey')
#'      - 'format': Either 'Google Forms' or 'Qualtrics'
#'      - 'address': For Google Forms, the web address to the Google Sheet containing
#'           the results. For Qualtrics, the survey ID.
#'      - 'name_column': The column name, as a string, where teacher names are located.
#'      Here is an example of the list format:
#'      list(
#'          list(
#'            tool_name = 'ELA Observations',
#'            format = 'Google Forms',
#'            address = "https://googlesheetsurl.com,
#'            name_column = "Teacher's Last Name"
#'          ),
#'          list(
#'            tool_name = 'Student Survey',
#'            format = 'Qualtrics',
#'            address = "qualtricssurveyID",
#'            name_column = "teacher_name"
#'          )
#'        )
#' @param site_name Name of site, as string. Added to sheet title and introductory
#'       line of the first sheet in the dashboard.
#' @param folder_url URL, as a string, of the folder where you want to place the dashboard.
#'       Folder must already be created.
#'
#' @returns Displays dashboard in browser for checking. returns `NULL`.
#'
#' @keywords internal
g2g_create_googlesheet_of_responses_from_list_single_site <- function(list_of_tools, site_name, folder_url) {

  # initialize sheet -------------------------
  # do this before pulling in the data so that if there is an issue, we haven't
  # wasted time pulling in the data

  folder_id <- googledrive::as_id(folder_url)

  dashboard_title <- glue::glue("{site_name} response dashboard")

  cli::cli_alert_info("Checking if dashboard already exists...")

  sheet <- g2g_create_or_return_sheet(dashboard_title, folder_id)

  # get data frame of all respondents and calculate overall responses -------

  cli::cli_alert_info("Pulling in all the data...")

  # create single data frame of all teacher names of respondents,
  # which overall counts will be computed from

  # suppress Google Sheets and Qualtrics messages about data imports
  suppressMessages(
    all_teacher_names <- g2g_get_teacher_names_all_tools(list_of_tools)
  )
  # calculate overall number of responses for each data tool
  # will be displayed in first sheet of dashboard
  number_of_responses <- g2g_total_responses(all_teacher_names) |>
    dplyr::rename('Data Tool' = 'tool', 'Number Responses' = 'n_responses', 'Most Recent Response' = 'most_recent_response')

  # will iterate through all unique tools when making sheets of each tool
  unique_tools <- number_of_responses[['Data Tool']]

  # total responses sheet (first sheet) -------------------

  cli::cli_alert_info("Creating the introduction sheet...")

  # clear previous contents of first sheet
  # precautionary measure to ensure none of the old d ata remains
  # needed because I'm not sure if range_write overrites the whole sheet or only the range where data is added
  suppressMessages(
    googlesheets4::range_clear(ss = sheet, sheet = 'Total Responses', range = "A1:Z100")
  )

  # create intro text

  # display current time as 'Last Updated'
  current_time <- lubridate::now(tzone = 'EST') |> format("%Y-%m-%d %H:%M")

  # first two lines of text on first sheet
  intro_text_line1 <- data.frame(text = stringr::str_c(site_name, " Response Dashboard"))
  intro_text_line2 <- data.frame(text = stringr::str_c("Last Update: ", current_time, " EST"))

  suppressMessages({
    googlesheets4::range_write(ss = sheet, data = intro_text_line1, sheet = 'Total Responses', range = "A1", col_names = FALSE)
    googlesheets4::range_write(ss = sheet, data = intro_text_line2, sheet = 'Total Responses', range = "A2", col_names = FALSE)

    # table with total number of responses for each tool
    googlesheets4::range_write(ss = sheet, data = number_of_responses, sheet = 'Total Responses', range = "A4", col_names = TRUE)
  })

  # write note about student survey, if there is a student survey shown
  if ('Student Survey' %in% unique_tools) {

    student_survey_text <- data.frame(text = '*Note: The number of responses for the student survey represent the number of teachers who have administered the survey.')

    # text will start one line after the table with the number of results
    starting_position <- 4 + nrow(number_of_responses) + 2

    suppressMessages(
      googlesheets4::range_write(ss = sheet, data = student_survey_text, sheet = 'Total Responses', range = paste0("A", starting_position), col_names = FALSE)
    )

  }

  # sheets showing teachers who have responded for each tool ---------------

  # iterate through each tool and create a sheet showing all teachers who have
  # responded to the tool, along with date-time of responses

  cli::cli_alert_info("Creating sheets for each data tool...")

  for (single_tool in unique_tools) {

    single_tool_teacher_names <- all_teacher_names |>
      dplyr::filter(.data$tool == !!single_tool) |>
      dplyr::select(!'tool') |>
      dplyr::rename('Teacher' = 'teacher', 'Date of response' = 'response_date')

    suppressMessages({
      googlesheets4::sheet_write(single_tool_teacher_names, ss = sheet, sheet = single_tool)
      googlesheets4::range_autofit(ss = sheet, sheet = single_tool, dimension = 'columns')
    })

  }

  cli::cli_alert_success("Created {site_name} dashboard with {length(unique_tools)} tools.")
  cli::cli_alert_info("URL: {sheet$spreadsheet_url}")

  # display sheet in browser to ensure it worked
  googlesheets4::gs4_browse(sheet)

  invisible(NULL)

}

#' Create Google Sheet response dashboard for all sites in parameters data frame
#'
#' Create dashboards in Google Sheets with the number of responses for data tools.
#' Each dashboard contains one sheet with total number of responses for each tool.
#' Additional sheets are also added that show which teachers responded for each tool.
#' Overwrites currently existing dashboards with the same name. One dashboard is
#' created for each unique value in the `site_name` column.
#'
#' @param df_of_parameters Data frame with parameters for sites.
#'      Must include the following columns:
#'
#'      - "site_name": String of site name
#'      - "tool_name":" String of name of tool
#'      - "format": Format of the tool. Either 'Google Forms' or 'Qualtrics'
#'      - "address": url to responses Sheet for Google Forms, survey ID for Qualtrics
#'      - "name_column": Column in the data that contains teacher names
#'      - "only_keep_distinct": Boolean, whether to only keep distinct teacher names.
#'           Typically will be TRUE for students surveys, FALSE for others.
#'      - "folder_url": url to folder on Google Drive where we want the dashboard to live.
#'           Folder must already be created. The url must be the same for all distinct sites.
#'
#' @returns NULL
#'
#' @export
g2g_create_googlesheet_response_dashboards <- function(df_of_parameters) {

  list_of_parameters_all_sites <- df_of_parameters |>
    dplyr::group_by(.data$site_name) |>
    dplyr::group_split() |>
    purrr::map(convert_df_to_list_single_site)

  for (site in list_of_parameters_all_sites) {

    cli::cli_h1(site$site_name)

    g2g_create_googlesheet_of_responses_from_list_single_site(
      list_of_tools = site$tools,
      site_name = site$site_name,
      folder_url = site$folder_url
    )

  }

  invisible(NULL)

}
