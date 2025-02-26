#' Calculate percentages from long-form data
#'
#' This function calculates percentages from data in long format. It groups by the specified columns
#' and calculates the percentage representation of each category within its parent groups.
#'
#' @param .data A data frame in long format.
#' @param grouping_columns Character vector of column names to group by when calculating percentages.
#'        Percentages are calculated for the values in the last column, within the context of the
#'        other grouping columns.
#' @param count_column Character, an optional column name containing values to sum instead of
#'        counting rows. Default is NULL, which means each row counts as 1.
#' @param add_n One of 'none', '.percent_pretty', or a column name from grouping_columns. Controls how to
#'        add count information:
#'        - 'none': No additional count information (default)
#'        - '.percent_pretty': Add count to the formatted percentage (e.g., "75% (n=10)")
#'        - <column name>: Add count to the specified column from grouping_columns
#'
#' @return A tibble with the original grouping columns plus:
#'  - `.n_response`: count/sum for each group combination
#'  - `.n_total`: total count/sum within parent groups
#'  - `.percent`: the calculated percentage as a decimal (between 0 and 1)
#'  - `.percent_pretty`: formatted percentage string (always included)
#'
#' @examples
#' # Example with a simple dataset
#' data <- data.frame(
#'   category = rep(c("A", "B"), each = 10),
#'   subcategory = c(rep("X", 7), rep("Y", 3), rep("X", 4), rep("Y", 6))
#' )
#'
#' # Calculate percentages of subcategories within categories
#' g2g_calc_percentages(data, grouping_columns = c("category", "subcategory"))
#'
#' @importFrom rlang .data
#' @importFrom dplyr group_by mutate summarize ungroup across between
#' @importFrom tidyr drop_na all_of
#' @importFrom tibble as_tibble
#' @importFrom stringr str_c
#' @importFrom scales percent
#' @importFrom glue glue
#'
#' @export
g2g_calc_percentages <- function(.data, grouping_columns, count_column = NULL, add_n = 'none') {

  # Make sure we have the required columns
  required_columns <- grouping_columns
  if (!is.null(count_column)) required_columns <- c(required_columns, count_column)

  col_names <- colnames(.data)
  if (!all(required_columns %in% col_names)) {
    missing_cols <- setdiff(required_columns, col_names)
    stop(stringr::str_c("`.data` must contain the following columns: ",
                        paste0(missing_cols, collapse = ", ")), call. = FALSE)
  }

  # Validate add_n parameter
  add_n_options <- c('none', '.percent_pretty')
  if (length(grouping_columns) > 0) {
    add_n_options <- c(add_n_options, grouping_columns)
  }

  if (!add_n %in% add_n_options) {
    stop(paste0("`add_n` must be one of '", paste0(add_n_options, collapse= "', "), "'"),
         call. = FALSE)
  }

  # Calculate counts or sums
  if (is.null(count_column)) {
    # Count rows
    df <- .data |>
      tidyr::drop_na(tidyr::all_of(grouping_columns)) |>
      dplyr::group_by(dplyr::across(tidyr::all_of(grouping_columns))) |>
      dplyr::summarize(.n_response = dplyr::n(), .groups = "drop")
  } else {
    # Sum values
    df <- .data |>
      tidyr::drop_na(tidyr::all_of(c(grouping_columns, count_column))) |>
      dplyr::group_by(dplyr::across(tidyr::all_of(grouping_columns))) |>
      dplyr::summarize(.n_response = sum(.data[[count_column]], na.rm = TRUE), .groups = "drop")
  }

  # Calculate totals and percentages
  if (length(grouping_columns) > 1) {
    # Get parent grouping columns (all except the last)
    parent_columns <- grouping_columns[-length(grouping_columns)]

    df <- df |>
      dplyr::group_by(dplyr::across(tidyr::all_of(parent_columns))) |>
      dplyr::mutate(
        .n_total = sum(.data$.n_response),
        .percent = .data$.n_response / .data$.n_total,
        .percent_pretty = scales::percent(.data$.percent, accuracy = 1)
      ) |>
      dplyr::ungroup()
  } else {
    # No parent groups, calculate overall percentage
    df <- df |>
      dplyr::mutate(
        .n_total = sum(.data$.n_response),
        .percent = .data$.n_response / .data$.n_total,
        .percent_pretty = scales::percent(.data$.percent, accuracy = 1)
      )
  }

  # Add count information if requested
  if (add_n == '.percent_pretty') {
    df$.percent_pretty <- glue::glue("{df$.percent_pretty}\n(n={df$.n_response})")
  } else if (add_n %in% grouping_columns) {
    df[[add_n]] <- glue::glue("{df[[add_n]]} (n={df$.n_total})")
  }

  # Verify percentages are valid
  if (!all(dplyr::between(df$.percent, 0, 1))) {
    stop("All expected percentages did not fall between 0 and 1. Please re-check your input data (`.data`)",
         call. = FALSE)
  }

  # Ensure return value is a tibble
  return(tibble::as_tibble(df))
}
