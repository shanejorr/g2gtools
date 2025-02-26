library(testthat)

test_that("g2g_calc_percentages calculates percentages correctly", {

  test_data <- tibble::tibble(
    category = rep(c("A", "B"), each = 10),
    subcategory = c(rep("X", 7), rep("Y", 3), rep("X", 4), rep("Y", 6)),
    # Create values that align properly with categories and subcategories
    value = c(
      # Category A (10 rows)
      rep(1, 7),  # 7 X's with value 1
      rep(2, 3),  # 3 Y's with value 2
      # Category B (10 rows)
      rep(3, 4),  # 4 X's with value 3
      rep(4, 6)   # 6 Y's with value 4
    )
  )

  # Test 1: Basic percentage calculation with two grouping columns
  result <- g2g_calc_percentages(test_data, grouping_columns = c("category", "subcategory"))

  # Check structure
  expect_equal(ncol(result), 6) # Original columns + 4 new columns
  expect_true(all(c(".n_response", ".n_total", ".percent", ".percent_pretty") %in% colnames(result)))

  # Check calculations for category A
  a_rows <- result[result$category == "A", ]
  expect_equal(a_rows$subcategory, c("X", "Y"))
  expect_equal(a_rows$.n_response, c(7, 3))
  expect_equal(a_rows$.n_total, c(10, 10))
  expect_equal(a_rows$.percent, c(0.7, 0.3))

  # Check calculations for category B
  b_rows <- result[result$category == "B", ]
  expect_equal(b_rows$subcategory, c("X", "Y"))
  expect_equal(b_rows$.n_response, c(4, 6))
  expect_equal(b_rows$.n_total, c(10, 10))
  expect_equal(b_rows$.percent, c(0.4, 0.6))

  # Test 2: Using count_column to sum values instead of counting rows
  result_sum <- g2g_calc_percentages(test_data,
                                     grouping_columns = c("category", "subcategory"),
                                     count_column = "value")

  # Category A calculations
  a_rows_sum <- result_sum[result_sum$category == "A", ]
  expect_equal(a_rows_sum$.n_response, c(7, 6))  # X: 7*1=7, Y: 3*2=6
  expect_equal(a_rows_sum$.n_total, c(13, 13))   # Total: 7+6=13
  expect_equal(a_rows_sum$.percent, c(7/13, 6/13))

  # Category B calculations
  b_rows_sum <- result_sum[result_sum$category == "B", ]
  expect_equal(b_rows_sum$.n_response, c(12, 24))  # X: 4*3=12, Y: 6*4=24
  expect_equal(b_rows_sum$.n_total, c(36, 36))     # Total: 12+24=36
  expect_equal(b_rows_sum$.percent, c(12/36, 24/36))

  # Test 3: add_n parameter with '.percent_pretty'
  result_pretty <- g2g_calc_percentages(test_data,
                                        grouping_columns = c("category", "subcategory"),
                                        add_n = '.percent_pretty')

  # Check formatting of .percent_pretty
  expect_match(result_pretty$.percent_pretty[1], "70%\\s*\\(n=7\\)")

  # Test 4: add_n parameter with grouping column name
  result_group_n <- g2g_calc_percentages(test_data,
                                         grouping_columns = c("category", "subcategory"),
                                         add_n = 'subcategory')

  # Check formatting of subcategory
  expect_match(result_group_n$subcategory[1], "X \\(n=10\\)")

  # Test 5: Single grouping column
  result_single <- g2g_calc_percentages(test_data, grouping_columns = c("category"))
  expect_equal(nrow(result_single), 2)
  expect_equal(result_single$category, c("A", "B"))
  expect_equal(result_single$.n_response, c(10, 10))
  expect_equal(result_single$.n_total, c(20, 20))
  expect_equal(result_single$.percent, c(0.5, 0.5))

  # Test 6: Error handling for missing columns
  expect_error(g2g_calc_percentages(test_data, grouping_columns = c("nonexistent")),
               "`.data` must contain the following columns")

  # Test 7: Error handling for invalid add_n
  expect_error(g2g_calc_percentages(test_data,
                                    grouping_columns = c("category", "subcategory"),
                                    add_n = 'invalid'),
               "`add_n` must be one of")
})
