# Test calculate_stats with valid data
test_that("calculate_stats returns correct results with valid data", {
  test_data <- data.frame(
    Taxa1 = c(1, 2, 3, 4, 5),
    Taxa2 = c(2, 3, 4, 5, 6)
  )
  result <- calculate_stats(test_data)
  expect_true(all(colnames(result) == c("mean", "median", "sd", "variance", "range_min", "range_max", "IQR")))
  expect_equal(result$mean, c(3, 4))
  expect_equal(result$median, c(3, 4))
})

# Test calculate_stats with empty data
test_that("calculate_stats handles empty data", {
  test_data <- data.frame()
  expect_error(calculate_stats(test_data), "Data must have at least one column.")
})
