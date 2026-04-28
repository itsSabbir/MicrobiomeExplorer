test_that("calculateStats returns correct results with valid data", {
  test_data <- data.frame(
    Taxa1 = c(1, 2, 3, 4, 5),
    Taxa2 = c(2, 3, 4, 5, 6)
  )
  result <- calculateStats(test_data)
  expect_true(all(colnames(result) == c("mean", "median", "sd", "variance", "range_min", "range_max", "IQR")))
  expect_equal(result$mean, c(3, 4))
  expect_equal(result$median, c(3, 4))
})

test_that("calculateStats handles empty data", {
  test_data <- data.frame()
  expect_error(calculateStats(test_data), "Data must have at least one column.")
})

test_that("calculateStats computes exact sd and IQR", {
  test_data <- data.frame(x = c(2, 4, 4, 4, 5, 5, 7, 9))
  result <- calculateStats(test_data)
  expect_equal(result$sd, sd(test_data$x), tolerance = 1e-10)
  expect_equal(result$IQR, IQR(test_data$x), tolerance = 1e-10)
  expect_equal(result$variance, var(test_data$x), tolerance = 1e-10)
})

test_that("calculateStats handles single-row data", {
  test_data <- data.frame(a = 5, b = 10)
  result <- calculateStats(test_data)
  expect_equal(result$mean, c(5, 10))
  expect_true(is.na(result$sd[1]))
})

test_that("calculateStats works with matrix input", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  result <- calculateStats(mat)
  expect_equal(nrow(result), 4)
  expect_equal(result$mean[1], mean(1:3))
})

test_that("calculateStats handles NA values gracefully", {
  test_data <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, NA, NA))
  result <- calculateStats(test_data)
  expect_equal(result$mean[1], mean(c(1, 2, 4)))
  expect_true(is.nan(result$mean[2]) || is.na(result$mean[2]))
})

test_that("calculateStats rejects non-matrix/data.frame input", {
  expect_error(calculateStats("text"), "matrix or dataframe")
  expect_error(calculateStats(list(a = 1)), "matrix or dataframe")
})
