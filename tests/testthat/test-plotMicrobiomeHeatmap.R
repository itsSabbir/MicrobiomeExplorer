test_that("plotMicrobiomeHeatmap creates a heatmap object", {
  test_data <- matrix(abs(rnorm(20)), nrow = 5)
  rownames(test_data) <- paste0("S", 1:5)
  colnames(test_data) <- paste0("T", 1:4)
  result <- plotMicrobiomeHeatmap(test_data)
  expect_s4_class(result, "Heatmap")
})

test_that("plotMicrobiomeHeatmap handles non-numeric data", {
  test_data <- data.frame(Taxa1 = c("A", "B", "C"), Taxa2 = c("D", "E", "F"))
  expect_error(plotMicrobiomeHeatmap(test_data))
})

test_that("plotMicrobiomeHeatmap normalization works", {
  test_data <- matrix(rpois(20, 10), nrow = 5, ncol = 4)
  rownames(test_data) <- paste0("S", 1:5)
  colnames(test_data) <- paste0("T", 1:4)
  result <- plotMicrobiomeHeatmap(test_data, normalize = TRUE)
  expect_s4_class(result, "Heatmap")
})

test_that("plotMicrobiomeHeatmap errors on zero-sum rows with normalization", {
  test_data <- matrix(0, nrow = 3, ncol = 4)
  test_data[1, ] <- c(1, 2, 3, 4)
  rownames(test_data) <- paste0("S", 1:3)
  colnames(test_data) <- paste0("T", 1:4)
  expect_error(plotMicrobiomeHeatmap(test_data, normalize = TRUE), "zero")
})

test_that("plotMicrobiomeHeatmap respects clustering options", {
  test_data <- matrix(rpois(30, 10), nrow = 5, ncol = 6)
  rownames(test_data) <- paste0("S", 1:5)
  colnames(test_data) <- paste0("T", 1:6)
  result <- plotMicrobiomeHeatmap(test_data, cluster_rows = FALSE, cluster_cols = FALSE)
  expect_s4_class(result, "Heatmap")
})

test_that("plotMicrobiomeHeatmap accepts custom color palette", {
  test_data <- matrix(rpois(20, 10), nrow = 5, ncol = 4)
  rownames(test_data) <- paste0("S", 1:5)
  colnames(test_data) <- paste0("T", 1:4)
  custom_colors <- c("blue", "white", "red")
  result <- plotMicrobiomeHeatmap(test_data, color_palette = custom_colors)
  expect_s4_class(result, "Heatmap")
})

test_that("plotMicrobiomeHeatmap rejects invalid input", {
  expect_error(plotMicrobiomeHeatmap("text"), "matrix or dataframe")
  expect_error(plotMicrobiomeHeatmap(matrix(nrow = 0, ncol = 0)), "non-zero dimensions")
})
