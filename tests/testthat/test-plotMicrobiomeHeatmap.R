# Test plot_microbiome_heatmap with valid data
test_that("plot_microbiome_heatmap creates a heatmap object", {
  test_data <- matrix(rnorm(20), nrow = 5)
  result <- plot_microbiome_heatmap(test_data)
  expect_s4_class(result, "Heatmap")
})

# Test plot_microbiome_heatmap with non-numeric data
test_that("plot_microbiome_heatmap handles non-numeric data", {
  test_data <- data.frame(Taxa1 = c("A", "B", "C"), Taxa2 = c("D", "E", "F"))
  expect_error(plot_microbiome_heatmap(test_data))
})
