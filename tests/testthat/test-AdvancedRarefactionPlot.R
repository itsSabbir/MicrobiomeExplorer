# Define the number of samples and taxa
num_samples <- 10
num_taxa <- 5

# Set seed for reproducibility
set.seed(42)

# Generate random data to simulate microbiome counts
data_values <- matrix(rpois(num_samples * num_taxa, lambda = 10), nrow = num_samples, ncol = num_taxa)

# Create sample and taxa names
sample_names <- paste("Sample", 1:num_samples, sep = "_")
taxa_names <- paste("Taxa", 1:num_taxa, sep = "_")

# Creating the DataFrame
sample_data <- data.frame(data_values)
colnames(sample_data) <- taxa_names
rownames(sample_data) <- sample_names


test_that("AdvancedRarefactionPlot handles invalid inputs correctly", {
  # Test with non-matrix and non-dataframe data
  expect_error(AdvancedRarefactionPlot(data = list(a = 1, b = 2)))

  # Test with an empty dataset
  expect_error(AdvancedRarefactionPlot(data = matrix(numeric(0), nrow = 0, ncol = 0)))

})



test_that("AdvancedRarefactionPlot correctly represents the data", {
  # Ensure ggplot2 is loaded or use ggplot2::ggplot_build
  library(ggplot2)

  # Test with the sample_data
  known_plot <- AdvancedRarefactionPlot(data = sample_data)

  # Validate axis labels
  expect_equal(known_plot$labels$x, "Sample Size") # Adjust according to your function
  expect_equal(known_plot$labels$y, "Diversity Index") # Adjust according to your function

  # Check for distinguishable curves
  num_of_lines_in_plot <- length(unique(ggplot2::ggplot_build(known_plot)$data[[1]]$group))
  expect_equal(num_of_lines_in_plot, nrow(sample_data))
})




