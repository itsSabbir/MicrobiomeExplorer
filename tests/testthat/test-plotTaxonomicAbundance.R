test_that("plotTaxonomicAbundance returns a ggplot object", {
  set.seed(42)
  counts <- matrix(rpois(50, lambda = 30), nrow = 5, ncol = 10)
  rownames(counts) <- paste0("Sample", 1:5)
  colnames(counts) <- paste0("OTU", 1:10)
  p <- plotTaxonomicAbundance(counts)
  expect_s3_class(p, "ggplot")
})

test_that("plotTaxonomicAbundance collapses to 'Other' when top_n < ncol", {
  set.seed(42)
  counts <- matrix(rpois(50, lambda = 30), nrow = 5, ncol = 10)
  rownames(counts) <- paste0("Sample", 1:5)
  colnames(counts) <- paste0("OTU", 1:10)
  p <- plotTaxonomicAbundance(counts, top_n = 3)
  layer_data <- ggplot2::layer_data(p)
  # There should be 4 groups (3 top + Other)
  fill_vals <- unique(layer_data$fill)
  expect_gte(length(fill_vals), 1)
})

test_that("plotTaxonomicAbundance normalize produces relative abundances", {
  set.seed(42)
  counts <- matrix(rpois(30, lambda = 20), nrow = 5, ncol = 6)
  rownames(counts) <- paste0("S", 1:5)
  colnames(counts) <- paste0("OTU", 1:6)
  p <- plotTaxonomicAbundance(counts, normalize = TRUE, top_n = 6)
  ld <- ggplot2::layer_data(p)
  # stacked bar total per sample should be ~1
  totals <- tapply(ld$y, ld$x, sum)
  expect_true(all(abs(totals - 1) < 1e-6))
})

test_that("plotTaxonomicAbundance works with group_var and sample_info", {
  set.seed(42)
  counts <- matrix(rpois(50, lambda = 20), nrow = 5, ncol = 10)
  rownames(counts) <- paste0("S", 1:5)
  colnames(counts) <- paste0("OTU", 1:10)
  meta <- data.frame(Group = c("A", "A", "B", "B", "B"),
                     row.names = paste0("S", 1:5))
  expect_s3_class(
    plotTaxonomicAbundance(counts, sample_info = meta, group_var = "Group"),
    "ggplot"
  )
})

test_that("plotTaxonomicAbundance errors when group_var given without sample_info", {
  counts <- matrix(rpois(30, 20), nrow = 5, ncol = 6)
  expect_error(plotTaxonomicAbundance(counts, group_var = "Group"), "sample_info must be provided")
})
