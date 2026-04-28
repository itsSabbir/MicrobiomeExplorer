test_that("calculateCorrelation returns list with correct components", {
  set.seed(42)
  counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10)
  rownames(counts) <- paste0("S", 1:10)
  colnames(counts) <- paste0("OTU", 1:10)
  result <- calculateCorrelation(counts)
  expect_true(is.list(result))
  expect_true(all(c("correlation", "pvalue", "method", "n_taxa") %in% names(result)))
})

test_that("calculateCorrelation correlation matrix is symmetric", {
  set.seed(42)
  counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10)
  rownames(counts) <- paste0("S", 1:10)
  colnames(counts) <- paste0("OTU", 1:10)
  result <- calculateCorrelation(counts)
  expect_equal(result$correlation, t(result$correlation))
})

test_that("calculateCorrelation diagonal is all ones", {
  set.seed(42)
  counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10)
  rownames(counts) <- paste0("S", 1:10)
  colnames(counts) <- paste0("OTU", 1:10)
  result <- calculateCorrelation(counts)
  expect_true(all(diag(result$correlation) == 1))
})

test_that("calculateCorrelation min_prevalence reduces n_taxa", {
  set.seed(42)
  counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10)
  # Make some taxa absent in most samples
  counts[, 1:3] <- 0
  counts[1, 1:3] <- 1
  rownames(counts) <- paste0("S", 1:10)
  colnames(counts) <- paste0("OTU", 1:10)
  result_low  <- calculateCorrelation(counts, min_prevalence = 0.0)
  result_high <- calculateCorrelation(counts, min_prevalence = 0.5)
  expect_gt(result_low$n_taxa, result_high$n_taxa)
})

test_that("calculateCorrelation works for both methods", {
  set.seed(42)
  counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10)
  rownames(counts) <- paste0("S", 1:10)
  colnames(counts) <- paste0("OTU", 1:10)
  expect_equal(calculateCorrelation(counts, method = "spearman")$method, "spearman")
  expect_equal(calculateCorrelation(counts, method = "pearson")$method, "pearson")
})

test_that("calculateCorrelation errors on invalid method", {
  counts <- matrix(rpois(50, 10), nrow = 5, ncol = 10)
  expect_error(calculateCorrelation(counts, method = "kendall"), "spearman.*pearson")
})
