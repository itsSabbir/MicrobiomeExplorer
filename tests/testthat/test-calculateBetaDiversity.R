test_that("calculateBetaDiversity returns a dist object", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 20), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  result <- calculateBetaDiversity(counts, method = "bray")
  expect_s3_class(result, "dist")
})

test_that("calculateBetaDiversity works for all three methods", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 20), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  expect_s3_class(calculateBetaDiversity(counts, method = "bray"),      "dist")
  expect_s3_class(calculateBetaDiversity(counts, method = "jaccard"),   "dist")
  expect_s3_class(calculateBetaDiversity(counts, method = "euclidean"), "dist")
})

test_that("calculateBetaDiversity binary flag changes output", {
  set.seed(42)
  counts <- matrix(rpois(60, lambda = 20), nrow = 10, ncol = 6)
  rownames(counts) <- paste0("Sample", 1:10)
  colnames(counts) <- paste0("OTU", 1:6)
  d_count  <- calculateBetaDiversity(counts, method = "jaccard", binary = FALSE)
  d_binary <- calculateBetaDiversity(counts, method = "jaccard", binary = TRUE)
  expect_false(all(as.numeric(d_count) == as.numeric(d_binary)))
})

test_that("calculateBetaDiversity preserves sample names", {
  set.seed(42)
  counts <- matrix(rpois(30, lambda = 10), nrow = 5, ncol = 6)
  rownames(counts) <- paste0("S", 1:5)
  result <- calculateBetaDiversity(counts)
  expect_equal(attr(result, "Labels"), paste0("S", 1:5))
})

test_that("calculateBetaDiversity errors on invalid method", {
  counts <- matrix(rpois(30, 10), nrow = 5, ncol = 6)
  expect_error(calculateBetaDiversity(counts, method = "manhattan"), "method must be one of")
})

test_that("calculateBetaDiversity errors on negative values", {
  counts <- matrix(c(-1, 1, 2, 3, 4, 5), nrow = 2)
  expect_error(calculateBetaDiversity(counts), "non-negative")
})

test_that("calculateBetaDiversity accepts data.frame input", {
  set.seed(42)
  df <- as.data.frame(matrix(rpois(30, 10), nrow = 5, ncol = 6))
  rownames(df) <- paste0("S", 1:5)
  expect_s3_class(calculateBetaDiversity(df), "dist")
})
