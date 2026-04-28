test_that("performDifferentialExpression rejects non-matrix input", {
  expect_error(
    performDifferentialExpression(data.frame(a = 1:5), factor(c("A", "B", "A", "B", "A"))),
    "must be a matrix"
  )
})

test_that("performDifferentialExpression rejects mismatched conditions", {
  mat <- matrix(rpois(100, 20), nrow = 20, ncol = 5)
  conditions <- factor(c("A", "B", "A"))
  expect_error(
    performDifferentialExpression(mat, conditions),
    "length equal to the number of columns"
  )
})

test_that("performDifferentialExpression rejects non-factor conditions", {
  mat <- matrix(rpois(80, 20), nrow = 20, ncol = 4)
  expect_error(
    performDifferentialExpression(mat, c("A", "B", "A", "B")),
    "must be a factor"
  )
})

test_that("performDifferentialExpression rejects invalid analysisType", {
  mat <- matrix(rpois(80, 20), nrow = 20, ncol = 4)
  conditions <- factor(rep(c("A", "B"), 2))
  expect_error(
    performDifferentialExpression(mat, conditions, analysisType = "InvalidType"),
    "must be either"
  )
})

test_that("performDifferentialExpression DESeq2 returns list with DESeq2 element", {
  skip_if_not_installed("DESeq2")
  set.seed(42)
  mat <- matrix(rpois(200, lambda = 50), nrow = 50, ncol = 4)
  conditions <- factor(rep(c("Control", "Treatment"), 2))
  result <- performDifferentialExpression(mat, conditions, analysisType = "DESeq2")
  expect_true(is.list(result))
  expect_true("DESeq2" %in% names(result))
})

test_that("performDifferentialExpression EdgeR returns list with EdgeR element", {
  skip_if_not_installed("edgeR")
  set.seed(42)
  mat <- matrix(rpois(200, lambda = 50), nrow = 50, ncol = 4)
  conditions <- factor(rep(c("Control", "Treatment"), 2))
  result <- performDifferentialExpression(mat, conditions, analysisType = "EdgeR")
  expect_true(is.list(result))
  expect_true("EdgeR" %in% names(result))
})
