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

capture_de_messages <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(
    expr,
    message = function(message) {
      messages <<- c(messages, conditionMessage(message))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, messages = messages)
}

edger_counts <- matrix(
  as.integer(c(
    40, 42, 41, 120, 130, 125,
    35, 37, 34, 95, 98, 102,
    100, 98, 102, 30, 32, 28,
    80, 76, 84, 24, 20, 22,
    50, 52, 49, 51, 50, 52,
    60, 58, 62, 59, 63, 61,
    20, 22, 21, 18, 19, 20,
    5, 6, 7, 30, 29, 31
  )),
  nrow = 8,
  byrow = TRUE
)
rownames(edger_counts) <- paste0("Taxon", seq_len(nrow(edger_counts)))
colnames(edger_counts) <- paste0("Sample", seq_len(ncol(edger_counts)))
edger_conditions <- factor(rep(c("Control", "Treatment"), each = 3))

test_that("performDifferentialExpression DESeq2 returns list with DESeq2 element", {
  skip_if_not_installed("DESeq2")
  set.seed(42)
  mat <- matrix(rpois(200, lambda = 50), nrow = 50, ncol = 4)
  conditions <- factor(rep(c("Control", "Treatment"), 2))
  run <- capture_de_messages(performDifferentialExpression(mat, conditions, analysisType = "DESeq2"))
  result <- run$value
  expect_true(is.list(result))
  expect_true("DESeq2" %in% names(result))
  expect_true(any(grepl("\\[de\\] DESeq2 ran with 50 features", run$messages)))
})

test_that("performDifferentialExpression EdgeR returns list with EdgeR element", {
  skip_if_not_installed("edgeR")
  set.seed(42)
  mat <- matrix(rpois(200, lambda = 50), nrow = 50, ncol = 4)
  conditions <- factor(rep(c("Control", "Treatment"), 2))
  run <- capture_de_messages(performDifferentialExpression(mat, conditions, analysisType = "EdgeR"))
  result <- run$value
  expect_true(is.list(result))
  expect_true("EdgeR" %in% names(result))
  expect_true(any(grepl("\\[de\\] EdgeR ran with 50 features", run$messages)))
})

test_that("performDifferentialExpression EdgeR returns topTags from raw counts", {
  skip_if_not_installed("edgeR")
  run <- capture_de_messages(
    performDifferentialExpression(edger_counts, edger_conditions, analysisType = "EdgeR")
  )
  edge_result <- run$value$EdgeR
  edge_table <- edge_result$table
  expect_s4_class(edge_result, "TopTags")
  expect_true(all(c("logFC", "PValue") %in% names(edge_table)))
  expect_true(all(is.finite(edge_table$logFC)))
  expect_true(all(edge_table$PValue >= 0 & edge_table$PValue <= 1))
  expect_true(any(grepl("\\[de\\] EdgeR ran with 8 features", run$messages)))
})

test_that("performDifferentialExpression EdgeR differs from log2-pretransformed counts", {
  skip_if_not_installed("edgeR")
  raw_run <- capture_de_messages(
    performDifferentialExpression(edger_counts, edger_conditions, analysisType = "EdgeR")
  )
  transformed_counts <- log2(edger_counts + 1)
  group <- factor(edger_conditions)
  y <- edgeR::DGEList(counts = transformed_counts, group = group)
  y <- edgeR::calcNormFactors(y)
  design <- stats::model.matrix(~ group)
  y <- edgeR::estimateDisp(y, design)
  fit <- edgeR::glmQLFit(y, design)
  transformed_result <- edgeR::glmQLFTest(fit, coef = 2)
  transformed_table <- edgeR::topTags(transformed_result)$table
  raw_table <- raw_run$value$EdgeR$table
  shared_taxa <- intersect(rownames(raw_table), rownames(transformed_table))
  expect_gt(max(abs(raw_table[shared_taxa, "logFC"] - transformed_table[shared_taxa, "logFC"])), 1e-6)
})

test_that("performDifferentialExpression DESeq2 retries sparse counts with poscounts", {
  skip_if_not_installed("DESeq2")
  sparse_counts <- matrix(
    as.integer(c(
      10, 0, 8, 0, 6, 7,
      0, 9, 8, 5, 0, 6,
      4, 5, 0, 8, 9, 0,
      0, 3, 4, 7, 8, 0,
      2, 0, 3, 9, 8, 7,
      0, 6, 5, 0, 8, 9,
      3, 4, 0, 6, 0, 7,
      0, 5, 6, 7, 8, 0
    )),
    nrow = 8,
    byrow = TRUE
  )
  conditions <- factor(rep(c("Control", "Treatment"), each = 3))
  run <- capture_de_messages(
    performDifferentialExpression(
      sparse_counts,
      conditions,
      analysisType = "DESeq2",
      countThreshold = 1,
      minSamples = 2
    )
  )
  expect_true("DESeq2" %in% names(run$value))
  expect_true(any(grepl("retrying with poscounts", run$messages)))
})
